#| -*-Scheme-*-

$Id: toplev.scm,v 4.27 2006/09/29 19:30:07 cph Exp $

Copyright 1987,1988,1989,1990,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1995,1997,2000,2001,2002 Massachusetts Institute of Technology
Copyright 2006 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; SCode Optimizer: Top Level
;;; package: (scode-optimizer top-level)

(declare (usual-integrations))

;;;; User Interface

(define bin-pathname-type "bin")

(define (integrate/procedure procedure)
  (procedure-components procedure
    (lambda (*lambda environment)
      (scode-eval (integrate/scode *lambda #f) environment))))

(define (integrate/sexp s-expression environment declarations receiver)
  (integrate/simple (lambda (s-expressions)
		      (phase:syntax s-expressions environment declarations))
		    (list s-expression)
		    receiver))

(define (integrate/scode scode receiver)
  (integrate/simple identity-procedure scode receiver))

(define (sf input-string #!optional bin-string spec-string)
  (syntax-file input-string
	       (and (not (default-object? bin-string)) bin-string)
	       (and (not (default-object? spec-string)) spec-string)))

(define (syntax&integrate s-expression declarations #!optional environment)
  (fluid-let ((sf:noisy? #f))
    (integrate/sexp s-expression
		    (if (default-object? environment)
			(nearest-repl/environment)
			environment)
		    declarations
		    #f)))

(define sf:noisy? #t)

(define (sf/set-usual-integrations-default-deletions! del-list)
  (if (not (list-of-symbols? del-list))
      (error "sf/set-usual-integrations-default-deletions!: Bad deletion list"
	     del-list))
  (set! sf/usual-integrations-default-deletions del-list)
  unspecific)

(define (pathname/normalize pathname)
  (pathname-default-type (merge-pathnames pathname) "scm"))

(define sf/default-syntax-table
  system-global-environment)

(define sf/default-declarations
  '())

(define sf/top-level-definitions
  '())

(define sf/usual-integrations-default-deletions
  '())

(define (list-of-symbols? object)
  (or (null? object)
      (and (pair? object)
	   (symbol? (car object))
	   (list-of-symbols? (cdr object)))))

;;;; File Syntaxer

(define (syntax-file input-string bin-string spec-string)
  (if (not (environment? sf/default-syntax-table))
      (error "Malformed binding of SF/DEFAULT-SYNTAX-TABLE:"
	     sf/default-syntax-table))
  (if (not (list-of-symbols? sf/top-level-definitions))
      (error "Malformed binding of SF/TOP-LEVEL-DEFINITIONS:"
	     sf/top-level-definitions))
  (for-each (lambda (input-string)
	      (receive (input-pathname bin-pathname spec-pathname)
		  (sf/pathname-defaulting input-string bin-string spec-string)
		(sf/internal input-pathname bin-pathname spec-pathname
			     sf/default-syntax-table
			     sf/default-declarations)))
	    (if (pair? input-string)
		input-string
		(list input-string))))

(define (sf/pathname-defaulting input-string bin-string spec-string)
  spec-string				;ignored
  (let ((input-path (pathname/normalize input-string)))
    (values input-path
	    (let ((bin-path
		   (pathname-new-type
		    input-path
		    (let ((input-type (pathname-type input-path)))
		      (if (and (string? input-type)
			       (not (string=? "scm" input-type)))
			  (string-append "b"
					 (if (> (string-length input-type) 2)
					     (string-head input-type 2)
					     input-type))
			  bin-pathname-type)))))
	      (if bin-string
		  (merge-pathnames bin-string bin-path)
		  bin-path))
	    #f)))

(define (sf/internal input-pathname bin-pathname spec-pathname
		     environment declarations)
  spec-pathname				;ignored
  (let ((start-date (get-decoded-time)))
    (if sf:noisy?
	(let ((port (notification-output-port)))
	  (fresh-line port)
	  (write-string "Syntax file: " port)
	  (write (enough-namestring input-pathname) port)
	  (write-string " " port)
	  (write (enough-namestring bin-pathname) port)
	  (newline port)))
    (fasdump (make-comment
	      `((SOURCE-FILE . ,(->namestring input-pathname))
		(DATE ,(decoded-time/year start-date)
		      ,(decoded-time/month start-date)
		      ,(decoded-time/day start-date))
		(TIME ,(decoded-time/hour start-date)
		      ,(decoded-time/minute start-date)
		      ,(decoded-time/second start-date)))
	      (sf/file->scode input-pathname bin-pathname
			      environment declarations))
	     bin-pathname)))

(define (sf/file->scode input-pathname output-pathname
			environment declarations)
  (fluid-let ((sf/default-externs-pathname
	       (make-pathname (pathname-host input-pathname)
			      (pathname-device input-pathname)
			      (pathname-directory input-pathname)
			      #f
			      externs-pathname-type
			      'NEWEST)))
    (receive (expression externs-block externs)
	(integrate/file input-pathname environment declarations)
      (if output-pathname
	  (write-externs-file (pathname-new-type output-pathname
						 externs-pathname-type)
			      externs-block
			      externs))
      expression)))

(define externs-pathname-type
  "ext")

(define sf/default-externs-pathname
  (make-pathname #f #f #f #f externs-pathname-type 'NEWEST))

(define (read-externs-file pathname)
  (let ((pathname (merge-pathnames pathname sf/default-externs-pathname)))
    (let ((namestring (->namestring pathname)))
      (if (file-exists? pathname)
	  (let ((object (fasload pathname))
		(wrong-version
		 (lambda (version)
		   (warn (string-append
			   "Externs file is wrong version (expected "
			   (number->string externs-file-version)
			   ", found "
			   (number->string version)
			   "):")
			  namestring)
		   (values #f '()))))
	    (cond ((and (vector? object)
			(>= (vector-length object) 4)
			(eq? externs-file-tag (vector-ref object 0))
			(exact-integer? (vector-ref object 1))
			(>= (vector-ref object 1) 2))
		   (if (= externs-file-version (vector-ref object 1))
		       (values (vector-ref object 2) (vector-ref object 3))
		       (wrong-version (vector-ref object 1))))
		  ((and (list? object)
			(for-all? object
			  (lambda (element)
			    (and (vector? element)
				 (= 4 (vector-length element))))))
		   (wrong-version 1))
		  (else
		   (error "Not an externs file:" namestring))))
	  (begin
	    (warn "Nonexistent externs file:" namestring)
	    (values #f '()))))))

(define (write-externs-file pathname externs-block externs)
  (cond ((not (null? externs))
	 (fasdump (vector externs-file-tag externs-file-version
			  externs-block externs)
		  pathname))
	((file-exists? pathname)
	 (delete-file pathname))))

(define externs-file-tag
  (string->symbol "#[(scode-optimizer top-level)externs-file]"))

(define externs-file-version
  4)

;;;; Optimizer Top Level

(define (integrate/file file-name environment declarations)
  (integrate/kernel
   (lambda ()
     (phase:syntax (phase:read file-name)
		   environment
		   declarations))))

(define (integrate/simple preprocessor input receiver)
  (call-with-values
      (lambda ()
	(integrate/kernel (lambda () (preprocessor input))))
    (or receiver
	(lambda (expression externs-block externs)
	  externs-block externs		;ignored
	  expression))))

(define (integrate/kernel get-scode)
  (fluid-let ((previous-name #f)
	      (previous-process-time #f)
	      (previous-real-time #f))
    (receive (expression externs-block externs)
	(call-with-values
	    (lambda ()
	      (call-with-values (lambda () (phase:transform (get-scode)))
		phase:optimize))
	  phase:generate-scode)
      (end-phase)
      (values expression externs-block externs))))

(define (phase:read filename)
  (mark-phase "Read")
  (read-file filename))

(define (phase:syntax s-expressions environment declarations)
  (mark-phase "Syntax")
  (syntax* (if (null? declarations)
	       s-expressions
	       (cons (cons (close-syntax 'DECLARE system-global-environment)
			   declarations)
		     s-expressions))
	   environment))

(define (phase:transform scode)
  (mark-phase "Transform")
  (transform/top-level scode sf/top-level-definitions))

(define (phase:optimize block expression)
  (mark-phase "Optimize")
  (integrate/top-level block expression))

(define (phase:generate-scode operations environment expression)
  (mark-phase "Generate SCode")
  (receive (externs-block externs)
      (operations->external operations environment)
    (values (cgen/external expression) externs-block externs)))

(define previous-name)
(define previous-process-time)
(define previous-real-time)

(define (mark-phase this-name)
  (end-phase)
  (if (eq? sf:noisy? 'old-style)
      (let ((port (notification-output-port)))
	(fresh-line port)
	(write-string "    " port)
	(write-string this-name port)
	(write-string "..." port)
	(newline port)))
  (set! previous-name this-name)
  unspecific)

(define (end-phase)
  (let ((this-process-time (process-time-clock))
	(this-real-time (real-time-clock)))
    (if previous-process-time
	(let ((delta-process-time (- this-process-time previous-process-time)))
	  (time-report "      Time taken"
		       delta-process-time
		       (- this-real-time previous-real-time))))
    (set! previous-process-time this-process-time)
    (set! previous-real-time this-real-time))
  unspecific)

;; Should match the compiler.  We'll merge the two at some point.
(define (time-report prefix process-time real-time)
  (if (eq? sf:noisy? 'old-style)
      (let ((port (notification-output-port)))
	(fresh-line port)
	(write-string prefix port)
	(write-string ": " port)
	(write (/ (exact->inexact process-time) 1000) port)
	(write-string " (process time); " port)
	(write (/ (exact->inexact real-time) 1000) port)
	(write-string " (real time)" port)
	(newline port))))