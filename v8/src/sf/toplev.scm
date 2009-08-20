#| -*-Scheme-*-

$Id$

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; SCode Optimizer: Top Level
;;; package: (scode-optimizer top-level)

(declare (usual-integrations))

;;;; User Interface

(define bin-pathname-type "bin")

(define (integrate/procedure procedure declarations)
  (procedure-components procedure
    (lambda (*lambda environment)
      (scode-eval (integrate/scode *lambda declarations false) environment))))

(define (integrate/sexp s-expression syntax-table declarations receiver)
  (integrate/simple (lambda (s-expressions)
		      (phase:syntax s-expressions syntax-table))
		    (list s-expression) declarations receiver))

(define (integrate/scode scode declarations receiver)
  (integrate/simple identity-procedure scode declarations receiver))

(define (sf input-string #!optional bin-string spec-string)
  (syntax-file input-string
	       (and (not (default-object? bin-string)) bin-string)
	       (and (not (default-object? spec-string)) spec-string)))

(define (syntax&integrate s-expression declarations #!optional syntax-table)
  (fluid-let ((sf:noisy? false))
    (integrate/sexp s-expression
		    (if (default-object? syntax-table)
			(nearest-repl/syntax-table)
			syntax-table)
		    declarations
		    false)))

(define sf:noisy? true)

(define (sf/set-default-syntax-table! syntax-table)
  (set! sf/default-syntax-table syntax-table))

(define (sf/set-file-syntax-table! pathname syntax-table)
  (pathname-map/insert! file-info/syntax-table
			(pathname/normalize pathname)
			syntax-table))

(define (sf/set-usual-integrations-default-deletions! del-list)
  (if (not (list-of-symbols? del-list))
      (error "sf/set-usual-integrations-default-deletions!: Bad deletion list"
	     del-list))
  (set! sf/usual-integrations-default-deletions del-list))

(define (sf/add-file-declarations! pathname declarations)
  (let ((pathname (pathname/normalize pathname)))
    (pathname-map/insert! file-info/declarations
			  pathname
			  (append! (file-info/get-declarations pathname)
				   (list-copy declarations)))))

(define (file-info/find pathname)
  (let ((pathname (pathname/normalize pathname)))
    (values (pathname-map/lookup file-info/syntax-table
				 pathname
				 identity-procedure
				 (lambda () sf/default-syntax-table))
	    (file-info/get-declarations pathname))))

(define (file-info/get-declarations pathname)
  (pathname-map/lookup file-info/declarations
		       pathname
		       identity-procedure
		       (lambda () sf/default-declarations)))

(define (pathname/normalize pathname)
  (pathname-default-type (merge-pathnames pathname) "scm"))

(define file-info/syntax-table
  (pathname-map/make))

(define file-info/declarations
  (pathname-map/make))

(define sf/default-syntax-table
  false)

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
  (perhaps-issue-compatibility-warning)
  (if (not (or (false? sf/default-syntax-table)
	       (syntax-table? sf/default-syntax-table)))
      (error "Malformed binding of SF/DEFAULT-SYNTAX-TABLE:"
	     sf/default-syntax-table))
  (if (not (list-of-symbols? sf/top-level-definitions))
      (error "Malformed binding of SF/TOP-LEVEL-DEFINITIONS:"
	     sf/top-level-definitions))
  (for-each (lambda (input-string)
	      (call-with-values
		  (lambda ()
		    (sf/pathname-defaulting input-string
					    bin-string
					    spec-string))
		(lambda (input-pathname bin-pathname spec-pathname)
		  (call-with-values (lambda () (file-info/find input-pathname))
		    (lambda (syntax-table declarations)
		      (sf/internal input-pathname bin-pathname spec-pathname
				   syntax-table declarations))))))
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
	    false)))

(define (sf/internal input-pathname bin-pathname spec-pathname
		     syntax-table declarations)
  spec-pathname				;ignored
  (let ((start-date (get-decoded-time)))
    (if sf:noisy?
	(begin
	  (newline)
	  (write-string "Syntax file: ")
	  (write (enough-namestring input-pathname))
	  (write-string " ")
	  (write (enough-namestring bin-pathname))))
    (fasdump (make-comment
	      `((SOURCE-FILE . ,(->namestring input-pathname))
		(DATE ,(decoded-time/year start-date)
		      ,(decoded-time/month start-date)
		      ,(decoded-time/day start-date))
		(TIME ,(decoded-time/hour start-date)
		      ,(decoded-time/minute start-date)
		      ,(decoded-time/second start-date)))
	      (sf/file->scode input-pathname bin-pathname
			      syntax-table declarations))
	     bin-pathname)))

(define (sf/file->scode input-pathname output-pathname
			syntax-table declarations)
  (fluid-let ((sf/default-externs-pathname
	       (make-pathname (pathname-host input-pathname)
			      (pathname-device input-pathname)
			      (pathname-directory input-pathname)
			      false
			      externs-pathname-type
			      'NEWEST)))
    (call-with-values
	(lambda ()
	  (integrate/file input-pathname syntax-table declarations))
      (lambda (expression externs-block externs)
	(if output-pathname
	    (write-externs-file (pathname-new-type output-pathname
						   externs-pathname-type)
				externs-block
				externs))
	expression))))

(define externs-pathname-type
  "ext")

(define sf/default-externs-pathname
  (make-pathname false false false false externs-pathname-type 'NEWEST))

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
		   (values false '()))))
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
	    (values false '()))))))

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

(define (integrate/file file-name syntax-table declarations)
  (integrate/kernel (lambda ()
		      (phase:syntax (phase:read file-name) syntax-table))
		    declarations))

(define (integrate/simple preprocessor input declarations receiver)
  (call-with-values
      (lambda ()
	(integrate/kernel (lambda () (preprocessor input)) declarations))
    (or receiver
	(lambda (expression externs-block externs)
	  externs-block externs		;ignored
	  expression))))

(define (integrate/kernel get-scode declarations)
  (fluid-let ((previous-name false)
	      (previous-process-time false)
	      (previous-real-time false))
    (call-with-values
	(lambda ()
	  (call-with-values
	      (lambda ()
		(call-with-values
		    (lambda ()
		      (phase:transform (canonicalize-scode (get-scode)
							   declarations)))
		  phase:optimize))
	    phase:generate-scode))
      (lambda (expression externs-block externs)
	(end-phase)
	(values expression externs-block externs)))))

(define (canonicalize-scode scode declarations)
  (let ((declarations (process-declarations declarations)))
    (if (null? declarations)
	scode
	(scan-defines (make-sequence
		       (list (make-block-declaration declarations)
			     scode))
		      make-open-block))))

(define (phase:read filename)
  (mark-phase "Read")
  (read-file filename))

(define (phase:syntax s-expression #!optional syntax-table)
  (mark-phase "Syntax")
  (syntax* s-expression
	   (make-syntax-table
	    (if (or (default-object? syntax-table) (not syntax-table))
		system-global-syntax-table
		syntax-table))))

(define (phase:transform scode)
  (mark-phase "Transform")
  (transform/top-level scode sf/top-level-definitions))

(define (phase:optimize block expression)
  (mark-phase "Optimize")
  (integrate/top-level block expression))

(define (phase:generate-scode operations environment expression)
  (mark-phase "Generate SCode")
  (call-with-values (lambda () (operations->external operations environment))
    (lambda (externs-block externs)
      (values (cgen/external expression) externs-block externs))))

(define previous-name)
(define previous-process-time)
(define previous-real-time)

(define (mark-phase this-name)
  (end-phase)
  (if sf:noisy?
      (begin
	(newline)
	(write-string "    ")
	(write-string this-name)
	(write-string "...")))
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
  (if sf:noisy?
      (begin
	(newline)
	(write-string prefix)
	(write-string ": ")
	(write (/ (exact->inexact process-time) 1000))
	(write-string " (process time); ")
	(write (/ (exact->inexact real-time) 1000))
	(write-string " (real time)"))))

(define compatibility-detection-frob (vector #F '()))

(define (perhaps-issue-compatibility-warning)
  (if (eq? (vector-ref compatibility-detection-frob 0)
	   (vector-ref compatibility-detection-frob 1))
      (begin
	(warn "!! You are syntaxing while in compatibilty mode, where #F is the")
	(warn "!! same as '().  The resulting file may be incorrect for the")
	(warn "!! standard environment."))))

