#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; SCode Optimizer: Top Level
;;; package: (scode-optimizer top-level)

(declare (usual-integrations))

;;;; User Interface

(define (bin-pathname-type)
  (if sf/cross-compiling? "nib" "bin"))

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
  (guarantee-list-of-type del-list symbol? "list of symbols"
			  'sf/set-usual-integrations-default-deletions!)
  (set! sf/usual-integrations-default-deletions del-list)
  unspecific)

(define (pathname/normalize pathname)
  ;; This assumes we're sitting in the source directory.
  (pathname-default-type (merge-pathnames pathname) "scm"))

(define (sf/object-pathname pathname)
  (merge-pathnames (enough-pathname pathname sf/source-root) sf/object-root))

(define sf/default-syntax-table
  system-global-environment)

(define sf/default-declarations
  '())

(define sf/top-level-definitions
  '())

(define sf/usual-integrations-default-deletions
  '())

(define sf/cross-compiling?
  #f)

(define sf/source-root
  #!default)

(define sf/object-root
  #!default)

;;;; File Syntaxer

(define (syntax-file input-string bin-string spec-string)
  (guarantee environment? sf/default-syntax-table 'syntax-file)
  (guarantee-list-of-type sf/top-level-definitions symbol? 'syntax-file)
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
		   (sf/object-pathname
		    (pathname-new-type
		     input-path
		     (let ((input-type (pathname-type input-path)))
		       (if (and (string? input-type)
				(not (string=? "scm" input-type)))
			   (string-append "b"
					  (if (> (string-length input-type) 2)
					      (string-head input-type 2)
					      input-type))
			   (bin-pathname-type)))))))
	      (if bin-string
		  (merge-pathnames bin-string bin-path)
		  bin-path))
	    #f)))

(define (sf/internal input-pathname bin-pathname spec-pathname
		     environment declarations)
  spec-pathname				;ignored
  (with-simple-restart 'continue
      (string-append "Skip processing file " (->namestring input-pathname))
    (lambda ()
      (let ((do-it
	     (let ((start-date (get-decoded-time)))
	       (lambda ()
		 (fasdump (make-scode-comment
			   `((source-file . ,(->namestring input-pathname))
			     (date ,(decoded-time/year start-date)
				   ,(decoded-time/month start-date)
				   ,(decoded-time/day start-date))
			     (time ,(decoded-time/hour start-date)
				   ,(decoded-time/minute start-date)
				   ,(decoded-time/second start-date)))
			   (sf/file->scode input-pathname bin-pathname
					   environment declarations))
			  bin-pathname
			  #t)))))
	(if sf:noisy?
	    (let ((message
		   (lambda (port)
		     (write-string "Generating SCode for file: " port)
		     (write (enough-namestring input-pathname) port)
		     (write-string " => " port)
		     (write (enough-namestring bin-pathname) port))))
	      (if (eq? sf:noisy? 'old-style)
		  (timed message do-it)
		  (with-notification message do-it)))
	    (do-it))))))

(define (sf/file->scode input-pathname output-pathname
			environment declarations)
  (fluid-let ((sf/default-externs-pathname
	       (lambda ()
		 (make-pathname (pathname-host input-pathname)
				(pathname-device input-pathname)
				(pathname-directory input-pathname)
				#f
				(externs-pathname-type)
				'newest))))
    (receive (expression externs-block externs)
	(integrate/file input-pathname environment declarations)
      (if output-pathname
	  (write-externs-file (pathname-new-type output-pathname
						 (externs-pathname-type))
			      externs-block
			      externs))
      expression)))

(define (externs-pathname-type)
  (if sf/cross-compiling? "txe" "ext"))

(define (sf/default-externs-pathname)
  (make-pathname #f #f #f #f (externs-pathname-type) 'newest))

(define (read-externs-file pathname)
  (let ((pathname
	 (sf/object-pathname
	  (merge-pathnames pathname (sf/default-externs-pathname)))))
    (let ((namestring (->namestring pathname)))
      (if (file-exists? pathname)
	  (let ((object (fasload pathname #t))
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
			(every (lambda (element)
				 (and (vector? element)
				      (= 4 (vector-length element))))
			       object))
		   (wrong-version 1))
		  (else
		   (error "Not an externs file:" namestring))))
	  (begin
	    (warn "Missing externs file:" namestring)
	    (values #f '()))))))

(define (write-externs-file pathname externs-block externs)
  (cond ((not (null? externs))
	 (fasdump (vector externs-file-tag externs-file-version
			  externs-block externs)
		  pathname
		  #t))
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
  (let ((scode (get-scode)))
    (if (r7rs-scode-file? scode)
	(integrate/r7rs-scode-file scode)
	(integrate/kernel-1 (lambda () (phase:transform (get-scode)))))))

(define (integrate/kernel-1 get-transformed)
  (call-with-values
      (lambda ()
	(call-with-values get-transformed
	  phase:optimize))
    phase:generate-scode))

(define (phase:read filename)
  (in-phase "Read"
    (lambda ()
      (or (read-r7rs-source filename)
	  (read-file filename)))))

(define (phase:syntax s-expressions environment declarations)
  (in-phase "Syntax"
    (lambda ()
      (if (r7rs-source? s-expressions)
	  (syntax-r7rs-source s-expressions (current-library-db))
	  (syntax* (if (null? declarations)
		       s-expressions
		       (cons (cons (close-syntax 'declare
						 (runtime-environment->syntactic
						  system-global-environment))
				   declarations)
			     s-expressions))
		   environment)))))

(define (phase:transform scode)
  (in-phase "Transform"
    (lambda ()
      (transform/top-level scode sf/top-level-definitions))))

(define (phase:optimize block expression)
  (in-phase "Optimize"
    (lambda ()
      (integrate/top-level block expression))))

(define (phase:generate-scode operations environment expression)
  (in-phase "Generate SCode"
    (lambda ()
      (receive (externs-block externs)
	  (operations->external operations environment)
	(values (cgen/external expression) externs-block externs)))))

(define (integrate/r7rs-scode-file scode)
  (values (map-r7rs-scode-file integrate/r7rs-library scode)
	  #f
	  '()))

(define (integrate/r7rs-library library)
  (let ((imports (scode-library-imports-used library)))
    (map-scode-library (lambda (contents)
			 (receive (optimized externs-block externs)
			     (integrate/kernel-1
			      (lambda ()
				(phase:transform-r7rs imports contents)))
			   (declare (ignore externs-block externs))
			   optimized))
		       library)))

(define (phase:transform-r7rs imports scode)
  (in-phase "Transform"
    (lambda ()
      (transform/r7rs-library imports scode))))

(define (in-phase name thunk)
  (if (eq? sf:noisy? 'old-style)
      (timed (lambda (port)
	       (write-string name port))
	     thunk)
      (thunk)))

(define (timed message thunk)
  (let ((start-process-time (process-time-clock))
	(start-real-time (real-time-clock)))
    (let ((v (with-notification message thunk)))
      (let ((process-time (- (process-time-clock) start-process-time))
	    (real-time (- (real-time-clock) start-real-time)))
	(write-notification-line
	 (lambda (port)
	   (write-string "Time taken: " port)
	   (write (/ (exact->inexact process-time) 1000) port)
	   (write-string " (process time); " port)
	   (write (/ (exact->inexact real-time) 1000) port)
	   (write-string " (real time)" port))))
      v)))