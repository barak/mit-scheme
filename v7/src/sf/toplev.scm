#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/toplev.scm,v 4.3 1988/10/30 14:27:50 jinx Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Top Level

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations))

;;;; User Interface

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
	       (if (default-object? bin-string) false bin-string)
	       (if (default-object? spec-string) false spec-string)))

(define (scold input-string #!optional bin-string spec-string)
  "Use this only for syntaxing the cold-load root file.
Currently only the 68000 implementation needs this."
  (fluid-let ((wrapping-hook wrap-with-control-point))
    (syntax-file input-string bin-string spec-string)))

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
		       (lambda () '())))

(define (pathname/normalize pathname)
  (pathname-default-type (pathname->absolute-pathname (->pathname pathname))
			 "scm"))

(define file-info/syntax-table
  (pathname-map/make))

(define file-info/declarations
  (pathname-map/make))

(define sf/default-syntax-table
  false)

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

(define sf/default-externs-pathname
  (make-pathname false false false false "ext" 'NEWEST))

(define sfu? false)

(define (syntax-file input-string bin-string spec-string)
  (if (not (or (false? sf/default-syntax-table)
	       (syntax-table? sf/default-syntax-table)))
      (error "Malformed binding of SF/DEFAULT-SYNTAX-TABLE"
	     sf/default-syntax-table))
  (if (not (list-of-symbols? sf/top-level-definitions))
      (error "Malformed binding of SF/TOP-LEVEL-DEFINITIONS"
	     sf/top-level-definitions))
  (for-each (lambda (input-string)
	      (with-values
		  (lambda ()
		    (sf/pathname-defaulting input-string
					    bin-string
					    spec-string))
		(lambda (input-pathname bin-pathname spec-pathname)
		  (with-values (lambda () (file-info/find input-pathname))
		    (lambda (syntax-table declarations)
		      (sf/internal input-pathname bin-pathname spec-pathname
				   syntax-table declarations))))))
	    (if (pair? input-string)
		input-string
		(list input-string))))

(define (sf/pathname-defaulting input-string bin-string spec-string)
  (let ((pathname
	 (merge-pathnames
	  (->pathname input-string)
	  (make-pathname false false '() false "scm" 'NEWEST))))
    (let ((input-path (pathname->input-truename pathname)))
      (if (not input-path)
	  (error "SF: File does not exist" pathname))
      (let ((input-type (pathname-type input-path)))
	(let ((bin-path
	       (let ((bin-path
		      (pathname-new-type
		       input-path
		       (if (equal? "scm" input-type)
			   "bin"
			   (string-append "b" input-type)))))
		 (if bin-string
		     (merge-pathnames (->pathname bin-string) bin-path)
		     bin-path))))
	  (let ((spec-path
		 (and (or spec-string sfu?)
		      (let ((spec-path
			     (pathname-new-type
			      bin-path
			      (if (equal? "scm" input-type)
				  "unf"
				  (string-append "u" input-type)))))
			(if spec-string
			    (merge-pathnames (->pathname spec-string)
					     spec-path)
			    spec-path)))))
	    (values input-path bin-path spec-path)))))))

(define (sf/internal input-pathname bin-pathname spec-pathname
		     syntax-table declarations)
  (fluid-let ((sf/default-externs-pathname
	       (make-pathname (pathname-host input-pathname)
			      (pathname-device input-pathname)
			      (pathname-directory input-pathname)
			      false
			      "ext"
			      'NEWEST)))
    (let ((start-date (get-decoded-time))
	  (input-filename (pathname->string input-pathname))
	  (bin-filename (pathname->string bin-pathname))
	  (spec-filename (and spec-pathname (pathname->string spec-pathname))))
      (newline)
      (write-string "Syntax file: ")
      (write input-filename)
      (write-string " ")
      (write bin-filename)
      (write-string " ")
      (write spec-filename)
      (with-values
	  (lambda ()
	    (integrate/file input-pathname syntax-table declarations
			    spec-pathname))
	(lambda (expression externs events)
	  (fasdump (wrapping-hook
		    (make-comment `((SOURCE-FILE . ,input-filename)
				    (DATE ,(decoded-time/year start-date)
					  ,(decoded-time/month start-date)
					  ,(decoded-time/day start-date))
				    (TIME ,(decoded-time/hour start-date)
					  ,(decoded-time/minute start-date)
					  ,(decoded-time/second start-date)))
				  (set! expression false)))
		   bin-pathname)
	  (write-externs-file (pathname-new-type
			       bin-pathname
			       (pathname-type sf/default-externs-pathname))
			      (set! externs false))
	  (if spec-pathname
	      (begin (newline)
		     (write-string "Writing ")
		     (write spec-filename)
		     (with-output-to-file spec-pathname
		       (lambda ()
			 (newline)
			 (write `(DATE ,(decoded-time/year start-date)
				       ,(decoded-time/month start-date)
				       ,(decoded-time/day start-date)
				       ,(decoded-time/hour start-date)
				       ,(decoded-time/minute start-date)
				       ,(decoded-time/second start-date)))
			 (newline)
			 (write `(SOURCE-FILE ,input-filename))
			 (newline)
			 (write `(BINARY-FILE ,bin-filename))
			 (for-each (lambda (event)
				     (newline)
				     (write `(,(car event)
					      (RUNTIME ,(cdr event)))))
				   events)))
		     (write-string " -- done"))))))))

(define (read-externs-file pathname)
  (let ((pathname
	 (merge-pathnames (->pathname pathname) sf/default-externs-pathname)))
    (if (file-exists? pathname)
	(fasload pathname)
	(begin (warn "Nonexistent externs file" (pathname->string pathname))
	       '()))))

(define (write-externs-file pathname externs)
  (cond ((not (null? externs))
	 (fasdump externs pathname))
	((file-exists? pathname)
	 (delete-file pathname))))

(define (print-spec identifier names)
  (newline)
  (newline)
  (write-string "(")
  (write identifier)
  (let loop
      ((names
	(sort names
	      (lambda (x y)
		(string<? (symbol->string x)
			  (symbol->string y))))))
    (if (not (null? names))
	(begin (newline)
	       (write (car names))
	       (loop (cdr names)))))
  (write-string ")"))

(define (wrapping-hook scode)
  scode)

(define control-point-tail
  `(3 ,(object-new-type (microcode-type 'NULL) 16)
      () () () () () () () () () () () () () () ()))

(define (wrap-with-control-point scode)
  (system-list->vector type-code-control-point
		       `(,return-address-restart-execution
			 ,scode
			 ,system-global-environment
			 ,return-address-non-existent-continuation
			 ,@control-point-tail)))

(define type-code-control-point
  (microcode-type 'CONTROL-POINT))

(define return-address-restart-execution
  (make-return-address (microcode-return 'RESTART-EXECUTION)))

(define return-address-non-existent-continuation
  (make-return-address (microcode-return 'NON-EXISTENT-CONTINUATION)))

;;;; Optimizer Top Level

(define (integrate/file file-name syntax-table declarations compute-free?)
  compute-free?				;ignored
  (integrate/kernel (lambda ()
		      (phase:syntax (phase:read file-name) syntax-table))
		    declarations))

(define (integrate/simple preprocessor input declarations receiver)
  (with-values
      (lambda ()
	(integrate/kernel (lambda () (preprocessor input)) declarations))
    (or receiver
	(lambda (expression externs events)
	  externs events		;ignored
	  expression))))

(define (integrate/kernel get-scode declarations)
  (fluid-let ((previous-name false)
	      (previous-process-time false)
	      (previous-real-time false)
	      (events '()))
    (with-values
	(lambda ()
	  (with-values
	      (lambda ()
		(with-values
		    (lambda ()
		      (phase:transform (canonicalize-scode (get-scode)
							   declarations)))
		  phase:optimize))
	    phase:generate-scode))
      (lambda (externs expression)
	(end-phase)
	(values expression externs (reverse! events))))))

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
	   (if (or (default-object? syntax-table) (not syntax-table))
	       (make-syntax-table system-global-syntax-table)
	       syntax-table)))

(define (phase:transform scode)
  (mark-phase "Transform")
  (transform/top-level scode sf/top-level-definitions))

(define (phase:optimize block expression)
  (mark-phase "Optimize")
  (integrate/top-level block expression))

(define (phase:generate-scode operations environment expression)
  (mark-phase "Generate SCode")
  (values (operations->external operations environment)
	  (cgen/external expression)))

(define previous-name)
(define previous-process-time)
(define previous-real-time)
(define events)

(define (mark-phase this-name)
  (end-phase)
  (newline)
  (write-string "    ")
  (write-string this-name)
  (write-string "...")
  (set! previous-name this-name))

(define (end-phase)
  (let ((this-process-time (process-time-clock))
	(this-real-time (real-time-clock)))
    (if previous-process-time
	(let ((delta-process-time (- this-process-time previous-process-time)))
	  (set! events (cons (cons previous-name delta-process-time) events))
	  (time-report "      Time taken"
		       delta-process-time
		       (- this-real-time previous-real-time))))
    (set! previous-process-time this-process-time)
    (set! previous-real-time this-real-time)))

;; Should match the compiler.  We'll merge the two at some point.
(define (time-report prefix process-time real-time)
  (newline)
  (write-string prefix)
  (write-string ": ")
  (write (/ process-time 1000))
  (write-string " (process time); ")
  (write (/ real-time 1000))
  (write-string " (real time)"))