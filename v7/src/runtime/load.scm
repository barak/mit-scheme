#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/load.scm,v 14.20 1991/02/15 18:06:13 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Code Loader
;;; package: (runtime load)

(declare (usual-integrations))

(define (initialize-package!)
  (set! load-noisily? false)
  (set! load/loading? false)
  (set! load/suppress-loading-message? false)
  (set! load/default-types '("com" "bin" "scm"))
  (set! load/default-find-pathname-with-type search-types-in-order)
  (set! fasload/default-types '("com" "bin"))
  (add-event-receiver! event:after-restart load-init-file))

(define load-noisily?)
(define load/loading?)
(define load/suppress-loading-message?)
(define load/default-types)
(define load/after-load-hooks)
(define load/default-find-pathname-with-type)
(define fasload/default-types)

(define (read-file filename)
  (call-with-input-file
      (pathname-default-version (->pathname filename) 'NEWEST)
    (lambda (port)
      (stream->list (read-stream port)))))

(define (fasload filename #!optional suppress-loading-message?)
  (fasload/internal
   (find-true-pathname (->pathname filename) fasload/default-types)
   (if (default-object? suppress-loading-message?)
       load/suppress-loading-message?
       suppress-loading-message?)))

(define (fasload/internal true-pathname suppress-loading-message?)
  (let ((value
	 (let ((true-filename (pathname->string true-pathname)))
	   (loading-message suppress-loading-message? true-filename
	     (lambda ()
	       ((ucode-primitive binary-fasload) true-filename))))))
    (fasload/update-debugging-info! value true-pathname)
    value))

(define (load-noisily filename #!optional environment syntax-table purify?)
  (fluid-let ((load-noisily? true))
    (load filename
	  ;; This defaulting is a kludge until we get the optional
	  ;; defaulting fixed.  Right now it must match the defaulting
	  ;; of `load'.
	  (if (default-object? environment) default-object environment)
	  (if (default-object? syntax-table) default-object syntax-table)
	  (if (default-object? purify?) default-object purify?))))

(define (load-init-file)
  (let ((truename (init-file-truename)))
    (if truename
	(load truename user-initial-environment)))
  unspecific)

(define (loading-message suppress-loading-message? true-filename do-it)
  (if suppress-loading-message?
      (do-it)
      (let ((port (cmdl/output-port (nearest-cmdl))))
	(newline port)
	(write-string "Loading " port)
	(write true-filename port)
	(let ((value (do-it)))
	  (write-string " -- done" port)
	  value))))

;;; This is careful to do the minimum number of file existence probes
;;; before opening the input file.

(define (load filename/s #!optional environment syntax-table purify?)
  (let ((environment
	 ;; Kludge until optional defaulting fixed.
	 (if (or (default-object? environment)
		 (eq? environment default-object))
	     default-object
	     (->environment environment)))
	(syntax-table
	 ;; Kludge until optional defaulting fixed.
	 (if (or (default-object? syntax-table)
		 (eq? syntax-table default-object))
	     default-object
	     (guarantee-syntax-table syntax-table)))
	(purify?
	 (if (or (default-object? purify?)
		 (eq? purify? default-object))
	     false
	     purify?)))
    (with-values
	(lambda ()
	  (fluid-let ((load/loading? true)
		      (load/after-load-hooks '()))
	    (let ((kernel
		   (lambda (filename last-file?)
		     (let ((value
			    (let ((pathname (->pathname filename)))
			      (load/internal
			       pathname
			       (find-true-pathname pathname
						   load/default-types)
			       environment
			       syntax-table
			       purify?
			       load-noisily?))))
		       (cond (last-file? value)
			     (load-noisily? (write-line value)))))))
	      (let ((value
		     (if (pair? filename/s)
			 (let loop ((filenames filename/s))
			   (if (null? (cdr filenames))
			       (kernel (car filenames) true)
			       (begin
				 (kernel (car filenames) false)
				 (loop (cdr filenames)))))
			 (kernel filename/s true))))
		(values value load/after-load-hooks)))))
      (lambda (result hooks)
	(if (not (null? hooks))
	    (for-each (lambda (hook) (hook)) (reverse hooks)))
	result))))

(define (load/push-hook! hook)
  (if (not load/loading?)
      (error "not loading any file" 'LOAD/PUSH-HOOK!))
  (set! load/after-load-hooks (cons hook load/after-load-hooks))
  unspecific)

(define default-object
  "default-object")

(define (load-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply load args)))

(define (fasload-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply fasload args)))

(define (find-true-pathname pathname default-types)
  (or (pathname->input-truename pathname)
      (let ((truename
	     (let ((pathname (pathname-default-version pathname 'NEWEST)))
	       (if (pathname-type pathname)
		   (pathname->input-truename pathname)
		   (load/default-find-pathname-with-type pathname
							 default-types)))))
	(if (not truename)
	    (error:open-file pathname))
	truename)))

(define (search-types-in-order pathname default-types)
  (let loop ((types default-types))
    (and (not (null? types))
	 (or (pathname->input-truename
	      (pathname-new-type pathname (car types)))
	     (loop (cdr types))))))

(define (find-latest-file pathname default-types)
  (let loop
      ((types default-types)
       (latest-pathname false)
       (latest-modification-time 0))
    (if (not (pair? types))
	latest-pathname
	(let ((truename
	       (pathname->input-truename
		(pathname-new-type pathname (car types))))
	      (skip
	       (lambda ()
		 (loop (cdr types) latest-pathname latest-modification-time))))
	  (if (not truename)
	      (skip)
	      (let ((modification-time (file-modification-time truename)))
		(if (> modification-time latest-modification-time)
		    (loop (cdr types) truename modification-time)
		    (skip))))))))

(define (load/internal pathname true-pathname environment syntax-table
		       purify? load-noisily?)
  (let* ((port (open-input-file/internal pathname true-pathname))
	 (fasl-marker (peek-char port)))
    (if (and (not (eof-object? fasl-marker))
	     (= 250 (char->ascii fasl-marker)))
	(begin
	  (close-input-port port)
	  (scode-eval
	   (let ((scode
		  (fasload/internal true-pathname
				    load/suppress-loading-message?)))
	     (if purify? (purify (load/purification-root scode)))
	     scode)
	   (if (eq? environment default-object)
	       (nearest-repl/environment)
	       environment)))
	(let ((value-stream
	       (lambda ()
		 (eval-stream (read-stream port) environment syntax-table))))
	  (if load-noisily?
	      (write-stream (value-stream)
			    (lambda (value)
			      (hook/repl-write (nearest-repl) value)))
	      (loading-message load/suppress-loading-message?
			       (pathname->string true-pathname)
			       (lambda ()
				 (write-stream (value-stream)
					       (lambda (value)
						 value
						 false)))))))))

(define (load/purification-root scode)
  (or (and (comment? scode)
	   (let ((text (comment-text scode)))
	     (and (dbg-info-vector? text)
		  (dbg-info-vector/purification-root text))))
      scode))

(define (read-stream port)
  (parse-objects port
		 (current-parser-table)
		 (lambda (object)
		   (and (eof-object? object)
			(begin
			  (close-input-port port)
			  true)))))

(define (eval-stream stream environment syntax-table)
  (stream-map stream
	      (let ((repl (nearest-repl)))
		(let ((environment
		       (if (eq? environment default-object)
			   (repl/environment repl)
			   environment))
		      (syntax-table
		       (make-syntax-table
			(if (eq? syntax-table default-object)
			    (repl/syntax-table repl)
			    syntax-table))))
		  (lambda (s-expression)
		    (hook/repl-eval repl
				    s-expression
				    environment
				    syntax-table))))))

(define (write-stream stream write)
  (if (stream-pair? stream)
      (let loop ((value (stream-car stream)) (stream (stream-cdr stream)))
	(if (stream-pair? stream)
	    (begin
	      (write value)
	      (loop (stream-car stream) (stream-cdr stream)))
	    value))
      unspecific))