#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/runtime/load.scm,v 14.8 1989/08/17 14:51:08 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
  (set! load/suppress-loading-message? false)
  (set! load/default-types '("com" "bin" "scm"))
  (set! fasload/default-types '("com" "bin"))
  (add-event-receiver! event:after-restart load-init-file))

(define load-noisily?)
(define load/suppress-loading-message?)
(define load/default-types)
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
    (let ((kernel
	   (lambda (filename last-file?)
	     (let ((value
		    (let ((pathname (->pathname filename)))
		      (load/internal pathname
				     (find-true-pathname pathname
							 load/default-types)
				     environment
				     syntax-table
				     purify?
				     load-noisily?))))
	       (cond (last-file? value)
		     (load-noisily? (write-line value)))))))
      (if (pair? filename/s)
	  (let loop ((filenames filename/s))
	    (if (null? (cdr filenames))
		(kernel (car filenames) true)
		(begin
		  (kernel (car filenames) false)
		  (loop (cdr filenames)))))
	  (kernel filename/s true)))))

(define default-object
  "default-object")

(define (load/internal pathname true-pathname environment syntax-table
		       purify? load-noisily?)
  (let ((true-filename (pathname->string true-pathname)))
    (let ((port (open-input-file/internal pathname true-filename)))
      (if (= 250 (char->ascii (peek-char port)))
	  (begin
	    (close-input-port port)
	    (scode-eval
	     (let ((scode
		    (fasload/internal true-pathname
				      load/suppress-loading-message?)))
	       (if purify?
		   (purify
		    (or (and (comment? scode)
			     (let ((text (comment-text scode)))
			       (and (dbg-info-vector? text)
				    (dbg-info-vector/purification-root text))))
			scode)))
	       scode)
	     (if (eq? environment default-object)
		 (nearest-repl/environment)
		 environment)))
	  (let ((value-stream
		 (eval-stream (read-stream port) environment syntax-table)))
	    (if load-noisily?
		(write-stream value-stream
			      (lambda (value)
				(hook/repl-write (nearest-repl) value)))
		(loading-message load/suppress-loading-message? true-filename
		  (lambda ()
		    (write-stream value-stream
				  (lambda (value) value false))))))))))

(define (find-true-pathname pathname default-types)
  (or (let ((try
	     (lambda (pathname)
	       (pathname->input-truename
		(pathname-default-version pathname 'NEWEST)))))
	(if (pathname-type pathname)
	    (try pathname)
	    (or (pathname->input-truename pathname)
		(let loop ((types default-types))
		  (and (not (null? types))
		       (or (try (pathname-new-type pathname (car types)))
			   (loop (cdr types))))))))
      (error "No such file" pathname)))
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
	      (lambda (s-expression)
		(let ((repl (nearest-repl)))
		  (hook/repl-eval repl
				  s-expression
				  (if (eq? environment default-object)
				      (repl/environment repl)
				      environment)
				  (if (eq? syntax-table default-object)
				      (repl/syntax-table repl)
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