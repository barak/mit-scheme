#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/load.scm,v 14.32 1992/04/05 02:00:34 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
  (set! hook/process-command-line default/process-command-line)
  (add-event-receiver! event:after-restart process-command-line))

(define load-noisily?)
(define load/loading?)
(define load/suppress-loading-message?)
(define load/default-types)
(define load/after-load-hooks)
(define load/current-pathname)
(define load/default-find-pathname-with-type)
(define fasload/default-types)

(define (read-file filename)
  (call-with-input-file (pathname-default-version filename 'NEWEST)
    (lambda (port)
      (stream->list (read-stream port)))))

(define (fasload filename #!optional suppress-loading-message?)
  (fasload/internal (find-pathname filename fasload/default-types)
		    (if (default-object? suppress-loading-message?)
			load/suppress-loading-message?
			suppress-loading-message?)))

(define (fasload/internal pathname suppress-loading-message?)
  (let ((value
	 (loading-message suppress-loading-message? pathname
	   (lambda ()
	     ((ucode-primitive binary-fasload) (->namestring pathname))))))
    (fasload/update-debugging-info! value pathname)
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
  (let ((pathname (init-file-pathname)))
    (if pathname
	(load pathname user-initial-environment)))
  unspecific)

(define (loading-message suppress-loading-message? pathname do-it)
  (if suppress-loading-message?
      (do-it)
      (let ((port (nearest-cmdl/port)))
	(fresh-line port)
	(write-string ";Loading " port)
	(write (enough-namestring pathname) port)
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
		     (let ((pathname
			    (find-pathname filename load/default-types)))
		       (fluid-let ((load/current-pathname pathname))
			 (let ((value
				(load/internal pathname
					       environment
					       syntax-table
					       purify?
					       load-noisily?)))
			   (cond (last-file? value)
				 (load-noisily? (write-line value)))))))))
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

(define (find-pathname filename default-types)
  (let ((pathname (merge-pathnames filename)))
    (if (file-exists? pathname)
	pathname
	(or (and (not (pathname-type pathname))
		 (load/default-find-pathname-with-type pathname default-types))
	    (find-pathname
	     (error:file-operation filename
				   "find"
				   "file"
				   "file does not exist"
				   find-pathname
				   (list filename default-types))
	     default-types)))))

(define (search-types-in-order pathname default-types)
  (let loop ((types default-types))
    (and (not (null? types))
	 (let ((pathname (pathname-new-type pathname (car types))))
	   (if (file-exists? pathname)
	       pathname
	       (loop (cdr types)))))))

(define (find-latest-file pathname default-types)
  (let loop
      ((types default-types)
       (latest-pathname false)
       (latest-time 0))
    (if (not (pair? types))
	latest-pathname
	(let ((pathname (pathname-new-type pathname (car types)))
	      (skip
	       (lambda ()
		 (loop (cdr types) latest-pathname latest-time))))
	  (let ((time (file-modification-time-indirect pathname)))
	    (if (and time (> time latest-time))
		(loop (cdr types) pathname time)
		(skip)))))))

(define (load/internal pathname environment syntax-table purify? load-noisily?)
  (let* ((port (open-input-file pathname))
	 (fasl-marker (peek-char port)))
    (if (and (not (eof-object? fasl-marker))
	     (= 250 (char->ascii fasl-marker)))
	(begin
	  (close-input-port port)
	  (extended-scode-eval
	   (let ((scode
		  (fasload/internal pathname load/suppress-loading-message?)))
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
	      (loading-message load/suppress-loading-message? pathname
		(lambda ()
		  (write-stream (value-stream)
				(lambda (value) value false)))))))))

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
		    (hook/repl-eval s-expression
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

(define (process-command-line)
  (hook/process-command-line ((ucode-primitive get-unused-command-line 0))))

(define hook/process-command-line)
(define (default/process-command-line unused-command-line)
  (if unused-command-line
      (letrec ((unused-command-line-length (vector-length unused-command-line))
	       (unused-for-each
		(lambda (proc start end)
		  (if (< start end)
		      (begin (proc (vector-ref unused-command-line start))
			     (unused-for-each proc (1+ start) end)))))
	       (find-first-dash
		(lambda (index)
		  (let loop ((index index))
		    (if (= index unused-command-line-length)
			unused-command-line-length
			(let ((first (vector-ref unused-command-line index)))
			  (cond ((zero? (string-length first))
				 (loop (1+ index)))
				((char=? (string-ref first 0) #\-)
				 index)
				(else (loop (1+ index))))))))))
	(let find-no-init-file-option ((index 0))
	  (if (= index unused-command-line-length)
	      (load-init-file)
	      (or (string=?
		   "-no-init-file"
		   (string-downcase (vector-ref unused-command-line index)))
		  (find-no-init-file-option (1+ index)))))
	(let process-next-option ((index 0)
				  (unhandled-options '()))
	  (if (= index unused-command-line-length)
	      (if (not (null? unhandled-options))
		  (warn "Unhandled command line options:"
			(reverse unhandled-options)))
	      (let ((option
		     (string-downcase (vector-ref unused-command-line index))))
		(cond ((string=? "-no-init-file" option)
		       (process-next-option (1+ index) unhandled-options))
		      ((string=? "-eval" option)
		       (let ((next-option (find-first-dash (1+ index))))
			 (unused-for-each
			  (lambda (string)
			    (eval (with-input-from-string string read)
				  user-initial-environment))
			  (1+ index)
			  next-option)
			 (process-next-option next-option unhandled-options)))
		      ((string=? "-load" option)
		       (let ((next-option (find-first-dash (1+ index))))
			 (unused-for-each load (1+ index) next-option)
			 (process-next-option next-option unhandled-options)))
		      (else (process-next-option
			     (1+ index)
			     (cons (vector-ref unused-command-line index)
				   unhandled-options))))))))
      (load-init-file)))