#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/load.scm,v 14.40 1992/08/18 02:56:22 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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
  (initialize-command-line-parsers)
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
  (set! generate-suspend-file? true)
  (hook/process-command-line ((ucode-primitive get-unused-command-line 0))))

(define hook/process-command-line)

(define *unused-command-line*)
(define *command-line-parsers* '())

(define *load-init-file?*)

(define (default/process-command-line unused-command-line)
  (let ((after-parsing-actions
	 (list (lambda ()
		 (if *load-init-file?*
		     (load-init-file))))))

    (define (process-keyword command-line unused-options)
      (if (not (null? command-line))
	  (let* ((keyword (car command-line))
		 (place (assoc keyword *command-line-parsers*)))
	    (cond (place
		   (with-values
		       (lambda () ((cdr place) command-line))
		     (lambda (next tail-action)
		       (if tail-action
			   (set! after-parsing-actions
				 (cons tail-action after-parsing-actions)))
		       (process-keyword next unused-options))))
		  ((zero? (string-length keyword))
		   (process-keyword (cdr command-line)
				    unused-options))
		  (else
		   (if (or (not (char=? (string-ref keyword 0) #\-))
			   (= (string-length keyword) 1))
		       (warn "process-command-line: Invalid keyword" keyword))
		   (find-next-keyword (cdr command-line)
				      (cons (car command-line)
					    unused-options)))))
	  (let ((unused (reverse unused-options)))
	    (if (not (null? unused))
		(warn "Unhandled command line options:" unused))
	    unused)))

    (define (find-next-keyword command-line unused-options)
      (if (null? command-line)
	  (process-keyword '() unused-options)
	  (let ((keyword (car command-line)))
	    (if (or (< (string-length keyword) 2)
		    (not (char=? (string-ref keyword 0) #\-)))
		(find-next-keyword (cdr command-line)
				   (cons keyword unused-options))
		(process-keyword command-line unused-options)))))

    (if (not unused-command-line)
	(begin
	  (set! *unused-command-line* #f)
	  (load-init-file))

	(begin
	  (set! *unused-command-line*)
	  (fluid-let ((*load-init-file?* true))
	    (set! *unused-command-line*
		  (process-keyword (vector->list unused-command-line) '()))
	    (for-each (lambda (act) (act))
		      (reverse after-parsing-actions)))))))

;;   KEYWORD must be a string with at least two characters and the first
;; being a dash (#\-).
;;   PROC is a procedure of one argument.  It will be invoked on the
;; list of command line elements extending to the right of the keyword
;; (and including it).
;;   PROC returns two values: the sublist starting with the first
;; non-handled command-line element (typically the next keyword), and
;; either #F or a procedure to invoke after the whole command line has
;; been parsed (and the init file loaded).  Thus PROC has the option
;; of executing the appropriate action at parsing time, or delaying it
;; until after the parsing is complete.  The execution of the PROCs
;; (or their associated delayed actions) is strictly left-to-right,
;; with the init file loaded between the end of parsing and the
;; delayed actions.

(define (set-command-line-parser! keyword proc)
  (if (or (not (string? keyword))
	  (< (string-length keyword) 2)
	  (not (char=? (string-ref keyword 0) #\-)))
      (error "set-command-line-parser!: Invalid keyword" keyword))
  (let ((place (assoc keyword *command-line-parsers*)))
    (if place
	(set-cdr! place proc)
	(begin
	  (set! *command-line-parsers*
		(cons (cons keyword proc)
		      *command-line-parsers*))
	  unspecific))))

(define (for-each-non-keyword command-line processor)
  (define (end command-line accum)
    (if (null? accum)
	(values command-line #f)
	(let ((objects (reverse accum)))
	  (values command-line
		  (lambda ()
		    (for-each processor objects))))))
  
  (let loop ((command-line command-line)
	     (accum '()))
    (if (null? command-line)
	(end '() accum)
	(let ((next (car command-line)))
	  (if (and (> (string-length next) 0)
		   (char=? (string-ref next 0) #\-))
	      (end command-line accum)
	      (loop (cdr command-line)
		    (cons next accum)))))))

(define (initialize-command-line-parsers)
  (set-command-line-parser!
   "-no-init-file"
   (lambda (command-line)
     (set! *load-init-file?* false)
     (values (cdr command-line) #f)))

  (set! generate-suspend-file? true)
  (set-command-line-parser!
   "-no-suspend-file"
   (lambda (command-line)
     (set! generate-suspend-file? false)
     (values (cdr command-line) #f)))

  (set-command-line-parser!
   "-load"
   (lambda (command-line)
     (for-each-non-keyword (cdr command-line) load)))

  (set-command-line-parser!
   "-eval"
   (lambda (command-line)
     (for-each-non-keyword (cdr command-line)
			   (lambda (arg)
			     (eval (with-input-from-string arg read)
				   user-initial-environment))))))

;;;; Loader for packed binaries

(define (load-packed-binaries pathname fname count environment)
  (define (search-alist path alist predicate?)
    (let loop ((alist alist))
      (and (not (null? alist))
	   (if (predicate? path (cadar alist))
	       (car alist)
	       (loop (cdr alist))))))

  (define (find-filename fname alist)
    (search-alist (->pathname fname) alist
      (lambda (path1 path2)
	(and (equal? (pathname-directory path1)
		     (pathname-directory path2))
	     (equal? (pathname-name path1)
		     (pathname-name path2))
	     (or (equal? (pathname-type path1) (pathname-type path2))
		 (and (member (pathname-type path1) '(#f "bin" "com"))
		      (member (pathname-type path2) '(#f "bin" "com"))))))))

  (define (directory-represented? dname alist)
    (search-alist (pathname-as-directory (->pathname dname)) alist
      (lambda (path1 path2)
	(equal? (pathname-directory path1)
		(pathname-directory path2)))))

  (define (loading-message fname suppress? kind)
    (if (not suppress?)
	(let ((port (nearest-cmdl/port)))
	  (fresh-line port)
	  (write-string kind port)
	  (write-string (->namestring (->pathname fname)))
	  (write-string "..."))))

  (define (process-bunch alist)
    (let ((real-load load)
	  (real-fasload fasload)
	  (real-file-exists? file-exists?)
	  (real-file-directory? file-directory?))

      (fluid-let
	  ((load
	    (lambda (fname #!optional env syntax-table purify?)
	      (let ((env (if (default-object? env)
			     environment
			     env))
		    (st (if (default-object? syntax-table)
			    default-object
			    syntax-table))
		    (purify? (if (default-object? purify?)
				 default-object
				 purify?)))
		(let ((place (find-filename fname alist)))
		  (if (not place)
		      (real-load fname env st purify?)
		      (let ((scode (caddr place)))
			(loading-message fname
                                         load/suppress-loading-message?
					 ";Pseudo-loading ")
			(if (and purify? (not (eq? purify? default-object)))
			    (purify (load/purification-root scode)))
			(extended-scode-eval scode env)))))))
	   (fasload
	    (lambda (filename #!optional suppress-message?)
	      (let ((suppress-message?
		     (if (default-object? suppress-message?)
			 load/suppress-loading-message?
			 suppress-message?))
		    (place (find-filename filename alist)))
		(if (not place)
		    (real-fasload filename suppress-message?)
		    (begin
		      (loading-message filename
		                       suppress-message?
				       ";Pseudo-fasloading ")
		      (caddr place))))))
	   (file-exists?
	    (lambda (fname)
	      (or (find-filename fname alist)
		  (real-file-exists? fname))))
	   (file-directory?
	    (lambda (dname)
	      (or (directory-represented? dname alist)
		  (real-file-directory? dname))))
	   (flush-purification-queue! (lambda () 'done)))
        (load (caar alist))))
    (flush-purification-queue!))

  (with-binary-input-file (->truename pathname)
    (lambda (channel)
      ((ucode-primitive binary-fasload) channel) ; Dismiss header.
      (let ((process-next-bunch
	     (lambda ()
	       (process-bunch
		(map (lambda (pair)
		       (list (car pair)
			     (->pathname (car pair))
			     (cdr pair)))
		     ((ucode-primitive binary-fasload) channel))))))


	(do ((count count (-1+ count)))
	    ((= count 1)
	     (process-next-bunch))
	  (process-next-bunch))))))

;;; Utilities for the binary unpacker

(define (with-binary-file-channel file action open extract-channel name)
  (let ((port false))
    (dynamic-wind
     (lambda ()
       (if port
           (error "cannot re-enter with-binary-file-channel" name)))
     (lambda ()
       (set! port (open file))
       (action (channel-descriptor (extract-channel port))))
     (lambda ()
       (if (and port
                (not (eq? port true)))
           (begin
             (close-port port)
             (set! port true)))))))

(define (with-binary-input-file file action)
  (with-binary-file-channel file action
    open-binary-input-file
    input-port/channel
    'with-binary-input-file))