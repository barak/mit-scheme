#| -*-Scheme-*-

$Id: load.scm,v 14.57 2001/03/08 20:58:23 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

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

;;;; Code Loader
;;; package: (runtime load)

(declare (usual-integrations))

(define (initialize-package!)
  (set! *purification-root-marker* (intern "#[PURIFICATION-ROOT]"))
  (set! load-noisily? #f)
  (set! load/loading? #f)
  (set! load/suppress-loading-message? #f)
  (set! load/default-types
	`(("com" ,load/internal)
	  ("so" ,load-object-file)
	  ("sl" ,load-object-file)
	  ("bin" ,load/internal)
	  ("scm" ,load/internal)))
  (set! fasload/default-types
	`(("com" ,fasload/internal)
	  ("bin" ,fasload/internal)))
  (set! load/default-find-pathname-with-type search-types-in-order)
  (set! load/current-pathname)
  (set! condition-type:not-loading
	(make-condition-type 'NOT-LOADING condition-type:error '()
	  "No file being loaded."))
  (reset-loaded-object-files!)
  (add-event-receiver! event:after-restart reset-loaded-object-files!)
  (initialize-command-line-parsers)
  (set! hook/process-command-line default/process-command-line)
  (add-event-receiver! event:after-restart process-command-line))

(define load-noisily?)
(define load/loading?)
(define load/suppress-loading-message?)
(define load/default-types)
(define load/after-load-hooks)
(define load/current-pathname)
(define condition-type:not-loading)
(define load/default-find-pathname-with-type)
(define fasload/default-types)
(define loaded-object-files)

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
	 (if (or (default-object? syntax-table)
		 (eq? syntax-table default-object))
	     default-object
	     (guarantee-syntax-table syntax-table 'LOAD)))
	(purify?
	 (if (or (default-object? purify?) (eq? purify? default-object))
	     #f
	     purify?)))
    (handle-load-hooks
     (lambda ()
       (let ((kernel
	      (lambda (filename last-file?)
		(call-with-values
		    (lambda () (find-pathname filename load/default-types))
		  (lambda (pathname loader)
		    (fluid-let ((load/current-pathname pathname))
		      (let ((load-it
			     (lambda ()
			       (loader pathname
				       environment
				       syntax-table
				       purify?
				       load-noisily?))))
			(cond (last-file? (load-it))
			      (load-noisily? (write-line (load-it)))
			      (else (load-it) unspecific)))))))))
	 (if (pair? filename/s)
	     (let loop ((filenames filename/s))
	       (if (null? (cdr filenames))
		   (kernel (car filenames) #t)
		   (begin
		     (kernel (car filenames) #f)
		     (loop (cdr filenames)))))
	     (kernel filename/s #t)))))))

(define (fasload filename #!optional suppress-loading-message?)
  (call-with-values (lambda () (find-pathname filename fasload/default-types))
    (lambda (pathname loader)
      (loader pathname
	      (if (default-object? suppress-loading-message?)
		  load/suppress-loading-message?
		  suppress-loading-message?)))))

(define (current-load-pathname)
  (if (not load/loading?) (error condition-type:not-loading))
  load/current-pathname)

(define (load/push-hook! hook)
  (if (not load/loading?) (error condition-type:not-loading))
  (set! load/after-load-hooks (cons hook load/after-load-hooks))
  unspecific)

(define (handle-load-hooks thunk)
  (call-with-values
      (lambda ()
	(fluid-let ((load/loading? #t)
		    (load/after-load-hooks '()))
	  (let ((result (thunk)))
	    (values result (reverse load/after-load-hooks)))))
    (lambda (result hooks)
      (for-each (lambda (hook) (hook)) hooks)
      result)))

(define default-object
  "default-object")

(define (load-noisily filename #!optional environment syntax-table purify?)
  (fluid-let ((load-noisily? #t))
    (load filename
	  ;; This defaulting is a kludge until we get the optional
	  ;; defaulting fixed.  Right now it must match the defaulting
	  ;; of `load'.
	  (if (default-object? environment) default-object environment)
	  (if (default-object? syntax-table) default-object syntax-table)
	  (if (default-object? purify?) default-object purify?))))

(define (load-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply load args)))

(define (fasload-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply fasload args)))

(define (find-pathname filename default-types)
  (let ((pathname (merge-pathnames filename))
	(fail
	 (lambda ()
	   (find-pathname (error:file-operation filename
						"find"
						"file"
						"file does not exist"
						find-pathname
						(list filename default-types))
			  default-types))))
    (cond ((file-exists? pathname)
	   (values pathname
		   (let ((find-loader
			  (lambda (extension)
			    (let ((place (assoc extension default-types)))
			      (and place
				   (cadr place))))))
		     (or (and (pathname-type pathname)
			      (find-loader (pathname-type pathname)))
			 (find-loader "scm")
			 (find-loader "bin")))))
	  ((pathname-type pathname)
	   (fail))
	  (else
	   (call-with-values
	       (lambda ()
		 (load/default-find-pathname-with-type pathname default-types))
	     (lambda (pathname loader)
	       (if (not pathname)
		   (fail)
		   (values pathname loader))))))))

(define (search-types-in-order pathname default-types)
  (let loop ((types default-types))
    (if (null? types)
	(values #f #f)
	 (let ((pathname (pathname-new-type pathname (caar types))))
	   (if (file-exists? pathname)
	       (values pathname (cadar types))
	       (loop (cdr types)))))))

(define (find-latest-file pathname default-types)
  (let loop ((types default-types)
	     (latest-pathname #f)
	     (latest-loader #f)
	     (latest-time 0))
    (if (not (pair? types))
	(values latest-pathname latest-loader)
	(let ((pathname (pathname-new-type pathname (caar types)))
	      (skip
	       (lambda ()
		 (loop (cdr types)
		       latest-pathname
		       latest-loader
		       latest-time))))
	  (let ((time (file-modification-time-indirect pathname)))
	    (if (and time (> time latest-time))
		(loop (cdr types) pathname (cadar types) time)
		(skip)))))))

(define (load/internal pathname environment syntax-table purify? load-noisily?)
  (let* ((port (open-input-file pathname))
	 (fasl-marker (peek-char port)))
    (if (and (not (eof-object? fasl-marker))
	     (= 250 (char->ascii fasl-marker)))
	(begin
	  (close-input-port port)
	  (load-scode-end (fasload/internal pathname
					    load/suppress-loading-message?)
			  environment
			  purify?))
	(let ((value-stream
	       (lambda ()
		 (eval-stream (read-stream port) environment syntax-table))))
	  (if load-noisily?
	      (write-stream (value-stream)
			    (lambda (exp&value)
			      (hook/repl-write (nearest-repl)
					       (car exp&value)
					       (cdr exp&value))))
	      (loading-message load/suppress-loading-message? pathname
		(lambda ()
		  (write-stream (value-stream)
				(lambda (exp&value) exp&value #f)))))))))

(define (fasload/internal pathname suppress-loading-message?)
  (let ((value
	 (loading-message suppress-loading-message? pathname
	   (lambda ()
	     ((ucode-primitive binary-fasload) (->namestring pathname))))))
    (fasload/update-debugging-info! value pathname)
    value))

(define (load-object-file pathname environment
			  syntax-table purify? load-noisily?)
  syntax-table load-noisily?		; ignored
  (loading-message
   load/suppress-loading-message? pathname
   (lambda ()
     (let* ((handle
	     ((ucode-primitive load-object-file 1) (->namestring pathname)))
	    (cth
	     ((ucode-primitive object-lookup-symbol 3)
	      handle "dload_initialize_file" 0)))
       (if (not cth)
	   (error "load-object-file: Cannot find init procedure" pathname))
       (let ((scode ((ucode-primitive initialize-c-compiled-block 1)
		     ((ucode-primitive address-to-string 1)
		      ((ucode-primitive invoke-c-thunk 1)
		       cth)))))
	 (fasload/update-debugging-info! scode pathname)
	 (load-scode-end scode environment purify?))))))

(define (load-scode-end scode environment purify?)
  (if purify? (purify (load/purification-root scode)))
  (extended-scode-eval scode
		       (if (eq? environment default-object)
			   (nearest-repl/environment)
			   environment)))

(define (load-library-object-file name errors?)
  (let ((directory (system-library-directory-pathname "lib"))
	(nsf
	 (lambda ()
	   (and errors?
		(error "No library object file of this name:" name)))))
    (if (not directory)
	(nsf))
    (let ((pathname (merge-pathnames name directory)))
      (if (there-exists? loaded-object-files
	    (lambda (pathname*)
	      (pathname=? pathname pathname*)))
	  #t
	  (let ((pathname*
		 (let ((find
			(lambda (type)
			  (let ((pathname (pathname-new-type pathname type)))
			    (and (file-exists? pathname)
				 pathname)))))
		   (or (find "so")
		       (find "sl")))))
	    (cond ((not pathname*)
		   (nsf))
		  ((ignore-errors (lambda () (load pathname*)))
		   => (lambda (condition)
			(if errors?
			    (signal-condition condition)
			    condition)))
		  (else
		   (set! loaded-object-files
			 (cons pathname loaded-object-files))
		   #t)))))))

(define (reset-loaded-object-files!)
  (set! loaded-object-files '())
  unspecific)

(define (loading-message suppress-loading-message? pathname do-it)
  (if suppress-loading-message?
      (do-it)
      (let ((port (notification-output-port)))
	(fresh-line port)
	(write-string ";Loading " port)
	(write (enough-namestring pathname) port)
	(let ((value (do-it)))
	  (write-string " -- done" port)
	  (newline port)
	  value))))

(define *purification-root-marker*)

(define (load/purification-root object)
  (or (and (comment? object)
	   (let ((text (comment-text object)))
	     (and (dbg-info-vector? text)
		  (dbg-info-vector/purification-root text))))
      (and (object-type? (ucode-type compiled-entry) object)
	   (let* ((block ((ucode-primitive compiled-code-address->block 1)
			  object))
		  (index (- (system-vector-length block) 3)))
	     (and (not (negative? index))
		  (let ((frob (system-vector-ref block index)))
		    (and (pair? frob)
			 (eq? (car frob) *purification-root-marker*)
			 (cdr frob))))))
      object))

(define (read-file filename)
  (call-with-input-file (pathname-default-version filename 'NEWEST)
    (lambda (port)
      (stream->list (read-stream port)))))

(define (read-stream port)
  (parse-objects port
		 (current-parser-table)
		 (lambda (object)
		   (and (eof-object? object)
			(begin
			  (close-input-port port)
			  #t)))))

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
		    (cons s-expression
			  (hook/repl-eval #f
					  s-expression
					  environment
					  syntax-table)))))))

(define (write-stream stream write)
  (if (stream-pair? stream)
      (let loop ((exp&value (stream-car stream)) (stream (stream-cdr stream)))
	(if (stream-pair? stream)
	    (begin
	      (write exp&value)
	      (loop (stream-car stream) (stream-cdr stream)))
	    (cdr exp&value)))
      unspecific))

;;;; Command Line Parser

(define (process-command-line)
  (set! generate-suspend-file? #f)
  (hook/process-command-line ((ucode-primitive get-unused-command-line 0))))

(define hook/process-command-line)

(define *unused-command-line*)
(define *command-line-parsers* '())

(define *load-init-file?*)

(define (default/process-command-line unused-command-line)
  (let ((after-parsing-actions '()))

    (define (process-keyword command-line unused-options)
      (if (not (null? command-line))
	  (let* ((keyword (car command-line))
		 (place (assoc keyword *command-line-parsers*)))
	    (cond (place
		   (call-with-values
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
	  (fluid-let ((*load-init-file?* #t))
	    (set! *unused-command-line*
		  (process-keyword (vector->list unused-command-line) '()))
	    (for-each (lambda (act) (act))
		      (reverse after-parsing-actions))
	    (if *load-init-file?* (load-init-file)))))))

(define (load-init-file)
  (let ((pathname (init-file-pathname)))
    (if pathname
	(load pathname user-initial-environment)))
  unspecific)

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
  (if (not (and (string? keyword)
		(>= (string-length keyword) 2)
		(char=? #\- (string-ref keyword 0))))
      (error:wrong-type-argument keyword
				 "command-line option keyword"
				 'SET-COMMAND-LINE-PARSER!))
  (let ((place (assoc keyword *command-line-parsers*)))
    (if place
	(set-cdr! place proc)
	(begin
	  (set! *command-line-parsers*
		(cons (cons keyword proc)
		      *command-line-parsers*))
	  unspecific))))

(define (simple-command-line-parser keyword thunk)
  (set-command-line-parser! keyword
			    (lambda (command-line)
			      (values (cdr command-line) thunk))))

;; Upwards compatibility.
(define simple-option-parser simple-command-line-parser)

(define (argument-command-line-parser keyword multiple? procedure)
  (set-command-line-parser!
   keyword
   (if multiple?
       (lambda (command-line)
	 (for-each-non-keyword (cdr command-line) procedure))
       (lambda (command-line)
	 (if (null? (cdr command-line))
	     (values '()
		     (lambda ()
		       (warn "Missing argument to command-line option:"
			     keyword)))
	     (values (cddr command-line)
		     (lambda () (procedure (cadr command-line)))))))))

(define (for-each-non-keyword command-line processor)
  (let ((end
	 (lambda (command-line accum)
	   (if (null? accum)
	       (values command-line #f)
	       (let ((objects (reverse accum)))
		 (values command-line
			 (lambda () (for-each processor objects))))))))
    (let loop ((command-line command-line) (accum '()))
      (if (null? command-line)
	  (end '() accum)
	  (let ((next (car command-line)))
	    (if (and (> (string-length next) 0)
		     (char=? #\- (string-ref next 0)))
		(end command-line accum)
		(loop (cdr command-line) (cons next accum))))))))

(define (initialize-command-line-parsers)
  (simple-command-line-parser "-no-init-file"
			      (lambda ()
				(set! *load-init-file?* #f)
				unspecific))
  (set! generate-suspend-file? #f)
  (simple-command-line-parser "-suspend-file"
			      (lambda ()
				(set! generate-suspend-file? #t)
				unspecific))
  (simple-command-line-parser "-no-suspend-file"
			      (lambda ()
				(set! generate-suspend-file? #f)
				unspecific))
  (argument-command-line-parser "-load" #t load)
  (argument-command-line-parser "-eval" #t
				(lambda (arg)
				  (eval (with-input-from-string arg read)
					user-initial-environment))))

;;;; Loader for packed binaries

(define (load-packed-binaries pathname fname count environment)
  (define (process-bunch alist)
    (let ((real-load load)
	  (real-fasload fasload)
	  (real-file-exists? file-exists?)
	  (real-file-directory? file-directory?)
	  (to-purify '()))
      (fluid-let
	  ((load
	    (lambda (fname #!optional env syntax-table purify?)
	      (let ((env (if (default-object? env) default-object env))
		    (purify?
		     (if (default-object? purify?) default-object purify?)))
		(let ((place (find-filename fname alist)))
		  (if (not place)
		      (real-load fname
				 env
				 (if (default-object? syntax-table)
				     default-object
				     syntax-table)
				 purify?)
		      (handle-load-hooks
		       (lambda ()
			 (let ((scode (caddr place)))
			   (loading-message fname
					    load/suppress-loading-message?
					    ";Pseudo-loading ")
			   (if (and (not (eq? purify? default-object)) purify?)
			       (set! to-purify
				     (cons (load/purification-root scode)
					   to-purify)))
			   (fluid-let ((load/current-pathname (cadr place)))
			     (extended-scode-eval scode
						  (if (eq? env default-object)
						      environment
						      env)))))))))))
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
		  (real-file-directory? dname)))))
        (load (caar alist)))
      (set! alist)
      (for-each purify (reverse! to-purify)))
    (flush-purification-queue!))

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

  (define (search-alist path alist predicate?)
    (let loop ((alist alist))
      (and (not (null? alist))
	   (if (predicate? path (cadar alist))
	       (car alist)
	       (loop (cdr alist))))))

  (define (loading-message fname suppress? kind)
    (if (not suppress?)
	(let ((port (notification-output-port)))
	  (fresh-line port)
	  (write-string kind port)
	  (write-string (->namestring (->pathname fname)) port)
	  (write-string "..." port)
	  (newline port))))

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

(define (with-binary-input-file file action)
  (with-binary-file-channel file action
    open-binary-input-file
    port/input-channel
    'with-binary-input-file))

(define (with-binary-file-channel file action open extract-channel name)
  (let ((port #f))
    (dynamic-wind
     (lambda ()
       (if port
           (error "cannot re-enter with-binary-file-channel" name)))
     (lambda ()
       (set! port (open file))
       (action (channel-descriptor (extract-channel port))))
     (lambda ()
       (if (and port
                (not (eq? port #t)))
           (begin
             (close-port port)
             (set! port #t)))))))