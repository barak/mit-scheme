#| -*-Scheme-*-

$Id: load.scm,v 14.91 2007/04/15 15:50:34 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Code Loader
;;; package: (runtime load)

(declare (usual-integrations))

(define (initialize-package!)
  (set! *purification-root-marker* (intern "#[PURIFICATION-ROOT]"))
  (set! load-noisily? #f)
  (set! load/loading? #f)
  (set! load/suppress-loading-message? #f)
  (set! load/default-types
	`((#f ,wrapper/load/built-in)
	  ("so" ,load-object-file)
	  ("sl" ,load-object-file)
	  ("dylib" ,load-object-file)
	  ("com" ,load/internal)
	  ("bin" ,load/internal)
	  ("scm" ,load/internal)))
  (set! fasload/default-types
	`((#f ,wrapper/fasload/built-in)
	  ("so" ,fasload-object-file)
	  ("sl" ,fasload-object-file)
	  ("dylib" ,fasload-object-file)
	  ("com" ,fasload/internal)
	  ("bin" ,fasload/internal)))
  (set! load/default-find-pathname-with-type search-types-in-order)
  (set! *eval-unit* #f)
  (set! *current-load-environment* 'NONE)
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
(define *eval-unit*)
(define *current-load-environment*)
(define condition-type:not-loading)
(define load/default-find-pathname-with-type)
(define fasload/default-types)
(define loaded-object-files)

;;; This is careful to do the minimum number of file existence probes
;;; before opening the input file.

(define (load filename/s #!optional environment syntax-table purify?)
  syntax-table				;ignored
  (let ((environment
	 (if (default-object? environment)
	     (if (eq? *current-load-environment* 'NONE)
		 (nearest-repl/environment)
		 *current-load-environment*)
	     (->environment environment)))
	(purify?
	 (if (default-object? purify?)
	     #f
	     purify?)))
    (fluid-let ((*current-load-environment* environment))
      (handle-load-hooks
       (lambda ()
	 (let ((kernel
		(lambda (filename last-file?)
		  (receive (pathname loader)
		      (find-pathname filename load/default-types)
		    (with-eval-unit (pathname->uri pathname)
		      (lambda ()
			(let ((load-it
			       (lambda ()
				 (loader pathname
					 environment
					 purify?
					 load-noisily?))))
			  (cond (last-file? (load-it))
				(load-noisily? (write-line (load-it)))
				(else (load-it) unspecific)))))))))
	   (if (pair? filename/s)
	       (let loop ((filenames filename/s))
		 (if (pair? (cdr filenames))
		     (begin
		       (kernel (car filenames) #f)
		       (loop (cdr filenames)))
		     (kernel (car filenames) #t)))
	       (kernel filename/s #t))))))))

(define (fasload filename #!optional suppress-loading-message?)
  (receive (pathname loader)
      (find-pathname filename fasload/default-types)
    (loader pathname
	    (if (default-object? suppress-loading-message?)
		load/suppress-loading-message?
		suppress-loading-message?))))

(define (current-eval-unit #!optional error?)
  (or *eval-unit*
      (begin
	(if error? (error condition-type:not-loading))
	#f)))

(define (with-eval-unit uri thunk)
  (fluid-let ((*eval-unit* (->absolute-uri uri 'WITH-EVAL-UNIT)))
    (thunk)))

(define (current-load-pathname)
  (or (uri->pathname (current-eval-unit) #f)
      (error condition-type:not-loading)))

(define (load/push-hook! hook)
  (if (not load/loading?) (error condition-type:not-loading))
  (set! load/after-load-hooks (cons hook load/after-load-hooks))
  unspecific)

(define (handle-load-hooks thunk)
  (receive (result hooks)
      (fluid-let ((load/loading? #t)
		  (load/after-load-hooks '()))
	(let ((result (thunk)))
	  (values result (reverse load/after-load-hooks))))
    (for-each (lambda (hook) (hook)) hooks)
    result))

(define (load-noisily filename #!optional environment syntax-table purify?)
  (fluid-let ((load-noisily? #t))
    (load filename environment syntax-table purify?)))

(define (load-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply load args)))

(define (fasload-latest . args)
  (fluid-let ((load/default-find-pathname-with-type find-latest-file))
    (apply fasload args)))

(define (find-pathname filename default-types)
  (let ((pathname (merge-pathnames filename))
	(find-loader
	 (lambda (extension)
	   (let ((place (assoc extension default-types)))
	     (and place
		  (cadr place)))))
	(fail
	 (lambda ()
	   (find-pathname (error:file-operation filename
						"find"
						"file"
						"file does not exist"
						find-pathname
						(list filename default-types))
			  default-types))))
    (cond ((built-in-object-file pathname)
	   => (lambda (value)
		(values pathname
			((find-loader #f) value))))
	  ((file-regular? pathname)
	   (values pathname
		   (or (and (pathname-type pathname)
			    (find-loader (pathname-type pathname)))
		       (and (fasl-file? pathname)
			    (find-loader "bin"))
		       (find-loader "scm"))))
	  ((pathname-type pathname)
	   (fail))
	  (else
	   (receive (pathname loader)
	       (load/default-find-pathname-with-type pathname default-types)
	     (if (not pathname)
		 (fail)
		 (values pathname loader)))))))

(define (search-types-in-order pathname default-types)
  (let loop ((types default-types))
    (cond ((not (pair? types))
	   (values #f #f))
	  ((not (caar types))
	   (let ((value (built-in-object-file pathname)))
	     (if value
		 (values pathname ((cadar types) value))
		 (loop (cdr types)))))
	  (else
	   (let ((pathname (pathname-new-type pathname (caar types))))
	     (if (file-regular? pathname)
		 (values pathname (cadar types))
		 (loop (cdr types))))))))

;; This always considers a built-in to be the newest.

(define (find-latest-file pathname default-types)
  (let loop ((types default-types)
	     (latest-pathname #f)
	     (latest-loader #f)
	     (latest-time 0))
    (cond ((not (pair? types))
	   (values latest-pathname latest-loader))
	  ((not (caar types))
	   (let ((value (built-in-object-file pathname)))
	     (if value
		 (values pathname ((cadar types) value))
		 (loop (cdr types)
		       latest-pathname
		       latest-loader
		       latest-time))))
	  (else
	   (let ((pathname (pathname-new-type pathname (caar types))))
	     (let ((time (file-modification-time-indirect pathname)))
	       (if (and time (> time latest-time))
		   (loop (cdr types) pathname (cadar types) time)
		   (loop (cdr types)
			 latest-pathname
			 latest-loader
			 latest-time))))))))

(define (load/internal pathname environment purify? load-noisily?)
  (if (fasl-file? pathname)
      (load-scode-end (fasload/internal pathname
					load/suppress-loading-message?)
		      environment
		      purify?)
      (call-with-input-file pathname
	(lambda (port)
	  (let ((value-stream
		 (lambda ()
		   (eval-stream (read-stream port environment) environment))))
	    (if load-noisily?
		(write-stream (value-stream)
			      (lambda (exp&value)
				(repl-write (cdr exp&value) (car exp&value))))
		(with-loading-message pathname
		  (lambda ()
		    (write-stream (value-stream)
				  (lambda (exp&value) exp&value #f))))))))))

(define (fasload/internal pathname suppress-loading-message?)
  (let ((namestring (->namestring pathname)))
    (if (and (not suppress-loading-message?)
	     (file-modification-time<? pathname
				       (pathname-new-type pathname "scm")))
	(warn "Source file newer than binary:" namestring))
    (let ((value
	   (with-loading-message pathname
	     (lambda ()
	       ((ucode-primitive binary-fasload) namestring))
	     suppress-loading-message?)))
      (fasload/update-debugging-info! value pathname)
      value)))

(define (fasload-object-file pathname suppress-loading-message?)
  (with-loading-message pathname
    (lambda ()
      (let ((scode (fasload-liarc-object-file pathname)))
	(fasload/update-debugging-info! scode pathname)
	scode))
    suppress-loading-message?))

(define (fasload-liarc-object-file pathname)
  (let* ((handle ((ucode-primitive load-object-file 1)
		  (->namestring pathname)))
	 (cth ((ucode-primitive object-lookup-symbol 3)
	       handle "dload_initialize_file" 0)))
    (if (not cth)
	(error "Cannot find init procedure:" pathname))
    ((ucode-primitive initialize-c-compiled-block 1)
     ((ucode-primitive address-to-string 1)
      ((ucode-primitive invoke-c-thunk 1)
       cth)))))

(define (built-in-object-file pathname)
  (let ((handle (liarc-object-pathname->handle pathname)))
    (and handle
	 ((ucode-primitive initialize-c-compiled-block 1) handle))))

(define (liarc-object-pathname->handle pathname)
  (let ((pathname (merge-pathnames pathname)))
    (let ((d (pathname-directory pathname))
	  (n (pathname-name pathname))
	  (t (pathname-type pathname)))
      (and (pair? d)
	   (let ((tail (last d)))
	     (and (string? tail)	;Doesn't handle UP ("..").
		  (string-append tail "_" n
				 (cond ((not t) ".so")
				       ((string? t) (string-append "." t))
				       (else "")))))))))

(define (wrapper/fasload/built-in value)
  (lambda (pathname suppress-loading-message?)
    (with-loading-message pathname
      (lambda ()
	(fasload/update-debugging-info! value pathname)
	value)
      suppress-loading-message?)))

(define (load-object-file pathname environment purify? load-noisily?)
  load-noisily?		; ignored
  (load-scode-end
   (fasload-object-file pathname load/suppress-loading-message?)
   environment
   purify?))

(define (wrapper/load/built-in scode)
  (lambda (pathname environment purify? load-noisily?)
    load-noisily?			; ignored
    (with-loading-message pathname
      (lambda ()
	(fasload/update-debugging-info! scode pathname)
	(load-scode-end scode environment purify?)))))

(define (load-scode-end scode environment purify?)
  (if purify? (purify (load/purification-root scode)))
  (extended-scode-eval scode
		       (if (default-object? environment)
			   (nearest-repl/environment)
			   environment)))

(define (load-library-object-file name errors? #!optional noisy?)
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
			    (and (file-regular? pathname)
				 pathname)))))
		   (or (find "so")
		       (find "sl")))))
	    (cond ((not pathname*)
		   (nsf))
		  ((ignore-errors
		    (lambda ()
		      (fluid-let ((load/suppress-loading-message?
				   (if (default-object? noisy?) #f noisy?)))
			(load pathname*))))
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

(define (with-loading-message pathname thunk #!optional suppress-message?)
  (if (if (default-object? suppress-message?)
	  load/suppress-loading-message?
	  suppress-message?)
      (thunk)
      (with-notification (lambda (port)
			   (write-string "Loading " port)
			   (write (enough-namestring pathname) port))
	thunk)))

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

(define (read-file filename #!optional environment)
  (call-with-input-file (pathname-default-version filename 'NEWEST)
    (lambda (port)
      (stream->list (read-stream port environment)))))

(define (read-stream port environment)
  (parse-objects port
		 environment
		 (lambda (object)
		   (and (eof-object? object)
			(begin
			  (close-input-port port)
			  #t)))))

(define (eval-stream stream environment)
  (stream-map stream
	      (lambda (s-expression)
		(cons s-expression
		      (repl-eval s-expression environment)))))

(define (write-stream stream write)
  (if (stream-pair? stream)
      (let loop ((exp&value (stream-car stream)) (stream (stream-cdr stream)))
	(if (stream-pair? stream)
	    (begin
	      (write exp&value)
	      (loop (stream-car stream) (stream-cdr stream)))
	    (cdr exp&value)))
      unspecific))

(define (fasl-file? pathname)
  (call-with-binary-input-file pathname
    (lambda (port)
      (let ((n (vector-ref (gc-space-status) 0)))
	(let ((marker (make-string n)))
	  (and (eqv? (read-string! marker port) n)
	       (let loop ((i 0))
		 (if (fix:< i n)
		     (and (fix:= (vector-8b-ref marker i) #xFA)
			  (loop (fix:+ i 1)))
		     #t))))))))

;;;; Command Line Parser

(define (process-command-line)
  (set! generate-suspend-file? #f)
  (hook/process-command-line ((ucode-primitive get-unused-command-line 0))))

(define *unused-command-line*)
(define *command-line-parsers*)
(define *load-init-file?*)

(define hook/process-command-line)
(define (default/process-command-line unused-command-line)
  (let ((after-parsing-actions '()))

    (define (process-keyword command-line unused)
      (if (pair? command-line)
	  (let ((keyword (car command-line)))
	    (if (option-keyword? keyword)
		(let ((parser (find-keyword-parser keyword)))
		  (if parser
		      (receive (next tail-action) (parser command-line)
			(if tail-action
			    (set! after-parsing-actions
				  (cons tail-action after-parsing-actions)))
			(process-keyword next unused))
		      (find-next-keyword command-line unused)))
		(begin
		  (warn "Invalid keyword:" keyword)
		  (find-next-keyword command-line unused))))
	  (done unused)))

    (define (find-next-keyword command-line unused)
      (let ((unused (cons (car command-line) unused))
	    (command-line (cdr command-line)))
	(if (pair? command-line)
	    (if (option-keyword? (car command-line))
		(process-keyword command-line unused)
		(find-next-keyword command-line unused))
	    (done unused))))

    (define (done unused)
      (let ((unused (reverse! unused)))
	(if (pair? unused)
	    (warn "Unhandled command line options:" unused))
	unused))

    (if unused-command-line
	(begin
	  (set! *unused-command-line*)
	  (fluid-let ((*load-init-file?* #t))
	    (set! *unused-command-line*
		  (process-keyword (vector->list unused-command-line) '()))
	    (for-each (lambda (act) (act))
		      (reverse after-parsing-actions))
	    (if *load-init-file?* (load-init-file))))
	(begin
	  (set! *unused-command-line* #f)
	  (load-init-file)))))

(define (find-keyword-parser keyword)
  (let ((entry (assoc (strip-leading-hyphens keyword) *command-line-parsers*)))
    (and entry
	 (cdr entry))))

(define (option-keyword? argument)
  (and (fix:> (string-length argument) 1)
       (char=? #\- (string-ref argument 0))
       (or (not (char=? #\- (string-ref argument 1)))
	   (and (fix:> (string-length argument) 2)
		(not (char=? #\- (string-ref argument 2)))))))

(define (load-init-file)
  (let ((pathname (init-file-pathname)))
    (if pathname
	(load pathname user-initial-environment)))
  unspecific)

;; KEYWORD must be a string with at least one character.  For
;; backwards compatibility, the string may have a leading hyphen,
;; which is stripped.
;;
;; PROC is a procedure of one argument.  It will be invoked on the
;; list of command line elements extending to the right of the keyword
;; (and including it).
;;
;; PROC returns two values: the sublist starting with the first
;; non-handled command-line element (typically the next keyword), and
;; either #F or a procedure to invoke after the whole command line has
;; been parsed (and the init file loaded).  Thus PROC has the option
;; of executing the appropriate action at parsing time, or delaying it
;; until after the parsing is complete.  The execution of the PROCs
;; (or their associated delayed actions) is strictly left-to-right,
;; with the init file loaded between the end of parsing and the
;; delayed actions.

(define (set-command-line-parser! keyword proc)
  (guarantee-string keyword 'SET-COMMAND-LINE-PARSER!)
  (let ((keyword (strip-leading-hyphens keyword)))
    (if (string-null? keyword)
	(error:bad-range-argument keyword 'SET-COMMAND-LINE-PARSER!))
    (let ((place (assoc keyword *command-line-parsers*)))
      (if place
	  (set-cdr! place proc)
	  (begin
	    (set! *command-line-parsers*
		  (cons (cons keyword proc)
			*command-line-parsers*))
	    unspecific)))))

(define (strip-leading-hyphens keyword)
  (let ((end (string-length keyword)))
    (let loop ((start 0))
      (cond ((and (fix:< start end)
		  (char=? #\- (string-ref keyword start)))
	     (loop (fix:+ start 1)))
	    ((fix:= start 0)
	     keyword)
	    (else
	     (substring keyword start end))))))

(define (simple-command-line-parser keyword thunk)
  (set-command-line-parser! keyword
    (lambda (command-line)
      (values (cdr command-line) thunk))))

;; Upwards compatibility.
(define simple-option-parser simple-command-line-parser)

(define (argument-command-line-parser keyword multiple? procedure)
  (set-command-line-parser! keyword
    (if multiple?
	(lambda (command-line)
	  (for-each-non-keyword (cdr command-line) procedure))
	(lambda (command-line)
	  (if (pair? (cdr command-line))
	      (values (cddr command-line)
		      (lambda () (procedure (cadr command-line))))
	      (values '()
		      (lambda ()
			(warn "Missing argument to command-line option:"
			      (string-append "--" keyword)))))))))

(define (for-each-non-keyword command-line processor)
  (let ((end
	 (lambda (command-line accum)
	   (if (pair? accum)
	       (let ((objects (reverse! accum)))
		 (values command-line
			 (lambda () (for-each processor objects))))
	       (values command-line #f)))))
    (let loop ((command-line command-line) (accum '()))
      (if (pair? command-line)
	  (let ((next (car command-line)))
	    (if (option-keyword? next)
		(end command-line accum)
		(loop (cdr command-line) (cons next accum))))
	  (end '() accum)))))

(define (initialize-command-line-parsers)
  (set! *command-line-parsers* '())
  (simple-command-line-parser "no-init-file"
    (lambda ()
      (set! *load-init-file?* #f)
      unspecific))
  (set! generate-suspend-file? #f)
  (simple-command-line-parser "suspend-file"
    (lambda ()
      (set! generate-suspend-file? #t)
      unspecific))
  (simple-command-line-parser "no-suspend-file"
    (lambda ()
      (set! generate-suspend-file? #f)
      unspecific))
  (argument-command-line-parser "load" #t
    (lambda (arg)
      (run-in-nearest-repl
       (lambda (repl)
	 (load arg (repl/environment repl))))))
  (argument-command-line-parser "eval" #t
    (lambda (arg)
      (run-in-nearest-repl
       (lambda (repl)
	 (let ((environment (repl/environment repl)))
	   (repl-eval/write (read (open-input-string arg)
				  environment)
			    environment
			    repl)))))))