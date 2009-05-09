#| -*-Scheme-*-

$Id: load.scm,v 14.108 2009/04/25 23:47:08 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
  (set! condition-type:not-loading
	(make-condition-type 'NOT-LOADING condition-type:error '()
	  "No file being loaded."))
  (initialize-command-line-parsers)
  (set! hook/process-command-line default/process-command-line)
  (add-event-receiver! event:after-restart process-command-line))

(define load/loading? #f)
(define load/after-load-hooks)
(define load/suppress-loading-message? #f)
(define *eval-unit* #f)
(define *current-load-environment* 'NONE)
(define *write-notifications?* #t)

(define *purification-root-marker*)
(define condition-type:not-loading)

;; Obsolete and ignored:
(define load-noisily? #f)

(define (load pathname #!optional environment syntax-table purify?)
  syntax-table				;ignored
  (let ((environment
	 (if (default-object? environment)
	     (current-load-environment)
	     (->environment environment)))
	(purify?
	 (if (default-object? purify?)
	     #f
	     purify?)))
    (handle-load-hooks
     (lambda ()
       (if (list? pathname)
	   (for-each (lambda (pathname)
		       (load-1 pathname environment purify?))
		     pathname)
	   (load-1 pathname environment purify?))))))

(define (load-1 pathname environment purify?)
  (receive (pathname* loader notifier) (choose-load-method pathname)
    (if pathname*
	(maybe-notify load/suppress-loading-message?
		      (loader environment purify?)
		      notifier)
	(load-failure load-1 pathname environment purify?))))

(define (file-loadable? pathname)
  (receive (pathname* loader notifier) (choose-load-method pathname)
    loader notifier
    (if pathname* #t #f)))

(define (choose-load-method pathname)
  (let ((pathname (merge-pathnames pathname)))
    (receive (pathname* loader notifier) (choose-fasload-method pathname)
      (if pathname*
	  (values pathname*
		  (wrap-loader pathname (fasloader->loader loader))
		  notifier)
	  (let ((pathname*
		 (if (file-regular? pathname)
		     pathname
		     (let ((pathname (pathname-default-type pathname "scm")))
		       (and (file-regular? pathname)
			    pathname)))))
	    (if pathname*
		(values pathname*
			(wrap-loader pathname* (source-loader pathname*))
			(loading-notifier pathname*))
		(values #f #f #f)))))))

(define (fasloader->loader loader)
  (lambda (environment purify?)
    (let ((scode (loader)))
      (if purify? (purify (load/purification-root scode)))
      (extended-scode-eval scode environment))))

(define (source-loader pathname)
  (lambda (environment purify?)
    purify?
    (call-with-input-file pathname
      (lambda (port)
	(let loop ((value unspecific))
	  (let ((sexp (read port environment)))
	    (if (eof-object? sexp)
		value
		(loop (repl-eval sexp environment)))))))))

(define (wrap-loader pathname loader)
  (lambda (environment purify?)
    (lambda ()
      (with-load-environment environment
	(lambda ()
	  (with-eval-unit (pathname->uri pathname)
	    (lambda ()
	      (loader environment purify?))))))))

(define (fasload pathname #!optional suppress-notifications?)
  (receive (pathname* loader notifier) (choose-fasload-method pathname)
    (if pathname*
	(maybe-notify suppress-notifications? loader notifier)
	(load-failure fasload pathname suppress-notifications?))))

(define (file-fasloadable? pathname)
  (receive (pathname* loader notifier) (choose-fasload-method pathname)
    loader notifier
    (if pathname* #t #f)))

(define (choose-fasload-method pathname)
  (let* ((pathname (merge-pathnames pathname))
	 (thunk
	  (if (pathname-type pathname)
	      (or (try-fasl-file pathname)
		  (try-object-file pathname))
	      (or (try-fasl-file pathname)
		  (try-fasl-file (pathname-new-type pathname "com"))
		  (try-object-file (pathname-new-type pathname "so"))
		  (try-fasl-file (pathname-new-type pathname "bin"))))))
    (if thunk
	(receive (pathname loader notifier) (thunk)
	  (values pathname
		  (lambda ()
		    (let ((object (loader)))
		      (fasload/update-debugging-info! object pathname)
		      object))
		  notifier))
	(values #f #f #f))))

(define (try-fasl-file pathname)
  (and (fasl-file? pathname)
       (lambda ()
	 (values pathname
		 (lambda ()
		   ((ucode-primitive binary-fasload)
		    (->namestring pathname)))
		 (let ((notifier (loading-notifier pathname)))
		   (lambda (thunk)
		     (if (file-modification-time<?
			  pathname
			  (pathname-new-type pathname "scm"))
			 (warn "Source file newer than binary:" pathname))
		     (notifier thunk)))))))

(define (try-object-file pathname)
  (let ((object (built-in-object-file pathname)))
    (if object
	(lambda ()
	  (values pathname
		  (lambda () object)
		  (init-notifier pathname)))
	(and (object-file? pathname)
	     (lambda ()
	       (values pathname
		       (lambda () (fasload-object-file pathname))
		       (loading-notifier pathname)))))))

(define (fasl-file? pathname)
  (and (file-regular? pathname)
       (call-with-binary-input-file pathname
	 (lambda (port)
	   (let ((n (vector-ref (gc-space-status) 0)))
	     (let ((marker (make-string n)))
	       (and (eqv? (read-string! marker port) n)
		    (let loop ((i 0))
		      (if (fix:< i n)
			  (and (fix:= (vector-8b-ref marker i) #xFA)
			       (loop (fix:+ i 1)))
			  #t)))))))))

(define (object-file? pathname)
  (and (let ((type (pathname-type pathname)))
	 (and (string? type)
	      (string=? type "so")))
       (file-regular? pathname)))

(define (load/purification-root object)
  (or (and (comment? object)
	   (let ((text (comment-text object)))
	     (and (dbg-info-vector? text)
		  (dbg-info-vector/purification-root text))))
      (and (object-type? (ucode-type compiled-entry) object)
	   (let* ((block ((ucode-primitive compiled-code-address->block 1)
			  object))
		  (index (fix:- (system-vector-length block) 3)))
	     (and (fix:>= index 0)
		  (let ((frob (system-vector-ref block index)))
		    (and (pair? frob)
			 (eq? (car frob) *purification-root-marker*)
			 (cdr frob))))))
      object))

(define (maybe-notify suppress-notifications? loader notifier)
  (let ((notify?
	 (if (if (default-object? suppress-notifications?)
		 load/suppress-loading-message?
		 suppress-notifications?)
	     #f
	     *write-notifications?*)))
    (fluid-let ((*write-notifications?* notify?))
      (if notify?
	  (notifier loader)
	  (loader)))))

(define (loading-notifier pathname)
  (lambda (thunk)
    (with-notification (lambda (port)
			 (write-string "Loading " port)
			 (write (enough-namestring pathname) port))
      thunk)))

(define (init-notifier pathname)
  (lambda (thunk)
    (write-notification-line
     (lambda (port)
       (write-string "Initialized " port)
       (write (enough-namestring pathname) port)))
    (thunk)))

(define (with-eval-unit uri thunk)
  (fluid-let ((*eval-unit* (->absolute-uri uri 'WITH-EVAL-UNIT)))
    (thunk)))

(define (current-eval-unit #!optional error?)
  (let ((unit *eval-unit*))
    (if (and (not unit)
	     (if (default-object? error?) #t error?))
	(error condition-type:not-loading))
    unit))

(define (current-load-pathname)
  (or (uri->pathname (current-eval-unit) #f)
      (error condition-type:not-loading)))

(define (current-load-environment)
  (let ((env *current-load-environment*))
    (if (eq? env 'NONE)
	(nearest-repl/environment)
	env)))

(define (set-load-environment! environment)
  (guarantee-environment environment 'SET-LOAD-ENVIRONMENT!)
  (if (not (eq? *current-load-environment* 'NONE))
      (begin
	(set! *current-load-environment* environment)
	unspecific)))

(define (with-load-environment environment thunk)
  (guarantee-environment environment 'WITH-LOAD-ENVIRONMENT)
  (fluid-let ((*current-load-environment* environment))
    (thunk)))

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

(define (load-failure procedure pathname . arguments)
  (apply procedure
	 (error:file-operation pathname
			       "find" "file" "file does not exist"
			       procedure
			       (cons pathname arguments))
	 arguments))

(define (fasload-object-file pathname)
  (let ((pathname (object-file-pathname pathname)))
    (let ((handle (dld-load-file pathname))
	  (uri (pathname->standard-uri pathname)))
      (let ((nonce (liarc-object-file-nonce handle)))
	(if nonce
	    (register-liarc-object-file uri nonce)))
      (initialize-object-file handle uri))))

(define (register-liarc-object-file uri nonce)
  (add-event-receiver!
   event:after-restore
   (lambda ()
     (let ((handle (dld-load-file (standard-uri->pathname uri))))
       (let ((nonce* (liarc-object-file-nonce handle)))
	 (if (not (and nonce* (string=? nonce* nonce)))
	     (begin
	       (dld-unload-file handle)
	       (error "Can't restore liarc object file:" uri))))
       (initialize-object-file handle uri)))))

(define (liarc-object-file-nonce handle)
  (let ((nonce
	 (ignore-errors
	  (lambda ()
	    ((ucode-primitive address-to-string 1)
	     (dld-lookup-symbol handle "dload_nonce"))))))
    (and (string? nonce)
	 nonce)))

(define (initialize-object-file handle uri)
  ((ucode-primitive initialize-c-compiled-block 1)
   ((ucode-primitive address-to-string 1)
    ((ucode-primitive initialize-liarc-object-file 2)
     (dld-lookup-symbol handle "dload_initialize_file")
     (object-file-prefix uri)))))

(define (object-file-prefix uri)
  (uri->string
   (let ((pathname (uri->pathname uri #f)))
     (if pathname
	 (pathname->uri
	  (directory-pathname pathname))
	 ;; This kludge has far too much knowledge of the URI
	 ;; argument.  It's an expedient to work around the lack of
	 ;; URI comparison operations.
	 (make-uri (uri-scheme uri)
		   (uri-authority uri)
		   (let ((path (uri-path uri)))
		     (let ((p (except-last-pair path))
			   (s (last path)))
		       (append
			(except-last-pair p)
			(if (and (equal? p
					 '("" "software" "mit-scheme"
					      "lib" "lib"))
				 (string-suffix? ".so" s))
			    (list (string-head s (fix:- (string-length s) 3)))
			    '())
			(list ""))))
		   #f
		   #f)))))

(define (built-in-object-file pathname)
  ((ucode-primitive initialize-c-compiled-block 1)
   (uri->string (pathname->standard-uri (object-file-pathname pathname)))))

(define (object-file-pathname pathname)
  (pathname-default-type (pathname-simplify (merge-pathnames pathname))
			 "so"))

(define (load-library-object-file name errors?)
  (let ((pathname
	 (merge-pathnames (pathname-new-type name "so")
			  (system-library-directory-pathname "lib"))))
    (if (and errors? (not (file-regular? pathname)))
	(error "No library object file of this name:" name))
    (if (dld-loaded-file? pathname)
	#t
	(let ((load-it (lambda () (load pathname))))
	  (if errors?
	      (load-it)
	      (ignore-errors load-it))))))

(define (with-loader-base-uri uri thunk)
  (let ((directory (directory-pathname (current-load-pathname))))
    (with-working-directory-pathname directory
      (lambda ()
	(let ((path
	       (let ((lib (system-library-uri))
		     (trim-path
		      (lambda (uri)
			(reverse! (let ((rp (reverse (uri-path uri))))
				    (if (and (pair? rp)
					     (string-null? (car rp)))
					(cdr rp)
					rp))))))
		 (and (eq? (uri-scheme uri) (uri-scheme lib))
		      (uri-authority=? (uri-authority uri) (uri-authority lib))
		      (equal? (uri-query uri) (uri-query lib))
		      (equal? (uri-fragment uri) (uri-fragment lib))
		      (let loop ((pu (trim-path uri)) (pl (trim-path lib)))
			(if (pair? pl)
			    (and (pair? pu)
				 (string=? (car pu) (car pl))
				 (loop (cdr pu) (cdr pl)))
			    (make-pathname #f #f (cons 'RELATIVE pu)
					   #f #f #f)))))))
	  (if path
	      (with-directory-rewriting-rule directory path thunk)
	      (thunk)))))))

(define (pathname->standard-uri pathname)
  (let ((uri
	 (pathname->uri
	  (enough-pathname pathname (system-library-directory-pathname)))))
    (if (uri-absolute? uri)
	uri
	(system-library-uri uri))))

(define (standard-uri->pathname uri)
  (or (uri->pathname uri #f)
      (merge-pathnames
       (uri->pathname (make-uri #f #f (list-tail (uri-path uri) 4) #f #f))
       (system-library-directory-pathname))))

(define (system-uri #!optional rel-uri)
  (if (string? system-base-uri)
      (begin
	(set! system-base-uri (string->uri system-base-uri))
	unspecific))
  (maybe-merge rel-uri system-base-uri 'SYSTEM-URI))

(define system-base-uri "http://www.gnu.org/software/mit-scheme/")

(define (system-library-uri #!optional rel-uri)
  (maybe-merge rel-uri (system-uri "lib/") 'SYSTEM-LIBRARY-URI))

(define (maybe-merge rel-uri base-uri caller)
  (if (default-object? rel-uri)
      base-uri
      (merge-uris (->relative-uri rel-uri caller) base-uri)))

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
	 (cddr entry))))

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

(define (set-command-line-parser! keyword proc #!optional description)
  (guarantee-string keyword 'SET-COMMAND-LINE-PARSER!)
  (let ((keyword (strip-leading-hyphens keyword))
	(desc (if (default-object? description)
		  ""
		  (begin
		    (guarantee-string description 'SET-COMMAND-LINE-PARSER!)
		    description))))
    (if (string-null? keyword)
	(error:bad-range-argument keyword 'SET-COMMAND-LINE-PARSER!))
    (let ((place (assoc keyword *command-line-parsers*)))
      (if place
	  (begin
	    (set-car! (cdr place) desc)
	    (set-cdr! (cdr place) proc))
	  (begin
	    (set! *command-line-parsers*
		  (cons (cons* keyword desc proc)
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

(define (command-line-option-description keyword-line description-lines caller)
  (if (pair? description-lines)
      (if (and (null? (cdr description-lines))
	       (not (car description-lines)))
	  ""
	  (begin
	    (for-each (lambda (description-line)
			(guarantee-string description-line caller))
		      description-lines)
	    (decorated-string-append "" "\n  " ""
				     (cons keyword-line description-lines))))
      (string-append keyword-line "\n  (No description.)")))

(define (simple-command-line-parser keyword thunk . description-lines)
  (guarantee-string keyword 'SIMPLE-COMMAND-LINE-PARSER)
  (set-command-line-parser! keyword
    (lambda (command-line)
      (values (cdr command-line) thunk))
    (command-line-option-description
     (string-append "--" keyword)
     description-lines
     'SIMPLE-COMMAND-LINE-PARSER)))

;; Upwards compatibility.
(define simple-option-parser simple-command-line-parser)

(define (argument-command-line-parser keyword multiple? procedure
				      . description-lines)
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
			      (string-append "--" keyword)))))))
    (command-line-option-description
     (string-append "--" keyword " ARG" (if multiple? " ..." ""))
     description-lines
     'ARGUMENT-COMMAND-LINE-PARSER)))

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

(define (show-command-line-options)
  (write-string "

ADDITIONAL OPTIONS supported by this band:\n")
  (do ((parsers (sort *command-line-parsers*
		      (lambda (a b) (string<? (car a) (car b))))
		(cdr parsers)))
      ((null? parsers))
    (let ((description (cadar parsers)))
      (if (not (string-null? description))
	  (begin
	    (newline)
	    (write-string description)
	    (newline)))))
  (%exit 0))

(define (initialize-command-line-parsers)
  (set! *command-line-parsers* '())
  (simple-command-line-parser "no-init-file"
    (lambda ()
      (set! *load-init-file?* #f)
      unspecific)
    "Inhibits automatic loading of the ~/.scheme.init file.")
  (set! generate-suspend-file? #f)
  (simple-command-line-parser "suspend-file"
    (lambda ()
      (set! generate-suspend-file? #t)
      unspecific)
    "If specified, Scheme saves a band to ~/scheme_suspend on reception"
    "of some signals.  This is unavailable on some operating systems."
    "Under Unix, this is triggered by SIGUSR1 and SIGPWR, and also, if"
    "Scheme is not running under Emacs, SIGHUP.")
  (simple-command-line-parser "no-suspend-file"
    (lambda ()
      (set! generate-suspend-file? #f)
      unspecific)
    "Inhibits automatic saving of bands to ~/scheme_suspend.")
  (argument-command-line-parser "load" #t
    (lambda (arg)
      (run-in-nearest-repl
       (lambda (repl)
	 (fluid-let ((load/suppress-loading-message? (cmdl/batch-mode? repl)))
	   (load arg (repl/environment repl))))))
    "Loads the argument files as if in the REPL."
    "In batch mode, loading messages are suppressed.")
  (argument-command-line-parser "eval" #t
    (lambda (arg)
      (run-in-nearest-repl
       (lambda (repl)
	 (let ((environment (repl/environment repl)))
	   (repl-eval/write (read (open-input-string arg)
				  environment)
			    environment
			    repl)))))
    "Evaluates the argument expressions as if in the REPL.")
  (simple-command-line-parser "help" show-command-line-options #f)
  (simple-command-line-parser "version" (lambda () (%exit 0)) #f))