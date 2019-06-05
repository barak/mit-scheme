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

;;;; Code Loader
;;; package: (runtime load)

(declare (usual-integrations))

(define *purification-root-marker*
  '|#[PURIFICATION-ROOT]|)

(define-deferred condition-type:not-loading
  (make-condition-type 'not-loading condition-type:error '()
    "No file being loaded."))

(define-deferred param:after-load-hooks
  (make-settable-parameter '()))

(define-deferred current-load-environment
  (make-general-parameter #!default
			  (lambda (object)
			    (if (default-object? object)
				object
				(guarantee environment? object)))
			  default-parameter-merger
			  (lambda (value)
			    (if (default-object? value)
				(nearest-repl/environment)
				value))
			  #f))

(define-deferred param:eval-unit
  (make-unsettable-parameter #f
    (lambda (value)
      (and value
	   (->absolute-uri value)))))

(define-deferred current-load-pathname
  (make-forwarding-parameter param:eval-unit
    (lambda (pathname)
      (pathname->uri (merge-pathnames pathname)))
    (lambda (eval-unit)
      (let ((pathname (and eval-unit (uri->pathname eval-unit #f))))
	(if (not pathname)
	    (error condition-type:not-loading))
	pathname))))

(define-deferred param:loading?
  (make-unsettable-parameter #f))

(define-deferred param:suppress-loading-message?
  (make-settable-parameter #f))

(define-deferred param:write-notifications?
  (make-unsettable-parameter #t))

;; Backwards compatibility:
(define load/loading? #f)
(define load/suppress-loading-message? #!default)
(define (suppress-loading-message?)
  (if (default-object? load/suppress-loading-message?)
      (param:suppress-loading-message?)
      load/suppress-loading-message?))

(define (load pathname #!optional environment syntax-table purify?)
  (declare (ignore syntax-table))
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
	(maybe-notify (suppress-loading-message?)
		      (loader environment purify?)
		      notifier)
	(load-failure load-1 pathname environment purify?))))

(define (file-loadable? pathname)
  (receive (pathname* loader notifier) (choose-load-method pathname)
    (declare (ignore loader notifier))
    (if pathname* #t #f)))

(define (choose-load-method pathname)
  (let ((pathname (merge-pathnames pathname)))
    (receive (pathname* loader notifier) (choose-fasload-method pathname)
      (if pathname*
	  (values pathname*
		  (wrap-loader pathname (fasloader->loader pathname loader))
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

(define (fasloader->loader pathname loader)
  (lambda (environment purify?)
    (let ((scode (loader)))
      (if purify? (purify (load/purification-root scode)))
      (if (r7rs-scode-file? scode)
	  (eval-r7rs-scode-file scode pathname (current-library-db))
	  (extended-scode-eval scode environment)))))

(define (source-loader pathname)
  (lambda (environment purify?)
    (declare (ignore purify?))
    (let ((source (read-r7rs-source pathname)))
      (if source
	  (eval-r7rs-source source (current-library-db))
	  (call-with-input-file pathname
	    (lambda (port)
	      (let loop ((value unspecific))
		(let ((sexp (read port)))
		  (if (eof-object? sexp)
		      value
		      (loop (repl-eval sexp environment)))))))))))

(define (wrap-loader pathname loader)
  (lambda (environment purify?)
    (lambda ()
      (parameterize ((current-load-pathname pathname)
		     (current-load-environment environment))
	(loader environment purify?)))))

(define (fasload pathname #!optional suppress-notifications?)
  (receive (pathname* loader notifier) (choose-fasload-method pathname)
    (if pathname*
	(maybe-notify suppress-notifications? loader notifier)
	(load-failure fasload pathname suppress-notifications?))))

(define (file-fasloadable? pathname)
  (receive (pathname* loader notifier) (choose-fasload-method pathname)
    (declare (ignore loader notifier))
    (if pathname* #t #f)))

(define (choose-fasload-method pathname)
  (let* ((pathname (merge-pathnames pathname))
	 (thunk
	  (if (pathname-type pathname)
	      (or (try-object-file pathname)
		  (try-fasl-file pathname))
	      (or (try-fasl-file pathname)
		  (try-fasl-file (pathname-new-type pathname "com"))
		  (try-object-file (pathname-new-type pathname "so"))
		  (let ((bin-type (if package/cross-compiling? "nib" "bin")))
		    (try-fasl-file (pathname-new-type pathname bin-type)))))))
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
		    (string-for-primitive (->namestring pathname))))
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
	   (let ((n (bytes-per-object)))
	     (let ((marker (make-bytevector n)))
	       (and (eqv? (read-bytevector! marker port) n)
		    (let loop ((i 0))
		      (if (fix:< i n)
			  (and (fix:= (bytevector-u8-ref marker i) #xFA)
			       (loop (fix:+ i 1)))
			  #t)))))))))

(define (object-file? pathname)
  (and (let ((type (pathname-type pathname)))
	 (and (string? type)
	      (string=? type "so")))
       (file-regular? pathname)))

(define (load/purification-root object)
  (or (and (scode-comment? object)
	   (let ((text (scode-comment-text object)))
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
		 (suppress-loading-message?)
		 suppress-notifications?)
	     #f
	     (param:write-notifications?))))
    (parameterize ((param:write-notifications? notify?))
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

(define (load/push-hook! hook)
  (if (not (param:loading?)) (error condition-type:not-loading))
  (param:after-load-hooks (cons hook (param:after-load-hooks))))

(define (handle-load-hooks thunk)
  (receive (result hooks)
      (fluid-let ((load/loading? #t))	;backwards compatibility
	(parameterize ((param:loading? #t)
		       (param:after-load-hooks '()))
	  (let ((result (thunk)))
	    (values result (reverse (param:after-load-hooks))))))
    (for-each (lambda (hook) (hook)) hooks)
    result))

(define (load-failure procedure pathname . arguments)
  (apply procedure
	 (error:file-operation 0
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
			  (system-library-directory-pathname "lib" errors?))))
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
					     (fix:= 0
						    (string-length (car rp))))
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
			    (make-pathname #f #f (cons 'relative pu)
					   #f #f #f)))))))
	  (if path
	      (with-directory-rewriting-rule directory path thunk)
	      (thunk)))))))

(define (standard-library-directory-pathname)
  (last library-directory-path))

(define (pathname->standard-uri pathname)
  (let ((uri
	 (pathname->uri
	  (enough-pathname pathname (standard-library-directory-pathname)))))
    (if (uri-absolute? uri)
	uri
	(system-library-uri uri))))

(define (standard-uri->pathname uri)
  (or (uri->pathname uri #f)
      (merge-pathnames
       (uri->pathname (make-uri #f #f (list-tail (uri-path uri) 4) #f #f))
       (standard-library-directory-pathname))))

(define (system-uri #!optional rel-uri)
  (if (string? system-base-uri)
      (begin
	(set! system-base-uri (string->uri system-base-uri))
	unspecific))
  (maybe-merge rel-uri system-base-uri 'system-uri))

(define system-base-uri "http://www.gnu.org/software/mit-scheme/")

(define (system-library-uri #!optional rel-uri)
  (maybe-merge rel-uri (system-uri "lib/") 'system-library-uri))

(define (maybe-merge rel-uri base-uri caller)
  (if (default-object? rel-uri)
      base-uri
      (merge-uris (->relative-uri rel-uri caller) base-uri)))