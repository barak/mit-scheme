#| -*-Scheme-*-

$Id: packag.scm,v 14.25 1996/04/24 04:22:46 cph Exp $

Copyright (c) 1988-96 Massachusetts Institute of Technology

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

;;;; Simple Package Namespace
;;; package: (package)

(declare (usual-integrations))

;;; Kludge -- package objects want to be records, but this file must
;;; be loaded first, before the record package.  The way we solve this
;;; problem is to build the initial packages without an appropriate
;;; record type, then build the record type and clobber it into the
;;; packages.  Thereafter, packages are constructed normally.

(define package-tag #f)

(define-integrable (make-package parent name environment)
  (%record package-tag parent '() name environment))

(define (package? object)
  (and (%record? object)
       (eq? (%record-ref object 0) package-tag)))

(define-integrable (package/parent package)
  (%record-ref package 1))

(define-integrable (package/children package)
  (%record-ref package 2))

(define-integrable (set-package/children! package children)
  (%record-set! package 2 children))

(define-integrable (package/%name package)
  (%record-ref package 3))

(define-integrable (package/environment package)
  (%record-ref package 4))

(define-integrable (set-package/environment! package environment)
  (%record-set! package 4 environment))

(define (finalize-package-record-type!)
  (let ((rtd
	 (make-record-type "package" '(PARENT CHILDREN %NAME ENVIRONMENT))))
    (let ((tag (record-type-dispatch-tag rtd)))
      (set! package-tag tag)
      (let loop ((package system-global-package))
	(%record-set! package 0 tag)
	(for-each loop (package/children package))))
    (set-record-type-unparser-method! rtd
      (standard-unparser-method 'PACKAGE
	(lambda (package port)
	  (write-char #\space port)
	  (write (package/name package) port))))))

(define (package/child package name)
  (let loop ((children (package/children package)))
    (and (not (null? children))
	 (if (eq? name (package/%name (car children)))
	     (car children)
	     (loop (cdr children))))))

(define (package/name package)
  (let loop ((package package) (result '()))
    (if (package/parent package)
	(loop (package/parent package) (cons (package/%name package) result))
	result)))

(define (name->package name)
  (let loop ((path name) (package system-global-package))
    (if (null? path)
	package
	(let ((child (package/child package (car path))))
	  (and child
	       (loop (cdr path) child))))))

(define (environment->package environment)
  (and (interpreter-environment? environment)
       (interpreter-environment->package environment)))

(define (interpreter-environment->package environment)
  (and (not (lexical-unreferenceable? environment package-name-tag))
       (let ((package (lexical-reference environment package-name-tag)))
	 (and (package? package)
	      (eq? environment (package/environment package))
	      package))))

(define-integrable package-name-tag
  ((ucode-primitive string->symbol) "#[(package)package-name-tag]"))

(define (find-package name)
  (let loop ((path name) (package system-global-package))
    (if (null? path)
	package
	(loop (cdr path)
	      (or (package/child package (car path))
		  (error "Unable to find package"
			 (list-difference name (cdr path))))))))

(define (list-difference list tail)
  (let loop ((list list))
    (if (eq? list tail)
	'()
	(cons (car list) (loop (cdr list))))))

(define (package/add-child! package name environment #!optional force?)
  (let ((child (package/child package name))
	(finish
	 (lambda (child)
	   (if (not (interpreter-environment->package environment))
	       (local-assignment environment package-name-tag child))
	   child)))
    (if child
	(if (and (not (default-object? force?)) force?)
	    (begin
	      (set-package/environment! child environment)
	      (finish child))
	    (error "Package already has child of given name:" package name))
	(let ((child (make-package package name environment)))
	  (set-package/children! package
				 (cons child (package/children package)))
	  (finish child)))))

(define system-global-package)

(define system-loader/enable-query?
  false)

(define (package/system-loader filename options load-interpreted?)
  (let ((pathname (->pathname filename)))
    (with-working-directory-pathname (directory-pathname pathname)
      (lambda ()
	(fluid-let ((load/default-types
		     (if (if (eq? load-interpreted? 'QUERY)
			     (and system-loader/enable-query?
				  (prompt-for-confirmation "Load interpreted"))
			     load-interpreted?)
			 (list (assoc "bin" load/default-types)
			       (assoc "scm" load/default-types))
			 load/default-types)))
	  (let ((syntax-table (nearest-repl/syntax-table)))
	    (load (let ((rewrite (assq 'MAKE-CONSTRUCTOR-NAME options))
			(pathname (pathname-new-type pathname "bco")))
		    (if rewrite
			((cdr rewrite) pathname)
			pathname))
		  system-global-environment
		  syntax-table false)
	    ((load (let ((rewrite (assq 'MAKE-LOADER-NAME options))
			 (pathname (pathname-new-type pathname "bld")))
		     (if rewrite
			 ((cdr rewrite) pathname)
			 pathname))
		   system-global-environment
		   syntax-table false)
	     (lambda (component environment)
	       (cond ((filename->compiled-object filename component)
		      => (lambda (value)
			   (purify (load/purification-root value))
			   (scode-eval value environment)))
		     (else
		      (load component environment syntax-table true))))
	     options))))))
  ;; Make sure that everything we just loaded is purified.  If the
  ;; program runs before it gets purified, some of its run-time state
  ;; can end up being purified also.
  (flush-purification-queue!))

(define (filename->compiled-object system component)
  (let ((prim (ucode-primitive initialize-c-compiled-block 1)))
    (and (implemented-primitive-procedure? prim)
	 (let* ((name
		 (let* ((p (->pathname component))
			(d (pathname-directory p)))
		   (string-append
		    (if (or (not d) (null? d))
			system
			(car (last-pair d)))
		    "_"
		    (string-replace (pathname-name p) ; kludge
				    #\-
				    #\_))))
		(value (prim name)))
	   (if (or (not value)
		   load/suppress-loading-message?)
	       value
	       (let ((port (notification-output-port)))
		 (fresh-line port)
		 (write-string ";Initialized " port)
		 (write name port)
		 value))))))

(define-integrable (package/reference package name)
  (lexical-reference (package/environment package) name))

(define (initialize-package!)
  (set! system-global-package
	(make-package false false system-global-environment))
  (local-assignment system-global-environment
		    package-name-tag
		    system-global-package))