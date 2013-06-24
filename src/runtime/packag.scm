#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

(define (package-name? object)
  (list-of-type? object symbol?))

(define (package/reference package name)
  (lexical-reference (package/environment package) name))

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
    (and (pair? children)
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
    (if (pair? path)
	(let ((child (package/child package (car path))))
	  (and child
	       (loop (cdr path) child)))
	package)))

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

(define (find-package name #!optional error?)
  (let loop ((path name) (package system-global-package))
    (if (pair? path)
	(loop (cdr path)
	      (let ((child (package/child package (car path))))
		(if (and (not child) error?)
		    (error "Unable to find package:"
			   (list-difference name (cdr path))))
		child))
	package)))

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
	(begin
	  (if (not (if (default-object? force?)
		       *allow-package-redefinition?*
		       force?))
	      (error "Package already has child of given name:" package name))
	  (set-package/environment! child environment)
	  (set-package/children! child '())
	  (finish child))
	(let ((child (make-package package name environment)))
	  (set-package/children! package
				 (cons child (package/children package)))
	  (finish child)))))

(define system-global-package)
(define *allow-package-redefinition?* #f)

(define (initialize-package!)
  (set! system-global-package (make-package #f #f system-global-environment))
  (local-assignment system-global-environment
		    package-name-tag
		    system-global-package))

(define (load-package-set filename #!optional options)
  (let ((pathname (merge-pathnames filename))
	(os-type microcode-id/operating-system))
    (let ((dir (directory-pathname pathname))
	  (pkg (package-set-pathname pathname os-type))
	  (options
	   (cons (cons 'OS-TYPE os-type)
		 (if (default-object? options) '() options))))
      (with-working-directory-pathname dir
	(lambda ()
	  (let ((file (fasload pkg)))
	    (if (not (package-file? file))
		(error "Malformed package-description file:" pkg))
	    (construct-packages-from-file file)
	    (let ((alternate-loader
		   (lookup-option 'ALTERNATE-PACKAGE-LOADER options))
		  (load-component
		   (lambda (name environment)
		     (load name environment 'DEFAULT #t))))
	      (if alternate-loader
		  (alternate-loader load-component options)
		  (begin
		    (load-packages-from-file file options load-component)
		    (initialize-packages-from-file file)))))))))
  ;; Make sure that everything we just loaded is purified.  If the
  ;; program runs before it gets purified, some of its run-time state
  ;; can end up being purified also.
  (flush-purification-queue!))

;; Obsolete and ignored:
(define system-loader/enable-query? #f)

(define (package-set-pathname pathname #!optional os-type)
  (pathname-new-type
   (pathname-new-name pathname
		      (string-append (pathname-name pathname)
				     "-"
				     (case (if (default-object? os-type)
					       microcode-id/operating-system
					       os-type)
				       ((NT) "w32")
				       ((OS/2) "os2")
				       ((UNIX) "unx")
				       (else "unk"))))
   "pkd"))

(define-integrable (make-package-file tag version descriptions loads)
  (vector tag version descriptions loads))

(define-integrable (package-file/tag pf) (vector-ref pf 0))
(define-integrable (package-file/version pf) (vector-ref pf 1))
(define-integrable (package-file/descriptions pf) (vector-ref pf 2))
(define-integrable (package-file/loads pf) (vector-ref pf 3))

(define-integrable (make-package-description name ancestors internal-names
					     exports imports extension?)
  (vector name ancestors internal-names exports imports extension?))

(define-integrable (package-description/name pd) (vector-ref pd 0))
(define-integrable (package-description/ancestors pd) (vector-ref pd 1))
(define-integrable (package-description/internal-names pd) (vector-ref pd 2))
(define-integrable (package-description/exports pd) (vector-ref pd 3))
(define-integrable (package-description/imports pd) (vector-ref pd 4))
(define-integrable (package-description/extension? pd) (vector-ref pd 5))

(define-integrable (make-load-description name file-cases initializations
					  finalizations)
  (vector name file-cases initializations finalizations))

(define-integrable (load-description/name pd) (vector-ref pd 0))
(define-integrable (load-description/file-cases pd) (vector-ref pd 1))
(define-integrable (load-description/initializations pd) (vector-ref pd 2))
(define-integrable (load-description/finalizations pd) (vector-ref pd 3))

(define (package-file? object)
  (and (vector? object)
       (fix:= (vector-length object) 4)
       (eq? (package-file/tag object) 'PACKAGE-DESCRIPTIONS)
       (and (index-fixnum? (package-file/version object))
	    (fix:= (package-file/version object) 2))
       (vector-of-type? (package-file/descriptions object)
			package-description?)
       (vector-of-type? (package-file/loads object)
			load-description?)))

(define (package-description? object)
  (and (vector? object)
       (fix:= (vector-length object) 6)
       (package-name? (package-description/name object))
       (list-of-type? (package-description/ancestors object) package-name?)
       (vector-of-type? (package-description/internal-names object) symbol?)
       (vector-of-type? (package-description/exports object) link-description?)
       (vector-of-type? (package-description/imports object) link-description?)
       (boolean? (package-description/extension? object))))

(define (link-description? object)
  (and (vector? object)
       (cond ((fix:= (vector-length object) 2)
	      (and (symbol? (vector-ref object 0))
		   (package-name? (vector-ref object 1))))
	     ((fix:= (vector-length object) 3)
	      (and (symbol? (vector-ref object 0))
		   (package-name? (vector-ref object 1))
		   (symbol? (vector-ref object 2))))
	     (else #f))))

(define (load-description? object)
  (and (vector? object)
       (fix:= (vector-length object) 4)
       (package-name? (load-description/name object))
       (vector-of-type? (load-description/file-cases object)
	 (lambda (file-case)
	   (if (pair? file-case)
	       (and (symbol? (car file-case))
		    (vector-of-type? (cdr file-case)
		      (lambda (clause)
			(and (pair? clause)
			     (or (eq? (car clause) 'ELSE)
				 (vector-of-type? (car clause) symbol?))
			     (vector-of-type? (cdr clause) string?)))))
	       (vector-of-type? file-case string?))))
       (vector? (load-description/initializations object))
       (vector? (load-description/finalizations object))))

;; CONSTRUCT-PACKAGES-FROM-FILE is called from the cold load and must
;; only use procedures that are inline-coded by the compiler.

(define (construct-packages-from-file file)
  (let ((descriptions (package-file/descriptions file))
	(skip-package?
	 (lambda (name)
	   (or (null? name)
	       (and (pair? name)
		    (eq? (car name) 'PACKAGE)
		    (null? (cdr name)))))))
    (let ((n (vector-length descriptions)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((description (vector-ref descriptions i)))
	  (let ((name (package-description/name description)))
	    (if (not (skip-package? name))
		(construct-normal-package-from-description description)))))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((description (vector-ref descriptions i)))
	  (let ((name (package-description/name description)))
	    (if (not (skip-package? name))
		(create-links-from-description description))))))))

(define (construct-normal-package-from-description description)
  (let ((name (package-description/name description))
	(extension? (package-description/extension? description))
	(environment
	 (extend-package-environment
	  (let ((ancestors (package-description/ancestors description)))
	    (if (pair? ancestors)
		(package/environment (find-package (car ancestors)))
		null-environment))
	  (cons (package-description/internal-names description)
		(lambda (name) name))
	  (cons (package-description/exports description)
		(lambda (binding) (vector-ref binding 0)))
	  (cons (package-description/imports description)
		(lambda (binding) (vector-ref binding 0))))))
    (let loop ((path name) (package system-global-package))
      (if (pair? (cdr path))
	  (loop (cdr path)
		(or (package/child package (car path))
		    (error "Unable to find package:"
			   (list-difference name (cdr path)))))
	  (if (not (and extension? (package/child package (car path))))
	      (package/add-child! package (car path) environment))))))

(define (create-links-from-description description)
  (let ((environment
	 (find-package-environment (package-description/name description))))
    (let ((bindings (package-description/exports description)))
      (let ((n (vector-length bindings)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (let ((binding (vector-ref bindings i)))
	    (link-variables (find-package-environment (vector-ref binding 1))
			    (if (fix:= (vector-length binding) 3)
				(vector-ref binding 2)
				(vector-ref binding 0))
			    environment
			    (vector-ref binding 0))))))
    (let ((bindings (package-description/imports description)))
      (let ((n (vector-length bindings)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (let ((binding (vector-ref bindings i)))
	    (let ((source-environment
		   (find-package-environment (vector-ref binding 1)))
		  (source-name
		   (if (fix:= (vector-length binding) 3)
		       (vector-ref binding 2)
		       (vector-ref binding 0))))
	      (guarantee-binding source-environment source-name)
	      (link-variables environment (vector-ref binding 0)
			      source-environment source-name))))))))

(define (extend-package-environment environment . name-sources)
  (let ((names
	 (do ((name-sources name-sources (cdr name-sources))
	      (names '()
		     (let ((v (car (car name-sources)))
			   (p (cdr (car name-sources))))
		       (let ((end (vector-length v)))
			 (do ((j 0 (fix:+ j 1))
			      (names names
				     (let ((name (p (vector-ref v j))))
				       (if (let find ((names names))
					     (if (pair? names)
						 (if (eq? (car names) name)
						     #t
						     (find (cdr names)))
						 #f))
					   names
					   (cons name names)))))
			     ((not (fix:< j end)) names))))))
	     ((not (pair? name-sources)) names))))
    (let ((n
	   (do ((names names (cdr names))
		(n 1 (fix:+ n 1)))
	       ((not (pair? names)) n))))
      (let ((vn ((ucode-primitive vector-cons) n #f))
	    (vv
	     ((ucode-primitive vector-cons)
	      n
	      (make-unmapped-unassigned-reference-trap))))
	(vector-set! vn 0 'DUMMY-PROCEDURE)
	(do ((names names (cdr names))
	     (j 1 (fix:+ j 1)))
	    ((not (pair? names)))
	  (vector-set! vn j (car names)))
	(vector-set! vv 0
		     (system-pair-cons (ucode-type procedure)
				       (system-pair-cons (ucode-type lambda)
							 #f
							 vn)
				       environment))
	((ucode-primitive object-set-type) (ucode-type environment) vv)))))

(define null-environment
  ((ucode-primitive object-set-type)
   ((ucode-primitive object-type) #f)
   (fix:xor ((ucode-primitive object-datum) #F) 1)))

(define (find-package-environment name)
  (package/environment (find-package name)))

(define (guarantee-binding environment name)
  (if (lexical-unbound? environment name)
      (local-assignment environment
			name
			(make-unmapped-unassigned-reference-trap))))

(define-integrable (make-unmapped-unassigned-reference-trap)
  (primitive-object-set-type (ucode-type reference-trap) 0))

(define-primitives
  lexical-reference
  lexical-unbound?
  lexical-unreferenceable?
  link-variables
  local-assignment
  primitive-object-set-type)

;; LOAD-PACKAGES-FROM-FILE is called from the cold load and must only
;; use procedures that are inline-coded by the compiler.

(define (load-packages-from-file file options file-loader)
  (let ((loads (package-file/loads file)))
    (let ((n (vector-length loads)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((description (vector-ref loads i)))
	  (load-package-from-description
	   (find-package (load-description/name description))
	   description
	   options
	   file-loader))))))

(define (load-package-from-description package description options file-loader)
  (let ((environment (package/environment package)))
    (let ((load-files
	   (lambda (filenames)
	     (let ((n (vector-length filenames)))
	       (do ((i 0 (fix:+ i 1)))
		   ((fix:= i n))
		 (file-loader (vector-ref filenames i) environment)))))
	  (cases (load-description/file-cases description)))
      (let ((n (vector-length cases)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (let ((file-case (vector-ref cases i)))
	    (if (pair? file-case)
		(let ((option (lookup-option (car file-case) options)))
		  (if (not option)
		      (error "Missing key:" (car file-case)))
		  (let ((clauses (cdr file-case)))
		    (let ((n (vector-length clauses)))
		      (do ((i 0 (fix:+ i 1)))
			  ((fix:= i n))
			(let ((clause (vector-ref clauses i)))
			  (if (let ((keys (car clause)))
				(or (eq? keys 'ELSE)
				    (let ((n (vector-length keys)))
				      (let loop ((i 0))
					(and (fix:< i n)
					     (or (eq? (vector-ref keys i)
						      option)
						 (loop (fix:+ i 1))))))))
			      (load-files (cdr clause))))))))
		(load-files file-case))))))))

(define (lookup-option key options)
  (let loop ((options options))
    (and (pair? options)
	 (if (eq? (car (car options)) key)
	     (cdr (car options))
	     (loop (cdr options))))))

(define (initialize-packages-from-file file)
  (initialize/finalize file load-description/initializations "Initializing"))

(define (finalize-packages-from-file file)
  (initialize/finalize file load-description/finalizations "Finalizing"))

(define (initialize/finalize file selector verb)
  (for-each-vector-element (package-file/loads file)
    (lambda (description)
      (let ((expressions (selector description)))
	(if (fix:> (vector-length expressions) 0)
	    (let ((name (load-description/name description)))
	      (with-notification (lambda (port)
				   (write-string verb port)
				   (write-string " package " port)
				   (write name port))
		(lambda ()
		  (for-each-vector-element expressions
		    (let ((environment (find-package-environment name)))
		      (lambda (expression)
			(eval expression environment))))))))))))