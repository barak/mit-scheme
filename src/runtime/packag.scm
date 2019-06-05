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

(define-integrable (package/name package)
  (%record-ref package 3))

(define-integrable (package/environment package)
  (%record-ref package 4))

(define-integrable (set-package/environment! package environment)
  (%record-set! package 4 environment))

(define (package-name? object)
  (list-of-type? object symbol?))

(define (package-name=? name1 name2)
  (or (and (null? name1) (null? name2))
      (and (pair? name1)
	   (pair? name2)
	   (eq? (car name1) (car name2))
	   (package-name=? (cdr name1) (cdr name2)))))

(define (package/reference package name)
  (lexical-reference (package/environment package) name))

(define (finalize-package-record-type!)
  (let ((rtd
	 (make-record-type "package" '(parent children name environment))))
    (set! package-tag rtd)
    (for-each (lambda (p) (%record-set! p 0 rtd)) *packages*)
    (define-print-method (record-predicate rtd)
      (standard-print-method 'package
	(lambda (package)
	  (list (package/name package)))))))

(define (name->package name)
  (find-package name #f))

(define (all-packages)
  (let loop ((packages *packages*))
    (if (pair? packages)
	(cons (car packages) (loop (cdr packages)))
	'())))

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
  '|#[(package)package-name-tag]|)

(define (find-package name #!optional error?)
  (let package-loop ((packages *packages*))
    (if (pair? packages)
	(if (package-name=? name (package/name (car packages)))
	    (car packages)
	    (package-loop (cdr packages)))
	(begin
	  (if error? (error "Unable to find package:" name))
	  #f))))

(define (name-append name package)
  (let loop ((names (package/name package)))
    (if (pair? names)
	(cons (car names) (loop (cdr names)))
	(cons name '()))))

(define (package/add-child! package name environment #!optional force?)
  (let* ((real-name (name-append name package))
	 (child (find-package real-name #f)))
    (if child
	(begin
	  (if (not (if (default-object? force?)
		       *allow-package-redefinition?*
		       force?))
	      (error "Package already has child of given name:" package name))
	  (set-package/environment! child environment)
	  (set-package/children! child '())
	  (if (not (interpreter-environment->package environment))
	      (local-assignment environment package-name-tag child))
	  child)
	(package/create real-name package environment))))

(define *packages* '())
(define *allow-package-redefinition?* #f)

(define (initialize-package!)
  (set! *packages* '())
  (package/create '() #f system-global-environment))

(define (load-package-set filename #!optional options)
  (let ((pathname (merge-pathnames filename))
	(os-type microcode-id/operating-system))
    (let ((dir (directory-pathname pathname))
	  (pkg (package-set-pathname pathname os-type))
	  (options
	   (cons (cons 'os-type os-type)
		 (if (default-object? options) '() options))))
      (with-working-directory-pathname dir
	(lambda ()
	  (let ((file (fasload pkg)))
	    (if (not (package-file? file))
		(error "Malformed package-description file:" pkg))
	    (construct-packages-from-file file)
	    (let ((alternate-loader
		   (lookup-option 'alternate-package-loader options))
		  (load-component
		   (lambda (name environment)
		     (load name environment 'default #t))))
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

(define package/cross-compiling?
  #f)

(define (package-set-pathname pathname #!optional os-type)
  (let ((p (->pathname pathname)))
    (pathname-new-type
     (pathname-new-name p
			(string-append
			 (or (pathname-name p)
			     ;; Interpret dirname/ as dirname/dirname-OS.pkd.
			     (let ((dir (pathname-directory p)))
			       (if (pair? dir)
				   (let ((name (last dir)))
				     (if (string? name)
					 name
					 ""))
				   "")))
			 "-"
			 (microcode-id/operating-system-suffix os-type)))
     (if package/cross-compiling?
	 "dkp"
	 "pkd"))))

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
       (eq? (package-file/tag object) 'package-descriptions)
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
	     ((fix:= (vector-length object) 4)
	      (and (symbol? (vector-ref object 0))
		   (package-name? (vector-ref object 1))
		   (symbol? (vector-ref object 2))
		   (or (eq? #f (vector-ref object 3))
		       (eq? 'deprecated (vector-ref object 3)))))
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
			     (or (eq? (car clause) 'else)
				 (vector-of-type? (car clause) symbol?))
			     (vector-of-type? (cdr clause) string?)))))
	       (vector-of-type? file-case string?))))
       (vector? (load-description/initializations object))
       (vector? (load-description/finalizations object))))

;; CONSTRUCT-PACKAGES-FROM-FILE is called from the cold load before
;; the runtime system is loaded.  Thus it must only call procedures
;; that are defined in this file.

(define (construct-packages-from-file file)
  (let ((descriptions (package-file/descriptions file))
	(skip-package?
	 (lambda (name)
	   (or (null? name)
	       (and (pair? name)
		    (eq? (car name) 'package)
		    (null? (cdr name)))))))
    (let ((n (vector-length descriptions)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((description (vector-ref descriptions i)))
	  (let ((name (package-description/name description)))
	    (if (and (not (skip-package? name))
		     (not (package-description/extension? description))
		     ;; If there is an existing package, treat this as
		     ;; though an extension.
		     (not (find-package name #f)))
		(create-package-from-description description)))))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(let ((description (vector-ref descriptions i)))
	  (let ((name (package-description/name description)))
	    (if (not (skip-package? name))
		(create-links-from-description description))))))))

(define (create-package-from-description description)
  (let* ((parent (let ((ancestors (package-description/ancestors description)))
		   (if (pair? ancestors)
		       (find-package (car ancestors))
		       #f)))
	 (environment
	  (extend-package-environment
	   (if parent (package/environment parent) null-environment)
	   (cons (package-description/internal-names description)
		 (lambda (name) name))
	   (cons (package-description/exports description)
		 (lambda (binding) (vector-ref binding 0)))
	   (cons (package-description/imports description)
		 (lambda (binding) (vector-ref binding 0))))))
    (package/create (package-description/name description) parent environment)))

(define (package/create name parent environment)
  (let ((new (make-package parent name environment)))
    (local-assignment environment package-name-tag new)
    (if parent
	(set-package/children! parent (cons new (package/children parent))))
    (set! *packages* (cons new *packages*))
    new))

(define (create-links-from-description description)
  (let ((environment
	 (find-package-environment (package-description/name description))))
    (let ((bindings (package-description/exports description)))
      (let ((n (vector-length bindings)))
	(do ((i 0 (fix:+ i 1)))
	    ((fix:= i n))
	  (let ((binding (vector-ref bindings i)))
	    (link-variables (find-package-environment (vector-ref binding 1))
			    (if (fix:>= (vector-length binding) 3)
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
		   (if (fix:>= (vector-length binding) 3)
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
	(vector-set! vn 0 'dummy-procedure)
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
   (fix:xor ((ucode-primitive object-datum) #f) 1)))

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
				(or (eq? keys 'else)
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