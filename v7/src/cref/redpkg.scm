#| -*-Scheme-*-

$Id: redpkg.scm,v 1.18 2001/08/20 21:02:41 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Package Model Reader

(declare (usual-integrations)
	 (integrate-external "object"))

(define (read-package-model filename)
  (let ((model-pathname (merge-pathnames filename)))
    (with-values
	(lambda ()
	  (sort-descriptions (read-and-parse-model model-pathname)))
      (lambda (packages extensions loads globals)
	(descriptions->pmodel
	 packages
	 extensions
	 loads
	 (map (lambda (pathname)
		(cons
		 (->namestring pathname)
		 (let ((pathname
			(pathname-new-type (merge-pathnames pathname
							    model-pathname)
					   "pkd")))
		   (if (file-exists? pathname)
		       (let ((contents (fasload pathname)))
			 (if (package-file? contents)
			     contents
			     (begin
			       (warn "Malformed package-description file:"
				     pathname)
			       #f)))
		       (begin
			 (warn "Can't find package-description file:" pathname)
			 #f)))))
	      globals)
	 model-pathname)))))

(define (sort-descriptions descriptions)
  (letrec
      ((loop
	(lambda (descriptions packages extensions loads globals)
	  (if (pair? descriptions)
	      (let ((description (car descriptions))
		    (descriptions (cdr descriptions)))
		(case (car description)
		  ((DEFINE-PACKAGE)
		   (loop descriptions
			 (cons (cdr description) packages)
			 extensions
			 (if (interesting-package-to-load? (cdr description))
			     (cons (cdr description) loads)
			     loads)
			 globals))
		  ((EXTEND-PACKAGE)
		   (loop descriptions
			 packages
			 (cons (cdr description) extensions)
			 (if (interesting-package-to-load? (cdr description))
			     (cons (cdr description) loads)
			     loads)
			 globals))
		  ((GLOBAL-DEFINITIONS)
		   (loop descriptions
			 packages
			 extensions
			 loads
			 (append! (reverse (cdr description)) globals)))
		  ((NESTED-DESCRIPTIONS)
		   (call-with-values
		       (lambda ()
			 (loop (cdr description)
			       packages
			       extensions
			       loads
			       globals))
		     (lambda (packages extensions loads globals)
		       (loop descriptions packages extensions loads globals))))
		  (else
		   (error "Unknown description keyword:" (car description)))))
	      (values packages extensions loads globals)))))
    (call-with-values (lambda () (loop descriptions '() '() '() '()))
      (lambda (packages extensions loads globals)
	(values (reverse! packages)
		(reverse! extensions)
		(reverse! loads)
		(reverse! globals))))))

(define (interesting-package-to-load? description)
  (or (pair? (package-description/file-cases description))
      (pair? (package-description/initializations description))
      (pair? (package-description/finalizations description))))

(define (read-file-analyses! pmodel)
  (call-with-values (lambda () (cache-file-analyses! pmodel))
    (lambda (analyses changes?)
      (for-each (lambda (p&c)
		  (record-file-analysis! pmodel
					 (car p&c)
					 (analysis-cache/pathname (cdr p&c))
					 (analysis-cache/data (cdr p&c))))
		analyses)
      changes?)))

(define-structure (analysis-cache
		   (type vector)
		   (constructor make-analysis-cache (pathname time data))
		   (conc-name analysis-cache/))
  (pathname false read-only true)
  (time false)
  (data false))

(define (cache-file-analyses! pmodel)
  (let ((pathname (pathname-new-type (pmodel/pathname pmodel) "fre"))
	(changes? (list #f)))
    (let ((result
	   (let ((caches (if (file-exists? pathname) (fasload pathname) '())))
	     (let ((cache-packages
		    (lambda (packages)
		      (append-map!
		       (lambda (package)
			 (map (lambda (pathname)
				(cons package
				      (cache-file-analysis! pmodel
							    caches
							    pathname
							    changes?)))
			      (package/files package)))
		       packages))))
	       (append! (cache-packages (pmodel/packages pmodel))
			(cache-packages (pmodel/extra-packages pmodel)))))))
      (if (car changes?)
	  (fasdump (map cdr result) pathname))
      (values result (car changes?)))))

(define (cache-file-analysis! pmodel caches pathname changes?)
  (let ((cache (analysis-cache/lookup caches pathname))
	(full-pathname
	 (merge-pathnames (pathname-new-type pathname "bin")
			  (pmodel/pathname pmodel))))
    (let ((time (file-modification-time full-pathname)))
      (if (not time)
	  (error "unable to open file" full-pathname))
      (if cache
	  (begin
	    (if (> time (analysis-cache/time cache))
		(begin
		  (set-analysis-cache/data! cache (analyze-file full-pathname))
		  (set-analysis-cache/time! cache time)
		  (set-car! changes? #t)))
	    cache)
	  (begin
	    (set-car! changes? #t)
	    (make-analysis-cache pathname
				 time
				 (analyze-file full-pathname)))))))

(define (analysis-cache/lookup caches pathname)
  (let loop ((caches caches))
    (and (not (null? caches))
	 (if (pathname=? pathname (analysis-cache/pathname (car caches)))
	     (car caches)
	     (loop (cdr caches))))))

(define (record-file-analysis! pmodel package pathname entries)
  (for-each
   (let ((filename (->namestring pathname))
	 (root-package (pmodel/root-package pmodel))
	 (primitive-package (pmodel/primitive-package pmodel)))
     (lambda (entry)
       (let ((name (vector-ref entry 0))
	     (expression
	      (make-expression package filename (vector-ref entry 1))))
	 (for-each-vector-element (vector-ref entry 2)
	   (lambda (name)
	     (cond ((symbol? name)
		    (make-reference package name expression))
		   ((primitive-procedure? name)
		    (make-reference primitive-package
				    (primitive-procedure-name name)
				    expression))
		   ((access? name)
		    (if (eq? (access-environment name)
			     system-global-environment)
			(make-reference root-package
					(access-name name)
					expression)
			(warn "Non-root access" (unsyntax name))))
		   (else
		    (error "Illegal reference name" name)))))
	 (if name
	     (bind! package name expression #t)))))
   entries))

(define (resolve-references! pmodel)
  (for-each (lambda (package)
	      (for-each resolve-reference!
			(package/sorted-references package)))
	    (pmodel/packages pmodel)))

(define (resolve-reference! reference)
  (let ((binding
	 (package-lookup (reference/package reference)
			 (reference/name reference))))
    (if binding
	(begin
	  (set-reference/binding! reference binding)
	  (set-binding/references! binding
				   (cons reference
					 (binding/references binding)))))))

;;;; Package Descriptions

(define (read-and-parse-model pathname)
  (parse-package-expressions
   (read-file (pathname-default-type pathname "pkg"))
   pathname))

(define (parse-package-expressions expressions pathname)
  (map (lambda (expression)
	 (parse-package-expression expression pathname))
       expressions))

(define (parse-package-expression expression pathname)
  (let ((lose
	 (lambda ()
	   (error "Ill-formed package expression:" expression))))
    (if (not (and (pair? expression)
		  (symbol? (car expression))
		  (list? (cdr expression))))
	(lose))
    (case (car expression)
      ((DEFINE-PACKAGE)
       (cons 'DEFINE-PACKAGE
	     (parse-package-definition (parse-name (cadr expression))
				       (cddr expression))))
      ((EXTEND-PACKAGE)
       (cons 'EXTEND-PACKAGE
	     (parse-package-extension (parse-name (cadr expression))
				      (cddr expression))))
      ((GLOBAL-DEFINITIONS)
       (let ((filenames (cdr expression)))
	 (if (not (for-all? filenames string?))
	     (lose))
	 (cons 'GLOBAL-DEFINITIONS (map parse-filename filenames))))
      ((OS-TYPE-CASE)
       (if (not (and (list? (cdr expression))
		     (for-all? (cdr expression)
		       (lambda (clause)
			 (and (or (eq? 'ELSE (car clause))
				  (and (list? (car clause))
				       (for-all? (car clause) symbol?)))
			      (list? (cdr clause)))))))
	   (lose))
       (cons 'NESTED-DESCRIPTIONS
	     (let loop ((clauses (cdr expression)))
	       (cond ((null? clauses)
		      '())
		     ((or (eq? 'ELSE (caar clauses))
			  (memq microcode-id/operating-system (caar clauses)))
		      (parse-package-expressions (cdar clauses) pathname))
		     (else
		      (loop (cdr clauses)))))))
      ((INCLUDE)
       (cons 'NESTED-DESCRIPTIONS
	     (let ((filenames (cdr expression)))
	       (if (not (for-all? filenames string?))
		   (lose))
	       (append-map (lambda (filename)
			     (read-and-parse-model
			      (merge-pathnames filename pathname)))
			   filenames))))
      (else
       (lose)))))

(define (parse-package-definition name options)
  (check-package-options options)
  (call-with-values
      (lambda ()
	(let ((option (assq 'PARENT options)))
	  (if option
	      (let ((options (delq option options)))
		(if (not (and (pair? (cdr option))
			      (null? (cddr option))))
		    (error "Ill-formed PARENT option:" option))
		(if (assq 'PARENT options)
		    (error "Multiple PARENT options."))
		(values (parse-name (cadr option)) options))
	      (values 'NONE options))))
    (lambda (parent options)
      (let ((package (make-package-description name parent)))
	(process-package-options package options)
	package))))

(define (parse-package-extension name options)
  (check-package-options options)
  (let ((option (assq 'PARENT options)))
    (if option
	(error "PARENT option illegal in package extension:" option)))
  (let ((package (make-package-description name 'NONE)))
    (process-package-options package options)
    package))

(define (check-package-options options)
  (if (not (list? options))
      (error "Package options must be a list:" options))
  (for-each (lambda (option)
	      (if (not (and (pair? option)
			    (symbol? (car option))
			    (list? (cdr option))))
		  (error "Ill-formed package option:" option)))
	    options))

(define (process-package-options package options)
  (for-each (lambda (option)
	      (case (car option)
		((FILES)
		 (set-package-description/file-cases!
		  package
		  (append! (package-description/file-cases package)
			   (list (parse-filenames (cdr option))))))
		((FILE-CASE)
		 (set-package-description/file-cases!
		  package
		  (append! (package-description/file-cases package)
			   (list (parse-file-case (cdr option))))))
		((EXPORT)
		 (set-package-description/exports!
		  package
		  (append! (package-description/exports package)
			   (list (parse-import/export (cdr option))))))
		((IMPORT)
		 (set-package-description/imports!
		  package
		  (append! (package-description/imports package)
			   (list (parse-import/export (cdr option))))))
		((INITIALIZATION)
		 (let ((initialization (parse-initialization (cdr option))))
		   (if initialization
		       (set-package-description/initializations!
			package
			(append! (package-description/initializations package)
				 (list initialization))))))
		((FINALIZATION)
		 (let ((finalization (parse-initialization (cdr option))))
		   (if finalization
		       (set-package-description/finalizations!
			package
			(append! (package-description/finalizations package)
				 (list finalization))))))
		(else
		 (error "Unrecognized option keyword:" (car option)))))
	    options))

(define (parse-name name)
  (if (not (check-list name symbol?))
      (error "illegal name" name))
  name)

(define (parse-filenames filenames)
  (if (not (check-list filenames string?))
      (error "illegal filenames" filenames))
  (list #F (cons 'ELSE (map parse-filename filenames))))

(define (parse-file-case file-case)
  (if (not (and (pair? file-case)
		(symbol? (car file-case))
		(check-list (cdr file-case)
		  (lambda (clause)
		    (and (pair? clause)
			 (or (eq? 'ELSE (car clause))
			     (check-list (car clause) symbol?))
			 (check-list (cdr clause) string?))))))
      (error "Illegal file-case" file-case))
  (cons (car file-case)
	(map (lambda (clause)
	       (cons (car clause)
		     (map parse-filename (cdr clause))))
	     (cdr file-case))))

(define-integrable (parse-filename filename)
  (->pathname filename))

(define (parse-initialization initialization)
  (if (and (pair? initialization) (null? (cdr initialization)))
      (car initialization)
      (begin
	(warn "Illegal initialization/finalization:" initialization)
	#f)))

(define (parse-import/export object)
  (if (not (and (pair? object)
		(check-list (cdr object)
			    (lambda (item)
			      (or (symbol? item)
				  (and (pair? item)
				       (symbol? (car item))
				       (pair? (cdr item))
				       (symbol? (cadr item))
				       (null? (cddr item))))))))
      (error "illegal import/export list" object))
  (cons (parse-name (car object))
	(map (lambda (entry)
	       (if (pair? entry)
		   (cons (car entry) (cadr entry))
		   (cons entry entry)))
	     (cdr object))))

(define (check-list items predicate)
  (and (list? items)
       (for-all? items predicate)))

;;;; Packages

(define (descriptions->pmodel descriptions extensions loads globals pathname)
  (let ((packages
	 (map (lambda (description)
		(make-package (package-description/name description) 'UNKNOWN))
	      descriptions))
	(extra-packages '()))
    (let ((root-package
	   (or (name->package packages '())
	       (make-package '() #f))))
      (let ((get-package
	     (lambda (name intern?)
	       (if (null? name)
		   root-package
		   (or (name->package packages name)
		       (name->package extra-packages name)
		       (if intern?
			   (let ((package (make-package name 'UNKNOWN)))
			     (set! extra-packages
				   (cons package extra-packages))
			     package)
			   (error "Unknown package name:" name)))))))
	;; GLOBALS is a list of the bindings supplied externally.
	(for-each (lambda (global)
		    (if (cdr global)
			(process-globals-info (cdr global)
					      (->namestring (car global))
					      get-package)))
		  globals)
	(for-each
	 (lambda (package description)
	   (let ((parent
		  (let ((parent-name (package-description/parent description)))
		    (and (not (eq? parent-name 'NONE))
			 (get-package parent-name #t)))))
	     (set-package/parent! package parent)
	     (if parent
		 (set-package/children!
		  parent
		  (cons package (package/children parent)))))
	   (process-package-description package description get-package #t))
	 packages
	 descriptions)
	(for-each
	 (lambda (extension)
	   (process-package-description
	    (get-package (package-description/name extension) #f)
	    extension
	    get-package
	    #f))
	 extensions)
	(make-pmodel root-package
		     (make-package primitive-package-name #f)
		     packages
		     extra-packages
		     (map (lambda (package)
			    (process-package-load
			     (get-package (package-description/name package)
					  #f)
			     package))
			  loads)
		     pathname)))))

(define (process-globals-info file namestring get-package)
  (for-each-vector-element (vector-ref file 2)
    (lambda (desc)
      (let ((package (get-package (vector-ref desc 0) #t)))
	(let loop
	    ((package package)
	     (ancestors (vector-ref desc 1)))
	  (if (eq? 'UNKNOWN (package/parent package))
	      (if (pair? ancestors)
		  (let ((parent (get-package (car ancestors) #t)))
		    (set-package/parent! package parent)
		    (loop parent (cdr ancestors)))
		  (set-package/parent! package #f))))
	(let ((expression (make-expression package namestring #f)))
	  ;; Unlinked internal names.
	  (for-each-vector-element (vector-ref desc 2)
	    (lambda (name)
	      (bind! package name expression #f)))
	  ;; Exported bindings.
	  (for-each-vector-element (vector-ref desc 3)
	    (lambda (entry)
	      (let ((name (vector-ref entry 0))
		    (external-package (get-package (vector-ref entry 1) #t))
		    (external-name
		     (if (fix:= (vector-length entry) 2)
			 (vector-ref entry 0)
			 (vector-ref entry 2))))
		(bind! package name expression #f)
		(link! package name
		       external-package external-name
		       package #f))))
	  ;; Imported bindings.
	  (for-each-vector-element (vector-ref desc 4)
	    (lambda (entry)
	      (let ((external-package (get-package (vector-ref entry 1) #t))
		    (external-name 
		     (if (fix:= (vector-length entry) 2)
			 (vector-ref entry 0)
			 (vector-ref entry 2))))
		(bind! external-package external-name expression #f)
		(link! external-package external-name
		       package (vector-ref entry 0)
		       package #f)))))))))

(define (package-lookup package name)
  (let package-loop ((package package))
    (or (package/find-binding package name)
	(and (package/parent package)
	     (package-loop (package/parent package))))))

(define (name->package packages name)
  (list-search-positive packages
    (lambda (package)
      (symbol-list=? name (package/name package)))))

(define (process-package-description package description get-package new?)
  (let ((file-cases (package-description/file-cases description)))
    (set-package/files!
     package
     (append! (package/files package)
	      (append-map! (lambda (file-case)
			     (append-map cdr (cdr file-case)))
			   file-cases))))
  (for-each (lambda (export)
	      (let ((destination (get-package (car export) #t)))
		(for-each (lambda (names)
			    (link! package (car names)
				   destination (cdr names)
				   package new?))
			  (cdr export))))
	    (package-description/exports description))
  (for-each (lambda (import)
	      (let ((source (get-package (car import) #t)))
		(for-each (lambda (names)
			    (link! source (cdr names)
				   package (car names)
				   package new?))
			  (cdr import))))
	    (package-description/imports description)))

(define primitive-package-name
  (list (string->symbol "#[(cross-reference reader)primitives]")))

(define (process-package-load package description)
  (make-package-load package
		     (package-description/file-cases description)
		     (package-description/initializations description)
		     (package-description/finalizations description)))

;;;; Binding and Reference

(define (bind! package name expression new?)
  (let ((value-cell (binding/value-cell (intern-binding! package name new?))))
    (set-expression/value-cell! expression value-cell)
    (set-value-cell/expressions!
     value-cell
     (cons expression (value-cell/expressions value-cell)))))

(define (link! source-package source-name
	       destination-package destination-name
	       owner-package new?)
  (let ((source-binding (intern-binding! source-package source-name new?))
	(destination-binding
	 (package/find-binding destination-package destination-name)))
    (if (and destination-binding
	     (not (eq? (binding/value-cell destination-binding)
		       (binding/value-cell source-binding))))
	(error "Attempt to reinsert binding:" destination-name))
    (let ((destination-binding
	   (make-binding destination-package
			 destination-name
			 (binding/value-cell source-binding)
			 new?)))
      (rb-tree/insert! (package/bindings destination-package)
		       destination-name
		       destination-binding)
      (make-link source-binding destination-binding owner-package new?))))

(define (intern-binding! package name new?)
  (let ((binding (package/find-binding package name)))
    (if binding
	(begin
	  (if new? (set-binding/new?! binding #t))
	  binding)
	(let ((binding
	       (let ((value-cell (make-value-cell)))
		 (let ((binding (make-binding package name value-cell new?)))
		   (set-value-cell/source-binding! value-cell binding)
		   binding))))
	  (rb-tree/insert! (package/bindings package) name binding)
	  binding))))

(define (make-reference package name expression)
  (let ((references (package/references package))
	(add-reference!
	 (lambda (reference)
	   (set-reference/expressions!
	    reference
	    (cons expression (reference/expressions reference)))
	   (set-expression/references!
	    expression
	    (cons reference (expression/references expression))))))
    (let ((reference (rb-tree/lookup references name #f)))
      (if reference
	  (begin
	    (if (not (memq expression (reference/expressions reference)))
		(add-reference! reference))
	    reference)
	  (let ((reference (%make-reference package name)))
	    (rb-tree/insert! references name reference)
	    (add-reference! reference)
	    reference)))))