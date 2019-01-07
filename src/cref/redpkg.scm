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

;;;; Package Model Reader

(declare (usual-integrations)
	 (integrate-external "object"))

(define (read-package-model filename os-type)
  (let ((model-pathname (cref/source-pathname filename)))
    (receive (packages extensions loads globals)
	(sort-descriptions (read-and-parse-model model-pathname os-type))
      (descriptions->pmodel
       packages
       extensions
       loads
       (map (lambda (name)
	      (let ((pathname (find-global-definitions name model-pathname
						       os-type)))
		(and pathname
		     (cons (->namestring pathname)
			   (let ((contents (fasload pathname #t)))
			     (if (package-file? contents)
				 contents
				 (begin
				   (warn "Malformed package-description file:"
					 pathname)
				   #f)))))))
	    globals)
       model-pathname))))

(define (find-global-definitions name model-pathname os-type)
  (let* ((filename (->pathname
		    (cond ((symbol? name) (symbol->string name))
			  ((string? name) name)
			  (else (error "Not a globals name:" name)))))
	 (pkd (package-set-pathname filename os-type)))
    (or
     (if (symbol? name)
	 (let ((pathname (ignore-errors
			  (lambda ()
			    (system-library-pathname pkd)))))
	   (and (not (condition? pathname))
		pathname))
	 (or (let* ((object-model-pathname
		     (merge-pathnames
		      (enough-pathname model-pathname cref/source-root)
		      cref/object-root))
		    (pathname (merge-pathnames pkd object-model-pathname)))
	       (and (file-exists? pathname)
		    pathname))
	     (let ((pathname (merge-pathnames pkd model-pathname)))
	       (and (file-exists? pathname)
		    pathname))))
     (begin
       (warn "Could not find global definitions:" pkd)
       #f))))

(define (sort-descriptions descriptions)
  (letrec
      ((loop
	(lambda (descriptions packages extensions loads globals)
	  (if (pair? descriptions)
	      (let ((description (car descriptions))
		    (descriptions (cdr descriptions)))
		(case (car description)
		  ((define-package)
		   (loop descriptions
			 (cons (cdr description) packages)
			 extensions
			 (if (interesting-package-to-load? (cdr description))
			     (cons (cdr description) loads)
			     loads)
			 globals))
		  ((extend-package)
		   (loop descriptions
			 packages
			 (cons (cdr description) extensions)
			 (if (interesting-package-to-load? (cdr description))
			     (cons (cdr description) loads)
			     loads)
			 globals))
		  ((global-definitions)
		   (loop descriptions
			 packages
			 extensions
			 loads
			 (append! (reverse (cdr description)) globals)))
		  ((nested-descriptions)
		   (receive (packages extensions loads globals)
		       (loop (cdr description)
			     packages
			     extensions
			     loads
			     globals)
		     (loop descriptions packages extensions loads globals)))
		  (else
		   (warn "Unexpected description:" description)
		   (loop descriptions packages extensions loads globals))))
	      (values packages extensions loads globals)))))
    (receive (packages extensions loads globals)
	(loop descriptions '() '() '() '())
      (values (reverse! packages)
	      (reverse! extensions)
	      (reverse! loads)
	      (reverse! globals)))))

(define (interesting-package-to-load? description)
  (or (pair? (package-description/file-cases description))
      (pair? (package-description/initializations description))
      (pair? (package-description/finalizations description))))

(define (read-file-analyses! pmodel os-type)
  (receive (analyses changes?) (cache-file-analyses! pmodel os-type)
    (for-each (lambda (p&c)
		(record-file-analysis! pmodel
				       (car p&c)
				       (analysis-cache/pathname (cdr p&c))
				       (analysis-cache/data (cdr p&c))))
	      analyses)
    changes?))

(define-structure (analysis-cache
		   (type vector)
		   (constructor make-analysis-cache (pathname time data))
		   (conc-name analysis-cache/))
  (pathname #f read-only #t)
  (time #f)
  (data #f))

(define (cache-file-analyses! pmodel os-type)
  (let ((pathname
	 (pathname-new-type
	  (package-set-pathname (pmodel/object-pathname pmodel) os-type)
	  "fre"))
	(changes? (list #f)))
    (let ((result
	   (let ((caches
		  (if (file-exists? pathname) (fasload pathname #t) '())))
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
	  (fasdump (map cdr result) pathname #t))
      (values result (car changes?)))))

(define (cache-file-analysis! pmodel caches pathname changes?)
  (let ((cache (analysis-cache/lookup caches pathname))
	(full-pathname
	 (merge-pathnames
	  (pathname-new-type pathname (if sf/cross-compiling? "nib" "bin"))
	  (pmodel/object-pathname pmodel))))
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
		   ((scode-access? name)
		    (if (eq? (scode-access-environment name)
			     system-global-environment)
			(make-reference root-package
					(scode-access-name name)
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
			(package/references package)))
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

(define (read-and-parse-model pathname os-type)
  (parse-package-expressions
   (read-file (pathname-default-type pathname "pkg"))
   pathname
   os-type))

(define (parse-package-expressions expressions pathname os-type)
  (filter-map (lambda (expression)
		(parse-package-expression expression pathname os-type))
	      expressions))

(define (parse-package-expression expression pathname os-type)
  (let ((lose
	 (lambda ()
	   (error "Ill-formed package expression:" expression))))
    (if (not (and (pair? expression)
		  (symbol? (car expression))
		  (list? (cdr expression))))
	(lose))
    (case (car expression)
      ((define-package)
       (cons 'define-package
	     (parse-package-definition (parse-name (cadr expression))
				       (cddr expression))))
      ((extend-package)
       (cons 'extend-package
	     (parse-package-extension (parse-name (cadr expression))
				      (cddr expression))))
      ((global-definitions)
       (let ((filenames (cdr expression)))
	 (if (not (every (lambda (f) (or (string? f) (symbol? f))) filenames))
	     (lose))
	 (cons 'global-definitions filenames)))
      ((os-type-case)
       (if (not (and (list? (cdr expression))
		     (every (lambda (clause)
			      (and (or (eq? 'else (car clause))
				       (and (list? (car clause))
					    (every symbol? (car clause))))
				   (list? (cdr clause))))
			    (cdr expression))))
	   (lose))
       (cons 'nested-descriptions
	     (let loop ((clauses (cdr expression)))
	       (cond ((null? clauses)
		      '())
		     ((or (eq? 'else (caar clauses))
			  (memq os-type (caar clauses)))
		      (parse-package-expressions (cdar clauses)
						 pathname
						 os-type))
		     (else
		      (loop (cdr clauses)))))))
      ((include)
       (cons 'nested-descriptions
	     (let ((filenames (cdr expression)))
	       (if (not (every string? filenames))
		   (lose))
	       (append-map (lambda (filename)
			     (read-and-parse-model
			      (merge-pathnames filename pathname)
			      os-type))
			   filenames))))
      (else
       (warn "Unexpected description:" expression)
       #f))))

(define (parse-package-definition name options)
  (check-package-options options)
  (receive (parent options)
      (let ((option (assq 'parent options)))
	(if option
	    (let ((options (delq option options)))
	      (if (not (and (pair? (cdr option))
			    (null? (cddr option))))
		  (error "Ill-formed PARENT option:" option))
	      (if (assq 'parent options)
		  (error "Multiple PARENT options."))
	      (values (and (cadr option)
			   (parse-name (cadr option)))
		      options))
	    (values 'none options)))
    (let ((package (make-package-description name parent)))
      (process-package-options package options)
      package)))

(define (parse-package-extension name options)
  (check-package-options options)
  (let ((option (assq 'parent options)))
    (if option
	(error "PARENT option illegal in package extension:" option)))
  (let ((package (make-package-description name 'none)))
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
		((files)
		 (set-package-description/file-cases!
		  package
		  (append! (package-description/file-cases package)
			   (list (parse-filenames (cdr option))))))
		((file-case)
		 (set-package-description/file-cases!
		  package
		  (append! (package-description/file-cases package)
			   (list (parse-file-case (cdr option))))))
		((export)
		 (let ((export
			(cond ((and (pair? (cdr option))
				    (eq? 'deprecated (cadr option)))
			       (parse-import/export (cddr option) #t))
			      ;; 9.2 compatibility
			      ((and (pair? (cdr option))
				    (pair? (cddr option))
				    (symbol? (caddr option))
				    (string-prefix-ci?
				     "deprecated:"
				     (symbol->string (caddr option))))
			       (parse-import/export (cons (cadr option)
							  (cdddr option))
						    #t))
			      (else
			       (parse-import/export (cdr option) #f)))))
		   (set-package-description/exports!
		    package
		    (append! (package-description/exports package)
			     (list export)))))
		((export-deprecated)
		 (set-package-description/exports!
		  package
		  (append! (package-description/exports package)
			   (list (parse-import/export (cdr option) #t)))))
		((import)
		 (set-package-description/imports!
		  package
		  (append! (package-description/imports package)
			   (list (parse-import/export (cdr option) #f)))))
		((initialization)
		 (let ((initialization (parse-initialization (cdr option))))
		   (if initialization
		       (set-package-description/initializations!
			package
			(append! (package-description/initializations package)
				 (list initialization))))))
		((finalization)
		 (let ((finalization (parse-initialization (cdr option))))
		   (if finalization
		       (set-package-description/finalizations!
			package
			(append! (package-description/finalizations package)
				 (list finalization))))))
		(else
		 (warn "Unexpected option:" option))))
	    options))

(define (parse-name name)
  (if (not (check-list name symbol?))
      (error "illegal name" name))
  name)

(define (parse-filenames filenames)
  (if (not (check-list filenames string?))
      (error "illegal filenames" filenames))
  (list #f (cons 'else (map parse-filename filenames))))

(define (parse-file-case file-case)
  (if (not (and (pair? file-case)
		(symbol? (car file-case))
		(check-list (cdr file-case)
		  (lambda (clause)
		    (and (pair? clause)
			 (or (eq? 'else (car clause))
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

(define (parse-import/export object deprecated?)
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
		   (vector (car entry) (cadr entry) deprecated?)
		   (vector entry entry deprecated?)))
	     (cdr object))))

(define (check-list items predicate)
  (and (list? items)
       (every predicate items)))

;;;; Packages

(define (descriptions->pmodel descriptions extensions loads globals pathname)
  (let ((packages
	 (map (lambda (description)
		(make-package (package-description/name description) 'unknown))
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
		       (begin
			 (if (not intern?)
			     (warn "Unknown package name:" name))
			 (let ((package (make-package name 'unknown)))
			   (set! extra-packages
				 (cons package extra-packages))
			   package)))))))
	;; GLOBALS is a list of the bindings supplied externally.
	(for-each (lambda (global)
		    (if (and global (cdr global))
			(process-globals-info (cdr global)
					      (->namestring (car global))
					      get-package)))
		  globals)
	(for-each
	 (lambda (package description)
	   (let ((parent
		  (let ((parent-name (package-description/parent description)))
		    (and parent-name
			 (not (eq? parent-name 'none))
			 (get-package parent-name #t)))))
	     (set-package/parent! package parent)
	     (if parent
		 (set-package/children!
		  parent
		  (cons package (package/children parent)))))
	   (process-package-description package description get-package))
	 packages
	 descriptions)
	(for-each
	 (lambda (extension)
	   (process-package-description
	    (get-package (package-description/name extension) #f)
	    extension
	    get-package))
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
	  (if (eq? 'unknown (package/parent package))
	      (if (pair? ancestors)
		  (let ((parent (get-package (car ancestors) #t)))
		    (set-package/parent! package parent)
		    (loop parent (cdr ancestors)))
		  (set-package/parent! package #f))))
	(let ((new-expression
	       (lambda () (make-expression package namestring #f))))
	  ;; Unlinked internal names.
	  (for-each-vector-element (vector-ref desc 2)
	    (lambda (name)
	      (bind! package name (new-expression) #f)))
	  ;; Exported bindings.
	  (for-each-exported-name (vector-ref desc 3)
	    (lambda (name exports)
	      (bind! package name (new-expression) #f)
	      (for-each
		(lambda (entry)
		  (let ((external-package (get-package (vector-ref entry 1) #t))
			(external-name
			 (if (fix:= (vector-length entry) 2)
			     (vector-ref entry 0)
			     (vector-ref entry 2))))
		    (link! package name
			   external-package external-name
			   package #f
			   (and (fix:= (vector-length entry) 4)
				(vector-ref entry 3)))))
		exports)))
	  ;; Imported bindings.
	  (for-each-vector-element (vector-ref desc 4)
	    (lambda (entry)
	      (let ((external-package (get-package (vector-ref entry 1) #t))
		    (external-name
		     (if (fix:= (vector-length entry) 2)
			 (vector-ref entry 0)
			 (vector-ref entry 2))))
		(link! external-package external-name
		       package (vector-ref entry 0)
		       package #f
		       (and (fix:= (vector-length entry) 4)
			    (vector-ref entry 3)))))))))))

(define (for-each-exported-name exports receiver)
  (for-each
    (lambda (name.exports)
      (receiver (car name.exports) (cdr name.exports)))
    (let ((len (vector-length exports)))
      (let loop ((i 0)
		 (names.exports '()))
	(if (fix:< i len)
	    (let* ((export (vector-ref exports i))
		   (name (vector-ref export 0))
		   (entry (assq name names.exports)))
	      (if entry
		  (begin
		    (set-cdr! entry (cons export (cdr entry)))
		    (loop (fix:1+ i) names.exports))
		  (loop (fix:1+ i) (cons (list name export) names.exports))))
	    names.exports)))))

(define (package-lookup package name)
  (let package-loop ((package package))
    (or (package/find-binding package name)
	(and (package? (package/parent package))
	     (package-loop (package/parent package))))))

(define (name->package packages name)
  (find (lambda (package)
	  (symbol-list=? name (package/name package)))
	packages))

(define (process-package-description package description get-package)
  (let ((file-cases (package-description/file-cases description)))
    (set-package/files!
     package
     (append! (package/files package)
	      (append-map! (lambda (file-case)
			     (append-map cdr (cdr file-case)))
			   file-cases))))
  (for-each (lambda (name.exports)
	      (let ((destination (get-package (car name.exports) #t)))
		(for-each (lambda (export)
			    (link! package (vector-ref export 1)
				   destination (vector-ref export 0)
				   package #t (vector-ref export 2)))
			  (cdr name.exports))))
	    (package-description/exports description))
  (for-each (lambda (name.imports)
	      (let ((source (get-package (car name.imports) #t)))
		(for-each (lambda (import)
			    (link! source (vector-ref import 1)
				   package (vector-ref import 0)
				   package #t #f))
			  (cdr name.imports))))
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
    (set-value-cell/expressions! value-cell
				 (cons expression
				       (value-cell/expressions value-cell)))))

(define (link! source-package source-name
	       destination-package destination-name
	       owner-package new? deprecated?)
  (let ((source-binding (intern-binding! source-package source-name #f)))
    (make-link source-binding
	       (let ((binding
		      (package/find-binding destination-package
					    destination-name)))
		 (if binding
		     (begin
		       (if (not (eq? (binding/value-cell binding)
				     (binding/value-cell source-binding)))
			   (error "Attempt to reinsert binding:"
				  destination-name destination-package))
		       (if new? (set-binding/new?! binding #t))
		       (if deprecated? (set-binding/deprecated?! binding #t))
		       binding)
		     (let ((binding
			    (make-binding destination-package
					  destination-name
					  (binding/value-cell source-binding)
					  new?)))
		       (package/put-binding! destination-package binding)
		       (if deprecated? (set-binding/deprecated?! binding #t))
		       binding)))
	       owner-package
	       new?)))

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
	  (package/put-binding! package binding)
	  binding))))

(define (make-reference package name expression)
  (let ((add-reference!
	 (lambda (reference)
	   (set-reference/expressions!
	    reference
	    (cons expression (reference/expressions reference)))
	   (set-expression/references!
	    expression
	    (cons reference (expression/references expression))))))
    (let ((reference (package/find-reference package name)))
      (if reference
	  (begin
	    (if (not (memq expression (reference/expressions reference)))
		(add-reference! reference))
	    reference)
	  (let ((reference (%make-reference package name)))
	    (package/put-reference! package reference)
	    (add-reference! reference)
	    reference)))))