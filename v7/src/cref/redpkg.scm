#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/redpkg.scm,v 1.1 1988/06/13 12:38:30 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Package Model Reader

(declare (usual-integrations)
	 (integrate-external "object"))

(define (read-package-model filename)
  (let* ((pathname (pathname->absolute-pathname (->pathname filename)))
	 (default-pathname (pathname-directory-path pathname)))
    (with-values
	(lambda ()
	  (sort-descriptions
	   (map (lambda (expression)
		  (parse-package-expression expression))
		(read-package-description-file pathname))))
      (lambda (packages globals)
	(let ((pmodel (descriptions->pmodel packages default-pathname)))
	  (for-each
	   (let ((root-package (pmodel/root-package pmodel)))
	     (lambda (pathname)
	       (for-each (let ((expression
				(make-expression root-package
						 (pathname->string pathname)
						 false)))
			   (lambda (name)
			     (bind! root-package name expression)))
			 (fasload
			  (merge-pathnames (pathname-new-type pathname "glob")
					   default-pathname)))))
	   globals)
	  pmodel)))))

(define (sort-descriptions descriptions)
  (let loop
      ((descriptions descriptions)
       (packages '())
       (globals '()))
    (cond ((null? descriptions)
	   (values (reverse! packages) globals))
	  ((package-description? (car descriptions))
	   (loop (cdr descriptions)
		 (cons (car descriptions) packages)
		 globals))
	  ((and (pair? (car descriptions))
		(eq? (car (car descriptions)) 'GLOBAL-DEFINITIONS))
	   (loop (cdr descriptions)
		 packages
		 (append globals (cdr (car descriptions)))))
	  (else
	   (error "Illegal description" (car descriptions))))))

(define (read-package-description-file pathname)
  (read-file (pathname-default-type pathname "pkg")))

(define (read-file-analyses! pmodel)
  (for-each (lambda (package)
	      (for-each (lambda (pathname)
			  (read-file-analysis! pmodel package pathname))
			(package/files package)))
	    (pmodel/packages pmodel)))

(define (read-file-analysis! pmodel package pathname)
  (let ((filename (pathname->string pathname))
	(root-package (pmodel/root-package pmodel))
	(primitive-package (pmodel/primitive-package pmodel)))
    (for-each (lambda (entry)
		(let ((name (vector-ref entry 0))
		      (expression
		       (make-expression package
					filename
					(vector-ref entry 4))))
		  (let ((intern!
			 (lambda (name)
			   (cond ((symbol? name)
				  (make-reference package name expression))
				 ((primitive-procedure? name)
				  (make-reference
				   primitive-package
				   (primitive-procedure-name name)
				   expression))
				 ((access? name)
				  (if (access-environment name)
				      (error "Non-root access" name))
				  (make-reference root-package
						  (access-name name)
						  expression))
				 (else
				  (error "Illegal reference name" name))))))
		    (for-each intern! (vector-ref entry 1))
		    (for-each intern! (vector-ref entry 2))
		    (for-each intern! (vector-ref entry 3)))
		  (if name
		      (bind! package name expression))))
	      (read-analyzed-file
	       (merge-pathnames (pathname-new-type pathname "bin")
				(pmodel/default-pathname pmodel))))))

(define (resolve-references! pmodel)
  (for-each (lambda (package)
	      (for-each resolve-reference!
			(btree-fringe (package/references package))))
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

(define (parse-package-expression expression)
  (if (not (pair? expression))
      (error "package expression not a pair" expression))
  (case (car expression)
    ((DEFINE-PACKAGE)
     (parse-package-description (parse-name (cadr expression))
				(cddr expression)))
    ((GLOBAL-DEFINITIONS)
     (let ((filenames (cdr expression)))
       (if (not (check-list filenames string?))
	   (error "illegal filenames" filenames))
       (cons 'GLOBAL-DEFINITIONS (map parse-filename filenames))))
    (else
     (error "unrecognized expression keyword" (car expression)))))

(define (parse-package-description name options)
  (let ((none "none"))
    (let ((file-cases '())
	  (parent none)
	  (initialization none)
	  (exports '())
	  (imports '()))
      (if (not (list? options))
	  (error "options not list" options))
      (for-each (lambda (option)
		  (if (not (pair? option))
		      (error "Illegal option" option))
		  (case (car option)
		    ((FILES)
		     (set! file-cases
			   (cons (parse-filenames (cdr option)) file-cases)))
		    ((FILE-CASE)
		     (set! file-cases
			   (cons (parse-file-case (cdr option)) file-cases)))
		    ((PARENT)
		     (if (not (eq? parent none))
			 (error "option reoccurs" option))
		     (if (not (and (pair? (cdr option)) (null? (cddr option))))
			 (error "illegal option" option))
		     (set! parent (parse-name (cadr option))))
		    ((EXPORT)
		     (set! exports (cons (parse-export (cdr option)) exports)))
		    ((IMPORT)
		     (set! imports (cons (parse-import (cdr option)) imports)))
		    ((INITIALIZATION)
		     (if (not (eq? initialization none))
			 (error "option reoccurs" option))
		     (set! initialization (parse-initialization (cdr option))))
		    (else
		     (error "unrecognized option keyword" (car option)))))
		options)
      (make-package-description
       name
       file-cases
       (if (eq? parent none) 'NONE parent)
       (if (eq? initialization none) '#F initialization)
       (reverse! exports)
       (reverse! imports)))))

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
  (string->pathname filename))

(define (parse-initialization initialization)
  (if (not (and (pair? initialization) (null? (cdr initialization))))
      (error "illegal initialization" initialization))
  (car initialization))

(define (parse-import import)
  (if (not (and (pair? import) (check-list (cdr import) symbol?)))
      (error "illegal import" import))
  (cons (parse-name (car import)) (cdr import)))

(define (parse-export export)
  (if (not (and (pair? export) (check-list (cdr export) symbol?)))
      (error "illegal export" export))
  (cons (parse-name (car export)) (cdr export)))

(define (check-list items predicate)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items))
	    false)
	(null? items))))

;;;; Packages

(define (package-lookup package name)
  (let package-loop ((package package))
    (or (package/find-binding package name)
	(and (package/parent package)
	     (package-loop (package/parent package))))))

(define (name->package packages name)
  (list-search-positive packages
    (lambda (package)
      (symbol-list=? name (package/name package)))))

(define (descriptions->pmodel descriptions default-pathname)
  (let ((packages
	 (map (lambda (description)
		(make-package
		 (package-description/name description)
		 (package-description/file-cases description)
		 (package-description/initialization description)
		 'UNKNOWN))
	      descriptions))
	(extra-packages '()))
    (let ((root-package
	   (or (name->package packages '())
	       (make-package '() '() '#F false))))
      (let ((get-package
	     (lambda (name)
	       (if (null? name)
		   root-package
		   (or (name->package packages name)
		       (let ((package (make-package name '() #F 'UNKNOWN)))
			 (set! extra-packages (cons package extra-packages))
			 package))))))
	(for-each (lambda (package description)
		    (let ((parent
			   (let ((parent-name
				  (package-description/parent description)))
			     (and (not (eq? parent-name 'NONE))
				  (get-package parent-name)))))
		      (set-package/parent! package parent)
		      (if parent
			  (set-package/children!
			   parent
			   (cons package (package/children parent)))))
		    (for-each (lambda (export)
				(let ((destination (get-package (car export))))
				  (for-each (lambda (name)
					      (link! package name
						     destination name))
					    (cdr export))))
			      (package-description/exports description))
		    (for-each (lambda (import)
				(let ((source (get-package (car import))))
				  (for-each (lambda (name)
					      (link! source name package name))
					    (cdr import))))
			      (package-description/imports description)))
		  packages
		  descriptions))
      (make-pmodel root-package
		   (make-package primitive-package-name '() '() false)
		   packages
		   extra-packages
		   default-pathname))))

(define primitive-package-name
  (list (string->uninterned-symbol "primitives")))

;;;; Binding and Reference

(define (bind! package name expression)
  (let ((value-cell (binding/value-cell (intern-binding! package name))))
    (set-expression/value-cell! expression value-cell)
    (set-value-cell/expressions!
     value-cell
     (cons expression (value-cell/expressions value-cell)))))

(define (link! source-package source-name destination-package destination-name)
  (let ((source-binding (intern-binding! source-package source-name)))
    (make-link source-binding
	       (btree-insert! (package/bindings destination-package)
			      destination-name
		 (lambda (destination-name)
		   (make-binding destination-package
				 destination-name
				 (binding/value-cell source-binding)))
		 (lambda (binding)
		   binding
		   (error "Attempt to reinsert binding" destination-name))
		 identity-procedure))))

(define (intern-binding! package name)
  (btree-insert! (package/bindings package) name
    (lambda (name)
      (let ((value-cell (make-value-cell)))
	(let ((binding (make-binding package name value-cell)))
	  (set-value-cell/source-binding! value-cell binding)
	  binding)))
    identity-procedure
    identity-procedure))

(define (make-reference package name expression)
  (let ((add-reference!
	 (lambda (reference)
	   (set-reference/expressions!
	    reference
	    (cons expression (reference/expressions reference)))
	   (set-expression/references!
	    expression
	    (cons reference (expression/references expression))))))
    (btree-insert! (package/references package) name
      (lambda (name)
	(%make-reference package name))
      (lambda (reference)
	(if (not (memq expression (reference/expressions reference)))
	    (add-reference! reference))
	reference)
      (lambda (reference)
	(add-reference! reference)
	reference))))