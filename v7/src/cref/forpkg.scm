#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/forpkg.scm,v 1.2 1988/06/14 10:29:46 cph Exp $

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

;;;; Package Model Formatter

(declare (usual-integrations)
	 (integrate-external "object"))

(define (format-packages pmodel)
  (let ((indentation "  ")
	(width 79)
	(root-package (pmodel/root-package pmodel))	(packages (pmodel/packages pmodel)))
    (let ((free-references
	   (mapcan (lambda (package)
		     (list-transform-negative
			 (btree-fringe (package/references package))
		       reference/binding))
		   packages)))
      (if (not (null? free-references))
	  (begin
	    (format-references indentation width "Free References" false
	      (sort free-references reference<?))
	    (write-string "\f\n"))))
    (with-values (lambda () (get-value-cells/unusual packages))
      (lambda (undefined multiple)
	(if (not (null? undefined))
	    (begin
	      (format-value-cells indentation width "Undefined Bindings"
				  undefined)
	      (write-string "\f\n")))
	(if (not (null? multiple))
	    (begin
	      (format-value-cells indentation width
				  "Bindings with Multiple Definitions"
				  multiple)
	      (write-string "\f\n")))))
    (if (not (memq root-package packages))
	(begin
	  (write-label "Global References")
	  (for-each
	   (lambda (binding)
	     (let ((references (binding/references binding)))
	       (if (not (null? references))
		   (format-expressions
		    indentation width root-package
		    (write-to-string (binding/name binding))
		    (mapcan (lambda (reference)
			      (list-copy (reference/expressions reference)))
			    references)))))
	   (btree-fringe (package/bindings root-package)))
	  (write-string "\f\n")))
    (format-references
     indentation width "Primitives" root-package
     (btree-fringe (package/references (pmodel/primitive-package pmodel))))
    (for-each (lambda (package)
		(write-string "\f\n")
		(format-package indentation width package))
	      packages)))

(define (format-package indentation width package)
  (write-package-name "Package" package)
  (if (package/parent package)
      (write-package-name "Parent" (package/parent package)))
  (format-package/files indentation width package)
  (let ((classes
	 (classify-bindings-by-package
	  (lambda (binding)
	    (binding/package (binding/source-binding binding)))
	  (btree-fringe (package/bindings package)))))
    (let ((class (assq package classes)))
      (if class
	  (format-package/bindings indentation width package (cdr class)))
      (for-each (lambda (class)
		  (if (not (eq? package (car class)))
		      (format-package/imports indentation width package
					      (car class)
					      (cdr class))))
		classes)
      (if class
	  (for-each
	   (lambda (class)
	     (if (not (eq? package (car class)))
		 (format-package/exports indentation width (car class)
					 (sort (cdr class) binding<?))))
	   (classify-bindings-by-package
	    binding/package
	    (mapcan (lambda (binding)
		      (list-copy
		       (value-cell/bindings (binding/value-cell binding))))
		    (cdr class))))))))

(define (format-value-cells indentation width label value-cells)
  (write-label label)
  (for-each (lambda (binding)
	      (format-expressions
	       indentation width false
	       (string-append
		(write-to-string (binding/name binding))
		" "
		(write-to-string (package/name (binding/package binding))))
	       (binding/expressions binding)))
	    (sort (map value-cell/source-binding value-cells)
		  binding<?)))

(define (get-value-cells/unusual packages)
  (with-values (lambda () (get-value-cells packages))
    (lambda (unlinked linked)
      (values
       (list-transform-positive linked
	 (lambda (value-cell)
	   (null? (value-cell/expressions value-cell))))
       (list-transform-positive (append unlinked linked)
	 (lambda (value-cell)
	   (let ((expressions (value-cell/expressions value-cell)))
	     (and (not (null? expressions))
		  (not (null? (cdr expressions)))))))))))

(define (get-value-cells packages)
  (let ((unlinked '())
	(linked '()))
    (for-each
     (lambda (package)
       (for-each (lambda (binding)
		   (let ((value-cell (binding/value-cell binding)))
		     (cond ((null? (cdr (value-cell/bindings value-cell)))
			    (set! unlinked (cons value-cell unlinked)))
			   ((not (memq value-cell linked))
			    (set! linked (cons value-cell linked))))))
		 (btree-fringe (package/bindings package))))
     packages)
    (values unlinked linked)))

(define (write-package-name label package)
  (write-string label)
  (write-string ": ")
  (write (package/name package))
  (newline))

(define (format-package/files indentation width package)
  width
  (if (positive? (package/n-files package))
      (begin
	(newline)
	(write-label "Files")
	(for-each (lambda (pathname)
		    (write-string indentation)
		    (write (pathname->string pathname))
		    (newline))
		  (package/files package)))))

(define (format-package/bindings indentation width package bindings)
  (format-bindings
   indentation width package bindings
   "Bindings"
   (lambda (binding)
     (let ((name (write-to-string (binding/name binding))))
       (if (< (package/n-files package) 2)
	   name
	   (string-append
	    name
	    " "
	    (write-to-string
	     (map expression/file (binding/expressions binding)))))))))

(define (format-package/imports indentation width local-package remote-package
				bindings)
  (format-exports indentation width local-package remote-package bindings
		  local-map/import "Imports from"))

(define (format-package/exports indentation width remote-package bindings)
  (format-exports indentation width remote-package remote-package bindings
		  local-map/export "Exports to"))

(define (format-exports indentation width local-package remote-package
			bindings local-map label)
  (format-bindings
   indentation width local-package bindings
   (string-append label
		  " package "
		  (write-to-string (package/name remote-package)))
   (lambda (destination-binding)
     (with-values
	 (lambda ()
	   (local-map (binding/source-binding destination-binding)
		      destination-binding))
       (lambda (local-binding remote-binding)
	 (let ((local-name (binding/name local-binding))
	       (remote-name (binding/name remote-binding)))
	   (let ((name-string (write-to-string local-name)))
	     (if (eq? local-name remote-name)
		 name-string
		 (string-append name-string
				" ["
				(write-to-string remote-name)
				"]")))))))))

(define (local-map/export source destination)
  (values source destination))

(define (local-map/import source destination)
  (values destination source))

(define (format-bindings indentation width package
			 bindings label binding->name)
  (newline)
  (write-label label)
  (for-each (lambda (binding)
	      (format-expressions
	       indentation width package
	       (binding->name binding)
	       (mapcan (lambda (reference)
			 (list-copy (reference/expressions reference)))
		       (binding/references binding))))
	    bindings))

(define (classify-bindings-by-package binding->package bindings)
  (let ((classes '()))
    (for-each
     (lambda (binding)
       (let ((package (binding->package binding)))
	 (let ((entry (assq package classes)))
	   (if entry
	       (set-cdr! entry (cons binding (cdr entry)))
	       (set! classes (cons (list package binding) classes))))))
     bindings)
    (for-each (lambda (class)
		(set-cdr! class (reverse! (cdr class))))
	      classes)
    (sort classes
	  (lambda (x y)
	    (package<? (car x) (car y))))))

(define (format-references indentation width label package references)
  (write-label label)
  (for-each (lambda (reference)
	      (format-expressions indentation width package
				  (write-to-string (reference/name reference))
				  (reference/expressions reference)))
	    references))

(define (format-expressions indentation width package name expressions)
  (with-values (lambda ()
		 (classify-expression-names
		  (map (lambda (expression)
			 (expression->name expression package))
		       expressions)))
    (lambda (symbols pairs)
      (write-string indentation)
      (write-string name)
      (newline)
      (let ((indentation (new-indentation indentation)))
	(write-strings/compact indentation width
			       (map write-to-string (sort symbols symbol<?)))
	(write-items/miser indentation width write
	  (sort pairs
	    (lambda (x y)
	      (or (string<? (car x) (car y))
		  (and (string=? (car x) (car y))
		       (or (null? (cdr x))
			   (and (not (null? (cdr y)))
				(symbol<? (cadr x) (cadr y)))))))))))))

(define (classify-expression-names names)
  (if (null? names)
      (values '() '())
      (with-values (lambda () (classify-expression-names (cdr names)))
	(lambda (symbols pairs)
	  (if (pair? (car names))
	      (values symbols (cons (car names) pairs))
	      (values (cons (car names) symbols) pairs))))))

(define (expression->name expression package)
  (let ((package* (expression/package expression))
	(value-cell (expression/value-cell expression)))
    (let ((binding
	   (and value-cell
		(list-search-positive (value-cell/bindings value-cell)
		  (lambda (binding)
		    (eq? package* (binding/package binding)))))))
      (if binding
	  (let ((name (binding/name binding)))
	    (if (and package
		     (let ((binding* (package/find-binding package name)))
		       (and binding*
			    (eq? (binding/value-cell binding)
				 (binding/value-cell binding*)))))
		name
		(list (expression/file expression) name)))
	  (list (expression/file expression))))))

(define (write-label label)
  (write-string label)
  (write-string ":")
  (newline))

(define (write-strings/compact indentation width strings)
  (if (not (null? strings))
      (begin
	(let loop ((strings strings) (offset 0) (prefix indentation))
	  (if (not (null? strings))
	      (let ((length (string-length (car strings))))
		(let ((new-offset (+ offset (string-length prefix) length)))
		  (if (and (> new-offset width)
			   (not (zero? offset)))
		      (begin (newline)
			     (loop strings 0 indentation))
		      (begin (write-string prefix)
			     (write-string (car strings))
			     (loop (cdr strings) new-offset " ")))))))
	(newline))))

(define (write-items/miser indentation width write-item items)
  width
  (for-each (lambda (item)
	      (write-string indentation)
	      (write-item item)
	      (newline))
	    items))

(define (new-indentation indentation)
  (string-append indentation "    "))