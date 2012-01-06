#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Package Model Formatter

(declare (usual-integrations)
	 (integrate-external "object"))

(define (format-packages pmodel port)
  (let ((output? (format-packages-unusual pmodel port))
	(indentation "  ")
	(width 79)
	(packages (pmodel/packages pmodel)))
    (if (pair? packages)
	(begin
	  (if output?
	      (write-separator port))
	  (format-package port indentation width (car packages))
	  (for-each (lambda (package)
		      (write-separator port)
		      (format-package port indentation width package))
		    (cdr packages))))))

(define (format-packages-unusual pmodel port)
  (let ((indentation "  ")
	(width 79)
	(packages (pmodel/packages pmodel))
	(output? #f))
    (let ((free-references
	   (append-map! (lambda (package)
			  (delete-matching-items
			      (package/sorted-references package)
			    reference/binding))
			packages)))
      (if (pair? free-references)
	  (begin
	    (format-references port indentation width "Free References" #f
	      (sort free-references reference<?))
	    (set! output? #t))))
    (receive (undefined multiple) (get-value-cells/unusual packages)
      (if (pair? undefined)
	  (begin
	    (if output?
		(write-separator port))
	    (format-value-cells port indentation width "Undefined Bindings"
				undefined)
	  (set! output? #t)))
      (if (pair? multiple)
	  (begin
	    (if output?
		(write-separator port))
	    (format-value-cells port indentation width
				"Bindings with Multiple Definitions"
				multiple)
	    (set! output? #t))))
    output?))

(define (write-separator port)
  (write-char #\page port)
  (newline port))

(define (format-package port indentation width package)
  (write-package-name "Package" package port)
  (if (package? (package/parent package))
      (write-package-name "Parent" (package/parent package) port))
  (format-package/files port indentation width package)
  (let ((classes
	 (classify-bindings-by-package
	  (lambda (binding)
	    (binding/package (binding/source-binding binding)))
	  (package/sorted-bindings package))))
    (let ((class (assq package classes)))
      (if class
	  (format-package/bindings port indentation width package (cdr class)))
      (for-each (lambda (class)
		  (if (not (eq? package (car class)))
		      (format-package/imports port indentation width package
					      (car class)
					      (cdr class))))
		classes)
      (if class
	  (for-each
	   (lambda (class)
	     (if (not (eq? package (car class)))
		 (format-package/exports port indentation width (car class)
					 (sort (cdr class) binding<?))))
	   (classify-bindings-by-package
	    binding/package
	    (append-map (lambda (binding)
			  (value-cell/bindings (binding/value-cell binding)))
			(cdr class))))))))

(define (format-value-cells port indentation width label value-cells)
  (write-label label port)
  (for-each (lambda (binding)
	      (format-expressions
	       port indentation width #f
	       (string-append
		(binding/name-string binding)
		" "
		(package/name-string (binding/package binding)))
	       (binding/expressions binding)))
	    (sort (map value-cell/source-binding value-cells)
		  binding<?)))

(define (get-value-cells/unusual packages)
  (receive (unlinked linked) (get-value-cells packages)
    (values (delete-matching-items linked
	      (lambda (value-cell)
		(pair? (value-cell/expressions value-cell))))
	    (keep-matching-items (append unlinked linked)
	      (lambda (value-cell)
		(let ((expressions (value-cell/expressions value-cell)))
		  (and (pair? expressions)
		       (pair? (cdr expressions)))))))))

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
		 (package/sorted-bindings package)))
     packages)
    (values unlinked linked)))

(define (write-package-name label package port)
  (write-string label port)
  (write-string ": " port)
  (write-string (package/name-string package) port)
  (newline port))

(define (format-package/files port indentation width package)
  width
  (if (positive? (package/n-files package))
      (begin
	(newline port)
	(write-label "Files" port)
	(for-each (lambda (pathname)
		    (write-string indentation port)
		    (write (->namestring pathname) port)
		    (newline port))
		  (package/files package)))))

(define (format-package/bindings port indentation width package bindings)
  (format-bindings
   port indentation width package bindings
   "Bindings"
   (lambda (binding)
     (let ((name (binding/name-string binding))
	   (expressions (binding/expressions binding)))
       (if (and (>= (package/n-files package) 2)
		(pair? expressions))
	   (string-append name
			  " ("
			  (decorated-string-append
			   "" " " ""
			   (map expression/file expressions))
			  ")")
	   name)))))

(define (format-package/imports port indentation width local-package
				remote-package bindings)
  (format-exports port indentation width local-package remote-package bindings
		  local-map/import "Imports from"))

(define (format-package/exports port indentation width remote-package bindings)
  (format-exports port indentation width remote-package remote-package bindings
		  local-map/export "Exports to"))

(define (format-exports port indentation width local-package remote-package
			bindings local-map label)
  (format-bindings
   port indentation width local-package bindings
   (string-append label " package " (package/name-string remote-package))
   (lambda (destination-binding)
     (receive (local-binding remote-binding)
	 (local-map (binding/source-binding destination-binding)
		    destination-binding)
       (let ((local-name (binding/name local-binding))
	     (remote-name (binding/name remote-binding)))
	 (let ((name-string (name->string local-name)))
	   (if (eq? local-name remote-name)
	       name-string
	       (string-append name-string
			      " ["
			      (name->string remote-name)
			      "]"))))))))

(define (local-map/export source destination)
  (values source destination))

(define (local-map/import source destination)
  (values destination source))

(define (format-bindings port indentation width package
			 bindings label binding->name)
  (newline port)
  (write-label label port)
  (for-each (lambda (binding)
	      (format-expressions
	       port indentation width package
	       (binding->name binding)
	       (append-map reference/expressions
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

(define (format-references port indentation width label package references)
  (write-label label port)
  (for-each
   (lambda (reference)
     (format-expressions port indentation width package
			 (name->string (reference/name reference))
			 (reference/expressions reference)))
   references))

(define (format-expressions port indentation width package name expressions)
  (receive (names pairs)
      (classify-expression-names
       (map (lambda (expression)
	      (expression->name expression package))
	    expressions))
    (write-string indentation port)
    (write-string name port)
    (newline port)
    (let ((indentation (new-indentation indentation)))
      (write-strings/compact port indentation width
			     (sort (map name->string names) string<?))
      (write-items/miser port indentation width
	(sort pairs
	  (lambda (x y)
	    (or (string<? (car x) (car y))
		(and (string=? (car x) (car y))
		     (or (not (pair? (cdr x)))
			 (and (pair? (cdr y))
			      (name<? (cadr x) (cadr y))))))))))))

(define (classify-expression-names names)
  (if (pair? names)
      (receive (symbols pairs) (classify-expression-names (cdr names))
	(if (pair? (car names))
	    (values symbols (cons (car names) pairs))
	    (values (cons (car names) symbols) pairs)))
      (values '() '())))

(define (expression->name expression package)
  (let ((package* (expression/package expression))
	(value-cell (expression/value-cell expression)))
    (let ((binding
	   (and value-cell
		(find-matching-item (value-cell/bindings value-cell)
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

(define (write-label label port)
  (write-string label port)
  (write-string ":" port)
  (newline port))

(define (write-strings/compact port indentation width strings)
  (if (pair? strings)
      (begin
	(let loop ((strings strings) (offset 0) (prefix indentation))
	  (if (pair? strings)
	      (let ((new-offset
		     (+ offset
			(string-length prefix)
			(string-length (car strings)))))
		(if (and (> new-offset width)
			 (> offset 0))
		    (begin
		      (newline port)
		      (loop strings 0 indentation))
		    (begin
		      (write-string prefix port)
		      (write-string (car strings) port)
		      (loop (cdr strings) new-offset " "))))))
	(newline port))))

(define (write-items/miser port indentation width items)
  width
  (for-each (lambda (item)
	      (write-string indentation port)
	      (write item port)
	      (newline port))
	    items))

(define (new-indentation indentation)
  (string-append indentation "    "))

(define (binding/name-string binding)
  (name->string (binding/name binding)))

(define (package/name-string package)
  (package-name->string (package/name package)))

(define (package-name->string name)
  (string-append "("
		 (decorated-string-append "" " " ""
					  (map name->string name))
		 ")"))