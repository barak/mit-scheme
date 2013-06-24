#| -*-Scheme-*-

$Id: forpkg.scm,v 1.12 2003/02/14 18:28:10 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Package Model Formatter

(declare (usual-integrations)
	 (integrate-external "object"))

(define (format-packages pmodel)
  (let ((output? (format-packages-unusual pmodel))
	(port (current-output-port))
	(indentation "  ")
	(width 79)
	(packages (pmodel/packages pmodel)))
    (if (not (null? packages))
	(begin
	  (if output?
	      (output-port/write-string port "\f\n"))
	  (format-package port indentation width (car packages))
	  (for-each (lambda (package)
		      (output-port/write-string port "\f\n")
		      (format-package port indentation width package))
		    (cdr packages))))))

(define (format-packages-unusual pmodel)
  (let ((port (current-output-port))
	(indentation "  ")
	(width 79)
	(packages (pmodel/packages pmodel))
	(output? #f))
    (let ((free-references
	   (append-map! (lambda (package)
			  (list-transform-negative
			      (package/sorted-references package)
			    reference/binding))
			packages)))
      (if (not (null? free-references))
	  (begin
	    (format-references port indentation width "Free References" #f
	      (sort free-references reference<?))
	    (set! output? #t))))
    (with-values (lambda () (get-value-cells/unusual packages))
      (lambda (undefined multiple)
	(if (not (null? undefined))
	    (begin
	      (if output?
		  (output-port/write-string port "\f\n"))
	      (format-value-cells port indentation width "Undefined Bindings"
				  undefined)
	    (set! output? #t)))
	(if (not (null? multiple))
	    (begin
	      (if output?
		  (output-port/write-string port "\f\n"))
	      (format-value-cells port indentation width
				  "Bindings with Multiple Definitions"
				  multiple)
	      (set! output? #t)))))
    output?))

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
		 (package/sorted-bindings package)))
     packages)
    (values unlinked linked)))

(define (write-package-name label package port)
  (output-port/write-string port label)
  (output-port/write-string port ": ")
  (output-port/write-string port (package/name-string package))
  (output-port/write-char port #\newline))

(define (format-package/files port indentation width package)
  width
  (if (positive? (package/n-files package))
      (begin
	(output-port/write-char port #\newline)
	(write-label "Files" port)
	(for-each (lambda (pathname)
		    (output-port/write-string port indentation)
		    (output-port/write-char port #\")
		    (output-port/write-string port (->namestring pathname))
		    (output-port/write-char port #\")
		    (output-port/write-char port #\newline))
		  (package/files package)))))

(define (format-package/bindings port indentation width package bindings)
  (format-bindings
   port indentation width package bindings
   "Bindings"
   (lambda (binding)
     (let* ((name (binding/name-string binding))
	    (expressions (binding/expressions binding)))
       (if (or (< (package/n-files package) 2)
	       (null? expressions))
	   name
	   (apply string-append
		  name
		  " "
		  (let loop ((expressions expressions)
			     (p "("))
		    (cons p
			  (cons (expression/file (car expressions))
				(if (null? (cdr expressions))
				    (list ")")
				    (loop (cdr expressions) " ")))))))))))

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
     (with-values
	 (lambda ()
	   (local-map (binding/source-binding destination-binding)
		      destination-binding))
       (lambda (local-binding remote-binding)
	 (let ((local-name (binding/name local-binding))
	       (remote-name (binding/name remote-binding)))
	   (let ((name-string (binding-name->string local-name)))
	     (if (eq? local-name remote-name)
		 name-string
		 (string-append name-string
				" ["
				(binding-name->string remote-name)
				"]")))))))))

(define (local-map/export source destination)
  (values source destination))

(define (local-map/import source destination)
  (values destination source))

(define (format-bindings port indentation width package
			 bindings label binding->name)
  (output-port/write-char port #\newline)
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
			 (binding-name->string (reference/name reference))
			 (reference/expressions reference)))
   references))

(define (format-expressions port indentation width package name expressions)
  (with-values
      (lambda ()
	(classify-expression-names
	 (map (lambda (expression)
		(expression->name expression package))
	      expressions)))
    (lambda (symbols pairs)
      (output-port/write-string port indentation)
      (output-port/write-string port name)
      (output-port/write-char port #\newline)
      (let ((indentation (new-indentation indentation)))
	(write-strings/compact port indentation width
			       (map symbol-name (sort symbols symbol<?)))
	(write-items/miser port indentation width
	  (lambda (item port)
	    (output-port/write-char port #\()
	    (output-port/write-char port #\")
	    (output-port/write-string port (car item))
	    (output-port/write-char port #\")
	    (if (not (null? (cdr item)))
		(begin
		  (output-port/write-char port #\space)
		  (output-port/write-string port (symbol-name (cadr item)))))
	    (output-port/write-char port #\)))
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

(define (write-label label port)
  (output-port/write-string port label)
  (output-port/write-string port ":")
  (output-port/write-char port #\newline))

(define (write-strings/compact port indentation width strings)
  (if (not (null? strings))
      (begin
	(let loop ((strings strings) (offset 0) (prefix indentation))
	  (if (not (null? strings))
	      (let ((length (string-length (car strings))))
		(let ((new-offset (+ offset (string-length prefix) length)))
		  (if (and (> new-offset width)
			   (not (zero? offset)))
		      (begin
			(output-port/write-char port #\newline)
			(loop strings 0 indentation))
		      (begin
			(output-port/write-string port prefix)
			(output-port/write-string port (car strings))
			(loop (cdr strings) new-offset " ")))))))
	(output-port/write-char port #\newline))))

(define (write-items/miser port indentation width write-item items)
  width
  (for-each (lambda (item)
	      (output-port/write-string port indentation)
	      (write-item item port)
	      (output-port/write-char port #\newline))
	    items))

(define (new-indentation indentation)
  (string-append indentation "    "))

(define-integrable (binding/name-string binding)
  (binding-name->string (binding/name binding)))

(define (binding-name->string name)
  (if (symbol? name)
      (symbol-name name)
      (write-to-string name)))

(define-integrable (package/name-string package)
  (package-name->string (package/name package)))

(define (package-name->string name)
  (if (null? name)
      "()"
      (apply string-append
	     (let loop ((name name) (p "("))
	       (cons p
		     (cons (binding-name->string (car name))
			   (if (null? (cdr name))
			       (list ")")
			       (loop (cdr name) " "))))))))