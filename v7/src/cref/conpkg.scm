#| -*-Scheme-*-

$Id: conpkg.scm,v 1.11 2001/08/18 04:48:34 cph Exp $

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

;;;; Generate construction program from package model

(declare (usual-integrations)
	 (integrate-external "object"))

(define (construct-external-descriptions pmodel)
  (let* ((packages (pmodel/packages pmodel))
	 (alist
	  (append! (map (lambda (package)
			  (cons package
				(construct-external-description package #f)))
			packages)
		   (map (lambda (package)
			  (cons package
				(construct-external-description package #t)))
			(list-transform-positive
			    (pmodel/extra-packages pmodel)
			  (lambda (package)
			    (pair? (package/files package))))))))
    (vector 'PACKAGE-DESCRIPTIONS	;tag
	    2				;version
	    (list->vector
	     (map cdr
		  (sort alist
		    (lambda (a b)
		      (package-structure<? (car a) (car b))))))
	    (list->vector (map cdr alist)))))

(define (construct-external-description package extension?)
  (call-with-values
      (lambda ()
	(split-bindings-list (package/sorted-bindings package)))
    (lambda (internal exports imports)
      (vector (package/name package)
	      (let loop ((package package))
		(let ((parent (package/parent package)))
		  (if parent
		      (cons (package/name parent) (loop parent))
		      '())))
	      (map (let ((map-files
			  (lambda (clause)
			    (map ->namestring
				 (file-case-clause/files clause)))))
		     (lambda (file-case)
		       (cons (file-case/type file-case)
			     (if (file-case/type file-case)
				 (map (lambda (clause)
					(cons (file-case-clause/keys clause)
					      (map-files clause)))
				      (file-case/clauses file-case))
				 (map-files
				  (car (file-case/clauses file-case)))))))
		   (package/file-cases package))
	      (package/initialization package)
	      (package/finalization package)
	      (list->vector internal)
	      (list->vector
	       (map (lambda (n.l)
		      (list->vector
		       (cons (car n.l)
			     (map (lambda (link)
				    (let ((dest (link/destination link)))
				      (cons (package/name
					     (binding/package dest))
					    (binding/name dest))))
				  (cdr n.l)))))
		    exports))
	      (list->vector
	       (map (lambda (n.s)
		      (let ((name (car n.s))
			    (source (cdr n.s)))
			(if (eq? name (binding/name source))
			    (vector name
				    (package/name (binding/package source)))
			    (vector name
				    (package/name (binding/package source))
				    (binding/name source)))))
		    imports))
	      extension?))))

(define (split-bindings-list bindings)
  (let loop ((bindings bindings) (internal '()) (exports '()) (imports '()))
    (if (pair? bindings)
	(let ((binding (car bindings))
	      (bindings (cdr bindings)))
	  (let ((name (binding/name binding))
		(source (binding/source-binding binding))
		(links
		 (list-transform-positive (binding/links binding) link/new?)))
	    (if (and source
		     (or (binding/new? binding)
			 (pair? links)))
		(if (eq? binding source)
		    (if (pair? links)
			(loop bindings
			      internal
			      (cons (cons name links) exports)
			      imports)
			(loop bindings
			      (cons name internal)
			      exports
			      imports))
		    (loop bindings
			  internal
			  exports
			  (cons (cons name source) imports)))
		(loop bindings internal exports imports))))
	(values (reverse! internal) (reverse! exports) (reverse! imports)))))

(define (package-structure<? x y)
  (cond ((package/topological<? x y) true)
	((package/topological<? y x) false)
	(else (package<? x y))))

(define (package/topological<? x y)
  (and (not (eq? x y))
       (let loop ((y (package/parent y)))
	 (and y
	      (if (eq? x y)
		  true
		  (loop (package/parent y)))))))