#| -*-Scheme-*-

$Id: conpkg.scm,v 1.9 2001/08/15 02:59:35 cph Exp $

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
	  (map (lambda (package)
		 (cons package (construct-external-description package)))
	       packages)))
    (vector 'PACKAGE-DESCRIPTIONS	;tag
	    2				;version
	    (list->vector
	     (map (lambda (package)
		    (cdr (assq package alist)))
		  (sort packages package-structure<?)))
	    (list->vector (map cdr alist)))))

(define (construct-external-description package)
  (call-with-values
      (lambda ()
	(split-bindings-list (package/sorted-bindings package)))
    (lambda (internal external)
      (vector (package/name package)
	      (let ((parent (package/parent package)))
		(if parent
		    (package/name parent)
		    'NONE))
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
	      (list->vector
	       (map binding/name
		    (list-transform-negative internal
		      (lambda (binding)
			(pair? (binding/links binding))))))
	      (list->vector
	       (map (lambda (binding)
		      (list->vector
		       (cons (binding/name binding)
			     (map (lambda (link)
				    (let ((dest (link/destination link)))
				      (cons (package/name
					     (binding/package dest))
					    (binding/name dest))))
				  (binding/links binding)))))
		    (list-transform-positive internal
		      (lambda (binding)
			(pair? (binding/links binding))))))
	      (list->vector
	       (map (lambda (binding)
		      (let ((source (binding/source-binding binding)))
			(if (eq? (binding/name binding) (binding/name source))
			    (vector (binding/name binding)
				    (package/name (binding/package source)))
			    (vector (binding/name binding)
				    (package/name (binding/package source))
				    (binding/name source)))))
		    external))))))

(define (split-bindings-list bindings)
  (let loop ((bindings bindings) (internal '()) (external '()))
    (if (pair? bindings)
	(if (binding/internal? (car bindings))
	    (loop (cdr bindings)
		  (cons (car bindings) internal)
		  external)
	    (loop (cdr bindings)
		  internal
		  (cons (car bindings) external)))
	(values (reverse! internal) (reverse! external)))))

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