#| -*-Scheme-*-

$Id: conpkg.scm,v 1.12 2001/08/20 02:48:57 cph Exp $

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

(define (construct-external-description package extension?)
  (call-with-values (lambda () (split-links package))
    (lambda (exports imports)
      (vector (package/name package)
	      (let loop ((package package))
		(let ((parent (package/parent package)))
		  (if parent
		      (cons (package/name parent) (loop parent))
		      '())))
	      (map (lambda (file-case)
		     (cons (file-case/type file-case)
			   (if (file-case/type file-case)
			       (map (lambda (clause)
				      (cons (file-case-clause/keys clause)
					    (map-files clause)))
				    (file-case/clauses file-case))
			       (map-files
				(car (file-case/clauses file-case))))))
		   (package/file-cases package))
	      (package/initialization package)
	      (package/finalization package)
	      (list->vector
	       (map binding/name
		    (list-transform-positive (package/sorted-bindings package)
		      (lambda (binding)
			(and (binding/new? binding)
			     (binding/internal? binding)
			     (not (there-exists? (binding/links binding)
				    (lambda (link)
				      (memq link
					    (package/links package))))))))))
	      (list->vector
	       (map (lambda (link)
		      (let ((source (link/source link))
			    (destination (link/destination link)))
			(let ((sn (binding/name source))
			      (dp (package/name (binding/package destination)))
			      (dn (binding/name destination)))
			  (if (eq? sn dn)
			      (vector sn dp)
			      (vector sn dp dn)))))
		    exports))
	      (list->vector
	       (map (lambda (link)
		      (let ((source (link/source link))
			    (destination (link/destination link)))
			(let ((dn (binding/name destination))
			      (sp (package/name (binding/package source)))
			      (sn (binding/name source)))
			  (if (eq? dn sn)
			      (vector dn sp)
			      (vector dn sp sn)))))
		    imports))
	      extension?))))

(define (split-links package)
  (let loop ((links (package/links package)) (exports '()) (imports '()))
    (if (pair? links)
	(let ((link (car links))
	      (links (cdr links)))
	  (if (eq? (binding/package (link/source link)) package)
	      (loop links (cons link exports) imports)
	      (loop links exports (cons link imports))))
	(values exports imports))))

(define (map-files clause)
  (map ->namestring (file-case-clause/files clause)))