#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Generate construction program from package model

(declare (usual-integrations)
	 (integrate-external "object"))

(define (construct-external-descriptions pmodel)
  (vector 'PACKAGE-DESCRIPTIONS		;tag
	  2				;version
	  (list->vector
	   (map cdr
		(sort (append!
		       (map (lambda (package)
			      (cons package (package->external package #f)))
			    (pmodel/packages pmodel))
		       (map (lambda (package)
			      (cons package (package->external package #t)))
			    (new-extension-packages pmodel)))
		      (lambda (a b)
			(package-structure<? (car a) (car b))))))
	  (list->vector
	   (map package-load->external
		(list-transform-positive (pmodel/loads pmodel)
		  (lambda (load)
		    (or (pair? (package-load/file-cases load))
			(pair? (package-load/initializations load))
			(pair? (package-load/finalizations load)))))))))

(define (new-extension-packages pmodel)
  (list-transform-positive (pmodel/extra-packages pmodel)
    (lambda (package)
      (or (there-exists? (package/links package) link/new?)
	  (there-exists? (package/sorted-bindings package)
	    new-internal-binding?)))))

(define (new-internal-binding? binding)
  (and (binding/new? binding)
       (binding/internal? binding)
       (not (there-exists? (binding/links binding)
	      (let ((package (binding/package binding)))
		(lambda (link)
		  (eq? (link/owner link) package)))))))

(define (package-structure<? x y)
  (cond ((package/topological<? x y) #t)
	((package/topological<? y x) #f)
	(else (package<? x y))))

(define (package/topological<? x y)
  (and (not (eq? x y))
       (let loop ((y (package/parent y)))
	 (and (package? y)
	      (if (eq? x y)
		  #t
		  (loop (package/parent y)))))))

(define (package->external package extension?)
  (call-with-values (lambda () (split-links package))
    (lambda (exports imports)
      (vector (package/name package)
	      (let loop ((package package))
		(let ((parent (package/parent package)))
		  (if (package? parent)
		      (cons (package/name parent) (loop parent))
		      '())))
	      (list->vector
	       (map binding/name
		    (list-transform-positive (package/sorted-bindings package)
		      new-internal-binding?)))
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
	  (if (link/new? link)
	      (if (eq? (binding/package (link/source link)) package)
		  (loop links (cons link exports) imports)
		  (loop links exports (cons link imports)))
	      (loop links exports imports)))
	(values exports imports))))

(define (package-load->external description)
  (vector (package/name (package-load/package description))
	  (list->vector
	   (map (lambda (file-case)
		  (if (file-case/type file-case)
		      (cons (file-case/type file-case)
			    (map-clauses file-case))
		      (map-files (car (file-case/clauses file-case)))))
		(package-load/file-cases description)))
	  (list->vector (package-load/initializations description))
	  (list->vector (package-load/finalizations description))))

(define (map-clauses file-case)
  (list->vector
   (map (lambda (clause)
	  (cons (let ((keys (file-case-clause/keys clause)))
		  (if (list? keys)
		      (list->vector keys)
		      keys))
		(map-files clause)))
	(file-case/clauses file-case))))

(define (map-files clause)
  (list->vector (map ->namestring (file-case-clause/files clause))))