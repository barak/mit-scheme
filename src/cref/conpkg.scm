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

;;;; Generate construction program from package model

(declare (usual-integrations)
	 (integrate-external "object"))

(define (construct-external-descriptions pmodel)
  (vector 'package-descriptions		;tag
	  2				;version
	  (list->vector
	   (map cdr
		(sort (append!
		       (map (lambda (package)
			      (cons (package/ancestry package)
				    (package->external package #f)))
			    (pmodel/packages pmodel))
		       (map (lambda (package)
			      (cons (package/ancestry package)
				    (package->external package #t)))
			    (new-extension-packages pmodel)))
		      (lambda (a b)
			(package-ancestry<? (car a) (car b))))))
	  (list->vector
	   (map package-load->external
		(filter (lambda (load)
			  (or (pair? (package-load/file-cases load))
			      (pair? (package-load/initializations load))
			      (pair? (package-load/finalizations load))))
			(pmodel/loads pmodel))))))

(define (new-extension-packages pmodel)
  (filter (lambda (package)
	    (or (any link/new? (package/links package))
		(any new-internal-binding? (package/bindings package))))
	  (pmodel/extra-packages pmodel)))

(define (new-internal-binding? binding)
  (and (binding/new? binding)
       (binding/internal? binding)
       (not (any (let ((package (binding/package binding)))
		   (lambda (link)
		     (eq? (link/owner link) package)))
		 (binding/links binding)))))

(define (package/ancestry package)
  (let loop ((parent (package/parent package))
	     (ancestors (list (package/name package))))
    (if parent
	(loop (package/parent parent)
	      (cons (package/name parent) ancestors))
	ancestors)))

(define (package-ancestry<? x y)
  (cond ((symbol-list<? (car x) (car y)) #t)
	((symbol-list<? (car y) (car x)) #f)
	((null? (cdr x)) (not (null? (cdr y))))
	((null? (cdr y)) #f)
	(else (package-ancestry<? (cdr x) (cdr y)))))

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
		    (filter new-internal-binding?
			    (package/bindings package))))
	      (list->vector
	       (map (lambda (link)
		      (let ((source (link/source link))
			    (destination (link/destination link)))
			(let ((sn (binding/name source))
			      (dp (package/name (binding/package destination)))
			      (dn (binding/name destination))
			      (d? (and (binding/deprecated? destination)
				       'deprecated)))
			  (if (and (not d?) (eq? sn dn))
			      (vector sn dp)
			      (vector sn dp dn d?)))))
		    exports))
	      (list->vector
	       (map (lambda (link)
		      (let ((source (link/source link))
			    (destination (link/destination link)))
			(let ((dn (binding/name destination))
			      (sp (package/name (binding/package source)))
			      (sn (binding/name source))
			      (d? (and (binding/deprecated? source)
				       'deprecated)))
			  (if (and (not d?) (eq? dn sn))
			      (vector dn sp)
			      (vector dn sp sn d?)))))
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