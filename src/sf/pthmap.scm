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

;;;; Pathname Maps
;;; package: (scode-optimizer)

(declare (usual-integrations))

(define pathname-map/make)
(define pathname-map?)
(define pathname-map/lookup)
(define pathname-map/insert!)

(let ()

(define pathname-map/tag "pathname-map")
(define pathname-map/root-node cdr)

(declare (integrate-operator node/make))

(define (node/make)
  (cons unbound-value '()))

(define unbound-value "unbound-value")
(define node/value car)
(define set-node/value! set-car!)
(define node/alist cdr)
(define set-node/alist! set-cdr!)

(define (node/associate node key)
  (let ((entry (assoc key (node/alist node))))
    (and entry
	 (cdr entry))))

(define (make-node-list pathname)
  (cons-if (pathname-device pathname)
	   (append (pathname-directory pathname)
		   (cons-if (pathname-name pathname)
			    (cons-if (pathname-type pathname)
				     (cons-if (pathname-version pathname)
					      '()))))))

(declare (integrate-operator cons-if))

(define (cons-if item rest)
  (if item
      (cons item rest)
      rest))

(define (find-node node node-list)
  (if (null? node-list)
      node
      (let ((node (node/associate node (car node-list))))
	(and node
	     (find-node node (cdr node-list))))))

(define (find-or-create-node node node-list)
  (if (null? node-list)
      node
      (let ((next (node/associate node (car node-list))))
	(if next
	    (find-or-create-node next (cdr node-list))
	    (create-node node node-list)))))

(define (create-node node node-list)
  (let ((next (node/make)))
    (set-node/alist! node
		     (cons (cons (car node-list) next)
			   (node/alist node)))
    (if (null? (cdr node-list))
	next
	(create-node next (cdr node-list)))))

(set! pathname-map/make
  (named-lambda (pathname-map/make)
    (cons pathname-map/tag (node/make))))

(set! pathname-map?
  (named-lambda (pathname-map? object)
    (and (pair? object)
	 (eq? (car object) pathname-map/tag))))

(define-print-method pathname-map?
  (standard-print-method 'pathname-map))

(set! pathname-map/lookup
  (named-lambda (pathname-map/lookup map pathname if-found if-not)
    (let ((node
	   (find-node (pathname-map/root-node map)
		      (make-node-list pathname))))
      (if node
	  (let ((value (node/value node)))
	    (if (eq? value unbound-value)
		(if-not)
		(if-found value)))
	  (if-not)))))

(set! pathname-map/insert!
  (named-lambda (pathname-map/insert! map pathname value)
    (set-node/value! (find-or-create-node (pathname-map/root-node map)
					  (make-node-list pathname))
		     value)))

unspecific)