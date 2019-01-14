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

;;;; Global Databases
;;; package: (runtime global-database)

(declare (usual-integrations))

(define-deferred event:after-restore (make-event-distributor))
(define-deferred event:after-restart (make-event-distributor))
(define-deferred event:before-exit (make-event-distributor))
(define-deferred named-structure-descriptions (make-1d-table))

(define (unparser/set-tagged-pair-method! tag method)
  (define-print-method (lambda (object)
			 (and (pair? object)
			      (eq? tag (car object))))
    method))

(define (unparser/set-tagged-vector-method! tag method)
  (define-print-method (lambda (object)
			 (and (vector? object)
			      (fix:> (vector-length object) 0)
			      (eq? tag (vector-ref object 0))))
    method))

(define (named-structure/get-tag-description tag)
  (1d-table/get named-structure-descriptions tag #f))

(define (named-structure/set-tag-description! tag description)
  (1d-table/put! named-structure-descriptions tag description))