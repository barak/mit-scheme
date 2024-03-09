#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;;; Tags for efficient dispatching
;;; package: (runtime tagged-dispatch)

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations)
	 (integrate-external "dispatch-low"))

(define (tag-name? object)
  (or (symbol? object)
      (and (pair? object)
	   (symbol? (car object))
	   (list-of-type? (cdr object)
			  (lambda (elt)
			    (or (object-non-pointer? elt)
				(tag-name? elt)))))))
(register-predicate! tag-name? 'dispatch-tag-name)

(define (make-dispatch-metatag name)
  (guarantee tag-name? name 'make-dispatch-metatag)
  (letrec*
      ((predicate
	(lambda (object)
	  (and (%record? object)
	       (eq? metatag (%record-ref object 0)))))
       (metatag (%make-tag metatag-tag name predicate '())))
    (set-predicate-tag! predicate metatag)
    (set-dispatch-tag<=! metatag metatag-tag)
    metatag))

(define (dispatch-metatag-constructor metatag #!optional caller)
  (guarantee dispatch-metatag? metatag 'dispatch-metatag-constructor)
  (lambda (name predicate . extra)
    (guarantee tag-name? name caller)
    (guarantee unary-procedure? predicate caller)
    (if (predicate? predicate)
	(error "Can't assign multiple tags to the same predicate:" name))
    (let ((tag (%make-tag metatag name predicate extra)))
      (set-predicate-tag! predicate tag)
      tag)))

(define (dispatch-tag-metatag tag)
  (guarantee dispatch-tag? tag 'dispatch-tag-metatag)
  (%dispatch-tag-metatag tag))

(define (dispatch-tag-name tag)
  (guarantee dispatch-tag? tag 'dispatch-tag-name)
  (%dispatch-tag-name tag))

(define (dispatch-tag->predicate tag)
  (guarantee dispatch-tag? tag 'dispatch-tag->predicate)
  (%dispatch-tag-predicate tag))

(define (dispatch-tag-extra-ref tag index)
  (guarantee dispatch-tag? tag 'dispatch-tag-extra-ref)
  (%dispatch-tag-extra-ref tag index))

(define (dispatch-tag-extra-length tag)
  (guarantee dispatch-tag? tag 'dispatch-tag-extra-length)
  (%dispatch-tag-extra-length tag))

(define (dispatch-tag-extra tag)
  (do ((i (fix:- (dispatch-tag-extra-length tag) 1) (fix:- i 1))
       (elts '() (cons (%dispatch-tag-extra-ref tag i) elts)))
      ((fix:< i 0) elts)))

(define (any-dispatch-tag-superset procedure tag)
  (guarantee dispatch-tag? tag 'any-dispatch-tag-superset)
  (%any-dispatch-tag-superset procedure tag))

(define (add-dispatch-tag-superset tag superset)
  (guarantee dispatch-tag? tag 'add-dispatch-tag-superset)
  (guarantee dispatch-tag? superset 'add-dispatch-tag-superset)
  (%add-dispatch-tag-superset! tag superset))

(define-print-method dispatch-tag?
  (standard-print-method 'dispatch-tag
    (lambda (tag)
      (list (dispatch-tag-name tag)
	    (dispatch-tag-name (dispatch-tag-metatag tag))))))

(define-print-method dispatch-metatag?
  (standard-print-method 'dispatch-metatag
    (lambda (tag)
      (list (dispatch-tag-name tag)))))

(define-pp-describer dispatch-tag?
  (lambda (tag)
    (list (list 'metatag (dispatch-tag-metatag tag))
	  (list 'name (dispatch-tag-name tag))
	  (list 'predicate (dispatch-tag->predicate tag))
	  (cons 'extra (dispatch-tag-extra tag)))))