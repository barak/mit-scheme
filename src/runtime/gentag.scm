#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations))

(define (%make-tag metatag name predicate extra)
  (let ((tag
	 (%record metatag
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  (get-tag-cache-number)
		  name
		  predicate
		  extra
		  (%make-weak-set))))
    (set-predicate-tag! predicate tag)
    tag))

(define (tag-name? object)
  (or (symbol? object)
      (and (pair? object)
	   (symbol? (car object))
	   (list? (cdr object))
	   (every (lambda (elt)
		    (or (object-non-pointer? elt)
			(tag-name? elt)))
		  (cdr object)))))
(register-predicate! tag-name? 'tag-name)

(define (set-predicate-tag! predicate tag)
  (defer-boot-action 'set-predicate-tag!
    (lambda ()
      (set-predicate-tag! predicate tag))))

(define (tag? object)
  (and (%record? object)
       (metatag? (%record-ref object 0))))
(register-predicate! tag? 'tag '<= %record?)

(define-integrable (%tag-name tag)
  (%record-ref tag 9))

(define-integrable (%tag->predicate tag)
  (%record-ref tag 10))

(define-integrable (%tag-extra tag)
  (%record-ref tag 11))

(define-integrable (%tag-supersets tag)
  (%record-ref tag 12))

(define-integrable tag-cache-number-adds-ok
  ;; This constant controls the number of non-zero bits tag cache
  ;; numbers will have.
  ;;
  ;; The value of this constant is the number of tag cache numbers
  ;; that can be added and still be certain the result will be a
  ;; fixnum.  This is implicitly used by all the code that computes
  ;; primary cache locations from multiple tags.
  4)

(define-deferred get-tag-cache-number
  (let ((modulus
	 (int:quotient
	  (let loop ((n 2)) (if (fix:fixnum? n) (loop (int:* n 2)) n))
	  tag-cache-number-adds-ok))
	(state (make-random-state)))
    (lambda ()
      (random modulus state))))

(define (make-metatag name)
  (guarantee tag-name? name 'make-metatag)
  (letrec*
      ((predicate
	(lambda (object)
	  (and (%record? object)
	       (eq? metatag (%record-ref object 0)))))
       (metatag (%make-tag metatag-tag name predicate '#())))
    (set-tag<=! metatag metatag-tag)
    metatag))

(define (metatag-constructor metatag #!optional caller)
  (guarantee metatag? metatag 'metatag-constructor)
  (lambda (name predicate . extra)
    (guarantee tag-name? name caller)
    (guarantee unary-procedure? predicate caller)
    (if (predicate? predicate)
	(error "Can't assign multiple tags to the same predicate:" name))
    (%make-tag metatag name predicate (list->vector extra))))

(define (metatag? object)
  (and (%record? object)
       (eq? metatag-tag (%record-ref object 0))))

(define metatag-tag)
(add-boot-init!
 (lambda ()
   (set! metatag-tag (%make-tag #f 'metatag metatag? '#()))
   (%record-set! metatag-tag 0 metatag-tag)))

(define (set-tag<=! t1 t2)
  (defer-boot-action 'predicate-relations
    (lambda ()
      (set-tag<=! t1 t2))))

(define (tag-metatag tag)
  (guarantee tag? tag 'tag-metatag)
  (%record-ref tag 0))

(define (tag-name tag)
  (guarantee tag? tag 'tag-name)
  (%tag-name tag))

(define (tag->predicate tag)
  (guarantee tag? tag 'tag->predicate)
  (%tag->predicate tag))

(define (tag-extra tag index)
  (guarantee tag? tag 'tag-extra)
  (vector-ref (%tag-extra tag) index))

(define (any-tag-superset procedure tag)
  (guarantee tag? tag 'any-tag-superset)
  (%weak-set-any procedure (%tag-supersets tag)))

(define (add-tag-superset tag superset)
  (guarantee tag? tag 'add-tag-superset)
  (guarantee tag? superset 'add-tag-superset)
  (%add-to-weak-set superset (%tag-supersets tag)))

(defer-boot-action 'predicate-relations
  (lambda ()
    (set-predicate<=! metatag? tag?)))

(define-unparser-method tag?
  (simple-unparser-method
   (lambda (tag)
     (if (metatag? tag) 'metatag 'tag))
   (lambda (tag)
     (list (tag-name tag)))))

(define-pp-describer tag?
  (lambda (tag)
    (list (list 'metatag (tag-metatag tag))
	  (list 'name (tag-name tag))
	  (list 'predicate (tag->predicate tag))
	  (cons 'extra (vector->list (%tag-extra tag))))))