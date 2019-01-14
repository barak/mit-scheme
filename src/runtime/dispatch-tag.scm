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

;;;; Tags for efficient dispatching

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations))

(define (%make-tag metatag name predicate extra)
  (let ((tag
	 (apply %record
		metatag
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
		(%make-weak-set)
		extra)))
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
(register-predicate! tag-name? 'dispatch-tag-name)

(define (dispatch-tag? object)
  (and (%record? object)
       (dispatch-metatag? (%record-ref object 0))))
(register-predicate! dispatch-tag? 'tag '<= %record?)

(define-integrable (%dispatch-tag-name tag)
  (%record-ref tag 9))

(define-integrable (%dispatch-tag->predicate tag)
  (%record-ref tag 10))

(define-integrable (%tag-supersets tag)
  (%record-ref tag 11))

(define-integrable (%dispatch-tag-extra-length tag)
  (fix:- (%record-length tag) 12))

(define-integrable (%dispatch-tag-extra-ref tag index)
  (%record-ref tag (%dispatch-tag-extra-index index)))

(define-integrable (%dispatch-tag-extra-set! tag index value)
  (%record-set! tag (%dispatch-tag-extra-index index) value))

(define-integrable (%dispatch-tag-extra-index index)
  (fix:+ 12 index))

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

(define (make-dispatch-metatag name)
  (guarantee tag-name? name 'make-dispatch-metatag)
  (letrec*
      ((predicate
	(lambda (object)
	  (and (%record? object)
	       (eq? metatag (%record-ref object 0)))))
       (metatag (%make-tag metatag-tag name predicate '())))
    (set-dispatch-tag<=! metatag metatag-tag)
    metatag))

(define (dispatch-metatag-constructor metatag #!optional caller)
  (guarantee dispatch-metatag? metatag 'dispatch-metatag-constructor)
  (lambda (name predicate . extra)
    (guarantee tag-name? name caller)
    (guarantee unary-procedure? predicate caller)
    (if (predicate? predicate)
	(error "Can't assign multiple tags to the same predicate:" name))
    (%make-tag metatag name predicate extra)))

(define (dispatch-metatag? object)
  (and (%record? object)
       (eq? metatag-tag (%record-ref object 0))))
(set-predicate<=! dispatch-metatag? dispatch-tag?)

(define metatag-tag)
(add-boot-init!
 (lambda ()
   (set! metatag-tag (%make-tag #f 'metatag dispatch-metatag? '()))
   (%record-set! metatag-tag 0 metatag-tag)))

(define (dispatch-tag-metatag tag)
  (guarantee dispatch-tag? tag 'dispatch-tag-metatag)
  (%record-ref tag 0))

(define (dispatch-tag-name tag)
  (guarantee dispatch-tag? tag 'dispatch-tag-name)
  (%dispatch-tag-name tag))

(define (dispatch-tag->predicate tag)
  (guarantee dispatch-tag? tag 'dispatch-tag->predicate)
  (%dispatch-tag->predicate tag))

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
  (%weak-set-any procedure (%tag-supersets tag)))

(define (add-dispatch-tag-superset tag superset)
  (guarantee dispatch-tag? tag 'add-dispatch-tag-superset)
  (guarantee dispatch-tag? superset 'add-dispatch-tag-superset)
  (%add-to-weak-set superset (%tag-supersets tag)))

(define-print-method dispatch-tag?
  (standard-print-method
   (lambda (tag)
     (if (dispatch-metatag? tag) 'dispatch-metatag 'dispatch-tag))
   (lambda (tag)
     (list (let ((name (dispatch-tag-name tag)))
	     (if (symbol? name)
		 (strip-angle-brackets name)
		 name))))))

(define-pp-describer dispatch-tag?
  (lambda (tag)
    (list (list 'metatag (dispatch-tag-metatag tag))
	  (list 'name (dispatch-tag-name tag))
	  (list 'predicate (dispatch-tag->predicate tag))
	  (cons 'extra (dispatch-tag-extra tag)))))