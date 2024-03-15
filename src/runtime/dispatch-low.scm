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

;;;; Tags for efficient dispatching -- cold-load support
;;; package: (runtime tagged-dispatch)

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations))

(define-integrable (%%make-tag metatag cache-number name predicate
			       supersets subsets extra)
  (apply %record
	 metatag
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 (cache-number)
	 name
	 predicate
	 supersets
	 subsets
	 extra))

(define (cold-load:%make-tag metatag name predicate . extra)
  (let ((tag (%%make-tag metatag (lambda () #f) name predicate #f #f extra)))
    (set! need-cache-numbers (cons tag need-cache-numbers))
    tag))

(define need-cache-numbers '())

(define %make-tag cold-load:%make-tag)

(define after-cold-load:%make-tag
  (named-lambda (%make-tag metatag name predicate . extra)
    (%%make-tag metatag get-tag-cache-number name predicate
		(weak-list-set eq?)
		(weak-list-set eq?)
		extra)))

(define (%dispatch-tag? object)
  (and (%record? object)
       (%dispatch-metatag? (%record-ref object 0))))

(define-integrable (%dispatch-tag-metatag tag)
  (%record-ref tag 0))

(define-integrable (%dispatch-tag-name tag)
  (%record-ref tag 9))

(define-integrable (%dispatch-tag-predicate tag)
  (%record-ref tag 10))

(define-integrable (%set-dispatch-tag-predicate! tag predicate)
  (%record-set! tag 10 predicate))

(define-integrable (%dispatch-tag-supersets tag)
  (%record-ref tag 11))

(define-integrable (%set-dispatch-tag-supersets! tag supersets)
  (%record-set! tag 11 supersets))

(define-integrable (%dispatch-tag-subsets tag)
  (%record-ref tag 12))

(define-integrable (%set-dispatch-tag-subsets! tag supersets)
  (%record-set! tag 12 supersets))

(define-integrable (%dispatch-tag-extra-length tag)
  (fix:- (%record-length tag) 13))

(define-integrable (%dispatch-tag-extra-ref tag index)
  (%record-ref tag (%dispatch-tag-extra-index index)))

(define-integrable (%dispatch-tag-extra-set! tag index value)
  (%record-set! tag (%dispatch-tag-extra-index index) value))

(define-integrable (%dispatch-tag-extra-index index)
  (fix:+ 13 index))

(define (%any-dispatch-tag-superset procedure tag)
  (weak-list-set-any procedure (%dispatch-tag-supersets tag)))

(define (%add-dispatch-tag-superset! tag superset)
  (weak-list-set-add! superset (%dispatch-tag-supersets tag)))

(define (%dispatch-metatag? object)
  (and (%record? object)
       (eq? metatag-tag (%record-ref object 0))))

(define (%make-dispatch-metatag name)

  (define (test object)
    (and (%record? object)
	 (eq? metatag (%record-ref object 0))))

  (define metatag
    (%make-tag metatag-tag name test))

  metatag)

(define metatag-tag (%make-tag #f 'metatag %dispatch-metatag?))
(%record-set! metatag-tag 0 metatag-tag)

(define (%dispatch-metatag-constructor metatag)
  (lambda (name predicate . extra)
    (apply %make-tag metatag name predicate extra)))

(define-integrable tag-cache-number-adds-ok
  ;; This constant controls the number of non-zero bits tag cache
  ;; numbers will have.
  ;;
  ;; The value of this constant is the number of tag cache numbers
  ;; that can be added and still be certain the result will be a
  ;; fixnum.  This is implicitly used by all the code that computes
  ;; primary cache locations from multiple tags.
  4)

(define get-tag-cache-number)
(define (initialize-cache-numbers!)
  (set! get-tag-cache-number
	(let ((modulus
	       (int:quotient (int:+ fx-greatest 1)
			     tag-cache-number-adds-ok))
	      (state (make-random-state #t)))
	  (lambda ()
	    (random modulus state))))
  (for-each (lambda (tag)
	      (do ((i 1 (fix:+ i 1)))
		  ((not (fix:< i 9)))
		(%record-set! tag i (get-tag-cache-number)))
	      (%set-dispatch-tag-supersets! tag (weak-list-set eq?))
	      (%set-dispatch-tag-subsets! tag (weak-list-set eq?)))
	    need-cache-numbers)
  (set! need-cache-numbers)
  (set! %make-tag after-cold-load:%make-tag)
  (%set-dispatch-tag-predicate! metatag-tag dispatch-metatag?)
  (set-predicate-tag! dispatch-metatag? metatag-tag))