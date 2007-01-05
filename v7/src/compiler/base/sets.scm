#| -*-Scheme-*-

$Id: sets.scm,v 4.5 2007/01/05 15:33:03 cph Exp $

Copyright (c) 1987, 1999 Massachusetts Institute of Technology

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

;;;; Simple Set Abstraction

(declare (usual-integrations))

(define (eq-set-adjoin element set)
  (if (memq element set)
      set
      (cons element set)))

(define (eqv-set-adjoin element set)
  (if (memv element set)
      set
      (cons element set)))

(define (eq-set-delete set item)
  (define (loop set)
    (cond ((null? set) '())
	  ((eq? (car set) item) (cdr set))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eqv-set-delete set item)
  (define (loop set)
    (cond ((null? set) '())
	  ((eqv? (car set) item) (cdr set))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eq-set-substitute set old new)
  (define (loop set)
    (cond ((null? set) '())
	  ((eq? (car set) old) (cons new (cdr set)))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eqv-set-substitute set old new)
  (define (loop set)
    (cond ((null? set) '())
	  ((eqv? (car set) old) (cons new (cdr set)))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (set-search set procedure)
  (define (loop items)
    (and (not (null? items))
	 (or (procedure (car items))
	     (loop (cdr items)))))
  (loop set))

;;; The dataflow analyzer assumes that
;;; (eq? (list-tail (eq-set-union x y) n) y) for some n.

(define (eq-set-union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (if (memq (car x) y)
		      y
		      (cons (car x) y)))))))

(define (eqv-set-union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (if (memv (car x) y)
		      y
		      (cons (car x) y)))))))

(define (eq-set-difference x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memq (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x))))))
  (loop x))

(define (eqv-set-difference x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memv (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x))))))
  (loop x))

(define (eq-set-intersection x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memq (car x) y) (cons (car x) (loop (cdr x))))
	  (else (loop (cdr x)))))
  (loop x))

(define (eqv-set-intersection x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memv (car x) y) (cons (car x) (loop (cdr x))))
	  (else (loop (cdr x)))))
  (loop x))

(define (eq-set-disjoint? x y)
  (define (loop x)
    (cond ((null? x) true)
	  ((memq (car x) y) false)
	  (else (loop (cdr x)))))
  (loop x))

(define (eqv-set-disjoint? x y)
  (define (loop x)
    (cond ((null? x) true)
	  ((memv (car x) y) false)
	  (else (loop (cdr x)))))
  (loop x))

(define (eq-set-subset? x y)
  (define (loop x)
    (cond ((null? x) true)
	  ((memq (car x) y) (loop (cdr x)))
	  (else false)))
  (loop x))

(define (eqv-set-subset? x y)
  (define (loop x)
    (cond ((null? x) true)
	  ((memv (car x) y) (loop (cdr x)))
	  (else false)))
  (loop x))

(define (eq-set-same-set? x y)
  (and (eq-set-subset? x y)
       (eq-set-subset? y x)))

(define (eqv-set-same-set? x y)
  (and (eqv-set-subset? x y)
       (eqv-set-subset? y x)))

(define (list->eq-set elements)
  (if (null? elements)
      '()
      (eq-set-adjoin (car elements)
		     (list->eq-set (cdr elements)))))

(define (list->eqv-set elements)
  (if (null? elements)
      '()
      (eqv-set-adjoin (car elements)
		      (list->eqv-set (cdr elements)))))

(define (map->eq-set procedure items)
  (let loop ((items items))
    (if (null? items)
	'()
	(eq-set-adjoin (procedure (car items))
		       (loop (cdr items))))))

(define (map->eqv-set procedure items)
  (let loop ((items items))
    (if (null? items)
	'()
	(eqv-set-adjoin (procedure (car items))
			(loop (cdr items))))))