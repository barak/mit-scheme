#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/sets.scm,v 1.1 1987/03/19 00:44:43 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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