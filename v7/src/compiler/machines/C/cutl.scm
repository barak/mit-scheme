#| -*-Scheme-*-

$Id: cutl.scm,v 1.1 1993/06/08 06:13:32 gjr Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; C back-end utilities
;;; package: (compiler)

(declare (usual-integrations))

(define (->back-end-number x)
  (if (number? x)
      (number->string x)
      x))

(define (back-end:= x y)
  (cond ((and (number? x) (number? y))
	 (= x y))
	(else
	 (equal? x y))))

(define (back-end:+ x y)
  (cond ((and (number? x) (number? y))
	 (+ x y))
	((and (number? y) (= y 0))
	 x)
	((and (number? x) (= x 0))
	 y)
	(else
	 (string-append "("
			(->back-end-number x)
			" + "
			(->back-end-number y)
			")"))))

(define (back-end:- x y)
  (cond ((and (number? x) (number? y))
	 (- x y))
	((and (number? y) (= y 0))
	 x)
	((equal? x y)
	 0)
	(else
	 (string-append "("
			(->back-end-number x)
			" - "
			(->back-end-number y)
			")"))))

(define (back-end:* x y)
  (cond ((and (number? x) (number? y))
	 (* x y))
	((and (number? y) (= y 1))
	 x)
	((and (number? y) (= y 0))
	 0)
	((and (number? x) (= x 1))
	 y)
	((and (number? x) (= x 0))
	 0)
	(else
	 (string-append "("
			(->back-end-number x)
			" * "
			(->back-end-number y)
			")"))))

(define (back-end:quotient x y)
  (cond ((and (number? x) (number? y))
	 (quotient x y))
	((and (number? y) (= y 1))
	 x)
	((and (number? x) (= x 0))
	 0)
	((equal? x y)
	 1)
	(else
	 (string-append "("
			(->back-end-number x)
			" / "
			(->back-end-number y)
			")"))))

(define (back-end:expt x y)
  (cond ((and (number? x) (number? y))
	 (expt x y))
	((and (number? x)
	      (or (= x 0) (= x 1)))
	 x)
	((and (number? y) (= y 0))
	 1)
	((and (number? y) (= y 1))
	 x)
	((and (number? x) (= x 2))
	 (string-append "(1 << "
			(->back-end-number y)
			")"))
	(else
	 (error "back-end:expt: Cannot exponentiate"
		x y))))

;; This is a lie, but it is used only in places where false is the
;; correct default.

(define (back-end:< x y)
  (and (number? x)
       (number? y)
       (< x y)))