#| -*-Scheme-*-

$Id: cutl.scm,v 1.4 2003/02/14 18:28:02 cph Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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