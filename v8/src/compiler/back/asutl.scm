#| -*-Scheme-*-

$Id: 14bfff933aa5e578ff30547674abc369e81217b6 $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Assembler Utilities
;;; package: (compiler)

(declare (usual-integrations))

(define-integrable (back-end:= x y)
  (= x y))

(define-integrable (back-end:+ x y)
  (+ x y))

(define-integrable (back-end:- x y)
  (- x y))

(define-integrable (back-end:* x y)
  (* x y))

(define-integrable (back-end:quotient x y)
  (quotient x y))

(define-integrable (back-end:expt x y)
  (expt x y))

(define-integrable (back-end:< x y)
  (< x y))

(define make-non-pointer-literal
  (let ((type-maximum (expt 2 scheme-type-width))
	(type-scale-factor (expt 2 scheme-datum-width)))
    (lambda (type datum)
      (if (not (and (exact-nonnegative-integer? type)
		    (< type type-maximum)))
	  (error "non-pointer type out of range" type))
      (if (not (and (exact-nonnegative-integer? datum)
		    (< datum type-scale-factor)))
	  (error "non-pointer datum out of range" datum))
      (+ (* type type-scale-factor) datum))))