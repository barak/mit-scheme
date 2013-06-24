#| -*-Scheme-*-

$Id: asutl.scm,v 1.8 2008/01/30 20:01:41 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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