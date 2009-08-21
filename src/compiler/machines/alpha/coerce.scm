#| -*-Scheme-*-

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

(declare (usual-integrations))

;;;; Alpha coercions
;;; Package: (compiler lap-syntaxer)

;;; Coercion top level

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define coerce-1-bit-unsigned (make-coercion 'UNSIGNED 1))
(define coerce-2-bit-unsigned (make-coercion 'UNSIGNED 2))
(define coerce-3-bit-unsigned (make-coercion 'UNSIGNED 3))
(define coerce-5-bit-unsigned (make-coercion 'UNSIGNED 5))
(define coerce-6-bit-unsigned (make-coercion 'UNSIGNED 6))
(define coerce-7-bit-unsigned (make-coercion 'UNSIGNED 7))
(define coerce-8-bit-unsigned (make-coercion 'UNSIGNED 8))
(define coerce-11-bit-unsigned (make-coercion 'UNSIGNED 11))
(define coerce-16-bit-unsigned (make-coercion 'UNSIGNED 16))
(define coerce-26-bit-unsigned (make-coercion 'UNSIGNED 26))
(define coerce-32-bit-unsigned (make-coercion 'UNSIGNED 32))

(define coerce-14-bit-signed (make-coercion 'SIGNED 14))
(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-21-bit-signed (make-coercion 'SIGNED 21))
