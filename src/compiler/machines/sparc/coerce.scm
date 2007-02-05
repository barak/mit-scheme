#| -*-Scheme-*-

$Id: coerce.scm,v 1.6 2007/01/05 21:19:21 cph Exp $
$MC68020-Header: coerce.scm,v 1.10 88/08/31 05:56:37 GMT cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; SPARC coercions

;;; Coercion top level

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define coerce-1-bit-unsigned (make-coercion 'UNSIGNED 1))
(define coerce-2-bit-unsigned (make-coercion 'UNSIGNED 2))
(define coerce-3-bit-unsigned (make-coercion 'UNSIGNED 3))
(define coerce-4-bit-unsigned (make-coercion 'UNSIGNED 4))
(define coerce-5-bit-unsigned (make-coercion 'UNSIGNED 5))
(define coerce-6-bit-unsigned (make-coercion 'UNSIGNED 6))
(define coerce-8-bit-unsigned (make-coercion 'UNSIGNED 8))
(define coerce-9-bit-unsigned (make-coercion 'UNSIGNED 9))
(define coerce-10-bit-unsigned (make-coercion 'UNSIGNED 10))
(define coerce-11-bit-unsigned (make-coercion 'UNSIGNED 11))
(define coerce-13-bit-unsigned (make-coercion 'UNSIGNED 13))
(define coerce-15-bit-unsigned (make-coercion 'UNSIGNED 15))
(define coerce-16-bit-unsigned (make-coercion 'UNSIGNED 16))
(define coerce-20-bit-unsigned (make-coercion 'UNSIGNED 20))
(define coerce-22-bit-unsigned (make-coercion 'UNSIGNED 22))
(define coerce-25-bit-unsigned (make-coercion 'UNSIGNED 25))
(define coerce-26-bit-unsigned (make-coercion 'UNSIGNED 26))
(define coerce-30-bit-unsigned (make-coercion 'UNSIGNED 30))
(define coerce-32-bit-unsigned (make-coercion 'UNSIGNED 32))

(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-13-bit-signed (make-coercion 'SIGNED 13))
(define coerce-22-bit-signed (make-coercion 'SIGNED 22))
(define coerce-26-bit-signed (make-coercion 'SIGNED 26))
(define coerce-30-bit-signed (make-coercion 'SIGNED 30))
(define coerce-32-bit-signed (make-coercion 'SIGNED 32))
