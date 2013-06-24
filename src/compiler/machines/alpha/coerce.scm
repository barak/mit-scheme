#| -*-Scheme-*-

$Id: coerce.scm,v 1.3 1999/01/02 06:48:57 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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
