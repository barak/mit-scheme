#| -*-Scheme-*-

$Id: coerce.scm,v 1.1 1992/08/29 13:51:16 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

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

(define coerce-14-bit-signed (make-coercion 'SIGNED 14))
(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-21-bit-signed (make-coercion 'SIGNED 21))
