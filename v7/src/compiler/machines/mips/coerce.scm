#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/coerce.scm,v 1.1 1990/05/07 04:10:32 jinx Rel $
$MC68020-Header: coerce.scm,v 1.10 88/08/31 05:56:37 GMT cph Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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

(declare (usual-integrations))

;;;; MIPS coercions

;;; Coercion top level

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define coerce-1-bit-unsigned (make-coercion 'UNSIGNED 1))
(define coerce-4-bit-unsigned (make-coercion 'UNSIGNED 4))
(define coerce-5-bit-unsigned (make-coercion 'UNSIGNED 5))
(define coerce-6-bit-unsigned (make-coercion 'UNSIGNED 6))
(define coerce-10-bit-unsigned (make-coercion 'UNSIGNED 10))
(define coerce-11-bit-unsigned (make-coercion 'UNSIGNED 11))
(define coerce-15-bit-unsigned (make-coercion 'UNSIGNED 15))
(define coerce-16-bit-unsigned (make-coercion 'UNSIGNED 16))
(define coerce-20-bit-unsigned (make-coercion 'UNSIGNED 20))
(define coerce-25-bit-unsigned (make-coercion 'UNSIGNED 25))
(define coerce-26-bit-unsigned (make-coercion 'UNSIGNED 26))
(define coerce-32-bit-unsigned (make-coercion 'UNSIGNED 32))

(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-26-bit-signed (make-coercion 'SIGNED 26))
(define coerce-32-bit-signed (make-coercion 'SIGNED 32))
