#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/coerce.scm,v 1.1 1987/07/28 18:26:43 jinx Exp $

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

;;;; Vax Specific Coercions

(declare (usual-integrations))

(define coerce-literal
  (standard-coercion
   (lambda (n)
     (if (<= 0 n 63)
	 n
	 (error "Bad short literal" n)))))

(define coerce-short-label
  (standard-coercion
   (lambda (offset)
     (or (if (negative? offset)
	     (and (>= offset -128) (+ offset 256))
	     (and (< offset 128) offset))
	 (error "Short label out of range" offset)))))

;; *** NOTE ***
;; If you add coercions here, remember to also add them to
;; EXPAND-DESCRIPTOR in isnmac.scm .

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer)
     (LITERAL . ,coerce-literal)
     (SHORT-LABEL . ,coerce-short-label))))

(define-coercion 'UNSIGNED 1)
(define-coercion 'UNSIGNED 2)
(define-coercion 'UNSIGNED 3)
(define-coercion 'UNSIGNED 4)
(define-coercion 'UNSIGNED 5)
(define-coercion 'UNSIGNED 6)
(define-coercion 'UNSIGNED 7)
(define-coercion 'UNSIGNED 8)
(define-coercion 'UNSIGNED 9)
(define-coercion 'UNSIGNED 10)
(define-coercion 'UNSIGNED 11)
(define-coercion 'UNSIGNED 12)
(define-coercion 'UNSIGNED 13)
(define-coercion 'UNSIGNED 14)
(define-coercion 'UNSIGNED 15)
(define-coercion 'UNSIGNED 16)
(define-coercion 'UNSIGNED 32)

(define-coercion 'SIGNED 8)
(define-coercion 'SIGNED 16)
(define-coercion 'SIGNED 32)

(define-coercion 'LITERAL 8)
(define-coercion 'SHORT-LABEL 8)