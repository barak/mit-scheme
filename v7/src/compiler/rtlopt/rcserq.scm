#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcserq.scm,v 1.2 1987/04/24 14:15:53 cph Exp $

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

;;;; RTL Common Subexpression Elimination: Register/Quantity Abstractions
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define quantity-tag (make-vector-tag false 'QUANTITY))
(define quantity? (tagged-vector-predicate quantity-tag))
(define-vector-slots quantity 1 number first-register last-register)

(define *next-quantity-number*)

(define (generate-quantity-number)
  (let ((n *next-quantity-number*))
    (set! *next-quantity-number* (1+ *next-quantity-number*))
    n))

(define (make-quantity number first-register last-register)
  (vector quantity-tag number first-register last-register))

(define (new-quantity register)
  (make-quantity (generate-quantity-number) register register))

(define (quantity-copy quantity)
  (make-quantity (quantity-number quantity)
		 (quantity-first-register quantity)
		 (quantity-last-register quantity)))

(define (get-register-quantity register)
  (or (register-quantity register)
      (let ((quantity (new-quantity register)))
	(set-register-quantity! register quantity)
	quantity)))

(define-register-references quantity)
(define-register-references next-equivalent)
(define-register-references previous-equivalent)
(define-register-references expression)
(define-register-references tick)
(define-register-references in-table)