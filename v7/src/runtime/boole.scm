#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/boole.scm,v 14.1 1988/05/20 00:51:46 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Boolean Operations

(declare (usual-integrations))

(define-primitives not (false? not))

(define false #F)
(define true #T)

(define (boolean? object)
  (or (eq? object #F)
      (eq? object #T)))

(define (boolean=? x y)
  (if x y (not y)))

(define (boolean/or . arguments)
  (let loop ((arguments arguments))
    (cond ((null? arguments) false)
	  ((car arguments) true)
	  (else (loop (cdr arguments))))))

(define (boolean/and . arguments)
  (let loop ((arguments arguments))
    (cond ((null? arguments) true)
	  ((car arguments) (loop (cdr arguments)))
	  (else false))))

(define (there-exists? items predicate)
  (let loop ((items items))
    (and (not (null? items))
	 (or (predicate (car items))
	     (loop (cdr items))))))

(define (for-all? items predicate)
  (let loop ((items items))
    (or (null? items)
	(and (predicate (car items))
	     (loop (cdr items))))))