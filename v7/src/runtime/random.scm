#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/random.scm,v 14.1 1988/05/20 01:01:13 cph Exp $

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

;;;; Random Number Generator
;;; package: random-number-package

(declare (usual-integrations))

(define seed)
(define a)
(define m)
(define c)

(define (initialize-package!)
  (set! seed 1)
  (set! a (+ (* 3141 1000 1000) (* 592 1000) 621))
  (set! m (integer-expt 2 63))
  (set! c 1))

(define (random k)
  (if (not (integer? k))
      (error "RANDOM is valid only for integers" k))
  (if (not (and (positive? k) (<= k m)))
      (error "RANDOM is valid only for integers from 1 to" m))
  (set! seed (remainder (+ (* a seed) c) m))
  (quotient (* seed k) m))

(define (randomize k)
  (if (not (integer? k))
      (error "RANDOMIZE is valid only for integers" k))
  (if (not (and (positive? k) (<= k m)))
      (error "RANDOMIZE is valid only for integers from 1 to" m))
  (set! seed k))