#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/mvalue.scm,v 3.0 1987/03/10 13:25:05 cph Rel $

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

;;;; Multiple Value Support

(declare (usual-integrations))

(define (transmit-values transmitter receiver)
  (transmitter receiver))

(define (multiple-value-list transmitter)
  (transmitter list))

(define (return . values)
  (lambda (receiver)
    (apply receiver values)))

;;; For efficiency:

(define (return-2 v0 v1)
  (lambda (receiver)
    (receiver v0 v1)))

(define (return-3 v0 v1 v2)
  (lambda (receiver)
    (receiver v0 v1 v2)))

(define (return-4 v0 v1 v2 v3)
  (lambda (receiver)
    (receiver v0 v1 v2 v3)))

(define (return-5 v0 v1 v2 v3 v4)
  (lambda (receiver)
    (receiver v0 v1 v2 v3 v4)))

(define (return-6 v0 v1 v2 v3 v4 v5)
  (lambda (receiver)
    (receiver v0 v1 v2 v3 v4 v5)))

(define (list-multiple first . rest)
  (apply call-multiple list first rest))

(define (cons-multiple cars cdrs)
  (call-multiple cons cars cdrs))

(define (call-multiple procedure . transmitters)
  (apply return
	 (apply map
		procedure
		(map multiple-value-list transmitters))))