#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcdemn.scm,v 14.2 1988/06/13 11:45:08 cph Rel $

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

;;;; Garbage Collector Daemons
;;; package: (runtime gc-daemons)

(declare (usual-integrations))

(define (initialize-package!)
  (set! gc-daemons '())
  (set! secondary-gc-daemons '())
  (let ((fixed-objects (get-fixed-objects-vector)))
    (vector-set! fixed-objects #x0B trigger-gc-daemons)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define gc-daemons)
(define secondary-gc-daemons)

(define (trigger-gc-daemons)
  (trigger-daemons gc-daemons))

(define (trigger-secondary-gc-daemons!)
  (trigger-daemons secondary-gc-daemons))

(define (trigger-daemons daemons . extra-args)
  (let loop ((daemons daemons))
    (if (not (null? daemons))
	(begin (apply (car daemons) extra-args)
	       (loop (cdr daemons))))))

(define (add-gc-daemon! daemon)
  (set! gc-daemons (cons daemon gc-daemons)))

(define (add-secondary-gc-daemon! daemon)
  (set! secondary-gc-daemons (cons daemon secondary-gc-daemons)))