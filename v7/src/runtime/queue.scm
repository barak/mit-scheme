#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/queue.scm,v 14.2 1988/06/13 11:50:28 cph Rel $

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

;;;; Simple Queue Abstraction
;;; package: ()

(declare (usual-integrations))

(define-integrable (make-queue)
  (cons '() '()))

(define-integrable (queue-empty? queue)
  (null? (car queue)))

(define-integrable (queued?/unsafe queue item)
  (memq item (car queue)))

(define (enqueue!/unsafe queue object)
  (let ((next (cons object '())))
    (if (null? (cdr queue))
	(set-car! queue next)
	(set-cdr! (cdr queue) next))
    (set-cdr! queue next)))

(define (dequeue!/unsafe queue)
  (let ((next (car queue)))
    (if (null? next)
	(error "Attempt to dequeue from empty queue"))
    (if (null? (cdr next))
	(begin (set-car! queue '())
	       (set-cdr! queue '()))
	(set-car! queue (cdr next)))
    (car next)))

(define (queue-map!/unsafe queue procedure)
  (let loop ()
    (if (not (queue-empty? queue))
	(begin (procedure (dequeue!/unsafe queue))
	       (loop)))))

;;; Safe (interrupt locked) versions of the above operations.

(define-integrable (queued? queue item)
  (without-interrupts (lambda () (queued?/unsafe queue item))))

(define-integrable (enqueue! queue object)
  (without-interrupts (lambda () (enqueue!/unsafe queue object))))

(define-integrable (dequeue! queue)
  (without-interrupts (lambda () (dequeue!/unsafe queue))))

(define (queue-map! queue procedure)
  (let ((empty "empty"))
    (let loop ()
      (let ((item
	     (without-interrupts
	      (lambda ()
		(if (queue-empty? queue)
		    empty
		    (dequeue!/unsafe queue))))))
	(if (not (eq? item empty))
	    (begin (procedure item)
		   (loop)))))))