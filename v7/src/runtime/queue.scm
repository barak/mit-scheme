#| -*-Scheme-*-

$Id: queue.scm,v 14.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
    (set-cdr! queue next)
    unspecific))

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

(define-integrable (queue->list/unsafe queue)
  (car queue))

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

(define (queue->list queue)
  (without-interrupts
    (lambda ()
      (list-copy (queue->list/unsafe queue)))))