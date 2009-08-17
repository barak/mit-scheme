#| -*-Scheme-*-

$Id: 8eb253640703ba6afad261d1d54bf0d939d5dcf9 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Simple Queue Abstraction
;;; package: (runtime simple-queue)

(declare (usual-integrations))

(define-integrable (make-queue)
  (cons '() '()))

(define-integrable (queue-empty? queue)
  (not (pair? (car queue))))

(define-integrable (queued?/unsafe queue item)
  (memq item (car queue)))

(define (enqueue!/unsafe queue object)
  (let ((next (cons object '())))
    (if (pair? (cdr queue))
	(set-cdr! (cdr queue) next)
	(set-car! queue next))
    (set-cdr! queue next)
    unspecific))

(define (dequeue!/unsafe queue)
  (let ((next (car queue)))
    (if (not (pair? next))
	(error "Attempt to dequeue from empty queue"))
    (if (pair? (cdr next))
	(set-car! queue (cdr next))
	(begin
	  (set-car! queue '())
	  (set-cdr! queue '())))
    (car next)))

(define (queue-map!/unsafe queue procedure)
  (let loop ()
    (if (not (queue-empty? queue))
	(begin
	  (procedure (dequeue!/unsafe queue))
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
  (let ((empty (list 'EMPTY)))
    (let loop ()
      (let ((item
	     (without-interrupts
	      (lambda ()
		(if (queue-empty? queue)
		    empty
		    (dequeue!/unsafe queue))))))
	(if (not (eq? item empty))
	    (begin
	      (procedure item)
	      (loop)))))))

(define (queue->list queue)
  (without-interrupts
    (lambda ()
      (list-copy (queue->list/unsafe queue)))))