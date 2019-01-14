#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
  (cons* #f '() '()))

(define-integrable (make-serial-queue)
  (cons* (make-thread-mutex) '() '()))

(define-integrable (queue-empty? queue)
  (not (pair? (cadr queue))))

(define-integrable (queued?/unsafe queue item)
  (memq item (cadr queue)))

(define (enqueue!/unsafe queue object)
  (let ((next (cons object '())))
    (if (pair? (cddr queue))
	(set-cdr! (cddr queue) next)
	(set-car! (cdr queue) next))
    (set-cdr! (cdr queue) next)
    unspecific))

(define (dequeue!/unsafe queue)
  (let ((next (cadr queue)))
    (if (not (pair? next))
	(error "Attempt to dequeue from empty queue"))
    (if (pair? (cdr next))
	(set-car! (cdr queue) (cdr next))
	(begin
	  (set-car! (cdr queue) '())
	  (set-cdr! (cdr queue) '())))
    (car next)))

(define (queue-map!/unsafe queue procedure)
  (let loop ()
    (if (not (queue-empty? queue))
	(begin
	  (procedure (dequeue!/unsafe queue))
	  (loop)))))

(define-integrable (queue->list/unsafe queue)
  (cadr queue))

;;; Safe versions of the above operations (when used on a serializing
;;; queue).

(define-integrable (with-queue-lock queue thunk)
  (let ((mutex (car queue)))
    (if mutex
	(with-thread-mutex-lock mutex
	  (lambda ()
	    (without-interruption thunk)))
	(without-interruption thunk))))

(define-integrable (queued? queue item)
  (with-queue-lock queue (lambda () (queued?/unsafe queue item))))

(define-integrable (enqueue! queue object)
  (with-queue-lock queue (lambda () (enqueue!/unsafe queue object))))

(define-integrable (dequeue! queue)
  (with-queue-lock queue (lambda () (dequeue!/unsafe queue))))

(define (queue-map! queue procedure)
  (let ((empty (list 'empty)))
    (let loop ()
      (let ((item
	     (with-queue-lock queue
	      (lambda ()
		(if (queue-empty? queue)
		    empty
		    (dequeue!/unsafe queue))))))
	(if (not (eq? item empty))
	    (begin
	      (procedure item)
	      (loop)))))))

(define (queue->list queue)
  (with-queue-lock queue
    (lambda ()
      (list-copy (queue->list/unsafe queue)))))