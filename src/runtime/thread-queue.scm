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

;;;; Thread-blocking Queues
;;; package: (runtime thread-queue)

(declare (usual-integrations))

;;; These queues are like the simple queues provided by the (runtime
;;; simple-queue) package, EXCEPT that they will, when empty, block a
;;; thread attempting to dequeue an item.  Also, a maximum size can be
;;; specified so that the queue will, when full, block a thread
;;; attempting to queue an item.  Note that #F should not be used as
;;; an item.  Thread-queue/peek-no-hang returns #F if the queue is
;;; empty.
;;;
;;; If multiple threads block dequeuing, bad mojo is afoot(?).  They
;;; are ALL restarted whenever an item becomes available and,
;;; depending on the thread timer interrupts, ANY ONE of them may be
;;; able to dequeue the item.

(define-structure (thread-queue (constructor %make-thread-queue)
				(conc-name %thread-queue/)
				(print-procedure
				 (bracketed-print-method
				  'thread-queue
				  (lambda (queue port)
				    (print-thread-queue queue port)))))
  first-pair
  last-pair
  element-count
  max-elements
  waiting-queuers
  waiting-dequeuers
  mutex)

(define-guarantee thread-queue "a thread-queue")

(define-syntax %assert
  (syntax-rules ()
    ((_ condition)
     #f)))

#;(define-syntax %assert
  (syntax-rules ()
    ((_ condition)
     (if (not condition)
	 (error "Assertion failed:" 'condition)))))

(define-integrable (%locked? queue)
  (thread-mutex-owner (%thread-queue/mutex queue)))

(define (with-queue-locked queue thunk)
  (with-thread-mutex-lock (%thread-queue/mutex queue)
    (lambda ()
      (with-thread-events-blocked thunk))))

(define (with-queue-unlocked queue thunk)
  (without-thread-mutex-lock (%thread-queue/mutex queue)
   ;; suspend-current-thread will unblock (and re-block) thread-events
   thunk))

(define (print-thread-queue queue port)
  (write-string " elements:" port)
  (write-string (number->string
		 (%thread-queue/element-count queue)) port)
  (let ((max (%thread-queue/max-elements queue)))
    (if max
	(begin
	  (write-string " max:" port)
	  (write-string (number->string max) port)))))

(define (make-thread-queue #!optional max-size)
  (let ((max (cond ((default-object? max-size) #f)
		   ((integer? max-size) max-size)
		   (else (error "Max-size must be an integer:" max-size)))))
    (%make-thread-queue #f #f 0 max '() '() (make-thread-mutex))))

(define (thread-queue/empty? queue)
  (guarantee-thread-queue queue 'thread-queue/empty?)
  (%empty? queue))

(define-integrable (%empty? queue)
  ;;(%assert (%locked? queue))  Seems unnecessary.
  (zero? (%thread-queue/element-count queue)))

(define (thread-queue/empty! queue)
  (guarantee-thread-queue queue 'thread-queue/empty!)
  (with-queue-locked queue
   (lambda ()
     (if (not (%empty? queue))
	 (begin
	   (set-%thread-queue/first-pair! queue #f)
	   (set-%thread-queue/last-pair! queue #f)
	   (set-%thread-queue/element-count! queue 0)
	   (%resume-queuers queue)))))
  unspecific)

(define (thread-queue/queue! queue item)
  (guarantee-thread-queue queue 'thread-queue/queue!)
  (if (not item) (error "Cannot queue #F:" queue))
  (with-queue-locked queue
   (lambda ()
     (do ()
	 ((%queue-no-hang! queue item))
       (set-%thread-queue/waiting-queuers!
	queue (append! (%thread-queue/waiting-queuers queue)
		       (list (current-thread))))
       (with-queue-unlocked queue
	suspend-current-thread)))))

(define (thread-queue/queue-no-hang! queue item)
  ;; Returns #F when QUEUE is maxed out.
  (guarantee-thread-queue queue 'thread-queue/queue-no-hang!)
  (if (not item) (error "Cannot queue #F:" queue))
  (with-queue-locked queue
   (lambda ()
     (%queue-no-hang! queue item))))

(define (%queue-no-hang! queue item)
  (%assert (%locked? queue))
  (let ((max (%thread-queue/max-elements queue)))
    (if max
	(if (< (%thread-queue/element-count queue) max)
	    (%queue! queue item)
	    #f)
	(%queue! queue item))))

(define (thread-queue/dequeue-no-hang! queue msec)
  (guarantee-thread-queue queue 'thread-queue/dequeue-no-hang!)
  (guarantee non-negative-fixnum? msec 'thread-queue/dequeue-no-hang!)
  (thread-queue/dequeue-until!
   queue (+ (real-time-clock) (internal-time/seconds->ticks (/ msec 1000)))))

(define (thread-queue/dequeue-until! queue time)
  (when-non-empty-before time queue %dequeue!))

(declare (integrate-operator when-non-empty-before))
(define (when-non-empty-before time queue operation)
  (with-queue-locked queue
   (lambda ()
     (let loop ()
       (if (not (%empty? queue))
	   (operation queue)
	   (let ((now (real-time-clock)))
	     (if (<= time now)
		 #f
		 (begin
		   (set-%thread-queue/waiting-dequeuers!
		    queue (append! (%thread-queue/waiting-dequeuers queue)
				   (list (current-thread))))
		   (register-timer-event (- time now) #f)
		   (with-queue-unlocked queue
		    suspend-current-thread)
		   (loop)))))))))

(define (thread-queue/dequeue! queue)
  (guarantee-thread-queue queue 'thread-queue/dequeue!)
  (with-queue-locked queue
   (lambda ()
     (do ()
	 ((and (not (%empty? queue))
	       (%dequeue! queue)))
       (set-%thread-queue/waiting-dequeuers!
	queue (append! (%thread-queue/waiting-dequeuers queue)
		       (list (current-thread))))
       (with-queue-unlocked queue
	suspend-current-thread)))))

(define (thread-queue/peek-no-hang queue msec)
  (guarantee-thread-queue queue 'thread-queue/peek-no-hang)
  (guarantee non-negative-fixnum? msec 'thread-queue/peek-no-hang)
  (thread-queue/peek-until
   queue (+ (real-time-clock) (internal-time/seconds->ticks (/ msec 1000)))))

(define (thread-queue/peek-until queue time)
  (when-non-empty-before time queue %peek))

(define (thread-queue/peek queue)
  (guarantee-thread-queue queue 'thread-queue/peek)
  (with-queue-locked queue
   (lambda ()
     (do ()
	 ((and (not (%empty? queue))
	       (%peek queue)))
       (set-%thread-queue/waiting-dequeuers!
	queue (append! (%thread-queue/waiting-dequeuers queue)
		       (list (current-thread))))
       (with-queue-unlocked queue
	suspend-current-thread)))))

(define-integrable (%peek queue)
  (%assert (%locked? queue))
  (car (%thread-queue/first-pair queue)))

(define (%queue! queue item)
  (%assert (%locked? queue))
  (let ((last (%thread-queue/last-pair queue))
	(new (cons item '())))
    (if last (set-cdr! last new))
    (set-%thread-queue/last-pair! queue new)
    (if (not (%thread-queue/first-pair queue))
	(set-%thread-queue/first-pair! queue new)))
  (set-%thread-queue/element-count!
   queue (1+ (%thread-queue/element-count queue)))
  (%resume-dequeuers queue)
  item)

(define (%dequeue! queue)
  (%assert (%locked? queue))
  (let* ((first (%thread-queue/first-pair queue))
	 (item (car first)))
    (if (eq? first (%thread-queue/last-pair queue))
	(begin
	  (set-%thread-queue/first-pair! queue #f)
	  (set-%thread-queue/last-pair! queue #f))
        (set-%thread-queue/first-pair! queue (cdr first)))
    (set-%thread-queue/element-count!
     queue (-1+ (%thread-queue/element-count queue)))
    (%resume-queuers queue)
    item))

(define (%resume-queuers queue)
  (%assert (%locked? queue))
  (do ((queuers (%thread-queue/waiting-queuers queue)
		(cdr queuers)))
      ((null? queuers)
       unspecific)
    (signal-thread-event (car queuers) #f))
  (set-%thread-queue/waiting-queuers! queue '()))

(define (%resume-dequeuers queue)
  (%assert (%locked? queue))
  (do ((dequeuers (%thread-queue/waiting-dequeuers queue)
		  (cdr dequeuers)))
      ((null? dequeuers)
       unspecific)
    (signal-thread-event (car dequeuers) #f))
  (set-%thread-queue/waiting-dequeuers! queue '()))

(define (thread-queue/push! queue item)
  ;; Place ITEM at the head of the queue, instead of the end.
  (guarantee-thread-queue queue 'thread-queue/push!)
  (if (not item) (error "Cannot queue #F:" queue))
  (with-queue-locked queue
   (lambda ()
     (let ((max (%thread-queue/max-elements queue)))
       (if max
	   (if (< (%thread-queue/element-count queue) max)
	       (%push! queue item)
	       (let ((last (%thread-queue/last-pair queue))
		     (first (%thread-queue/first-pair queue)))
		 (let ((new-last
			(let before-last ((list first))
			  ;; Assume LIST is always a pair, thus that
			  ;; max > 0, and LAST is in FIRST.
			  (if (eq? (cdr list) last)
			      list
			      (before-last (cdr list))))))
		   (set-cdr! new-last '())
		   (set-%thread-queue/last-pair! queue new-last)
		   (set-car! last item)	;Clobber most recently queued item!
		   (set-cdr! last first)
		   (set-%thread-queue/first-pair! queue last))
		 item))
	   (%push! queue item))))))

(define (%push! queue item)
  (%assert (%locked? queue))
  (let* ((first (%thread-queue/first-pair queue))
	 (new (cons item first)))
    (set-%thread-queue/first-pair! queue new)
    (if (not (%thread-queue/last-pair queue))
	(set-%thread-queue/last-pair! queue new))
    (set-%thread-queue/element-count! queue
				      (1+ (%thread-queue/element-count queue)))
    (%resume-dequeuers queue)
    item))