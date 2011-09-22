#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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
;;; If multiple threads block on a thread-queue, bad mojo is afoot.
;;; They are ALL restarted whenever an item becomes available and,
;;; depending on the thread timer interrupts, ANY ONE of them may be
;;; able to dequeue the item.

(define-structure (thread-queue (constructor %make-thread-queue)
				(conc-name %thread-queue/)
				(print-procedure
				 (standard-unparser-method
				  'thread-queue
				  (lambda (queue port)
				    (print-thread-queue queue port)))))
  first-pair
  last-pair
  element-count
  max-elements
  waiting-queuers
  waiting-dequeuers)

(define-guarantee thread-queue "a thread-queue")

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
    (%make-thread-queue #f #f 0 max '() '())))

(define (thread-queue/empty? queue)
  (%empty? queue))

(define-integrable (%empty? queue)
  (zero? (%thread-queue/element-count queue)))

(define (thread-queue/empty! queue)
  (without-interrupts
   (lambda ()
     (if (not (%empty? queue))
	 (begin
	   (set-%thread-queue/first-pair! queue #f)
	   (set-%thread-queue/last-pair! queue #f)
	   (set-%thread-queue/element-count! queue 0)
	   (%resume-queuers queue)))))
  unspecific)

(define (thread-queue/queue! queue item)
  (if (not item) (error "Cannot queue #F:" queue))
  (without-interrupts
   (lambda ()
     (do ()
	 ((%queue-no-hang! queue item))
       (set-%thread-queue/waiting-queuers!
	queue (append! (%thread-queue/waiting-queuers queue)
		       (list (current-thread))))
       (suspend-current-thread)))))

(define (thread-queue/queue-no-hang! queue item)
  ;; Returns #F when QUEUE is maxed out.
  (if (not item) (error "Cannot queue #F:" queue))
  (without-interrupts
   (lambda ()
     (%queue-no-hang! queue item))))

(define (%queue-no-hang! queue item)
  (let ((max (%thread-queue/max-elements queue)))
    (if max
	(if (< (%thread-queue/element-count queue) max)
	    (%queue! queue item)
	    #f)
	(%queue! queue item))))

(define (thread-queue/dequeue-no-hang queue timeout)
  (guarantee-thread-queue queue 'thread-queue/dequeue-no-hang)
  (guarantee-non-negative-fixnum timeout 'thread-queue/dequeue-no-hang)
  (thread-queue/dequeue-until queue (+ (real-time-clock) timeout)))

(define (thread-queue/dequeue-until queue time)
  (guarantee-thread-queue queue 'thread-queue/dequeue-until)
  (guarantee-integer time 'thread-queue/dequeue-until)
  (when-non-empty-before time queue %dequeue!))

(declare (integrate-operator when-non-empty-before))
(define (when-non-empty-before time queue operation)
  (without-interrupts
   (lambda ()
     (let loop ()
       (if (not (%empty? queue))
	   (operation queue)
	   (let ((now (real-time-clock)))
	     (if (<= time now)
		 #f
		 (begin
		   (register-timer-event (- time now) (lambda () unspecific))
		   (suspend-current-thread)
		   (loop)))))))))

(define (thread-queue/dequeue! queue)
  (without-interrupts
   (lambda ()
     (do ()
	 ((and (not (%empty? queue))
	       (%dequeue! queue)))
       (set-%thread-queue/waiting-dequeuers!
	queue (append! (%thread-queue/waiting-dequeuers queue)
		       (list (current-thread))))
       (suspend-current-thread)))))

(define (thread-queue/peek-no-hang queue timeout)
  (guarantee-thread-queue queue 'thread-queue/peek-no-hang)
  (guarantee-non-negative-fixnum timeout 'thread-queue/peek-no-hang)
  (thread-queue/peek-until queue (+ (real-time-clock) timeout)))

(define (thread-queue/peek-until queue time)
  (guarantee-thread-queue queue 'thread-queue/peek-until)
  (guarantee-integer time 'thread-queue/peek-until)
  (when-non-empty-before time queue %peek))

(define (thread-queue/peek queue)
  (without-interrupts
   (lambda ()
     (do ()
	 ((and (not (%empty? queue))
	       (%peek queue)))
       (set-%thread-queue/waiting-dequeuers!
	queue (append! (%thread-queue/waiting-dequeuers queue)
		       (list (current-thread))))
       (suspend-current-thread)))))

(define-integrable (%peek queue)
  (car (%thread-queue/first-pair queue)))

(define (%queue! queue item)
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
  (do ((queuers (%thread-queue/waiting-queuers queue)
		(cdr queuers)))
      ((null? queuers)
       unspecific)
    (signal-thread-event (car queuers) (lambda () unspecific)))
  (set-%thread-queue/waiting-queuers! queue '()))

(define (%resume-dequeuers queue)
  (do ((dequeuers (%thread-queue/waiting-dequeuers queue)
		  (cdr dequeuers)))
      ((null? dequeuers)
       unspecific)
    (signal-thread-event (car dequeuers) (lambda () unspecific)))
  (set-%thread-queue/waiting-dequeuers! queue '()))

(define (thread-queue/push! queue item)
  ;; Place ITEM at the head of the queue, instead of the end.
  (if (not item) (error "Cannot queue #F:" queue))
  (without-interrupts
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
  (let* ((first (%thread-queue/first-pair queue))
	 (new (cons item first)))
    (set-%thread-queue/first-pair! queue new)
    (if (not (%thread-queue/last-pair queue))
	(set-%thread-queue/last-pair! queue new))
    (set-%thread-queue/element-count! queue
				      (1+ (%thread-queue/element-count queue)))
    (%resume-dequeuers queue)
    item))

(define (test)
  ;; Sets up a "producer" thread that puts the letters of the alphabet
  ;; into a thread-queue, one each 2-3 seconds.  A "consumer" thread
  ;; waits on the queue, printing what it reads.
  (outf-error ";Thread Queue Test\n")
  (let ((queue (make-thread-queue)))
    (create-thread
     #f
     (lambda ()
       (outf-error ";    Consumer: "(current-thread)"\n")
       (let loop ()
	 (outf-error ";    Consumer reads.\n")
	 (let ((item (thread-queue/dequeue! queue)))
	   (outf-error ";    Consumer read "item"\n")
	   (loop)))))
    (create-thread
     #f
     (lambda ()
       (outf-error ";    Producer: "(current-thread)"\n")
       (for-each (lambda (item)
		   (outf-error ";    Producer: sleeping...\n")
		   (sleep-current-thread 2000)
		   (outf-error ";    Producer: queuing "item"...\n")
		   (thread-queue/queue! queue item)
		   (outf-error ";    Producer: queued "item"\n"))
		 '(#\a #\b #\c #\d #\e))
       (outf-error ";    Producer done.\n")))))