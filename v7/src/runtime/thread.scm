#| -*-Scheme-*-

$Id: thread.scm,v 1.34 2001/04/03 03:44:02 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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

;;;; Multiple Threads of Control
;;; package: (runtime thread)

(declare (usual-integrations))

(define-structure (thread
		   (constructor %make-thread ())
		   (conc-name thread/))
  (execution-state 'RUNNING)
  ;; One of:
  ;; RUNNING
  ;; RUNNING-WITHOUT-PREEMPTION
  ;; WAITING
  ;; STOPPED
  ;; DEAD

  (next #f)
  ;; Pointer to next thread in run queue, or #F if none.

  (continuation #f)
  ;; #F if current thread or exited, else continuation for thread.

  (block-events? #f)
  ;; If true, events may not be delivered to this thread.  Instead,
  ;; they are queued.

  (pending-events (make-ring) read-only #t)
  ;; Doubly-linked circular list of events waiting to be delivered.

  (joined-threads '())
  ;; List of threads that have successfully called JOIN-THREAD on this
  ;; thread.

  (joined-to '())
  ;; List of threads to which this thread has joined.

  (exit-value no-exit-value-marker)
  ;; If the thread exits, the exit value is stored here so that
  ;; joined threads can get it.  If the thread has been detached,
  ;; this field holds a condition of type THREAD-DETACHED.

  (root-state-point #f)
  ;; Root state-point of the local state space of the thread.  Used to
  ;; unwind the thread's state space when it is exited.

  (mutexes '())
  ;; List of mutexes that this thread owns or is waiting to own.  Used
  ;; to disassociate the thread from those mutexes when it is exited.

  (properties (make-1d-table) read-only #t))

(define-integrable (guarantee-thread thread procedure)
  (if (not (thread? thread))
      (error:wrong-type-argument thread "thread" procedure)))

(define no-exit-value-marker
  (list 'NO-EXIT-VALUE-MARKER))

(define-integrable (thread-dead? thread)
  (eq? 'DEAD (thread/execution-state thread)))

(define thread-population)
(define first-running-thread)
(define last-running-thread)
(define thread-timer-running?)
(define root-continuation-default)

(define (initialize-package!)
  (initialize-error-conditions!)
  (set! thread-population (make-population))
  (set! first-running-thread #f)
  (set! last-running-thread #f)
  (set! thread-timer-running? #f)
  (set! timer-records #f)
  (set! timer-interval 100)
  (initialize-input-blocking)
  (add-event-receiver! event:after-restore initialize-input-blocking)
  (detach-thread (make-thread #f))
  (add-event-receiver! event:before-exit stop-thread-timer))

(define (make-thread continuation)
  (let ((thread (%make-thread)))
    (set-thread/continuation! thread continuation)
    (set-thread/root-state-point! thread
				  (current-state-point state-space:local))
    (add-to-population! thread-population thread)
    (thread-running thread)
    thread))

(define-integrable (without-interrupts thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((value (thunk)))
      (set-interrupt-enables! interrupt-mask)
      value)))

(define (threads-list)
  (map-over-population thread-population (lambda (thread) thread)))

(define (thread-execution-state thread)
  (guarantee-thread thread thread-execution-state)
  (thread/execution-state thread))

(define (create-thread root-continuation thunk)
  (if (not (or (not root-continuation) (continuation? root-continuation)))
      (error:wrong-type-argument root-continuation
				 "continuation or #f"
				 create-thread))
  (call-with-current-continuation
   (lambda (return)
     (%within-continuation (or root-continuation root-continuation-default)
			   #t
       (lambda ()
	 (fluid-let ((state-space:local (make-state-space)))
	   (call-with-current-continuation
	    (lambda (continuation)
	      (let ((thread (make-thread continuation)))
		(%within-continuation (let ((k return)) (set! return #f) k)
				      #t
				      (lambda () thread)))))
	   (set-interrupt-enables! interrupt-mask/all)
	   (exit-current-thread (thunk))))))))

(define (create-thread-continuation)
  root-continuation-default)

(define (with-create-thread-continuation continuation thunk)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation
				 "continuation"
				 with-create-thread-continuation))
  (fluid-let ((root-continuation-default continuation))
    (thunk)))

(define (current-thread)
  (or first-running-thread
      (let ((thread (console-thread)))
	(if thread
	    (call-with-current-continuation
	     (lambda (continuation)
	       (let ((condition
		      (make-condition condition-type:no-current-thread
				      continuation
				      'BOUND-RESTARTS
				      '())))
		 (signal-thread-event thread
		   (lambda ()
		     (error condition)))))))
	(run-first-thread))))

(define (call-with-current-thread return? procedure)
  (let ((thread first-running-thread))
    (cond (thread (procedure thread))
	  ((not return?) (run-first-thread)))))

(define (console-thread)
  (thread-mutex-owner (port/thread-mutex console-i/o-port)))

(define (other-running-threads?)
  (thread/next (current-thread)))

(define (thread-continuation thread)
  (guarantee-thread thread thread-continuation)
  (without-interrupts
   (lambda ()
     (and (eq? 'WAITING (thread/execution-state thread))
	  (thread/continuation thread)))))

(define (thread-running thread)
  (%thread-running thread)
  (%maybe-toggle-thread-timer))

(define (%thread-running thread)
  (set-thread/execution-state! thread 'RUNNING)
  (let ((prev last-running-thread))
    (if prev
	(set-thread/next! prev thread)
	(set! first-running-thread thread)))
  (set! last-running-thread thread)
  unspecific)

(define (thread-not-running thread state)
  (set-thread/execution-state! thread state)
  (let ((thread* (thread/next thread)))
    (set-thread/next! thread #f)
    (set! first-running-thread thread*))
  (run-first-thread))

(define (run-first-thread)
  (if first-running-thread
      (run-thread first-running-thread)
      (begin
	(set! last-running-thread #f)
	(%maybe-toggle-thread-timer)
	(wait-for-input))))

(define (run-thread thread)
  (let ((continuation (thread/continuation thread)))
    (set-thread/continuation! thread #f)
    (%within-continuation continuation #t
      (lambda ()
	(%resume-current-thread thread)))))

(define (%resume-current-thread thread)
  (if (not (thread/block-events? thread))
      (begin
	(handle-thread-events thread)
	(set-thread/block-events?! thread #f)))
  (%maybe-toggle-thread-timer))

(define (suspend-current-thread)
  (without-interrupts %suspend-current-thread))

(define (%suspend-current-thread)
  (call-with-current-thread #f
    (lambda (thread)
      (let ((block-events? (thread/block-events? thread)))
	(set-thread/block-events?! thread #f)
	(maybe-signal-input-thread-events)
	(let ((any-events? (handle-thread-events thread)))
	  (set-thread/block-events?! thread block-events?)
	  (if (not any-events?)
	      (call-with-current-continuation
	       (lambda (continuation)
		 (set-thread/continuation! thread continuation)
		 (set-thread/block-events?! thread #f)
		 (thread-not-running thread 'WAITING)))))))))

(define (stop-current-thread)
  (without-interrupts
   (lambda ()
     (call-with-current-thread #f
       (lambda (thread)
	 (call-with-current-continuation
	  (lambda (continuation)
	    (set-thread/continuation! thread continuation)
	    (thread-not-running thread 'STOPPED))))))))

(define (restart-thread thread discard-events? event)
  (guarantee-thread thread restart-thread)
  (let ((discard-events?
	 (if (eq? discard-events? 'ASK)
	     (prompt-for-confirmation
	      "Restarting other thread; discard events in its queue")
	     discard-events?)))
    (without-interrupts
     (lambda ()
       (if (not (eq? 'STOPPED (thread/execution-state thread)))
	   (error:bad-range-argument thread restart-thread))
       (if discard-events? (ring/discard-all (thread/pending-events thread)))
       (if event (%signal-thread-event thread event))
       (thread-running thread)))))

(define (disallow-preempt-current-thread)
  (set-thread/execution-state! (current-thread) 'RUNNING-WITHOUT-PREEMPTION))

(define (allow-preempt-current-thread)
  (set-thread/execution-state! (current-thread) 'RUNNING))

(define (thread-timer-interrupt-handler)
  (set-interrupt-enables! interrupt-mask/gc-ok)
  (deliver-timer-events)
  (maybe-signal-input-thread-events)
  (let ((thread first-running-thread))
    (cond ((not thread)
	   (%maybe-toggle-thread-timer))
	  ((thread/continuation thread)
	   (run-thread thread))
	  ((not (eq? 'RUNNING-WITHOUT-PREEMPTION
		     (thread/execution-state thread)))
	   (yield-thread thread))
	  (else
	   (%resume-current-thread thread)))))

(define (yield-current-thread)
  (without-interrupts
   (lambda ()
     (call-with-current-thread #t
       (lambda (thread)
	 ;; Allow preemption now, since the current thread has
	 ;; volunteered to yield control.
	 (set-thread/execution-state! thread 'RUNNING)
	 (yield-thread thread))))))

(define (yield-thread thread)
  (let ((next (thread/next thread)))
    (if (not next)
	(%resume-current-thread thread)
	(call-with-current-continuation
	 (lambda (continuation)
	   (set-thread/continuation! thread continuation)
	   (set-thread/next! thread #f)
	   (set-thread/next! last-running-thread thread)
	   (set! last-running-thread thread)
	   (set! first-running-thread next)
	   (run-thread next))))))

(define (exit-current-thread value)
  (let ((thread (current-thread)))
    (set-interrupt-enables! interrupt-mask/gc-ok)
    (set-thread/block-events?! thread #t)
    (ring/discard-all (thread/pending-events thread))
    (translate-to-state-point (thread/root-state-point thread))
    (%deregister-input-thread-events thread #t)
    (%discard-thread-timer-records thread)
    (%disassociate-joined-threads thread)
    (%disassociate-thread-mutexes thread)
    (if (eq? no-exit-value-marker (thread/exit-value thread))
	(release-joined-threads thread value))
    (thread-not-running thread 'DEAD)))

(define (join-thread thread event-constructor)
  (guarantee-thread thread join-thread)
  (let ((self (current-thread)))
    (if (eq? thread self)
	(signal-thread-deadlock self "join thread" join-thread thread)
	(without-interrupts
	 (lambda ()
	   (let ((value (thread/exit-value thread)))
	     (cond ((eq? value no-exit-value-marker)
		    (set-thread/joined-threads!
		     thread
		     (cons (cons self event-constructor)
			   (thread/joined-threads thread)))
		    (set-thread/joined-to!
		     self
		     (cons thread (thread/joined-to self))))
		   ((eq? value detached-thread-marker)
		    (signal-thread-detached thread))
		   (else
		    (signal-thread-event
		     self
		     (event-constructor thread value))))))))))

(define (detach-thread thread)
  (guarantee-thread thread detach-thread)
  (without-interrupts
   (lambda ()
     (if (eq? (thread/exit-value thread) detached-thread-marker)
	 (signal-thread-detached thread))
     (release-joined-threads thread detached-thread-marker))))

(define detached-thread-marker
  (list 'DETACHED-THREAD-MARKER))

(define (release-joined-threads thread value)
  (set-thread/exit-value! thread value)
  (do ((joined (thread/joined-threads thread) (cdr joined)))
      ((null? joined))
    (let ((joined (caar joined))
	  (event ((cdar joined) thread value)))
      (set-thread/joined-to! joined (delq! thread (thread/joined-to joined)))
      (%signal-thread-event joined event)))
  (%maybe-toggle-thread-timer))

(define (%disassociate-joined-threads thread)
  (do ((threads (thread/joined-to thread) (cdr threads)))
      ((null? threads))
    (set-thread/joined-threads!
     (car threads)
     (del-assq! thread (thread/joined-threads (car threads)))))
  (set-thread/joined-to! thread '()))

;;;; Input Thread Events

(define input-registry)
(define input-registrations)

(define-structure (dentry (conc-name dentry/))
  (descriptor #f read-only #t)
  first-tentry
  last-tentry
  prev
  next)

(define-structure (tentry (conc-name tentry/)
			  (constructor make-tentry (thread event permanent?)))
  dentry
  thread
  event
  (permanent? #f read-only #t)
  prev
  next)

(define (initialize-input-blocking)
  (set! input-registry (and have-select? (make-select-registry)))
  (set! input-registrations #f)
  unspecific)

(define-integrable (maybe-signal-input-thread-events)
  (if input-registrations
      (signal-select-result (select-registry-test input-registry #f))))

(define (wait-for-input)
  (let ((catch-errors
	 (lambda (thunk)
	   (let ((thread (console-thread)))
	     (if thread
		 (bind-condition-handler '()
		     (lambda (condition)
		       (error:derived-thread thread condition))
		   thunk)
		 (call-with-current-continuation
		  (lambda (k)
		    (bind-condition-handler '()
			(lambda (condition)
			  condition
			  (within-continuation k thunk))
		      thunk))))))))
    (if (not input-registrations)
	(begin
	  ;; Busy-waiting here is a bad idea -- should implement a
	  ;; primitive to block the Scheme process while waiting for a
	  ;; signal.
	  (catch-errors
	   (lambda ()
	     (set-interrupt-enables! interrupt-mask/all)
	     (do () (#f)))))
	(let ((result
	       (catch-errors
		(lambda ()
		  (set-interrupt-enables! interrupt-mask/all)
		  (select-registry-test input-registry #t)))))
	  (set-interrupt-enables! interrupt-mask/gc-ok)
	  (signal-select-result result)
	  (let ((thread first-running-thread))
	    (if thread
		(if (thread/continuation thread)
		    (run-thread thread))
		(wait-for-input)))))))

(define (signal-select-result result)
  (cond ((pair? result)
	 (signal-input-thread-events result))
	((eq? 'PROCESS-STATUS-CHANGE result)
	 (signal-input-thread-events '(PROCESS-STATUS-CHANGE)))))

(define (block-on-input-descriptor descriptor)
  (without-interrupts
   (lambda ()
     (let ((result 'INTERRUPT)
	   (registration-1)
	   (registration-2))
       (dynamic-wind
	(lambda ()
	  (let ((thread (current-thread)))
	    (set! registration-1
		  (%register-input-thread-event
		   descriptor
		   thread
		   (lambda ()
		     (set! result 'INPUT-AVAILABLE)
		     unspecific)
		   #f #t))
	    (set! registration-2
		  (%register-input-thread-event
		   'PROCESS-STATUS-CHANGE
		   thread
		   (lambda ()
		     (set! result 'PROCESS-STATUS-CHANGE)
		     unspecific)
		   #f #t)))
	  unspecific)
	(lambda ()
	  (%suspend-current-thread)
	  result)
	(lambda ()
	  (%deregister-input-thread-event registration-1)
	  (%deregister-input-thread-event registration-2)))))))

(define (permanently-register-input-thread-event descriptor thread event)
  (guarantee-thread thread permanently-register-input-thread-event)
  (without-interrupts
   (lambda ()
     (%register-input-thread-event descriptor thread event #t #f))))

(define (register-input-thread-event descriptor thread event)
  (guarantee-thread thread register-input-thread-event)
  (without-interrupts
   (lambda ()
     (%register-input-thread-event descriptor thread event #f #f))))

(define (deregister-input-thread-event tentry)
  (if (not (tentry? tentry))
      (error:wrong-type-argument tentry "input thread event registration"
				 'DEREGISTER-INPUT-THREAD-EVENT))
  (without-interrupts
   (lambda ()
     (%deregister-input-thread-event tentry)
     (%maybe-toggle-thread-timer))))

(define (%register-input-thread-event descriptor thread event
				      permanent? front?)
  (let ((tentry (make-tentry thread event permanent?))
	(dentry
	 (let loop ((dentry input-registrations))
	   (and dentry
		(if (eqv? descriptor (dentry/descriptor dentry))
		    dentry
		    (loop (dentry/next dentry)))))))
    (if (not dentry)
	(let ((dentry (make-dentry descriptor #f #f #f #f)))
	  (set-tentry/dentry! tentry dentry)
	  (set-tentry/prev! tentry #f)
	  (set-tentry/next! tentry #f)
	  (set-dentry/first-tentry! dentry tentry)
	  (set-dentry/last-tentry! dentry tentry)
	  (if input-registrations
	      (set-dentry/prev! input-registrations dentry))
	  (set-dentry/next! dentry input-registrations)
	  (set! input-registrations dentry)
	  (if (not (eq? 'PROCESS-STATUS-CHANGE descriptor))
	      (add-to-select-registry! input-registry descriptor)))
	(begin
	  (set-tentry/dentry! tentry dentry)
	  (if front?
	      (let ((next (dentry/first-tentry dentry)))
		(set-tentry/prev! tentry #f)
		(set-tentry/next! tentry next)
		(set-dentry/first-tentry! dentry tentry)
		(set-tentry/prev! next tentry))
	      (let ((prev (dentry/last-tentry dentry)))
		(set-tentry/prev! tentry prev)
		(set-tentry/next! tentry #f)
		(set-dentry/last-tentry! dentry tentry)
		(set-tentry/next! prev tentry)))))
    (%maybe-toggle-thread-timer)
    tentry))

(define (%deregister-input-thread-event tentry)
  (if (tentry/dentry tentry)
      (delete-tentry! tentry)))

(define (%deregister-input-thread-events thread permanent?)
  (let loop ((dentry input-registrations) (tentries '()))
    (if (not dentry)
	(do ((tentries tentries (cdr tentries)))
	    ((null? tentries))
	  (delete-tentry! (car tentries)))
	(loop (dentry/next dentry)
	      (let loop
		  ((tentry (dentry/first-tentry dentry)) (tentries tentries))
		(if (not tentry)
		    tentries
		    (loop (tentry/next tentry)
			  (if (and (eq? thread (tentry/thread tentry))
				   (or permanent?
				       (not (tentry/permanent? tentry))))
			      (cons tentry tentries)
			      tentries))))))))

(define (signal-input-thread-events descriptors)
  (let loop ((dentry input-registrations) (events '()))
    (cond ((not dentry)
	   (do ((events events (cdr events)))
	       ((null? events))
	     (%signal-thread-event (caar events) (cdar events)))
	   (%maybe-toggle-thread-timer))
	  ((let ((descriptor (dentry/descriptor dentry)))
	     (let loop ((descriptors descriptors))
	       (and (not (null? descriptors))
		    (or (eqv? descriptor (car descriptors))
			(loop (cdr descriptors))))))
	   (let ((next (dentry/next dentry))
		 (tentry (dentry/first-tentry dentry)))
	     (let ((events
		    (cons (cons (tentry/thread tentry)
				(tentry/event tentry))
			  events)))
	       (if (tentry/permanent? tentry)
		   (move-tentry-to-back! tentry)
		   (delete-tentry! tentry))
	       (loop next events))))
	  (else
	   (loop (dentry/next dentry) events)))))

(define (move-tentry-to-back! tentry)
  (let ((next (tentry/next tentry)))
    (if next
	(let ((dentry (tentry/dentry tentry))
	      (prev (tentry/prev tentry)))
	  (set-tentry/prev! tentry (dentry/last-tentry dentry))
	  (set-tentry/next! tentry #f)
	  (set-dentry/last-tentry! dentry tentry)
	  (set-tentry/prev! next prev)
	  (if (not prev) (set-dentry/first-tentry! dentry next))))))

(define (delete-tentry! tentry)
  (let ((dentry (tentry/dentry tentry))
	(prev (tentry/prev tentry))
	(next (tentry/next tentry)))
    (set-tentry/dentry! tentry #f)
    (set-tentry/thread! tentry #f)
    (set-tentry/event! tentry #f)
    (set-tentry/prev! tentry #f)
    (set-tentry/next! tentry #f)
    (if prev
	(set-tentry/next! prev next)
	(set-dentry/first-tentry! dentry next))
    (if next
	(set-tentry/prev! next prev)
	(set-dentry/last-tentry! dentry prev))
    (if (not (or prev next))
	(begin
	  (let ((descriptor (dentry/descriptor dentry)))
	    (if (not (eq? 'PROCESS-STATUS-CHANGE descriptor))
		(remove-from-select-registry! input-registry descriptor)))
	  (let ((prev (dentry/prev dentry))
		(next (dentry/next dentry)))
	    (if prev
		(set-dentry/next! prev next)
		(set! input-registrations next))
	    (if next
		(set-dentry/prev! next prev))))))
  unspecific)

;;;; Events

(define (block-thread-events)
  (without-interrupts
   (lambda ()
     (let ((thread first-running-thread))
       (if thread
	   (let ((result (thread/block-events? thread)))
	     (set-thread/block-events?! thread #t)
	     result)
	   #f)))))

(define (unblock-thread-events)
  (without-interrupts
   (lambda ()
     (call-with-current-thread #t
       (lambda (thread)
	 (handle-thread-events thread)
	 (set-thread/block-events?! thread #f))))))

(define (with-thread-events-blocked thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (let ((thread first-running-thread))
      (if thread
	  (let ((block-events? (thread/block-events? thread)))
	    (set-thread/block-events?! thread #t)
	    (let ((value
		   ((ucode-primitive with-stack-marker 3)
		    (lambda ()
		      (set-interrupt-enables! interrupt-mask)
		      (let ((value (thunk)))
			(set-interrupt-enables! interrupt-mask/gc-ok)
			value))
		    with-thread-events-blocked
		    block-events?)))
	      (let ((thread first-running-thread))
		(if thread
		    (set-thread/block-events?! thread block-events?)))
	      (set-interrupt-enables! interrupt-mask)
	      value))
	  (begin
	    (set-interrupt-enables! interrupt-mask)
	    (thunk))))))

(define (get-thread-event-block)
  (without-interrupts
   (lambda ()
     (let ((thread first-running-thread))
       (if thread
	   (thread/block-events? thread)
	   #f)))))

(define (set-thread-event-block! block?)
  (without-interrupts
   (lambda ()
     (let ((thread first-running-thread))
       (if thread
	   (set-thread/block-events?! thread block?)))
     unspecific)))

(define (signal-thread-event thread event)
  (guarantee-thread thread signal-thread-event)
  (let ((self first-running-thread))
    (if (eq? thread self)
	(let ((block-events? (block-thread-events)))
	  (ring/enqueue (thread/pending-events thread) event)
	  (if (not block-events?)
	      (unblock-thread-events)))
	(without-interrupts
	 (lambda ()
	   (if (eq? 'DEAD (thread/execution-state thread))
	       (signal-thread-dead thread "signal event to"
				   signal-thread-event thread event))
	   (%signal-thread-event thread event)
	   (if (and (not self) first-running-thread)
	       (run-thread first-running-thread)
	       (%maybe-toggle-thread-timer)))))))

(define (%signal-thread-event thread event)
  (ring/enqueue (thread/pending-events thread) event)
  (if (and (not (thread/block-events? thread))
	   (eq? 'WAITING (thread/execution-state thread)))
      (%thread-running thread)))

(define (handle-thread-events thread)
  (let loop ((any-events? #f))
    (let ((event (ring/dequeue (thread/pending-events thread) #t)))
      (if (eq? #t event)
	  any-events?
	  (begin
	    (if event
		(let ((block? (thread/block-events? thread)))
		  (set-thread/block-events?! thread #t)
		  (event)
		  (set-interrupt-enables! interrupt-mask/gc-ok)
		  (set-thread/block-events?! thread block?)))
	    (loop #t))))))

(define (allow-thread-event-delivery)
  (without-interrupts
   (lambda ()
     (let ((thread first-running-thread))
       (if thread
	   (let ((block-events? (thread/block-events? thread)))
	     (set-thread/block-events?! thread #f)
	     (deliver-timer-events)
	     (maybe-signal-input-thread-events)
	     (handle-thread-events thread)
	     (set-thread/block-events?! thread block-events?))
	   (begin
	     (deliver-timer-events)
	     (maybe-signal-input-thread-events)))))))

;;;; Timer Events

(define timer-records)
(define timer-interval)

(define-structure (timer-record
		   (conc-name timer-record/))
  (time #f read-only #t)
  thread
  event
  next)

(define (register-timer-event interval event)
  (let ((time (+ (real-time-clock) interval)))
    (let ((new-record (make-timer-record time (current-thread) event #f)))
      (without-interrupts
       (lambda ()
	 (let loop ((record timer-records) (prev #f))
	   (if (or (not record) (< time (timer-record/time record)))
	       (begin
		 (set-timer-record/next! new-record record)
		 (if prev
		     (set-timer-record/next! prev new-record)
		     (set! timer-records new-record)))
	       (loop (timer-record/next record) record)))))
      new-record)))

(define (sleep-current-thread interval)
  (let ((delivered? #f))
    (let ((block-events? (block-thread-events)))
      (register-timer-event interval
			    (lambda () (set! delivered? #t) unspecific))
      (do () (delivered?)
	(suspend-current-thread))
      (if (not block-events?)
	  (unblock-thread-events)))))

(define (deliver-timer-events)
  (let ((time (real-time-clock)))
    (do ((record timer-records (timer-record/next record)))
	((or (not record) (< time (timer-record/time record)))
	 (set! timer-records record))
      (let ((thread (timer-record/thread record))
	    (event (timer-record/event record)))
	(set-timer-record/thread! record #f)
	(set-timer-record/event! record #f)
	(%signal-thread-event thread event))))
  unspecific)

(define (deregister-timer-event registration)
  (if (not (timer-record? registration))
      (error:wrong-type-argument registration "timer event registration"
				 'DEREGISTER-TIMER-EVENT))
  (without-interrupts
   (lambda ()
     (let loop ((record timer-records) (prev #f))
       (if record
	   (let ((next (timer-record/next record)))
	     (if (eq? record registration)
		 (if prev
		     (set-timer-record/next! prev next)
		     (set! timer-records next))
		 (loop next record)))))
     (%maybe-toggle-thread-timer))))

(define-integrable (threads-pending-timer-events?)
  timer-records)

(define (deregister-all-events)
  (let ((thread (current-thread)))
    (set-interrupt-enables! interrupt-mask/gc-ok)
    (let ((block-events? (thread/block-events? thread)))
      (set-thread/block-events?! thread #t)
      (ring/discard-all (thread/pending-events thread))
      (%deregister-input-thread-events thread #f)
      (%discard-thread-timer-records thread)
      (set-thread/block-events?! thread block-events?))
    (set-interrupt-enables! interrupt-mask/all)))

(define (%discard-thread-timer-records thread)
  (let loop ((record timer-records) (prev #f))
    (if record
	(let ((next (timer-record/next record)))
	  (if (eq? thread (timer-record/thread record))
	      (begin
		(if prev
		    (set-timer-record/next! prev next)
		    (set! timer-records next))
		(loop next prev))
	      (loop next record))))))

(define (thread-timer-interval)
  timer-interval)

(define (set-thread-timer-interval! interval)
  (if (not (or (false? interval)
	       (and (exact-integer? interval)
		    (> interval 0))))
      (error:wrong-type-argument interval #f 'SET-THREAD-TIMER-INTERVAL!))
  (without-interrupts
    (lambda ()
      (set! timer-interval interval)
      (%maybe-toggle-thread-timer))))

(define (start-thread-timer)
  (without-interrupts %maybe-toggle-thread-timer))

(define (stop-thread-timer)
  (without-interrupts %stop-thread-timer))

(define (with-thread-timer-stopped thunk)
  (dynamic-wind %stop-thread-timer thunk %maybe-toggle-thread-timer))

(define (%maybe-toggle-thread-timer)
  (cond ((and timer-interval
	      (or input-registrations
		  (let ((current-thread first-running-thread))
		    (and current-thread
			 (thread/next current-thread)))))
	 (%start-thread-timer timer-interval #t))
	(timer-records
	 (let ((next-event-time (timer-record/time timer-records)))
	   (let ((next-event-interval (- next-event-time (real-time-clock))))
	     (if (or (not timer-interval)
		     (> next-event-interval timer-interval))
		 (%start-thread-timer next-event-interval next-event-time)
		 (%start-thread-timer timer-interval #t)))))
	(else
	 (%stop-thread-timer))))

(define (%start-thread-timer interval time)
  ;; If TIME is #T, that means interval is TIMER-INTERVAL.  Otherwise,
  ;; INTERVAL is longer than TIMER-INTERVAL, and TIME is when INTERVAL
  ;; ends.  The cases are as follows:
  ;; 1. Timer not running: start it.
  ;; 2. Timer running TIMER-INTERVAL: do nothing.
  ;; 3. Timer running long interval, request sooner: restart it.
  ;; 4. Otherwise: do nothing.
  (if (or (not thread-timer-running?)
	  (and (not (eq? #t thread-timer-running?))
	       (< (if (eq? #t time)
		      (+ (real-time-clock) interval)
		      time)
		  thread-timer-running?)))
      (begin
	((ucode-primitive real-timer-set) interval interval)
	(set! thread-timer-running? time)
	unspecific)))

(define (%stop-thread-timer)
  (if thread-timer-running?
      (begin
	((ucode-primitive real-timer-clear))
	(set! thread-timer-running? #f)
	((ucode-primitive clear-interrupts!) interrupt-bit/timer))))

;;;; Mutexes

(define-structure (thread-mutex
		   (constructor make-thread-mutex ())
		   (conc-name thread-mutex/))
  (waiting-threads (make-ring) read-only #t)
  (owner #f))

(define-integrable (guarantee-thread-mutex mutex procedure)
  (if (not (thread-mutex? mutex))
      (error:wrong-type-argument mutex "thread-mutex" procedure)))

(define (thread-mutex-owner mutex)
  (guarantee-thread-mutex mutex thread-mutex-owner)
  (thread-mutex/owner mutex))

(define (lock-thread-mutex mutex)
  (guarantee-thread-mutex mutex lock-thread-mutex)
  (without-interrupts
   (lambda ()
     (let ((thread (current-thread))
	   (owner (thread-mutex/owner mutex)))
       (if (eq? owner thread)
	   (signal-thread-deadlock thread "lock thread mutex"
				   lock-thread-mutex mutex))
       (%lock-thread-mutex mutex thread owner)))))

(define (%lock-thread-mutex mutex thread owner)
  (add-thread-mutex! thread mutex)
  (if owner
      (begin
	(ring/enqueue (thread-mutex/waiting-threads mutex) thread)
	(do () ((eq? thread (thread-mutex/owner mutex)))
	  (%suspend-current-thread)))
      (set-thread-mutex/owner! mutex thread)))

(define (unlock-thread-mutex mutex)
  (guarantee-thread-mutex mutex unlock-thread-mutex)
  (without-interrupts
   (lambda ()
     (let ((owner (thread-mutex/owner mutex)))
       (if (and thread (not (eq? owner (current-thread))))
	   (error "Don't own mutex:" mutex))
       (%unlock-thread-mutex mutex owner)))))

(define (%unlock-thread-mutex mutex owner)
  (remove-thread-mutex! owner mutex)
  (if (%%unlock-thread-mutex mutex)
      (%maybe-toggle-thread-timer)))

(define (%%unlock-thread-mutex mutex)
  (let ((thread (ring/dequeue (thread-mutex/waiting-threads mutex) #f)))
    (set-thread-mutex/owner! mutex thread)
    (if thread (%signal-thread-event thread #f))
    thread))

(define (try-lock-thread-mutex mutex)
  (guarantee-thread-mutex mutex try-lock-thread-mutex)
  (without-interrupts
   (lambda ()
     (and (not (thread-mutex/owner mutex))
	  (let ((thread (current-thread)))
	    (set-thread-mutex/owner! mutex thread)
	    (add-thread-mutex! thread mutex)
	    #t)))))

(define (with-thread-mutex-locked mutex thunk)
  (guarantee-thread-mutex mutex lock-thread-mutex)
  (let ((thread (current-thread))
	(grabbed-lock?))
    (dynamic-wind
     (lambda ()
       (let ((owner (thread-mutex/owner mutex)))
	 (if (eq? owner thread)
	     (begin
	       (set! grabbed-lock? #f)
	       unspecific)
	     (begin
	       (set! grabbed-lock? #t)
	       (%lock-thread-mutex mutex thread owner)))))
     thunk
     (lambda ()
       (if (and grabbed-lock? (eq? (thread-mutex/owner mutex) thread))
	   (%unlock-thread-mutex mutex thread))))))

(define (%disassociate-thread-mutexes thread)
  (do ((mutexes (thread/mutexes thread) (cdr mutexes)))
      ((null? mutexes))
    (let ((mutex (car mutexes)))
      (if (eq? (thread-mutex/owner mutex) thread)
	  (%%unlock-thread-mutex mutex)
	  (ring/remove-item (thread-mutex/waiting-threads mutex) thread))))
  (set-thread/mutexes! thread '()))

(define-integrable (add-thread-mutex! thread mutex)
  (set-thread/mutexes! thread (cons mutex (thread/mutexes thread))))

(define-integrable (remove-thread-mutex! thread mutex)
  (set-thread/mutexes! thread (delq! mutex (thread/mutexes thread))))

;;;; Circular Rings

(define-structure (link (conc-name link/))
  prev
  next
  item)

(define (make-ring)
  (let ((link (make-link #f #f #f)))
    (set-link/prev! link link)
    (set-link/next! link link)
    link))

(define-integrable (ring/empty? ring)
  (eq? (link/next ring) ring))

(define (ring/enqueue ring item)
  (let ((prev (link/prev ring)))
    (let ((link (make-link prev ring item)))
      (set-link/next! prev link)
      (set-link/prev! ring link))))

(define (ring/dequeue ring default)
  (let ((link (link/next ring)))
    (if (eq? link ring)
	default
	(begin
	  (let ((next (link/next link)))
	    (set-link/next! ring next)
	    (set-link/prev! next ring))
	  (link/item link)))))

(define (ring/discard-all ring)
  (set-link/prev! ring ring)
  (set-link/next! ring ring))

(define (ring/remove-item ring item)
  (let loop ((link (link/next ring)))
    (if (not (eq? link ring))
	(if (eq? (link/item link) item)
	    (let ((prev (link/prev link))
		  (next (link/next link)))
	      (set-link/next! prev next)
	      (set-link/prev! next prev))
	    (loop (link/next link))))))

;;;; Error Conditions

(define condition-type:thread-control-error)
(define thread-control-error/thread)
(define condition-type:thread-deadlock)
(define signal-thread-deadlock)
(define thread-deadlock/description)
(define thread-deadlock/operator)
(define thread-deadlock/operand)
(define condition-type:thread-detached)
(define signal-thread-detached)
(define condition-type:thread-dead)
(define signal-thread-dead)
(define thread-dead/verb)
(define condition-type:no-current-thread)

(define (initialize-error-conditions!)
  (set! condition-type:thread-control-error
	(make-condition-type 'THREAD-CONTROL-ERROR condition-type:control-error
	    '(THREAD)
	  (lambda (condition port)
	    (write-string "Anonymous error associated with " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string "." port))))
  (set! thread-control-error/thread
	(condition-accessor condition-type:thread-control-error 'THREAD))

  (set! condition-type:thread-deadlock
	(make-condition-type 'THREAD-DEADLOCK
	    condition-type:thread-control-error
	    '(DESCRIPTION OPERATOR OPERAND)
	  (lambda (condition port)
	    (write-string "Deadlock detected while trying to " port)
	    (write-string (thread-deadlock/description condition) port)
	    (write-string ": " port)
	    (write (thread-deadlock/operand condition) port)
	    (write-string "." port))))
  (set! signal-thread-deadlock
	(condition-signaller condition-type:thread-deadlock
			     '(THREAD DESCRIPTION OPERATOR OPERAND)
			     standard-error-handler))
  (set! thread-deadlock/description
	(condition-accessor condition-type:thread-deadlock 'DESCRIPTION))
  (set! thread-deadlock/operator
	(condition-accessor condition-type:thread-deadlock 'OPERATOR))
  (set! thread-deadlock/operand
	(condition-accessor condition-type:thread-deadlock 'OPERAND))

  (set! condition-type:thread-detached
	(make-condition-type 'THREAD-DETACHED
	    condition-type:thread-control-error
	    '()
	  (lambda (condition port)
	    (write-string "Attempt to join detached thread: " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string "." port))))
  (set! signal-thread-detached
	(condition-signaller condition-type:thread-detached
			     '(THREAD)
			     standard-error-handler))

  (set! condition-type:thread-dead
	(make-condition-type 'THREAD-DEAD condition-type:thread-control-error
	    '(VERB OPERATOR OPERANDS)
	  (lambda (condition port)
	    (write-string "Unable to " port)
	    (write-string (thread-dead/verb condition) port)
	    (write-string " thread " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string " because it is dead." port))))
  (set! signal-thread-dead
	(let ((signaller
	       (condition-signaller condition-type:thread-dead
				    '(THREAD VERB OPERATOR OPERANDS)
				    standard-error-handler)))
	  (lambda (thread verb operator . operands)
	    (signaller thread verb operator operands))))
  (set! thread-dead/verb
	(condition-accessor condition-type:thread-dead 'VERB))

  (set! condition-type:no-current-thread
	(make-condition-type 'NO-CURRENT-THREAD condition-type:control-error
	    '()
	  (lambda (condition port)
	    condition
	    (write-string "No current thread!" port))))
  unspecific)