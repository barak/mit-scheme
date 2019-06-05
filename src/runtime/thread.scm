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

;;;; Multiple Threads of Control
;;; package: (runtime thread)

(declare (usual-integrations))

;;; This allows a host without the SMP primitives to avoid calling them.
(define enable-smp? #f)

(define-structure (thread
		   (constructor %make-thread (properties))
		   (conc-name thread/))
  (execution-state 'running)
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
  ;; If #t, events may not run in this thread and should be queued.
  ;; If 'suspended, events were blocked when the thread suspended.
  ;; Events should wake the thread and %resume-current-thread should
  ;; run them but then it should continue with events blocked (#t).

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

  (floating-point-environment #f)
  ;; A floating-point environment descriptor, or #T if the thread is
  ;; running and has modified its floating-point environment since it
  ;; was last cached.  While a thread is running, this is a cache of
  ;; the machine's floating-point environment.

  (mutexes '())
  ;; List of mutexes that this thread owns or is waiting to own.  Used
  ;; to disassociate the thread from those mutexes when it is exited.

  (start-times #f)
  ;; The system times when this thread last started running.

  (process-time 0)
  (real-time 0)
  ;; The total system and real times during which this thread has run.

  (properties #f read-only #t))

(define no-exit-value-marker
  (list 'no-exit-value-marker))

(define (thread-dead? thread)
  (guarantee thread? thread 'thread-dead?)
  (eq? 'dead (thread/execution-state thread)))

(define (thread-get thread property)
  (guarantee thread? thread 'thread-get)
  (without-interrupts
   (lambda ()
     (1d-table/get (thread/properties thread) property #f))))

(define (thread-put! thread property value)
  (guarantee thread? thread 'thread-put!)
  (without-interrupts
   (lambda ()
     (1d-table/put! (thread/properties thread) property value))))

(define thread-population)
(define first-running-thread)
(define last-running-thread)
(define next-scheduled-timeout)
(define root-continuation-default)

(define (initialize-low!)
  ;; Called early in the cold load to create the first thread.
  (set! thread-population (make-population/unsafe))
  (set! first-running-thread #f)
  (set! last-running-thread #f)
  (set! next-scheduled-timeout #f)
  (set! timer-records #f)
  (set! timer-interval 100)
  (reset-threads-low!)
  (let* ((properties (make-1d-table/unsafe))
	 (first (%make-thread properties)))
    (1d-table/put! properties 'name 'initial)
    (set-thread/exit-value! first detached-thread-marker)
    (set-thread/root-state-point! first
				  (current-state-point state-space:local))
    (add-to-population!/unsafe thread-population first)
    (%thread-running first)))

(define (initialize-high!)
  ;; Called later in the cold load, when more of the runtime is initialized.
  (set! root-continuation-default (make-unsettable-parameter #f))
  (initialize-error-conditions!)
  (reset-threads-high!)
  (record-start-times! first-running-thread)
  (add-event-receiver! event:after-restore reset-threads!)
  (add-event-receiver! event:before-exit stop-thread-timer)
  (named-structure/set-tag-description! thread-mutex-tag
    (make-define-structure-type 'vector
				"thread-mutex"
				'#(waiting-threads owner)
				'#(1 2)
				(vector 2 (lambda () #f))
				#f
				thread-mutex-tag
				3))
  (named-structure/set-tag-description! link-tag
    (make-define-structure-type 'vector
				"link"
				'#(prev next item)
				'#(1 2 3)
				(vector 3 (lambda () #f))
				#f
				link-tag
				4)))

(define-print-method link?
  (standard-print-method 'link))

(define (reset-threads!)
  (reset-threads-low!)
  (reset-threads-high!))

(define (reset-threads-low!)
  (set! enable-smp?
	(and ((ucode-primitive get-primitive-address 2) 'smp-count #f)
	     ((ucode-primitive smp-count 0)))))

(define (reset-threads-high!)
  (set! io-registry (and have-select? (make-select-registry)))
  (set! io-registrations #f)
  (set! subprocess-registrations '())
  (map-over-population! thread-population
    (let ((times (get-system-times)))
      (lambda (thread)
	(if (thread/start-times thread)
	    (set-thread/start-times! thread times))))))

(define (make-thread continuation)
  (let ((thread (%make-thread (make-1d-table))))
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

(define (with-obarray-lock thunk)
  ;; Serialize with myriad parts of the microcode that hack the
  ;; obarray element of the fixed-objects vector.
  (if enable-smp?
      (without-interrupts
       (lambda ()
	 (if (not (eq? #t ((ucode-primitive smp-lock-obarray 1) #t)))
	     (outf-error "\nwith-obarray-lock: lock failed\n"))
	 (let ((value (thunk)))
	   (if (not (eq? #t ((ucode-primitive smp-lock-obarray 1) #f)))
	       (outf-error "\nwith-obarray-lock: unlock failed\n"))
	   value)))
      (without-interrupts thunk)))

(define (threads-list)
  (map-over-population thread-population (lambda (thread) thread)))

(define (thread-execution-state thread)
  (guarantee thread? thread 'thread-execution-state)
  (thread/execution-state thread))

(define (create-thread root-continuation thunk #!optional name)
  (if (not (or (not root-continuation) (continuation? root-continuation)))
      (error:wrong-type-argument root-continuation
				 "continuation or #f"
				 create-thread))
  (let ((root-continuation
	 (or root-continuation (root-continuation-default))))
    (call-with-current-continuation
     (lambda (return)
       (%within-continuation root-continuation #t
	 (lambda ()
	   (call-with-new-local-state-space
	    (lambda ()
	      (call-with-current-continuation
		(lambda (continuation)
		  (let ((thread (make-thread continuation)))
		    (if (not (default-object? name))
			(1d-table/put! (thread/properties thread)
				       'name name))
		    (%within-continuation (let ((k return)) (set! return #f) k)
					  #t
					  (lambda () thread)))))
	      (set-interrupt-enables! interrupt-mask/all)
	      (exit-current-thread
	       (with-create-thread-continuation root-continuation
						thunk))))))))))

(define (call-with-new-local-state-space thunk)
  (let ((temp (make-state-space)))
    (let ((swap!
	   (lambda ()
	     (set! state-space:local (set! temp (set! state-space:local)))
	     unspecific)))
      (shallow-fluid-bind swap! thunk swap!))))

(define (create-thread-continuation)
  (root-continuation-default))

(define (with-create-thread-continuation continuation thunk)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation
				 "continuation"
				 with-create-thread-continuation))
  (parameterize ((root-continuation-default continuation))
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
				      'bound-restarts
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
  (thread-mutex-owner (textual-port-thread-mutex (console-i/o-port))))

(define (other-running-threads?)
  (thread/next (current-thread)))

(define (thread-continuation thread)
  (guarantee thread? thread 'thread-continuation)
  (without-interrupts
   (lambda ()
     (and (eq? 'waiting (thread/execution-state thread))
	  (thread/continuation thread)))))

(define (thread-running thread)
  (%thread-running thread)
  (%maybe-toggle-thread-timer))

(define (%thread-running thread)
  (set-thread/execution-state! thread 'running)
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
	(wait-for-io))))

(define (run-thread thread)
  (let ((continuation (thread/continuation thread))
	(fp-env (thread/floating-point-environment thread)))
    (set-thread/continuation! thread #f)
    (%within-continuation continuation #t
      (lambda ()
	(enter-float-environment fp-env)
	(record-start-times! thread)
	(%resume-current-thread thread)))))

(define (%resume-current-thread thread)
  (let ((block-events? (thread/block-events? thread)))
    (cond ((eq? #f block-events?)
	   (handle-thread-events thread))
	  ((eq? 'suspended block-events?)
	   (handle-thread-events thread)
	   (set-thread/block-events?! thread #t))))
  (%maybe-toggle-thread-timer))

(define (suspend-current-thread)
  (without-interrupts %suspend-current-thread))

(define (%suspend-current-thread)
  (call-with-current-thread #f
    (lambda (thread)
      (let ((block-events? (thread/block-events? thread)))
	(set-thread/block-events?! thread (and block-events? 'suspended))
	(maybe-signal-io-thread-events)
	(let ((any-events? (handle-thread-events thread)))
	  (if any-events?
	      (begin
		(set-thread/block-events?! thread block-events?)
		(%maybe-toggle-thread-timer))
	      (call-with-current-continuation
	       (lambda (continuation)
		 (set-thread/continuation! thread continuation)
		 (maybe-save-thread-float-environment! thread)
		 (account-for-times thread (get-system-times))
		 (thread-not-running thread 'waiting)))))))))

(define (stop-current-thread)
  (without-interrupts
   (lambda ()
     (call-with-current-thread #f
       (lambda (thread)
	 (call-with-current-continuation
	  (lambda (continuation)
	    (set-thread/continuation! thread continuation)
	    (maybe-save-thread-float-environment! thread)
	    (account-for-times thread (get-system-times))
	    (thread-not-running thread 'stopped))))))))

(define (restart-thread thread discard-events? event)
  (guarantee thread? thread 'restart-thread)
  (let ((discard-events?
	 (if (eq? discard-events? 'ask)
	     (prompt-for-confirmation
	      "Restarting other thread; discard events in its queue")
	     discard-events?)))
    (without-interrupts
     (lambda ()
       (if (not (eq? 'stopped (thread/execution-state thread)))
	   (error:bad-range-argument thread restart-thread))
       (if discard-events? (ring/discard-all (thread/pending-events thread)))
       (if event (%signal-thread-event thread event))
       (thread-running thread)))))

(define (disallow-preempt-current-thread)
  (set-thread/execution-state! (current-thread) 'running-without-preemption))

(define (allow-preempt-current-thread)
  (set-thread/execution-state! (current-thread) 'running))

(define (thread-timer-interrupt-handler)
  ;; Preserve the floating-point environment here to guarantee that the
  ;; thread timer won't raise or clear exceptions (particularly the
  ;; inexact result exception) that the interrupted thread cares about.
  (let* ((times (get-system-times))
	 (fp-env (enter-default-float-environment first-running-thread)))
    (set! next-scheduled-timeout #f)
    (set-interrupt-enables! interrupt-mask/gc-ok)
    (account-for-times first-running-thread times)
    (deliver-timer-events times)
    (maybe-signal-io-thread-events)
    (let ((thread first-running-thread))
      (cond ((not thread)
	     (%maybe-toggle-thread-timer))
	    ((thread/continuation thread)
	     (run-thread thread))
	    ((not (eq? 'running-without-preemption
		       (thread/execution-state thread)))
	     (yield-thread thread fp-env))
	    (else
	     (restore-float-environment-from-default fp-env)
	     (record-start-times! thread)
	     (%resume-current-thread thread))))))

(define (get-system-times)
  (cons (real-time-clock) (process-time-clock)))

(define-integrable system-times/real car)
  
(define-integrable system-times/process cdr)

(define (record-start-times! thread)
  (if (not (eq? #f (thread/start-times thread)))
      (outf-error "\n;record-start-times!: already recorded!\n"))
  (set-thread/start-times! thread (get-system-times)))

(define (account-for-times thread end)
  (if thread
      (let ((start (thread/start-times thread)))
	(if (eq? #f start)
	    (outf-error "\n;account-for-times: start time not recorded\n")
	    (begin
	      (set-thread/process-time! thread
					(+ (thread/process-time thread)
					   (- (system-times/process end)
					      (system-times/process start))))
	      (set-thread/real-time! thread
				     (+ (thread/real-time thread)
					(- (system-times/real end)
					   (system-times/real start))))
	      (set-thread/start-times! thread #f))))))

(define (yield-current-thread)
  (without-interrupts
   (lambda ()
     (call-with-current-thread #t
       (lambda (thread)
	 (account-for-times thread (get-system-times))
	 ;; Allow preemption now, since the current thread has
	 ;; volunteered to yield control.
	 (set-thread/execution-state! thread 'running)
	 (maybe-signal-io-thread-events)
	 (yield-thread thread))))))

(define (yield-thread thread #!optional fp-env)
  (let ((next (thread/next thread)))
    (if (not next)
	(begin
	  (if (not (default-object? fp-env))
	      (restore-float-environment-from-default fp-env))
	  (record-start-times! thread)
	  (%resume-current-thread thread))
	(call-with-current-continuation
	 (lambda (continuation)
	   (set-thread/continuation! thread continuation)
	   (maybe-save-thread-float-environment! thread fp-env)
	   (set-thread/next! thread #f)
	   (set-thread/next! last-running-thread thread)
	   (set! last-running-thread thread)
	   (set! first-running-thread next)
	   (run-thread next))))))

(define (thread-float-environment thread)
  (thread/floating-point-environment thread))

(define (set-thread-float-environment! thread fp-env)
  (set-thread/floating-point-environment! thread fp-env))

(define (exit-current-thread value)
  (let ((thread (current-thread)))
    (set-interrupt-enables! interrupt-mask/gc-ok)
    (set-thread/block-events?! thread #t)
    (ring/discard-all (thread/pending-events thread))
    (translate-to-state-point (thread/root-state-point thread))
    (%deregister-io-thread-events thread)
    (%discard-thread-timer-records thread)
    (%deregister-subprocess-events thread)
    (%disassociate-joined-threads thread)
    (%disassociate-thread-mutexes thread)
    (if (eq? no-exit-value-marker (thread/exit-value thread))
	(release-joined-threads thread value))
    (thread-not-running thread 'dead)))

(define (join-thread thread event-constructor)
  (guarantee thread? thread 'join-thread)
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
  (guarantee thread? thread 'detach-thread)
  (without-interrupts
   (lambda ()
     (if (eq? (thread/exit-value thread) detached-thread-marker)
	 (signal-thread-detached thread))
     (release-joined-threads thread detached-thread-marker))))

(define detached-thread-marker
  (list 'detached-thread-marker))

(define (release-joined-threads thread value)
  (set-thread/exit-value! thread value)
  (do ((joined (thread/joined-threads thread) (cdr joined)))
      ((not (pair? joined)))
    (let ((joined (caar joined))
	  (event ((cdar joined) thread value)))
      (set-thread/joined-to! joined (delq! thread (thread/joined-to joined)))
      (%signal-thread-event joined event)))
  (%maybe-toggle-thread-timer))

(define (%disassociate-joined-threads thread)
  (do ((threads (thread/joined-to thread) (cdr threads)))
      ((not (pair? threads)))
    (set-thread/joined-threads!
     (car threads)
     (del-assq! thread (thread/joined-threads (car threads)))))
  (set-thread/joined-to! thread '()))

;;;; IO Thread Events

(define io-registry)
(define io-registrations)

(define-structure (dentry (conc-name dentry/))
  (descriptor #f read-only #t)
  (mode #f read-only #t)
  first-tentry
  last-tentry
  prev
  next)

(define-structure (tentry (conc-name tentry/)
			  (constructor make-tentry (thread event)))
  dentry
  thread
  event
  prev
  next)

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
	  (remove-from-select-registry! io-registry
					(dentry/descriptor dentry)
					(dentry/mode dentry))
	  (let ((prev (dentry/prev dentry))
		(next (dentry/next dentry)))
	    (if prev
		(set-dentry/next! prev next)
		(set! io-registrations next))
	    (if next
		(set-dentry/prev! next prev)))))))

(define (wait-for-io)
  (%maybe-toggle-thread-timer #f)
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
    (let ((result
	   (catch-errors
	    (lambda ()
	      (set-interrupt-enables! interrupt-mask/all)
	      (test-select-registry io-registry #t)))))
      (set-interrupt-enables! interrupt-mask/gc-ok)
      (signal-select-result result)
      (let ((thread first-running-thread))
	(if thread
	    (if (thread/continuation thread)
		(run-thread thread)
		(%maybe-toggle-thread-timer))
	    (wait-for-io))))))

(define (signal-select-result result)
  (cond ((vector? result)
	 (signal-io-thread-events (vector-ref result 0)
				  (vector-ref result 1)
				  (vector-ref result 2)))
	((eq? 'process-status-change result)
	 (%handle-subprocess-status-change))))

(define (maybe-signal-io-thread-events)
  (if (or io-registrations
	  (not (null? subprocess-registrations)))
      (signal-select-result (test-select-registry io-registry #f))))

(define (block-on-io-descriptor descriptor mode)
  (let ((result 'interrupt)
	(registration #f))
    (dynamic-wind
     (lambda ()
       (if registration (error "Re-entered block-on-io-descrptor."))
       (set! registration
	     (register-io-thread-event descriptor mode (current-thread)
				       (named-lambda (block-on-io-event mode)
					 (set! result mode)))))
     (lambda ()
       (with-thread-events-blocked
	(lambda ()
	  (if (eq? result 'interrupt)
	      (suspend-current-thread)))))
     (lambda ()
       (if (and registration
		;; Ensure another thread does not de-register our IO event.
		(eq? (current-thread) (tentry/thread registration)))
	   (begin
	     (deregister-io-thread-event registration)
	     (set! registration #f)))))
    result))

(define (permanently-register-io-thread-event descriptor mode thread event)
  (let ((stop? #f)
	(registration #f))
    (letrec ((handler
	      (named-lambda (permanent-io-event mode*)
		(if (not stop?)
		    (event mode*))
		(if (not (or stop? (memq mode* '(error hangup #f))))
		    (register))))
	     (register
	      (lambda ()
		(deregister)
		(if (not stop?)
		    (set! registration
			  (register-io-thread-event descriptor mode
						    thread handler)))))
	     (deregister
	      (lambda ()
		(if registration
		    (begin
		      (deregister-io-thread-event registration)
		      (set! registration #f))))))
      (register)
      (cons 'deregister-permanent-io-event
	    (lambda ()
	      (set! stop? #t)
	      (deregister))))))

(define (register-io-thread-event descriptor mode thread event)
  (guarantee-select-mode mode 'register-io-thread-event)
  (guarantee thread? thread 'register-io-thread-event)
  (without-interrupts
   (lambda ()
     (let ((registration
	    (%register-io-thread-event descriptor mode thread event)))
       (%maybe-toggle-thread-timer)
       registration))))

(define (%register-io-thread-event descriptor mode thread event)
  (let ((tentry (make-tentry thread event)))
    (let loop ((dentry io-registrations))
      (cond ((not dentry)
	     (let ((dentry
		    (make-dentry descriptor
				 mode
				 tentry
				 tentry
				 #f
				 io-registrations)))
	       (set-tentry/dentry! tentry dentry)
	       (set-tentry/prev! tentry #f)
	       (set-tentry/next! tentry #f)
	       (if io-registrations
		   (set-dentry/prev! io-registrations dentry))
	       (set! io-registrations dentry)
	       (add-to-select-registry! io-registry descriptor mode)))
	    ((and (eqv? descriptor (dentry/descriptor dentry))
		  (eq? mode (dentry/mode dentry)))
	     (set-tentry/dentry! tentry dentry)
	     (let ((prev (dentry/last-tentry dentry)))
	       (set-tentry/prev! tentry prev)
	       (set-tentry/next! tentry #f)
	       (set-dentry/last-tentry! dentry tentry)
	       (set-tentry/next! prev tentry)))
	    (else
	     (loop (dentry/next dentry)))))
    tentry))

(define (deregister-io-thread-event registration)
  (if (and (pair? registration)
	   (eq? (car registration) 'deregister-permanent-io-event))
      ((cdr registration))
      (deregister-io-thread-event* registration)))

(define (deregister-io-thread-event* tentry)
  (if (not (tentry? tentry))
      (error:wrong-type-argument tentry "IO thread event registration"
				 'deregister-io-thread-event))
  (without-interrupts
   (lambda ()
     (%deregister-io-thread-event tentry)
     (%maybe-toggle-thread-timer))))

(define (deregister-io-descriptor-events descriptor mode)
  (guarantee-select-mode mode 'deregister-io-descriptor-events)
  (without-interrupts
   (lambda ()
     (let loop ((dentry io-registrations))
       (cond ((not dentry)
	      unspecific)
	     ((and (eqv? descriptor (dentry/descriptor dentry))
		   (eq? mode (dentry/mode dentry)))
	      (remove-from-select-registry! io-registry descriptor mode)
	      (let ((prev (dentry/prev dentry))
		    (next (dentry/next dentry)))
		(if prev
		    (set-dentry/next! prev next)
		    (set! io-registrations next))
		(if next
		    (set-dentry/prev! next prev))))
	     (else
	      (loop (dentry/next dentry)))))
     (%maybe-toggle-thread-timer))))

(define (%deregister-io-descriptor descriptor)
  (let dloop ((dentry io-registrations))
    (cond ((not dentry)
	   unspecific)
	  ((eqv? descriptor (dentry/descriptor dentry))
	   (let tloop ((tentry (dentry/first-tentry dentry)))
	     (if tentry
		 (let ((thread (tentry/thread tentry))
		       (event (tentry/event tentry)))
		   (%signal-thread-event thread
					 (and event
					      (lambda () (event #f))))
		   (tloop (tentry/next tentry)))))
	   (remove-from-select-registry! io-registry
					 (dentry/descriptor dentry)
					 (dentry/mode dentry))
	   (let ((prev (dentry/prev dentry))
		 (next (dentry/next dentry)))
	     (if prev
		 (set-dentry/next! prev next)
		 (set! io-registrations next))
	     (if next
		 (set-dentry/prev! next prev)))
	   (dloop (dentry/next dentry)))
	  (else
	   (dloop (dentry/next dentry)))))
  (%maybe-toggle-thread-timer))

(define (%deregister-io-thread-event tentry)
  (if (tentry/dentry tentry)
      (delete-tentry! tentry)))

(define (%deregister-io-thread-events thread)
  (let loop ((dentry io-registrations) (tentries '()))
    (if (not dentry)
	(do ((tentries tentries (cdr tentries)))
	    ((not (pair? tentries)))
	  (delete-tentry! (car tentries)))
	(loop (dentry/next dentry)
	      (let loop
		  ((tentry (dentry/first-tentry dentry)) (tentries tentries))
		(if (not tentry)
		    tentries
		    (loop (tentry/next tentry)
			  (if (eq? thread (tentry/thread tentry))
			      (cons tentry tentries)
			      tentries))))))))

(define (guarantee-select-mode mode procedure)
  (if (not (memq mode '(read write read-write)))
      (error:wrong-type-argument mode "select mode" procedure)))

(define (signal-io-thread-events n vfd vmode)
  (let ((search
	 (lambda (descriptor predicate)
	   (let scan-dentries ((dentry io-registrations))
	     (and dentry
		  (if (and (eqv? descriptor (dentry/descriptor dentry))
			   (predicate (dentry/mode dentry)))
		      dentry
		      (scan-dentries (dentry/next dentry))))))))
    (let loop ((i 0) (events '()))
      (if (fix:< i n)
	  (let ((descriptor (vector-ref vfd i))
		(mode (vector-ref vmode i)))
	    (let ((dentry
		   (search
		    descriptor
		    (case mode
		      ((read) (lambda (mode) (memq mode '(read read/write))))
		      ((write) (lambda (mode) (memq mode '(write read/write))))
		      ((read/write) (lambda (mode) mode))
		      ((error hangup) (lambda (mode) mode #t))
		      (else (error "Illegal mode:" mode))))))
	      (if (not dentry)
		  (loop (fix:+ i 1) events)
		  (let ((tentry (dentry/first-tentry dentry)))
		    (let ((events
			   (cons (cons (tentry/thread tentry)
				       (let ((e (tentry/event tentry)))
					 (and e
					      (lambda () (e mode)))))
				 events)))
		      (delete-tentry! tentry)
		      (loop (fix:+ i 1) events))))))
	  (do ((events events (cdr events)))
	      ((not (pair? events)))
	    (%signal-thread-event (caar events) (cdar events)))))))

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
		    'with-thread-events-blocked
		    block-events?)))
	      (%set-thread-event-block! block-events?)
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
     (%set-thread-event-block! block?))))

(define (%set-thread-event-block! block?)
  (let ((thread first-running-thread))
    (if thread
	(begin
	  (if (not block?)
	      (handle-thread-events thread))
	  (set-thread/block-events?! thread block?))))
  unspecific)

(define (signal-thread-event thread event #!optional no-error?)
  (guarantee thread? thread 'signal-thread-event)
  (let ((self first-running-thread)
	(noerr? (and (not (default-object? no-error?))
		     no-error?)))
    (if (eq? thread self)
	(let ((block-events? (block-thread-events)))
	  (%add-pending-event thread event)
	  (if (not block-events?)
	      (unblock-thread-events)))
	(without-interrupts
	 (lambda ()
	   (if (eq? 'dead (thread/execution-state thread))
	       (if (not noerr?)
		   (signal-thread-dead thread "signal event to"
				       signal-thread-event thread event))
	       (begin
		 (%signal-thread-event thread event)
		 (if (and (not self) first-running-thread)
		     (run-thread first-running-thread)
		     (%maybe-toggle-thread-timer)))))))))

(define (%signal-thread-event thread event)
  (%add-pending-event thread event)
  (if (and (not (eq? #t (thread/block-events? thread)))
	   (eq? 'waiting (thread/execution-state thread)))
      (%thread-running thread)))

(define (%add-pending-event thread event)
  ;; PENDING-EVENTS has three states: (1) empty; (2) one #F event; or
  ;; (3) any number of non-#F events.  This optimizes #F events away
  ;; when they aren't needed.
  (let ((ring (thread/pending-events thread)))
    (let ((count (ring/count-max-2 ring)))
      (if event
	  (if (and (fix:= count 1)
		   (not (ring/first-item ring)))
	      (ring/set-first-item! ring event)
	      (ring/enqueue ring event))
	  (if (fix:= count 0)
	      (ring/enqueue ring event))))))

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
	     (deliver-timer-events (get-system-times))
	     (maybe-signal-io-thread-events)
	     (handle-thread-events thread)
	     (set-thread/block-events?! thread block-events?))
	   (begin
	     (deliver-timer-events (get-system-times))
	     (maybe-signal-io-thread-events))))
     (%maybe-toggle-thread-timer))))

(define (handle-current-thread-events)
  (without-interrupts
   (lambda ()
     (let ((thread first-running-thread))
       (if (and thread
		(not (thread/block-events? thread)))
	   (handle-thread-events thread))))))

;;;; Subprocess Events

(define subprocess-registrations)
(define subprocess-support-loaded? #f)

(define (%deregister-subprocess-events thread)
  (if subprocess-support-loaded?
      (deregister-subprocess-events thread)))

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
  (register-time-event (+ (real-time-clock) interval) event))

(define (register-time-event time event)
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
	       (loop (timer-record/next record) record)))
	 (%maybe-toggle-thread-timer)))
      new-record))

(define (sleep-current-thread interval)
  (let ((delivered? #f))
    (let ((block-events? (block-thread-events)))
      (register-timer-event interval
			    (lambda () (set! delivered? #t) unspecific))
      (do () (delivered?)
	(suspend-current-thread))
      (if (not block-events?)
	  (unblock-thread-events)))))

(define (deliver-timer-events times)
  (let ((time (system-times/real times)))
    (do ((record timer-records (timer-record/next record)))
	((or (not record) (< time (timer-record/time record)))
	 (set! timer-records record)
	 unspecific)
      (let ((thread (timer-record/thread record))
	    (event (timer-record/event record)))
	(set-timer-record/thread! record #f)
	(set-timer-record/event! record #f)
	(%signal-thread-event thread event)))))

(define (deregister-time-event registration)
  (if (not (timer-record? registration))
      (error:wrong-type-argument registration "timer event registration"
				 'deregister-timer-event))
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

(define-integrable (deregister-timer-event registration)
  (deregister-time-event registration))

(define-integrable (threads-pending-timer-events?)
  timer-records)

(define (deregister-all-events)
  (let ((thread (current-thread)))
    (set-interrupt-enables! interrupt-mask/gc-ok)
    (let ((block-events? (thread/block-events? thread)))
      (set-thread/block-events?! thread #t)
      (ring/discard-all (thread/pending-events thread))
      (%deregister-io-thread-events thread)
      (%discard-thread-timer-records thread)
      (%deregister-subprocess-events thread)
      (set-thread/block-events?! thread block-events?))
    (%maybe-toggle-thread-timer)
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
  (if interval
      (guarantee exact-positive-integer? interval 'set-thread-timer-interval!))
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

(define (%maybe-toggle-thread-timer #!optional consider-non-timers?)
  (let ((now (real-time-clock)))
    (let ((start
	   (lambda (time)
	     (set! next-scheduled-timeout time)
	     ((ucode-primitive real-timer-set) (- time now) 0))))
      (cond (timer-records
	     (let ((next-event-time (timer-record/time timer-records)))
	       (if (<= next-event-time now)
		   ;; We're due.  If the timer is already scheduled for
		   ;; the next event time, let it fire when it is ready
		   ;; -- it's a difference of microseconds.  Otherwise,
		   ;; if we're overdue, request a timer interrupt now.
		   (if (or (not next-scheduled-timeout)
			   (< next-event-time next-scheduled-timeout))
		       (begin
			 ;; Don't set the timer to non-positive values.
			 ;; Instead signal the interrupt now.  This is ugly
			 ;; but much simpler than refactoring the scheduler
			 ;; so that we can do the right thing here.
			 (set! next-scheduled-timeout now)
			 ((ucode-primitive request-interrupts! 1)
			  interrupt-bit/timer)))
		   (start
		    (if (and consider-non-timers? timer-interval)
			(min next-event-time (+ now timer-interval))
			next-event-time)))))
	    ((and consider-non-timers?
		  timer-interval
		  (or io-registrations
		      (not (null? subprocess-registrations))
		      (let ((current-thread first-running-thread))
			(and current-thread
			     (thread/next current-thread)))))
	     (start (+ now timer-interval)))
	    (else
	     (%stop-thread-timer))))))

(define (%stop-thread-timer)
  (if next-scheduled-timeout
      (begin
	((ucode-primitive real-timer-clear))
	(set! next-scheduled-timeout #f)
	((ucode-primitive clear-interrupts!) interrupt-bit/timer))))

;;;; Mutexes

;;; A record type cannot be created very early in the cold load, but
;;; creating thread mutexes early is convenient for users of serial
;;; populations, queues, etc.  The following define-structure is
;;; hand-expanded as a tagged vector (not record) in thread-low.scm.

#;(define-structure (thread-mutex
		   (constructor make-thread-mutex ())
		   (conc-name thread-mutex/))
  (waiting-threads (make-ring) read-only #t)
  (owner #f))

(define-print-method thread-mutex?
  (standard-print-method 'thread-mutex))

(define-integrable (guarantee-thread-mutex mutex procedure)
  (if (not (thread-mutex? mutex))
      (error:wrong-type-argument mutex "thread-mutex" procedure)))

(define (assert-thread-mutex-owned mutex #!optional caller)
  (guarantee-thread-mutex mutex 'assert-thread-mutex-owned)
  (if (not (eq? (current-thread) (thread-mutex/owner mutex)))
      (if (default-object? caller)
	  (error "Don't own mutex:" mutex)
	  (error "Don't own mutex:" mutex caller))))

;;; Never use THREAD-MUTEX-OWNER except as a debugging feature.  If you
;;; are tempted to make locking decisions on the basis of who owns a
;;; mutex, fix your design so you're no longer tempted.  Mutexes are
;;; non-recursive.  Use ASSERT-THREAD-MUTEX-OWNED to assert that you
;;; own a mutex so you're less tempted to call THREAD-MUTEX-OWNER ever.

(define (thread-mutex-owner mutex)
  (guarantee-thread-mutex mutex 'thread-mutex-owner)
  (thread-mutex/owner mutex))

(define (lock-thread-mutex mutex)
  (guarantee-thread-mutex mutex 'lock-thread-mutex)
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
  (guarantee-thread-mutex mutex 'unlock-thread-mutex)
  (without-interrupts
   (lambda ()
     (let ((owner (thread-mutex/owner mutex)))
       (if (or (not owner)
	       (not (eq? owner (current-thread))))
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
  (guarantee-thread-mutex mutex 'try-lock-thread-mutex)
  (without-interrupts
   (lambda ()
     (and (not (thread-mutex/owner mutex))
	  (let ((thread first-running-thread))
	    ;; GC daemons may use this when there is no current thread.
	    (and thread
		 (begin
		   (set-thread-mutex/owner! mutex thread)
		   (add-thread-mutex! thread mutex)
		   #t)))))))

(define (with-thread-mutex-lock mutex thunk)
  (guarantee-thread-mutex mutex 'with-thread-mutex-lock)
  (dynamic-wind (lambda () (lock-thread-mutex mutex))
		thunk
		(lambda () (unlock-thread-mutex mutex))))

(define (without-thread-mutex-lock mutex thunk)
  (guarantee-thread-mutex mutex 'with-thread-mutex-lock)
  (dynamic-wind (lambda () (unlock-thread-mutex mutex))
		thunk
		(lambda () (lock-thread-mutex mutex))))

(define (with-thread-mutex-try-lock mutex locked not-locked)
  (guarantee-thread-mutex mutex 'with-thread-mutex-try-lock)
  (let ((locked?))
    (dynamic-wind (lambda ()
		    (set! locked? (try-lock-thread-mutex mutex)))
		  (lambda ()
		    (if locked? (locked) (not-locked)))
		  (lambda ()
		    (if locked? (unlock-thread-mutex mutex))
		    (set! locked?)))))

;;; WITH-THREAD-MUTEX-LOCKED is retained for compatibility for old
;;; programs that require recursion.  For new programs, better to
;;; refactor into clearer invariants that do not require recursion if
;;; possible and use WITH-THREAD-MUTEX-LOCK to help detect lock order
;;; mistakes.

(define (with-thread-mutex-locked mutex thunk)
  (guarantee-thread-mutex mutex 'with-thread-mutex-locked)
  (let ((thread (current-thread))
	(grabbed-lock?))
    (dynamic-wind
     (lambda ()
       (let ((owner (thread-mutex/owner mutex)))
	 (if (eq? owner thread)
	     (set! grabbed-lock? #f)
	     (begin
	       (set! grabbed-lock? #t)
	       (%lock-thread-mutex mutex thread owner)))))
     thunk
     (lambda ()
       (if (and grabbed-lock? (eq? (thread-mutex/owner mutex) thread))
	   (%unlock-thread-mutex mutex thread))))))

(define (with-thread-mutex-unlocked mutex thunk)
  (guarantee-thread-mutex mutex 'with-thread-mutex-unlocked)
  (let ((thread (current-thread))
	(released-lock?))
    (dynamic-wind
     (lambda ()
       (let ((owner (thread-mutex/owner mutex)))
	 (if (not (eq? owner thread))
	     (set! released-lock? #f)
	     (begin
	       (set! released-lock? #t)
	       (%unlock-thread-mutex mutex owner)))))
     thunk
     (lambda ()
       (if released-lock?
	   (let ((owner (thread-mutex/owner mutex)))
	     (if (not (eq? owner thread))
		 (%lock-thread-mutex mutex thread owner))))))))

(define (%disassociate-thread-mutexes thread)
  (do ((mutexes (thread/mutexes thread) (cdr mutexes)))
      ((not (pair? mutexes)))
    (let ((mutex (car mutexes)))
      (if (eq? (thread-mutex/owner mutex) thread)
	  (%%unlock-thread-mutex mutex)
	  (ring/remove-item (thread-mutex/waiting-threads mutex) thread))))
  (set-thread/mutexes! thread '()))

(define-integrable (add-thread-mutex! thread mutex)
  (set-thread/mutexes! thread (cons mutex (thread/mutexes thread))))

(define-integrable (remove-thread-mutex! thread mutex)
  (set-thread/mutexes! thread (delq! mutex (thread/mutexes thread))))

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
	(make-condition-type 'thread-control-error condition-type:control-error
	    '(thread)
	  (lambda (condition port)
	    (write-string "Anonymous error associated with " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string "." port))))
  (set! thread-control-error/thread
	(condition-accessor condition-type:thread-control-error 'thread))

  (set! condition-type:thread-deadlock
	(make-condition-type 'thread-deadlock
	    condition-type:thread-control-error
	    '(description operator operand)
	  (lambda (condition port)
	    (write-string "Deadlock detected while trying to " port)
	    (write-string (thread-deadlock/description condition) port)
	    (write-string ": " port)
	    (write (thread-deadlock/operand condition) port)
	    (write-string "." port))))
  (set! signal-thread-deadlock
	(condition-signaller condition-type:thread-deadlock
			     '(thread description operator operand)
			     standard-error-handler))
  (set! thread-deadlock/description
	(condition-accessor condition-type:thread-deadlock 'description))
  (set! thread-deadlock/operator
	(condition-accessor condition-type:thread-deadlock 'operator))
  (set! thread-deadlock/operand
	(condition-accessor condition-type:thread-deadlock 'operand))

  (set! condition-type:thread-detached
	(make-condition-type 'thread-detached
	    condition-type:thread-control-error
	    '()
	  (lambda (condition port)
	    (write-string "Attempt to join detached thread: " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string "." port))))
  (set! signal-thread-detached
	(condition-signaller condition-type:thread-detached
			     '(thread)
			     standard-error-handler))

  (set! condition-type:thread-dead
	(make-condition-type 'thread-dead condition-type:thread-control-error
	    '(verb operator operands)
	  (lambda (condition port)
	    (write-string "Unable to " port)
	    (write-string (thread-dead/verb condition) port)
	    (write-string " thread " port)
	    (write (thread-control-error/thread condition) port)
	    (write-string " because it is dead." port))))
  (set! signal-thread-dead
	(let ((signaller
	       (condition-signaller condition-type:thread-dead
				    '(thread verb operator operands)
				    standard-error-handler)))
	  (lambda (thread verb operator . operands)
	    (signaller thread verb operator operands))))
  (set! thread-dead/verb
	(condition-accessor condition-type:thread-dead 'verb))

  (set! condition-type:no-current-thread
	(make-condition-type 'no-current-thread condition-type:control-error
	    '()
	  (lambda (condition port)
	    condition
	    (write-string "No current thread!" port))))
  unspecific)