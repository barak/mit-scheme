#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/events.scm,v 14.2 1991/04/25 14:40:13 markf Exp $

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

;;;; Event Distribution
;;; package: (runtime event-distributor)

(declare (usual-integrations))

(define (initialize-package!)
  (set! add-event-receiver! (make-receiver-modifier 'ADD-RECEIVER))
  (set! remove-event-receiver! (make-receiver-modifier 'REMOVE-RECEIVER)))

(define (initialize-unparser!)
  (unparser/set-tagged-vector-method!
   event-distributor
   (unparser/standard-method 'EVENT-DISTRIBUTOR)))

(define-structure (event-distributor
		   (constructor make-event-distributor ())
		   (conc-name event-distributor/)
		   (print-procedure false))
  (events (make-queue))
  (lock false)
  (receivers '()))

(define (event-distributor/invoke! event-distributor . arguments)
  (enqueue! (event-distributor/events event-distributor)
	    (cons 'INVOKE-RECEIVERS arguments))
  (process-events! event-distributor))

(define (make-receiver-modifier keyword)
  (lambda (event-distributor receiver)
    (if (not (event-distributor? event-distributor))
	(error "Not an event distributor" event-distributor))
    (enqueue! (event-distributor/events event-distributor)
	      (cons keyword receiver))
    (process-events! event-distributor)))

(define add-event-receiver!)
(define remove-event-receiver!)

(define (process-events! event-distributor)
  (let ((old-lock))
    (dynamic-wind
     (lambda ()
       (let ((lock (event-distributor/lock event-distributor)))
	 (set-event-distributor/lock! event-distributor true)
	 (set! old-lock lock)
	 unspecific))
     (lambda ()
       (if (not old-lock)
	   (queue-map! (event-distributor/events event-distributor)
	     (lambda (event)
	       (case (car event)
		 ((INVOKE-RECEIVERS)
		  (do ((receivers
			(event-distributor/receivers event-distributor)
			(cdr receivers)))
		      ((null? receivers))
		    (apply (car receivers) (cdr event))))
		 ((ADD-RECEIVER)
		  (set-event-distributor/receivers!
		   event-distributor
		   (append! (event-distributor/receivers event-distributor)
			    (list (cdr event)))))
		 ((REMOVE-RECEIVER)
		  (set-event-distributor/receivers!
		   event-distributor
		   (delv! (cdr event)
			  (event-distributor/receivers event-distributor))))
		 (else
		  (error "Illegal event" event)))))))
     (lambda ()
       (set-event-distributor/lock! event-distributor old-lock)))))