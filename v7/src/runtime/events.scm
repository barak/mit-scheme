#| -*-Scheme-*-

$Id: events.scm,v 14.4 1999/01/02 06:11:34 cph Exp $

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

;;;; Event Distribution
;;; package: (runtime event-distributor)

(declare (usual-integrations))

(define (initialize-package!)
  (set! add-event-receiver! (make-receiver-modifier 'ADD-RECEIVER))
  (set! remove-event-receiver! (make-receiver-modifier 'REMOVE-RECEIVER))
  unspecific)

(define-structure (event-distributor
		   (constructor make-event-distributor ())
		   (conc-name event-distributor/))
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