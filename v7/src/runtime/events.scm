#| -*-Scheme-*-

$Id: events.scm,v 14.9 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
		  (let ((receiver (cdr event))
			(receivers
			 (event-distributor/receivers event-distributor)))
		    (if (not (memv receiver receivers))
			(set-event-distributor/receivers!
			 event-distributor
			 (append! receivers (list receiver))))))
		 ((REMOVE-RECEIVER)
		  (set-event-distributor/receivers!
		   event-distributor
		   (delv! (cdr event)
			  (event-distributor/receivers event-distributor))))
		 (else
		  (error "Illegal event" event)))))))
     (lambda ()
       (set-event-distributor/lock! event-distributor old-lock)))))