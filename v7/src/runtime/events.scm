;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/events.scm,v 13.42 1987/03/17 18:49:40 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Event Distribution

(declare (usual-integrations))

(define make-event-distributor)
(define event-distributor?)
(define add-event-receiver!)
(define remove-event-receiver!)

(let ((:type (make-named-tag "EVENT-DISTRIBUTOR")))
  (set! make-event-distributor
	(named-lambda (make-event-distributor)
	  (define receivers '())
	  (define queue-head '())
	  (define queue-tail '())
	  (define event-in-progress? false)
	  (lambda arguments
	    (if (null? queue-head)
		(begin (set! queue-head (list arguments))
		       (set! queue-tail queue-head))
		(begin (set-cdr! queue-tail (list arguments))
		       (set! queue-tail (cdr queue-tail))))
	    (if (not (set! event-in-progress? true))
		(begin (let ((arguments (car queue-head)))
			 (set! queue-head (cdr queue-head))
			 (let loop ((receivers receivers))
			      (if (not (null? receivers))
				  (begin (apply (car receivers) arguments)
					 (loop (cdr receivers))))))
		       (set! event-in-progress? false))))))

  (set! event-distributor?
	(named-lambda (event-distributor? object)
	  (and (compound-procedure? object)
	       (let ((e (procedure-environment object)))
		 (and (not (lexical-unreferenceable? e ':TYPE))
		      (eq? (access :type e) :type)
		      e)))))

  (define ((make-receiver-modifier name operation)
	   event-distributor event-receiver)
    (let ((e (event-distributor? event-distributor)))
      (if (not e)
	  (error "Not an event distributor" name event-distributor))
      (without-interrupts
       (lambda ()
	 (set! (access receivers e)
	       (operation event-receiver (access receivers e)))))))

  (set! add-event-receiver!
	(make-receiver-modifier 'ADD-EVENT-RECEIVER!
	  (lambda (receiver receivers)
	    (append! receivers (list receiver)))))

  (set! remove-event-receiver!
	(make-receiver-modifier 'REMOVE-EVENT-RECEIVER! delq!))

)