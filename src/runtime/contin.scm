#| -*-Scheme-*-

$Id: contin.scm,v 14.12 2003/02/14 18:28:32 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Continuations
;;; package: (runtime continuation)

(declare (usual-integrations))

(define (call-with-current-continuation receiver)
  (call/cc (ucode-primitive call-with-current-continuation 1)
	   'REENTRANT
	   receiver))

;;; The following is not properly tail recursive because it builds the
;;; extra frame that invokes cont on the result.  This is done to
;;; guarantee that the continuation is still valid, since the
;;; continuation invocation code is the code that maintains this
;;; state.  Note that any other way of verifying this information
;;; would also add a continuation frame to the stack!

(define (non-reentrant-call-with-current-continuation receiver)
  (call/cc (ucode-primitive non-reentrant-call-with-current-continuation 1)
	   'UNUSED
	   (lambda (cont) (cont (receiver cont)))))

(define (call/cc primitive type receiver)
  (primitive
   (lambda (control-point)
     (let ((continuation
	    (make-continuation type
			       control-point
			       (get-dynamic-state)
			       (get-thread-event-block))))
       (%%within-continuation
	continuation
	(lambda () (receiver continuation)))))))

(define-integrable (%%within-continuation continuation thunk)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point continuation)
   thunk))

(define (%within-continuation continuation thread-switch? thunk)
  (%%within-continuation
   continuation
   (let ((restore-state (state-restoration-procedure continuation)))
     (lambda ()
       (restore-state thread-switch?)
       (thunk)))))

(define (invocation-method/reentrant continuation value)
  (%%within-continuation
   continuation
   (let ((restore-state (state-restoration-procedure continuation)))
     (lambda ()
       (restore-state #f)
       value))))

(define (state-restoration-procedure continuation)
  (let ((dynamic-state (continuation/dynamic-state continuation))
	(block-thread-events?
	 (continuation/block-thread-events? continuation)))
    (lambda (thread-switch?)
      (set-dynamic-state! dynamic-state thread-switch?)
      (set-thread-event-block! block-thread-events?))))

;;; These two are correctly locked for multiprocessing, but not for
;;; multiprocessors.

(define (within-continuation continuation thunk)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation "continuation"
				 'WITHIN-CONTINUATION))
  (if (without-interrupts
       (lambda ()
	 (let ((method (continuation/invocation-method continuation)))
	   (if (eq? method invocation-method/reentrant)
	       #t
	       (and (eq? method invocation-method/unused)
		    (begin
		      (set-continuation/invocation-method!
		       continuation
		       invocation-method/used)
		      #t))))))
      (%within-continuation continuation #f thunk)
      (error "Reentering used continuation" continuation)))

(define (invocation-method/unused continuation value)
  (if (eq? (without-interrupts
	    (lambda ()
	      (let ((method (continuation/invocation-method continuation)))
		(set-continuation/invocation-method! continuation
						     invocation-method/used)
		method)))
	   invocation-method/unused)
      (invocation-method/reentrant continuation value)
      (invocation-method/used continuation value)))

(define (invocation-method/used continuation value)
  value
  (error "Reentering used continuation" continuation))

(define (make-continuation type control-point dynamic-state
			   block-thread-events?)
  (make-entity
   (case type
     ((REENTRANT) invocation-method/reentrant)
     ((UNUSED) invocation-method/unused)
     ((USED) invocation-method/used)
     (else (error "Illegal continuation type" type)))
   (make-%continuation control-point dynamic-state block-thread-events?)))

(define (continuation/type continuation)
  (let ((invocation-method (continuation/invocation-method continuation)))
    (cond ((eq? invocation-method invocation-method/reentrant) 'REENTRANT)
	  ((eq? invocation-method invocation-method/unused) 'UNUSED)
	  ((eq? invocation-method invocation-method/used) 'USED)
	  (else (error "Illegal invocation-method" invocation-method)))))

(define (continuation? object)
  (and (entity? object)
       (if (%continuation? (entity-extra object))
	   #t
	   (continuation? (entity-procedure object)))))

(define (guarantee-continuation continuation)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation "continuation" #f))
  continuation)

(define-integrable (continuation/invocation-method continuation)
  (entity-procedure continuation))

(define-integrable (set-continuation/invocation-method! continuation method)
  (set-entity-procedure! continuation method))

(define-integrable (continuation/control-point continuation)
  (%continuation/control-point (entity-extra continuation)))

(define-integrable (continuation/dynamic-state continuation)
  (%continuation/dynamic-state (entity-extra continuation)))

(define-integrable (continuation/block-thread-events? continuation)
  (%continuation/block-thread-events? (entity-extra continuation)))

(define-structure (%continuation (constructor make-%continuation)
				 (conc-name %continuation/))
  (control-point #f read-only #t)
  (dynamic-state #f read-only #t)
  (block-thread-events? #f read-only #t))