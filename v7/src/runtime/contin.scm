#| -*-Scheme-*-

$Id: contin.scm,v 14.7 1992/11/25 06:38:46 gjr Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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

;;;; Continuations
;;; package: (runtime continuation)

(declare (usual-integrations))

(define (call-with-current-continuation receiver)
  (call/cc (ucode-primitive call-with-current-continuation 1)
	   'REENTRANT
	   receiver))

;; The following is not properly tail recursive because it builds the
;; extra frame that invokes cont on the result.
;; This is done to guarantee that the continuation is still valid,
;; since the continuation invocation code is the code that maintains
;; this state.  Note that any other way of verifying this information
;; would also add a continuation frame to the stack!

(define (non-reentrant-call-with-current-continuation receiver)
  (call/cc (ucode-primitive non-reentrant-call-with-current-continuation 1)
	   'UNUSED
	   (lambda (cont)
	     (cont (receiver cont)))))

(define (call/cc primitive type receiver)
  (primitive
   (lambda (control-point)
     (let ((continuation
	    (make-continuation type control-point (get-dynamic-state))))
       (%%within-continuation
	continuation
	(lambda ()
	  (receiver continuation)))))))

(define-integrable (%%within-continuation continuation thunk)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point continuation)
   thunk))

(define (%within-continuation continuation thread-switch? thunk)
  (%%within-continuation
   continuation
   (let ((dynamic-state (continuation/dynamic-state continuation)))
     (lambda ()
       (set-dynamic-state! dynamic-state thread-switch?)
       (thunk)))))

(define (invocation-method/reentrant continuation value)
  (%%within-continuation
   continuation
   (let ((dynamic-state (continuation/dynamic-state continuation)))
     (lambda ()
       (set-dynamic-state! dynamic-state false)
       value))))

;; These two are correctly locked for multiprocessing, but not for
;; multiprocessors.

(define (within-continuation continuation thunk)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation "continuation"
				 'WITHIN-CONTINUATION))
  (if (without-interrupts
       (lambda ()
	 (let ((method (continuation/invocation-method continuation)))
	   (if (eq? method invocation-method/reentrant)
	       true
	       (and (eq? method invocation-method/unused)
		    (begin
		      (set-continuation/invocation-method!
		       continuation
		       invocation-method/used)
		      true))))))
      (%within-continuation continuation false thunk)
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

(define (make-continuation type control-point dynamic-state)
  (make-entity
   (case type
     ((REENTRANT) invocation-method/reentrant)
     ((UNUSED) invocation-method/unused)
     ((USED) invocation-method/used)
     (else (error "Illegal continuation type" type)))
   (make-%continuation control-point dynamic-state)))

(define (continuation/type continuation)
  (let ((invocation-method (continuation/invocation-method continuation)))
    (cond ((eq? invocation-method invocation-method/reentrant) 'REENTRANT)
	  ((eq? invocation-method invocation-method/unused) 'UNUSED)
	  ((eq? invocation-method invocation-method/used) 'USED)
	  (else (error "Illegal invocation-method" invocation-method)))))

(define (continuation? object)
  (and (entity? object)
       (if (%continuation? (entity-extra object))
	   true
	   (continuation? (entity-procedure object)))))

(define (guarantee-continuation continuation)
  (if (not (continuation? continuation))
      (error:wrong-type-argument continuation "continuation" false))
  continuation)

(define-integrable (continuation/invocation-method continuation)
  (entity-procedure continuation))

(define-integrable (set-continuation/invocation-method! continuation method)
  (set-entity-procedure! continuation method))

(define-integrable (continuation/control-point continuation)
  (%continuation/control-point (entity-extra continuation)))

(define-integrable (continuation/dynamic-state continuation)
  (%continuation/dynamic-state (entity-extra continuation)))

(define-structure (%continuation (constructor make-%continuation)
				 (conc-name %continuation/))
  (control-point false read-only true)
  (dynamic-state false read-only true))