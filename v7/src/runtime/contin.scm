#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/contin.scm,v 14.4 1989/08/15 13:19:35 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
  (call/cc (ucode-primitive call-with-current-continuation)
	   'REENTRANT
	   receiver))

(define (non-reentrant-call-with-current-continuation receiver)
  (call/cc (ucode-primitive non-reentrant-call-with-current-continuation)
	   'UNUSED
	   receiver))

(define (call/cc primitive type receiver)
  (primitive
   (lambda (control-point)
     (let ((continuation
	    (make-continuation type
			       control-point
			       (current-dynamic-state)
			       (get-fluid-bindings))))
       (continuation (receiver continuation))))))

(define (%within-continuation continuation thunk)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point continuation)
   (let ((dynamic-state (continuation/dynamic-state continuation))
	 (fluid-bindings (continuation/fluid-bindings continuation)))
     (lambda ()
       (set-fluid-bindings! fluid-bindings)
       (translate-to-state-point dynamic-state)
       (thunk)))))

(define (invocation-method/reentrant continuation value)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point continuation)
   (let ((dynamic-state (continuation/dynamic-state continuation))
	 (fluid-bindings (continuation/fluid-bindings continuation)))
     (lambda ()
       (set-fluid-bindings! fluid-bindings)
       (translate-to-state-point dynamic-state)
       value))))

;; These two are correctly locked for multiprocessing, but not for
;; multiprocessors.

(define (within-continuation continuation thunk)
  (guarantee-continuation continuation)
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
      (%within-continuation continuation thunk)
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

(define (make-continuation type control-point dynamic-state fluid-bindings)
  (make-entity
   (case type
     ((REENTRANT) invocation-method/reentrant)
     ((UNUSED) invocation-method/unused)
     ((USED) invocation-method/used)
     (else (error "Illegal continuation type" type)))
   (make-%continuation control-point dynamic-state fluid-bindings)))

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
      (error "Illegal continuation" continuation))
  continuation)

(define-integrable (continuation/invocation-method continuation)
  (entity-procedure continuation))

(define-integrable (set-continuation/invocation-method! continuation method)
  (set-entity-procedure! continuation method))

(define-integrable (continuation/control-point continuation)
  (%continuation/control-point (entity-extra continuation)))

(define-integrable (continuation/dynamic-state continuation)
  (%continuation/dynamic-state (entity-extra continuation)))

(define-integrable (continuation/fluid-bindings continuation)
  (%continuation/fluid-bindings (entity-extra continuation)))

(define-structure (%continuation (constructor make-%continuation)
				 (conc-name %continuation/))
  (control-point false read-only true)
  (dynamic-state false read-only true)
  (fluid-bindings false read-only true))