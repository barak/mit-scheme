;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/wind.scm,v 13.41 1987/01/23 00:22:29 jinx Exp $
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; State Space Model

(declare (usual-integrations)
	 (compilable-primitive-functions
	  set-fixed-objects-vector!))

(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'STATE-SPACE-TAG)
	     "State Space")

(vector-set! (get-fixed-objects-vector)
	     (fixed-objects-vector-slot 'STATE-POINT-TAG)
	     "State Point")

(set-fixed-objects-vector! (get-fixed-objects-vector))

(define make-state-space
  (let ((prim (make-primitive-procedure 'MAKE-STATE-SPACE)))
    (named-lambda (make-state-space #!optional mutable?)
      (if (unassigned? mutable?) (set! mutable? #!true))
      (prim mutable?))))

(define execute-at-new-state-point
  (make-primitive-procedure 'EXECUTE-AT-NEW-STATE-POINT))

(define translate-to-state-point
  (make-primitive-procedure 'TRANSLATE-TO-STATE-POINT))

;;; The following code implements the current model of DYNAMIC-WIND as
;;; a special case of the more general concept.

(define system-state-space
  (make-state-space #!false))

(define current-dynamic-state
  (let ((prim (make-primitive-procedure 'current-dynamic-state)))
    (named-lambda (current-dynamic-state #!optional state-space)
      (prim (if (unassigned? state-space)
		system-state-space
		state-space)))))

(define set-current-dynamic-state!
  (make-primitive-procedure 'set-current-dynamic-state!))

;; NOTICE that the "before" thunk is executed IN THE NEW STATE,
;; the "after" thunk is executed IN THE OLD STATE.  It is hard to
;; imagine why anyone would care about this.

(define (dynamic-wind before during after)
  (execute-at-new-state-point system-state-space
			      before
			      during
			      after))

;; This is so the microcode can find the base state point.

(let ((fov (get-fixed-objects-vector)))
  (vector-set! fov 
	       (fixed-objects-vector-slot 'STATE-SPACE-ROOT)
	       (current-dynamic-state))
  (set-fixed-objects-vector! fov))


  (set-fixed-objects-vector! fov))