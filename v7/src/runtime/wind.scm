#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/wind.scm,v 14.2 1988/06/22 21:24:34 cph Exp $

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

;;;; State Space Model
;;; package: (runtime state-space)

(declare (usual-integrations))

(define (initialize-package!)
  (let ((fixed-objects (get-fixed-objects-vector))
	(state-space-tag "State Space")
	(state-point-tag "State Point"))
    (unparser/set-tagged-vector-method!
     state-space-tag
     (unparser/standard-method 'STATE-SPACE))
    (unparser/set-tagged-vector-method!
     state-point-tag
     (unparser/standard-method 'STATE-POINT))
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'STATE-SPACE-TAG)
		 state-space-tag)
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'STATE-POINT-TAG)
		 state-point-tag)
    (set! system-state-space (make-state-space false))
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'STATE-SPACE-ROOT)
		 (current-dynamic-state))
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define-structure (state-point (type vector)
			       (initial-offset 1)
			       (constructor false)
			       (conc-name state-point/))
  (before-thunk false read-only true)
  (after-thunk false read-only true)
  (nearer-point false read-only true)
  (distance-to-root false read-only true))

(define (state-point/space point)
  (let ((next (state-point/nearer-point point)))
    (if (positive? (state-point/distance-to-root point))
	(state-point/space next)
	next)))

(define-primitives
  execute-at-new-state-point
  translate-to-state-point
  set-current-dynamic-state!
  (get-fluid-bindings 0) 
  (set-fluid-bindings! 1))

(define (make-state-space #!optional mutable?)
  ((ucode-primitive make-state-space)
   (if (default-object? mutable?) true mutable?)))

(define system-state-space)

(define (current-dynamic-state #!optional state-space)
  ((ucode-primitive current-dynamic-state)
   (if (default-object? state-space) system-state-space state-space)))

;;; NOTE: the "before" thunk is executed IN THE NEW STATE, the "after"
;;; thunk is executed IN THE OLD STATE.  Your programs should not
;;; depend on this if it can be avoided.
(define (dynamic-wind before during after)
  (execute-at-new-state-point system-state-space before during after))