#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Continuation Parser
;;; package: (runtime new-continuation-parser)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables) '(runtime history))

(define-record-type <pstate>
    make-pstate
    pstate?
  (cpoint-frames pstate-cpoint-frames)
  (item-bindings pstate-item-bindings))

(define (continuation->stack-frame* continuation)
  (parse-one-frame
   (make-pstate
    (control-point-frames (continuation/control-point continuation))
    (initial-item-bindings
     'dynamic-state (continuation/dynamic-state continuation)
     'block-thread-events? (continuation/block-thread-events? continuation)))))

(define (parse-one-frame pstate)
  (let ((cpoints (pstate-cpoint-frames pstate)))
    (and (pair? cpoints)
	 (make-stack-frame
	  pstate
	  (delay
	    (parse-one-frame
	     (make-pstate (cdr cpoints)
			  (update-item-bindings (pstate-item-bindings pstate)
						(car cpoints)))))))))

(define (pstate-cpoint-frame pstate)
  (car (pstate-cpoint-frames pstate)))

(define (pstate-item-ref pstate name)
  (item-bindings-ref (pstate-item-bindings pstate) name))

;;;; Tracked items

(define (define-item name initial-value updater)
  (let ((entry (vector name initial-value updater))
	(tail
	 (find-tail (lambda (entry)
		      (eq? (vector-ref entry 0) name))
		    defined-items)))
    (if tail
	(set-car! tail entry)
	(set! defined-items (cons entry defined-items)))))

(define defined-items
  '())

(define (initial-item-bindings . inits)
  (map (lambda (entry)
	 (cons entry
	       (let* ((name (vector-ref entry 0))
		      (value (get-keyword-value inits name)))
		 (if (default-object? value)
		     (vector-ref entry 1)
		     value))))
       defined-items))

(define (item-bindings-ref bindings name)
  (let ((p
	 (find (lambda (binding)
		 (eq? (vector-ref (car binding) 0) name))
	       bindings)))
    (if (not p)
	(error "Unknown item name:" name))
    (cdr p)))

(define (update-item-bindings bindings cpoint)
  (map (lambda (binding)
	 (cons (car binding)
	       ((vector-ref (car binding) 2) (cdr binding) cpoint)))
       bindings))

(define (simple-item-updater filter)
  (lambda (value cpoint)
    (let ((keyword (filter cpoint)))
      (if keyword
	  (cpoint-frame-field-value cpoint keyword)
	  value))))

(define (stack-marker-type-filter marker-type)
  (lambda (cpoint)
    (and (cpoint-frame:stack-marker-of-type? marker-type cpoint)
	 'marker-instance)))

(define-item 'previous-type #f
  (lambda (value cpoint)
    (if (cpoint-frame:join-stacklets? cpoint)
	value
	(cpoint-frame-type cpoint))))

(define-item 'dynamic-state #f
  (simple-item-updater (stack-marker-type-filter %translate-to-state-point)))

(define-item 'block-thread-events? #f
  (simple-item-updater (stack-marker-type-filter 'with-thread-events-blocked)))

(define-item 'interrupt-mask #f
  (simple-item-updater
   (lambda (cpoint)
     (cond ((cpoint-frame:restore-interrupt-mask? cpoint)
	    'interrupt-mask)
	   ((cpoint-frame:stack-marker-of-type? 'set-interrupt-enables! cpoint)
	    'marker-instance)
	   (else #f)))))

(define-item 'history #f
  (lambda (value cpoint)
    (if (cpoint-frame:restore-history? cpoint)
	(history-transform (cpoint-frame-field-value cpoint 'history))
	value)))

(define-item 'next-restore-history 0
  (lambda (value cpoint)
    (if (cpoint-frame:restore-history? cpoint)
	(begin
	  (assert (or (fix:= value 0)
		      (fix:= value (cpoint-frame-start cpoint))))
	  (let ((index (cpoint-frame-next-restore-history cpoint)))
	    (assert (or (fix:= index 0)
			(fix:>= index (cpoint-frame-end cpoint))))
	    index))
	(begin
	  (assert (or (fix:= value 0)
		      (fix:>= value (cpoint-frame-end cpoint))))
	  value))))

(define (cpoint-frame-next-restore-history cpoint)
  (let ((offset (cpoint-frame-field-value cpoint 'previous-restore-history-offset)))
    (if (fix:= offset 0)
	0
	(fix:- (cpoint-frame-cpoint-end cpoint) offset))))

(define (pstate-previous-restore-history-offset pstate)
  (let ((index (pstate-item-ref pstate 'next-restore-history)))
    (if (fix:= index 0)
	0
	(fix:- (cpoint-frame-cpoint-end (pstate-cpoint-frame pstate)) index))))

(define-item 'next-return-code #f
  (lambda (value cpoint)
    (if (cpoint-frame:compiled-code? cpoint)
	(begin
	  (assert (and value (fix:>= value (cpoint-frame-end cpoint))))
	  value)
	(begin
	  (assert (or (not value) (fix:= value (cpoint-frame-start cpoint))))
	  (if (cpoint-frame:return-to-compiled-code? cpoint)
	      (let ((index (cpoint-frame-field-value cpoint 'last-return-code)))
		;; Check that index is in appropriate range.
		(assert (fix:> index 0))
		(assert (fix:< index (cpoint-frame-cpoint-end cpoint)))
		(assert (fix:>= index (cpoint-frame-end cpoint)))
		index)
	      #f)))))

;;;; Stack-frame abstraction

(define-record-type <stack-frame>
    make-stack-frame
    stack-frame*?
  (pstate stack-frame-pstate)
  (%next stack-frame-%next))

(define (stack-frame-cpoint frame)
  (pstate-cpoint-frame (stack-frame-pstate frame)))

(define (stack-frame*/next frame)
  (force (stack-frame-%next frame)))

(define (stack-frame*->continuation frame)
  (let ((pstate (stack-frame-pstate frame)))
    (make-continuation
     (old-control-point (pstate-cpoint-frames pstate)
			(pstate-item-ref pstate 'interrupt-mask)
			(pstate-item-ref pstate 'history)
			(pstate-previous-restore-history-offset pstate))
     (pstate-item-ref pstate 'dynamic-state)
     (pstate-item-ref pstate 'block-thread-events?))))

(define (stack-frame*/block-thread-events? frame)
  (pstate-item-ref (stack-frame-pstate frame) 'block-thread-events?))

(define (stack-frame*/compiled-return-address? frame)
  (cpoint-frame:compiled-address? (stack-frame-cpoint frame)))

(define (stack-frame*/compiled-code? frame)
  (cpoint-frame:compiled-code? (stack-frame-cpoint frame)))

(define (stack-frame*/dynamic-state frame)
  (pstate-item-ref (stack-frame-pstate frame) 'dynamic-state))

(define (stack-frame*/elements frame)
  (cpoint-frame-raw (stack-frame-cpoint frame)))

(define (stack-frame*/length frame)
  (cpoint-frame-length (stack-frame-cpoint frame)))

(define (stack-frame*/previous-type frame)
  (pstate-item-ref (stack-frame-pstate frame) 'previous-type))

(define (stack-frame*/reductions frame)
  (let ((history (pstate-item-ref (stack-frame-pstate frame) 'history)))
    (if (eq? history undefined-history)
	'()
	(history-reductions history))))

(define undefined-history
  (list 'undefined-history))

(define (stack-frame*/ref frame index)
  (guarantee non-negative-fixnum? index 'stack-frame*/ref)
  (let loop ((frame frame) (i index))
    (let ((n (stack-frame*/length frame)))
      (if (fix:< i n)
	  (cpoint-frame-ref (stack-frame-cpoint frame) i)
	  (let ((frame* (stack-frame*/next frame)))
	    (if (not frame*)
		(error:bad-range-argument i 'stack-frame*/ref))
	    (loop frame (fix:- i n)))))))

(define (stack-frame*/repl-eval-boundary? frame)
  (cpoint-frame:repl-eval-boundary? (stack-frame-cpoint frame)))

(define (stack-frame*/resolve-stack-address frame address)
  (let* ((offset (stack-address-offset address))
	 (index
	  (fix:- (let ((cpoint (stack-frame-cpoint frame)))
		   (fix:- (cpoint-frame-cpoint-end cpoint)
			  (cpoint-frame-start cpoint)))
		 offset)))
    (assert (fix:>= index 0))
    (let loop ((frame frame) (index index))
      (let ((length (stack-frame/length frame)))
	(if (fix:< index length)
	    (values frame index)
	    (loop (stack-frame/next frame) (fix:- index length)))))))

(define (stack-frame*/return-address frame)
  (cpoint-frame-return-address (stack-frame-cpoint frame)))

(define (stack-frame*/return-code frame)
  (cpoint-frame-return-code (stack-frame-cpoint frame)))

(define (stack-frame*/next-subproblem frame)
  (if (stack-frame*/subproblem? frame)
      (let ((frame* (stack-frame*/next frame)))
	(and frame*
	     (stack-frame*/skip-non-subproblems frame*)))
      (stack-frame*/skip-non-subproblems frame)))

(define (stack-frame*/skip-non-subproblems frame)
  (if (stack-frame*/subproblem? frame)
      frame
      (let ((frame* (stack-frame*/next frame)))
	(and frame*
	     (stack-frame*/skip-non-subproblems frame*)))))

(define (stack-frame*/subproblem? frame)
  (let ((cpoint (stack-frame-cpoint frame)))
    (or (cpoint-frame:subproblem? cpoint)
	(cpoint-frame:repl-eval-boundary? cpoint))))

(define (stack-frame*/hardware-trap? frame)
  (cpoint-frame:hardware-trap? (stack-frame-cpoint frame)))
(register-predicate! stack-frame*/hardware-trap? 'stack-frame*/hardware-trap
		     '<= stack-frame?)

(define (stack-frame*/hardware-trap-code frame)
  (guarantee stack-frame*/hardware-trap? frame 'stack-frame*/hardware-trap-code)
  (cdr (cpoint-frame-field-value (stack-frame-cpoint frame) 'code-name)))

;; debugging-info/compiled-code?
;; debugging-info/undefined-environment?
;; debugging-info/undefined-expression?
;; debugging-info/unknown-expression?
;; debugging-info/noise
;; debugging-info/noise?
;; stack-frame*/debugging-info