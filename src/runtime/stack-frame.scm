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

;;;; Stack-frame abstraction
;;; package: (runtime stack-frame)

(declare (usual-integrations))

(define-record-type <stack-frame>
    make-stack-frame
    stack-frame*?
  (cpoint stack-frame-cpoint)
  (pstate stack-frame-pstate)
  (%next stack-frame-%next))

(define (stack-frame*->continuation frame)
  (declare (ignore frame))
  (error "Unimplemented."))

(define (stack-frame*/block-thread-events? frame)
  (pstate-block-thread-events? (stack-frame-pstate frame)))

(define (stack-frame*/compiled-return-address? frame)
  (cpoint-frame-compiled-address? (stack-frame-cpoint frame)))

(define (stack-frame*/compiled-code? frame)
  (cpoint-frame-compiled-code? (stack-frame-cpoint frame)))

(define (stack-frame*/dynamic-state frame)
  (pstate-dynamic-state (stack-frame-pstate frame)))

(define (stack-frame*/elements frame)
  (cpoint-frame-raw (stack-frame-cpoint frame)))

(define (stack-frame*/length frame)
  (cpoint-frame-length (stack-frame-cpoint frame)))

(define (stack-frame*/next frame)
  (force (stack-frame-%next frame)))

(define (stack-frame*/next-subproblem frame)
  (if (stack-frame*/subproblem? frame)
      (let ((frame* (stack-frame*/next frame)))
	(and frame*
	     (stack-frame*/skip-non-subproblems frame*)))
      (stack-frame*/skip-non-subproblems frame)))

(define (stack-frame*/previous-type frame)
  (pstate-previous-type (stack-frame-pstate frame)))

(define (stack-frame*/reductions frame)
  (let ((history (pstate-history (stack-frame-pstate frame))))
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
	  (cpoint-frame-elt (stack-frame-cpoint frame) i)
	  (let ((frame* (stack-frame*/next frame)))
	    (if (not frame*)
		(error:bad-range-argument i 'stack-frame*/ref))
	    (loop frame (fix:- i n)))))))

(define (stack-frame*/repl-eval-boundary? frame)
  (cpoint-frame-repl-eval-boundary? (stack-frame-cpoint frame)))

;;; The old parser kept track of the "offset" of the current stack frame by
;;; counting the distance between the start of that frame and the end of the
;;; control point that it was in.  It used the result of the
;;; stack-address-offset primitive and the tracked offset to create an index
;;; which could then be used to identify the frame that the offset point into.
(define (stack-frame*/resolve-stack-address frame address)
  (declare (ignore frame address))
  (error "Unimplemented."))

(define (stack-frame*/return-address frame)
  (cpoint-frame-return-address (stack-frame-cpoint frame)))

(define (stack-frame*/return-code frame)
  (cpoint-frame-return-code (stack-frame-cpoint frame)))

;;; The conpar logic for this is a little complex.
(define (stack-frame*/skip-non-subproblems frame)
  (declare (ignore frame))
  (error "Unimplemented."))

(define (stack-frame*/subproblem? frame)
  (let ((cpoint (stack-frame-cpoint frame)))
    (or (cpoint-frame-subproblem? cpoint)
	(cpoint-frame-repl-eval-boundary? cpoint))))

(define (stack-frame*/hardware-trap? frame)
  (cpoint-frame-hardware-trap? (stack-frame-cpoint frame)))
(register-predicate! stack-frame*/hardware-trap? 'stack-frame*/hardware-trap
		     '<= stack-frame?)

(define (stack-frame*/hardware-trap-code frame)
  (guarantee hardware-trap-frame? frame 'stack-frame*/hardware-trap-code)
  (cpoint-frame-hardware-trap-code (stack-frame-cpoint frame)))

;; debugging-info/compiled-code?
;; debugging-info/undefined-environment?
;; debugging-info/undefined-expression?
;; debugging-info/unknown-expression?
;; debugging-info/noise
;; debugging-info/noise?
;; stack-frame*/debugging-info
