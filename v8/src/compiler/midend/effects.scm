#| -*-Scheme-*-

$Id: 24b811f71a071fcd5ad67d5e524e66f473ebe1a1 $

Copyright (c) 1995-1999 Massachusetts Institute of Technology

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

;;;; Effect:
;;; package: (compiler midend)

(declare (usual-integrations))

;;;; Effects
;;
;; An `Effect' is a set of abstract mutable locations or structures.
;;
;; The imperative behaviour of a procedure may be described as two
;; Effects: the set of locations which it might change and the set of
;; locations on which the behaviour depends (i.e. observes).  The
;; relative order of evaluation of two expressions is important only
;; if the observed effects of one of the expressions is not disjoint
;; with the performed effects of the other.

;; We represent Effect sets by bit vectors implemented as small fixnums

(define effect:none 0)

(define (effect:none? effect)
  (fix:zero? effect))

(define (effect:union e1 e2)        (fix:or e1 e2))
(define (effect:intersection e1 e2) (fix:and e1 e2))
(define (effect:except e1 e2)       (fix:andc e1 e2))

(define (effect:subset? e1 e2)
  (fix:= (fix:or e1 e2) e2))

(define (effect:disjoint? e1 e2)
  (effect:none? (effect:intersection e1 e2)))

(define (effect:same? e1 e2) (fix:= e1 e2))

(define (effect:union* . e*) (reduce effect:union effect:none e*))
		
;; The specific effects we care about:

(define effect:set-car! 1)
(define effect:set-cdr! 2)
(define effect:vector-set! 4)
(define effect:string-set! 8)
(define effect:%record-set! 16)
(define effect:flo:vector-set! 32)
(define effect:variable-cache 64)	; i.e. global variables
(define effect:allocation 128)
(define effect:other 256)		; anything else, including I/O

(define effect:unknown (- (* 2 effect:other) 1))

;; Aliases

(define effect:bit-string-set! effect:other)

#|
(define (effect:form->lookup form)
  ;; Returns a function from forms to the effects they embody.  Does not
  ;; look into a LAMBDA unless it is called directly.

  (let ((table (make-monotonic-strong-eq-hash-table)))

    (define (lookup-effect form)
      (monotonic-strong-eq-hash-table/get table form effect:none))

    (define (walk form)
      (define (remember effect)
	(if (not (effect:none? effect))
	    (monotonic-strong-eq-hash-table/put! table form effect))
	effect)
      (if (not (pair? form))
	  (illegal form))
      (case (car form)
	((QUOTE)    effect:none)
	((LOOKUP)   effect:none)
	((DECLARE)  effect:none)
	((LAMBDA)   effect:none)
	((LET)      (remember (walk-let form)))
	((CALL)     (remember (walk-call form)))
	((BEGIN)    (remember (walk-begin form)))
	((IF)       (remember (walk-if form)))
	((LETREC)   (remember (walk-letrec form)))
	(else
	 (illegal form))))

    (define (walk* forms)
      (reduce effect:union effect:none (map walk forms)))

    (define (walk-call form)
      (let ((operator  (call/operator form))
	    (pre-call-effects (walk* (cdr form))))
	(cond ((LAMBDA/? operator)
	       (effect:union pre-call-effects (walk (lambda/body operator))))
	      ((QUOTE/? operator)
	       (let ((operator (quote/text operator)))
		 ;; This needs the operator effects to be classifed.
		 (if (operator-satisfies? operator '(SIDE-EFFECT-FREE))
		     pre-call-effects
		     effect:unknown)))
	      (else  effect:unknown))))

    (define (walk-if form)
      (walk* (cdr form)))

    (define (walk-begin form)
      (walk* (cdr form)))

    (define (walk-let form)
      (effect:union (walk* (map second (let/bindings form)))
		    (walk  (let/body form))))
	
    (define (walk-letrec form)
      (walk  (let/body form)))
	
    lookup-effect))
|#