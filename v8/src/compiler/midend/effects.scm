#| -*-Scheme-*-

$Id: effects.scm,v 1.1 1995/09/05 19:08:51 adams Exp $

Copyright (c) 1995-1995 Massachusetts Institute of Technology

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