;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/sdata.scm,v 13.42 1987/04/03 00:52:12 jinx Exp $
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

;;;; Abstract Data Field

(declare (usual-integrations))

(define unbound-object?)
(define make-unbound-object)

(define unassigned-object?)
(define make-unassigned-object)

(define &typed-singleton-cons)
(define &singleton-element)
(define &singleton-set-element!)

(define &typed-pair-cons)
(define &pair-car)
(define &pair-set-car!)
(define &pair-cdr)
(define &pair-set-cdr!)

(define &typed-triple-cons)
(define &triple-first)
(define &triple-set-first!)
(define &triple-second)
(define &triple-set-second!)
(define &triple-third)
(define &triple-set-third!)

(define &typed-vector-cons)
(define &list-to-vector)
(define &vector-size)
(define &vector-ref)
(define &vector-to-list)
(define &subvector-to-list)

(let ((&unbound-object '(&UNBOUND-OBJECT))
      (&unbound-datum 2)
      (&unassigned-object '(&UNASSIGNED-OBJECT))
      (&unassigned-datum 0)
      (&unassigned-type (microcode-type 'UNASSIGNED))
      (&make-object (make-primitive-procedure '&MAKE-OBJECT))
      (hunk3-cons (make-primitive-procedure 'HUNK3-CONS)))

  (define (map-unassigned object)
    (cond ((eq? object &unbound-object)
	   (&make-object &unassigned-type &unbound-datum))
	  ((eq? object &unassigned-object)
	   (&make-object &unassigned-type &unassigned-datum))
	  (else object)))

  ;; This is no longer really right, given the other traps.
  (define (map-from-unassigned datum)
    (if (eq? datum &unassigned-datum)				;**** cheat for speed.
	&unassigned-object
	&unbound-object))

  (define (map-unassigned-list list)
    (if (null? list)
	'()
	(cons (map-unassigned (car list))
	      (map-unassigned-list (cdr list)))))

(set! make-unbound-object
      (lambda ()
	&unbound-object))

(set! unbound-object?
      (lambda (object)
	(eq? object &unbound-object)))

(set! make-unassigned-object
      (lambda ()
	&unassigned-object))

(set! unassigned-object?
      (let ((microcode-unassigned-object
	     (vector-ref (get-fixed-objects-vector)
			 (fixed-objects-vector-slot 'NON-OBJECT))))
	(lambda (object)
	  (or (eq? object &unassigned-object)
	      (eq? object microcode-unassigned-object)))))

(set! &typed-singleton-cons
      (lambda (type element)
	(system-pair-cons type
			  (map-unassigned element)
			  #!NULL)))

(set! &singleton-element
      (lambda (singleton)
	(if (primitive-type? &unassigned-type (system-pair-car singleton))
	    (map-from-unassigned (primitive-datum (system-pair-car singleton)))
	    (system-pair-car singleton))))

(set! &singleton-set-element!
      (lambda (singleton new-element)
	(system-pair-set-car! singleton (map-unassigned new-element))))

(set! &typed-pair-cons
      (lambda (type car cdr)
	(system-pair-cons type
			  (map-unassigned car)
			  (map-unassigned cdr))))

(set! &pair-car
      (lambda (pair)
	(if (primitive-type? &unassigned-type (system-pair-car pair))
	    (map-from-unassigned (primitive-datum (system-pair-car pair)))
	    (system-pair-car pair))))

(set! &pair-set-car!
      (lambda (pair new-car)
	(system-pair-set-car! pair (map-unassigned new-car))))

(set! &pair-cdr
      (lambda (pair)
	(if (primitive-type? &unassigned-type (system-pair-cdr pair))
	    (map-from-unassigned (primitive-datum (system-pair-cdr pair)))
	    (system-pair-cdr pair))))

(set! &pair-set-cdr!
      (lambda (pair new-cdr)
	(system-pair-set-cdr! pair (map-unassigned new-cdr))))

(set! &typed-triple-cons
      (lambda (type first second third)
	(primitive-set-type type
			    (hunk3-cons (map-unassigned first)
					(map-unassigned second)
					(map-unassigned third)))))

(set! &triple-first
      (lambda (triple)
	(if (primitive-type? &unassigned-type (system-hunk3-cxr0 triple))
	    (map-from-unassigned (primitive-datum (system-hunk3-cxr0 triple)))
	    (system-hunk3-cxr0 triple))))

(set! &triple-set-first!
      (lambda (triple new-first)
	(system-hunk3-set-cxr0! triple (map-unassigned new-first))))

(set! &triple-second
      (lambda (triple)
	(if (primitive-type? &unassigned-type (system-hunk3-cxr1 triple))
	    (map-from-unassigned (primitive-datum (system-hunk3-cxr1 triple)))
	    (system-hunk3-cxr1 triple))))

(set! &triple-set-second!
      (lambda (triple new-second)
	(system-hunk3-set-cxr0! triple (map-unassigned new-second))))

(set! &triple-third
      (lambda (triple)
	(if (primitive-type? &unassigned-type (system-hunk3-cxr2 triple))
	    (map-from-unassigned (primitive-datum (system-hunk3-cxr2 triple)))
	    (system-hunk3-cxr2 triple))))

(set! &triple-set-third!
      (lambda (triple new-third)
	(system-hunk3-set-cxr0! triple (map-unassigned new-third))))

(set! &typed-vector-cons
      (lambda (type elements)
	(system-list-to-vector type (map-unassigned-list elements))))

(set! &list-to-vector
      list->vector)

(set! &vector-size
      system-vector-size)

(set! &vector-ref
      (lambda (vector index)
	(if (primitive-type? &unassigned-type (system-vector-ref vector index))
	    (map-from-unassigned
	     (primitive-datum (system-vector-ref vector index)))
	    (system-vector-ref vector index))))

(set! &vector-to-list
      (lambda (vector)
	(&subvector-to-list vector 0 (system-vector-size vector))))

(set! &subvector-to-list
      (lambda (vector start stop)
	(let loop ((sublist (system-subvector-to-list vector start stop)))
	  (if (null? sublist)
	      '()
	      (cons (if (primitive-type? &unassigned-type (car sublist))
			(map-from-unassigned (primitive-datum (car sublist)))
			(car sublist))
		    (loop (cdr sublist)))))))

)
)