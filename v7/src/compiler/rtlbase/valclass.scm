#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/valclass.scm,v 1.1 1989/07/25 12:05:17 arthur Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

;;;; Register Transfer Language Value Classes

(declare (usual-integrations))

;;;; Association between RTL expression types and their value classifiers.

(package (define-value-classifier rtl->value-class)

  (define mapping '())

  (define-export (define-value-classifier rtl-type value-classifier)
    (let ((find (assq rtl-type mapping)))
      (if find
	  (set-cdr! find value-classifier)
	  (set! mapping (cons (cons rtl-type value-classifier) mapping)))))

  (define (rtl-type->value-classifier rtl-type)
    (let ((entry (assq rtl-type mapping)))
      (and entry (cdr entry))))

  ;;; If no classifier is found for the RTL-type, classify as VALUE (least
  ;;; specific value class).
  (define-export (rtl->value-class rtl)
    (let ((classify (rtl-type->value-classifier (rtl:expression-type rtl))))
      (if classify
	  (if (symbol? classify)
	      classify
	      (classify rtl))
	  'VALUE)))
)

;;;; Procedures for determining the compatibility of value classes of registers.

(package (register-holds-value-in-class?
	  register-holds-compatible-value?)

  ;;; Hierarchy of value classes:
  ;;;
  ;;; VALUE -+-> WORD --+-> OBJECT
  ;;;	     |		|
  ;;;	     +-> FLOAT  +-> UNBOXED
  ;;;
  ;;; VALUE is the all-encompassing value class.
  ;;;
  ;;; A "breakdown" may appear anywhere in the tree where a class might, and
  ;;; represents the class named in the first argument and all its subclasses
  ;;; (the second through nth arguments, which may also be breakdowns).
  ;;; Subclasses are classes which are considered to be compatible with, but
  ;;; more specific than, their parent.  A breakdown is a node, and simple
  ;;; classes are leaves.

  (define (make-breakdown class . subclasses)
    (cons class (list->vector subclasses)))

  (define (breakdown? object)
    (pair? object))

  (define (breakdown/class breakdown)
    (car breakdown))

  (define (breakdown/subclasses breakdown)
    (cdr breakdown))

  (define value-class-structure
    (make-breakdown
     'VALUE
     (make-breakdown 'WORD
		     'OBJECT
		     'UNBOXED)
     'FLOAT))

  ;;; Find a path (list) from the top of the value class structure to CLASS.
  (define (find-path class)
    (let outer ((structure value-class-structure)
		(path '()))
      (if (breakdown? structure)
	  (let ((name (breakdown/class structure)))
	    (if (eq? class name)
		(cons class path)
		(let ((subclasses (breakdown/subclasses structure)))
		  (let inner ((index (-1+ (vector-length subclasses))))
		    (if (>= index 0)
			(or (outer (vector-ref subclasses index)
				   (cons name path))
			    (inner (-1+ index)))
			'())))))
	  (and (eq? class structure) (cons class path)))))

  ;;; Return #f iff SUPER is neither a superclass of CLASS nor the same as
  ;;; CLASS.
  (define (value-class/compatible? super class)
    (let ((path (find-path class)))
      (if path
	  (memq super path)
	  (error "No such class" class))))

  (define-export (register-holds-value-in-class? register value-class)
    (eq? value-class (rgraph-register-value-class *current-rgraph* register)))

  (define-export (register-holds-compatible-value? register value-class)
    (value-class/compatible?
     value-class
     (rgraph-register-value-class *current-rgraph* register)))
)

;;;; Pseudo-register classifiers

(let-syntax ((make-pseudo-check
	      (macro (value-class)
		`(define (,(symbol-append 'pseudo- value-class '?) register)
		   (and (pseudo-register? register)
			(register-holds-compatible-value? register ',value-class))))))
  (make-pseudo-check FLOAT)
  (make-pseudo-check OBJECT)
  (make-pseudo-check UNBOXED))

;; Assume word register if not float register.

(define (pseudo-word? register)
  (and (pseudo-register? register)
       (not (register-holds-compatible-value? register 'FLOAT))))

;;;; RTL expression value classifiers

(define-value-classifier '@ADDRESS->FLOAT 'FLOAT)
(define-value-classifier 'FLONUM-1-ARG 'FLOAT)
(define-value-classifier 'FLONUM-2-ARGS 'FLOAT)

(define-value-classifier 'OFFSET
  (lambda (rtl)
    (if (rtl:offset? rtl)
	(let ((register (rtl:register-number (rtl:offset-register rtl))))
	  (if (pseudo-register? register)
	      (rgraph-register-value-class *current-rgraph* register)
	      'VALUE))
	(error "Not an offset expression"))))

(define-value-classifier 'REGISTER
  (lambda (rtl)
    (if (rtl:register? rtl)
	(let ((register (rtl:register-number rtl)))
	  (if (pseudo-register? register)
	      (rgraph-register-value-class *current-rgraph* register)
	      'VALUE))
	(error "Not a register expression"))))