;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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

;;;; Definition Scanner

(declare (usual-integrations))

;;; Scanning of internal definitions is necessary to reduce the number
;;; of "real auxiliary" variables in the system.  These bindings are
;;; maintained in alists by the microcode, and cannot be compiled as
;;; ordinary formals can.

;;; The following support is provided.  SCAN-DEFINES will find the
;;; top-level definitions in a sequence, and returns an ordered list
;;; of those names, and a new sequence in which those definitions are
;;; replaced by assignments.  UNSCAN-DEFINES will invert that.

;;; The Open Block abstraction can be used to store scanned
;;; definitions in code, which is extremely useful for code analysis
;;; and transformation.  The supplied procedures, MAKE-OPEN-BLOCK and
;;; OPEN-BLOCK-COMPONENTS, will connect directly to SCAN-DEFINES and
;;; UNSCAN-DEFINES, respectively.

(define scan-defines)
(define unscan-defines)
(define make-open-block)
(define open-block?)
(define open-block-components)

(let ((open-block-tag (make-named-tag "OPEN-BLOCK"))
      (sequence-2-type (microcode-type 'SEQUENCE-2))
      (sequence-3-type (microcode-type 'SEQUENCE-3))
      (null-sequence '(NULL-SEQUENCE)))

;;;; Scanning

;;; This depends on the fact that the lambda abstraction will preserve
;;; the order of the auxiliaries.  That is, giving MAKE-LAMBDA a list
;;; of auxiliaries will result in LAMBDA-COMPONENTS returning an
;;; EQUAL?  list.

(set! scan-defines
(named-lambda (scan-defines expression receiver)
  ((scan-loop expression receiver) '() '() null-sequence)))

(define (scan-loop expression receiver)
  (cond ((primitive-type? sequence-2-type expression)
	 (scan-loop (&pair-cdr expression)
		    (scan-loop (&pair-car expression)
			       receiver)))
	((primitive-type? sequence-3-type expression)
	 (let ((first (&triple-first expression)))
	   (if (and (vector? first)
		    (not (zero? (vector-length first)))
		    (eq? (vector-ref first 0) open-block-tag))
	       (lambda (names declarations body)
		 (receiver (append (vector-ref first 1) names)
			   (append (vector-ref first 2) declarations)
			   (cons-sequence (&triple-third expression)
					  body)))
	       (scan-loop (&triple-third expression)
			  (scan-loop (&triple-second expression)
				     (scan-loop first
						receiver))))))
	((definition? expression)
	 (definition-components expression
	   (lambda (name value)
	     (lambda (names declarations body)
	       (receiver (cons name names)
			 declarations
			 (cons-sequence (make-assignment name value)
					body))))))
	((block-declaration? expression)
	 (lambda (names declarations body)
	   (receiver names
		     (append (block-declaration-text expression)
			     declarations)
		     body)))
	(else
	 (lambda (names declarations body)
	   (receiver names
		     declarations
		     (cons-sequence expression body))))))

(define (cons-sequence action sequence)
  (cond ((primitive-type? sequence-2-type sequence)
	 (&typed-triple-cons sequence-3-type
			     action
			     (&pair-car sequence)
			     (&pair-cdr sequence)))
	((eq? sequence null-sequence)
	 action)
	(else
	 (&typed-pair-cons sequence-2-type action sequence))))

(set! unscan-defines
(named-lambda (unscan-defines names declarations body)
  (unscan-loop names body
    (lambda (names* body*)
      (if (not (null? names*))
	  (error "Extraneous auxiliaries -- get a wizard"
		 'UNSCAN-DEFINES
		 names*))
      (if (null? declarations)
	  body*
	  (&typed-pair-cons sequence-2-type
			    (make-block-declaration declarations)
			    body*))))))

(define (unscan-loop names body receiver)
  (cond ((null? names) (receiver '() body))
	((assignment? body)
	 (assignment-components body
	   (lambda (name value)
	     (if (eq? name (car names))
		 (receiver (cdr names)
			   (make-definition name value))
		 (receiver names
			   body)))))
	((primitive-type? sequence-2-type body)
	 (unscan-loop names (&pair-car body)
	   (lambda (names* body*)
	     (unscan-loop names* (&pair-cdr body)
	       (lambda (names** body**)
		 (receiver names**
			   (&typed-pair-cons sequence-2-type
					     body*
					     body**)))))))
	((primitive-type? sequence-3-type body)
	 (unscan-loop names (&triple-first body)
	   (lambda (names* body*)
	     (unscan-loop names* (&triple-second body)
	       (lambda (names** body**)
		 (unscan-loop names** (&triple-third body)
		   (lambda (names*** body***)
		     (receiver names***
			       (&typed-triple-cons sequence-3-type
						   body*
						   body**
						   body***)))))))))
	(else
	 (receiver names
		   body))))

;;;; Open Block

(set! make-open-block
(named-lambda (make-open-block names declarations body)
  (if (and (null? names)
	   (null? declarations))
      body
      (&typed-triple-cons
       sequence-3-type
       (vector open-block-tag names declarations)
       (if (null? names)
	   '()
	   (make-sequence
	    (map (lambda (name)
		   (make-definition name (make-unassigned-object)))
		 names)))
       body))))
	

(set! open-block?
(named-lambda (open-block? object)
  (and (primitive-type? sequence-3-type object)
       (vector? (&triple-first object))
       (eq? (vector-ref (&triple-first object) 0) open-block-tag))))

(set! open-block-components
(named-lambda (open-block-components open-block receiver)
  (receiver (vector-ref (&triple-first open-block) 1)
	    (vector-ref (&triple-first open-block) 2)
	    (&triple-third open-block))))

;;; end LET
