#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/scan.scm,v 14.3 1989/04/18 16:29:59 cph Rel $

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

;;;; Definition Scanner
;;; package: (runtime scode-scan)

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

(define-integrable open-block-tag
  (string->symbol "#[open-block]"))

(define-integrable sequence-2-type
  (ucode-type sequence-2))

(define-integrable sequence-3-type
  (ucode-type sequence-3))

(define null-sequence
  '(NULL-SEQUENCE))

(define (cons-sequence action sequence)
  (cond ((object-type? sequence-2-type sequence)
	 (&typed-triple-cons sequence-3-type
			     action
			     (&pair-car sequence)
			     (&pair-cdr sequence)))
	((eq? sequence null-sequence)
	 action)
	(else
	 (&typed-pair-cons sequence-2-type action sequence))))

;;;; Scanning

;;; This depends on the fact that the lambda abstraction will preserve
;;; the order of the auxiliaries.  That is, giving MAKE-LAMBDA a list
;;; of auxiliaries will result in LAMBDA-COMPONENTS returning an
;;; EQUAL?  list.

(define (scan-defines expression receiver)
  ((scan-loop expression receiver) '() '() null-sequence))

(define (scan-loop expression receiver)
  (cond ((object-type? sequence-2-type expression)
	 (scan-loop (&pair-cdr expression)
		    (scan-loop (&pair-car expression)
			       receiver)))
	((object-type? sequence-3-type expression)
	 (let ((first (&triple-first expression)))
	   (if (and (vector? first)
		    (not (zero? (vector-length first)))
		    (eq? (vector-ref first 0) open-block-tag))
	       (scan-loop
		(&triple-third expression)
		(lambda (names declarations body)
		  (receiver (append (vector-ref first 1) names)
			    (append (vector-ref first 2) declarations)
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

(define (unscan-defines names declarations body)
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
			    body*)))))

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
	((object-type? sequence-2-type body)
	 (unscan-loop names (&pair-car body)
	   (lambda (names* body*)
	     (unscan-loop names* (&pair-cdr body)
	       (lambda (names** body**)
		 (receiver names**
			   (&typed-pair-cons sequence-2-type
					     body*
					     body**)))))))
	((object-type? sequence-3-type body)
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

(define (make-open-block names declarations body)
  (if (and (null? names)
	   (null? declarations))
      body
      (&typed-triple-cons
       sequence-3-type
       (vector open-block-tag names declarations)
       (if (null? names)
	   '()
	   (make-sequence (map make-definition names)))
       body)))

(define (open-block? object)
  (and (object-type? sequence-3-type object)
       (vector? (&triple-first object))
       (eq? (vector-ref (&triple-first object) 0) open-block-tag)))

(define (open-block-components open-block receiver)
  (receiver (vector-ref (&triple-first open-block) 1)
	    (vector-ref (&triple-first open-block) 2)
	    (&triple-third open-block)))