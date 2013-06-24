#| -*-Scheme-*-

$Id: scan.scm,v 14.6 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
  ((ucode-primitive string->symbol) "#[open-block]"))

(define-integrable sequence-2-type
  (ucode-type sequence-2))

(define-integrable sequence-3-type
  (ucode-type sequence-3))

(define null-sequence
  '(NULL-SEQUENCE))

(define (cons-sequence action seq)
  (cond ((object-type? sequence-2-type seq)
	 (&typed-triple-cons sequence-3-type
			     action
			     (&pair-car seq)
			     (&pair-cdr seq)))
	((eq? seq null-sequence)
	 action)
	(else
	 (&typed-pair-cons sequence-2-type action seq))))

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