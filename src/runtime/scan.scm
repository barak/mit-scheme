#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

(define-integrable sequence-type
  (ucode-type sequence-2))

(define null-sequence
  '(NULL-SEQUENCE))

(define (cons-sequence action seq)
  (if (eq? seq null-sequence)
      action
      (&typed-pair-cons sequence-type action seq)))

;;;; Scanning

;;; This depends on the fact that the lambda abstraction will preserve
;;; the order of the auxiliaries.  That is, giving MAKE-LAMBDA a list
;;; of auxiliaries will result in LAMBDA-COMPONENTS returning an
;;; EQUAL?  list.

(define (scan-defines expression receiver)
  ((scan-loop expression receiver) '() '() null-sequence))

(define (scan-loop expression receiver)
  (cond ((open-block? expression)	; must come before SEQUENCE? clause
	 (scan-loop
	  (%open-block-actions expression)
	  (lambda (names declarations body)
	    (receiver (append (%open-block-names expression) names)
		      (append (%open-block-declarations expression) declarations)
		      body))))
	((sequence? expression)
	 ;; Build the sequence from the tail-end first so that the
	 ;; null-sequence shows up in the tail and is detected by
	 ;; cons-sequence.
	 (scan-loop (sequence-immediate-second expression)
		    (scan-loop (sequence-immediate-first expression)
			       receiver)))
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

  (define (unscan-loop names body)
    (cond ((not (pair? names))
	   (values '() body))

	  ((assignment? body)
	   (assignment-components body
	     (lambda (name value)
	       (if (eq? name (car names))
		   (values (cdr names) (make-definition name value))
		   (values names body)))))

	  ((sequence? body)
	   (let ((head (sequence-immediate-first body))
		 (tail (sequence-immediate-second body)))

	     (receive (names1 unscanned-head) (unscan-loop names head)
	       (receive (names2 unscanned-tail) (unscan-loop names1 tail)
		 (values names2
			 ;; Only cons a new sequence if something changed.
			 (if (and (eq? head unscanned-head)
				  (eq? tail unscanned-tail))
			     body
			     (&typed-pair-cons
			      sequence-type
			      unscanned-head unscanned-tail)))))))

	  (else
	   (values names body))))

  (receive (names* body*) (unscan-loop names body)
    (if (not (null? names*))
	(error "Extraneous auxiliaries -- get a wizard"
	       'UNSCAN-DEFINES
	       names*))

    (if (null? declarations)
	body*
	(&typed-pair-cons
	 sequence-type
	 (make-block-declaration declarations)
	 body*))))

;;;; Open Block

(define (make-open-block names declarations actions)
  (if (and (null? names)
	   (null? declarations))
      actions
      (&typed-pair-cons
       sequence-type
       (make-open-block-descriptor names declarations)
       (&typed-pair-cons
	sequence-type
	(make-open-block-definitions names)
	actions))))

(define (open-block? object)
  (and (sequence? object)
       (open-block-descriptor? (sequence-immediate-first object))
       (sequence? (sequence-immediate-second object))))

(define (open-block-actions open-block)
  (guarantee-open-block open-block 'OPEN-BLOCK-ACTIONS)
  (%open-block-actions open-block))

(define (open-block-declarations open-block)
  (guarantee-open-block open-block 'OPEN-BLOCK-DECLARATIONS)
  (%open-block-declarations open-block))

(define (open-block-definitions open-block)
  (guarantee-open-block open-block 'OPEN-BLOCK-DEFINITIONS)
  (%open-block-definitions open-block))

(define (open-block-names open-block)
  (guarantee-open-block open-block 'OPEN-BLOCK-NAMES)
  (%open-block-names open-block))

(define (open-block-components open-block receiver)
  (guarantee-open-block open-block 'OPEN-BLOCK-COMPONENTS)
  (let ((descriptor (sequence-immediate-first open-block)))
    (receiver (%open-block-descriptor-names descriptor)
	      (%open-block-descriptor-declarations descriptor)
	      (%open-block-actions open-block))))

(define (make-open-block-definitions names)
  (let ((definitions
	  (map (lambda (name)
		 (make-definition name (make-unassigned-reference-trap)))
	       names)))
    (if (null? definitions)
	'()
	(make-sequence definitions))))

(define-guarantee open-block "SCode open-block")

(define (%open-block-descriptor open-block)
  (sequence-immediate-first open-block))

(define (%open-block-actions open-block)
  (sequence-immediate-second (sequence-immediate-second open-block)))

(define (%open-block-declarations open-block)
  (%open-block-descriptor-declarations (%open-block-descriptor open-block)))

(define (%open-block-definitions open-block)
  (sequence-immediate-first (sequence-immediate-second open-block)))

(define (%open-block-names open-block)
  (%open-block-descriptor-names (%open-block-descriptor open-block)))

(define (make-open-block-descriptor names declarations)
  (vector open-block-tag names declarations))

(define (open-block-descriptor? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? (vector-ref object 0) open-block-tag)))

(define (%open-block-descriptor-names descriptor)
  (vector-ref descriptor 1))

(define (%open-block-descriptor-declarations descriptor)
  (vector-ref descriptor 2))