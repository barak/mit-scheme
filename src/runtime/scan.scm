#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;; The Open Block abstraction can be used to store scanned definitions in code,
;;; which is extremely useful for code analysis and transformation.

(define-integrable sequence-type
  (ucode-type sequence))

(define null-sequence
  '(null-sequence))

(define (cons-sequence action seq)
  (if (eq? seq null-sequence)
      action
      (&typed-pair-cons sequence-type action seq)))

;;;; Scanning

;;; This depends on the fact that the lambda abstraction will preserve
;;; the order of the auxiliaries.  That is, giving MAKE-LAMBDA a list
;;; of auxiliaries will result in SCODE-LAMBDA-COMPONENTS returning an
;;; EQUAL?  list.

(define (scan-defines expression receiver)
  ((scan-loop expression receiver) '() '() null-sequence))

(define (scan-loop expression receiver)
  (cond ((scode-open-block? expression)	;must come before SCODE-SEQUENCE? clause
	 (scan-loop
	  (%open-block-actions expression)
	  (lambda (names declarations body)
	    (receiver (append (%open-block-names expression) names)
		      (append (%open-block-declarations expression)
			      declarations)
		      body))))
	((scode-sequence? expression)
	 ;; Build the sequence from the tail-end first so that the
	 ;; null-sequence shows up in the tail and is detected by
	 ;; cons-sequence.
	 (let loop
	     ((actions (scode-sequence-actions expression))
	      (receiver receiver))
	   (if (pair? actions)
	       (loop (cdr actions)
		     (scan-loop (car actions) receiver))
	       receiver)))
	((scode-definition? expression)
	 (let ((name (scode-definition-name expression))
	       (value (scode-definition-value expression)))
	   (lambda (names declarations body)
	     (receiver (cons name names)
		       declarations
		       (cons-sequence (make-scode-assignment name value)
				      body)))))
	((scode-block-declaration? expression)
	 (lambda (names declarations body)
	   (receiver names
		     (append (scode-block-declaration-text expression)
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

	  ((scode-assignment? body)
	   (let ((name (scode-assignment-name body))
		 (value (scode-assignment-value body)))
	     (if (eq? name (car names))
		 (values (cdr names) (make-scode-definition name value))
		 (values names body))))

	  ((scode-sequence? body)
	   (let loop
	       ((names names)
		(actions (scode-sequence-actions body))
		(unscanned-actions '()))
	     (if (pair? actions)
		 (receive (names* unscanned-action)
		     (unscan-loop names (car actions))
		   (loop names*
			 (cdr actions)
			 (cons unscanned-action unscanned-actions)))
		 (values names
			 (make-scode-sequence (reverse unscanned-actions))))))

	  (else
	   (values names body))))

  (receive (names* body*) (unscan-loop names body)
    (if (not (null? names*))
	(error "Extraneous auxiliaries -- get a wizard"
	       'unscan-defines
	       names*))

    (if (null? declarations)
	body*
	(&typed-pair-cons sequence-type
			  (make-scode-block-declaration declarations)
			  body*))))

;;;; Open Block

(define (make-scode-open-block names declarations actions)
  (if (and (null? names)
	   (null? declarations))
      actions
      (make-scode-sequence
       (cons (make-open-block-descriptor names declarations)
	     (append (map %make-open-block-definition names)
		     (list actions))))))

(define (%make-open-block-definition name)
  (make-scode-definition name (make-unassigned-reference-trap)))

(define (scode-open-block? object)
  (and (scode-sequence? object)
       (let ((actions (scode-sequence-actions object)))
	 (and (open-block-descriptor? (car actions))
	      (let ((names (%open-block-descriptor-names (car actions))))
		(and (fix:> (length (cdr actions)) (length names))
		     (every %open-block-definition-named?
			    names
			    (cdr actions))))))))
(register-predicate! scode-open-block? 'open-block '<= scode-sequence?)

(define (%open-block-definition-named? name expr)
  (and (scode-definition? expr)
       (eq? name (scode-definition-name expr))
       (unassigned-reference-trap? (scode-definition-value expr))))

(define (scode-open-block-names open-block)
  (guarantee scode-open-block? open-block 'scode-open-block-names)
  (%open-block-names open-block))

(define (scode-open-block-declarations open-block)
  (guarantee scode-open-block? open-block 'scode-open-block-declarations)
  (%open-block-declarations open-block))

(define (scode-open-block-actions open-block)
  (guarantee scode-open-block? open-block 'scode-open-block-actions)
  (%open-block-actions open-block))

(define (%open-block-descriptor open-block)
  (car (scode-sequence-actions open-block)))

(define (%open-block-names open-block)
  (%open-block-descriptor-names (%open-block-descriptor open-block)))

(define (%open-block-declarations open-block)
  (%open-block-descriptor-declarations (%open-block-descriptor open-block)))

(define (%open-block-actions open-block)
  (make-scode-sequence
   (list-tail (cdr (scode-sequence-actions open-block))
	      (length (%open-block-names open-block)))))

(define-integrable (make-open-block-descriptor names declarations)
  (vector open-block-tag names declarations))

(define (open-block-descriptor? object)
  (and (vector? object)
       (fix:> (vector-length object) 0)
       (eq? open-block-tag (vector-ref object 0))))

(define-integrable open-block-tag '|#[open-block]|)

(define-integrable (%open-block-descriptor-names descriptor)
  (vector-ref descriptor 1))

(define-integrable (%open-block-descriptor-declarations descriptor)
  (vector-ref descriptor 2))