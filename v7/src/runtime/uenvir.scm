;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/uenvir.scm,v 14.1 1988/05/20 01:04:16 cph Exp $
;;;
;;;	Copyright (c) 1988 Massachusetts Institute of Technology
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
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Microcode Environments

(declare (usual-integrations))

;;;; Environment

(define-integrable (environment? object)
  (object-type? (ucode-type environment) object))

(define (environment-procedure environment)
  (select-procedure (environment->external environment)))

(define (environment-has-parent? environment)
  (not (eq? (select-parent (environment->external environment))
	    null-environment)))

(define (environment-parent environment)
  (select-parent (environment->external environment)))

(define (environment-bindings environment)
  (environment-split environment
    (lambda (external internal)
      (map (lambda (name)
	     (cons name
		   (if (lexical-unassigned? internal name)
		       '()
		       `(,(lexical-reference internal name)))))
	   (list-transform-negative
	       (map* (lambda-bound (select-lambda external))
		     car
		     (let ((extension (environment-extension internal)))
		       (if (environment-extension? extension)
			   (environment-extension-aux-list extension)
			   '())))
	     (lambda (name)
	       (lexical-unbound? internal name)))))))

(define (environment-arguments environment)
  (environment-split environment
    (lambda (external internal)
      (let ((lookup
	     (lambda (name)
	       (if (lexical-unassigned? internal name)
		   (make-unassigned-reference-trap)
		   (lexical-reference internal name)))))
	(lambda-components* (select-lambda external)
	  (lambda (name required optional rest body)
	    name body
	    (map* (let loop ((names optional))
		    (cond ((null? names) (if rest (lookup rest) '()))
			  ((lexical-unassigned? internal (car names)) '())
			  (else
			   (cons (lookup (car names)) (loop (cdr names))))))
		  lookup
		  required)))))))

(define (set-environment-parent! environment parent)
  (system-pair-set-cdr!
   (let ((extension (environment-extension environment)))
     (if (environment-extension? extension)
	 (begin (set-environment-extension-parent! extension parent)
		(environment-extension-procedure extension))
	 extension))
   parent))

(define (remove-environment-parent! environment)
  (set-environment-parent! environment null-environment))

(define null-environment
  (object-new-type (ucode-type null) 1))

(define (environment-split environment receiver)
  (let ((procedure (select-procedure environment)))
    (let ((lambda (compound-procedure-lambda procedure)))
      (receiver (if (internal-lambda? lambda)
		    (compound-procedure-environment procedure)
		    environment)
		environment))))

(define (environment->external environment)
  (let ((procedure (select-procedure environment)))
    (if (internal-lambda? (compound-procedure-lambda procedure))
	(compound-procedure-environment procedure)
	environment)))

(define-integrable (select-extension environment)
  (system-vector-ref environment 0))

(define (select-procedure environment)
  (let ((object (select-extension environment)))
    (if (environment-extension? object)
	(environment-extension-procedure object)
	object)))

(define (select-parent environment)
  (compound-procedure-environment (select-procedure environment)))

(define (select-lambda environment)
  (compound-procedure-lambda (select-procedure environment)))

(define (environment-extension environment)
  (select-extension (environment->external environment)))