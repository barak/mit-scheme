#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/operan.scm,v 4.4 1988/10/24 22:18:41 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Operator Analysis

(declare (usual-integrations))

(package (operator-analysis)

(define-export (operator-analysis procedures applications)
  (for-each (lambda (procedure)
	      (if (procedure-continuation? procedure)
		  (set-continuation/combinations! procedure '())))
	    procedures)
  (for-each (lambda (application)
	      (if (eq? (application-type application) 'COMBINATION)
		  (analyze/combination application)))
	    applications)
  (for-each (lambda (procedure)
	      (if (procedure-continuation? procedure)
		  (set-continuation/passed-out?!
		   procedure
		   (continuation-passed-out? procedure))))
	    procedures)
  (for-each (lambda (procedure)
	      (set-procedure-always-known-operator?!
	       procedure
	       (if (procedure-continuation? procedure)
		   (analyze/continuation procedure)
		   (analyze/procedure procedure))))
	    procedures))

(define (analyze/combination combination)
  (for-each (lambda (continuation)
	      (set-continuation/combinations!
	       continuation
	       (cons combination
		     (continuation/combinations continuation))))
	    (rvalue-values (combination/continuation combination))))

(define (continuation-passed-out? continuation)
  (there-exists? (continuation/combinations continuation)
    (lambda (combination)
      (and (not (combination/inline? combination))
	   (let ((operator (combination/operator combination)))
	     (or (rvalue-passed-in? operator)
		 (there-exists? (rvalue-values operator)
		   (lambda (rvalue) (not (rvalue/procedure? rvalue))))))))))

(define (analyze/continuation continuation)
  (3-logic/and
   (and (not (continuation/passed-out? continuation)) 'ALWAYS)
   (3-logic/and
    (for-some? (continuation/returns continuation)
      (lambda (return)
	(eq? (rvalue-known-value (return/operator return))
	     continuation)))
    (for-some? (continuation/combinations continuation)
      (lambda (combination)
	(eq? (rvalue-known-value (combination/continuation combination))
	     continuation))))))

(define (for-some? items predicate)
  (let loop ((items items) (default false))
    (cond ((null? items) 'ALWAYS)
	  ((predicate (car items)) (loop (cdr items) 'SOMETIMES))
	  (else default))))

(define (3-logic/and x y)
  (cond ((and (eq? x 'ALWAYS) (eq? y 'ALWAYS)) 'ALWAYS)
	((and (not x) (not y)) false)
	(else 'SOMETIMES)))

(define (analyze/procedure procedure)
  (and (not (procedure-passed-out? procedure))
       (let ((combinations (procedure-applications procedure)))
	 (and (not (null? combinations))
	      (for-all? combinations
		(lambda (combination)
		  (eq? (rvalue-known-value (combination/operator combination))
		       procedure)))))))

;;; end OPERATOR-ANALYSIS
)