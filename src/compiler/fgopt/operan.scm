#| -*-Scheme-*-

$Id: operan.scm,v 4.8 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1987, 1989, 1999 Massachusetts Institute of Technology

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

;;;; Operator Analysis

(declare (usual-integrations))

(define (operator-analysis procedures applications)
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
      (and (not (combination/simple-inline? combination))
	   (let ((operator (combination/operator combination)))
	     (or (rvalue-passed-in? operator)
		 (there-exists? (rvalue-values operator)
		   (lambda (rvalue) (not (rvalue/procedure? rvalue))))))))))

(define (analyze/continuation continuation)
  (let ((returns (continuation/returns continuation))
	(combinations (continuation/combinations continuation)))
    (and (or (not (null? returns))
	     (not (null? combinations)))
	 (3-logic/and
	  (and (not (continuation/passed-out? continuation)) 'ALWAYS)
	  (3-logic/and
	   (for-some? returns
	     (lambda (return)
	       (eq? (rvalue-known-value (return/operator return))
		    continuation)))
	   (for-some? combinations
	     (lambda (combination)
	       (eq? (rvalue-known-value (combination/continuation combination))
		    continuation))))))))

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