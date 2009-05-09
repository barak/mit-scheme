#| -*-Scheme-*-

$Id: cgen.scm,v 4.12 2009/02/02 15:03:21 riastradh Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Generate SCode from Expression
;;; package: (scode-optimizer cgen)

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution)
	 (integrate-external "object"))

(define *sf-associate*
  (lambda (new old)
    old new
    false))

(define (cgen/output old new)
  (*sf-associate* new (and old (object/scode old)))
  new)

(define (cgen/external quotation)
  (fluid-let ((flush-declarations? true))
    (cgen/output quotation
		 (cgen/top-level quotation))))

(define (cgen/external-with-declarations expression)
  (fluid-let ((flush-declarations? false))
    (cgen/expression (list false) expression)))

(define (cgen/top-level quotation)
  (let ((block (quotation/block quotation))
	(expression (quotation/expression quotation)))
    (let ((result (cgen/expression (list block) expression)))
      (if (open-block? expression)
	  result
	  (cgen/declaration (block/declarations block) result)))))

(define (cgen/declaration declarations expression)
  (let ((declarations (maybe-flush-declarations declarations)))
    (if (null? declarations)
	expression
	(make-declaration declarations expression))))

(define flush-declarations?)

(define (maybe-flush-declarations declarations)
  (if (null? declarations)
      '()
      (let ((declarations (declarations/original declarations)))
	(if flush-declarations?
	    (let loop ((declarations declarations))
	      (cond ((null? declarations) '())
		    ((declarations/known? (car declarations))
		     (loop (cdr declarations)))
		    (else
		     (if (not (known-compiler-declaration? (car declarations)))
			 (warn "Unused declaration" (car declarations)))
		     (cons (car declarations) (loop (cdr declarations))))))
	    declarations))))

(define *known-compiler-declarations*
  ;; Declarations which are not handled by SF but are known to be handled
  ;; by the compiler so SF ignores then silently.
  '(
    CONSTANT
    IGNORE-ASSIGNMENT-TRAPS
    IGNORE-REFERENCE-TRAPS
    NO-RANGE-CHECKS
    NO-TYPE-CHECKS
    PURE-FUNCTION
    RANGE-CHECKS
    SIDE-EFFECT-FREE
    TYPE-CHECKS
    USUAL-DEFINITION
    UUO-LINK
    ))

(define (known-compiler-declaration? declaration)
  (memq (car declaration) *known-compiler-declarations*))

(define (cgen/expressions interns expressions)
  (map (lambda (expression)
	 (cgen/expression interns expression))
       expressions))

(declare (integrate-operator cgen/expression))

(define (cgen/expression interns expression)
  ((expression/method dispatch-vector expression) interns expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define %define-method/cgen
  (expression/make-method-definer dispatch-vector))

(define-integrable (define-method/cgen type handler)
  (%define-method/cgen type
   (lambda (interns expression)
     (cgen/output expression (handler interns expression)))))

(define (cgen/variable interns variable)
  (cdr (or (assq variable (cdr interns))
	   (let ((association
		  (cons variable (make-variable (variable/name variable)))))
	     (set-cdr! interns (cons association (cdr interns)))
	     association))))

(define-method/cgen 'ACCESS
  (lambda (interns expression)
    (make-access (cgen/expression interns (access/environment expression))
		 (access/name expression))))

(define-method/cgen 'ASSIGNMENT
  (lambda (interns expression)
    (make-assignment-from-variable
     (cgen/variable interns (assignment/variable expression))
     (cgen/expression interns (assignment/value expression)))))

(define-method/cgen 'COMBINATION
  (lambda (interns expression)
    (make-combination
     (cgen/expression interns (combination/operator expression))
     (cgen/expressions interns (combination/operands expression)))))

(define-method/cgen 'CONDITIONAL
  (lambda (interns expression)
    (make-conditional
     (cgen/expression interns (conditional/predicate expression))
     (cgen/expression interns (conditional/consequent expression))
     (cgen/expression interns (conditional/alternative expression)))))

(define-method/cgen 'CONSTANT
  (lambda (interns expression)
    interns ; is ignored
    (constant/value expression)))

(define-method/cgen 'DECLARATION
  (lambda (interns expression)
    (cgen/declaration (declaration/declarations expression)
		      (cgen/expression interns
				       (declaration/expression expression)))))

(define-method/cgen 'DELAY
  (lambda (interns expression)
    (make-delay (cgen/expression interns (delay/expression expression)))))

(define-method/cgen 'DISJUNCTION
  (lambda (interns expression)
    (make-disjunction
     (cgen/expression interns (disjunction/predicate expression))
     (cgen/expression interns (disjunction/alternative expression)))))

(define-method/cgen 'PROCEDURE
  (lambda (interns procedure)
    interns ; ignored
    (make-lambda* (procedure/name procedure)
		  (map variable/name (procedure/required procedure))
		  (map variable/name (procedure/optional procedure))
		  (let ((rest (procedure/rest procedure)))
		    (and rest (variable/name rest)))
		  (let ((block (procedure/block procedure)))
		    (make-open-block
		     '()
		     (maybe-flush-declarations (block/declarations block))
		     (cgen/expression (list block)
				      (procedure/body procedure)))))))

(define-method/cgen 'OPEN-BLOCK
  (lambda (interns expression)
    interns ; is ignored
    (let ((block (open-block/block expression)))
      (make-open-block '()
		       (maybe-flush-declarations (block/declarations block))
		       (cgen/body (list block) expression)))))

(define (cgen/body interns open-block)
  (make-sequence
   (let loop
       ((variables (open-block/variables open-block))
	(values (open-block/values open-block))
	(actions (open-block/actions open-block)))
     (cond ((null? variables) (cgen/expressions interns actions))
	   ((null? actions) (error "Extraneous auxiliaries"))
	   ((eq? (car actions) open-block/value-marker)
	    (cons (make-definition (variable/name (car variables))
				   (cgen/expression interns (car values)))
		  (loop (cdr variables) (cdr values) (cdr actions))))
	   (else
	    (cons (cgen/expression interns (car actions))
		  (loop variables values (cdr actions))))))))

(define-method/cgen 'QUOTATION
  (lambda (interns expression)
    interns ; ignored
    (make-quotation (cgen/top-level expression))))

(define-method/cgen 'REFERENCE
  (lambda (interns expression)
    (cgen/variable interns (reference/variable expression))))

(define-method/cgen 'SEQUENCE
  (lambda (interns expression)
    (let ((actions
	   (if flush-declarations?
	       (remove-references (sequence/actions expression))
	       (sequence/actions expression))))
      (if (null? (cdr actions))
	  (cgen/expression interns (car actions))
	  (make-sequence (cgen/expressions interns actions))))))

(define (remove-references actions)
  (if (null? (cdr actions))
      actions
      (let ((rest (remove-references (cdr actions))))
	(if (reference? (car actions))
	    rest
	    (cons (car actions) rest)))))

(define-method/cgen 'THE-ENVIRONMENT
  (lambda (interns expression)
    interns expression ; ignored
    (make-the-environment)))