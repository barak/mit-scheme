#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/cgen.scm,v 3.0 1987/03/10 13:24:42 cph Exp $

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

;;;; SCode Optimizer: Generate SCode from Expression

(declare (usual-integrations))

(define (cgen/external quotation)
  (fluid-let ((flush-declarations? true))
    (cgen/top-level quotation)))

(define (cgen/external-with-declarations expression)
  (fluid-let ((flush-declarations? false))
    (cgen/expression (list false) expression)))

(define (cgen/top-level quotation)
  (let ((block (quotation/block quotation))
	(expression (quotation/expression quotation)))
    (cgen/declaration (block/declarations block)
		      (cgen/expression (list block) expression))))

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
	    (begin (for-each (lambda (declaration)
			       (if (not (declarations/known? declaration))
				   (warn "Unused declaration" declaration)))
			     declarations)
		   '())
	    declarations))))

(define (cgen/expressions interns expressions)
  (map (lambda (expression)
	 (cgen/expression interns expression))
       expressions))

(define (cgen/expression interns expression)
  ((expression/method dispatch-vector expression) interns expression))

(define dispatch-vector
  (expression/make-dispatch-vector))

(define define-method/cgen
  (expression/make-method-definer dispatch-vector))

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

(define-method/cgen 'IN-PACKAGE
  (lambda (interns expression)
    (make-in-package
     (cgen/expression interns (in-package/environment expression))
     (cgen/top-level (in-package/quotation expression)))))

(define-method/cgen 'PROCEDURE
  (lambda (interns procedure)
    (make-lambda* (variable/name (procedure/name procedure))
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
    (make-quotation (cgen/top-level expression))))

(define-method/cgen 'REFERENCE
  (lambda (interns expression)
    (cgen/variable interns (reference/variable expression))))

(define-method/cgen 'SEQUENCE
  (lambda (interns expression)
    (make-sequence (cgen/expressions interns (sequence/actions expression)))))

(define-method/cgen 'THE-ENVIRONMENT
  (lambda (interns expression)
    (make-the-environment)))