#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/anfile.scm,v 1.4 1990/10/05 11:36:32 cph Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Free/Bound Variable Analysis

(declare (usual-integrations))

(define (analyze-file pathname)
  (analyze/top-level (fasload pathname)))

(define (analyze/top-level expression)
  (with-values (lambda () (sort-expressions (process-top-level expression)))
    (lambda (definitions others)
      (let ((definition-analysis
	      (map analyze/top-level/definition definitions)))
	(if (not (null? others))
	    (cons (vector false
			  'EXPRESSION
			  (analyze-and-compress (make-sequence others)))
		  definition-analysis)
	    definition-analysis)))))

(define (sort-expressions expressions)
  (if (null? expressions)
      (values '() '())
      (let ((rest (lambda () (sort-expressions (cdr expressions)))))
	(if (block-declaration? (car expressions))
	    (rest)
	    (with-values rest
	      (lambda (definitions others)
		(if (definition? (car expressions))
		    (values (cons (car expressions) definitions) others)
		    (values definitions (cons (car expressions) others)))))))))

(define (process-top-level expression)
  (cond ((comment? expression)
	 (process-top-level (comment-expression expression)))
	((sequence? expression)
	 (mapcan process-top-level (sequence-actions expression)))
	(else
	 (list expression))))

(define (analyze/top-level/definition definition)
  (let ((name (definition-name definition))
	(expression (definition-value definition)))
    (cond ((unassigned-reference-trap? expression)
	   (vector name 'UNASSIGNED '#()))
	  ((scode-constant? expression)
	   (vector name 'CONSTANT '#()))
	  (else
	   (vector name
		   (cond ((lambda? expression) 'LAMBDA)
			 ((delay? expression) 'DELAY)
			 (else 'EXPRESSION))
		   (analyze-and-compress expression))))))

(define (analyze-and-compress expression)
  (list->vector (analyze/expression expression)))

(define (analyze/expression expression)
  ((scode-walk analyze/dispatch expression) expression))

(define (analyze/expressions expressions)
  (if (null? expressions)
      '()
      (eq-set-union (analyze/expression (car expressions))
		    (analyze/expressions (cdr expressions)))))

(define (analyze/uninteresting expression)
  (if (primitive-procedure? expression) (list expression) '()))

(define (analyze/error expression)
  (error "Illegal expression" expression))

(define (analyze/access expression)
  (if (access-environment expression)
      (warn "Access to non-global environment:" (unsyntax expression)))
  (list expression))

(define (analyze/variable expression)
  (list (variable-name expression)))

(define (analyze/assignment expression)
  (eq-set-adjoin (assignment-name expression)
		 (analyze/expression (assignment-value expression))))

(define (analyze/combination expression)
  (eq-set-union (analyze/expression (combination-operator expression))
		(analyze/expressions (combination-operands expression))))

(define (analyze/lambda expression)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      name declarations
      (eq-set-difference (analyze/expression body)
			 (append required
				 optional
				 (if rest (list rest) '())
				 auxiliary)))))

(define (analyze/error-combination expression)
  (combination-components expression
    (lambda (operator operands)
      (analyze/expressions (list operator (car operands) (cadr operands))))))

(define (analyze/delay expression)
  (analyze/expression (delay-expression expression)))

(define (analyze/sequence expression)
  (analyze/expressions (sequence-actions expression)))

(define (analyze/conditional expression)
  (analyze/expressions (conditional-components expression list)))

(define (analyze/disjunction expression)
  (analyze/expressions (disjunction-components expression list)))

(define (analyze/comment expression)
  (analyze/expression (comment-expression expression)))

(define analyze/dispatch
  (make-scode-walker
   analyze/uninteresting
   `((ACCESS ,analyze/access)
     (ASSIGNMENT ,analyze/assignment)
     (COMBINATION ,analyze/combination)
     (COMMENT ,analyze/comment)
     (CONDITIONAL ,analyze/conditional)
     (DEFINITION ,analyze/error)
     (DELAY ,analyze/delay)
     (DISJUNCTION ,analyze/disjunction)
     (ERROR-COMBINATION ,analyze/error-combination)
     (IN-PACKAGE ,analyze/error)
     (LAMBDA ,analyze/lambda)
     (SEQUENCE ,analyze/sequence)
     (VARIABLE ,analyze/variable))))

(define (eq-set-adjoin x y)
  (if (memq x y)
      y
      (cons x y)))

(define (eq-set-union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (if (memq (car x) y)
		      y
		      (cons (car x) y)))))))

(define (eq-set-difference x y)
  (let loop ((x x))
    (cond ((null? x) '())
	  ((memq (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x)))))))