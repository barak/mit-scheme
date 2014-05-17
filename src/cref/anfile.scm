#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Free/Bound Variable Analysis

(declare (usual-integrations))

(define (analyze-file pathname)
  (analyze/top-level (fasload pathname #t)))

(define (analyze/top-level expression)
  (receive (definitions others)
      (sort-expressions (process-top-level expression))
    (let ((definition-analysis (map analyze/top-level/definition definitions)))
      (if (pair? others)
	  (cons (vector false
			'EXPRESSION
			(analyze-and-compress (make-sequence others)))
		definition-analysis)
	  definition-analysis))))

(define (sort-expressions expressions)
  (if (null? expressions)
      (values '() '())
      (let ((rest (lambda () (sort-expressions (cdr expressions)))))
	(if (block-declaration? (car expressions))
	    (rest)
	    (receive (definitions others) (rest)
	      (if (definition? (car expressions))
		  (values (cons (car expressions) definitions) others)
		  (values definitions (cons (car expressions) others))))))))

(define (process-top-level expression)
  (cond ((comment? expression)
	 (process-top-level (comment-expression expression)))
	((sequence? expression)
	 (append-map! process-top-level (sequence-actions expression)))
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