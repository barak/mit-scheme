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
			'expression
			(analyze-and-compress (make-scode-sequence others)))
		definition-analysis)
	  definition-analysis))))

(define (sort-expressions expressions)
  (if (null? expressions)
      (values '() '())
      (let ((rest (lambda () (sort-expressions (cdr expressions)))))
	(if (scode-block-declaration? (car expressions))
	    (rest)
	    (receive (definitions others) (rest)
	      (if (scode-definition? (car expressions))
		  (values (cons (car expressions) definitions) others)
		  (values definitions (cons (car expressions) others))))))))

(define (process-top-level expression)
  (cond ((scode-comment? expression)
	 (process-top-level (scode-comment-expression expression)))
	((scode-sequence? expression)
	 (append-map! process-top-level (scode-sequence-actions expression)))
	(else
	 (list expression))))

(define (analyze/top-level/definition definition)
  (let ((name (scode-definition-name definition))
	(expression (scode-definition-value definition)))
    (cond ((unassigned-reference-trap? expression)
	   (vector name 'unassigned '#()))
	  ((scode-constant? expression)
	   (vector name 'constant '#()))
	  (else
	   (vector name
		   (cond ((scode-lambda? expression) 'lambda)
			 ((scode-delay? expression) 'delay)
			 (else 'expression))
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
  (if (scode-access-environment expression)
      (warn "Access to non-global environment:" (unsyntax expression)))
  (list expression))

(define (analyze/variable expression)
  (list (scode-variable-name expression)))

(define (analyze/assignment expression)
  (eq-set-adjoin (scode-assignment-name expression)
		 (analyze/expression (scode-assignment-value expression))))

(define (analyze/combination expression)
  (eq-set-union (analyze/expression (scode-combination-operator expression))
		(analyze/expressions (scode-combination-operands expression))))

(define (analyze/lambda expression)
  (scode-lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      name declarations
      (eq-set-difference (analyze/expression body)
			 (append required
				 optional
				 (if rest (list rest) '())
				 auxiliary)))))

(define (analyze/error-combination expression)
  (let ((operator (scode-combination-operator expression))
	(operands (scode-combination-operands expression)))
    (analyze/expressions (list operator (car operands) (cadr operands)))))

(define (analyze/delay expression)
  (analyze/expression (scode-delay-expression expression)))

(define (analyze/sequence expression)
  (analyze/expressions (scode-sequence-actions expression)))

(define (analyze/conditional expression)
  (analyze/expressions
   (list (scode-conditional-predicate expression)
	 (scode-conditional-consequent expression)
	 (scode-conditional-alternative expression))))

(define (analyze/disjunction expression)
  (analyze/expressions
   (list (scode-disjunction-predicate expression)
	 (scode-disjunction-alternative expression))))

(define (analyze/comment expression)
  (analyze/expression (scode-comment-expression expression)))

(define analyze/dispatch
  (make-scode-walker
   analyze/uninteresting
   `((access ,analyze/access)
     (assignment ,analyze/assignment)
     (combination ,analyze/combination)
     (comment ,analyze/comment)
     (conditional ,analyze/conditional)
     (definition ,analyze/error)
     (delay ,analyze/delay)
     (disjunction ,analyze/disjunction)
     (error-combination ,analyze/error-combination)
     (lambda ,analyze/lambda)
     (sequence ,analyze/sequence)
     (variable ,analyze/variable))))

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