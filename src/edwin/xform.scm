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

;;;; Instance Variable Transformation

(declare (usual-integrations))

(define (transform-instance-variables transforms name free expression)
  (fluid-let ((name-of-self name))
    (transform-expression (remove-transforms transforms free) expression)))

(define name-of-self)

(define (transform-expression transforms expression)
  ((scode-walk scode-walker expression) transforms expression))

(define (transform-expressions transforms expressions)
  (define (transform-expression-loop expressions)
    (if (null? expressions)
	'()
	(cons (transform-expression transforms (car expressions))
	      (transform-expression-loop (cdr expressions)))))
  (transform-expression-loop expressions))

(define (remove-transforms transforms names)
  (define (loop transforms)
    (cond ((null? transforms) '())
	  ((memq (caar transforms) names)
	   (loop (cdr transforms)))
	  (else
	   (cons (car transforms)
		 (loop (cdr transforms))))))
  (loop transforms))

(define (transform-constant transforms constant)
  transforms
  constant)

(define (transform-variable transforms variable)
  (let ((entry (assq (scode-variable-name variable) transforms)))
    (if (not entry)
	variable
	(make-scode-combination (make-primitive-procedure 'vector-ref)
				(list name-of-self (cdr entry))))))

(define (transform-assignment transforms assignment)
  (let ((name (scode-assignment-name assignment))
	(value (scode-assignment-value assignment)))
    (let ((entry (assq name transforms))
	  (value (transform-expression transforms value)))
      (if (not entry)
	  (make-scode-assignment name value)
	  (make-scode-combination (make-primitive-procedure 'vector-set!)
				  (list name-of-self
					(cdr entry)
					value))))))

(define (transform-combination transforms combination)
  (let ((operator (scode-combination-operator combination))
	(operands (scode-combination-operands combination)))
    (make-scode-combination (transform-expression transforms operator)
			    (transform-expressions transforms operands))))

(define (transform-lambda transforms expression)
  (lambda-components** expression
    (lambda (pattern bound body)
      (make-lambda** pattern bound
		     (transform-expression (remove-transforms transforms bound)
					   body)))))

(define (transform-open-block transforms open-block)
  (let ((names (scode-open-block-names open-block)))
    (make-scode-open-block
     names
     (scode-open-block-declarations open-block)
     (transform-expression (remove-transforms transforms names)
			   (scode-open-block-actions open-block)))))

(define (transform-definition transforms definition)
  (let ((name (scode-definition-name definition))
	(value (scode-definition-value definition)))
    (error "Free definition encountered:" name)
    (make-scode-definition name (transform-expression transforms value))))

(define (transform-sequence transforms expression)
  (make-scode-sequence
   (transform-expressions transforms (scode-sequence-actions expression))))

(define (transform-conditional transforms conditional)
  (make-scode-conditional
   (transform-expression transforms (scode-conditional-predicate conditional))
   (transform-expression transforms (scode-conditional-consequent conditional))
   (transform-expression transforms
			 (scode-conditional-alternative conditional))))

(define (transform-disjunction transforms disjunction)
  (make-scode-disjunction
   (transform-expression transforms (scode-disjunction-predicate disjunction))
   (transform-expression transforms
			 (scode-disjunction-alternative disjunction))))

(define (transform-comment transforms comment)
  (make-scode-comment
   (scode-comment-text comment)
   (transform-expression transforms (scode-comment-expression comment))))

(define (transform-delay transforms expression)
  (make-scode-delay
   (transform-expression transforms (scode-delay-expression expression))))

(define scode-walker
  (make-scode-walker transform-constant
		     `((ASSIGNMENT ,transform-assignment)
		       (COMBINATION ,transform-combination)
		       (COMMENT ,transform-comment)
		       (CONDITIONAL ,transform-conditional)
		       (DEFINITION ,transform-definition)
		       (DELAY ,transform-delay)
		       (DISJUNCTION ,transform-disjunction)
		       (LAMBDA ,transform-lambda)
		       (OPEN-BLOCK ,transform-open-block)
		       (SEQUENCE ,transform-sequence)
		       (VARIABLE ,transform-variable))))