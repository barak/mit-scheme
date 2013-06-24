;;; -*-Scheme-*-
;;;
;;;$Id: xform.scm,v 1.12 2002/02/03 03:38:55 cph Exp $
;;;
;;; Copyright (c) 1985, 1989, 1990, 1999, 2001, 2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

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
	(make-combination vector-ref (list name-of-self (cdr entry))))))

(define (transform-assignment transforms assignment)
  (assignment-components assignment
    (lambda (name value)
      (let ((entry (assq name transforms))
	    (value (transform-expression transforms value)))
	(if (not entry)
	    (make-assignment name value)
	    (make-combination vector-set!
			      (list name-of-self
				    (cdr entry)
				    value)))))))

(define (transform-combination transforms combination)
  (combination-components combination
    (lambda (operator operands)
      (make-combination (transform-expression transforms operator)
			(transform-expressions transforms operands)))))

(define (transform-lambda transforms expression)
  (lambda-components** expression
    (lambda (pattern bound body)
      (make-lambda** pattern bound
		     (transform-expression (remove-transforms transforms bound)
					   body)))))

(define (transform-open-block transforms open-block)
  (open-block-components open-block
    (lambda (names declarations body)
      (make-open-block names declarations
		       (transform-expression (remove-transforms transforms
								names)
					     body)))))

(define (transform-definition transforms definition)
  (definition-components definition
    (lambda (name value)
      (error "Free definition encountered:" name)
      (make-definition name (transform-expression transforms value)))))

(define (transform-sequence transforms expression)
  (make-sequence (transform-expressions transforms
					(sequence-actions expression))))

(define (transform-conditional transforms conditional)
  (conditional-components conditional
    (lambda (predicate consequent alternative)
      (make-conditional (transform-expression transforms predicate)
			(transform-expression transforms consequent)
			(transform-expression transforms alternative)))))

(define (transform-disjunction transforms disjunction)
  (disjunction-components disjunction
    (lambda (predicate alternative)
      (make-disjunction (transform-expression transforms predicate)
			(transform-expression transforms alternative)))))

(define (transform-comment transforms comment)
  (comment-components comment
    (lambda (text expression)
      (make-comment text (transform-expression transforms expression)))))

(define (transform-delay transforms expression)
  (make-delay (transform-expression transforms (delay-expression expression))))

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