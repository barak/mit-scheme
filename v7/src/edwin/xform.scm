;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Instance Variable Transformation

(declare (usual-integrations))

(define transform-instance-variables)
(let ()

(set! transform-instance-variables
(named-lambda (transform-instance-variables transforms name expression)
  (fluid-let ((name-of-self name))
    (transform-expression transforms expression))))

(define name-of-self)

(define (transform-expression transforms expression)
  ((transform-dispatch expression) transforms expression))

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
  constant)

(define (transform-variable transforms variable)
  (let ((entry (assq (variable-name variable) transforms)))
    (if (not entry)
	variable
	(make-combination vector-ref
			  (list (make-variable name-of-self)
				(cdr entry))))))

(define (transform-assignment transforms assignment)
  (assignment-components assignment
    (lambda (name value)
      (let ((entry (assq name transforms))
	    (value (transform-expression transforms value)))
	(if (not entry)
	    (make-assignment name value)
	    (make-combination vector-set!
			      (list (make-variable name-of-self)
				    (cdr entry)
				    value)))))))

(define (transform-combination transforms combination)
  (combination-components combination
    (lambda (operator operands)
      (make-combination (transform-expression transforms operator)
			(transform-expressions transforms operands)))))

(define (transform-lambda transforms lambda)
  (lambda-components** lambda
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

(define (transform-sequence transforms sequence)
  (make-sequence (transform-expressions transforms
					(sequence-actions sequence))))

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

(define (transform-delay transforms delay)
  (make-delay (transform-expression transforms (delay-expression delay))))

(define (transform-access transforms access)
  (access-components access
    (lambda (environment name)
      (make-access (transform-expression transforms environment)
		   name))))

(define (transform-in-package transforms in-package)
  (in-package-components in-package
    (lambda (environment expression)
      (make-in-package (transform-expression transforms environment)
		       expression))))

(define transform-dispatch
  (make-type-dispatcher
   `((,variable-type ,transform-variable)
     (,assignment-type ,transform-assignment)
     (,definition-type ,transform-definition)
     (,sequence-type ,transform-sequence)
     (,conditional-type ,transform-conditional)
     (,disjunction-type ,transform-disjunction)
     (,comment-type ,transform-comment)
     (,delay-type ,transform-delay)
     (,access-type ,transform-access)
     (,in-package-type ,transform-in-package)
     (,lambda-type ,transform-lambda)
     (,open-block-type ,transform-open-block)
     (,combination-type ,transform-combination))
   transform-constant))

)