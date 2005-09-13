#| -*-Scheme-*-

$Id: scode.scm,v 14.22 2005/04/16 03:15:22 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,2001,2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; SCode Abstraction
;;; package: (runtime scode)

(declare (usual-integrations))

(define (initialize-package!)
  (set! scode-constant/type-vector (make-scode-constant/type-vector))
  unspecific)

;;;; Constant

(define scode-constant/type-vector)

(define (scode-constant? object)
  (if (vector-ref scode-constant/type-vector (object-type object))
      #t
      (and (compiled-code-address? object)
	   (not (eq? (compiled-entry-type object) 'COMPILED-EXPRESSION)))))

(define (make-scode-constant/type-vector)
  (let ((type-vector (make-vector (microcode-type/code-limit) #f)))
    (for-each (lambda (name)
		(vector-set! type-vector (microcode-type name) #t))
	      '(BIGNUM
		CHARACTER
		COMPILED-CODE-BLOCK
		CONTROL-POINT
		DELAYED
		ENTITY
		ENVIRONMENT
		EXTENDED-PROCEDURE
		FALSE
		FLONUM
		HUNK3-A
		INTERNED-SYMBOL
		NEGATIVE-FIXNUM
		NON-MARKED-VECTOR
		PAIR
		POSITIVE-FIXNUM
		PRIMITIVE
		PROCEDURE
		QUAD
		RATNUM
		RECNUM
		REFERENCE-TRAP
		RETURN-CODE
		STRING
		TRIPLE
		TRUE
		UNINTERNED-SYMBOL
		VECTOR
		VECTOR-16B
		VECTOR-1B))
    type-vector))

;;;; Quotation

(define-integrable (make-quotation expression)
  (&typed-singleton-cons (ucode-type quotation) expression))

(define-integrable (quotation? object)
  (object-type? (ucode-type quotation) object))

(define-integrable (quotation-expression quotation)
  (&singleton-element quotation))

;;;; Variable

(define-integrable (make-variable name)
  (system-hunk3-cons (ucode-type variable) name #t '()))

(define-integrable (variable? object)
  (object-type? (ucode-type variable) object))

(define-integrable (variable-name variable)
  (system-hunk3-cxr0 variable))

(define-integrable (variable-components variable receiver)
  (receiver (variable-name variable)))

;;;; Definition/Assignment

(define-integrable (make-definition name value)
  (&typed-pair-cons (ucode-type definition) name value))

(define-integrable (definition? object)
  (object-type? (ucode-type definition) object))

(define-integrable (definition-name definition)
  (system-pair-car definition))

(define-integrable (definition-value definition)
  (&pair-cdr definition))

(define (definition-components definition receiver)
  (receiver (definition-name definition)
	    (definition-value definition)))

(define-integrable (assignment? object)
  (object-type? (ucode-type assignment) object))

(define-integrable (make-assignment-from-variable variable value)
  (&typed-pair-cons (ucode-type assignment) variable value))

(define-integrable (assignment-variable assignment)
  (system-pair-car assignment))

(define-integrable (assignment-value assignment)
  (&pair-cdr assignment))

(define (assignment-components-with-variable assignment receiver)
  (receiver (assignment-variable assignment)
	    (assignment-value assignment)))

(define-integrable (make-assignment name value)
  (make-assignment-from-variable (make-variable name) value))

(define-integrable (assignment-name assignment)
  (variable-name (assignment-variable assignment)))

(define (assignment-components assignment receiver)
  (receiver (assignment-name assignment)
	    (assignment-value assignment)))

;;;; Comment

(define-integrable (make-comment text expression)
  (&typed-pair-cons (ucode-type comment) expression text))

(define-integrable (comment? object)
  (object-type? (ucode-type comment) object))

(define-integrable (comment-text comment)
  (system-pair-cdr comment))

(define-integrable (set-comment-text! comment text)
  (system-pair-set-cdr! comment text))

(define-integrable (comment-expression comment)
  (&pair-car comment))

(define-integrable (set-comment-expression! comment expression)
  (&pair-set-car! comment expression))

(define (comment-components comment receiver)
  (receiver (comment-text comment)
	    (comment-expression comment)))

;;;; Declaration

(define-integrable (make-declaration text expression)
  (make-comment (cons declaration-tag text) expression))

(define (declaration? object)
  (and (comment? object)
       (let ((text (comment-text object)))
	 (and (pair? text)
	      (eq? (car text) declaration-tag)))))

(define-integrable declaration-tag
  ((ucode-primitive string->symbol) "#[declaration]"))

(define-integrable (declaration-text declaration)
  (cdr (comment-text declaration)))

(define-integrable (set-declaration-text! declaration text)
  (set-cdr! (comment-text declaration) text))

(define-integrable (declaration-expression declaration)
  (comment-expression declaration))

(define-integrable (set-declaration-expression! declaration expression)
  (set-comment-expression! declaration expression))

(define (declaration-components declaration receiver)
  (receiver (declaration-text declaration)
	    (declaration-expression declaration)))

;;;; The-Environment

(define-integrable (make-the-environment)
  (object-new-type (ucode-type the-environment) 0))

(define-integrable (the-environment? object)
  (object-type? (ucode-type the-environment) object))

;;;; Access

(define-integrable (make-access environment name)
  (&typed-pair-cons (ucode-type access) environment name))

(define-integrable (access? object)
  (object-type? (ucode-type access) object))

(define (access-environment expression)
  (&pair-car expression))

(define-integrable (access-name expression)
  (system-pair-cdr expression))

(define (access-components expression receiver)
  (receiver (access-environment expression)
	    (access-name expression)))

;;;; Absolute Reference

(define (make-absolute-reference name . rest)
  (let loop ((reference (make-access system-global-environment name))
	     (rest rest))
    (if (pair? rest)
	(loop (make-access reference (car rest)) (cdr rest))
	reference)))

(define (absolute-reference? object)
  (and (access? object)
       (system-global-environment? (access-environment object))))

(define-integrable (absolute-reference-name reference)
  (access-name reference))

(define (absolute-reference-to? object name)
  (and (absolute-reference? object)
       (eq? (absolute-reference-name object) name)))

;;;; Delay

(define-integrable (make-delay expression)
  (&typed-singleton-cons (ucode-type delay) expression))

(define-integrable (delay? object)
  (object-type? (ucode-type delay) object))

(define-integrable (delay-expression expression)
  (&singleton-element expression))

(define-integrable (delay-components expression receiver)
  (receiver (delay-expression expression)))