#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

(define (make-quotation expression)
  (&typed-singleton-cons (ucode-type quotation) expression))

(define (quotation? object)
  (object-type? (ucode-type quotation) object))

(define-guarantee quotation "SCode quotation")

(define (quotation-expression quotation)
  (guarantee-quotation quotation 'QUOTATION-EXPRESSION)
  (&singleton-element quotation))

;;;; Variable

(define (make-variable name)
  (guarantee-symbol name 'MAKE-VARIABLE)
  (system-hunk3-cons (ucode-type variable) name #t '()))

(define (variable? object)
  (object-type? (ucode-type variable) object))

(define-guarantee variable "SCode variable")

(define (variable-name variable)
  (guarantee-variable variable 'VARIABLE-NAME)
  (system-hunk3-cxr0 variable))

(define (variable-components variable receiver)
  (receiver (variable-name variable)))

;;;; Definition/Assignment

(define (make-definition name value)
  (guarantee-symbol name 'MAKE-DEFINITION)
  (&typed-pair-cons (ucode-type definition) name value))

(define (definition? object)
  (object-type? (ucode-type definition) object))

(define-guarantee definition "SCode definition")

(define (definition-name definition)
  (guarantee-definition definition 'DEFINITION-NAME)
  (system-pair-car definition))

(define (definition-value definition)
  (guarantee-definition definition 'DEFINITION-VALUE)
  (&pair-cdr definition))

(define (definition-components definition receiver)
  (receiver (definition-name definition)
	    (definition-value definition)))

(define (assignment? object)
  (object-type? (ucode-type assignment) object))

(define-guarantee assignment "SCode assignment")

(define (make-assignment-from-variable variable value)
  (guarantee-variable variable 'MAKE-ASSIGNMENT-FROM-VARIABLE)
  (&typed-pair-cons (ucode-type assignment) variable value))

(define (assignment-variable assignment)
  (guarantee-assignment assignment 'ASSIGNMENT-VARIABLE)
  (system-pair-car assignment))

(define (assignment-value assignment)
  (guarantee-assignment assignment 'ASSIGNMENT-VALUE)
  (&pair-cdr assignment))

(define (assignment-components-with-variable assignment receiver)
  (receiver (assignment-variable assignment)
	    (assignment-value assignment)))

(define (make-assignment name value)
  (guarantee-symbol name 'MAKE-ASSIGNMENT)
  (make-assignment-from-variable (make-variable name) value))

(define (assignment-name assignment)
  (variable-name (assignment-variable assignment)))

(define (assignment-components assignment receiver)
  (receiver (assignment-name assignment)
	    (assignment-value assignment)))

;;;; Comment

(define (make-comment text expression)
  (&typed-pair-cons (ucode-type comment) expression text))

(define (comment? object)
  (object-type? (ucode-type comment) object))

(define-guarantee comment "SCode comment")

(define (comment-text comment)
  (guarantee-comment comment 'COMMENT-TEXT)
  (system-pair-cdr comment))

(define (set-comment-text! comment text)
  (guarantee-comment comment 'SET-COMMENT-TEXT!)
  (system-pair-set-cdr! comment text))

(define (comment-expression comment)
  (guarantee-comment comment 'COMMENT-EXPRESSION)
  (&pair-car comment))

(define (set-comment-expression! comment expression)
  (guarantee-comment comment 'SET-COMMENT-EXPRESSION!)
  (&pair-set-car! comment expression))

(define (comment-components comment receiver)
  (receiver (comment-text comment)
	    (comment-expression comment)))

;;;; Declaration

(define (make-declaration text expression)
  (make-comment (cons declaration-tag text) expression))

(define (declaration? object)
  (and (comment? object)
       (let ((text (comment-text object)))
	 (and (pair? text)
	      (eq? (car text) declaration-tag)))))

(define declaration-tag
  ((ucode-primitive string->symbol) "#[declaration]"))

(define-guarantee declaration "SCode declaration")

(define (declaration-text declaration)
  (guarantee-declaration declaration 'DECLARATION-TEXT)
  (cdr (comment-text declaration)))

(define (set-declaration-text! declaration text)
  (guarantee-declaration declaration 'SET-DECLARATION-TEXT!)
  (set-cdr! (comment-text declaration) text))

(define (declaration-expression declaration)
  (guarantee-declaration declaration 'DECLARATION-EXPRESSION)
  (comment-expression declaration))

(define (set-declaration-expression! declaration expression)
  (guarantee-declaration declaration 'SET-DECLARATION-EXPRESSION!)
  (set-comment-expression! declaration expression))

(define (declaration-components declaration receiver)
  (receiver (declaration-text declaration)
	    (declaration-expression declaration)))

;;;; The-Environment

(define (make-the-environment)
  (object-new-type (ucode-type the-environment) 0))

(define (the-environment? object)
  (object-type? (ucode-type the-environment) object))

;;;; Access

(define (make-access environment name)
  (guarantee-symbol name 'MAKE-ACCESS)
  (&typed-pair-cons (ucode-type access) environment name))

(define (access? object)
  (object-type? (ucode-type access) object))

(define-guarantee access "SCode access")

(define (access-environment expression)
  (guarantee-access expression 'ACCESS-ENVIRONMENT)
  (&pair-car expression))

(define (access-name expression)
  (guarantee-access expression 'ACCESS-NAME)
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

(define-guarantee absolute-reference "SCode absolute reference")

(define (absolute-reference-name reference)
  (guarantee-absolute-reference reference 'ABSOLUTE-REFERENCE-NAME)
  (access-name reference))

(define (absolute-reference-to? object name)
  (and (absolute-reference? object)
       (eq? (absolute-reference-name object) name)))

;;;; Delay

(define (make-delay expression)
  (&typed-singleton-cons (ucode-type delay) expression))

(define (delay? object)
  (object-type? (ucode-type delay) object))

(define-guarantee delay "SCode delay")

(define (delay-expression expression)
  (guarantee-delay expression 'DELAY-EXPRESSION)
  (&singleton-element expression))

(define (delay-components expression receiver)
  (receiver (delay-expression expression)))