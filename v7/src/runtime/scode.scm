#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/scode.scm,v 14.11 1991/02/15 18:06:58 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; SCode Abstraction
;;; package: (runtime scode)

(declare (usual-integrations))

(define (initialize-package!)
  (set! scode-constant/type-vector (make-scode-constant/type-vector)))

;;;; Constant

(define scode-constant/type-vector)

(define (scode-constant? object)
  (if (vector-ref scode-constant/type-vector (object-type object))
      true
      (and (compiled-code-address? object)
	   (not (eq? (compiled-entry-type object) 'COMPILED-EXPRESSION)))))

(define (make-scode-constant/type-vector)
  (let ((type-vector (make-vector (microcode-type/code-limit) false)))
    (for-each (lambda (name)
		(vector-set! type-vector (microcode-type name) true))
	      '(BIGNUM
		CHARACTER
		COMPILED-CODE-BLOCK
		CONTROL-POINT
		DELAYED
		ENTITY
		ENVIRONMENT
		EXTENDED-PROCEDURE
		FIXNUM
		FLONUM
		HUNK3-A
		INTERNED-SYMBOL
		NON-MARKED-VECTOR
		NULL
		PAIR
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

;;;; Symbol

(define (symbol? object)
  (or (interned-symbol? object)
      (uninterned-symbol? object)))

(define-integrable (interned-symbol? object)
  (object-type? (ucode-type interned-symbol) object))

(define-integrable (uninterned-symbol? object)
  (object-type? (ucode-type uninterned-symbol) object))

(define (string->uninterned-symbol string)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'STRING->UNINTERNED-SYMBOL))
  (&typed-pair-cons (ucode-type uninterned-symbol)
		    string
		    (make-unbound-reference-trap)))

(define-integrable string->symbol
  (ucode-primitive string->symbol))

(define-integrable (intern string)
  (string->symbol (string-downcase string)))

(define (symbol-name symbol)
  (if (not (symbol? symbol))
      (error:wrong-type-argument symbol "symbol" 'SYMBOL-NAME))
  (system-pair-car symbol))

(define-integrable (symbol->string symbol)
  (string-copy (symbol-name symbol)))

(define (symbol-append . symbols)
  (let ((string (apply string-append (map symbol-name symbols))))
    (string-downcase! string)
    (string->symbol string)))

(define-integrable (symbol-hash symbol)
  (string-hash (symbol-name symbol)))

(define-integrable (symbol-hash-mod symbol modulus)
  (string-hash-mod (symbol-name symbol) modulus))

;;;; Variable

(define-integrable (make-variable name)
  (system-hunk3-cons (ucode-type variable) name true '()))

(define-integrable (variable? object)
  (object-type? (ucode-type variable) object))

(define-integrable (variable-name variable)
  (system-hunk3-cxr0 variable))

(define-integrable (variable-components variable receiver)
  (receiver (variable-name variable)))

;;;; Definition/Assignment

(define (make-definition name #!optional value)
  (&typed-pair-cons (ucode-type definition)
		    name
		    (if (default-object? value)
			(make-unassigned-reference-trap)
			value)))

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

(define (make-assignment-from-variable variable #!optional value)
  (&typed-pair-cons (ucode-type assignment)
		    variable
		    (if (default-object? value)
			(make-unassigned-reference-trap)
			value)))

(define-integrable (assignment-variable assignment)
  (system-pair-car assignment))

(define-integrable (assignment-value assignment)
  (&pair-cdr assignment))

(define (assignment-components-with-variable assignment receiver)
  (receiver (assignment-variable assignment)
	    (assignment-value assignment)))

(define (make-assignment name #!optional value)
  (make-assignment-from-variable (make-variable name)
				 (if (default-object? value)
				     (make-unassigned-reference-trap)
				     value)))

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
  (string->symbol "#[declaration]"))

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
    (if (null? rest)
	reference
	(loop (make-access reference (car rest)) (cdr rest)))))

(define (absolute-reference? object)
  (and (access? object)
       (system-global-environment? (access-environment object))))

(define-integrable (absolute-reference-name reference)
  (access-name reference))

(define (absolute-reference-to? object name)
  (and (absolute-reference? object)
       (eq? (absolute-reference-name object) name)))

;;;; In-Package

(define-integrable (make-in-package environment expression)
  (&typed-pair-cons (ucode-type in-package) environment expression))

(define-integrable (in-package? object)
  (object-type? (ucode-type in-package) object))

(define-integrable (in-package-environment expression)
  (&pair-car expression))

(define-integrable (in-package-expression expression)
  (&pair-cdr expression))

(define (in-package-components expression receiver)
  (receiver (in-package-environment expression)
	    (in-package-expression expression)))

;;;; Delay

(define-integrable (make-delay expression)
  (&typed-singleton-cons (ucode-type delay) expression))

(define-integrable (delay? object)
  (object-type? (ucode-type delay) object))

(define-integrable (delay-expression expression)
  (&singleton-element expression))

(define-integrable (delay-components expression receiver)
  (receiver (delay-expression expression)))