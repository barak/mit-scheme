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

;;;; SCODE Grab Bag

(declare (usual-integrations))

;;;; Constants

(define scode-constant?
  (let ((type-vector (make-vector number-of-microcode-types #!FALSE)))
    (for-each (lambda (name)
		(vector-set! type-vector (microcode-type name) #!TRUE))
	      '(NULL TRUE UNASSIGNED
		     FIXNUM BIGNUM FLONUM
		     CHARACTER STRING UNINTERNED-SYMBOL INTERNED-SYMBOL
		     NON-MARKED-VECTOR VECTOR-1B VECTOR-16B
		     PAIR TRIPLE VECTOR QUOTATION PRIMITIVE))
    (named-lambda (scode-constant? object)
      (vector-ref type-vector (primitive-type object)))))

(define make-null)
(define make-false)
(define make-true)

(let ()
  (define (make-constant-maker name)
    (let ((type (microcode-type name)))
      (lambda ()
	(primitive-set-type type 0))))
  (set! make-null (make-constant-maker 'NULL))
  (set! make-false (make-constant-maker 'FALSE))
  (set! make-true (make-constant-maker 'TRUE)))

;;;; QUOTATION

(define quotation?)
(define make-quotation)

(let ((type (microcode-type 'QUOTATION)))
  (set! quotation?
	(named-lambda (quotation? object)
	  (primitive-type? type object)))
  (set! make-quotation
	(named-lambda (make-quotation expression)
	  (&typed-singleton-cons type expression))))

(define quotation-expression &singleton-element)

;;;; SYMBOL

(define symbol?)
(define string->uninterned-symbol)
(let ()

(define utype
  (microcode-type 'UNINTERNED-SYMBOL))

(define itype
  (microcode-type 'INTERNED-SYMBOL))

(set! symbol?
(named-lambda (symbol? object)
  (or (primitive-type? itype object)
      (primitive-type? utype object))))

(set! string->uninterned-symbol
(named-lambda (string->uninterned-symbol string)
  (&typed-pair-cons utype
		    string
		    (make-unbound-object))))

)

(define string->symbol
  (make-primitive-procedure 'STRING->SYMBOL))

(define (symbol->string symbol)
  (make-object-safe (&pair-car symbol)))

(define make-symbol string->uninterned-symbol)
(define make-interned-symbol string->symbol)
(define symbol-print-name symbol->string)

(define (symbol-global-value symbol)
  (make-object-safe (&pair-cdr symbol)))

(define (set-symbol-global-value! symbol value)
  (&pair-set-cdr! symbol
		  ((if (object-dangerous? (&pair-cdr symbol))
		       make-object-dangerous
		       make-object-safe)
		   value)))

(define (make-named-tag name)
  (string->symbol (string-append "#[" name "]")))

;;;; VARIABLE

(define variable?)
(define make-variable)

(let ((type (microcode-type 'VARIABLE)))
  (set! variable?
	(named-lambda (variable? object)
	  (primitive-type? type object)))
  (set! make-variable
	(named-lambda (make-variable name)
	  (system-hunk3-cons type name (make-true) (make-null)))))

(define variable-name system-hunk3-cxr0)

(define (variable-components variable receiver)
  (receiver (variable-name variable)))

;;;; DEFINITION

(define definition?)
(define make-definition)

(let ((type (microcode-type 'DEFINITION)))
  (set! definition?
	(named-lambda (definition? object)
	  (primitive-type? type object)))
  (set! make-definition
	(named-lambda (make-definition name value)
	  (&typed-pair-cons type name value))))

(define (definition-components definition receiver)
  (receiver (definition-name definition)
	    (definition-value definition)))

(define definition-name system-pair-car)
(define set-definition-name! system-pair-set-car!)
(define definition-value &pair-cdr)
(define set-definition-value! &pair-set-cdr!)

;;;; ASSIGNMENT

(define assignment?)
(define make-assignment-from-variable)

(let ((type (microcode-type 'ASSIGNMENT)))
  (set! assignment?
	(named-lambda (assignment? object)
	  (primitive-type? type object)))
  (set! make-assignment-from-variable
	(named-lambda (make-assignment-from-variable variable value)
	  (&typed-pair-cons type variable value))))

(define (assignment-components-with-variable assignment receiver)
  (receiver (assignment-variable assignment)
	    (assignment-value assignment)))

(define assignment-variable system-pair-car)
(define set-assignment-variable! system-pair-set-car!)
(define assignment-value &pair-cdr)
(define set-assignment-value! &pair-set-cdr!)

(define (make-assignment name value)
  (make-assignment-from-variable (make-variable name) value))

(define (assignment-components assignment receiver)
  (assignment-components-with-variable assignment
    (lambda (variable value)
      (receiver (variable-name variable) value))))

(define (assignment-name assignment)
  (variable-name (assignment-variable assignment)))

;;;; COMMENT

(define comment?)
(define make-comment)

(let ((type (microcode-type 'COMMENT)))
  (set! comment?
	(named-lambda (comment? object)
	  (primitive-type? type object)))
  (set! make-comment
	(named-lambda (make-comment text expression)
	  (&typed-pair-cons type expression text))))

(define (comment-components comment receiver)
  (receiver (comment-text comment)
	    (comment-expression comment)))

(define comment-text &pair-cdr)
(define set-comment-text! &pair-set-cdr!)
(define comment-expression &pair-car)
(define set-comment-expression! &pair-set-car!)

;;;; DECLARATION

(define declaration?)
(define make-declaration)

(let ((tag (make-named-tag "DECLARATION")))
  (set! declaration?
	(named-lambda (declaration? object)
	  (and (comment? object)
	       (let ((text (comment-text object)))
		 (and (pair? text)
		      (eq? (car text) tag))))))
  (set! make-declaration
	(named-lambda (make-declaration text expression)
	  (make-comment (cons tag text) expression))))

(define (declaration-components declaration receiver)
  (comment-components declaration
    (lambda (text expression)
      (receiver (cdr text) expression))))

(define (declaration-text tagged-comment)
  (cdr (comment-text tagged-comment)))

(define (set-declaration-text! tagged-comment new-text)
  (set-cdr! (comment-text tagged-comment) new-text))

(define declaration-expression
  comment-expression)

(define set-declaration-expression!
  set-comment-expression!)

(define make-block-declaration)
(define block-declaration?)
(let ()

(define tag
  (make-named-tag "Block Declaration"))

(set! make-block-declaration
(named-lambda (make-block-declaration text)
  (cons tag text)))

(set! block-declaration?
(named-lambda (block-declaration? object)
  (and (pair? object) (eq? (car object) tag))))

)

(define block-declaration-text
  cdr)

;;;; THE-ENVIRONMENT

(define the-environment?)
(define make-the-environment)

(let ((type (microcode-type 'THE-ENVIRONMENT)))
  (set! the-environment?
	(named-lambda (the-environment? object)
	  (primitive-type? type object)))
  (set! make-the-environment
	(named-lambda (make-the-environment)
	  (primitive-set-type type 0))))

;;;; ACCESS

(define access?)
(define make-access)

(let ((type (microcode-type 'ACCESS)))
  (set! access?
	(named-lambda (access? object)
	  (primitive-type? type object)))
  (set! make-access
	(named-lambda (make-access environment name)
	  (&typed-pair-cons type environment name))))

(define (access-components access receiver)
  (receiver (access-environment access)
	    (access-name access)))

(define access-environment &pair-car)
(define access-name system-pair-cdr)

;;;; IN-PACKAGE

(define in-package?)
(define make-in-package)

(let ((type (microcode-type 'IN-PACKAGE)))
  (set! in-package?
	(named-lambda (in-package? object)
	  (primitive-type? type object)))
  (set! make-in-package
	(named-lambda (make-in-package environment expression)
	  (&typed-pair-cons type environment expression))))

(define (in-package-components in-package receiver)
  (receiver (in-package-environment in-package)
	    (in-package-expression in-package)))

(define in-package-environment &pair-car)
(define in-package-expression &pair-cdr)

;;;; DELAY

(define delay?)
(define make-delay)

(let ((type (microcode-type 'DELAY)))
  (set! delay?
	(named-lambda (delay? object)
	  (primitive-type? type object)))
  (set! make-delay
	(named-lambda (make-delay expression)
	  (&typed-singleton-cons type expression))))

(define delay-expression &singleton-element)

(define (delay-components delay receiver)
  (receiver (delay-expression delay)))
  (receiver (delay-expression delay)))