#| -*-Scheme-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Register Transfer Language: Expression Operations
;;; package: (compiler)

(declare (usual-integrations))

(define (rtl:invocation? rtl)
  (memq (rtl:expression-type rtl)
	'(INVOCATION:APPLY
	  INVOCATION:JUMP
	  INVOCATION:COMPUTED-JUMP
	  INVOCATION:LEXPR
	  INVOCATION:COMPUTED-LEXPR
	  INVOCATION:PRIMITIVE
	  INVOCATION:SPECIAL-PRIMITIVE
	  INVOCATION:UUO-LINK
	  INVOCATION:GLOBAL-LINK
	  INVOCATION:CACHE-REFERENCE
	  INVOCATION:LOOKUP
	  INVOCATION:REGISTER
	  INVOCATION:PROCEDURE
	  INVOCATION:NEW-APPLY)))

(define (rtl:invocation-prefix? rtl)
  (memq (rtl:expression-type rtl)
	'(INVOCATION-PREFIX:DYNAMIC-LINK
	  INVOCATION-PREFIX:MOVE-FRAME-UP)))

(define (rtl:expression-value-class expression)
  (case (rtl:expression-type expression)
    ((REGISTER)
     (register-value-class (rtl:register-number expression)))
    ((CONS-NON-POINTER CONS-POINTER CONSTANT FIXNUM->OBJECT FLOAT->OBJECT
		       GENERIC-BINARY GENERIC-UNARY OFFSET POST-INCREMENT
		       PRE-INCREMENT)
     value-class=object)
    ((FIXNUM->ADDRESS OBJECT->ADDRESS
		      ASSIGNMENT-CACHE VARIABLE-CACHE
		      OFFSET-ADDRESS
		      FLOAT-OFFSET-ADDRESS
		      BYTE-OFFSET-ADDRESS
		      STATIC-CELL ALIGN-FLOAT)
     value-class=address)
    ((CONS-CLOSURE CONS-MULTICLOSURE ENTRY:CONTINUATION ENTRY:PROCEDURE)
     (if untagged-entries?
	 value-class=object
	 value-class=address))
    ((MACHINE-CONSTANT)
     value-class=immediate)
    ((BYTE-OFFSET CHAR->ASCII)
     value-class=ascii)
    ((OBJECT->DATUM)
     value-class=datum)
    ((ADDRESS->FIXNUM FIXNUM-1-ARG FIXNUM-2-ARGS OBJECT->FIXNUM
		      OBJECT->UNSIGNED-FIXNUM)
     value-class=fixnum)
    ((OBJECT->TYPE)
     value-class=type)
    ((OBJECT->FLOAT FLONUM-1-ARG FLONUM-2-ARGS FLOAT-OFFSET)
     value-class=float)
    ((COERCE-VALUE-CLASS)
     (case (rtl:coerce-value-class-class expression)
       ((ADDRESS)  value-class=address)
       (else       (error "Unknown value class coercion:" expression))))
    (else
     (error "Unknown RTL expression type:" expression))))

(define (rtl:object-valued-expression? expression)
  (value-class=object? (rtl:expression-value-class expression)))

(define (rtl:volatile-expression? expression)
  (memq (rtl:expression-type expression) '(POST-INCREMENT PRE-INCREMENT)))

(define (rtl:machine-register-expression? expression)
  (and (rtl:register? expression)
       (machine-register? (rtl:register-number expression))))

(define (rtl:pseudo-register-expression? expression)
  (and (rtl:register? expression)
       (pseudo-register? (rtl:register-number expression))))

(define (rtl:stack-reference-expression? expression)
  (and (rtl:offset? expression)
       (interpreter-stack-pointer? (rtl:offset-base expression))))

(define (rtl:register-assignment? rtl)
  (and (rtl:assign? rtl)
       (rtl:register? (rtl:assign-address rtl))))

(define (rtl:expression-cost expression)
  (if (rtl:register? expression)
      1
      (or (rtl:constant-cost expression)
	  (let loop ((parts (cdr expression)) (cost 2))
	    (if (null? parts)
		cost
		(loop (cdr parts)
		      (if (pair? (car parts))
			  (+ cost (rtl:expression-cost (car parts)))
			  cost)))))))

(define (rtl:map-subexpressions expression procedure)
  (if (rtl:constant? expression)
      expression
      (cons (car expression)
	    (map (lambda (x)
		   (if (pair? x)
		       (procedure x)
		       x))
		 (cdr expression)))))

(define (rtl:for-each-subexpression expression procedure)
  (if (not (rtl:constant? expression))
      (for-each (lambda (x)
		  (if (pair? x)
		      (procedure x)))
		(cdr expression))))

(define (rtl:any-subexpression? expression predicate)
  (and (not (rtl:constant? expression))
       (there-exists? (cdr expression)
	 (lambda (x)
	   (and (pair? x)
		(predicate x))))))

(define (rtl:expression-contains? expression predicate)
  (let loop ((expression expression))
    (or (predicate expression)
	(rtl:any-subexpression? expression loop))))

(define (rtl:all-subexpressions? expression predicate)
  (or (rtl:constant? expression)
      (for-all? (cdr expression)
	(lambda (x)
	  (or (not (pair? x))
	      (predicate x))))))

(define (rtl:reduce-subparts expression operator initial if-expression if-not)
  (let ((remap
	 (if (rtl:constant? expression)
	     if-not
	     (lambda (x)
	       (if (pair? x)
		   (if-expression x)
		   (if-not x))))))
    (let loop ((parts (cdr expression)) (accum initial))
      (if (null? parts)
	  accum
	  (loop (cdr parts)
		(operator accum (remap (car parts))))))))

(define (rtl:expression=? x y)
  (let ((type (car x)))
    (and (eq? type (car y))
	 (if (eq? type 'CONSTANT)
	     (eqv? (cadr x) (cadr y))
	     (let loop ((x (cdr x)) (y (cdr y)))
	       ;; Because of fixed format, all expressions of same
	       ;; type have the same length, and each entry is either
	       ;; a subexpression or a non-expression.
	       (or (null? x)
		   (and (if (pair? (car x))
			    (rtl:expression=? (car x) (car y))
			    (eqv? (car x) (car y)))
			(loop (cdr x) (cdr y)))))))))

(define (rtl:match-subexpressions x y predicate)
  (let ((type (car x)))
    (and (eq? type (car y))
	 (if (eq? type 'CONSTANT)
	     (eqv? (cadr x) (cadr y))
	     (let loop ((x (cdr x)) (y (cdr y)))
	       (or (null? x)
		   (and (if (pair? (car x))
			    (predicate (car x) (car y))
			    (eqv? (car x) (car y)))
			(loop (cdr x) (cdr y)))))))))

(define (rtl:refers-to-register? rtl register)
  (let loop
      ((expression
	(if (rtl:register-assignment? rtl) (rtl:assign-expression rtl) rtl)))
    (cond ((not (pair? expression)) false)
	  ((rtl:register? expression)
	   (= (rtl:register-number expression) register))
	  ((rtl:contains-no-substitutable-registers? expression) false)
	  (else (there-exists? (cdr expression) loop)))))

(define (rtl:subst-register rtl register substitute)
  (letrec
      ((loop
	(lambda (expression)
	  (cond ((not (pair? expression)) expression)
		((rtl:register? expression)
		 (if (= (rtl:register-number expression) register)
		     substitute
		     expression))
		((rtl:contains-no-substitutable-registers? expression)
		 expression)
		(else (cons (car expression) (map loop (cdr expression))))))))
    (if (rtl:register-assignment? rtl)
	(list (rtl:expression-type rtl)
	      (rtl:assign-address rtl)
	      (loop (rtl:assign-expression rtl)))
	(loop rtl))))

(define (rtl:substitutable-registers rtl)
  (if (rtl:register-assignment? rtl)
      (rtl:substitutable-registers (rtl:assign-expression rtl))
      (let outer ((expression rtl) (registers '()))
	(cond ((not (pair? expression)) registers)
	      ((rtl:register? expression)
	       (let ((register (rtl:register-number expression)))
		 (if (memq register registers)
		     registers
		     (cons register registers))))
	      ((rtl:contains-no-substitutable-registers? expression) registers)
	      (else
	       (let inner
		   ((subexpressions (cdr expression)) (registers registers))
		 (if (null? subexpressions)
		     registers
		     (inner (cdr subexpressions)
			    (outer (car subexpressions) registers)))))))))

(define (rtl:contains-no-substitutable-registers? expression)
  ;; True for all expressions that cannot possibly contain registers.
  ;; In addition, this is also true of expressions that do contain
  ;; registers but are not candidates for substitution (e.g.
  ;; `pre-increment').
  (memq (rtl:expression-type expression)
	'(ASSIGNMENT-CACHE
	  CONS-CLOSURE
	  CONS-MULTICLOSURE
	  CONSTANT
	  ENTRY:CONTINUATION
	  ENTRY:PROCEDURE
	  MACHINE-CONSTANT
	  POST-INCREMENT
	  PRE-INCREMENT
	  VARIABLE-CACHE
	  STATIC-CELL)))

(define (rtl:constant-expression? expression)
  (case (rtl:expression-type expression)
    ((ASSIGNMENT-CACHE
      CONSTANT
      ENTRY:CONTINUATION
      ENTRY:PROCEDURE
      MACHINE-CONSTANT
      VARIABLE-CACHE
      STATIC-CELL)
     true)
    ((BYTE-OFFSET-ADDRESS
      CHAR->ASCII
      CONS-NON-POINTER
      CONS-POINTER
      FIXNUM-1-ARG
      FIXNUM-2-ARGS
      FIXNUM->ADDRESS
      FIXNUM->OBJECT
      FLOAT-OFFSET-ADDRESS
      FLONUM-1-ARG
      FLONUM-2-ARGS
      GENERIC-BINARY
      GENERIC-UNARY
      OBJECT->ADDRESS
      OBJECT->DATUM
      OBJECT->FIXNUM
      OBJECT->TYPE
      OBJECT->UNSIGNED-FIXNUM
      OFFSET-ADDRESS)
     (let loop ((subexpressions (cdr expression)))
       (or (null? subexpressions)
	   (and (let ((expression (car subexpressions)))
		  (or (not (pair? expression))
		      (rtl:constant-expression? expression)))
		(loop (cdr subexpressions))))))
    (else
     false)))

(define (rtx-set/union* set sets)
  (let loop ((set set) (sets sets) (accum '()))
    (let ((set (rtx-set/union set accum)))
      (if (null? sets)
	  set
	  (loop (car sets) (cdr sets) set)))))

(define (rtx-set/union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (let ((x (car x)))
		    (if (there-exists? y
			  (lambda (y)
			    (rtl:expression=? x y)))
			y
			(cons x y))))))))