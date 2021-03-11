#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Usual Integrations: Combination Expansions
;;; package: (scode-optimizer expansion)

(declare (usual-integrations)
	 (integrate-external "object"))

(define (usual-integrations/make-expansion-alist)

  ;; This procedure is huge.
  ;; At the bottom it returns a list of expansions


  ;;;; Fixed-arity arithmetic primitives

  (define (make-combination expression block primitive operands)
    (combination/make (and expression (object/scode expression))
		      block
		      (constant/make false primitive)
		      operands))

  (define (make-operand-binding expression block operand make-body)
    (combination/make (and expression (object/scode expression))
		      block
		      (let ((block (block/make block #t '()))
			    (name (string->uninterned-symbol "operand")))
			(let ((variable (variable/make&bind! block name)))
			  (procedure/make
			   #f
			   block lambda-tag:let (list variable) '() #f
			   (make-body block
				      (reference/make #f block variable)))))
		      (list operand)))

  (define (constant-eq? expression constant)
    (and (constant? expression)
	 (eq? (constant/value expression) constant)))

  (define (unary-arithmetic primitive)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (and (pair? operands)
	       (null? (cdr operands)))
	  (if-expanded (make-combination expr block primitive operands))
	  (if-not-expanded))))

  (define (binary-arithmetic primitive)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (and (pair? operands)
	       (pair? (cdr operands))
	       (null? (cddr operands)))
	  (if-expanded (make-combination expr block primitive operands))
	  (if-not-expanded))))

  (define zero?-expansion
    (unary-arithmetic (ucode-primitive zero?)))

  (define positive?-expansion
    (unary-arithmetic (ucode-primitive positive?)))

  (define negative?-expansion
    (unary-arithmetic (ucode-primitive negative?)))

  (define 1+-expansion
    (unary-arithmetic (ucode-primitive 1+)))

  (define -1+-expansion
    (unary-arithmetic (ucode-primitive -1+)))

  (define quotient-expansion
    (binary-arithmetic (ucode-primitive quotient 2)))

  (define remainder-expansion
    (binary-arithmetic (ucode-primitive remainder 2)))

  (define modulo-expansion
    (binary-arithmetic (ucode-primitive modulo 2)))

  ;;;; N-ary Arithmetic Predicates

  (define (pairwise-test binary-predicate if-left-zero if-right-zero)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (and (pair? operands)
	       (pair? (cdr operands))
	       (null? (cddr operands)))
	  (if-expanded
	   (cond ((constant-eq? (car operands) 0)
		  (make-combination expr block if-left-zero
				    (list (cadr operands))))
		 ((constant-eq? (cadr operands) 0)
		  (make-combination expr block if-right-zero
				    (list (car operands))))
		 (else
		  (make-combination expr block binary-predicate operands))))
	  (if-not-expanded))))

  (define (pairwise-test-inverse inverse-expansion)
    (lambda (expr operands if-expanded if-not-expanded block)
      (inverse-expansion
       expr operands
       (lambda (expression)
	 (if-expanded
	  (make-combination expr block (ucode-primitive not)
			    (list expression))))
       if-not-expanded
       block)))

  (define =-expansion
    (pairwise-test (ucode-primitive &=)
		   (ucode-primitive zero?)
		   (ucode-primitive zero?)))

  (define <-expansion
    (pairwise-test (ucode-primitive &<)
		   (ucode-primitive positive?)
		   (ucode-primitive negative?)))

  (define >-expansion
    (pairwise-test (ucode-primitive &>)
		   (ucode-primitive negative?)
		   (ucode-primitive positive?)))

  (define <=-expansion (pairwise-test-inverse >-expansion))
  (define >=-expansion (pairwise-test-inverse <-expansion))

  ;;;; Fixnum Operations

  (define (fix:<=-expansion expr operands if-expanded if-not-expanded block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(if-expanded
	 (make-combination
	  expr
	  block
	  (ucode-primitive not)
	  (list (make-combination false
				  block
				  (ucode-primitive greater-than-fixnum?)
				  operands))))
	(if-not-expanded)))

  (define (fix:>=-expansion expr operands if-expanded if-not-expanded block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(if-expanded
	 (make-combination
	  expr
	  block
	  (ucode-primitive not)
	  (list (make-combination false
				  block
				  (ucode-primitive less-than-fixnum?)
				  operands))))
	(if-not-expanded)))

  (define char=?-expansion
    (binary-arithmetic (ucode-primitive eq?)))

  ;;;; N-ary Arithmetic Field Operations

  (define (right-accumulation identity make-binary)
    (lambda (expr operands if-expanded if-not-expanded block)
      (let ((operands (delq identity operands)))
	(let ((n (length operands)))
	  (cond ((zero? n)
		 (if-expanded (constant/make
			       (and expr (object/scode expr));;?
			       identity)))
		((< n 5)
		 (if-expanded
		  (let loop
		      ((expr expr)
		       (first (car operands))
		       (rest (cdr operands)))
		    (if (null? rest)
			first
			(make-binary expr
				     block
				     first
				     (loop false (car rest) (cdr rest)))))))
		(else
		 (if-not-expanded)))))))

  (define +-expansion
    (right-accumulation 
     0
     (lambda (expr block x y)
       (cond ((constant-eq? x 1)
	      (make-combination expr block (ucode-primitive 1+) (list y)))
	     ((constant-eq? y 1)
	      (make-combination expr block (ucode-primitive 1+) (list x)))
	     (else
	      (make-combination expr block (ucode-primitive &+)
				(list x y)))))))

  (define *-expansion
    (right-accumulation 
     1
     (lambda (expr block x y)
       (make-combination expr block (ucode-primitive &*) (list x y)))))

  (define (right-accumulation-inverse identity inverse-expansion make-binary)
    (lambda (expr operands if-expanded if-not-expanded block)
      (let ((expand
	     (lambda (expr x y)
	       (if-expanded
		(if (constant-eq? y identity)
		    x
		    (make-binary expr block x y))))))
	(cond ((null? operands)
	       (if-not-expanded))
	      ((null? (cdr operands))
	       (expand expr (constant/make false identity) (car operands)))
	      (else
	       (inverse-expansion false (cdr operands)
				  (lambda (expression)
				    (expand expr (car operands) expression))
				  if-not-expanded
				  block))))))

  (define --expansion
    (right-accumulation-inverse
     0
     +-expansion
     (lambda (expr block x y)
       (if (constant-eq? y 1)
	   (make-combination expr block (ucode-primitive -1+) (list x))
	   (make-combination expr block (ucode-primitive &-) (list x y))))))

  (define /-expansion
    (right-accumulation-inverse
     1
     *-expansion
     (lambda (expr block x y)
       (make-combination expr block (ucode-primitive &/) (list x y)))))

  ;;;; N-ary List Operations

  (define (apply*-expansion expr operands if-expanded if-not-expanded block)
    (if (< 1 (length operands) 10)
	(if-expanded
	 (combination/make
	  (and expr (object/scode expr))
	  block
	  (global-ref/make 'APPLY)
	  (list (car operands)
		(cons*-expansion-loop false block (cdr operands)))))
	(if-not-expanded)))

  (define (cons*-expansion expr operands if-expanded if-not-expanded block)
    (if (< -1 (length operands) 9)
	(if-expanded (cons*-expansion-loop expr block operands))
	(if-not-expanded)))

  (define (cons*-expansion-loop expr block rest)
    (if (null? (cdr rest))
	(car rest)
	(make-combination expr
			  block
			  (ucode-primitive cons)
			  (list (car rest)
				(cons*-expansion-loop #f block (cdr rest))))))

  (define (list-expansion expr operands if-expanded if-not-expanded block)
    (if (< (length operands) 9)
	(if-expanded (list-expansion-loop expr block operands))
	(if-not-expanded)))

  (define (list-expansion-loop expr block rest)
    (if (null? rest)
	(constant/make (and expr (object/scode expr)) '())
	(make-combination expr block (ucode-primitive cons)
			  (list (car rest)
				(list-expansion-loop #f block (cdr rest))))))

  (define (values-expansion expr operands if-expanded if-not-expanded block)
    if-not-expanded
    (if-expanded
     (let ((block (block/make block true '())))
       (let ((variables
	      (map (lambda (operand)
		     operand
		     (variable/make&bind! block
					  (string->uninterned-symbol "value")))
		   operands)))
	 (combination/make
	  (and expr (object/scode expr))
	  block
	  (procedure/make
	   false
	   block lambda-tag:let variables '() false
	   (let ((block (block/make block true '())))
	     (let ((variable (variable/make&bind! block 'RECEIVER)))
	       (procedure/make
		false block lambda-tag:unnamed (list variable) '() false
		(combination/make false
				  block
				  (reference/make false block variable)
				  (map (lambda (variable)
					 (reference/make false block variable))
				       variables))))))
	  operands)))))

  (define (call-with-values-expansion expr operands
				      if-expanded if-not-expanded block)
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(if-expanded
	 (combination/make (and expr (object/scode expr))
			   block
			   (combination/make false block (car operands) '())
			   (cdr operands)))
	(if-not-expanded)))

  ;;;; General CAR/CDR Encodings

  (define (general-car-cdr-expansion encoding)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (= (length operands) 1)
	  (if-expanded
	   (make-combination expr
			     block
			     (ucode-primitive general-car-cdr)
			     (list (car operands)
				   (constant/make false encoding))))
	  (if-not-expanded))))

  (define caar-expansion (general-car-cdr-expansion #b111))
  (define cadr-expansion (general-car-cdr-expansion #b110))
  (define cdar-expansion (general-car-cdr-expansion #b101))
  (define cddr-expansion (general-car-cdr-expansion #b100))

  (define caaar-expansion (general-car-cdr-expansion #b1111))
  (define caadr-expansion (general-car-cdr-expansion #b1110))
  (define cadar-expansion (general-car-cdr-expansion #b1101))
  (define caddr-expansion (general-car-cdr-expansion #b1100))
  (define cdaar-expansion (general-car-cdr-expansion #b1011))
  (define cdadr-expansion (general-car-cdr-expansion #b1010))
  (define cddar-expansion (general-car-cdr-expansion #b1001))
  (define cdddr-expansion (general-car-cdr-expansion #b1000))

  (define caaaar-expansion (general-car-cdr-expansion #b11111))
  (define caaadr-expansion (general-car-cdr-expansion #b11110))
  (define caadar-expansion (general-car-cdr-expansion #b11101))
  (define caaddr-expansion (general-car-cdr-expansion #b11100))
  (define cadaar-expansion (general-car-cdr-expansion #b11011))
  (define cadadr-expansion (general-car-cdr-expansion #b11010))
  (define caddar-expansion (general-car-cdr-expansion #b11001))
  (define cadddr-expansion (general-car-cdr-expansion #b11000))
  (define cdaaar-expansion (general-car-cdr-expansion #b10111))
  (define cdaadr-expansion (general-car-cdr-expansion #b10110))
  (define cdadar-expansion (general-car-cdr-expansion #b10101))
  (define cdaddr-expansion (general-car-cdr-expansion #b10100))
  (define cddaar-expansion (general-car-cdr-expansion #b10011))
  (define cddadr-expansion (general-car-cdr-expansion #b10010))
  (define cdddar-expansion (general-car-cdr-expansion #b10001))
  (define cddddr-expansion (general-car-cdr-expansion #b10000))

  (define first-expansion   (general-car-cdr-expansion #b11))
  (define second-expansion  cadr-expansion)
  (define third-expansion   caddr-expansion)
  (define fourth-expansion  cadddr-expansion)
  (define fifth-expansion   (general-car-cdr-expansion #b110000))
  (define sixth-expansion   (general-car-cdr-expansion #b1100000))
  (define seventh-expansion (general-car-cdr-expansion #b11000000))
  (define eighth-expansion  (general-car-cdr-expansion #b110000000))

  ;;;; Miscellaneous

  (define (make-string-expansion expr operands if-expanded if-not-expanded
				 block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(if-expanded
	 (make-combination expr block (ucode-primitive string-allocate)
			   operands))
	(if-not-expanded)))

  (define (type-test-expansion type)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (and (pair? operands)
	       (null? (cdr operands)))
	  (if-expanded (make-type-test expr block type (car operands)))
	  (if-not-expanded))))

  (define (disjunction-type-test-expansion get-the-types)
    (lambda (expr operands if-expanded if-not-expanded block)
      (if (and (pair? operands)
	       (null? (cdr operands)))
	  (if-expanded
	   (if (null? (cdr get-the-types))
	       (make-type-test #f block (car get-the-types) (car operands))
	       (make-operand-binding expr block (car operands)
		 (lambda (block operand)
		   (make-disjunction
		    expr
		    (map (lambda (type)
			   (make-type-test #f block type operand))
			 get-the-types))))))
	  (if-not-expanded))))

  (define weak-pair?-expansion
    (type-test-expansion (cross-sf/ucode-type 'weak-cons)))

  (define fixnum-ucode-types
    (let ((-ve  (cross-sf/ucode-type 'negative-fixnum))
	  (+0ve (cross-sf/ucode-type 'positive-fixnum)))
      (if (= -ve +0ve)
	  (list +0ve)
	  (list +0ve -ve))))

  (define exact-integer?-expansion
    (disjunction-type-test-expansion
     (append fixnum-ucode-types (list (cross-sf/ucode-type 'big-fixnum)))))

  (define exact-rational?-expansion
    (disjunction-type-test-expansion
     (append fixnum-ucode-types (list (cross-sf/ucode-type 'big-fixnum)
				      (cross-sf/ucode-type 'ratnum)))))

  (define complex?-expansion
    (disjunction-type-test-expansion 
     (append fixnum-ucode-types (list (cross-sf/ucode-type 'big-fixnum) 
				      (cross-sf/ucode-type 'ratnum)
				      (cross-sf/ucode-type 'big-flonum)
				      (cross-sf/ucode-type 'recnum)))))

  (define symbol?-expansion
    (disjunction-type-test-expansion
     (list (cross-sf/ucode-type 'interned-symbol)
	   (cross-sf/ucode-type 'uninterned-symbol))))

  (define (make-disjunction expr clauses)
    (let loop ((clauses clauses))
      (if (null? (cdr clauses))
	  (car clauses)
	  (disjunction/make (and expr (object/scode expr))
			    (car clauses) (loop (cdr clauses))))))
      
  (define (make-type-test expr block type operand)
    (make-combination expr block
		      (ucode-primitive object-type?)
		      (list (constant/make false type) operand)))

  (define (string->symbol-expansion expr operands if-expanded if-not-expanded
				    block)
    block
    (if (and (pair? operands)
	     (constant? (car operands))
	     (string? (constant/value (car operands)))
	     (null? (cdr operands)))
	(if-expanded
	 (constant/make (and expr (object/scode expr))
			(string->symbol (constant/value (car operands)))))
	(if-not-expanded)))

  (define (int:->flonum-expansion expr operands if-expanded if-not-expanded
				  block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(if-expanded
	 (make-combination expr
			   block
			   (ucode-primitive integer->flonum 2)
			   (list (car operands) (constant/make #f #b10))))
	(if-not-expanded)))

  (define (global-operator name #!optional min-arity max-arity)
    (let ((min-arity (if (default-object? min-arity) 0 min-arity))
	  (max-arity (if (default-object? max-arity)
			 (if (default-object? min-arity)
			     #F
			     min-arity)
			 #F)))
      (lambda (expr operands if-expanded if-not-expanded block)
	(let ((operand-count (length operands)))
	  (if (and (<= min-arity operand-count)
		   (or (not max-arity) (<= operand-count max-arity)))
	      (if-expanded
	       (combination/make
		(and expr (object/scode expr))
		block
		(global-ref/make name)
		operands))
	      (if-not-expanded))))))

  (define (make-global-operator spec)
    (if (symbol? spec)
	(let ((arity
	       (procedure-arity
		(environment-lookup system-global-environment spec))))
	  `(,spec . ,(global-operator spec (car arity) (cdr arity))))
	`(,(car spec) . ,(apply global-operator spec))))

  (define usual-integrations/expansion-alist
    `((*                  . ,*-expansion)
      (+                  . ,+-expansion)
      (-                  . ,--expansion)
      (-1+                . ,-1+-expansion)
      (/                  . ,/-expansion)
      (1+                 . ,1+-expansion)
      (<                  . ,<-expansion)
      (<=                 . ,<=-expansion)
      (=                  . ,=-expansion)
      (>                  . ,>-expansion)
      (>=                 . ,>=-expansion)
      (apply*             . ,apply*-expansion)
      (caaaar             . ,caaaar-expansion)
      (caaadr             . ,caaadr-expansion)
      (caaar              . ,caaar-expansion)
      (caadar             . ,caadar-expansion)
      (caaddr             . ,caaddr-expansion)
      (caadr              . ,caadr-expansion)
      (caar               . ,caar-expansion)
      (cadaar             . ,cadaar-expansion)
      (cadadr             . ,cadadr-expansion)
      (cadar              . ,cadar-expansion)
      (caddar             . ,caddar-expansion)
      (cadddr             . ,cadddr-expansion)
      (caddr              . ,caddr-expansion)
      (cadr               . ,cadr-expansion)
      (call-with-values   . ,call-with-values-expansion)
      (cdaaar             . ,cdaaar-expansion)
      (cdaadr             . ,cdaadr-expansion)
      (cdaar              . ,cdaar-expansion)
      (cdadar             . ,cdadar-expansion)
      (cdaddr             . ,cdaddr-expansion)
      (cdadr              . ,cdadr-expansion)
      (cdar               . ,cdar-expansion)
      (cddaar             . ,cddaar-expansion)
      (cddadr             . ,cddadr-expansion)
      (cddar              . ,cddar-expansion)
      (cdddar             . ,cdddar-expansion)
      (cddddr             . ,cddddr-expansion)
      (cdddr              . ,cdddr-expansion)
      (cddr               . ,cddr-expansion)
      (char=?             . ,char=?-expansion)
      (complex?           . ,complex?-expansion)
      (cons*              . ,cons*-expansion)
      (eighth             . ,eighth-expansion)
      (exact-integer?     . ,exact-integer?-expansion)
      (exact-rational?    . ,exact-rational?-expansion)
      (fifth              . ,fifth-expansion)
      (first              . ,first-expansion)
      (fix:<=             . ,fix:<=-expansion)
      (fix:>=             . ,fix:>=-expansion)
      (fourth             . ,fourth-expansion)
      (int:->flonum       . ,int:->flonum-expansion)
      (int:integer?       . ,exact-integer?-expansion)
      (list               . ,list-expansion)
      (make-string        . ,make-string-expansion)
      ;;(modulo           . ,modulo-expansion)
      (negative?          . ,negative?-expansion)
      (complex?           . ,complex?-expansion)
      (positive?          . ,positive?-expansion)
      (quotient           . ,quotient-expansion)
      (remainder          . ,remainder-expansion)
      (second             . ,second-expansion)
      (seventh            . ,seventh-expansion)
      (sixth              . ,sixth-expansion)
      ;;(string->symbol     . ,string->symbol-expansion)
      (symbol?            . ,symbol?-expansion)
      (third              . ,third-expansion)
      (values             . ,values-expansion)
      (weak-pair?         . ,weak-pair?-expansion)
      (with-values        . ,call-with-values-expansion)
      (zero?              . ,zero?-expansion)
      ,@(map make-global-operator usual-integrations/global-operators)
      ))

  usual-integrations/expansion-alist)

(define usual-integrations/expansion-alist)

(define (usual-integrations/initialize-expanders!)
  (set! usual-integrations/expansion-alist
	(usual-integrations/make-expansion-alist))
  unspecific)

(define usual-integrations/global-operators
  '(;; <name>: use binding in system-global-environment to obtain arity
    ;; (<name> #!optional min-arity max-arity): as specified (for use for
    ;; names that might not be bound when SF is loaded)
    ABS
    ACOS
    ASIN
    ATAN
    CEILING
    CEILING->EXACT
    COS
    EQUAL?
    EQV?
    ERROR
    ERROR:BAD-RANGE-ARGUMENT
    ERROR:WRONG-TYPE-ARGUMENT
    ERROR:WRONG-TYPE-DATUM
    EXACT->INEXACT
    EXP
    EXPT
    FLOOR
    FLOOR->EXACT
    FOR-EACH
    INEXACT->EXACT
    INTERN
    LIST-REF
    LOG
    MAKE-STRING
    MAKE-VECTOR
    MAP
    MEMQ
    ROUND
    ROUND->EXACT
    SIN
    SQRT
    STRING
    STRING-APPEND
    STRING-COPY
    STRING->SYMBOL
    (SYMBOL-NAME 1)
    TAN
    TRUNCATE
    TRUNCATE->EXACT
    ))

;;;;  Hooks and utilities for user defined reductions and expanders

;;; User defined reductions appear in reduct.scm

;;; Scode->Scode expanders

(define (scode->scode-expander scode-expander)
  (lambda (expr operands if-expanded if-not-expanded block)
    (scode-expander
     (map cgen/external-with-declarations operands)
     (lambda (scode-expression)
       (if-expanded
	(reassign
	 expr
	 (transform/recursive
	  block
	  (integrate/get-top-level-block)
	  scode-expression))))
     if-not-expanded)))

;;; Kludge for EXPAND-OPERATOR declaration.
(define expander-evaluation-environment
  (the-environment))