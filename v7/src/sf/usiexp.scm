#| -*-Scheme-*-

$Id: usiexp.scm,v 4.22 1993/12/22 07:28:07 adams Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Usual Integrations: Combination Expansions
;; package: (scode-optimizer expansion)

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Fixed-arity arithmetic primitives

(define (make-combination expression block primitive operands)
  (combination/make (and expression
			 (object/scode expression))
		    block
		    (constant/make false primitive)
		    operands))

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

(define (fix:zero?-expansion expr operands if-expanded if-not-expanded block)
  (if (and (pair? operands) (null? (cdr operands)))
      (if-expanded
       (make-combination expr block (ucode-primitive eq?)
			 (list (car operands) (constant/make false 0))))
      (if-not-expanded)))

(define (fix:=-expansion expr operands if-expanded if-not-expanded block)
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (if-expanded
       (make-combination expr block (ucode-primitive eq?) operands))
      (if-not-expanded)))

(define char=?-expansion
  fix:=-expansion)

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

;;;; N-ary Arithmetic Field Operations

(define (right-accumulation identity make-binary)
  (lambda (expr operands if-expanded if-not-expanded block)
    (let ((operands (delq identity operands)))
      (let ((n (length operands)))
	(cond ((zero? n)
	       (if-expanded (constant/make
			     (and expr (object/scode expr))
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
  (right-accumulation 0
    (lambda (expr block x y)
      (cond ((constant-eq? x 1)
	     (make-combination expr block (ucode-primitive 1+) (list y)))
	    ((constant-eq? y 1)
	     (make-combination expr block (ucode-primitive 1+) (list x)))
	    (else
	     (make-combination expr block (ucode-primitive &+) (list x y)))))))

(define *-expansion
  (right-accumulation 1
    (lambda (expr block x y)
      (make-combination expr block (ucode-primitive &*) (list x y)))))

(define (expt-expansion expr operands if-expanded if-not-expanded block)
  (let ((make-binder
	 (lambda (make-body)
	   (if-expanded
	    (combination/make
	     (and expr (object/scode expr))
	     block
	     (let ((block (block/make block #t '()))
		   (name (string->uninterned-symbol "operand")))
	       (let ((variable (variable/make&bind! block name)))
		 (procedure/make
		  #f
		  block lambda-tag:let (list variable) '() #f
		  (make-body block (reference/make false block variable)))))
	     (list (car operands)))))))
    (cond ((not (and (pair? operands)
		     (pair? (cdr operands))
		     (null? (cddr operands))))
	   (if-not-expanded))
	  ;;((constant-eq? (cadr operands) 0)
	  ;; (if-expanded (constant/make (and expr (object/scode expr)) 1)))
	  ((constant-eq? (cadr operands) 1)
	   (if-expanded (car operands)))
	  ((constant-eq? (cadr operands) 2)
	   (make-binder
	    (lambda (block operand)
	      (make-combination #f
				block
				(ucode-primitive &*)
				(list operand operand)))))
	  ((constant-eq? (cadr operands) 3)
	   (make-binder
	    (lambda (block operand)
	      (make-combination
	       #f
	       block
	       (ucode-primitive &*)
	       (list operand
		     (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand)))))))
	  ((constant-eq? (cadr operands) 4)
	   (make-binder
	    (lambda (block operand)
	      (make-combination
	       #f
	       block
	       (ucode-primitive &*)
	       (list (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand))
		     (make-combination #f
				       block
				       (ucode-primitive &*)
				       (list operand operand)))))))
	  (else
	   (if-not-expanded)))))

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
  (right-accumulation-inverse 0 +-expansion
    (lambda (expr block x y)
      (if (constant-eq? y 1)
	  (make-combination expr block (ucode-primitive -1+) (list x))
	  (make-combination expr block (ucode-primitive &-) (list x y))))))

(define /-expansion
  (right-accumulation-inverse 1 *-expansion
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
			      (cons*-expansion-loop false block (cdr rest))))))

(define (list-expansion expr operands if-expanded if-not-expanded block)
  (if (< (length operands) 9)
      (if-expanded (list-expansion-loop expr block operands))
      (if-not-expanded)))

(define (list-expansion-loop expr block rest)
  (if (null? rest)
      (constant/make (and expr (object/scode expr)) '())
      (make-combination expr block (ucode-primitive cons)
			(list (car rest)
			      (list-expansion-loop false block (cdr rest))))))

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

(define second-expansion  cadr-expansion)
(define third-expansion   caddr-expansion)
(define fourth-expansion  cadddr-expansion)
(define fifth-expansion   (general-car-cdr-expansion #b110000))
(define sixth-expansion   (general-car-cdr-expansion #b1100000))
(define seventh-expansion (general-car-cdr-expansion #b11000000))
(define eighth-expansion  (general-car-cdr-expansion #b110000000))

;;;; Miscellaneous

(define (make-string-expansion expr operands if-expanded if-not-expanded block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-combination expr block (ucode-primitive string-allocate)
			 operands))
      (if-not-expanded)))

(define (type-test-expansion type-proc)
  (lambda (expr operands if-expanded if-not-expanded block)
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(if-expanded (make-type-test expr block (type-proc) (car operands)))
	(if-not-expanded))))

;; DO NOT make the following integrable! they are required to be procedures
;; for re-tagging

(define (ucode-type/character) (ucode-type character))
(define (ucode-type/cell) (ucode-type cell))
(define (ucode-type/vector) (ucode-type vector))
(define (ucode-type/record) (ucode-type record))
(define (ucode-type/weak-cons) (ucode-type weak-cons))
(define (ucode-type/big-flonum) (ucode-type big-flonum))
(define (ucode-type/fixnum) (ucode-type fixnum))
(define (ucode-type/positive-fixnum) (ucode-type positive-fixnum))
(define (ucode-type/negative-fixnum) (ucode-type negative-fixnum))

(define char?-expansion (type-test-expansion ucode-type/character))
(define cell?-expansion (type-test-expansion ucode-type/cell))
(define vector?-expansion (type-test-expansion ucode-type/vector))
(define %record?-expansion (type-test-expansion ucode-type/record))
(define weak-pair?-expansion (type-test-expansion ucode-type/weak-cons))
(define flo:flonum?-expansion (type-test-expansion ucode-type/big-flonum))
(define fix:fixnum?-expansion (type-test-expansion ucode-type/fixnum))

;; for +ve & -ve fixnums?
;(define (fix:fixnum?-expansion expr operands if-expanded if-not-expanded block)
;  (let ((pos-tag  (ucode-type/positive-fixnum))
;	(neg-tag  (ucode-type/negative-fixnum)))
;    (if (and (pair? operands)
;	     (null? (cdr operands)))
;	(if-expanded
;	 (if (eq? pos-tag neg-tag)
;	     (make-type-test false block pos-tag (car operands))
;	     (make-disjunction
;	      expr
;	      (make-type-test false block pos-tag (car operands))
;	      (make-type-test false block neg-tag (car operands)))))
;	(if-not-expanded))))

(define (exact-integer?-expansion expr operands if-expanded if-not-expanded
				  block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	expr
	(make-type-test false block (ucode-type/fixnum) (car operands))
	(make-type-test false block (ucode-type/big-fixnum) (car operands))))
      (if-not-expanded)))

(define (exact-rational?-expansion expr operands if-expanded if-not-expanded
				   block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	expr
	(make-type-test false block (ucode-type/fixnum) (car operands))
	(make-type-test false block (ucode-type/big-fixnum) (car operands))
	(make-type-test false block (ucode-type/ratnum) (car operands))))
      (if-not-expanded)))

(define (complex?-expansion expr operands if-expanded if-not-expanded block)
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	expr
	(make-type-test false block (ucode-type/fixnum) (car operands))
	(make-type-test false block (ucode-type/big-fixnum) (car operands))
	(make-type-test false block (ucode-type/ratnum) (car operands))
	(make-type-test false block (ucode-type/big-flonum) (car operands))
	(make-type-test false block (ucode-type/recnum) (car operands))))
      (if-not-expanded)))

(define (make-disjunction expr . clauses)
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
	   (string? (car operands))
	   (null? (cdr operands)))
      (if-expanded
       (constant/make (and expr (object/scode expr))
		      (string->symbol (car operands))))
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

;;;; Tables

(define usual-integrations/expansion-names
  '(
    %record?
    *
    +
    -
    -1+
    /
    1+
    <
    <=
    =
    >
    >=
    apply
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    call-with-values
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cell?
    char=?
    char?
    complex?
    cons*
    eighth
    exact-integer?
    exact-rational?
    expt
    fifth
    fix:<=
    fix:=
    fix:>=
    fix:fixnum?
    fix:zero?
    flo:flonum?
    fourth
    int:->flonum
    int:integer?
    list
    make-string
    ;; modulo	; Compiler does not currently open-code it.
    negative?
    number?
    positive?
    quotient
    remainder
    second
    seventh
    sixth
    string->symbol
    third
    values
    vector?
    weak-pair?
    with-values
    zero?
    ))

(define usual-integrations/expansion-values
  (list
   %record?-expansion
   *-expansion
   +-expansion
   --expansion
   -1+-expansion
   /-expansion
   1+-expansion
   <-expansion
   <=-expansion
   =-expansion
   >-expansion
   >=-expansion
   apply*-expansion
   caaaar-expansion
   caaadr-expansion
   caaar-expansion
   caadar-expansion
   caaddr-expansion
   caadr-expansion
   caar-expansion
   cadaar-expansion
   cadadr-expansion
   cadar-expansion
   caddar-expansion
   cadddr-expansion
   caddr-expansion
   cadr-expansion
   call-with-values-expansion
   cdaaar-expansion
   cdaadr-expansion
   cdaar-expansion
   cdadar-expansion
   cdaddr-expansion
   cdadr-expansion
   cdar-expansion
   cddaar-expansion
   cddadr-expansion
   cddar-expansion
   cdddar-expansion
   cddddr-expansion
   cdddr-expansion
   cddr-expansion
   cell?-expansion
   char=?-expansion
   char?-expansion
   complex?-expansion
   cons*-expansion
   eighth-expansion
   exact-integer?-expansion
   exact-rational?-expansion
   expt-expansion
   fifth-expansion
   fix:<=-expansion
   fix:=-expansion
   fix:>=-expansion
   fix:fixnum?-expansion
   fix:zero?-expansion
   flo:flonum?-expansion
   fourth-expansion
   int:->flonum-expansion
   exact-integer?-expansion
   list-expansion
   make-string-expansion
   ;; modulo-expansion
   negative?-expansion
   complex?-expansion
   positive?-expansion
   quotient-expansion
   remainder-expansion
   second-expansion
   seventh-expansion
   sixth-expansion
   string->symbol-expansion
   third-expansion
   values-expansion
   vector?-expansion
   weak-pair?-expansion
   call-with-values-expansion
   zero?-expansion
   ))

(define usual-integrations/expansion-alist
  (map cons
       usual-integrations/expansion-names
       usual-integrations/expansion-values))

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