#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/usiexp.scm,v 4.7 1990/10/19 22:25:50 cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution)
	 (integrate-external "object"))

;;;; N-ary Arithmetic Predicates

(define (make-combination primitive operands)
  (combination/make (constant/make primitive) operands))

(define (constant-eq? expression constant)
  (and (constant? expression)
       (eq? (constant/value expression) constant)))

(define (unary-arithmetic primitive)
  (lambda (operands if-expanded if-not-expanded block)
    block
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(if-expanded (make-combination primitive operands))
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

(define (pairwise-test binary-predicate if-left-zero if-right-zero)
  (lambda (operands if-expanded if-not-expanded block)
    block
    (if (and (pair? operands)
	     (pair? (cdr operands))
	     (null? (cddr operands)))
	(if-expanded
	 (cond ((constant-eq? (car operands) 0)
		(make-combination if-left-zero (list (cadr operands))))
	       ((constant-eq? (cadr operands) 0)
		(make-combination if-right-zero (list (car operands))))
	       (else
		(make-combination binary-predicate operands))))
	(if-not-expanded))))

(define (pairwise-test-inverse inverse-expansion)
  (lambda (operands if-expanded if-not-expanded block)
    (inverse-expansion operands
      (lambda (expression)
	(if-expanded
	 (make-combination (ucode-primitive not) (list expression))))
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

(define (fix:zero?-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands) (null? (cdr operands)))
      (if-expanded
       (make-combination (ucode-primitive eq?)
			 (list (car operands) (constant/make 0))))
      (if-not-expanded)))

(define (fix:=-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (if-expanded (make-combination (ucode-primitive eq?) operands))
      (if-not-expanded)))

(define char=?-expansion
  fix:=-expansion)

(define (fix:<=-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
		(pair? (cdr operands))
		(null? (cddr operands)))
      (if-expanded
       (make-combination
	(ucode-primitive not)
	(list (make-combination (ucode-primitive greater-than-fixnum?)
				operands))))
      (if-not-expanded)))

(define (fix:>=-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (pair? (cdr operands))
	   (null? (cddr operands)))
      (if-expanded
       (make-combination
	(ucode-primitive not)
	(list (make-combination (ucode-primitive less-than-fixnum?)
				operands))))
      (if-not-expanded)))

;;;; N-ary Arithmetic Field Operations

(define (right-accumulation identity make-binary)
  (lambda (operands if-expanded if-not-expanded block)
    block ; ignored
    (let ((operands (delq identity operands)))
      (let ((n (length operands)))
	(cond ((zero? n)
	       (if-expanded (constant/make identity)))
	      ((< n 5)
	       (if-expanded
		(let loop
		    ((first (car operands))
		     (rest (cdr operands)))
		  (if (null? rest)
		      first
		      (make-binary first
				   (loop (car rest) (cdr rest)))))))
	      (else
	       (if-not-expanded)))))))

(define +-expansion
  (right-accumulation 0
    (lambda (x y)
      (cond ((constant-eq? x 1)
	     (make-combination (ucode-primitive 1+) (list y)))
	    ((constant-eq? y 1)
	     (make-combination (ucode-primitive 1+) (list x)))
	    (else
	     (make-combination (ucode-primitive &+) (list x y)))))))

(define *-expansion
  (right-accumulation 1
    (lambda (x y)
      (make-combination (ucode-primitive &*) (list x y)))))

(define (right-accumulation-inverse identity inverse-expansion make-binary)
  (lambda (operands if-expanded if-not-expanded block)
    (let ((expand
	   (lambda (x y)
	     (if-expanded
	      (if (constant-eq? y identity)
		  x
		  (make-binary x y))))))
      (cond ((null? operands)
	     (if-not-expanded))
	    ((null? (cdr operands))
	     (expand (constant/make identity) (car operands)))
	    (else
	     (inverse-expansion (cdr operands)
	       (lambda (expression)
		 (expand (car operands) expression))
	       if-not-expanded
	       block))))))

(define --expansion
  (right-accumulation-inverse 0 +-expansion
    (lambda (x y)
      (if (constant-eq? y 1)
	  (make-combination (ucode-primitive -1+) (list x))
	  (make-combination (ucode-primitive &-) (list x y))))))

(define /-expansion
  (right-accumulation-inverse 1 *-expansion
    (lambda (x y)
      (make-combination (ucode-primitive &/) (list x y)))))

;;;; N-ary List Operations

(define (apply*-expansion operands if-expanded if-not-expanded block)
  block
  (if (< 1 (length operands) 10)
      (if-expanded
       (make-combination
	(ucode-primitive apply)
	(list (car operands) (cons*-expansion-loop (cdr operands)))))
      (if-not-expanded)))

(define (cons*-expansion operands if-expanded if-not-expanded block)
  block
  (if (< -1 (length operands) 9)
      (if-expanded (cons*-expansion-loop operands))
      (if-not-expanded)))

(define (cons*-expansion-loop rest)
  (if (null? (cdr rest))
      (car rest)
      (make-combination (ucode-primitive cons)
			(list (car rest)
			      (cons*-expansion-loop (cdr rest))))))

(define (list-expansion operands if-expanded if-not-expanded block)
  block ; ignored
  (if (< (length operands) 9)
      (if-expanded (list-expansion-loop operands))
      (if-not-expanded)))

(define (list-expansion-loop rest)
  (if (null? rest)
      (constant/make '())
      (make-combination (ucode-primitive cons)
			(list (car rest)
			      (list-expansion-loop (cdr rest))))))

;;;; General CAR/CDR Encodings

(define (general-car-cdr-expansion encoding)
  (lambda (operands if-expanded if-not-expanded block)
    block
    (if (= (length operands) 1)
	(if-expanded
	 (make-combination (ucode-primitive general-car-cdr)
			   (list (car operands)
				 (constant/make encoding))))
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

(define (make-string-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-combination (ucode-primitive string-allocate) operands))
      (if-not-expanded)))

(define (type-test-expansion type)
  (lambda (operands if-expanded if-not-expanded block)
    block
    (if (and (pair? operands)
	     (null? (cdr operands)))
	(if-expanded (make-type-test type (car operands)))
	(if-not-expanded))))

(define char?-expansion (type-test-expansion (ucode-type character)))
(define vector?-expansion (type-test-expansion (ucode-type vector)))
(define weak-pair?-expansion (type-test-expansion (ucode-type weak-cons)))
(define flo:flonum?-expansion (type-test-expansion (ucode-type big-flonum)))
(define fix:fixnum?-expansion (type-test-expansion (ucode-type fixnum)))

(define (exact-integer?-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	(make-type-test (ucode-type fixnum) (car operands))
	(make-type-test (ucode-type big-fixnum) (car operands))))
      (if-not-expanded)))

(define (exact-rational?-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	(make-type-test (ucode-type fixnum) (car operands))
	(make-type-test (ucode-type big-fixnum) (car operands))
	(make-type-test (ucode-type ratnum) (car operands))))
      (if-not-expanded)))

(define (complex?-expansion operands if-expanded if-not-expanded block)
  block
  (if (and (pair? operands)
	   (null? (cdr operands)))
      (if-expanded
       (make-disjunction
	(make-type-test (ucode-type fixnum) (car operands))
	(make-type-test (ucode-type big-fixnum) (car operands))
	(make-type-test (ucode-type ratnum) (car operands))
	(make-type-test (ucode-type big-flonum) (car operands))
	(make-type-test (ucode-type recnum) (car operands))))
      (if-not-expanded)))

(define (make-disjunction . clauses)
  (let loop ((clauses clauses))
    (if (null? (cdr clauses))
	(car clauses)
	(disjunction/make (car clauses) (loop (cdr clauses))))))
      

(define (make-type-test type operand)
  (make-combination (ucode-primitive object-type?)
		    (list (constant/make type) operand)))

;;;; Tables

(define usual-integrations/expansion-names
  '(
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
    char=?
    char?
    complex?
    cons*
    eighth
    exact-integer?
    exact-rational?
    fifth
    fix:<=
    fix:=
    fix:>=
    fix:fixnum?
    fix:zero?
    flo:flonum?
    fourth
    int:integer?
    list
    make-string
    negative?
    number?
    positive?
    second
    seventh
    sixth
    third
    vector?
    weak-pair?
    zero?
    ))

(define usual-integrations/expansion-values
  (list
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
   char=?-expansion
   char?-expansion
   complex?-expansion
   cons*-expansion
   eighth-expansion
   exact-integer?-expansion
   exact-rational?-expansion
   fifth-expansion
   fix:<=-expansion
   fix:=-expansion
   fix:>=-expansion
   fix:fixnum?-expansion
   fix:zero?-expansion
   flo:flonum?-expansion
   fourth-expansion
   exact-integer?-expansion
   list-expansion
   make-string-expansion
   negative?-expansion
   complex?-expansion
   positive?-expansion
   second-expansion
   seventh-expansion
   sixth-expansion
   third-expansion
   vector?-expansion
   weak-pair?-expansion
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
  (lambda (operands if-expanded if-not-expanded block)
    (scode-expander
     (map cgen/external-with-declarations operands)
     (lambda (scode-expression)
       (if-expanded
	(transform/recursive
	 block
	 (integrate/get-top-level-block)
	 scode-expression)))
     if-not-expanded)))

;;; Kludge for EXPAND-OPERATOR declaration.
(define expander-evaluation-environment
  (the-environment))