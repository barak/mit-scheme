#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/usiexp.scm,v 3.8 1988/05/11 04:19:27 jinx Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

(define (pairwise-test binary-predicate if-left-zero if-right-zero)
  (lambda (operands if-expanded if-not-expanded block)
    block ; ignored
    (cond ((or (null? operands)
	       (null? (cdr operands)))
	   (error "Too few operands" operands))
	  ((null? (cddr operands))
	   (if-expanded
	    (cond ((constant-eq? (car operands) 0)
		   (make-combination if-left-zero (list (cadr operands))))
		  ((constant-eq? (cadr operands) 0)
		   (make-combination if-right-zero (list (car operands))))
		  (else
		   (make-combination binary-predicate operands)))))
	  (else
	   (if-not-expanded)))))

(define (pairwise-test-inverse inverse-expansion)
  (lambda (operands if-expanded if-not-expanded block)
    (inverse-expansion operands
      (lambda (expression)
	(if-expanded (make-combination not (list expression))))
      if-not-expanded
      block)))

(define =-expansion
  (pairwise-test (make-primitive-procedure '&=) zero? zero?))

(define <-expansion
  (pairwise-test (make-primitive-procedure '&<) positive? negative?))

(define >-expansion
  (pairwise-test (make-primitive-procedure '&>) negative? positive?))

(define <=-expansion
  (pairwise-test-inverse >-expansion))

(define >=-expansion
  (pairwise-test-inverse <-expansion))

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
    (let ((&+ (make-primitive-procedure '&+)))
      (lambda (x y)
	(cond ((constant-eq? x 1) (make-combination 1+ (list y)))
	      ((constant-eq? y 1) (make-combination 1+ (list x)))
	      (else (make-combination &+ (list x y))))))))

(define *-expansion
  (right-accumulation 1
    (let ((&* (make-primitive-procedure '&*)))
      (lambda (x y)
	(make-combination &* (list x y))))))

(define (right-accumulation-inverse identity inverse-expansion make-binary)
  (lambda (operands if-expanded if-not-expanded block)
    (let ((expand
	   (lambda (x y)
	     (if-expanded
	      (if (constant-eq? y identity)
		  x
		  (make-binary x y))))))
      (cond ((null? operands)
	     (error "Too few operands"))
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
    (let ((&- (make-primitive-procedure '&-)))
      (lambda (x y)
	(if (constant-eq? y 1)
	    (make-combination -1+ (list x))
	    (make-combination &- (list x y)))))))

(define /-expansion
  (right-accumulation-inverse 1 *-expansion
    (let ((&/ (make-primitive-procedure '&/)))
      (lambda (x y)
	(make-combination &/ (list x y))))))

;;;; Miscellaneous Arithmetic

(define (divide-component-expansion divide selector)
  (lambda (operands if-expanded if-not-expanded block)
    if-not-expanded block ; ignored
    (if-expanded
     (make-combination selector
		       (list (make-combination divide operands))))))

(define quotient-expansion
  (divide-component-expansion integer-divide car))

(define remainder-expansion
  (divide-component-expansion integer-divide cdr))

(define fix:quotient-expansion
  (divide-component-expansion fix:divide car))

(define fix:remainder-expansion
  (divide-component-expansion fix:divide cdr))

;;;; N-ary List Operations

(define apply*-expansion
  (let ((apply-primitive (make-primitive-procedure 'APPLY)))
    (lambda (operands if-expanded if-not-expanded block)
      block ; ignored
      (let ((n (length operands)))
	(cond ((< n 2) (error "APPLY*-EXPANSION: Too few arguments" n))
	      ((< n 10)
	       (if-expanded
		(make-combination
		 apply-primitive
		 (list (car operands)
		       (cons*-expansion-loop (cdr operands))))))
	      (else (if-not-expanded)))))))

(define (cons*-expansion operands if-expanded if-not-expanded block)
  block ; ignored
  (let ((n (length operands)))
    (cond ((zero? n) (error "CONS*-EXPANSION: No arguments!"))
	  ((< n 9) (if-expanded (cons*-expansion-loop operands)))
	  (else (if-not-expanded)))))

(define (cons*-expansion-loop rest)
  (if (null? (cdr rest))
      (car rest)
      (make-combination cons
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
      (make-combination cons
			(list (car rest)
			      (list-expansion-loop (cdr rest))))))

;;;; General CAR/CDR Encodings

(define (general-car-cdr-expansion encoding)
  (lambda (operands if-expanded if-not-expanded block)
    if-not-expanded block ; ignored
    (if (= (length operands) 1)
	(if-expanded
	 (make-combination general-car-cdr
			   (list (car operands)
				 (constant/make encoding))))
	(error "Wrong number of arguments" (length operands)))))

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
  block ; ignored
  (let ((n (length operands)))
    (cond ((zero? n)
	   (error "MAKE-STRING-EXPANSION: No arguments"))
	  ((= n 1)
	   (if-expanded (make-combination string-allocate operands)))
	  (else
	   (if-not-expanded)))))

(define (identity-procedure-expansion operands if-expanded if-not-expanded
				      block)
  if-not-expanded block ; ignored
  (if (not (= (length operands) 1))
      (error "IDENTITY-PROCEDURE-EXPANSION: wrong number of arguments"
	     (length operands)))
  (if-expanded (car operands)))

;;;; Tables

(define usual-integrations/expansion-names
  '(= < > <= >= + - * / quotient remainder fix:quotient fix:remainder
      apply cons* list
      caar cadr cdar cddr
      caaar caadr cadar caddr cdaar cdadr cddar cdddr
      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
      second third fourth fifth sixth seventh eighth
      make-string identity-procedure
      ))

(define usual-integrations/expansion-values
  (list =-expansion <-expansion >-expansion <=-expansion >=-expansion
	+-expansion --expansion *-expansion /-expansion
	quotient-expansion remainder-expansion
	fix:quotient-expansion fix:remainder-expansion
	apply*-expansion cons*-expansion list-expansion
	caar-expansion cadr-expansion cdar-expansion cddr-expansion
	caaar-expansion caadr-expansion cadar-expansion caddr-expansion
	cdaar-expansion cdadr-expansion cddar-expansion cdddr-expansion
	caaaar-expansion caaadr-expansion caadar-expansion caaddr-expansion
	cadaar-expansion cadadr-expansion caddar-expansion cadddr-expansion
	cdaaar-expansion cdaadr-expansion cdadar-expansion cdaddr-expansion
	cddaar-expansion cddadr-expansion cdddar-expansion cddddr-expansion
	second-expansion third-expansion fourth-expansion fifth-expansion
	sixth-expansion seventh-expansion eighth-expansion
	make-string-expansion identity-procedure-expansion
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
     (map (access cgen/external-with-declarations package/cgen)
	  operands)
     (lambda (scode-expression)
       (if-expanded
	(transform/recursive
	 block
	 (integrate/get-top-level-block)
	 scode-expression)))
     if-not-expanded)))