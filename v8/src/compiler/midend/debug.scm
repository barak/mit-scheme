#| -*-Scheme-*-

$Id: debug.scm,v 1.1 1994/11/19 02:04:29 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; Useful debugging syntax

(declare (usual-integrations))

(syntax-table-define (repl/syntax-table (nearest-repl)) 'SHOW
  (macro (form)
    `(kmp/pp
      (%compile-proc ',(eval (list 'quasiquote form)
			     (repl/environment (nearest-repl)))))))

(syntax-table-define (repl/syntax-table (nearest-repl)) 'SHOW-RTL
  (macro (form)
    `(for-each
      pp
      (%compile-proc/rtl ',(eval (list 'quasiquote form)
				 (repl/environment (nearest-repl)))))))

(syntax-table-define (repl/syntax-table (nearest-repl)) 'RUN
  (macro (form)
    `(execute (%compile-proc ',(eval (list 'quasiquote form)
				     (repl/environment (nearest-repl))))
	      (the-environment))))

(syntax-table-define (repl/syntax-table (nearest-repl)) '%COMPILE
  (macro (form)
    `(%compile-proc
      ',(eval (list 'quasiquote form)
	      (repl/environment (nearest-repl))))))

(define %compile-proc
  (lambda (form)
    (compile
     (compile/syntax form))))

(define %compile-proc/rtl
  (lambda (form)
    (source->rtl
     (compile/syntax form))))

(define (compile/syntax form)
  (syntax form (repl/syntax-table (nearest-repl))))

(define &+ (make-primitive-procedure '&+))
(define &- (make-primitive-procedure '&-))
(define &* (make-primitive-procedure '&*))
(define &/ (make-primitive-procedure '&/))
(define &= (make-primitive-procedure '&=))
(define &< (make-primitive-procedure '&<))
(define &> (make-primitive-procedure '&>))

#|

(show (let ()
	(define fib
	  (lambda (n)
	    (cond ((< n 0)
		   (bkpt "Fib" n))
		  ((< n 2)
		   n)
		  (else
		   (+ (fib (- n 1)) (fib (- n 2)))))))
	(fib 6)))

(show (let ()
	(define fib
	  (lambda (n)
	    (cond ((,&< n 2)
		   n)
		  (else
		   (,&+ (fib (,&- n 1)) (fib (,&- n 2)))))))
	fib))

(show (let ()
	(define fib
	  (lambda (n)
	    (cond ((,fix:< n 2)
		   n)
		  (else
		   (,fix:+ (fib (,fix:- n 1)) (fib (,fix:- n 2)))))))
	fib))

(show (define (smemq el l)
	(define (phase-1 l1 l2)
	  (cond ((,not (,pair? l1)) ,false)
		((,eq? el (,car l1)) l1)
		(else
		 (phase-2 (,cdr l1) l2))))

	(define (phase-2 l1 l2)
	  (cond ((,not (,pair? l1)) ,false)
		((,eq? el (,car l1)) l1)
		((,eq? l1 l2) ,false)
		(else
		 (phase-1 (,cdr l1) (,cdr l2)))))

	(phase-1 l l)))

(show (lambda (x)
	(letrec ((foo (lambda () (bar x)))
		 (bar (lambda (z) (,+ z (foo)))))
	  bar)))

(show (lambda (x y)
	(if (and x (foo y))
	    (foo y)
	    (foo x))))

(define (simplify/open-code? value name)
  false)

(show (lambda (x)
	(let ((foo (lambda (y z) (,+ y z))))
	  (foo x (foo x (foo x x))))))

(show (lambda (x y q w)
	(let* ((z (foo y q x))
	       (t (foo x y w))
	       (h (foo z t y)))
	  (bar h z q))))

(show (lambda (x y q w)
	(let* ((z (foo y q x))
	       (t (foo x y w))
	       (h (foo z t y))
	       (l (foo h t w)))
	  (bar l z q))))

(show (lambda (x y z w q)
	(foo x y z)
	(foo y z w)
	(foo z w q)
	(foo w q x)
	(foo q x y)))

(show (lambda (n)
	(do ((i 0 (,+ i 1))
	     (fn 0 fn+1)
	     (fn+1 1 (,+ fn fn+1)))
	    ((,= i n) fn))))

(show (lambda (ol)
	(define (loop l accum)
	  (cond ((,pair? l)
		 (loop (,cdr l) (,cons (,car l) accum)))
		((,null? l)
		 accum)
		(else
		 (error "Not a list" ol))))
	(loop ol '())))

(show (if (foo)
	  23
	  (let ((y (bar)))
	    (lambda (x)
	      (,fix:- x y)))))

(show (define (foo x)
	(let loop ((x x))
	  (let ((y (,cons x x)))
	    (loop (,car y))))))

(show (define (foo x n)
	(let loop ((x x)
		   (n n))
	  (if (,not (,fix:> n 0))
	      x
	      (let ((y (,cons x x)))
		(loop (,car y)
		      (,fix:- n 1)))))))

|#
