#| -*-Scheme-*-

$Id$

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

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
    (compile/debug
     (compile/syntax form))))

(define %compile-proc/rtl
  (lambda (form)
    (source->rtl
     (compile/syntax form))))

(define (compile/syntax form)
  (syntax form (repl/syntax-table (nearest-repl))))

(define (compile/debug scode)
  (apply
   (lambda (result var-table)
     (set! *variable-properties* var-table)
     result)
   (within-midend false
     (lambda ()
       (let ((result (compile-0* scode)))
	 (set! *last-code-rewrite-table* *code-rewrite-table*)
	 (list result *variable-properties*))))))

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
