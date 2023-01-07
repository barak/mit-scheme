#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;;; Code generation library: optimizer
;;; package: (runtime cgen optimizer)

(declare (usual-integrations))

(add-boot-deps! '(runtime cgen))

(define (optimize-cgen-expr expr)
  (cgen-trace optimization-rules 'optimize-expr expr)
  (let ((apply-rules (cgen-rules-applier optimization-rules)))
    (expr-map (lambda (expr path)
                (declare (ignore path))
                (apply-rules expr))
              expr)))

(define (index< n)
  (lambda (object)
    (and (fixnum? object)
	 (>= object 0)
	 (< object n))))

(define-deferred rules:arithmetic
  (list
   (define-cgen-rule `(pcall + (? n1 ,fixnum?) (? n2 ,fixnum?))
     (lambda (n1 n2) (+ n1 n2)))

   (define-cgen-rule `(pcall + (? n1 ,fixnum?) (? e2))
     (lambda (n1 e2) (cgen:pcall '+ e2 n1)))

   (define-cgen-rule `(pcall + (? e1) 0)
     (lambda (e1) e1))

   (define-cgen-rule `(pcall + (? e1) (? n2 ,negative-fixnum?))
     (lambda (e1 n2) (cgen:pcall '- e1 (- n2))))

   (define-cgen-rule `(pcall +
                             (pcall + (? e1) (? n1 ,fixnum?))
                             (? n2 ,fixnum?))
     (lambda (e1 n1 n2)
       (cgen:pcall '+ e1 (+ n1 n2))))

   (define-cgen-rule `(pcall +
                             (pcall - (? e1) (? n1 ,fixnum?))
                             (? n2 ,fixnum?))
     (lambda (e1 n1 n2)
       (cgen:pcall '+ e1 (- n2 n1))))


   (define-cgen-rule `(pcall - (? n1 ,fixnum?) (? n2 ,fixnum?))
     (lambda (n1 n2) (- n1 n2)))

   (define-cgen-rule `(pcall - (? e1) 0)
     (lambda (e1) e1))

   (define-cgen-rule `(pcall - (? e1) (? n2 ,negative-fixnum?))
     (lambda (e1 n2) (cgen:pcall '+ e1 (- n2))))

   (define-cgen-rule `(pcall -
                             (pcall - (? e1) (? n1 ,fixnum?))
                             (? n2 ,fixnum?))
     (lambda (e1 n1 n2)
       (cgen:pcall '- e1 (+ n1 n2))))

   (define-cgen-rule `(pcall -
                             (pcall + (? e1) (? n1 ,fixnum?))
                             (? n2 ,fixnum?))
     (lambda (e1 n1 n2)
       (cgen:pcall '- e1 (- n2 n1))))
   ))

(define-deferred rules:length-tests
  (list
   (define-cgen-rule `(if (pcall list? (? a))
			  (pcall = (pcall length (? a)) (? n ,(index< 4)))
			  #f)
     (lambda (a n)
       (cgen:and*
	(let loop ((a a) (n n))
	  (if (> n 0)
	      (cons (cgen:pcall 'pair? a)
			(loop (cgen:pcall 'cdr a) (- n 1)))
	      (list (cgen:pcall 'null? a)))))))

   (define-cgen-rule `(if (pcall list? (? a))
			  (if (pcall = (pcall length (? a)) (? n ,(index< 4)))
			      (? b)
			      #f)
			  #f)
     (lambda (a n b)
       (cgen:and*
	(let loop ((a a) (n n))
	  (if (> n 0)
	      (cons (cgen:pcall 'pair? a)
			(loop (cgen:pcall 'cdr a) (- n 1)))
	      (list (cgen:pcall 'null? a)
		    b))))))

   (define-cgen-rule `(if (pcall list? (? a))
			  (pcall >= (pcall length (? a)) (? n ,(index< 4)))
			  #f)
     (lambda (a n)
       (cgen:and*
	(let loop ((a a) (n n))
	  (if (> n 0)
	      (cons (cgen:pcall 'pair? a)
		    (loop (cgen:pcall 'cdr a) (- n 1)))
	      (list (cgen:pcall 'list? a)))))))

   (define-cgen-rule `(if (pcall list? (? a))
			  (if (pcall >= (pcall length (? a)) (? n ,(index< 4)))
			      (? b)
			      #f)
			  #f)
     (lambda (a n b)
       (cgen:and*
	(let loop ((a a) (n n))
	  (if (> n 0)
	      (cons (cgen:pcall 'pair? a)
		    (loop (cgen:pcall 'cdr a) (- n 1)))
	      (list (cgen:pcall 'list? a)
		    b))))))


   (define-cgen-rule `(pcall >= (pcall length (?)) 0)
     (lambda () (cgen:and)))

   (define-cgen-rule `(pcall >= (pcall vector-length (?)) 0)
     (lambda () (cgen:and)))
   ))

(define-deferred rules:misc
  (list
   (define-cgen-rule `(if #t (? c) (?))
     (lambda (c) c))

   (define-cgen-rule `(if #f (?) (? a))
     (lambda (a) a))


   (define-cgen-rule `(pcall every (lambda ((??)) #t) (??))
     (lambda () '#t))

   (define-cgen-rule `(pcall vector-every (lambda ((??)) #t) (??))
     (lambda () '#t))

   (define-cgen-rule `(pcall any (lambda ((??)) #f) (??))
     (lambda () '#f))

   (define-cgen-rule `(pcall vector-any (lambda ((??)) #f) (??))
     (lambda () '#f))


   (define-cgen-rule `(pcall list-ref (? a) (? n ,(index< 10)))
     (lambda (a n)
       (let loop ((n n) (expr a))
	 (if (> n 0)
	     (loop (- n 1) (cgen:pcall 'cdr expr))
	     (cgen:pcall 'car expr)))))

   (define-cgen-rule `(pcall drop (? a) (? n ,(index< 10)))
     (lambda (a n)
       (let loop ((n n) (expr a))
	 (if (> n 0)
	     (loop (- n 1) (cgen:pcall 'cdr expr))
	     expr))))
   ))

(define-deferred optimization-rules
  (apply make-cgen-rule-set
	 (append rules:arithmetic
		 rules:length-tests
		 rules:misc)))