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

;;;; Test of quotients and remainders

(declare (usual-integrations))

(define (numbers)
  `(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 1234567 12345678901
      ,(fix:smallest-value)
      ,(fix:largest-value)
      ,(+ 1 (fix:largest-value))
      ,(+ 2 (fix:largest-value))
      ,(+ 3 (fix:largest-value))
      ,(* 3 (fix:largest-value))))

(define (numbers+/-)
  (append (numbers) (map - (numbers))))

(define (numerators)
  (cons 0 (numbers+/-)))

(define (denominators)
  (numbers+/-))

(define (prelude)
  '(begin))

(define (eval-compiled expression)
  (with-test-properties
      (lambda ()
	(let* ((env (make-top-level-environment))
	       (program `(begin ,(prelude) ,expression))
	       (scode (syntax&integrate program '((usual-integrations)) env))
	       (compiled (compile-scode scode)))
	  (eval compiled env)))
    'expression expression))

(define (eval-compiled* expressions)
  (map (lambda (procedure) (procedure))
       (eval-compiled
	`(list
	  ,@(map (lambda (expression)
		   `(lambda () ,expression))
		 expressions)))))

(define (define-division-test op divide predicate)
  (define-test op
    (lambda ()
      (let* ((numerators (filter predicate (numerators)))
	     (denominators (filter predicate (denominators)))
	     (procedure-expressions
	      (map (lambda (denominator)
		     `(lambda (x) (,op x ,denominator)))
		   denominators))
	     (procedures (eval-compiled* procedure-expressions)))
	(for-each
	 (lambda (denominator procedure-expression procedure)
	   (for-each (lambda (numerator)
		       (assert-eqv (procedure numerator)
				   (divide numerator denominator)
				   'expression
				   `(assert-eqv
				     (,procedure-expression
				      ,numerator)
				     ,(divide numerator denominator))))
		     numerators))
	 denominators procedure-expressions procedures)))))

(define-division-test 'quotient quotient (lambda (x) x #t))
(define-division-test 'remainder remainder (lambda (x) x #t))
(define-division-test 'int:quotient int:quotient (lambda (x) x #t))
(define-division-test 'int:remainder int:remainder (lambda (x) x #t))
(define-division-test 'fix:quotient fix:quotient fixnum?)
(define-division-test 'fix:remainder fix:remainder fixnum?)