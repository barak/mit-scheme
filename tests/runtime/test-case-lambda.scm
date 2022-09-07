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

;;;; Tests of case-lambda

(declare (usual-integrations))

(define-test 'empty
  (lambda ()
    (define f (identity-procedure (case-lambda (() 0))))
    (assert-eqv (f) 0)
    (assert-error (lambda () (f 1)))
    (assert-error (lambda () (f 2 2)))))

(define-test 'one
  (lambda ()
    (define f (identity-procedure (case-lambda ((x) x))))
    (assert-error (lambda () (f)))
    (assert-eqv (f 1) 1)
    (assert-error (lambda () (f 2 2)))
    (assert-error (lambda () (f 3 3 3)))))

(define-test 'one-three
  (lambda ()
    (define f
      (identity-procedure
       (case-lambda ((x) x) ((x y z) (declare (ignore x y)) z))))
    (assert-error (lambda () (f)))
    (assert-eqv (f 123) 123)
    (assert-error (lambda () (f 123 456)))
    (assert-eqv (f 123 456 789) 789)
    (assert-error (lambda () (f 123 456 789 0)))))

(define-test 'three-one
  (lambda ()
    (define f
      (identity-procedure
       (case-lambda ((x y z) (declare (ignore x y)) z) ((x) x))))
    (assert-error (lambda () (f)))
    (assert-eqv (f 123) 123)
    (assert-error (lambda () (f 123 456)))
    (assert-eqv (f 123 456 789) 789)
    (assert-error (lambda () (f 123 456 789 0)))))

(define-test 'overlap
  (lambda ()
    (define f
      (identity-procedure
       (case-lambda
	((x . r) (declare (ignore x r)) 0)
	((x y) (declare (ignore x y)) 1)
	((x y z) (declare (ignore x y z)) 2))))
    (assert-error (lambda () (f)))
    (assert-eqv (f 1) 0)
    (assert-eqv (f 2 2) 0)
    (assert-eqv (f 3 3 3) 0)
    (assert-eqv (f 4 4 4 4) 0)))

(define-test 'rest-disorder
  (lambda ()
    (define f
      (identity-procedure
       (case-lambda
	((x y z . r) (declare (ignore x y z r)) 0)
	((x y . r) (declare (ignore x y r)) 1)
	((x y z w . r) (declare (ignore x y z w r)) 2)
	(r r 3))))
    (assert-eqv (f) 3)
    (assert-eqv (f 1) 3)
    (assert-eqv (f 2 2) 1)
    (assert-eqv (f 3 3 3) 0)
    (assert-eqv (f 4 4 4 4) 0)))

(define-test 'optional-disorder
  (lambda ()
    (define f
      (identity-procedure
       (case-lambda
	((x y z . r) (declare (ignore x y z r)) 0)
	((x y #!optional z . r) (declare (ignore x y z r)) 1)
	((x y . r) (declare (ignore x y r)) 2)
	(r (declare (ignore r)) 3))))
    (assert-eqv (f) 3)
    (assert-eqv (f 1) 3)
    (assert-eqv (f 2 2) 1)
    (assert-eqv (f 3 3 3) 0)
    (assert-eqv (f 4 4 4 4) 0)))
