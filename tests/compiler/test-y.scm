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

;;;; Tests for the Y combinator

(declare (usual-integrations))

(define test-env
  (make-top-level-environment))

;;; XXX This should really confirm that we don't cons closures.

(define-test 'Y
  (lambda ()
    (define factorial-sexp
      '(let ()
         (define (Y f)
           (f (lambda (x) ((Y f) x))))
         (Y (named-lambda (Yf f)
              (named-lambda (factorial n)
                (if (< n 2)
                    1
                    (* n (f (- n 1)))))))))
    (define factorial-scode
      (syntax&integrate factorial-sexp '((usual-integrations)) test-env))
    (define factorial-compiled
      (compile-scode factorial-scode))
    (define factorial
      (eval factorial-compiled test-env))
    (assert-= (factorial 5) 120)))

(define-test 'U
  (lambda ()
    (define factorial-sexp
      '(let ()
         (define (U f)
           (f f))
         (U (named-lambda (Uf f)
              (named-lambda (factorial n)
                (if (< n 2)
                    1
                    (* n ((U f) (- n 1)))))))))
    (define factorial-scode
      (syntax&integrate factorial-sexp '((usual-integrations)) test-env))
    (define factorial-compiled
      (compile-scode factorial-scode))
    (define factorial
      (eval factorial-compiled test-env))
    (assert-= (factorial 5) 120)))