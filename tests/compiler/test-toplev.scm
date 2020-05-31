#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Tests for top-level entry points

(declare (usual-integrations))

(define test-env
  (the-environment))

(define assert-compiled-procedure
  (predicate-assertion compiled-procedure? 'compiled-procedure?))

(define-test 'example-with-flonum-data
  (lambda ()
    (let* ((code '(lambda (x) (flo:+ (flo:* 2. x) 1.)))
	   (scode (syntax&integrate code '((usual-integrations)) test-env))
	   (compiled-expression (compile-scode scode))
	   (procedure (eval compiled-expression test-env)))
      (assert-compiled-procedure procedure)
      (assert-eqv (procedure 123.) 247.))))

(define-test 'cross-example-with-flonum-data
  (lambda ()
    ;; XXX whattakludge
    (define finish-cross-compilation:scode
      (let ((env
             (extend-top-level-environment (->environment '(runtime)))))
	(load "../src/compiler/base/crsend" env)
	(eval 'finish-cross-compilation:scode env)))
    (let* ((code '(lambda (x) (flo:+ (flo:* 2. x) 1.)))
	   (scode (syntax&integrate code '((usual-integrations)) test-env))
	   (cross-compilation
	    (fluid-let ((compiler:cross-compiling? #t))
	      (compile-scode scode)))
	   (compiled-expression
	    (finish-cross-compilation:scode cross-compilation))
	   (procedure (eval compiled-expression test-env)))
      (assert-compiled-procedure procedure)
      (assert-eqv (procedure 123.) 247.))))