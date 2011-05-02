#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Tests of shadowing

(declare (usual-integrations))

(define-test 'test-interpreted-shadowing
  (lambda ()
    (run-shadowing-test identity-procedure)))

(define-test 'test-compiled-shadowing
  (lambda ()
    (run-shadowing-test compile-scode)))

(define (run-shadowing-test maybe-compile)
  (let ((root (make-top-level-environment)))
    (let ((child (extend-top-level-environment root)))
      (environment-define root 'foo 3)
      (link-variables root 'bar root 'foo)
      (eval (maybe-compile
	     (syntax&integrate '(begin
				  (define (get-foo) foo)
				  (define (get-bar) bar))
			       '((usual-integrations))
			       child))
	    child)
      (environment-define child 'bar 4)
      (assert-eqv (eval '(get-foo) child) 3)
      (assert-eqv (eval '(get-bar) child) 4))))