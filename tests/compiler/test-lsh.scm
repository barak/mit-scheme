#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; Test of fixnum shifting

(declare (usual-integrations))

(define-integrable (fix:noop x)
  (fix:not (identity-procedure (fix:not x))))

(define (fix:lsh3 x)
  (fix:lsh x 3))

(define (fix:lsh3-noop x)
  (fix:noop (fix:lsh x 3)))

(define (fix:rsh3 x)
  (fix:lsh x -3))

(define (fix:rsh3-prim x)
  ((make-primitive-procedure 'fixnum-lsh 2) x -3))

(define (fix:rsh3-noop x)
  (fix:noop (fix:lsh x -3)))

(define (with-expected-failure xfail body)
  (if (default-object? xfail)
      (body)
      (xfail body)))

(define-test 'lsh3+123
  (lambda ()
    (assert-eqv (fix:lsh3 +123) 984)))

(define-test 'lsh3-123
  (lambda ()
    (assert-eqv (fix:lsh3 -123) -984)))

(define-test 'rsh3+123
  (lambda ()
    (assert-eqv (fix:rsh3 +123) 15)))

(define-test 'rsh3-123
  (lambda ()
    (assert-eqv (fix:rsh3 -123) -16)))

(define-test 'rsh3-prim+123
  (lambda ()
    (assert-eqv (fix:rsh3-prim +123) 15)))

(define-test 'rsh3-prim-123
  (lambda ()
    (expect-failure (lambda () (assert-eqv (fix:rsh3-prim -123) -16)))))

(define-test 'lsh3-noop+123
  (lambda ()
    (assert-eqv (fix:lsh3-noop +123) 984)))

(define-test 'lsh3-noop-123
  (lambda ()
    (assert-eqv (fix:lsh3-noop -123) -984)))

(define-test 'rsh3-noop+123
  (lambda ()
    (assert-eqv (fix:rsh3-noop +123) 15)))

(define-test 'rsh3-noop-123
  (lambda ()
    (assert-eqv (fix:rsh3-noop -123) -16)))
