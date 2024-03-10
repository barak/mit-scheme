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

;;;; Tests for types

(declare (usual-integrations))

(define-test 'non-type
  (lambda ()
    (let ((np (lambda (object) object #f)))
      (assert-false (type? np))
      (assert-type-error (lambda () (type-name np)))
      (assert-type-error (lambda () (type-parts np)))
      (assert-type-error (lambda () (type-test np)))
      (assert-type-error (lambda () (type-supersets np))))))

(define-test 'simple-type
  (lambda ()
    (test-type-operations symbol?
			  '(disjoin interned-symbol uninterned-symbol))))

(define (test-type-operations type name)
  (assert-true (type? type))
  (assert-equal (type-name type) name)
  (assert-lset= eq?
		(type-parts type)
		(list interned-symbol? uninterned-symbol?)))

(define-test 'ordering
  (lambda ()
    (assert-true (type<= no-object? symbol?))
    (assert-true (type<= no-object? no-object?))
    (assert-false (type<= symbol? no-object?))

    (assert-false (type<= any-object? symbol?))
    (assert-true (type<= any-object? any-object?))
    (assert-true (type<= symbol? any-object?))

    (assert-true (type<= interned-symbol? symbol?))
    (assert-true (type<= uninterned-symbol? symbol?))
    (assert-false (type<= symbol? interned-symbol?))
    (assert-false (type<= symbol? uninterned-symbol?))
    (assert-false (type<= interned-symbol? uninterned-symbol?))
    (assert-false (type<= uninterned-symbol? interned-symbol?))
    ))