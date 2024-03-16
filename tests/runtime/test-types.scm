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
      (assert-type-error (lambda () (type-test np)))
      (assert-type-error (lambda () (type-supersets np))))))

(define-test 'simple-type
  (lambda ()
    (assert-true (type? pair?))
    (assert-eq (type-name pair?) 'pair)
    (assert-eq (type-derivation pair?) #f)
    (assert-null (type-supersets pair?))
    (assert-lset<= eq? (list non-empty-list?) (type-subsets pair?))

    (assert-true (type? symbol?))
    (assert-eq (type-name symbol?) 'symbol)
    (let ((d (type-derivation symbol?))
	  (elt-types (list interned-symbol? uninterned-symbol?)))
      (assert-true (pair? d))
      (assert-eq (car d) 'disjoin)
      (assert-lset= eq? (cdr d) elt-types)
      (assert-lset<= eq? elt-types (type-subsets symbol?)))

    (assert-true (type? interned-symbol?))
    (assert-eq (type-name interned-symbol?) 'interned-symbol)
    (assert-false (type-derivation interned-symbol?))
    (assert-lset<= eq? (list symbol?) (type-supersets interned-symbol?))
    (assert-lset<= eq? (list interned-symbol?) (type-subsets symbol?))))

(define-test 'ordering
  (lambda ()
    (assert-type<= no-object? symbol?)
    (assert-type<= no-object? no-object?)
    (assert-type!<= symbol? no-object?)

    (assert-type!<= any-object? symbol?)
    (assert-type<= any-object? any-object?)
    (assert-type<= symbol? any-object?)

    (assert-type<= interned-symbol? symbol?)
    (assert-type<= uninterned-symbol? symbol?)
    (assert-type!<= symbol? interned-symbol?)
    (assert-type!<= symbol? uninterned-symbol?)
    (assert-type!<= interned-symbol? uninterned-symbol?)
    (assert-type!<= uninterned-symbol? interned-symbol?)

    (assert-type<= type? apply-hook?)
    (assert-type!<= type? entity?)))

(define-test 'object->type
  (lambda ()
    (assert-eq (object->type #f) false?)
    (assert-type<= (object->type (list 'a)) pair?)
    (assert-type<= (object->type 'a) interned-symbol?)
    (assert-type<= (object->type 0) fixnum?)
    (assert-type<= (object->type (expt 10. 40)) flonum?)
    (assert-eq (object->type #!default) default-object?)
    (assert-eq (object->type '()) null?)
    (assert-eq (object->type type?) type?)))