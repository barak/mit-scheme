#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Tests for predicate dispatchers

(declare (usual-integrations))

(define-test 'generic-basic
  (lambda ()
    (assert-range-error (lambda () (simple-predicate-dispatcher 'foo 0)))

    (define foo
      (simple-predicate-dispatcher 'foo 1))

    (define-predicate-dispatch-handler foo (list any-object?)
      (lambda (arg)
	(declare (ignore arg))
        'foobar))

    (assert-equal (foo #f) 'foobar)))

(define-test 'generic-example1
  (lambda ()
    (define foo
      (simple-predicate-dispatcher 'foo 2))

    (define-predicate-dispatch-handler foo (list number? number?)
      (lambda (a b)
        (+ a b)))

    (let ((symbolic
           (lambda (a b)
             (list '+ a b))))
      (define-predicate-dispatch-handler foo (list number? symbol?) symbolic)
      (define-predicate-dispatch-handler foo (list symbol? number?) symbolic)
      (define-predicate-dispatch-handler foo (list symbol? symbol?) symbolic))

    (assert-equal (foo 1 2) 3)
    (assert-equal (foo 1 'a) '(+ 1 a))
    (assert-equal (foo 'a 2) '(+ a 2))
    (assert-equal (foo 'a 'b) '(+ a b))))

(define-test 'complex-relationships
  (lambda ()
    (test-complex-relationships most-specific-handler-set)
    (test-complex-relationships cached-most-specific-handler-set)))

(define (test-complex-relationships make-handler-set)
  (define tester
    (make-predicate-dispatcher 'tester 1 make-handler-set))

  (define-predicate-dispatch-default-handler tester
    (lambda (x) (declare (ignore x)) #f))

  (define-predicate-dispatch-handler tester
    (list %record?)
    (lambda (x) (declare (ignore x)) '%record))

  (define-predicate-dispatch-handler tester
    (list record?)
    (lambda (x) (declare (ignore x)) 'record))

  (define-predicate-dispatch-handler tester
    (list uri?)
    (lambda (x) (declare (ignore x)) 'uri))

  (define-predicate-dispatch-handler tester
    (list record-type?)
    (lambda (x) (declare (ignore x)) 'record-type))

  (define-predicate-dispatch-handler tester
    (list dispatch-tag?)
    (lambda (x) (declare (ignore x)) 'dispatch-tag))

  (define-predicate-dispatch-handler tester
    (list dispatch-metatag?)
    (lambda (x) (declare (ignore x)) 'dispatch-metatag))

  (let ((uri (string->uri "foo")))
    (assert-eqv (tester (predicate->dispatch-tag dispatch-tag?)) 'dispatch-tag)
    (assert-eqv (tester uri) 'uri)
    (assert-eqv (tester (record-type-descriptor uri)) 'record-type)
    (assert-eqv (tester (dispatch-tag-metatag (record-type-descriptor uri)))
		'dispatch-metatag)
    (assert-eqv (tester (%record 'a 'b)) '%record)))