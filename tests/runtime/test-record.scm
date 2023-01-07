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

;;;; Tests of record implementation

(define-record-type <t1> make-t1 t1? (a t1-a) (b t1-b))
(define make-t1-by-keyword (record-keyword-constructor <t1>))

(define-record-type (<t2> <t1>) make-t2 t2? (c t2-c) (a t2-a))
(define make-t2-by-name (record-constructor <t2> '(c b a)))
(define make-t2-by-keyword (record-keyword-constructor <t2>))

(define-test 'record-types
  (lambda ()
    (assert-eqv (record-type-parent <t1>) #f)
    (assert-equal (record-type-field-names <t1>) '(a b))
    (assert-eqv (record-type-parent <t2>) <t1>)
    (assert-equal (record-type-field-names <t2>) '(a b c a))))

(define-test 'root-record
  (lambda ()
    (let ((t1 (make-t1 2 3)))
      (assert-true (t1? t1))
      (assert-eqv (t1-a t1) 2)
      (assert-eqv (t1-b t1) 3)
      (assert-eqv (record-type-descriptor t1) <t1>)
      (assert-equal (pp-description t1) '((a 2) (b 3))))))

(define-test 'root-record-by-keyword
  (lambda ()
    (assert-error (lambda () (make-t1-by-keyword 'a 3 'b 2 'a 5)))
    (let ((t1 (make-t1-by-keyword 'b 2 'a 3)))
      (assert-true (t1? t1))
      (assert-eqv (t1-a t1) 3)
      (assert-eqv (t1-b t1) 2)
      (assert-eqv (record-type-descriptor t1) <t1>)
      (assert-equal (pp-description t1) '((a 3) (b 2))))))

(define-test 'sub-record
  (lambda ()
    (let ((t2 (make-t2 2 3 5 7)))
      (assert-true (t1? t2))
      (assert-eqv (t1-a t2) 2)
      (assert-eqv (t1-b t2) 3)
      (assert-true (t2? t2))
      (assert-eqv (t2-c t2) 5)
      (assert-eqv (t2-a t2) 7)
      (assert-eqv (record-type-descriptor t2) <t2>)
      (assert-equal (pp-description t2) '((a 2) (b 3) (c 5) (a 7))))))

(define-test 'sub-record-by-name
  (lambda ()
    (let ((t2 (make-t2-by-name 2 3 5)))
      (assert-true (t1? t2))
      (assert-eqv (t1-a t2) #f)
      (assert-eqv (t1-b t2) 3)
      (assert-true (t2? t2))
      (assert-eqv (t2-c t2) 2)
      (assert-eqv (t2-a t2) 5)
      (assert-eqv (record-type-descriptor t2) <t2>)
      (assert-equal (pp-description t2) '((a #f) (b 3) (c 2) (a 5))))))

(define-test 'sub-record-by-keyword
  (lambda ()
    (assert-error (lambda () (make-t2-by-keyword 'a 2 'b 3 'c 5 'a 7)))
    (assert-error (lambda () (make-t2-by-keyword 'a 2 'b 3 'c 5 'c 7)))
    (let ((t2 (make-t2-by-keyword 'a 2 'b 3 'c 5)))
      (assert-true (t1? t2))
      (assert-eqv (t1-a t2) #f)
      (assert-eqv (t1-b t2) 3)
      (assert-true (t2? t2))
      (assert-eqv (t2-c t2) 5)
      (assert-eqv (t2-a t2) 2)
      (assert-eqv (record-type-descriptor t2) <t2>)
      (assert-equal (pp-description t2) '((a #f) (b 3) (c 5) (a 2))))))

(define-record-type (<t3> <t2>) make-t3 t3? (d t3-d))
(define-record-type (<t4> <t1>) make-t4 t4? (w t3-w) (x t3-x) (y t3-y) (z t3-z))

(define-test 'sub-record-predicates
  (lambda ()
    (let ((t3 (make-t3 2 3 5 7 11)))
      (assert-true (t1? t3))
      (assert-true (t2? t3))
      (assert-true (t3? t3)))
    (let ((t4 (make-t4 2 3 5 7 <t3> 11)))
      (assert-true (t1? t4))
      (assert-false (t2? t4))
      (assert-false (t3? t4))
      (assert-true (t4? t4)))))