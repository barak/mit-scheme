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

;;;; Test of weak-pair operations

(declare (usual-integrations))

(define-test 'weak-fold
  (lambda ()

    (assert-error (lambda () (weak-fold cons '() 'a)))
    (assert-error (lambda () (weak-fold cons* '() '() 'a)))
    (assert-error (lambda () (weak-fold cons* '() 'a '())))

    (assert-equal (weak-fold cons '() '())
		  '())
    (assert-equal (weak-fold cons '() (weak-list 1))
		  '(1))
    (assert-equal (weak-fold cons '() (weak-list 1 2))
		  '(2 1))

    (assert-equal (weak-fold cons 'a '())
		  'a)
    (assert-equal (weak-fold cons 'a (weak-list 1))
		  '(1 . a))
    (assert-equal (weak-fold cons 'a (weak-list 1 2))
		  '(2 1 . a))

    (assert-equal (weak-fold rcons '() '())
		  '())
    (assert-equal (weak-fold rcons '() (weak-list 1))
		  '(() . 1))
    (assert-equal (weak-fold rcons '() (weak-list 1 2))
		  '((() . 1) . 2))

    (assert-equal (weak-fold rcons 'a '())
		  'a)
    (assert-equal (weak-fold rcons 'a (weak-list 1))
		  '(a . 1))
    (assert-equal (weak-fold rcons 'a (weak-list 1 2))
		  '((a . 1) . 2))

    (assert-equal (weak-fold + 0 '())
		  0)
    (assert-equal (weak-fold + 0 (weak-list 1))
		  1)
    (assert-equal (weak-fold + 0 (weak-list 1 2))
		  3)

    (assert-equal (weak-fold + 0 '() '())
		  0)
    (assert-equal (weak-fold + 0 (weak-list 2) (weak-list 3))
		  5)
    (assert-equal (weak-fold + 0 (weak-list 2 3) (weak-list 5 7))
		  17)

    (assert-equal (weak-fold cons* '() '() '())
		  '())
    (assert-equal (weak-fold cons* '() (weak-list 2) (weak-list 3))
		  '(2 3))
    (assert-equal (weak-fold cons* '() (weak-list 2 3) (weak-list 5 7))
		  '(3 7 2 5))

    (assert-equal (weak-fold list '() '() '())
		  '())
    (assert-equal (weak-fold list '() (weak-list 2) (weak-list 3))
		  '(2 3 ()))
    (assert-equal (weak-fold list '() (weak-list 2 3) (weak-list 5 7))
		  '(3 7 (2 5 ())))

    ))

(define-test 'weak-fold-right
  (lambda ()

    (assert-error (lambda () (weak-fold-right cons '() 'a)))
    (assert-error (lambda () (weak-fold-right cons* '() '() 'a)))
    (assert-error (lambda () (weak-fold-right cons* '() 'a '())))

    (assert-equal (weak-fold-right cons '() '())
		  '())
    (assert-equal (weak-fold-right cons '() (weak-list 1))
		  '(1))
    (assert-equal (weak-fold-right cons '() (weak-list 1 2))
		  '(1 2))

    (assert-equal (weak-fold-right cons 'a '())
		  'a)
    (assert-equal (weak-fold-right cons 'a (weak-list 1))
		  '(1 . a))
    (assert-equal (weak-fold-right cons 'a (weak-list 1 2))
		  '(1 2 . a))

    (assert-equal (weak-fold-right rcons '() '())
		  '())
    (assert-equal (weak-fold-right rcons '() (weak-list 1))
		  '(() . 1))
    (assert-equal (weak-fold-right rcons '() (weak-list 1 2))
		  '((() . 2) . 1))

    (assert-equal (weak-fold-right rcons 'a '())
		  'a)
    (assert-equal (weak-fold-right rcons 'a (weak-list 1))
		  '(a . 1))
    (assert-equal (weak-fold-right rcons 'a (weak-list 1 2))
		  '((a . 2) . 1))

    (assert-equal (weak-fold-right + 0 '())
		  0)
    (assert-equal (weak-fold-right + 0 (weak-list 1))
		  1)
    (assert-equal (weak-fold-right + 0 (weak-list 1 2))
		  3)

    (assert-equal (weak-fold-right + 0 '() '())
		  0)
    (assert-equal (weak-fold-right + 0 (weak-list 2) (weak-list 3))
		  5)
    (assert-equal (weak-fold-right + 0 (weak-list 2 3) (weak-list 5 7))
		  17)

    (assert-equal (weak-fold-right cons* '() '() '())
		  '())
    (assert-equal (weak-fold-right cons* '() (weak-list 2) (weak-list 3))
		  '(2 3))
    (assert-equal (weak-fold-right cons* '() (weak-list 2 3) (weak-list 5 7))
		  '(2 5 3 7))

    (assert-equal (weak-fold-right list '() '() '())
		  '())
    (assert-equal (weak-fold-right list '() (weak-list 2) (weak-list 3))
		  '(2 3 ()))
    (assert-equal (weak-fold-right list '() (weak-list 2 3) (weak-list 5 7))
		  '(2 5 (3 7 ())))

    ))

(define-test 'weak-fold-map
  (lambda ()

    (assert-error (lambda () (weak-fold-map cons '() add13 'a)))
    (assert-error (lambda () (weak-fold-map cons* '() * '() 'a)))
    (assert-error (lambda () (weak-fold-map cons* '() * 'a '())))

    (assert-equal (weak-fold-map cons '() add13 '())
		  '())
    (assert-equal (weak-fold-map cons '() add13 (weak-list 1))
		  '(14))
    (assert-equal (weak-fold-map cons '() add13 (weak-list 1 2))
		  '(15 14))

    (assert-equal (weak-fold-map cons 'a add13 '())
		  'a)
    (assert-equal (weak-fold-map cons 'a add13 (weak-list 1))
		  '(14 . a))
    (assert-equal (weak-fold-map cons 'a add13 (weak-list 1 2))
		  '(15 14 . a))

    (assert-equal (weak-fold-map rcons '() add13 '())
		  '())
    (assert-equal (weak-fold-map rcons '() add13 (weak-list 1))
		  '(() . 14))
    (assert-equal (weak-fold-map rcons '() add13 (weak-list 1 2))
		  '((() . 14) . 15))

    (assert-equal (weak-fold-map rcons 'a add13 '())
		  'a)
    (assert-equal (weak-fold-map rcons 'a add13 (weak-list 1))
		  '(a . 14))
    (assert-equal (weak-fold-map rcons 'a add13 (weak-list 1 2))
		  '((a . 14) . 15))

    (assert-equal (weak-fold-map + 0 add13 '())
		  0)
    (assert-equal (weak-fold-map + 0 add13 (weak-list 1))
		  14)
    (assert-equal (weak-fold-map + 0 add13 (weak-list 1 2))
		  29)

    (assert-equal (weak-fold-map + 0 * '() '())
		  0)
    (assert-equal (weak-fold-map + 0 * (weak-list 2) (weak-list 3))
		  6)
    (assert-equal (weak-fold-map + 0 * (weak-list 2 3) (weak-list 5 7))
		  31)

    (assert-equal (weak-fold-map cons '() * '() '())
		  '())
    (assert-equal (weak-fold-map cons '() * (weak-list 2) (weak-list 3))
		  '(6))
    (assert-equal (weak-fold-map cons '() * (weak-list 2 3) (weak-list 5 7))
		  '(21 10))

    (assert-equal (weak-fold-map list '() * '() '())
		  '())
    (assert-equal (weak-fold-map list '() * (weak-list 2) (weak-list 3))
		  '(6 ()))
    (assert-equal (weak-fold-map list '() * (weak-list 2 3) (weak-list 5 7))
		  '(21 (10 ())))

    ))

(define-test 'weak-fold-right-map
  (lambda ()

    (assert-error (lambda () (weak-fold-right-map cons '() add13 'a)))
    (assert-error (lambda () (weak-fold-right-map cons* '() * '() 'a)))
    (assert-error (lambda () (weak-fold-right-map cons* '() * 'a '())))

    (assert-equal (weak-fold-right-map cons '() add13 '())
		  '())
    (assert-equal (weak-fold-right-map cons '() add13 (weak-list 1))
		  '(14))
    (assert-equal (weak-fold-right-map cons '() add13 (weak-list 1 2))
		  '(14 15))

    (assert-equal (weak-fold-right-map cons 'a add13 '())
		  'a)
    (assert-equal (weak-fold-right-map cons 'a add13 (weak-list 1))
		  '(14 . a))
    (assert-equal (weak-fold-right-map cons 'a add13 (weak-list 1 2))
		  '(14 15 . a))

    (assert-equal (weak-fold-right-map rcons '() add13 '())
		  '())
    (assert-equal (weak-fold-right-map rcons '() add13 (weak-list 1))
		  '(() . 14))
    (assert-equal (weak-fold-right-map rcons '() add13 (weak-list 1 2))
		  '((() . 15) . 14))

    (assert-equal (weak-fold-right-map rcons 'a add13 '())
		  'a)
    (assert-equal (weak-fold-right-map rcons 'a add13 (weak-list 1))
		  '(a . 14))
    (assert-equal (weak-fold-right-map rcons 'a add13 (weak-list 1 2))
		  '((a . 15) . 14))

    (assert-equal (weak-fold-right-map + 0 add13 '())
		  0)
    (assert-equal (weak-fold-right-map + 0 add13 (weak-list 1))
		  14)
    (assert-equal (weak-fold-right-map + 0 add13 (weak-list 1 2))
		  29)

    (assert-equal (weak-fold-right-map + 0 * '() '())
		  0)
    (assert-equal (weak-fold-right-map + 0 * (weak-list 2) (weak-list 3))
		  6)
    (assert-equal (weak-fold-right-map + 0 * (weak-list 2 3) (weak-list 5 7))
		  31)

    (assert-equal (weak-fold-right-map cons '() * '() '())
		  '())
    (assert-equal (weak-fold-right-map cons '() * (weak-list 2) (weak-list 3))
		  '(6))
    (assert-equal (weak-fold-right-map cons '() *
				       (weak-list 2 3) (weak-list 5 7))
		  '(10 21))

    (assert-equal (weak-fold-right-map list '() * '() '())
		  '())
    (assert-equal (weak-fold-right-map list '() * (weak-list 2) (weak-list 3))
		  '(6 ()))
    (assert-equal (weak-fold-right-map list '() *
				       (weak-list 2 3) (weak-list 5 7))
		  '(10 (21 ())))

    ))

(define-test 'weak-alist-fold
  (lambda ()

    (assert-error (lambda () (weak-alist-fold kcons '() 'a)))

    (assert-equal (weak-alist-fold kcons '() '())
		  '())
    (assert-equal (weak-alist-fold kcons '() walist1)
		  '(a 1))
    (assert-equal (weak-alist-fold kcons '() walist2)
		  '(b 2 a 1))

    (assert-equal (weak-alist-fold kcons 'c '())
		  'c)
    (assert-equal (weak-alist-fold kcons 'c walist1)
		  '(a 1 . c))
    (assert-equal (weak-alist-fold kcons 'c walist2)
		  '(b 2 a 1 . c))

    (assert-equal (weak-alist-fold rkcons '() '())
		  '())
    (assert-equal (weak-alist-fold rkcons '() walist1)
		  '(1 a))
    (assert-equal (weak-alist-fold rkcons '() walist2)
		  '(2 b 1 a))

    (assert-equal (weak-alist-fold rkcons 'c '())
		  'c)
    (assert-equal (weak-alist-fold rkcons 'c walist1)
		  '(1 a . c))
    (assert-equal (weak-alist-fold rkcons 'c walist2)
		  '(2 b 1 a . c))

    (assert-equal (weak-alist-fold add13-datum '() '())
		  '())
    (assert-equal (weak-alist-fold add13-datum '() walist1)
		  '((a . 14)))
    (assert-equal (weak-alist-fold add13-datum '() walist2)
		  '((b . 15) (a . 14)))

    ))

(define-test 'weak-alist-fold-right
  (lambda ()

    (assert-error (lambda () (weak-alist-fold-right kcons '() 'a)))

    (assert-equal (weak-alist-fold-right kcons '() '())
		  '())
    (assert-equal (weak-alist-fold-right kcons '() walist1)
		  '(a 1))
    (assert-equal (weak-alist-fold-right kcons '() walist2)
		  '(a 1 b 2))

    (assert-equal (weak-alist-fold-right kcons 'c '())
		  'c)
    (assert-equal (weak-alist-fold-right kcons 'c walist1)
		  '(a 1 . c))
    (assert-equal (weak-alist-fold-right kcons 'c walist2)
		  '(a 1 b 2 . c))

    (assert-equal (weak-alist-fold-right rkcons '() '())
		  '())
    (assert-equal (weak-alist-fold-right rkcons '() walist1)
		  '(1 a))
    (assert-equal (weak-alist-fold-right rkcons '() walist2)
		  '(1 a 2 b))

    (assert-equal (weak-alist-fold-right rkcons 'c '())
		  'c)
    (assert-equal (weak-alist-fold-right rkcons 'c walist1)
		  '(1 a . c))
    (assert-equal (weak-alist-fold-right rkcons 'c walist2)
		  '(1 a 2 b . c))

    (assert-equal (weak-alist-fold-right add13-datum '() '())
		  '())
    (assert-equal (weak-alist-fold-right add13-datum '() walist1)
		  '((a . 14)))
    (assert-equal (weak-alist-fold-right add13-datum '() walist2)
		  '((a . 14) (b . 15)))

    ))

(define walist1
  (list (weak-cons 'a 1)))

(define walist2
  (list (weak-cons 'a 1) (weak-cons 'b 2)))

(define (rcons a b)
  (cons b a))

(define (kcons key datum acc)
  (cons key (cons datum acc)))

(define (rkcons key datum acc)
  (cons datum (cons key acc)))

(define (racons key datum acc)
  (alist-cons datum key acc))

(define (rcons* item . items)
  (let loop ((first item) (rest items))
    (if (pair? rest)
	(rcons first (loop (car rest) (cdr rest)))
	first)))

(define (add13-datum key datum acc)
  (alist-cons key (+ datum 13) acc))

(define (add13 a)
  (+ a 13))