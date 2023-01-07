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

;;;; Test of quick-sort

(declare (usual-integrations))

(define-test 'vector
  (lambda ()
    (assert-equal (quick-sort '#(0 2 1 3) <) '#(0 1 2 3))))

(define-test 'list
  (lambda ()
    (assert-equal (quick-sort '(0 2 1 3) <) '(0 1 2 3))))

(define-test 'vector!
  (lambda ()
    (let ((v (vector 0 2 1 3)))
      (quick-sort! v <)
      (assert-equal v '#(0 1 2 3)))))

(define-test 'vector-key
  (lambda ()
    (assert-equal (quick-sort '#((0 . a) (2 . c) (1 . b) (3 . d)) < car)
		  '#((0 . a) (1 . b) (2 . c) (3 . d)))))