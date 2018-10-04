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

;;;; Tests for library parser

(declare (usual-integrations))

(include "test-library-data/support-code.scm")

(define-test 'parse-library:empty
  (lambda ()
    (let ((parsed
	   (parse-define-library-form '(define-library (foo bar))
				      test-pathname)))
      (value-assert parsed-library?
		    "parsed library"
		    parsed)
      (assert-equal (parsed-library-name parsed)
		    '(foo bar))
      (assert-null (parsed-library-exports parsed))
      (assert-null (parsed-library-imports parsed))
      (assert-null (parsed-library-contents parsed))
      (assert-equal (parsed-library-pathname parsed)
		    test-pathname))))

(define-test 'parse-library:ex1
  (lambda ()
    (let ((parsed (parse-define-library-form ex1 test-pathname)))
      (assert-equal (parsed-library-name parsed)
		    '(foo bar))
      (assert-lset= equal?
		    (parsed-library-imports parsed)
		    (map convert-import ex1-imports))
      (assert-lset= library-export=?
		    (parsed-library-exports parsed)
		    (map convert-export ex1-exports))
      (assert-list= equal?
		    (parsed-library-contents parsed)
		    (append-map convert-content ex1-contents))
      (assert-equal (parsed-library-pathname parsed)
		    test-pathname))))

(define-test 'parse-library:ex2
  (lambda ()
    (let ((parsed (parse-define-library-form ex2 test-pathname)))
      (assert-equal (parsed-library-name parsed)
		    '(foo bar))
      (assert-lset= equal?
		    (parsed-library-imports parsed)
		    (map convert-import (append ex1-imports ex2-extra-imports)))
      (assert-lset= library-export=?
		    (parsed-library-exports parsed)
		    (map convert-export (append ex1-exports ex2-extra-exports)))
      (assert-list= equal?
		    (parsed-library-contents parsed)
		    (append-map convert-content
				(append ex2-extra-contents ex1-contents)))
      (assert-equal (parsed-library-pathname parsed)
		    test-pathname))))