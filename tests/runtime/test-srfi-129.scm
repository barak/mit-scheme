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

#| Copyright of original tests from which these are derived:

Copyright (C) John Cowan (2015). All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

;;;; Tests of SRFI 129

#!no-fold-case

(define-test 'features
  (lambda ()
    (assert-true (cond-expand (srfi-129 #t) (else #f))
		 'expectation-description
		 "srfi-129 is a supported feature")
    (assert-true (cond-expand ((library (srfi 129)) #t) (else #f))
		 'expectation-description
		 "(srfi 129) is a registered library")))

(define-test 'char-title-case?
  (lambda ()
    (assert-true (char-title-case? #\x01C5))
    (assert-true (char-title-case? #\x1FFC))
    (assert-false (char-title-case? #\Z))
    (assert-false (char-title-case? #\z))))

(define-test 'char-titlecase
  (lambda ()
    (assert-eqv (char-titlecase #\x01C4) #\x01C5)
    (assert-eqv (char-titlecase #\x01C6) #\x01C5)
    (assert-eqv (char-titlecase #\Z) #\Z)
    (assert-eqv (char-titlecase #\z) #\Z)))

(define-test 'string-titlecase
  (lambda ()
    (assert-string= (string-titlecase "\x01C5;") "\x01C5;")
    (assert-string= (string-titlecase "\x01C4;") "\x01C5;")
    (assert-string= (string-titlecase "\x00DF;") "Ss")
    (assert-string= (string-titlecase "x\x0130;") "Xi\x0307;")
    (assert-string= (string-titlecase "\x1F80;") "\x1F88;")
    (assert-string= (string-titlecase "\x1F88;") "\x1F88;")
    (define Floo "\xFB02;oo")
    (define Floo-bar "\xFB02;oo bar")
    (define Baffle "Ba\xFB04;e")
    (define LJUBLJANA "\x01C7;ub\x01C7;ana")
    (define Ljubljana "\x01C8;ub\x01C9;ana")
    (define ljubljana "\x01C9;ub\x01C9;ana")
    (assert-string= (string-titlecase "bAr baZ") "Bar Baz")
    (assert-string= (string-titlecase "floo") "Floo")
    (assert-string= (string-titlecase "FLOO") "Floo")
    (assert-string= (string-titlecase Floo) "Floo")
    (assert-string= (string-titlecase "floo bar") "Floo Bar")
    (assert-string= (string-titlecase "FLOO BAR") "Floo Bar")
    (assert-string= (string-titlecase Floo-bar) "Floo Bar")
    (assert-string= (string-titlecase Baffle) Baffle)
    (assert-string= (string-titlecase LJUBLJANA) Ljubljana)
    (assert-string= (string-titlecase Ljubljana) Ljubljana)
    (assert-string= (string-titlecase ljubljana) Ljubljana)))