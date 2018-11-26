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

;;;; Tests for READ and WRITE

(declare (usual-integrations))

(define (define-enumerated-test name cases procedure)
  (define-test name
    (map (lambda (casen)
	   (lambda ()
	     (with-test-properties (lambda () (apply procedure casen))
	       'DESCRIPTION (write-to-string casen))))
	 cases)))

(define (with-expected-failure xfail? body)
  (case xfail?
    ((xfail) (expect-failure body))
    ((xerror) (assert-error body))
    (else (body))))

(define assert-nan
  (predicate-assertion nan? "NaN"))

(define assert-inf
  (predicate-assertion infinite? "infinity"))

(define (assert-inf- x)
  (assert-inf x)
  (assert-< x 0))

(define (assert-inf+ x)
  (assert-inf x)
  (assert-< 0 x))

(define assert-symbol
  (predicate-assertion symbol? "symbol"))

(define assert-string
  (predicate-assertion string? "string"))

(define assert-exact-integer
  (predicate-assertion exact-integer? "exact integer"))

(define assert-exact-rational
  (predicate-assertion exact-rational? "exact rational"))

(define assert-real
  (predicate-assertion real? "real number"))

(define (complex-nonreal? object)
  (and (complex? object)
       (not (real? object))))

(define assert-complex-nonreal
  (predicate-assertion complex-nonreal? "complex nonreal"))

(define assert-flonum
  (predicate-assertion flo:flonum? "flonum"))

(define (read-from-string string)
  (read (open-input-string string)))

(define-test 'DOT-SYMBOL-GETS-BARS
  (lambda ()
    (assert-equal (write-to-string (string->symbol ".")) "|.|")))

(define-enumerated-test 'read/write-invariance
  `(("+inf.0" ,assert-inf+)
    ("-inf.0" ,assert-inf-)
    ("inf.0" ,assert-symbol)
    ("nan.0" ,assert-symbol)
    ("123" ,assert-exact-integer)
    ("1/34" ,assert-exact-rational)
    ("123+456i" ,assert-complex-nonreal)
    ("-0.i" ,assert-real)               ;real?
    ("0.-0.i" ,assert-real)             ;real?
    ("-0.-0.i" ,assert-real)            ;real?
    ("1.23" ,assert-flonum)
    ("+inf.0i" ,assert-complex-nonreal)
    ("-inf.0i" ,assert-complex-nonreal)
    ("1+inf.0i" ,assert-complex-nonreal)
    ("1-inf.0i" ,assert-complex-nonreal)
    ("2+inf.0i" ,assert-complex-nonreal)
    ("2-inf.0i" ,assert-complex-nonreal)
    ("+inf.0+i" ,assert-complex-nonreal)
    ("-inf.0+i" ,assert-complex-nonreal)
    ("+inf.0+2i" ,assert-complex-nonreal)
    ("-inf.0+2i" ,assert-complex-nonreal)
    ("+inf.0+inf.0i" ,assert-complex-nonreal)
    ("+inf.0-inf.0i" ,assert-complex-nonreal)
    ("-inf.0+inf.0i" ,assert-complex-nonreal)
    ("-inf.0-inf.0i" ,assert-complex-nonreal)
    ("+inf.0+nan.0i" ,assert-complex-nonreal)
    ("+nan.0+inf.0i" ,assert-complex-nonreal)
    ("\"|\"" ,assert-string xfail)
    ("\"\\\"\"" ,assert-string)
    ("\"\\\\\"" ,assert-string))
  (lambda (string #!optional assertion xfail?)
    (with-expected-failure xfail?
      (lambda ()
	(let ((object (read-from-string string)))
	  (assertion object)
	  (assert-equal (write-to-string object) string))))))

(define-enumerated-test 'read/write-invariance-hex
  `(("+inf.0" ,assert-inf+)
    ("-inf.0" ,assert-inf-)
    ("inf.0" ,assert-symbol)
    ("nan.0" ,assert-symbol)
    ("#x123" ,assert-exact-integer)
    ("#x1/34" ,assert-exact-rational)
    ("#x123+456i" ,assert-complex-nonreal)
    ("#x1.23p+4-1.ffp-8i" ,assert-complex-nonreal)
    ("#x1.23p+0" ,assert-flonum)
    ("#x+inf.0i" ,assert-complex-nonreal)
    ("#x-inf.0i" ,assert-complex-nonreal)
    ("#x1+inf.0i" ,assert-complex-nonreal)
    ("#x1-inf.0i" ,assert-complex-nonreal)
    ("#x+inf.0+i" ,assert-complex-nonreal)
    ("#x-inf.0+i" ,assert-complex-nonreal)
    ("#x+inf.0-i" ,assert-complex-nonreal)
    ("#x-inf.0-i" ,assert-complex-nonreal)
    ("#x1p+1+inf.0i" ,assert-complex-nonreal)
    ("#x1p+1-inf.0i" ,assert-complex-nonreal)
    ("#x-1p+1+inf.0i" ,assert-complex-nonreal)
    ("#x-1p+1-inf.0i" ,assert-complex-nonreal)
    ("#x+inf.0+1p+1i" ,assert-complex-nonreal)
    ("#x-inf.0+1p+1i" ,assert-complex-nonreal)
    ("#x+inf.0-1p+1i" ,assert-complex-nonreal)
    ("#x-inf.0-1p+1i" ,assert-complex-nonreal)
    ("#x+inf.0+inf.0i" ,assert-complex-nonreal)
    ("#x+inf.0-inf.0i" ,assert-complex-nonreal)
    ("#x-inf.0+inf.0i" ,assert-complex-nonreal)
    ("#x-inf.0-inf.0i" ,assert-complex-nonreal)
    ("#x+inf.0+nan.0i" ,assert-complex-nonreal)
    ("#x+nan.0+inf.0i" ,assert-complex-nonreal))
  (lambda (string #!optional assertion xfail?)
    (with-expected-failure xfail?
      (lambda ()
	(let ((object
               (parameterize ((param:reader-radix #x10))
		 (read-from-string string))))
	  (assertion object)
	  (let ((string*
		 (parameterize ((param:printer-radix #x10))
		   (write-to-string object))))
	    (assert-equal string* string)))))))

(define-enumerated-test 'read
  `(("+nan.0" ,assert-nan)
    ("-nan.0" ,assert-nan))
  (lambda (string assertion #!optional xfail?)
    (with-expected-failure xfail?
      (lambda ()
	(assertion (read-from-string string))))))