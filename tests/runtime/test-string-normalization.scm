#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Tests of string normalization

(declare (usual-integrations))

(define normalization-test-cases
  (map (lambda (columns)
	 (map list->string columns))
       (read-file
	(merge-pathnames "test-string-normalization-data"
			 (directory-pathname (current-load-pathname))))))

(define (norm-tc-source tc) (car tc))
(define (norm-tc-nfc tc) (cadr tc))
(define (norm-tc-nfd tc) (caddr tc))
(define (norm-tc-nfkc tc) (cadddr tc))
(define (norm-tc-nfkd tc) (car (cddddr tc)))

(define (nfc-test source expected)
  (lambda ()
    (with-test-properties
     (lambda ()
       (assert-ts= (string->nfc source)
		   expected))
     'expression `(string->nfc ,source))))

(define-test 'string->nfc
  (map (lambda (tc)
	 (list (nfc-test (norm-tc-source tc) (norm-tc-nfc tc))
	       (nfc-test    (norm-tc-nfc tc) (norm-tc-nfc tc))
	       (nfc-test    (norm-tc-nfd tc) (norm-tc-nfc tc))
	       (nfc-test   (norm-tc-nfkc tc) (norm-tc-nfkc tc))
	       (nfc-test   (norm-tc-nfkd tc) (norm-tc-nfkc tc))))
       normalization-test-cases))

(define (nfd-test source expected)
  (lambda ()
    (with-test-properties
     (lambda ()
       (assert-ts= (string->nfd source)
		   expected))
     'expression `(string->nfd ,source))))

(define-test 'string->nfd
  (map (lambda (tc)
	 (list (nfd-test (norm-tc-source tc) (norm-tc-nfd tc))
	       (nfd-test    (norm-tc-nfc tc) (norm-tc-nfd tc))
	       (nfd-test    (norm-tc-nfd tc) (norm-tc-nfd tc))
	       (nfd-test   (norm-tc-nfkc tc) (norm-tc-nfkd tc))
	       (nfd-test   (norm-tc-nfkd tc) (norm-tc-nfkd tc))))
       normalization-test-cases))

(define (trivial-string=? s1 s2)
  (let ((n (string-length s1)))
    (and (fix:= n (string-length s2))
	 (let loop ((i 0))
	   (if (fix:< i n)
	       (and (char=? (string-ref s1 i)
			    (string-ref s2 i))
		    (loop (fix:+ i 1)))
	       #t)))))

(define-comparator trivial-string=? 'string=?)
(define assert-ts= (simple-binary-assertion trivial-string=? #f))