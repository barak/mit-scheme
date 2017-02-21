#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Tests of string operations

(declare (usual-integrations))

(define (allbytes)
  (let ((v8b (make-vector-8b #x100)))
    (do ((i 0 (+ i 1))) ((>= i #x100))
      (vector-8b-set! v8b i i))
    v8b))

(define (allbytes:lower)
  (string-append
   "000102030405060708090a0b0c0d0e0f"
   "101112131415161718191a1b1c1d1e1f"
   "202122232425262728292a2b2c2d2e2f"
   "303132333435363738393a3b3c3d3e3f"
   "404142434445464748494a4b4c4d4e4f"
   "505152535455565758595a5b5c5d5e5f"
   "606162636465666768696a6b6c6d6e6f"
   "707172737475767778797a7b7c7d7e7f"
   "808182838485868788898a8b8c8d8e8f"
   "909192939495969798999a9b9c9d9e9f"
   "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
   "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
   "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
   "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
   "e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
   "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))

(define (allbytes:upper)
  (string-append
   "000102030405060708090A0B0C0D0E0F"
   "101112131415161718191A1B1C1D1E1F"
   "202122232425262728292A2B2C2D2E2F"
   "303132333435363738393A3B3C3D3E3F"
   "404142434445464748494A4B4C4D4E4F"
   "505152535455565758595A5B5C5D5E5F"
   "606162636465666768696A6B6C6D6E6F"
   "707172737475767778797A7B7C7D7E7F"
   "808182838485868788898A8B8C8D8E8F"
   "909192939495969798999A9B9C9D9E9F"
   "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF"
   "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF"
   "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF"
   "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF"
   "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF"
   "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF"))

(define-test 'HEXADECIMAL->VECTOR-8B/LOWERCASE
  (lambda ()
    (assert-equal (allbytes) (hexadecimal->vector-8b (allbytes:lower)))))

(define-test 'HEXADECIMAL->VECTOR-8B/UPPERCASE
  (lambda ()
    (assert-equal (allbytes) (hexadecimal->vector-8b (allbytes:upper)))))

;; Change this test if you change the case -- and consider whether
;; applications may break if you do.
(define-test 'VECTOR-8B->HEXADECIMAL
  (lambda ()
    (assert-equal (allbytes:lower) (vector-8b->hexadecimal (allbytes)))))

(define-test 'VECTOR-8B->HEXADECIMAL/LOWERCASE
  (lambda ()
    (assert-equal (allbytes:lower)
                  (string-downcase (vector-8b->hexadecimal (allbytes))))))

(define-test 'VECTOR-8B->HEXADECIMAL/UPPERCASE
  (lambda ()
    (assert-equal (allbytes:upper)
                  (string-upcase (vector-8b->hexadecimal (allbytes))))))

(define-test 'VECTOR-8B->HEXADECIMAL->VECTOR-8B
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x100))
      (let* ((v (random-byte-vector #x100)))
        (assert-equal v
          (hexadecimal->vector-8b (vector-8b->hexadecimal v)))))))

(define-test 'VECTOR-8B->HEXADECIMAL->UPPER->VECTOR-8B
  (lambda ()
    (do ((i 0 (+ i 1))) ((>= i #x100))
      (let* ((v (random-byte-vector #x100)))
        (assert-equal v
          (hexadecimal->vector-8b
           (string-upcase (vector-8b->hexadecimal v))))))))

;;;; Tests adapted from the Larceny R7RS test suite:

(define-test 'larceny-string
  (lambda ()

    (assert-string-ci< "A" "z")
    (assert-string-ci< "A" "z")
    (assert-string-ci< "a" "Z")
    (assert-string-ci< "a" "Z")
    (assert-string-ci<= "A" "z")
    (assert-string-ci<= "A" "z")
    (assert-string-ci<= "Z" "z")
    (assert-string-ci<= "Z" "z")
    (assert-string-ci<= "a" "Z")
    (assert-string-ci<= "a" "Z")
    (assert-string-ci<= "z" "Z")
    (assert-string-ci<= "z" "Z")
    (assert-string-ci= "z" "Z")
    (assert-string-ci!= "z" "a")
    (assert-string-ci> "Z" "a")
    (assert-string-ci> "Z" "a")
    (assert-string-ci> "z" "A")
    (assert-string-ci> "z" "A")
    (assert-string-ci>= "Z" "a")
    (assert-string-ci>= "Z" "a")
    (assert-string-ci>= "Z" "z")
    (assert-string-ci>= "Z" "z")
    (assert-string-ci>= "z" "A")
    (assert-string-ci>= "z" "A")
    (assert-string-ci>= "z" "Z")
    (assert-string-ci>= "z" "Z")

    (assert-string= (string-upcase "Hi") "HI")
    (assert-string= (string-upcase "HI") "HI")
    (assert-string= (string-downcase "Hi") "hi")
    (assert-string= (string-downcase "hi") "hi")
    (assert-string= (string-foldcase "Hi") "hi")
    (assert-string= (string-foldcase "HI") "hi")
    (assert-string= (string-foldcase "hi") "hi")
    (assert-string= (string-downcase "STRASSE")  "strasse")

    (assert-string= (string-upcase "Stra\xDF;e") "STRASSE")
    (assert-string= (string-downcase "Stra\xDF;e") "stra\xDF;e")
    (assert-string= (string-foldcase "Stra\xDF;e") "strasse")
    (assert-string= (string-downcase "\x3A3;") "\x3C3;")

    (assert-string= (string-upcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x39E;\x391;\x39F;\x3A3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C2;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C3;\x3C2;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;\x3C3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C2; \x3C3;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3; \x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3; \x3C3;")
    (assert-string= (string-foldcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;")
    (assert-string= (string-upcase "\x3BE;\x3B1;\x3BF;\x3C3;")
		    "\x39E;\x391;\x39F;\x3A3;")
    (assert-string= (string-upcase "\x3BE;\x3B1;\x3BF;\x3C2;")
		    "\x39E;\x391;\x39F;\x3A3;")

    (assert-string= (string-downcase "A\x3A3;'x") ; ' is a MidLetter
		    "a\x3C3;'x")

    (assert-string-ci= "Strasse" "Stra\xDF;e")
    (assert-string-ci= "STRASSE" "Stra\xDF;e")
    (assert-string-ci= "\x3BE;\x3B1;\x3BF;\x3C2;" "\x39E;\x391;\x39F;\x3A3;")
    (assert-string-ci= "\x3BE;\x3B1;\x3BF;\x3C3;" "\x39E;\x391;\x39F;\x3A3;")))

(define-test 'string-builder
  (lambda ()
    (let ((end (length latin-alphabet)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i end)))
	(let ((chars (list-head latin-alphabet i)))
	  (let ((result (build-string chars)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (chars->string chars))))
	(let ((strings (make-test-strings i latin-alphabet #f)))
	  (let ((result (build-string strings)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (string-append* strings))))
	(let ((strings (make-test-strings i latin-alphabet #t)))
	  (let ((result (build-string strings)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (string-append* strings))))))
    (let ((end (length greek-alphabet)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i end)))
	(let ((chars (list-head greek-alphabet i)))
	  (assert-string= (build-string chars)
			  (chars->string chars)))
	(let ((strings (make-test-strings i greek-alphabet #f)))
	  (assert-string= (build-string strings)
			  (string-append* strings)))
	(let ((strings (make-test-strings i greek-alphabet #t)))
	  (assert-string= (build-string strings)
			  (string-append* strings)))))))

(define legacy-string?
  (make-primitive-procedure 'string? 1))

(define latin-alphabet
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define greek-alphabet
  '(#\x3B1 #\x3B2 #\x3B3 #\x3B4 #\x3B5
    #\x3B6 #\x3B7 #\x3B8 #\x3B9 #\x3BA
    #\x3BB #\x3BC #\x3BD #\x3BE #\x3BF
    #\x3C0 #\x3C1 #\x3C2 #\x3C3 #\x3C4
    #\x3C5 #\x3C6 #\x3C7 #\x3C8 #\x3C9))

(define (build-string objects)
  (let ((builder (string-builder)))
    (for-each builder objects)
    (builder)))

(define (chars->string chars)
  (let ((s (make-string (length chars))))
    (do ((chars chars (cdr chars))
	 (i 0 (fix:+ i 1)))
	((not (pair? chars)))
      (string-set! s i (car chars)))
    s))

(define (make-test-strings n alphabet reverse?)
  (let loop ((k 0) (strings '()))
    (if (fix:< k n)
	(loop (fix:+ k 1)
	      (cons (chars->string (list-head alphabet k))
		    strings))
	(if reverse?
	    strings
	    (reverse! strings)))))