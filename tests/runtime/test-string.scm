#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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
