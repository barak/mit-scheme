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

;;;; Tests for bytevectors

(declare (usual-integrations))

(define-test 'make-bytevector
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((v (make-bytevector n)))
	(assert-true (bytevector? v))
	(assert-= (bytevector-length v) n)
	(do ((i 0 (+ i 1)))
	    ((not (< i n)))
	  (assert-< (bytevector-u8-ref v i) #x100)))
      (test-bytevector-properties (make-bytevector n 0)
				  (make-list n 0))
      (test-bytevector-properties (make-bytevector n 51)
				  (make-list n 51)))))

(define-test 'bytevector
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((bytes (reverse (iota n))))
	(test-bytevector-properties (apply bytevector bytes) bytes)))))

(define-test 'non-vector
  (lambda ()
    (let ((v (vector (reverse (iota 5)))))
      (assert-false (bytevector? v))
      (assert-type-error (lambda () (bytevector-length v)))
      (assert-type-error (lambda () (bytevector-u8-ref v 0)))
      (assert-type-error (lambda () (bytevector-u8-set! v 0 7)))
      (assert-type-error (lambda () (bytevector-append v v)))
      (assert-type-error (lambda () (bytevector-copy v 0 1))))))

(define-test 'bytevector=?
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((bytes (reverse (iota n))))
	;; compare two constructors
	(let ((v1 (apply bytevector bytes))
	      (v2 (list->bytevector bytes)))
	  (assert-true (bytevector=? v1 v2))
	  (assert-true (equal? v1 v2)))
	(if (pair? bytes)
	    (begin
	      ;; different lengths
	      (let ((v1 (apply bytevector bytes))
		    (v2 (list->bytevector (except-last-pair bytes))))
		(assert-false (bytevector=? v1 v2))
		(assert-false (equal? v1 v2)))
	      ;; different element
	      (do ((i 0 (+ i 1)))
		  ((not (< i n)))
		(let ((v1 (apply bytevector bytes))
		      (v2 (list->bytevector bytes)))
		  (bytevector-u8-set! v2 i n)
		  (assert-false (bytevector=? v1 v2))
		  (assert-false (equal? v1 v2))))))))))

(define-test 'bytevector-append
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((b1 (reverse (iota n)))
	    (b2 (iota n)))
	(let ((v1 (apply bytevector b1))
	      (v2 (apply bytevector b2)))
	  (test-bytevector-properties (bytevector-append v1 v2)
				      (append b1 b2)))))))

(define-test 'bytevector-copy
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((bytes (reverse (iota n))))
	(let ((v (apply bytevector bytes)))
	  (do ((end 0 (+ end 1)))
	      ((> end n))
	    (do ((start 0 (+ start 1)))
		((> start end))
	      (assert-equal (bytevector-copy v start end)
			    (apply bytevector (sublist bytes start end)))))
	  (assert-range-error (lambda () (bytevector-copy v 0 (+ n 1))))
	  (assert-range-error (lambda () (bytevector-copy v n (+ n 1))))
	  (assert-range-error (lambda () (bytevector-copy v -1 n))))))))

(define-test 'bytevector-copy!
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((bytes (reverse (iota n))))
	(let ((v (apply bytevector bytes)))
	  (do ((end 0 (+ end 1)))
	      ((> end n))
	    (do ((start 0 (+ start 1)))
		((> start end))
	      (let ((extra (- n (- end start))))
		(do ((to 0 (+ to 1)))
		    ((> to extra))
		  (let ((target (make-bytevector n n)))
		    (bytevector-copy! target to v start end)
		    (assert-equal
		     target
		     (apply bytevector
			    (append (make-list to n)
				    (sublist bytes start end)
				    (make-list (- extra to) n)))))))))
	  (assert-range-error
	   (lambda ()
	     (bytevector-copy! (make-bytevector n) 0 v 0 (+ n 1))))
	  (assert-range-error
	   (lambda ()
	     (bytevector-copy! (make-bytevector n) 0 v n (+ n 1))))
	  (assert-range-error
	   (lambda ()
	     (bytevector-copy! (make-bytevector n) 0 v -1 n))))))
    ;; shift left
    (let* ((bytes (reverse (iota 16)))
	   (v (apply bytevector bytes)))
      (bytevector-copy! v 3 v 5 9)
      (assert-equal v (bytevector 15 14 13 10 9 8 7 8 7 6 5 4 3 2 1 0)))
    ;; shift right
    (let* ((bytes (reverse (iota 16)))
	   (v (apply bytevector bytes)))
      (bytevector-copy! v 5 v 3 7)
      (assert-equal v (bytevector 15 14 13 12 11 12 11 10 9 6 5 4 3 2 1 0)))))

(define-test 'bytevector-fill!
  (lambda ()
    (do ((n 0 (+ n 1)))
	((not (< n 16)))
      (let ((bytes (reverse (iota n))))
	(do ((end 0 (+ end 1)))
	    ((> end n))
	  (do ((start 0 (+ start 1)))
	      ((> start end))
	    (let ((v (apply bytevector bytes)))
	      (bytevector-fill! v 51 start end)
	      (assert-equal v
			    (apply bytevector
				   (append (sublist bytes 0 start)
					   (make-list (- end start) 51)
					   (sublist bytes end n))))))))
      (assert-range-error
       (lambda ()
	 (bytevector-fill! (make-bytevector n) 51 0 (+ n 1))))
	  (assert-range-error
	   (lambda ()
	     (bytevector-fill! (make-bytevector n) 51 n (+ n 1))))
	  (assert-range-error
	   (lambda ()
	     (bytevector-fill! (make-bytevector n) 51 -1 n))))))

(define (test-bytevector-properties v bytes)
  (assert-true (bytevector? v))
  (assert-= (bytevector-length v) (length bytes))
  (do ((bytes bytes (cdr bytes))
       (index 0 (+ index 1)))
      ((not (pair? bytes)))
    (assert-= (bytevector-u8-ref v index) (car bytes)))
  (assert-range-error (lambda () (bytevector-u8-ref v -1)))
  (assert-range-error (lambda () (bytevector-u8-ref v (length bytes)))))

(define (list->bytevector bytes)
  (let ((v (make-bytevector (length bytes))))
    (do ((bytes bytes (cdr bytes))
	 (i 0 (+ i 1)))
	((not (pair? bytes)))
      (bytevector-u8-set! v i (car bytes)))
    v))

(define-test 'bytevector-u16-ref
  (lambda ()
    (do ((i 0 (+ i 1)))
	((not (< i 16)))
      (test-bytevector-u16-ref (iota i)))))

(define (test-bytevector-u16-ref bytes)
  (let ((bv (apply bytevector bytes))
	(index-limit (- (length bytes) 1)))
    (assert-u16-refs bv bytes)
    (assert-range-error (lambda () (bytevector-u16be-ref bv -1)))
    (assert-range-error (lambda () (bytevector-u16le-ref bv -1)))
    (assert-range-error (lambda () (bytevector-u16be-ref bv index-limit)))
    (assert-range-error (lambda () (bytevector-u16le-ref bv index-limit)))))

(define-test 'bytevector-u16-set!
  (lambda ()
    (do ((i 2 (+ i 1)))
	((not (< i 16)))
      (do ((j 0 (+ j 1)))
	  ((not (< (+ j 1) i)))
	(test-bytevector-u16-set! (iota i) j #xFFFE)))))

(define (test-bytevector-u16-set! bytes index-to-set value-to-set)
  (let ((value-as-bytes
	 (list (quotient value-to-set #x100)
	       (remainder value-to-set #x100))))
    (test-bytevector-u16-set!-1 bytevector-u16be-set! value-as-bytes
				bytes index-to-set value-to-set)
    (test-bytevector-u16-set!-1 bytevector-u16le-set! (reverse value-as-bytes)
				bytes index-to-set value-to-set)))

(define (test-bytevector-u16-set!-1 setter value-as-bytes
				    bytes index-to-set value-to-set)
  (let ((bv (apply bytevector bytes))
	(index-limit (- (length bytes) 1))
	(expected-bytes
	 (append (take bytes index-to-set)
		 value-as-bytes
		 (drop bytes (+ index-to-set 2)))))
    (setter bv index-to-set value-to-set)
    (assert-u16-refs bv expected-bytes)
    (assert-range-error (lambda () (setter bv -1 value-to-set)))
    (assert-range-error (lambda () (setter bv index-limit value-to-set)))))

(define (assert-u16-refs bv bytes)
  (do ((bytes bytes (cdr bytes))
       (index 0 (+ index 1)))
      ((not (>= (length bytes) 2)))
    (assert-= (bytevector-u16be-ref bv index)
	      (+ (* (car bytes) #x100)
		 (cadr bytes)))
    (assert-= (bytevector-u16le-ref bv index)
	      (+ (* (cadr bytes) #x100)
		 (car bytes)))))

(define-test 'u32-implementation
  (lambda ()
    ;; This will fail on 32-bit machines if the wrong implementation is used:
    (assert-true (u32? #xFFFFFFFF))
    ;; This should fail for either implementation:
    (assert-false (u32? #x100000000))))

(define-test 'bytevector-u32-ref
  (lambda ()
    (do ((i 0 (+ i 1)))
	((not (< i 32)))
      (test-bytevector-u32-ref (iota i)))))

(define (test-bytevector-u32-ref bytes)
  (let ((bv (apply bytevector bytes))
	(index-limit (- (length bytes) 1)))
    (assert-u32-refs bv bytes)
    (assert-range-error (lambda () (bytevector-u32be-ref bv -1)))
    (assert-range-error (lambda () (bytevector-u32le-ref bv -1)))
    (assert-range-error (lambda () (bytevector-u32be-ref bv index-limit)))
    (assert-range-error (lambda () (bytevector-u32le-ref bv index-limit)))))

(define-test 'bytevector-u32-set!
  (lambda ()
    (do ((i 2 (+ i 1)))
	((not (< i 32)))
      (do ((j 0 (+ j 1)))
	  ((not (< (+ j 3) i)))
	(test-bytevector-u32-set! (iota i) j #xFFFEFDFC)))))

(define (test-bytevector-u32-set! bytes index-to-set value-to-set)
  (let ((value-as-bytes
	 (list (quotient value-to-set #x1000000)
	       (remainder (quotient value-to-set #x10000) #x100)
	       (remainder (quotient value-to-set #x100) #x100)
	       (remainder value-to-set #x100))))
    (test-bytevector-u32-set!-1 bytevector-u32be-set! value-as-bytes
				bytes index-to-set value-to-set)
    (test-bytevector-u32-set!-1 bytevector-u32le-set! (reverse value-as-bytes)
				bytes index-to-set value-to-set)))

(define (test-bytevector-u32-set!-1 setter value-as-bytes
				    bytes index-to-set value-to-set)
  (let ((bv (apply bytevector bytes))
	(index-limit (- (length bytes) 1))
	(expected-bytes
	 (append (take bytes index-to-set)
		 value-as-bytes
		 (drop bytes (+ index-to-set 4)))))
    (setter bv index-to-set value-to-set)
    (assert-u32-refs bv expected-bytes)
    (assert-range-error (lambda () (setter bv -1 value-to-set)))
    (assert-range-error (lambda () (setter bv index-limit value-to-set)))))

(define (assert-u32-refs bv bytes)
  (do ((bytes bytes (cdr bytes))
       (index 0 (+ index 1)))
      ((not (>= (length bytes) 4)))
    (assert-= (bytevector-u32be-ref bv index)
	      (+ (* (car bytes) #x1000000)
		 (* (cadr bytes) #x10000)
		 (* (caddr bytes) #x100)
		 (cadddr bytes)))
    (assert-= (bytevector-u32le-ref bv index)
	      (+ (* (cadddr bytes) #x1000000)
		 (* (caddr bytes) #x10000)
		 (* (cadr bytes) #x100)
		 (car bytes)))))

;;;; Hexadecimal conversions

(define (allbytes)
  (let ((bv (make-bytevector #x100)))
    (do ((i 0 (+ i 1)))
	((not (< i #x100)))
      (bytevector-u8-set! bv i i))
    bv))

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

(define-test 'hexadecimal->bytevector/lowercase
  (lambda ()
    (assert-equal (hexadecimal->bytevector (allbytes:lower)) (allbytes))))

(define-test 'hexadecimal->bytevector/uppercase
  (lambda ()
    (assert-equal (hexadecimal->bytevector (allbytes:upper)) (allbytes))))

;; Change this test if you change the case -- and consider whether
;; applications may break if you do.
(define-test 'bytevector->hexadecimal
  (lambda ()
    (assert-equal (bytevector->hexadecimal (allbytes)) (allbytes:upper))))

(define-test 'bytevector->hexadecimal/lowercase
  (lambda ()
    (assert-equal (string-downcase (bytevector->hexadecimal (allbytes)))
                  (allbytes:lower))))

(define-test 'bytevector->hexadecimal/uppercase
  (lambda ()
    (assert-equal (string-upcase (bytevector->hexadecimal (allbytes)))
                  (allbytes:upper))))

(define-test 'bytevector->hexadecimal->bytevector
  (lambda ()
    (do ((i 0 (+ i 1)))
	((not (< i #x100)))
      (let* ((v (random-bytevector #x100)))
        (assert-equal (hexadecimal->bytevector (bytevector->hexadecimal v))
		      v)))))

(define-test 'bytevector->hexadecimal->upper->bytevector
  (lambda ()
    (do ((i 0 (+ i 1)))
	((not (< i #x100)))
      (let ((v (random-bytevector #x100)))
        (assert-equal (hexadecimal->bytevector
		       (string-upcase (bytevector->hexadecimal v)))
		      v)))))

(define-test 'bytevector-builder
  (lambda ()
    (let* ((end #x100)
	   (bv (random-bytevector end)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i end)))
	(let ((bytes (bytevector->list bv 0 i)))
	  (assert-equal (build-bytevector bytes) (bytes->bv bytes)))
	(let ((bvs (make-test-bvs (fix:+ i 1) bv)))
	  (assert-equal (build-bytevector bvs)
			(apply bytevector-append bvs))))
      (let ((bv1 (bytevector-copy bv 64 81)))
	(assert-equal (build-bytevector (append (bytevector->list bv 0 11)
						(list bv1)
						(bytevector->list bv 11 22)
						(list bv1)))
		      (bytevector-append (bytevector-copy bv 0 11)
					 bv1
					 (bytevector-copy bv 11 22)
					 bv1))))))

(define-test 'bytevector<?
  (lambda ()
    (let ((lists (random-byte-lists 16 64)))
      (for-each (lambda (l1)
		  (for-each (lambda (l2)
			      (assert-eqv (bytevector<? (bytes->bv l1)
							(bytes->bv l2))
					  (byte-list<? l1 l2)))
			    lists))
		lists))))

(define (byte-list<? l1 l2)
  (if (and (pair? l1) (pair? l2))
      (if (fix:= (car l1) (car l2))
	  (byte-list<? (cdr l1) (cdr l2))
	  (fix:< (car l1) (car l2)))
      (and (null? l1)
	   (not (null? l2)))))

(define (build-bytevector objects)
  (let ((builder (bytevector-builder)))
    (for-each builder objects)
    (builder)))

(define (bytes->bv bytes)
  (let ((bv (make-bytevector (length bytes))))
    (do ((bytes bytes (cdr bytes))
	 (i 0 (fix:+ i 1)))
	((not (pair? bytes)))
      (bytevector-u8-set! bv i (car bytes)))
    bv))

(define (make-test-bvs n bv)
  (let ((end (bytevector-length bv)))
    (let loop ((start 0) (bvs '()))
      (let ((start* (fix:+ start n)))
	(if (fix:<= start* end)
	    (loop start* (cons (bytevector-copy bv start start*) bvs))
	    (reverse! bvs))))))

(define (random-byte-lists n max-length)
  (map (lambda (i)
	 (declare (ignore i))
	 (random-byte-list max-length))
       (iota n)))

(define (random-byte-list max-length)
  (map (lambda (i)
	 (declare (ignore i))
	 (random #x100))
       (iota (random (+ max-length 1)))))