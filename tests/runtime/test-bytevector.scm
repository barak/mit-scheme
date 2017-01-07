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
	     (bytevector-copy! (make-bytevector n) 0 v -1 n))))))))

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