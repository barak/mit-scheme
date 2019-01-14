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

;;;; Tests for binary ports

(declare (usual-integrations))

(define (random-bytevector k)
  (let ((bv (make-bytevector k)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i k)))
      (bytevector-u8-set! bv i (random-integer #x100)))
    bv))

(define bv0 (bytevector))
(define bv1 (random-bytevector 9))
(define bv2 (random-bytevector 1000))
(define bv3 (random-bytevector 4096))	;buffer-size
(define bv4 (random-bytevector 10000))
(define all-bvs (list bv0 bv1 bv2 bv3 bv4))
(define strides '(1 2 3 5 7 11))

(define-test 'peek/read
  (lambda ()
    (for-each test-peek/read all-bvs)))

(define (test-peek/read bv)
  (let ((port (open-input-bytevector bv)))
    (assert-binary-input-port port)

    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i (bytevector-length bv))))
      (assert-true (u8-ready? port))
      (assert-eqv (peek-u8 port) (bytevector-u8-ref bv i))
      (assert-true (u8-ready? port))
      (assert-eqv (read-u8 port) (bytevector-u8-ref bv i)))

    (assert-true (u8-ready? port))
    (assert-eqv (peek-u8 port) (eof-object))
    (assert-true (u8-ready? port))
    (assert-eqv (read-u8 port) (eof-object))))

(define-test 'read-bytevector
  (lambda ()
    (for-each test-read-bytevector all-bvs)))

(define (test-read-bytevector bv)
  (for-each (lambda (stride)
	      (test-read-bytevector-1 bv (lambda () stride)))
	    strides)
  (test-read-bytevector-1 bv (sequential-get-stride strides))
  (test-read-bytevector-1 bv (sequential-get-stride (reverse strides))))

(define (test-read-bytevector-1 bv get-stride)
  (let ((port (open-input-bytevector bv)))
    (assert-binary-input-port port)

    (let loop ((i 0))
      (let ((stride (get-stride))
	    (remaining (fix:- (bytevector-length bv) i)))
	(assert-true (u8-ready? port))
	(if (fix:> remaining 0)
	    (let ((i* (fix:+ i (fix:min stride remaining))))
	      (assert-equal (read-bytevector stride port)
			    (bytevector-copy bv i i*))
	      (loop i*))
	    (assert-equal (read-bytevector stride port)
			  (eof-object)))))))

(define-test 'read-bytevector!
  (lambda ()
    (for-each test-read-bytevector! all-bvs)))

(define (test-read-bytevector! bv)
  (for-each (lambda (stride)
	      (test-read-bytevector!-1 bv (lambda () stride)))
	    strides)
  (test-read-bytevector!-1 bv (sequential-get-stride strides))
  (test-read-bytevector!-1 bv (sequential-get-stride (reverse strides))))

(define (test-read-bytevector!-1 bv get-stride)
  (let ((port (open-input-bytevector bv))
	(target (make-bytevector (fix:+ (bytevector-length bv) 1))))
    (assert-binary-input-port port)

    (let loop ((i 0))
      (let ((stride (get-stride))
	    (remaining (fix:- (bytevector-length bv) i)))
	(assert-true (u8-ready? port))
	(if (fix:> remaining 0)
	    (let ((i* (fix:+ i (fix:min stride remaining))))
	      (assert-eqv (read-bytevector! target port i i*)
			  (fix:- i* i))
	      (loop i*))
	    (assert-eqv (read-bytevector! target port i (fix:+ i 1))
			(eof-object)))))
    (assert-equal (bytevector-copy target 0 (bytevector-length bv))
		  bv)))

(define-test 'write-u8
  (lambda ()
    (for-each test-write-u8 all-bvs)))

(define (test-write-u8 bv)
  (let ((port (open-output-bytevector)))
    (assert-binary-output-port port)

    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i (bytevector-length bv))))
      (write-u8 (bytevector-u8-ref bv i) port))

    (assert-equal (get-output-bytevector port) bv)))

(define-test 'write-bytevector
  (lambda ()
    (for-each test-write-bytevector all-bvs)))

(define (test-write-bytevector bv)
  (for-each (lambda (stride)
	      (test-write-bytevector-1 bv (lambda () stride)))
	    strides)
  (test-write-bytevector-1 bv (sequential-get-stride strides))
  (test-write-bytevector-1 bv (sequential-get-stride (reverse strides))))

(define (test-write-bytevector-1 bv get-stride)
  (let ((port (open-output-bytevector)))
    (assert-binary-output-port port)

    (let loop ((i 0))
      (let ((stride (get-stride))
	    (remaining (fix:- (bytevector-length bv) i)))
	(if (fix:> remaining 0)
	    (let ((i* (fix:+ i (fix:min stride remaining))))
	      (assert-eqv (write-bytevector bv port i i*)
			  (fix:- i* i))
	      (loop i*)))))

    (assert-equal (get-output-bytevector port) bv)))

(define (sequential-get-stride strides)
  (lambda ()
    (let ((stride (car strides)))
      (if (pair? (cdr strides))
	  (set! strides (cdr strides)))
      stride)))

(define (assert-binary-input-port port)
  (assert-true (binary-port? port))
  (assert-false (textual-port? port))
  (assert-true (input-port? port))
  (assert-false (output-port? port))
  (assert-false (i/o-port? port)))

(define (assert-binary-output-port port)
  (assert-true (binary-port? port))
  (assert-false (textual-port? port))
  (assert-false (input-port? port))
  (assert-true (output-port? port))
  (assert-false (i/o-port? port)))

(define-test 'position
  (lambda ()
    (call-with-temporary-file-pathname
      (lambda (pathname)
	(call-with-binary-output-file pathname
	  (lambda (port)
	    (assert-= (binary-port-position port) 0)
	    (write-u8 42 port)
	    (assert-= (binary-port-position port) 1)
	    (write-bytevector (make-bytevector 1000 0) port)
	    (assert-= (binary-port-position port) 1001)))
	(call-with-binary-input-file pathname
	  (lambda (port)
	    (assert-= (binary-port-position port) 0)
	    (assert-= (read-u8 port) 42)
	    (assert-= (binary-port-position port) 1)
	    (assert-equal (read-bytevector 1000 port)
			  (make-bytevector 1000 0))
	    (assert-= (binary-port-position port) 1001)))))))