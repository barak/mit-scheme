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

;;;; Interface to Blowfish
;;; package: (runtime blowfish)

(declare (usual-integrations))

(define-primitives
  (blowfish-set-key 1)
  (blowfish-ecb 4)
  (blowfish-cbc blowfish-cbc-v2 5)
  (blowfish-cfb64 blowfish-cfb64-substring-v2 9)
  (blowfish-ofb64 blowfish-ofb64-substring 8))

(define (blowfish-available?)
  (load-library-object-file "prbfish" #f)
  (implemented-primitive-procedure?
   (ucode-primitive blowfish-cfb64-substring-v2 9)))

(define (blowfish-encrypt-port input output key init-vector encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key key))
	(input-buffer (make-bytevector 4096))
	(output-buffer (make-bytevector 4096)))
    (dynamic-wind
     (lambda ()
       unspecific)
     (lambda ()
       (let loop ((m 0))
	 (let ((n (read-bytevector! input-buffer input)))
	   (if (and n (not (eof-object? n)))
	       (let ((m
		      (blowfish-cfb64 input-buffer 0 n output-buffer 0
				      key init-vector m encrypt?)))
		 (let ((n* (write-bytevector output-buffer output 0 n)))
		   (if (not (eqv? n n*))
		       (error "Short write (requested, actual):" n n*)))
		 (loop m))))))
     (lambda ()
       (bytevector-fill! input-buffer 0)
       (bytevector-fill! output-buffer 0)))))

(define (compute-blowfish-init-vector)
  ;; This init vector includes a timestamp with a resolution of
  ;; milliseconds, plus 20 random bits.  This should make it very
  ;; difficult to generate two identical vectors.
  (let ((iv (make-bytevector 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (+ (* (+ (* (get-universal-time) 1000)
		     (remainder (real-time-clock) 1000))
		  #x100000)
	       (random #x100000))
	    (quotient t #x100)))
	((not (fix:< i 8)))
      (bytevector-u8-set! iv i (remainder t #x100)))
    iv))

(define (write-blowfish-file-header port)
  (write-bytevector blowfish-file-header-v2 port)
  (let ((init-vector (compute-blowfish-init-vector)))
    (write-bytevector init-vector port)
    init-vector))

(define (read-blowfish-file-header port)
  (let ((version (try-read-blowfish-file-header port)))
    (if (not version)
	(error:bad-range-argument port 'read-blowfish-file-header))
    (if (= version 1)
	(make-bytevector 8 0)
	(or (%safe-read-bytevector 8 port)
	    (error "Short read while getting init-vector:" port)))))

(define (try-read-blowfish-file-header port)
  (let* ((n (bytevector-length blowfish-file-header-v1))
	 (bv1 (%safe-read-bytevector n port)))
    (and bv1
	 (if (bytevector=? bv1 blowfish-file-header-v1)
	     1
	     (let* ((m (fix:- (bytevector-length blowfish-file-header-v2) n))
		    (bv2 (%safe-read-bytevector m port)))
	       (and bv2
		    (bytevector=? (bytevector-append bv1 bv2)
				  blowfish-file-header-v2)
		    2))))))

(define (%safe-read-bytevector n port)
  (let ((bv (read-bytevector n port)))
    (and bv
	 (not (eof-object? bv))
	 (fix:= (bytevector-length bv) n)
	 bv)))

(define (blowfish-file? pathname)
  (call-with-binary-input-file pathname try-read-blowfish-file-header))

(define-deferred blowfish-file-header-v1
  (string->utf8 "Blowfish, 16 rounds\n"))

(define-deferred blowfish-file-header-v2
  (string->utf8 "Blowfish, 16 rounds, version 2\n"))