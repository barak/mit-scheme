#| -*-Scheme-*-

$Id: blowfish.scm,v 1.24 2001/12/18 18:39:19 cph Exp $

Copyright (c) 1997-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Interface to Blowfish
;;; package: (runtime blowfish)

(declare (usual-integrations))

(define blowfish-set-key (ucode-primitive blowfish-set-key 1))
(define blowfish-ecb (ucode-primitive blowfish-ecb 4))
(define blowfish-cbc (ucode-primitive blowfish-cbc-v2 5))
(define blowfish-cfb64 (ucode-primitive blowfish-cfb64-substring-v2 9))
(define blowfish-ofb64 (ucode-primitive blowfish-ofb64-substring 8))

(define (blowfish-available?)
  (load-library-object-file "prbfish" #f)
  (implemented-primitive-procedure? blowfish-cfb64))

(define (blowfish-encrypt-port input output key init-vector encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key key))
	(input-buffer (make-string 4096))
	(output-buffer (make-string 4096)))
    (dynamic-wind
     (lambda ()
       unspecific)
     (lambda ()
       (let loop ((m 0))
	 (let ((n (input-port/read-string! input input-buffer)))
	   (if (not (fix:= 0 n))
	       (let ((m
		      (blowfish-cfb64 input-buffer 0 n output-buffer 0
				      key init-vector m encrypt?)))
		 (write-substring output-buffer 0 n output)
		 (loop m))))))
     (lambda ()
       (string-fill! input-buffer #\NUL)
       (string-fill! output-buffer #\NUL)))))

(define (compute-blowfish-init-vector)
  ;; This init vector includes a timestamp with a resolution of
  ;; milliseconds, plus 20 random bits.  This should make it very
  ;; difficult to generate two identical vectors.
  (let ((iv (make-string 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (+ (* (+ (* (get-universal-time) 1000)
		     (remainder (real-time-clock) 1000))
		  #x100000)
	       (random #x100000))
	    (quotient t #x100)))
	((fix:= 8 i))
      (vector-8b-set! iv i (remainder t #x100)))
    iv))

(define (write-blowfish-file-header port)
  (write-string blowfish-file-header-v2 port)
  (newline port)
  (let ((init-vector (compute-blowfish-init-vector)))
    (write-string init-vector port)
    init-vector))

(define (read-blowfish-file-header port)
  (let ((line (read-line port)))
    (cond ((string=? blowfish-file-header-v1 line)
	   (make-string 8 #\NUL))
	  ((string=? blowfish-file-header-v2 line)
	   (let ((init-vector (make-string 8)))
	     (if (not (= 8 (read-substring! init-vector 0 8 port)))
		 (error "Short read while getting init-vector:" port))
	     init-vector))
	  (else
	   (error:bad-range-argument port 'READ-BLOWFISH-FILE-HEADER)))))

(define (blowfish-file? pathname)
  (let ((line (call-with-binary-input-file pathname read-line)))
    (and (not (eof-object? line))
	 (or (string=? line blowfish-file-header-v1)
	     (string=? line blowfish-file-header-v2)))))

(define blowfish-file-header-v1 "Blowfish, 16 rounds")
(define blowfish-file-header-v2 "Blowfish, 16 rounds, version 2")