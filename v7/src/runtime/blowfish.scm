#| -*-Scheme-*-

$Id: blowfish.scm,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1997, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Interface to Blowfish and MD5
;;; package: ()

(declare (usual-integrations))

(define-primitives
  (md5 1)
  (md5-init 0)
  (md5-update 4)
  (md5-final 1)
  (blowfish-set-key 1)
  (blowfish-cbc 4)
  (blowfish-cfb64 5)
  (blowfish-cfb64-substring 7))

(define (blowfish-available?)
  (and (implemented-primitive-procedure? md5-init)
       (implemented-primitive-procedure? blowfish-cfb64-substring)))

(define (blowfish-encrypt-string plaintext key-string encrypt?)
  (blowfish-encrypt-substring plaintext 0 (string-length plaintext)
			      key-string encrypt?))

(define (blowfish-encrypt-substring plaintext start end key-string encrypt?)
  (blowfish-cfb64-substring plaintext start end
			    (blowfish-set-key (md5 key-string))
			    (make-string 8 #\NUL)
			    0
			    encrypt?))

(define (blowfish-encrypt-port input output key-string encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key (md5 key-string)))
	(buffer (make-string 4096))
	(init-vector (make-string 8 #\NUL)))
    (let loop ((m 0))
      (let ((n (input-port/read-string! input buffer)))
	(if (not (fix:= 0 n))
	    (begin
	      (write-string (blowfish-cfb64-substring buffer 0 n
						      key init-vector m
						      encrypt?)
			    output)
	      (loop (fix:and #x7 (fix:+ m n)))))))))

(define (write-blowfish-file-header port)
  (write-string blowfish-file-header port)
  (newline port))

(define (read-blowfish-file-header port)
  (if (not (string=? (read-line port) blowfish-file-header))
      (error "Not a Blowfish file:" port)))

(define blowfish-file-header
  "Blowfish, 16 rounds")

(define (md5-file filename)
  (call-with-binary-input-file filename
    (lambda (port)
      (let ((buffer (make-string 4096))
	    (context (md5-init)))
	(let loop ()
	  (let ((n (read-string! buffer 0 4096 port)))
	    (if (fix:= 0 n)
		(md5-final context)
		(begin
		  (md5-update context buffer 0 n)
		  (loop)))))))))

(define (md5-sum->number sum)
  (let ((l (string-length sum)))
    (do ((i 0 (fix:+ i 1))
	 (n 0 (+ (* n #x100) (vector-8b-ref sum i))))
	((fix:= i l) n))))

(define (md5-sum->hexadecimal sum)
  (let ((s (number->string (md5-sum->number sum) 16)))
    (string-downcase! s)
    (let ((d (fix:- 32 (string-length s))))
    (if (fix:> d 0)
	(string-append (make-string d #\0) s)
	s))))