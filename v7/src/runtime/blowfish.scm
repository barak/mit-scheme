#| -*-Scheme-*-

$Id: blowfish.scm,v 1.2 1997/06/09 08:08:00 cph Exp $

Copyright (c) 1997 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Interface to Blowfish and MD5
;;; package: ()

(declare (usual-integrations))

(define-primitives
  (md5 1)
  (blowfish-set-key 1)
  (blowfish-cbc 4)
  (blowfish-cfb64 5))

(define (blowfish-available?)
  (and (implemented-primitive-procedure? md5)
       (implemented-primitive-procedure? blowfish-set-key)))

(define (blowfish-encrypt-string plaintext key-string encrypt?)
  (blowfish-cfb64 plaintext
		  (blowfish-set-key (md5 key-string))
		  (make-string 8 #\NUL)
		  0
		  encrypt?))

(define (blowfish-encrypt-port input output key-string encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key (md5 key-string)))
	(buffer (make-string 512))
	(init-vector (make-string 8 #\NUL)))
    (let loop ((m 0))
      (let ((n (input-port/read-string! input buffer)))
	(if (not (fix:= 0 n))
	    (begin
	      (write-string (blowfish-cfb64 (if (fix:= 512 n)
						buffer
						(string-head buffer n))
					    key
					    init-vector
					    m
					    encrypt?)
			    output)
	      (loop (fix:and #x7 (fix:+ m (fix:and #x7 n))))))))))

(define (write-blowfish-file-header port)
  (write-string blowfish-file-header port)
  (newline port))

(define (read-blowfish-file-header port)
  (if (not (string=? (read-line port) blowfish-file-header))
      (error "Not a Blowfish file:" port)))

(define blowfish-file-header
  "Blowfish, 16 rounds")