#| -*-Scheme-*-

$Id: blowfish.scm,v 1.11 1999/08/09 18:25:45 cph Exp $

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
  (md5-final 1))

(define blowfish-available?)
(define blowfish-set-key)
(define blowfish-cbc)
(define blowfish-cfb64)

(let ((unlocked? 'UNKNOWN)
      (key-sum "8074396df211ba2da12a872b6e84d7ce"))

  (define (check-key)
    (initialize-key)
    (if (not unlocked?)
	(error "Blowfish support disabled in this implementation.")))

  (define (initialize-key)
    (if (eq? 'UNKNOWN unlocked?)
	(set! unlocked?
	      (and (implemented-primitive-procedure? md5-init)
		   (implemented-primitive-procedure?
		    (ucode-primitive blowfish-cfb64-substring-v2 9))
		   (let ((pathname
			  (call-with-current-continuation
			   (lambda (k)
			     (bind-condition-handler
				 (list condition-type:file-error)
				 (lambda (condition)
				   condition
				   (k #f))
			       (lambda ()
				 (system-library-pathname "blowfish.key")))))))
		     (and pathname
			  (string=? key-sum
				    (md5-sum->hexadecimal
				     (md5-file pathname)))))))))

  (set! blowfish-available?
	(lambda ()
	  (initialize-key)
	  unlocked?))

  (set! blowfish-set-key
	(lambda (string)
	  (check-key)
	  ((ucode-primitive blowfish-set-key 1) string)))

  (set! blowfish-cbc
	(lambda (input key init-vector encrypt?)
	  (check-key)
	  ((ucode-primitive blowfish-cbc-v2 5) input key init-vector
					       encrypt?)))

  (set! blowfish-cfb64
	(lambda (input input-start input-end output output-start
		       key init-vector num encrypt?)
	  (check-key)
	  ((ucode-primitive blowfish-cfb64-substring-v2 9)
	   input input-start input-end output output-start
	   key init-vector num encrypt?))))

(define (blowfish-encrypt-port input output key init-vector encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key key))
	(input-buffer (make-string 4096))
	(output-buffer (make-string 4096)))
    (let loop ((m 0))
      (let ((n (input-port/read-string! input input-buffer)))
	(if (not (fix:= 0 n))
	    (let ((m
		   (blowfish-cfb64 input-buffer 0 n output-buffer 0
				   key init-vector m encrypt?)))
	      (write-substring output-buffer 0 n output)
	      (loop m)))))))

(define (write-blowfish-file-header port)
  (write-string blowfish-file-header-v2 port)
  (newline port)
  (let ((init-vector (compute-blowfish-cfb-init-vector)))
    (write-string init-vector port)
    init-vector))

(define (compute-blowfish-cfb-init-vector)
  (let ((iv (make-string 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (get-universal-time) (quotient t #x100)))
	((fix:= 8 i))
      (vector-8b-set! iv i (remainder t #x100)))
    iv))

(define (read-blowfish-file-header port)
  (let ((line (read-line port)))
    (cond ((string=? blowfish-file-header-v1 line)
	   (make-string 8 #\NUL))
	  ((string=? blowfish-file-header-v2 line)
	   (let ((init-vector (make-string 8)))
	     (if (not (= 8 (read-string! init-vector 0 8 port)))
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