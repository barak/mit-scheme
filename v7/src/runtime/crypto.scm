#| -*-Scheme-*-

$Id: crypto.scm,v 14.1 2000/04/10 03:32:32 cph Exp $

Copyright (c) 2000 Massachusetts Institute of Technology

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

;;;; Interface to cryptography libraries
;;; package: ()

(declare (usual-integrations))

;;;; The mhash library

(define mhash-types)
(define mhash-get-block-size)
(define mhash-init)
(define mhash-hmac-init)
(let ((%mhash-count (ucode-primitive mhash_count 0))
      (%mhash-get-block-size (ucode-primitive mhash_get_block_size 1))
      (%mhash-get-hash-name (ucode-primitive mhash_get_hash_name 1))
      (%mhash-get-hash-pblock (ucode-primitive mhash_get_hash_pblock 1))
      (%mhash-init (ucode-primitive mhash_init 1))
      (%mhash-hmac-init (ucode-primitive mhash_hmac_init 3)))
  (let* ((names #f)
	 (guarantee-names
	  (lambda ()
	    (if (not names)
		(let ((n (%mhash-count)))
		  (let ((v (make-vector n)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i n))
		      (vector-set!
		       v i (intern (%mhash-get-hash-name i))))
		    (set! names v))))))
	 (hash-name->id
	  (lambda (name procedure)
	    (guarantee-names)
	    (let ((n (vector-length names)))
	      (let loop ((i 0))
		(if (fix:< i n)
		    (if (eq? name (vector-ref names i))
			i
			(loop (fix:+ i 1)))
		    (error:bad-range-argument name procedure)))))))
    (set! mhash-types
	  (lambda ()
	    (guarantee-names)
	    (vector->list names)))
    (set! mhash-get-block-size
	  (lambda (name)
	    (%mhash-get-block-size
	     (hash-name->id name 'MHASH-GET-BLOCK-SIZE))))
    (set! mhash-init
	  (lambda (name)
	    (%mhash-init (hash-name->id name 'MHASH-INIT))))
    (set! mhash-hmac-init
	  (lambda (name key)
	    (let ((id (hash-name->id name 'MHASH-INIT)))
	      (%mhash-hmac-init id key (%mhash-get-hash-pblock id)))))))

(define mhash-update (ucode-primitive mhash 4))
(define mhash-end (ucode-primitive mhash_end 1))
(define mhash-hmac-end (ucode-primitive mhash_hmac_end 1))

(define mhash-keygen-types)
(define mhash-keygen-uses-salt?)
(define mhash-keygen-uses-count?)
(define mhash-keygen-uses-hash-algorithm)
(define mhash-keygen-salt-size)
(define mhash-keygen-max-key-size)
(define mhash-keygen)
(let ((%mhash-keygen-count (ucode-primitive mhash_keygen_count 0))
      (%mhash-get-keygen-name (ucode-primitive mhash_get_keygen_name 1))
      (%mhash-keygen-uses-salt (ucode-primitive mhash_keygen_uses_salt 1))
      (%mhash-keygen-uses-count (ucode-primitive mhash_keygen_uses_count 1))
      (%mhash-keygen-uses-hash-algorithm
       (ucode-primitive mhash_keygen_uses_hash_algorithm 1))
      (%mhash-get-keygen-salt-size
       (ucode-primitive mhash_get_keygen_salt_size 1))
      (%mhash-get-keygen-max-key-size
       (ucode-primitive mhash_get_keygen_max_key_size 1))
      (%mhash-keygen (ucode-primitive mhash_keygen 4)))
  (let* ((names #f)
	 (guarantee-names
	  (lambda ()
	    (if (not names)
		(let ((n (%mhash-keygen-count)))
		  (let ((v (make-vector n)))
		    (do ((i 0 (fix:+ i 1)))
			((fix:= i n))
		      (vector-set!
		       v i (intern (%mhash-get-keygen-name i))))
		    (set! names v))))))
	 (keygen-name->id
	  (lambda (name procedure)
	    (guarantee-names)
	    (let ((n (vector-length names)))
	      (let loop ((i 0))
		(if (fix:< i n)
		    (if (eq? name (vector-ref names i))
			i
			(loop (fix:+ i 1)))
		    (error:bad-range-argument name procedure)))))))
    (set! mhash-keygen-types
	  (lambda ()
	    (guarantee-names)
	    (vector->list names)))
    (set! mhash-keygen-uses-salt?
	  (lambda (name)
	    (%mhash-keygen-uses-salt
	     (keygen-name->id name 'MHASH-KEYGEN-USES-SALT?))))
    (set! mhash-keygen-uses-count?
	  (lambda (name)
	    (%mhash-keygen-uses-count
	     (keygen-name->id name 'MHASH-KEYGEN-USES-COUNT?))))
    (set! mhash-keygen-uses-hash-algorithm
	  (lambda (name)
	    (%mhash-keygen-uses-hash-algorithm
	     (keygen-name->id name 'MHASH-KEYGEN-USES-HASH-ALGORITHM))))
    (set! mhash-keygen-salt-size
	  (lambda (name)
	    (%mhash-get-keygen-salt-size
	     (keygen-name->id name 'MHASH-KEYGEN-SALT-SIZE))))
    (set! mhash-keygen-max-key-size
	  (lambda (name)
	    (%mhash-get-keygen-max-key-size
	     (keygen-name->id name 'MHASH-KEYGEN-MAX-KEY-SIZE))))
    (set! mhash-keygen
	  (lambda (name parameters keyword passphrase)
	    (%mhash-keygen (keygen-name->id name 'MHASH-KEYGEN)
			   parameters
			   keyword
			   passphrase)))))

(define (mhash-available?)
  (implemented-primitive-procedure? mhash-update))

(define (mhash-file hash-type filename)
  (call-with-binary-input-file filename
    (lambda (port)
      (let ((buffer (make-string 4096))
	    (context (mhash-init hash-type)))
	(dynamic-wind (lambda ()
			unspecific)
		      (lambda ()
			(let loop ()
			  (let ((n (read-substring! buffer 0 4096 port)))
			    (if (fix:= 0 n)
				(mhash-end context)
				(begin
				  (mhash-update context buffer 0 n)
				  (loop))))))
		      (lambda ()
			(string-fill! buffer #\NUL)))))))

(define (mhash-string hash-type string)
  (mhash-substring hash-type string 0 (string-length string)))

(define (mhash-substring hash-type string start end)
  (let ((context (mhash-init hash-type)))
    (mhash-update context string start end)
    (mhash-end context)))

(define (mhash-sum->number sum)
  (let ((l (string-length sum)))
    (do ((i 0 (fix:+ i 1))
	 (n 0 (+ (* n #x100) (vector-8b-ref sum i))))
	((fix:= i l) n))))

(define (mhash-sum->hexadecimal sum)
  (let ((n (string-length sum))
	(digits "0123456789abcdef"))
    (let ((s (make-string (fix:* 2 n))))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(string-set! s (fix:* 2 i)
		     (string-ref digits
				 (fix:lsh (vector-8b-ref sum i) -4)))
	(string-set! s (fix:+ (fix:* 2 i) 1)
		     (string-ref digits
				 (fix:and (vector-8b-ref sum i) #x0F))))
      s)))

;;;; MD5

(define (md5-available?)
  (or (mhash-available?)
      (implemented-primitive-procedure? (ucode-primitive md5-init 0))))

(define (md5-file filename)
  (if (mhash-available?)
      (mhash-file 'MD5 filename)
      (call-with-binary-input-file filename
	(lambda (port)
	  (let ((buffer (make-string 4096))
		(context ((ucode-primitive md5-init 0))))
	    (dynamic-wind (lambda ()
			    unspecific)
			  (lambda ()
			    (let loop ()
			      (let ((n (read-substring! buffer 0 4096 port)))
				(if (fix:= 0 n)
				    ((ucode-primitive md5-final 1) context)
				    (begin
				      ((ucode-primitive md5-update 4)
				       context buffer 0 n)
				      (loop))))))
			  (lambda ()
			    (string-fill! buffer #\NUL))))))))

(define (md5-string string)
  (md5-substring string 0 (string-length string)))

(define (md5-substring string start end)
  (if (mhash-available?)
      (mhash-substring 'MD5 string start end)
      (let ((context ((ucode-primitive md5-init 0))))
	((ucode-primitive md5-update 4) context string start end)
	((ucode-primitive md5-final 1) context))))

(define md5-sum->number mhash-sum->number)
(define md5-sum->hexadecimal mhash-sum->hexadecimal)