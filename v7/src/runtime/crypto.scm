#| -*-Scheme-*-

$Id: crypto.scm,v 14.11 2001/01/29 19:32:57 cph Exp $

Copyright (c) 2000-2001 Massachusetts Institute of Technology

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
;;; package: (runtime crypto)

(declare (usual-integrations))

;;;; The mhash library

(define mhash-algorithm-names)
(define mhash-contexts)
(define mhash-hmac-contexts)

(define (mhash-name->id name procedure)
  (let ((n (vector-length mhash-algorithm-names)))
    (let loop ((i 0))
      (cond ((fix:= i n) (error:bad-range-argument name procedure))
	    ((eq? name (vector-ref mhash-algorithm-names i)) i)
	    (else (loop (fix:+ i 1)))))))

(define-structure mhash-context (index #f read-only #t))
(define-structure mhash-hmac-context (index #f read-only #t))

(define (guarantee-mhash-context object procedure)
  (if (not (mhash-context? object))
      (error:wrong-type-argument object "mhash context" procedure)))

(define (guarantee-mhash-hmac-context object procedure)
  (if (not (mhash-hmac-context? object))
      (error:wrong-type-argument object "mhash HMAC context" procedure)))

(define (mhash-type-names)
  (names-vector->list mhash-algorithm-names))

(define (mhash-get-block-size name)
  ((ucode-primitive mhash_get_block_size 1)
   (mhash-name->id name 'MHASH-GET-BLOCK-SIZE)))

(define (mhash-init name)
  (let ((id (mhash-name->id name 'MHASH-INIT)))
    (without-interrupts
     (lambda ()
       (let ((index ((ucode-primitive mhash_init 1) id)))
	 (if (not index)
	     (error "Unable to allocate mhash context:" name))
	 (let ((context (make-mhash-context index)))
	   (add-to-gc-finalizer! mhash-contexts context index)
	   context))))))

(define (mhash-update context string start end)
  (guarantee-mhash-context context 'MHASH-UPDATE)
  ((ucode-primitive mhash 4) (mhash-context-index context) string start end))

(define (mhash-end context)
  (guarantee-mhash-context context 'MHASH-END)
  (remove-from-gc-finalizer! mhash-contexts context))

(define (mhash-hmac-init name key)
  (let* ((id (mhash-name->id name 'MHASH-INIT))
	 (pblock ((ucode-primitive mhash_get_hash_pblock 1) id)))
    (without-interrupts
     (lambda ()
       (let ((index ((ucode-primitive mhash_hmac_init 3) id key pblock)))
	 (if (not index)
	     (error "Unable to allocate mhash HMAC context:" name))
	 (let ((context (make-mhash-hmac-context index)))
	   (add-to-gc-finalizer! mhash-hmac-contexts context index)
	   context))))))

(define (mhash-hmac-update context string start end)
  (guarantee-mhash-hmac-context context 'MHASH-HMAC-UPDATE)
  ((ucode-primitive mhash 4) (mhash-hmac-context-index context)
			     string start end))

(define (mhash-hmac-end context)
  (guarantee-mhash-hmac-context context 'MHASH-HMAC-END)
  (remove-from-gc-finalizer! mhash-hmac-contexts context))

(define mhash-keygen-names)

(define (keygen-name->id name procedure)
  (let ((n (vector-length mhash-keygen-names)))
    (let loop ((i 0))
      (cond ((fix:= i n) (error:bad-range-argument name procedure))
	    ((eq? name (vector-ref mhash-keygen-names i)) i)
	    (else (loop (fix:+ i 1)))))))

(define (mhash-keygen-type-names)
  (names-vector->list mhash-keygen-names))

(define (mhash-keygen-uses-salt? name)
  ((ucode-primitive mhash_keygen_uses_salt 1)
   (keygen-name->id name 'MHASH-KEYGEN-USES-SALT?)))

(define (mhash-keygen-uses-count? name)
  ((ucode-primitive mhash_keygen_uses_count 1)
   (keygen-name->id name 'MHASH-KEYGEN-USES-COUNT?)))

(define (mhash-keygen-uses-hash-algorithm name)
  ((ucode-primitive mhash_keygen_uses_hash_algorithm 1)
   (keygen-name->id name 'MHASH-KEYGEN-USES-HASH-ALGORITHM)))

(define (mhash-keygen-salt-size name)
  ((ucode-primitive mhash_get_keygen_salt_size 1)
   (keygen-name->id name 'MHASH-KEYGEN-SALT-SIZE)))

(define (mhash-keygen-max-key-size name)
  ((ucode-primitive mhash_get_keygen_max_key_size 1)
   (keygen-name->id name 'MHASH-KEYGEN-MAX-KEY-SIZE)))

(define (mhash-keygen type passphrase #!optional salt)
  (if (not (mhash-keygen-type? type))
      (error:wrong-type-argument type "mhash type" 'MHASH-KEYGEN))
  (let ((id (mhash-keygen-type-id type))
	(keyword (make-string (mhash-keygen-type-key-length type)))
	(v (mhash-keygen-type-parameter-vector type)))
    (if (not ((ucode-primitive mhash_keygen 4)
	      id
	      (if ((ucode-primitive mhash_keygen_uses_salt 1) id)
		  (begin
		    (if (or (default-object? salt) (not salt))
			(error "Salt required:"
			       (vector-ref mhash-keygen-names id)))
		    (let ((n
			   ((ucode-primitive mhash_get_keygen_salt_size 1)
			    id)))
		      (if (not (or (= n 0)
				   (= n (string-length salt))))
			  (error "Salt size incorrect:"
				 (string-length salt)
				 (error-irritant/noise "; should be:")
				 n)))
		    (let ((v (vector-copy v)))
		      (vector-set! v 0 salt)
		      v))
		  v)
	      keyword
	      passphrase))
	(error "Error signalled by mhash_keygen."))
    keyword))

(define-structure (mhash-keygen-type (constructor %make-mhash-keygen-type))
  (id #f read-only #t)
  (key-length #f read-only #t)
  (parameter-vector #f read-only #t))

(define (make-mhash-keygen-type name key-length hash-names #!optional count)
  (if (not (index-fixnum? key-length))
      (error:wrong-type-argument key-length "key length"
				 'MAKE-MHASH-KEYGEN-TYPE))
  (if (not (let ((m (mhash-keygen-max-key-size name)))
	     (or (= m 0)
		 (<= key-length m))))
      (error:bad-range-argument key-length 'MAKE-MHASH-KEYGEN-TYPE))
  (%make-mhash-keygen-type
   (keygen-name->id name 'MAKE-MHASH-KEYGEN-TYPE)
   key-length
   (let ((n-algorithms (mhash-keygen-uses-hash-algorithm name))
	 (hash-names
	  (if (list? hash-names) hash-names (list hash-names))))
     (let ((m (length hash-names)))
       (if (not (= n-algorithms m))
	   (error "Wrong number of hash types supplied:"
		  m
		  (error-irritant/noise "; should be:")
		  n-algorithms)))
     (let ((n (+ 2 n-algorithms)))
       (let ((v (make-vector n)))
	 (vector-set! v 0 #f)
	 (vector-set!
	  v 1
	  (and (mhash-keygen-uses-count? name)
	       (begin
		 (if (or (default-object? count) (not count))
		     (error "Iteration count required:" name))
		 (if (not (and (exact-integer? count)
			       (positive? count)))
		     (error:bad-range-argument count 'MAKE-MHASH-KEYGEN-TYPE))
		 count)))
	 (do ((i 2 (fix:+ i 1))
	      (names hash-names (cdr names)))
	     ((fix:= i n))
	   (vector-set! v i
			(mhash-name->id (car names) 'MAKE-MHASH-KEYGEN-TYPE)))
	 v)))))

(define (mhash-available?)
  (implemented-primitive-procedure? (ucode-primitive mhash 4)))

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

;;;; Package initialization

(define (initialize-package!)
  (if (mhash-available?)
      (begin
	(set! mhash-algorithm-names
	      (make-names-vector (ucode-primitive mhash_count 0)
				 (ucode-primitive mhash_get_hash_name 1)))
	(set! mhash-contexts
	      (make-gc-finalizer (ucode-primitive mhash_end 1)))
	(set! mhash-hmac-contexts
	      (make-gc-finalizer (ucode-primitive mhash_hmac_end 1)))
	(set! mhash-keygen-names
	      (make-names-vector (ucode-primitive mhash_keygen_count 0)
				 (ucode-primitive mhash_get_keygen_name 1)))
	unspecific)))

(define (make-names-vector get-count get-name)
  (let ((n (get-count)))
    (let ((v (make-vector n)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(vector-set! v i
		     (let ((name (get-name i)))
		       (and name
			    (intern name)))))
      v)))

(define (names-vector->list v)
  (let ((end (vector-length v)))
    (let loop ((index 0) (names '()))
      (if (fix:< index end)
	  (loop (fix:+ index 1)
		(let ((name (vector-ref v index)))
		  (if name
		      (cons name names)
		      names)))
	  names))))