#| -*-Scheme-*-

$Id: crypto.scm,v 14.17 2003/11/09 04:40:40 cph Exp $

Copyright 2000,2001,2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Interface to cryptography libraries
;;; package: (runtime crypto)

(declare (usual-integrations))

;;;; The mhash library

(define mhash-initialized?)
(define mhash-algorithm-names)
(define mhash-contexts)
(define mhash-hmac-contexts)

(define (mhash-name->id name procedure)
  (let ((n (vector-length mhash-algorithm-names)))
    (let loop ((i 0))
      (cond ((fix:= i n) (error:bad-range-argument name procedure))
	    ((eq? name (vector-ref mhash-algorithm-names i)) i)
	    (else (loop (fix:+ i 1)))))))

(define-structure mhash-context index)
(define-structure mhash-hmac-context index)

(define (guarantee-mhash-context object procedure)
  (if (not (mhash-context? object))
      (error:wrong-type-argument object "mhash context" procedure))
  (if (not (mhash-context-index object))
      (error:bad-range-argument object procedure)))

(define (guarantee-mhash-hmac-context object procedure)
  (if (not (mhash-hmac-context? object))
      (error:wrong-type-argument object "mhash HMAC context" procedure))
  (if (not (mhash-hmac-context-index object))
      (error:bad-range-argument object procedure)))

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
  (set-mhash-context-index! context #f)
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
  (set-mhash-hmac-context-index! context #f)
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
  (load-library-object-file "prmhash" #f)
  (and (implemented-primitive-procedure? (ucode-primitive mhash 4))
       (begin
	 (if (not mhash-initialized?)
	     (begin
	       (set! mhash-algorithm-names
		     (make-names-vector
		      (ucode-primitive mhash_count 0)
		      (ucode-primitive mhash_get_hash_name 1)))
	       (set! mhash-contexts
		     (make-gc-finalizer (ucode-primitive mhash_end 1)))
	       (set! mhash-hmac-contexts
		     (make-gc-finalizer (ucode-primitive mhash_hmac_end 1)))
	       (set! mhash-keygen-names
		     (make-names-vector
		      (ucode-primitive mhash_keygen_count 0)
		      (ucode-primitive mhash_get_keygen_name 1)))
	       (set! mhash-initialized? #t)))
	 #t)))

(define (reset-mhash-variables!)
  (set! mhash-initialized? #f)
  unspecific)

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
      (%md5-available?)))

(define (%md5-available?)
  (load-library-object-file "prmd5" #f)
  (implemented-primitive-procedure? (ucode-primitive md5-init 0)))

(define (md5-file filename)
  (cond ((mhash-available?)
	 (mhash-file 'MD5 filename))
	((%md5-available?)
	 (%md5-file filename))
	(else
	 (error "No MD5 support available."))))

(define (%md5-file filename)
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
			(string-fill! buffer #\NUL)))))))

(define (md5-string string)
  (md5-substring string 0 (string-length string)))

(define (md5-substring string start end)
  (cond ((mhash-available?)
	 (mhash-substring 'MD5 string start end))
	((%md5-available?)
	 (%md5-substring string start end))
	(else
	 (error "No MD5 support available."))))

(define (%md5-substring string start end)
  (let ((context ((ucode-primitive md5-init 0))))
    ((ucode-primitive md5-update 4) context string start end)
    ((ucode-primitive md5-final 1) context)))

(define md5-sum->number mhash-sum->number)
(define md5-sum->hexadecimal mhash-sum->hexadecimal)

;;;; The mcrypt library

(define mcrypt-initialized?)
(define mcrypt-algorithm-names-vector)
(define mcrypt-mode-names-vector)
(define mcrypt-contexts)
(define-structure mcrypt-context index)

(define (guarantee-mcrypt-context object procedure)
  (if (not (mcrypt-context? object))
      (error:wrong-type-argument object "mcrypt context" procedure))
  (if (not (mcrypt-context-index object))
      (error:bad-range-argument object procedure)))

(define (mcrypt-available?)
  (load-library-object-file "prmcrypt" #f)
  (and (implemented-primitive-procedure?
	(ucode-primitive mcrypt_module_open 2))
       (begin
	 (if (not mcrypt-initialized?)
	     (begin
	       (set! mcrypt-contexts
		     (make-gc-finalizer
		      (ucode-primitive mcrypt_generic_end 1)))
	       (set! mcrypt-algorithm-names-vector
		     ((ucode-primitive mcrypt_list_algorithms 0)))
	       (set! mcrypt-mode-names-vector
		     ((ucode-primitive mcrypt_list_modes 0)))
	       (set! mcrypt-initialized? #t)))
	 #t)))

(define (reset-mcrypt-variables!)
  (set! mcrypt-initialized? #f)
  unspecific)

(define (mcrypt-algorithm-names)
  (names-vector->list mcrypt-algorithm-names-vector))

(define (mcrypt-mode-names)
  (names-vector->list mcrypt-mode-names-vector))

(define (mcrypt-open-module algorithm mode)
  (without-interrupts
   (lambda ()
     (let ((index ((ucode-primitive mcrypt_module_open 2) algorithm mode)))
       (let ((context (make-mcrypt-context index)))
	 (add-to-gc-finalizer! mcrypt-contexts context index)
	 context)))))

(define (mcrypt-init context key init-vector)
  (guarantee-mcrypt-context context 'MCRYPT-INIT)
  (let ((code
	 ((ucode-primitive mcrypt_generic_init 3)
	  (mcrypt-context-index context) key init-vector)))
    (if (not (= code 0))
	(error "Error code signalled by mcrypt_generic_init:" code))))

(define (mcrypt-encrypt context input input-start input-end
			output output-start encrypt?)
  (guarantee-mcrypt-context context 'MCRYPT-ENCRYPT)
  (substring-move! input input-start input-end output output-start)
  (let ((code
	 ((if encrypt?
	      (ucode-primitive mcrypt_generic 4)
	      (ucode-primitive mdecrypt_generic 4))
	  (mcrypt-context-index context)
	  output
	  output-start
	  (fix:+ output-start (fix:- input-end input-start)))))
    (if (not (= code 0))
	(error (string-append "Error code signalled by "
			      (if encrypt?
				  "mcrypt_generic"
				  "mdecrypt_generic")
			      ":")
	       code))))

(define (mcrypt-end context)
  (guarantee-mcrypt-context context 'MCRYPT-END)
  (set-mcrypt-context-index! context #f)
  (remove-from-gc-finalizer! mcrypt-contexts context))

(define (mcrypt-generic-unary name context-op module-op)
  (lambda (object)
    (cond ((mcrypt-context? object) (context-op (mcrypt-context-index object)))
	  ((string? object) (module-op object))
	  (else (error:wrong-type-argument object "mcrypt context" name)))))

(define mcrypt-self-test
  (mcrypt-generic-unary
   'MCRYPT-SELF-TEST
   (ucode-primitive mcrypt_enc_self_test 1)
   (ucode-primitive mcrypt_module_self_test 1)))

(define mcrypt-block-algorithm-mode?
  (mcrypt-generic-unary
   'MCRYPT-BLOCK-ALGORITHM-MODE?
   (ucode-primitive mcrypt_enc_is_block_algorithm_mode 1)
   (ucode-primitive mcrypt_module_is_block_algorithm_mode 1)))

(define mcrypt-block-algorithm?
  (mcrypt-generic-unary
   'MCRYPT-BLOCK-ALGORITHM?
   (ucode-primitive mcrypt_enc_is_block_algorithm 1)
   (ucode-primitive mcrypt_module_is_block_algorithm 1)))

(define mcrypt-block-mode?
  (mcrypt-generic-unary
   'MCRYPT-BLOCK-MODE?
   (ucode-primitive mcrypt_enc_is_block_mode 1)
   (ucode-primitive mcrypt_module_is_block_mode 1)))

(define mcrypt-key-size
  (mcrypt-generic-unary
   'MCRYPT-KEY-SIZE
   (ucode-primitive mcrypt_enc_get_key_size 1)
   (ucode-primitive mcrypt_module_get_algo_key_size 1)))

(define mcrypt-supported-key-sizes
  (mcrypt-generic-unary
   'MCRYPT-SUPPORTED-KEY-SIZES
   (ucode-primitive mcrypt_enc_get_supported_key_sizes 1)
   (ucode-primitive mcrypt_module_get_algo_supported_key_sizes 1)))

(define (mcrypt-init-vector-size context)
  (guarantee-mcrypt-context context 'MCRYPT-INIT-VECTOR-SIZE)
  ((ucode-primitive mcrypt_enc_get_iv_size 1)
   (mcrypt-context-index context)))

(define (mcrypt-algorithm-name context)
  (guarantee-mcrypt-context context 'MCRYPT-ALGORITHM-NAME)
  ((ucode-primitive mcrypt_enc_get_algorithms_name 1)
   (mcrypt-context-index context)))

(define (mcrypt-mode-name context)
  (guarantee-mcrypt-context context 'MCRYPT-MODE-NAME)
  ((ucode-primitive mcrypt_enc_get_modes_name 1)
   (mcrypt-context-index context)))

(define (mcrypt-encrypt-port algorithm mode input output key init-vector
			     encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((context (mcrypt-open-module algorithm mode))
	(input-buffer (make-string 4096))
	(output-buffer (make-string 4096)))
    (mcrypt-init context key init-vector)
    (dynamic-wind
     (lambda ()
       unspecific)
     (lambda ()
       (let loop ()
	 (let ((n (input-port/read-string! input input-buffer)))
	   (if (not (fix:= 0 n))
	       (begin
		 (mcrypt-encrypt context input-buffer 0 n output-buffer 0
				 encrypt?)
		 (write-substring output-buffer 0 n output)
		 (loop)))))
       (mcrypt-end context))
     (lambda ()
       (string-fill! input-buffer #\NUL)
       (string-fill! output-buffer #\NUL)))))

;;;; Package initialization

(define (initialize-package!)
  (reset-mhash-variables!)
  (add-event-receiver! event:after-restart reset-mhash-variables!)
  (reset-mcrypt-variables!)
  (add-event-receiver! event:after-restart reset-mcrypt-variables!))

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