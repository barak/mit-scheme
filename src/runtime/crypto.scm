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

(define (guarantee-mhash-context object caller)
  (if (not (mhash-context? object))
      (error:wrong-type-argument object "mhash context" caller))
  (if (not (mhash-context-index object))
      (error:bad-range-argument object caller)))

(define (guarantee-mhash-hmac-context object caller)
  (if (not (mhash-hmac-context? object))
      (error:wrong-type-argument object "mhash HMAC context" caller))
  (if (not (mhash-hmac-context-index object))
      (error:bad-range-argument object caller)))

(define (mhash-type-names)
  (names-vector->list mhash-algorithm-names))

(define (mhash-get-block-size name)
  ((ucode-primitive mhash_get_block_size 1)
   (mhash-name->id name 'mhash-get-block-size)))

(define (mhash-init name)
  (let ((id (mhash-name->id name 'mhash-init)))
    (without-interruption
     (lambda ()
       (let ((index ((ucode-primitive mhash_init 1) id)))
	 (if (not index)
	     (error "Unable to allocate mhash context:" name))
	 (add-to-gc-finalizer! mhash-contexts (make-mhash-context index)))))))

(define (mhash-update context bytes start end)
  (guarantee-mhash-context context 'mhash-update)
  ((ucode-primitive mhash 4) (mhash-context-index context) bytes start end))

(define (mhash-end context)
  (remove-from-gc-finalizer! mhash-contexts context))

(define (mhash-hmac-init name key)
  (let* ((id (mhash-name->id name 'mhash-init))
	 (pblock ((ucode-primitive mhash_get_hash_pblock 1) id)))
    (without-interruption
     (lambda ()
       (let ((index ((ucode-primitive mhash_hmac_init 3) id key pblock)))
	 (if (not index)
	     (error "Unable to allocate mhash HMAC context:" name))
	 (add-to-gc-finalizer! mhash-hmac-contexts
			       (make-mhash-hmac-context index)))))))

(define (mhash-hmac-update context bytes start end)
  (guarantee-mhash-hmac-context context 'mhash-hmac-update)
  ((ucode-primitive mhash 4) (mhash-hmac-context-index context)
			     bytes start end))

(define (mhash-hmac-end context)
  (remove-from-gc-finalizer! mhash-hmac-contexts context))

(define mhash-keygen-names)

(define (keygen-name->id name caller)
  (let ((n (vector-length mhash-keygen-names)))
    (let loop ((i 0))
      (cond ((fix:= i n) (error:bad-range-argument name caller))
	    ((eq? name (vector-ref mhash-keygen-names i)) i)
	    (else (loop (fix:+ i 1)))))))

(define (mhash-keygen-type-names)
  (names-vector->list mhash-keygen-names))

(define (mhash-keygen-uses-salt? name)
  ((ucode-primitive mhash_keygen_uses_salt 1)
   (keygen-name->id name 'mhash-keygen-uses-salt?)))

(define (mhash-keygen-uses-count? name)
  ((ucode-primitive mhash_keygen_uses_count 1)
   (keygen-name->id name 'mhash-keygen-uses-count?)))

(define (mhash-keygen-uses-hash-algorithm name)
  ((ucode-primitive mhash_keygen_uses_hash_algorithm 1)
   (keygen-name->id name 'mhash-keygen-uses-hash-algorithm)))

(define (mhash-keygen-salt-size name)
  ((ucode-primitive mhash_get_keygen_salt_size 1)
   (keygen-name->id name 'mhash-keygen-salt-size)))

(define (mhash-keygen-max-key-size name)
  ((ucode-primitive mhash_get_keygen_max_key_size 1)
   (keygen-name->id name 'mhash-keygen-max-key-size)))

(define (mhash-keygen type passphrase #!optional salt)
  (if (not (mhash-keygen-type? type))
      (error:wrong-type-argument type "mhash type" 'mhash-keygen))
  (let ((id (mhash-keygen-type-id type))
	(keyword (make-bytevector (mhash-keygen-type-key-length type)))
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
				   (= n (bytevector-length salt))))
			  (error "Salt size incorrect:"
				 (bytevector-length salt)
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
  (guarantee index-fixnum? key-length 'make-mhash-keygen-type)
  (if (not (let ((m (mhash-keygen-max-key-size name)))
	     (or (= m 0)
		 (<= key-length m))))
      (error:bad-range-argument key-length 'make-mhash-keygen-type))
  (%make-mhash-keygen-type
   (keygen-name->id name 'make-mhash-keygen-type)
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
		     (error:bad-range-argument count 'make-mhash-keygen-type))
		 count)))
	 (do ((i 2 (fix:+ i 1))
	      (names hash-names (cdr names)))
	     ((fix:= i n))
	   (vector-set! v i
			(mhash-name->id (car names) 'make-mhash-keygen-type)))
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
		     (make-gc-finalizer (ucode-primitive mhash_end 1)
					mhash-context?
					mhash-context-index
					set-mhash-context-index!))
	       (set! mhash-hmac-contexts
		     (make-gc-finalizer (ucode-primitive mhash_hmac_end 1)
					mhash-hmac-context?
					mhash-hmac-context-index
					set-mhash-hmac-context-index!))
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
    (port-consumer (lambda () (mhash-init hash-type))
		   mhash-update
		   mhash-end)))

(define (mhash-string hash-type string #!optional start end)
  (mhash-bytevector hash-type (string->utf8 string start end)))

(define (mhash-bytevector hash-type bytes #!optional start end)
  (let* ((end (fix:end-index end (bytevector-length bytes) 'mhash-bytevector))
	 (start (fix:start-index start end 'mhash-bytevector))
	 (context (mhash-init hash-type)))
    (mhash-update context bytes start end)
    (mhash-end context)))

;;;; MD5

(define (md5-available?)
  (or (mhash-available?)
      (%md5-available?)))

(define (%md5-available?)
  (load-library-object-file "prmd5" #f)
  (implemented-primitive-procedure? (ucode-primitive md5-init 0)))

(define (md5-file filename)
  (cond ((mhash-available?) (mhash-file 'md5 filename))
	((%md5-available?) (%md5-file filename))
	(else (error "This Scheme system was built without MD5 support."))))

(define (%md5-file filename)
  (call-with-binary-input-file filename
    (port-consumer (ucode-primitive md5-init 0)
		   (ucode-primitive md5-update 4)
		   (ucode-primitive md5-final 1))))

(define (md5-string string #!optional start end)
  (md5-bytevector (string->utf8 string start end)))

(define (md5-bytevector bytes #!optional start end)
  (cond ((mhash-available?) (mhash-bytevector 'md5 bytes start end))
	((%md5-available?) (%md5-bytevector bytes start end))
	(else (error "This Scheme system was built without MD5 support."))))

(define (%md5-bytevector bytes #!optional start end)
  (let ((end (fix:end-index end (bytevector-length bytes) 'md5-bytevector))
	(start (fix:start-index start end 'md5-bytevector))
	(context ((ucode-primitive md5-init 0))))
    ((ucode-primitive md5-update 4) context bytes start end)
    ((ucode-primitive md5-final 1) context)))

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
		     (make-gc-finalizer (ucode-primitive mcrypt_generic_end 1)
					mcrypt-context?
					mcrypt-context-index
					set-mcrypt-context-index!))
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
  (without-interruption
   (lambda ()
     (add-to-gc-finalizer! mcrypt-contexts
			   (make-mcrypt-context
			    ((ucode-primitive mcrypt_module_open 2) algorithm
								    mode))))))

(define (mcrypt-init context key init-vector)
  (guarantee-mcrypt-context context 'mcrypt-init)
  (let ((code
	 ((ucode-primitive mcrypt_generic_init 3)
	  (mcrypt-context-index context) key init-vector)))
    (if (not (eqv? code 0))
	(error "Error code signalled by mcrypt_generic_init:" code))))

(define-integrable (make-mcrypt-transform! name primitive)
  (lambda (context bytes start end)
    (guarantee-mcrypt-context context name)
    (let ((code (primitive (mcrypt-context-index context) bytes start end)))
      (if (not (eqv? code 0))
	  (error (string-append "Error code signalled by " primitive ":")
		 code)))))

(define mcrypt-encrypt!
  (make-mcrypt-transform! 'mcrypt-encrypt!
			  (ucode-primitive mcrypt_generic 4)))

(define mcrypt-decrypt!
  (make-mcrypt-transform! 'mcrypt-decrypt!
			  (ucode-primitive mdecrypt_generic 4)))

(define (mcrypt-encrypt context input input-start input-end
			output output-start encrypt?)
  ((if encrypt? mcrypt-encrypt! mcrypt-decrypt!)
   context
   output
   output-start
   (bytevector-copy! output output-start input input-start input-end)))

(define (mcrypt-end context)
  (remove-from-gc-finalizer! mcrypt-contexts context))

(define (mcrypt-generic-unary name context-op module-op)
  (lambda (object)
    (cond ((mcrypt-context? object) (context-op (mcrypt-context-index object)))
	  ((bytevector? object) (module-op object))
	  (else (error:wrong-type-argument object "mcrypt context" name)))))

(define mcrypt-self-test
  (mcrypt-generic-unary
   'mcrypt-self-test
   (ucode-primitive mcrypt_enc_self_test 1)
   (ucode-primitive mcrypt_module_self_test 1)))

(define mcrypt-block-algorithm-mode?
  (mcrypt-generic-unary
   'mcrypt-block-algorithm-mode?
   (ucode-primitive mcrypt_enc_is_block_algorithm_mode 1)
   (ucode-primitive mcrypt_module_is_block_algorithm_mode 1)))

(define mcrypt-block-algorithm?
  (mcrypt-generic-unary
   'mcrypt-block-algorithm?
   (ucode-primitive mcrypt_enc_is_block_algorithm 1)
   (ucode-primitive mcrypt_module_is_block_algorithm 1)))

(define mcrypt-block-mode?
  (mcrypt-generic-unary
   'mcrypt-block-mode?
   (ucode-primitive mcrypt_enc_is_block_mode 1)
   (ucode-primitive mcrypt_module_is_block_mode 1)))

(define mcrypt-key-size
  (mcrypt-generic-unary
   'mcrypt-key-size
   (ucode-primitive mcrypt_enc_get_key_size 1)
   (ucode-primitive mcrypt_module_get_algo_key_size 1)))

(define mcrypt-supported-key-sizes
  (mcrypt-generic-unary
   'mcrypt-supported-key-sizes
   (ucode-primitive mcrypt_enc_get_supported_key_sizes 1)
   (ucode-primitive mcrypt_module_get_algo_supported_key_sizes 1)))

(define (mcrypt-init-vector-size context)
  (guarantee-mcrypt-context context 'mcrypt-init-vector-size)
  ((ucode-primitive mcrypt_enc_get_iv_size 1)
   (mcrypt-context-index context)))

(define (mcrypt-algorithm-name context)
  (guarantee-mcrypt-context context 'mcrypt-algorithm-name)
  ((ucode-primitive mcrypt_enc_get_algorithms_name 1)
   (mcrypt-context-index context)))

(define (mcrypt-mode-name context)
  (guarantee-mcrypt-context context 'mcrypt-mode-name)
  ((ucode-primitive mcrypt_enc_get_modes_name 1)
   (mcrypt-context-index context)))

(define (mcrypt-encrypt-port algorithm mode input output key init-vector
			     encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  ((port-transformer (lambda ()
		       (let ((context (mcrypt-open-module algorithm mode)))
			 (mcrypt-init context key init-vector)
			 context))
		     (if encrypt? mcrypt-encrypt! mcrypt-decrypt!)
		     mcrypt-end)
   input
   output))

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

(define (port-consumer initialize update finalize)
  (lambda (port)
    (call-with-buffer #x1000
      (lambda (buffer)
	(let ((context (initialize)))
	  (let loop ()
	    (let ((n (read-bytevector! buffer port)))
	      (if (and n (not (eof-object? n)))
		  (begin
		    (update context buffer 0 n)
		    (loop)))))
	  (finalize context))))))

(define (port-transformer initialize update finalize)
  (lambda (input-port output-port)
    (call-with-buffer #x1000
      (lambda (buffer)
	(let ((context (initialize)))
	  (let loop ()
	    (let ((n (read-bytevector! buffer input-port)))
	      (if (and n (fix:> n 0))
		  (begin
		    (update context buffer 0 n)
		    (let ((n* (write-bytevector buffer output-port 0 n)))
		      (if (not (eqv? n n*))
			  (error "Short write (requested, actual):" n n*)))
		    (loop)))))
	  (finalize context))))))

(define (call-with-buffer n procedure)
  (let ((buffer (make-bytevector n)))
    (dynamic-wind
	(lambda ()
	  unspecific)
	(lambda ()
	  (procedure buffer))
	(lambda ()
	  (bytevector-fill! buffer 0)))))