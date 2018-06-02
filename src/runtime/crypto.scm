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

;;;; Interface to cryptography libraries
;;; package: (runtime crypto)

(declare (usual-integrations))

;;;; MD5

(define (md5-available?)
  #t)

(define (md5-file filename)
  (call-with-binary-input-file filename
    (port-consumer (ucode-primitive md5-init 0)
		   (ucode-primitive md5-update 4)
		   (ucode-primitive md5-final 1))))

(define (md5-string string #!optional start end)
  (md5-bytevector (string->utf8 string start end)))

(define (md5-bytevector bytes #!optional start end)
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
	  (error (string "Error code signalled by "name":") code)))))

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
	  ((string? object) (module-op (string->utf8 object)))
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