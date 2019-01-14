#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; The mcrypt option.
;;; package: (mcrypt)

(declare (usual-integrations))

;;;; The mcrypt library
;;;
;;; Multithreading
;;; 
;;; The manual page says the library is thread safe except for module
;;; loading, part of mcrypt_module_open.  Presumably multiple threads
;;; should NOT use the same "thread descriptor" (ctype MCRYPT,
;;; parameter name often "td") returned by mcrypt_module_open.  Also
;;; presumably the mcrypt_mutex_register function should be called "in
;;; multithreaded application... with dynamic module loading support".
;;; It is assumed this is the case for MIT Scheme.
;;; 
;;; This plugin uses an OS mutex to implement lock and unlock
;;; functions passed to mcrypt_mutex_register, and locks and unlocks
;;; it during mcrypt_module_open.  The Scheme mcrypt-context object,
;;; representing an MCRYPT "thread", should be used by one Scheme
;;; thread only.  This restriction is not currently enforced.
;;;
;;; Memory Management
;;; 
;;; Searching the manpage for "free" finds that certain functions
;;; return values allocated by malloc:
;;;     mcrypt_enc_get_supported_key_sizes
;;;     mcrypt_enc_get_algorithms_name
;;;     mcrypt_enc_get_modes_name
;;;     mcrypt_module_get_algo_supported_key_sizes "differs [not!]...
;;;         because the return value here is allocated".  Perhaps it
;;;         was not allocated with malloc?  Perhaps mcrypt_free should
;;;         be used instead of free?
;;; 
;;; The arrays returned by two functions should be freed using
;;; mcrypt_free_p:
;;;     mcrypt_list_algorithms
;;;     mcrypt_list_modes
;;; 
;;; Using microcode/prmcrypt.c as a guide:
;;; 
;;; mcrypt_free is called by
;;;   cp2s, which is called by
;;;     mcrypt_enc_get_algorithms_name and
;;;     mcrypt_enc_get_modes_name, and
;;;   deallocate_key_sizes, which is the abort action for
;;;     convert_key_sizes, which is called by
;;;       mcrypt_enc_get_supported_key_sizes and
;;;       mcyrpt_module_get_algo_supported_key_sizes.
;;; mcrypt_free_p is called by
;;;   deallocate_list, which is the abort action for
;;;     LIST_ITEMS, which is "called" by
;;;       mcrypt_list_algorithms and
;;;       mcrypt_list_modes.
;;; 
;;; This plugin ensures that MCRYPTs and size/name lists returned by
;;; the library do not "leak" by putting cleanup thunks on a weak
;;; alist that is periodically scanned for objects that were GCed and
;;; not freed.

(define (import-mcrypt)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(mcrypt))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
	      '(mcrypt-algorithm-name
		mcrypt-algorithm-names
		mcrypt-block-algorithm-mode?
		mcrypt-block-algorithm?
		mcrypt-block-mode?
		mcrypt-context?
		mcrypt-decrypt!
		mcrypt-encrypt
		mcrypt-encrypt!
		mcrypt-encrypt-port
		mcrypt-end
		mcrypt-init
		mcrypt-init-vector-size
		mcrypt-key-size
		mcrypt-mode-name
		mcrypt-mode-names
		mcrypt-open-module
		mcrypt-self-test
		mcrypt-supported-key-sizes))))

(C-include "mcrypt")

(define mcrypt-initialized? #f)
(define mcrypt-algorithm-names-vector)
(define mcrypt-mode-names-vector)

(define (init!)
  (if (not mcrypt-initialized?)
      (begin
	(C-call "scmcrypt_mutex_register")
	(set! mcrypt-algorithm-names-vector (mcrypt-list-algorithms))
	(set! mcrypt-mode-names-vector (mcrypt-list-modes))
	(set! mcrypt-initialized? #t))))

(define (mcrypt-list-algorithms)
  (let ((mlist (make-mcrypt-name-list)))
    (C-call "scmcrypt_list_algorithms" mlist)
    (let ((vector (mcrypt-name-list-elements mlist)))
      (free-mcrypt-name-list mlist)
      vector)))

(define (mcrypt-list-modes)
  (let ((mlist (make-mcrypt-name-list)))
    (C-call "scmcrypt_list_modes" mlist)
    (let ((vector (mcrypt-name-list-elements mlist)))
      (free-mcrypt-name-list mlist)
      vector)))

(define (reset-mcrypt-variables!)
  (set! mcrypt-initialized? #f)
  (set! mcrypt-algorithm-names-vector)
  (set! mcrypt-mode-names-vector)
  (reset-cleanups!)
  unspecific)

(define (mcrypt-algorithm-names)
  (init!)
  (vector->list mcrypt-algorithm-names-vector))

(define (mcrypt-mode-names)
  (init!)
  (vector->list mcrypt-mode-names-vector))

(define-structure mcrypt-context algorithm mode alien)

(define (guarantee-mcrypt-context object caller)
  (if (not (mcrypt-context? object))
      (error:wrong-type-argument object "mcrypt context" caller))
  (if (alien-null? (mcrypt-context-alien object))
      (error:bad-range-argument object caller)))

(define (guarantee-subbytevector object start end operator)
  (guarantee bytevector? object operator)
  (guarantee index-fixnum? start operator)
  (guarantee index-fixnum? end operator)
  (if (not (fix:<= start end))
      (error:bad-range-argument start operator))
  (if (not (fix:<= end (bytevector-length object)))
      (error:bad-range-argument end operator)))

(define (mcrypt-open-module algorithm mode)
  (init!)
  (let* ((alien (make-alien '(struct |CRYPT_STREAM|)))
	 (context (make-mcrypt-context algorithm mode alien)))
    (add-cleanup context (make-mcrypt-context-cleanup alien))
    (C-call "mcrypt_module_open" alien
	    (string->utf8 algorithm) 0 (string->utf8 mode) 0)
    (if (alien-null? alien)
	(error "Failed to open mcrypt module:"
	       (C-peek-cstring (C-call "scmcrypt_get_ltdlerror"))))
    context))

(define (make-mcrypt-context-cleanup alien)
  (named-lambda (mcrypt-context-cleanup)
    (C-call "mcrypt_generic_end" alien)))

(define (mcrypt-init context key init-vector)
  (guarantee-mcrypt-context context 'mcrypt-init)
  (guarantee bytevector? key 'mcrypt-init)
  (let* ((code
	  (C-call "mcrypt_generic_init"
		  (mcrypt-context-alien context)
		  key (bytevector-length key) init-vector)))
    (if (< code 0)
	(error "Error code signalled by mcrypt_generic_init:"
	       (C-peek-cstring (C-call "mcrypt_strerror"
				       (make-alien '(const (* char)))
				       code))))))

(define-integrable (make-mcrypt-transform! name procedure)
  (lambda (context bytes start end)
    (guarantee-mcrypt-context context name)
    (guarantee-subbytevector bytes start end name)
    (let ((code (procedure context bytes start end)))
      (if (< code 0)
	  (error (string "Error code signalled by "name":") code)))))

(define mcrypt-encrypt!
  (make-mcrypt-transform!
   'mcrypt-encrypt!
   (named-lambda (mcrypt_generic context bytes start end)
     (C-call "scmcrypt_generic" (mcrypt-context-alien context)
	     bytes start end))))

(define mcrypt-decrypt!
  (make-mcrypt-transform!
   'mcrypt-decrypt!
   (named-lambda (mdecrypt_generic context bytes start end)
     (C-call "scmdecrypt_generic" (mcrypt-context-alien context)
	     bytes start end))))

(define (mcrypt-encrypt context input input-start input-end
			output output-start encrypt?)
  (guarantee-mcrypt-context context 'mcrypt-encrypt)
  (guarantee bytevector? input 'mcrypt-encrypt)
  (guarantee bytevector? output 'mcrypt-encrypt)
  ((if encrypt? mcrypt-encrypt! mcrypt-decrypt!)
   context
   output
   output-start
   (bytevector-copy! output output-start input input-start input-end)))

(define (mcrypt-end context)
  (let ((alien (mcrypt-context-alien context)))
    (if (not (alien-null? alien))
	(let ((code (C-call "mcrypt_generic_end" alien)))
	  (if (< code 0)
	      (error "Error code returned by mcrypt_generic_end:" code))
	  (alien-null! alien)
	  (remove-cleanup context)))))

(define (mcrypt-generic-unary name context-op module-op)
  (lambda (object)
    (cond ((mcrypt-context? object)
	   (context-op object))
	  ((bytevector? object)
	   (init!)
	   (module-op object))
	  ((string? object)
	   (init!)
	   (module-op (string->utf8 object)))
	  (else
	   (error:wrong-type-argument object "mcrypt context" name)))))

(define mcrypt-self-test
  (mcrypt-generic-unary
   'mcrypt-self-test
   (named-lambda (mcrypt-enc-self-test context)
     (C-call "mcrypt_enc_self_test" (mcrypt-context-alien context)))
   (named-lambda (mcrypt-module-self-test module-name)
     (C-call "mcrypt_module_self_test" module-name 0))))

(define mcrypt-block-algorithm-mode?
  (mcrypt-generic-unary
   'mcrypt-block-algorithm-mode?
   (named-lambda (mcrypt-enc-is-block-algorithm-mode? context)
     (C-call "mcrypt_enc_is_block_algorithm_mode"
	     (mcrypt-context-alien context)))
   (named-lambda (mcrypt-module-is-block-algorithm-mode? name)
     (C-call "mcrypt_module_is_block_algorithm_mode" name 0))))

(define mcrypt-block-algorithm?
  (mcrypt-generic-unary
   'mcrypt-block-algorithm?
   (named-lambda (mcrypt-enc-is-block-algorithm context)
     (C-call "mcrypt_enc_is_block_algorithm"
	     (mcrypt-context-alien context)))
   (named-lambda (mcrypt-module-is-block-algorithm name)
     (C-call "mcrypt_module_is_block_algorithm" name 0))))

(define mcrypt-block-mode?
  (mcrypt-generic-unary
   'mcrypt-block-mode?
   (named-lambda (mcrypt-enc-is-block-mode context)
     (C-call "mcrypt_enc_is_block_mode"
	     (mcrypt-context-alien context)))
   (named-lambda (mcrypt-module-is-block-mode name)
     (C-call "mcrypt_module_is_block_mode" name 0))))

(define mcrypt-key-size
  (mcrypt-generic-unary
   'mcrypt-key-size
   (named-lambda (mcrypt-enc-get-key-size context)
     (C-call "mcrypt_enc_get_key_size"
	     (mcrypt-context-alien context)))
   (named-lambda (mcrypt-module-get-algo-key-size name)
     (C-call "mcrypt_module_get_algo_key_size" name 0))))

(define mcrypt-supported-key-sizes
  (mcrypt-generic-unary
   'mcrypt-supported-key-sizes
   (named-lambda (mcrypt-enc-get-supported-key-sizes context)
     (let ((mlist (malloc (C-sizeof "struct mcrypt_list")
			  '(struct |mcrypt_list|))))
       (C-call "scmcrypt_enc_get_supported_key_sizes"
	       (mcrypt-context-alien context) mlist)
       (let ((sizes (mcrypt-size-list-elements mlist)))
	 (free mlist)
	 sizes)))
   (named-lambda (mcrypt-module-get-algo-supported-key-sizes name)
     (let ((mlist (make-mcrypt-size-list)))
       (C-call "scmcrypt_module_get_algo_supported_key_sizes" name 0 mlist)
       (let ((sizes (mcrypt-size-list-elements mlist)))
	 (free-mcrypt-size-list mlist)
	 sizes)))))

(define (mcrypt-init-vector-size context)
  (guarantee-mcrypt-context context 'mcrypt-init-vector-size)
  (C-call "mcrypt_enc_get_iv_size" (mcrypt-context-alien context)))

(define (mcrypt-algorithm-name context)
  (guarantee-mcrypt-context context 'mcrypt-algorithm-name)
  (mcrypt-context-algorithm context))

(define (mcrypt-mode-name context)
  (guarantee-mcrypt-context context 'mcrypt-mode-name)
  (mcrypt-context-mode context))

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

(define (port-transformer initialize update finalize)
  (lambda (input-port output-port)
    (call-with-buffer #x1000
      (lambda (buffer)
	(let ((context (initialize)))
	  (let loop ()
	    (let ((n (read-bytevector! buffer input-port)))
	      (if (and n (not (eof-object? n)) (fix:> n 0))
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

;;;; Mcrypt size lists.

(define (mcrypt-size-list-elements mlist)
  (let ((elements (C-> mlist "struct mcrypt_list elements"))
	(size (C-> mlist "struct mcrypt_list size")))
    (if (= size 0)
	#f
	(let ((vector (make-vector size)))
	  (let loop ((i 0))
	    (if (< i size)
		(begin
		  (vector-set! vector i (C-> elements "int"))
		  (alien-byte-increment! elements (C-sizeof "int"))
		  (loop (1+ i)))))
	  vector))))

(define (make-mcrypt-size-list)
  (let ((mlist (make-alien '(struct |mcrypt_list|)))
	(copy (make-alien '(struct |mcrypt_list|))))
    (add-cleanup mlist (make-mcrypt-size-list-cleanup copy))
    (C-call "malloc" copy (C-sizeof "struct mcrypt_list"))
    (C->= copy "struct mcrypt_list elements" 0)
    (copy-alien-address! mlist copy)
    mlist))

(define (make-mcrypt-size-list-cleanup mlist)
  (named-lambda (mcrypt-size-list-cleanup)
    (if (not (alien-null? mlist))
	(let ((elements (C-> mlist "struct mcrypt_list elements")))
	  (if (not (alien-null? elements))
	      (C-call "mcrypt_free" elements))
	  (C-call "free" mlist)
	  (alien-null! mlist)))))

(define (free-mcrypt-size-list mlist)
  (if (not (alien-null? mlist))
      (let ((elements (C-> mlist "struct mcrypt_list elements")))
	(if (not (alien-null? elements))
	    (C-call "mcrypt_free" elements))
	(C-call "free" mlist)
	(alien-null! mlist)
	(remove-cleanup mlist))))

;;;; Mcrypt name lists.

(define (mcrypt-name-list-elements mlist)
  (let ((elements (C-> mlist "struct mcrypt_list elements"))
	(size (C-> mlist "struct mcrypt_list size")))
    (let ((vector (make-vector size)))
      (let loop ((i 0))
	(if (< i size)
	    (begin
	      (vector-set! vector i (C-peek-cstringp! elements))
	      (loop (1+ i)))))
      vector)))

(define (make-mcrypt-name-list)
  (let ((mlist (make-alien '(struct |mcrypt_list|)))
	(copy (make-alien '(struct |mcrypt_list|))))
    (add-cleanup mlist (make-mcrypt-name-list-cleanup copy))
    (C-call "malloc" copy (C-sizeof "struct mcrypt_list"))
    (C->= copy "struct mcrypt_list elements" 0)
    (copy-alien-address! mlist copy)
    mlist))

(define (make-mcrypt-name-list-cleanup mlist)
  (named-lambda (mcrypt-name-list-cleanup)
    (if (not (alien-null? mlist))
	(let ((elements (C-> mlist "struct mcrypt_list elements"))
	      (size (C-> mlist "struct mcrypt_list size")))
	  (if (not (alien-null? elements))
	      (C-call "mcrypt_free_p" elements size))
	  (C-call "free" mlist)
	  (alien-null! mlist)))))

(define (free-mcrypt-name-list mlist)
  (if (not (alien-null? mlist))
      (let ((elements (C-> mlist "struct mcrypt_list elements"))
	    (size (C-> mlist "struct mcrypt_list size")))
	(if (not (alien-null? elements))
	    (C-call "mcrypt_free_p" elements size))
	(C-call "free" mlist)
	(alien-null! mlist)
	(remove-cleanup mlist))))

;;;; The cleanups list.

(define cleanups '())

(define (add-cleanup object cleaner)
  (set! cleanups (cons (weak-cons object cleaner) cleanups)))

(define (remove-cleanup object)
  (let ((entry (weak-assq object cleanups)))
    (if entry
	(set! cleanups (delq! entry cleanups))
	;; Already removed!
	)))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))

(define (cleanup-mcrypt-objects)
  (let loop ((entries cleanups)
	     (prev #f))
    (if (pair? entries)
	(let ((entry (car entries))
	      (next (cdr entries)))
	  (if (weak-pair/car? entry)
	      (loop next entries)
	      (let ((cleaner (weak-cdr entry)))
		(if prev
		    (set-cdr! prev next)
		    (set! cleanups next))
		(cleaner)
		(loop next prev)))))))

(define (reset-cleanups!)
  (for-each (lambda (entry)
	      (if (weak-pair/car? entry)
		  (let ((obj (weak-car entry)))
		    (cond ((alien? obj) (alien-null! obj))
			  ((mcrypt-context? obj)
			   (alien-null! (mcrypt-context-alien obj)))
			  (else
			   (error "Unexpected object on cleanup list:" obj))))))
	    cleanups)
  (set! cleanups '()))

(add-gc-daemon! cleanup-mcrypt-objects)
(add-event-receiver! event:after-restart reset-mcrypt-variables!)