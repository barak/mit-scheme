#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; mhash wrapper
;;; package: (mhash)

(declare (usual-integrations))

(C-include "mhash")

(define mhash-initialized? #f)
(define mhash-algorithm-names)
(define mhash-contexts '())
(define mhash-hmac-contexts '())
(define mhash-contexts-mutex)

(define (add-context-cleanup context)
  (with-thread-mutex-locked mhash-contexts-mutex
    (lambda ()
      (set! mhash-contexts
	    (cons (weak-cons context (mhash-context-alien context))
		  mhash-contexts)))))

(define (add-hmac-context-cleanup context)
  (with-thread-mutex-locked mhash-contexts-mutex
    (lambda ()
      (set! mhash-hmac-contexts
	    (cons (weak-cons context (mhash-hmac-context-alien context))
		  mhash-contexts)))))

(define (remove-context-cleanup context)
  (with-thread-mutex-locked mhash-contexts-mutex
    (lambda ()
      (let ((entry (weak-assq context mhash-contexts)))
	(if entry
	    (set! mhash-contexts (delq! context mhash-contexts)))))))

(define (remove-hmac-context-cleanup context)
  (with-thread-mutex-locked mhash-contexts-mutex
    (lambda ()
      (let ((entry (weak-assq context mhash-hmac-contexts)))
	(if entry
	    (set! mhash-hmac-contexts (delq! context mhash-hmac-contexts)))))))

(define (weak-assq obj alist)
  (let loop ((alist alist))
    (if (null? alist) #f
	(let* ((entry (car alist))
	       (key (weak-car entry)))
	  (if (eq? obj key) entry
	      (loop (cdr alist)))))))

(define (cleanup-contexts)
  (let loop ((entries mhash-contexts)
	     (prev #f))
    (if (pair? entries)
	(let ((entry (car entries))
	      (next (cdr entries)))
	  (if (weak-pair/car? entry)
	      (loop next entries)
	      (let ((context (weak-cdr entry)))
		(if prev
		    (set-cdr! prev next)
		    (set! mhash-contexts next))
		(if (not (alien-null? context))
		    (begin
		      (C-call "mhash_deinit" context 0)
		      (alien-null! context)))
		(loop next prev)))))))

(define (cleanup-hmac-contexts)
  (let loop ((entries mhash-hmac-contexts)
	     (prev #f))
    (if (pair? entries)
	(let ((entry (car entries))
	      (next (cdr entries)))
	  (if (weak-pair/car? entry)
	      (loop next entries)
	      (let ((context (weak-cdr entry)))
		(if prev
		    (set-cdr! prev next)
		    (set! mhash-hmac-contexts next))
		(if (not (alien-null? context))
		    (begin
		      (C-call "mhash_hmac_deinit" context 0)
		      (alien-null! context)))
		(loop next prev)))))))

(define (cleanup-mhash-contexts)
  (if (not (thread-mutex-owner mhash-contexts-mutex))
      (begin
	(cleanup-contexts)
	(cleanup-hmac-contexts))))

(define (mhash-name->id name procedure)
  (let ((n (vector-length mhash-algorithm-names)))
    (let loop ((i 0))
      (cond ((fix:= i n) (error:bad-range-argument name procedure))
	    ((eq? name (vector-ref mhash-algorithm-names i)) i)
	    (else (loop (fix:+ i 1)))))))

(define-structure mhash-context mutex alien id)
(define-structure mhash-hmac-context mutex alien id)

(define (guarantee-mhash-context object procedure)
  (if (not (mhash-context? object))
      (error:wrong-type-argument object "mhash context" procedure))
  (if (alien-null? (mhash-context-alien object))
      (error:bad-range-argument object procedure)))

(define (guarantee-mhash-hmac-context object procedure)
  (if (not (mhash-hmac-context? object))
      (error:wrong-type-argument object "mhash HMAC context" procedure))
  (if (alien-null? (mhash-hmac-context-alien object))
      (error:bad-range-argument object procedure)))

(define (with-context-locked context thunk)
  (with-thread-mutex-locked (mhash-context-mutex context) thunk))

(define (with-hmac-context-locked context thunk)
  (with-thread-mutex-locked (mhash-hmac-context-mutex context) thunk))

(define (with-context-locked-open context operator receiver)
  (with-thread-mutex-locked (mhash-context-mutex context)
    (lambda ()
      (let ((alien (mhash-context-alien context)))
	(if (alien-null? alien)
	    (error:bad-range-argument context operator))
	(receiver alien)))))

(define (with-hmac-context-locked-open context operator receiver)
  (with-thread-mutex-locked (mhash-hmac-context-mutex context)
    (lambda ()
      (let ((alien (mhash-hmac-context-alien context)))
	(if (alien-null? alien)
	    (error:bad-range-argument context operator))
	(receiver alien)))))

(define (mhash-type-names)
  (names-vector->list mhash-algorithm-names))

(define (mhash-get-block-size name)
  (C-call "mhash_get_block_size"
	  (mhash-name->id name 'MHASH-GET-BLOCK-SIZE)))

(define (mhash-init name)
  (let ((id (mhash-name->id name 'MHASH-INIT))
	(alien (make-alien '|MHASH_INSTANCE|)))
    (let ((context (make-mhash-context (make-thread-mutex) alien id)))
      (add-context-cleanup context)
      (with-context-locked context
	(lambda ()
	  (C-call "mhash_init" alien id)
	  (if (alien-null? alien)	; == MHASH_FAILED
	      (error "Unable to allocate mhash context:" name))))
      context)))

(define (mhash-update context string start end)
  (guarantee-substring string start end 'MHASH-UPDATE)
  (with-context-locked-open context 'MHASH-UPDATE
    (lambda (alien)
      (C-call "do_mhash" alien string start end))))

(define (mhash-end context)
  (with-context-locked-open context 'MHASH-END
    (lambda (alien)
      (let* ((id (mhash-context-id context))
	     (size (C-call "mhash_get_block_size" id))
	     (digest (make-string size)))
	(C-call "do_mhash_end" alien digest size)
	(remove-context-cleanup context)
	digest))))

(define (mhash-hmac-init name key)
  (guarantee-string key 'HMASH-HMAC-INIT)
  (let ((id (mhash-name->id name 'MHASH-HMAC-INIT))
	(alien (make-alien '|MHASH_INSTANCE|)))
    (let ((context (make-mhash-hmac-context (make-thread-mutex) alien id))
	  (block-size (C-call "mhash_get_hash_pblock" id))
	  (key-size (string-length key)))
      (add-hmac-context-cleanup context)
      (with-hmac-context-locked context
	(lambda ()
	  (C-call "mhash_hmac_init" alien id key key-size block-size)
	  (if (alien-null? alien)	; == MHASH_FAILED
	      (error "Unable to allocate mhash HMAC context:" name))))
      context)))

(define (mhash-hmac-update context string start end)
  (guarantee-substring string start end 'MHASH-HMAC-UPDATE)
  (with-hmac-context-locked-open context 'MHASH-HMAC-UPDATE
    (lambda (alien)
      (C-call "do_mhash" alien string start end))))

(define (mhash-hmac-end context)
  (with-hmac-context-locked-open context 'MHASH-HMAC-END
    (lambda (alien)
      (let* ((id (mhash-hmac-context-id context))
	     (size (C-call "mhash_get_block_size" id))
	     (digest (make-string size)))
	(C-call "do_mhash_hmac_end" alien digest size)
	(remove-hmac-context-cleanup context)
	digest))))

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
  (not (zero? (C-call "mhash_keygen_uses_salt"
		      (keygen-name->id name 'MHASH-KEYGEN-USES-SALT?)))))

(define (mhash-keygen-uses-count? name)
  (not (zero? (C-call "mhash_keygen_uses_count"
		      (keygen-name->id name 'MHASH-KEYGEN-USES-COUNT?)))))

(define (mhash-keygen-uses-hash-algorithm name)
  (C-call "mhash_keygen_uses_hash_algorithm"
	  (keygen-name->id name 'MHASH-KEYGEN-USES-HASH-ALGORITHM)))

(define (mhash-keygen-salt-size name)
  (C-call "mhash_get_keygen_salt_size"
	  (keygen-name->id name 'MHASH-KEYGEN-SALT-SIZE)))

(define (mhash-keygen-max-key-size name)
  (C-call "mhash_get_keygen_max_key_size"
	  (keygen-name->id name 'MHASH-KEYGEN-MAX-KEY-SIZE)))

(define (mhash-keygen type passphrase #!optional salt)
  (if (not (mhash-keygen-type? type))
      (error:wrong-type-argument type "mhash type" 'MHASH-KEYGEN))
  (let ((keygenid (mhash-keygen-type-id type))
	(keyword-size (mhash-keygen-type-key-length type)))
    (let ((params (salted-keygen-params
		   keygenid (mhash-keygen-type-parameter-vector type) salt))
	  (keyword (make-string keyword-size))
	  (max-key-size (C-call "mhash_get_keygen_max_key_size" keygenid)))

      (define (hashid-map params i)
	(let ((name (vector-ref params i)))
	  (if (not name)
	      0
	      (mhash-name->id name 'MHASH-KEYGEN))))

      (if (not (or (zero? max-key-size)
		   (< max-key-size (string-length keyword))))
	  (error "keyword size exceeds maximum:" max-key-size type))
      (if (not (zero? (C-call "do_mhash_keygen"
			      keygenid
			      (hashid-map params 3) ;hash_algorithm[0]
			      (hashid-map params 4) ;hash_algorithm[1]
			      (vector-ref params 1) ;count
			      (vector-ref params 0) ;salt
			      (string-length (vector-ref params 0))
			      keyword keyword-size
			      passphrase (string-length passphrase))))
	  (error "Error signalled by mhash_keygen."))
      keyword)))

(define (salted-keygen-params id params #!optional salt)
  (if (not (zero? (C-call "mhash_keygen_uses_salt" id)))
      (begin
	(if (or (default-object? salt) (not salt))
	    (error "Salt required:"
		   (vector-ref mhash-keygen-names id)))
	(let ((n (C-call "mhash_get_keygen_salt_size" id)))
	  (if (not (or (= n 0)
		       (= n (string-length salt))))
	      (error "Salt size incorrect:"
		     (string-length salt)
		     (error-irritant/noise "; should be:")
		     n)))
	(let ((p (vector-copy params)))
	  (vector-set! p 0 salt)
	  p))
      params))

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
  (let ((path (ignore-errors (lambda ()
			       (system-library-pathname "mhash-shim.so")))))
    (and (pathname? path)
	 (file-loadable? path)
	 (begin
	   (if (not mhash-initialized?)
	       (begin
		 (set! mhash-algorithm-names
		       (make-names-vector
			(lambda () (C-call "mhash_count"))
			(lambda (hashid)
			  (let* ((alien (make-alien-to-free
					 '(* char)
					 (lambda (alien)
					   (C-call "mhash_get_hash_name"
						   alien hashid))))
				 (str (c-peek-cstring alien)))
			    (free alien)
			    str))))
		 (set! mhash-keygen-names
		       (make-names-vector
			(lambda () (C-call "mhash_keygen_count"))
			(lambda (keygenid)
			  (let* ((alien (make-alien-to-free
					 '(* char)
					 (lambda (alien)
					   (C-call "mhash_get_keygen_name"
						   alien keygenid))))
				 (str (c-peek-cstring alien)))
			    (free alien)
			    str))))
		 (set! mhash-initialized? #t)))
	   #t))))

(define (reset-mhash-variables!)
  (set! mhash-initialized? #f)
  (for-each (lambda (weak) (alien-null! (weak-cdr weak))) mhash-contexts)
  (set! mhash-contexts '())
  (for-each (lambda (weak) (alien-null! (weak-cdr weak))) mhash-hmac-contexts)
  (set! mhash-hmac-contexts '())
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

;;;; Package initialization

(define (initialize-package!)
  (set! mhash-contexts-mutex (make-thread-mutex))
  (reset-mhash-variables!)
  (add-gc-daemon! cleanup-mhash-contexts)
  (add-event-receiver! event:after-restart reset-mhash-variables!))

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