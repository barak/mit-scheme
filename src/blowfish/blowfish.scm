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

;;;; Interface to Blowfish
;;; package: (blowfish)

(declare (usual-integrations))

(define (import-blowfish)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(blowfish))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
	      '(blowfish-cbc
		blowfish-cfb64
		blowfish-ecb
		blowfish-encrypt-port
		blowfish-file?
		blowfish-ofb64
		blowfish-set-key
		compute-blowfish-init-vector
		read-blowfish-file-header
		write-blowfish-file-header))))

(C-include "blowfish")

(define (blowfish-set-key bytes)
  (guarantee bytevector? bytes 'blowfish-set-key)
  (let ((len (bytevector-length bytes)))
    (if (> len 72)
	(error:bad-range-argument bytes "72 or fewer bytes" 'blowfish-set-key))
    (let ((key (make-bytevector (C-sizeof "BF_KEY"))))
      (C-call "BF_set_key" key len bytes)
      key)))

(define (blowfish-ecb input output key encrypt?)
  (guarantee-bfkey key 'blowfish-ecb)
  (guarantee-8byte-arg input 'blowfish-ecb)
  (guarantee-8byte-arg output 'blowfish-ecb)
  (C-call "BF_ecb_encrypt" input output key (bf-de/encrypt encrypt?)))

(define (blowfish-cbc input output key init-vector encrypt?)
  (guarantee-init-vector init-vector 'blowfish-cbc)
  (guarantee-bfkey key 'blowfish-cbc)
  (guarantee-8Xbyte-arg input 'blowfish-cbc)
  (if (or (eq? input output)
	  (not (= (bytevector-length output) (bytevector-length input))))
      (error:bad-range-argument output
				"a bytevector as long as the input bytevector"
				'blowfish-cbc))
  (C-call "BF_cbc_encrypt" input output (bytevector-length input)
	  key init-vector (bf-de/encrypt encrypt?)))

(define (blowfish-cfb64 input istart iend output ostart
			key init-vector num encrypt?)
  (guarantee-bfkey key 'blowfish-cfb64)
  (guarantee-init-vector init-vector 'blowfish-cfb64)
  (guarantee-subbytevector input istart iend 'blowfish-cfb64)
  (guarantee-subbytevector output ostart (+ ostart (- iend istart))
			   'blowfish-cfb64)
  (guarantee-init-index num 'blowfish-cfb64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a subbytevector not overlapping the input subbytevector"
	 'blowfish-cfb64))
    (C-call "do_BF_cfb64_encrypt" input istart output ostart ilen
	    key init-vector num (bf-de/encrypt encrypt?))))

(define (blowfish-ofb64 input istart iend output ostart
			key init-vector num)
  (guarantee-bfkey key 'blowfish-ofb64)
  (guarantee-init-vector init-vector 'blowfish-ofb64)
  (guarantee-subbytevector input istart iend 'blowfish-ofb64)
  (guarantee-subbytevector output ostart (+ ostart (- iend istart))
			   'blowfish-ofb64)
  (guarantee-init-index num 'blowfish-ofb64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a subbytevector not overlapping the input subbytevector"
	 'blowfish-ofb64))
    (C-call "do_BF_ofb64_encrypt" input istart output ostart ilen
	    key init-vector num)))

(define (bf-de/encrypt encrypt?)
  (if encrypt? (C-enum "BF_ENCRYPT") (C-enum "BF_DECRYPT")))

(define (guarantee-8byte-arg arg operator)
  (guarantee bytevector? arg operator)
  (if (not (= 8 (bytevector-length arg)))
      (error:bad-range-argument arg
				"8 bytes"
				operator)))

(define (guarantee-8Xbyte-arg arg operator)
  (guarantee bytevector? arg operator)
  (if (not (= 0 (modulo (bytevector-length arg) 8)))
      (error:bad-range-argument arg
				"a multiple of 8 bytes"
				operator)))

(define (guarantee-bfkey object operator)
  (if (not (and (bytevector? object)
		(fix:= (C-sizeof "BF_KEY")
		       (bytevector-length object))))
      (error:bad-range-argument object "a blowfish key" operator)))

(define (guarantee-init-vector object operator)
  (guarantee bytevector? object operator)
  (if (not (= 8 (bytevector-length object)))
      (error:bad-range-argument object
				"a blowfish init vector"
				operator)))

(define (guarantee-init-index object operator)
  (guarantee fixnum? object 'operator)
  (if (not (and (fix:<= 0 object) (fix:< object 8)))
      (error:bad-range-argument object
				"a blowfish init-vector index"
				operator)))

(define (guarantee-subbytevector object start end operator)
  (guarantee bytevector? object operator)
  (guarantee index-fixnum? start operator)
  (guarantee index-fixnum? end operator)
  (if (not (fix:<= start end))
      (error:bad-range-argument start operator))
  (if (not (fix:<= end (bytevector-length object)))
      (error:bad-range-argument end operator)))

(define (blowfish-encrypt-port input output key init-vector encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key key))
	(input-buffer (make-bytevector 4096))
	(output-buffer (make-bytevector 4096)))
    (dynamic-wind
     (lambda ()
       unspecific)
     (lambda ()
       (let loop ((m 0))
	 (let ((n (read-bytevector! input-buffer input)))
	   (if (and n (not (eof-object? n)))
	       (let ((m
		      (blowfish-cfb64 input-buffer 0 n output-buffer 0
				      key init-vector m encrypt?)))
		 (let ((n* (write-bytevector output-buffer output 0 n)))
		   (if (not (eqv? n n*))
		       (error "Short write (requested, actual):" n n*)))
		 (loop m))))))
     (lambda ()
       (bytevector-fill! input-buffer 0)
       (bytevector-fill! output-buffer 0)))))

(define (compute-blowfish-init-vector)
  (let ((iv (make-bytevector 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (+ (* (+ (* (get-universal-time) 1000)
		     (remainder (real-time-clock) 1000))
		  #x100000)
	       (random #x100000))
	    (quotient t #x100)))
	((not (fix:< i 8)))
      (bytevector-u8-set! iv i (remainder t #x100)))
    iv))

(define (write-blowfish-file-header port)
  (write-bytevector blowfish-file-header-v2 port)
  (let ((init-vector (compute-blowfish-init-vector)))
    (write-bytevector init-vector port)
    init-vector))

(define (read-blowfish-file-header port)
  (let ((version (try-read-blowfish-file-header port)))
    (if (not version)
	(error:bad-range-argument port 'read-blowfish-file-header))
    (if (= version 1)
	(make-bytevector 8 0)
	(or (%safe-read-bytevector 8 port)
	    (error "Short read while getting init-vector:" port)))))

(define (try-read-blowfish-file-header port)
  (let* ((n (bytevector-length blowfish-file-header-v1))
	 (bv1 (%safe-read-bytevector n port)))
    (and bv1
	 (if (bytevector=? bv1 blowfish-file-header-v1)
	     1
	     (let* ((m (fix:- (bytevector-length blowfish-file-header-v2) n))
		    (bv2 (%safe-read-bytevector m port)))
	       (and bv2
		    (bytevector=? (bytevector-append bv1 bv2)
				  blowfish-file-header-v2)
		    2))))))

(define (%safe-read-bytevector n port)
  (let ((bv (read-bytevector n port)))
    (and bv
	 (not (eof-object? bv))
	 (fix:= (bytevector-length bv) n)
	 bv)))

(define (blowfish-file? pathname)
  (call-with-binary-input-file pathname try-read-blowfish-file-header))

(define blowfish-file-header-v1
  (string->utf8 "Blowfish, 16 rounds\n"))

(define blowfish-file-header-v2
  (string->utf8 "Blowfish, 16 rounds, version 2\n"))