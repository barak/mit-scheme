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

;;;; Blowfish wrapper
;;; package: (blowfish)

(declare (usual-integrations))

(C-include "blowfish")

(define (blowfish-set-key string)
  ;; Generate a Blowfish key from STRING.
  ;; STRING must be 72 bytes or less in length.
  ;; For text-string keys, use MD5 on the text, and pass the digest here.
  (guarantee-string string 'blowfish-set-key)
  (let ((length (string-length string)))
    (if (> length 72)
	(error:bad-range-argument string
				  "a string of no more than 72 characters"
				  'blowfish-set-key))
    (let ((result (make-string (C-sizeof "BF_KEY"))))
      (C-call "BF_set_key" result length string)
      result)))

(define (blowfish-ecb input output key encrypt?)
  ;; Apply Blowfish in Electronic Code Book mode.
  ;; INPUT is an 8-byte string.
  ;; OUTPUT is an 8-byte string.
  ;; KEY is a Blowfish key.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F).
  (guarantee-bfkey key 'BLOWFISH-ECB)
  (guarantee-8char-arg input 'BLOWFISH-ECB)
  (guarantee-8char-arg output 'BLOWFISH-ECB)
  (C-call "BF_ecb_encrypt" input output key (bf-de/encrypt encrypt?)))

(define (blowfish-cbc input output key init-vector encrypt?)
  ;; Apply Blowfish in Cipher Block Chaining mode.
  ;; INPUT is a string whose length is a multiple of 8 bytes.
  ;; OUTPUT is a string whose length is the same as INPUT.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte string; it is modified after each call.
  ;; The value from any call may be passed in to a later call.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F).
  (guarantee-init-vector init-vector 'BLOWFISH-CBC)
  (guarantee-bfkey key 'BLOWFISH-CBC)
  (guarantee-8Xchar-arg input 'BLOWFISH-CBC)
  (if (or (eq? input output)
	  (not (= (string-length output) (string-length input))))
      (error:bad-range-argument output
				"a string as long as the input string"
				'BLOWFISH-CBC))
  (C-call "BF_cbc_encrypt" input output (string-length input)
	  key init-vector (bf-de/encrypt encrypt?)))

(define (blowfish-cfb64 input istart iend output ostart
			key init-vector num encrypt?)
  ;; Apply Blowfish in Cipher Feed-Back mode.
  ;; (INPUT,ISTART,IEND) is an arbitrary substring.
  ;; OUTPUT is a string as large as the input substring.
  ;; OSTART says where to start writing to the output string.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte string; it is modified after each call.
  ;; The value from any call may be passed in to a later call.
  ;; The initial value must be unique for each message/key pair.
  ;; NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the
  ;; number of bytes that have previously been processed in this stream.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F). 
  ;; Returned value is the new value of NUM.
  (guarantee-bfkey key 'BLOWFISH-CFB64)
  (guarantee-init-vector init-vector 'BLOWFISH-CFB64)
  (guarantee-substring input istart iend 'BLOWFISH-CFB64)
  (guarantee-substring output ostart (+ ostart (- iend istart)) 'BLOWFISH-CFB64)
  (guarantee-init-index num 'BLOWFISH-CFB64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a substring not overlapping the input substring"
	 'BLOWFISH-CFB64))
    (C-call "do_BF_cfb64_encrypt" input istart output ostart ilen
	    key init-vector num (bf-de/encrypt encrypt?))))

(define (blowfish-ofb64 input istart iend output ostart
			key init-vector num)
  ;; Apply Blowfish in Output Feed-Back mode.
  ;; (INPUT,ISTART,IEND) is an arbitrary substring.
  ;; OUTPUT is a string as large as the input substring.
  ;; OSTART says where to start writing to the output string.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte string; it is modified after each call.
  ;;   The value from any call may be passed in to a later call.
  ;;   The initial value must be unique for each message/key pair.
  ;; NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the
  ;;   number of bytes that have previously been processed in this stream.
  ;; Returned value is the new value of NUM.
  (guarantee-bfkey key 'BLOWFISH-OFB64)
  (guarantee-init-vector init-vector 'BLOWFISH-OFB64)
  (guarantee-substring input istart iend 'BLOWFISH-OFB64)
  (guarantee-substring output ostart (+ ostart (- iend istart)) 'BLOWFISH-OFB64)
  (guarantee-init-index num 'BLOWFISH-OFB64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a substring not overlapping the input substring"
	 'BLOWFISH-OFB64))
    (C-call "do_BF_ofb64_encrypt" input istart output ostart ilen
	    key init-vector num)))

(define (bf-de/encrypt encrypt?)
  (if encrypt? (C-enum "BF_ENCRYPT") (C-enum "BF_DECRYPT")))

(define (guarantee-8char-arg arg operator)
  (guarantee-string arg operator)
  (if (not (= 8 (string-length arg)))
      (error:bad-range-argument arg
				"an 8 character string"
				operator)))

(define (guarantee-8Xchar-arg arg operator)
  (guarantee-string arg operator)
  (if (not (= 0 (modulo (string-length arg) 8)))
      (error:bad-range-argument arg
				"a multiple of 8 characters string"
				operator)))

(define (guarantee-bfkey object operator)
  (if (not (and (string? object)
		(fix:= (C-sizeof "BF_KEY")
		       (string-length object))))
      (error:bad-range-argument object "a blowfish key" operator)))

(define (guarantee-init-vector object operator)
  (guarantee-string object operator)
  (if (not (= 8 (string-length object)))
      (error:bad-range-argument object
				"a blowfish init vector"
				operator)))

(define (guarantee-init-index object operator)
  (guarantee-fixnum object 'operator)
  (if (not (and (fix:<= 0 object) (fix:< object 8)))
      (error:bad-range-argument object
				"a blowfish init-vector index"
				operator)))

(define (blowfish-available?)
  (let ((path (ignore-errors (lambda ()
			       (system-library-pathname "blowfish-shim.so")))))
    (and (pathname? path)
	 (file-loadable? path))))

(define (blowfish-encrypt-port input output key init-vector encrypt?)
  ;; Assumes that INPUT is in blocking mode.
  (let ((key (blowfish-set-key key))
	(input-buffer (make-string 4096))
	(output-buffer (make-string 4096)))
    (dynamic-wind
     (lambda ()
       unspecific)
     (lambda ()
       (let loop ((m 0))
	 (let ((n (input-port/read-string! input input-buffer)))
	   (if (not (fix:= 0 n))
	       (let ((m
		      (blowfish-cfb64 input-buffer 0 n output-buffer 0
				      key init-vector m encrypt?)))
		 (write-substring output-buffer 0 n output)
		 (loop m))))))
     (lambda ()
       (string-fill! input-buffer #\NUL)
       (string-fill! output-buffer #\NUL)))))

(define (compute-blowfish-init-vector)
  ;; This init vector includes a timestamp with a resolution of
  ;; milliseconds, plus 20 random bits.  This should make it very
  ;; difficult to generate two identical vectors.
  (let ((iv (make-string 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (+ (* (+ (* (get-universal-time) 1000)
		     (remainder (real-time-clock) 1000))
		  #x100000)
	       (random #x100000))
	    (quotient t #x100)))
	((fix:= 8 i))
      (vector-8b-set! iv i (remainder t #x100)))
    iv))

(define (write-blowfish-file-header port)
  (write-string blowfish-file-header-v2 port)
  (newline port)
  (let ((init-vector (compute-blowfish-init-vector)))
    (write-string init-vector port)
    init-vector))

(define (read-blowfish-file-header port)
  (let ((line (read-line port)))
    (cond ((string=? blowfish-file-header-v1 line)
	   (make-string 8 #\NUL))
	  ((string=? blowfish-file-header-v2 line)
	   (let ((init-vector (make-string 8)))
	     (if (not (= 8 (read-substring! init-vector 0 8 port)))
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