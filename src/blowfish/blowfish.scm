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

;;;; The BLOWFISH option.
;;; package: (blowfish)

(declare (usual-integrations))

(C-include "blowfish")

(define (blowfish-set-key string)
  ;; Generate a Blowfish key from STRING.
  ;; STRING must be 72 bytes or less in length.
  ;; For text-string keys, use MD5 on the text, and pass the digest here.
  (guarantee string? string 'blowfish-set-key)
  (let* ((data (string->utf8 string))
	 (len (bytevector-length data)))
    (if (> len 72)
	(error:bad-range-argument
	 string "a string encodable in UTF8 with fewer than 72 bytes"
	 'blowfish-set-key))
    (let ((key (make-bytevector (C-sizeof "BF_KEY"))))
      (C-call "BF_set_key" key len data)
      key)))

(define (blowfish-ecb input output key encrypt?)
  ;; Apply Blowfish in Electronic Code Book mode.
  ;; INPUT is an 8-byte bytevector.
  ;; OUTPUT is an 8-byte bytevector.
  ;; KEY is a Blowfish key.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F).
  (guarantee-bfkey key 'BLOWFISH-ECB)
  (guarantee-8byte-arg input 'BLOWFISH-ECB)
  (guarantee-8byte-arg output 'BLOWFISH-ECB)
  (C-call "BF_ecb_encrypt" input output key (bf-de/encrypt encrypt?)))

(define (blowfish-cbc input output key init-vector encrypt?)
  ;; Apply Blowfish in Cipher Block Chaining mode.
  ;; INPUT is a bytevector whose length is a multiple of 8 bytes.
  ;; OUTPUT is a bytevector whose length is the same as INPUT.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte bytevector; it is modified after each call.
  ;; The value from any call may be passed in to a later call.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F).
  (guarantee-init-vector init-vector 'BLOWFISH-CBC)
  (guarantee-bfkey key 'BLOWFISH-CBC)
  (guarantee-8Xbyte-arg input 'BLOWFISH-CBC)
  (if (or (eq? input output)
	  (not (= (bytevector-length output) (bytevector-length input))))
      (error:bad-range-argument output
				"a bytevector as long as the input bytevector"
				'BLOWFISH-CBC))
  (C-call "BF_cbc_encrypt" input output (bytevector-length input)
	  key init-vector (bf-de/encrypt encrypt?)))

(define (blowfish-cfb64 input istart iend output ostart
			key init-vector num encrypt?)
  ;; Apply Blowfish in Cipher Feed-Back mode.
  ;; (INPUT,ISTART,IEND) is an arbitrary subbytevector.
  ;; OUTPUT is a bytevector as large as the input subbytevector.
  ;; OSTART says where to start writing to the output bytevector.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte bytevector; it is modified after each call.
  ;; The value from any call may be passed in to a later call.
  ;; The initial value must be unique for each message/key pair.
  ;; NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the
  ;; number of bytes that have previously been processed in this stream.
  ;; ENCRYPT? says whether to encrypt (non-#F) or decrypt (#F). 
  ;; Returned value is the new value of NUM.
  (guarantee-bfkey key 'BLOWFISH-CFB64)
  (guarantee-init-vector init-vector 'BLOWFISH-CFB64)
  (guarantee-subbytevector input istart iend 'BLOWFISH-CFB64)
  (guarantee-subbytevector output ostart (+ ostart (- iend istart))
			   'BLOWFISH-CFB64)
  (guarantee-init-index num 'BLOWFISH-CFB64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a subbytevector not overlapping the input subbytevector"
	 'BLOWFISH-CFB64))
    (C-call "do_BF_cfb64_encrypt" input istart output ostart ilen
	    key init-vector num (bf-de/encrypt encrypt?))))

(define (blowfish-ofb64 input istart iend output ostart
			key init-vector num)
  ;; Apply Blowfish in Output Feed-Back mode.
  ;; (INPUT,ISTART,IEND) is an arbitrary subbytevector.
  ;; OUTPUT is a bytevector as large as the input subbytevector.
  ;; OSTART says where to start writing to the output bytevector.
  ;; KEY is a Blowfish key.
  ;; INIT-VECTOR is an 8-byte bytevector; it is modified after each call.
  ;;   The value from any call may be passed in to a later call.
  ;;   The initial value must be unique for each message/key pair.
  ;; NUM is a digit from 0 to 7 inclusive; it is the low 3 bits of the
  ;;   number of bytes that have previously been processed in this stream.
  ;; Returned value is the new value of NUM.
  (guarantee-bfkey key 'BLOWFISH-OFB64)
  (guarantee-init-vector init-vector 'BLOWFISH-OFB64)
  (guarantee-subbytevector input istart iend 'BLOWFISH-OFB64)
  (guarantee-subbytevector output ostart (+ ostart (- iend istart))
			   'BLOWFISH-OFB64)
  (guarantee-init-index num 'BLOWFISH-OFB64)
  (let ((ilen (- iend istart)))
    (if (and (eq? input output)
	     (< ostart iend)
	     (< istart (+ ostart ilen)))
	(error:bad-range-argument
	 ostart
	 "an index of a subbytevector not overlapping the input subbytevector"
	 'BLOWFISH-OFB64))
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
	   (if (and (not (eof-object? n))
		    (not (fix:= 0 n)))
	       (let ((m
		      (blowfish-cfb64 input-buffer 0 n output-buffer 0
				      key init-vector m encrypt?)))
		 (write-bytevector output-buffer output 0 n)
		 (loop m))))))
     (lambda ()
       (bytevector-fill! input-buffer 0)
       (bytevector-fill! output-buffer 0)))))

(define (compute-blowfish-init-vector)
  ;; This init vector includes a timestamp with a resolution of
  ;; milliseconds, plus 20 random bits.  This should make it very
  ;; difficult to generate two identical vectors.
  (let ((iv (make-bytevector 8)))
    (do ((i 0 (fix:+ i 1))
	 (t (+ (* (+ (* (get-universal-time) 1000)
		     (remainder (real-time-clock) 1000))
		  #x100000)
	       (random #x100000))
	    (quotient t #x100)))
	((fix:= 8 i))
      (bytevector-u8-set! iv i (remainder t #x100)))
    iv))

(define (write-blowfish-file-header port)
  (write-bytevector blowfish-file-header-v2 port)
  (write-u8 (char->integer #\newline) port)
  (let ((init-vector (compute-blowfish-init-vector)))
    (write-bytevector init-vector port)
    init-vector))

(define (read-blowfish-file-header port)
  (let ((line (read-header port)))
    (cond ((bytevector=? blowfish-file-header-v1 line)
	   (make-bytevector 8 #\NUL))
	  ((bytevector=? blowfish-file-header-v2 line)
	   (read-bytevector 8 port))
	  (else
	   (error:bad-range-argument port 'READ-BLOWFISH-FILE-HEADER)))))

(define (read-header port)
  (let loop ((bytes '()))
    (let ((byte (read-u8 port)))
      (if (eof-object? byte)
	  (apply bytevector (reverse! bytes))
	  (if (fix:= byte (char->integer #\newline))
	      (apply bytevector (reverse! bytes))
	      (loop (cons byte bytes)))))))

(define (blowfish-file? pathname)
  (let ((line (call-with-binary-input-file pathname read-header)))
    (and (not (eof-object? line))
	 (or (bytevector=? line blowfish-file-header-v1)
	     (bytevector=? line blowfish-file-header-v2)))))

(define blowfish-file-header-v1 (string->utf8 "Blowfish, 16 rounds"))
(define blowfish-file-header-v2 (string->utf8 "Blowfish, 16 rounds, version 2"))