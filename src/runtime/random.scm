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

;;;; Random Number Generator
;;; package: (runtime random-number)

(declare (usual-integrations))

;;; Kludges to make this work in the cold load before much else has
;;; been loaded.

(define-primitives
  (allocate-bytevector 1)
  (char->integer 1)
  (bytevector-copy! 5)
  (bytevector-fill! 4)
  (bytevector-length 1)
  (bytevector-u8-ref 2)
  (bytevector-u8-set! 3))

(define (make-bytevector n b)
  (let ((bv (allocate-bytevector n)))
    (do ((i 0 (fix:+ i 1)))
	((fix:>= i n))
      (bytevector-u8-set! bv i b))
    bv))

(define (bytevector-zero-explicit! bv)
  ;; Don't let any compiler optimize this away.
  ((no-op bytevector-fill!) bv 0 0 (bytevector-length bv)))

(define (no-op x)
  x)

;;;; Random state data structure

;;; Just a container for a 32-byte ChaCha20 key.
;;;
;;; XXX For greater throughput, we could use an arbitrary-size buffer
;;; of outputs under a single key, erased as we read through them.

(define-integrable random-state-tag '|#[(runtime random-number)random-state]|)

(define-integrable (%make-random-state)
  (vector random-state-tag (make-bytevector 32 0)))

(define (random-state? object)
  (and (vector? object)
       (fix:= 2 (vector-length object))
       (eq? random-state-tag (vector-ref object 0))))
(register-predicate! random-state? 'random-state '<= vector?)

(define-guarantee random-state "random state")

(define-print-method random-state?
  (standard-print-method 'random-state))

(define-integrable (random-state-key s) (vector-ref s 1))

(define (make-random-state #!optional state)
  (if (or (eq? #t state) (int:integer? state))
      (let ((state (%make-random-state)))
	(random-source-randomize! state)
	state)
      (with-random-state state 'make-random-state
	(lambda (state)
	  (let ((state* (%make-random-state)))
	    (let ((key* (random-state-key state*))
		  (key (random-state-key state)))
	      (bytevector-copy! key* 0 key 0 32))
	    state*)))))

(declare (integrate-operator with-random-state))
(define (with-random-state state procedure body)
  (let ((state
	 (if (or (default-object? state) (not state))
	     (or *random-state* default-random-source)
	     state)))
    (guarantee-random-state state procedure)
    (with-random-state-lock state
      (lambda ()
	(body state)))))

(define-integrable (with-random-state-lock state thunk)
  (if (eq? state default-random-source)
      (with-thread-mutex-lock default-random-source-mutex
	(lambda ()
	  (without-interruption thunk)))
      (thunk)))

;;; Legacy API

(define (random modulus #!optional state)
  (cond ((int:integer? modulus)
	 (if (int:> modulus 0)
	     (%random-integer modulus state)
	     (error:bad-range-argument modulus 'random)))
	((flo:flonum? modulus)
	 (if (flo:> modulus 0.)
	     (flo:* (flo:random-unit-open state) modulus)
	     (error:bad-range-argument modulus 'random)))
	((real? modulus)
	 (error "Unsupported modulus:" modulus))
	(else
	 (error:wrong-type-argument modulus "real number" 'random))))

;;;; Export/import

;;; Just a vector of the 32 bytes of the ChaCha20 key.

(define-integrable ers:tag 'random-state-v2)
(define-integrable ers:length 33)

(define (export-random-state state)
  (guarantee-random-state state 'export-random-state)
  (let ((v (make-vector ers:length))
	(key (random-state-key state)))
    (vector-set! v 0 ers:tag)
    (do ((i 0 (fix:+ i 1)))
	((fix:>= i 32))
      (vector-set! v (fix:+ 1 i) (bytevector-u8-ref key i)))
    v))

(define (import-random-state v)
  (define (lose)
    (error:wrong-type-argument v "external random state v2"
			       'import-random-state))
  (if (not (vector? v))
      (lose))
  (if (not (fix:= ers:length (vector-length v)))
      (lose))
  (if (not (eq? ers:tag (vector-ref v 0)))
      (lose))
  (let* ((state (%make-random-state))
	 (key (random-state-key state)))
    (do ((i 0 (fix:+ i 1)))
	((fix:>= i 32))
      (let ((x (vector-ref v (fix:+ 1 i))))
	(if (not (index-fixnum? x))
	    (lose))
	(if (not (fix:< x 256))
	    (lose))
	(bytevector-u8-set! key i x)))
    state))

;;;; SRFI 27 API

(define (make-random-source)
  (%make-random-state))

(define (random-source-state-ref source)
  (let ((state source))
    (export-random-state state)))

(define (random-source-state-set! source exported-state)
  (let ((dst source)
	(src (import-random-state exported-state)))
    (bytevector-copy! (random-state-key dst) 0 (random-state-key src) 0 32)))

(define (random-source-randomize! source)
  (let ((state source))
    ((ucode-primitive get-entropy 1) (random-state-key state))))

(define (random-source-pseudo-randomize! source x y)
  (let* ((state source)
	 (key (random-state-key state)))
    (do ((i 0 (+ i 1)))
	((>= i 16))
      (let ((j i)
	    (byte (bitwise-and #xff (shift-right x (* 8 i)))))
	(bytevector-u8-set! key j byte)))
    (do ((i 0 (+ i 1)))
	((>= i 16))
      (let ((j (+ i 16))
	    (byte (bitwise-and #xff (shift-right y (* 8 i)))))
	(bytevector-u8-set! key j byte)))))

(define (random-source-make-integers source)
  (guarantee-random-state source 'random-source-make-integers)
  (let ((state source))
    (lambda (modulus)
      (if (int:> modulus 0)
	  (%random-integer modulus state)
	  (error:bad-range-argument modulus #f)))))

(define (random-source-make-reals source #!optional unit)
  (guarantee-random-state source 'random-source-make-reals)
  (let ((unit
	 (if (default-object? unit)
	     .5
	     (begin
	       (if (not (and (real? unit) (< 0 unit 1)))
		   (error:wrong-type-argument unit
					      "real unit"
					      'random-source-make-reals))
	       unit)))
	(state source))
    (if (flo:flonum? unit)
	;; Ignore UNIT and return maximum precision.
	(lambda () (flo:random-unit-open state))
	;; Limit the maximum size of UNIT to avoid problems.
	(let ((m (- (truncate (/ 1 (min 1/65536 unit))) 1)))
	  (lambda ()
	    (* unit (%random-integer m state)))))))

;;;; Core algorithm

;;; Given key k, split the 64-byte ChaCha20_k(0) into 32-byte halves x
;;; and k'; yield x as the output and replace the state by k'.

(define-integrable chacha20-core (ucode-primitive chacha20-core 5))

(define (%random-bytevector-short! bv start end state)
  (let ((key (random-state-key state))
	(output (allocate-bytevector 64)))
    (chacha20-core output 0 zero16 key chacha-const)
    (bytevector-copy! key 0 output 0 32)
    (bytevector-copy! bv start output 32 (fix:+ 32 (fix:- end start)))
    (bytevector-zero-explicit! output)))

(define (random-bytevector! bv #!optional start end state)
  (let* ((end (fix:end-index end (bytevector-length bv) 'random-bytevector!))
         (start (fix:start-index start end 'random-bytevector!))
         (n (- end start)))
    (if (fix:< n 32)
        (with-random-state state 'random-bytevector
          (lambda (state)
            ;; Small enough to be serviced in a single request.
            (%random-bytevector-short! bv start end state)))
        (let ((key (allocate-bytevector 32))
              (nonce (make-bytevector 16 0)))
          ;; Grab a key in a single request; then derive a long byte
          ;; vector from the key.
          (with-random-state state 'random-bytevector
            (lambda (state)
              (%random-bytevector-short! key 0 32 state)))
          (let ((n/64 (fix:quotient n 64)))
            (do ((i 0 (fix:+ i 1)))
                ((fix:>= i n/64))
              (chacha20-core bv (fix:+ start (fix:* i 64))
                             nonce key chacha-const)
              (let loop ((j 0) (t 1))
                (if (fix:< j 8)
                    (let ((t (fix:+ t (bytevector-u8-ref nonce j))))
                      (bytevector-u8-set! nonce j (fix:and t #xff))
                      (loop (fix:+ j 1) (fix:lsh t -8))))))
            (let* ((rem (fix:- n (fix:* n/64 64))))
              (if (fix:positive? rem)
                  (let ((output (allocate-bytevector 64)))
                    (chacha20-core output 0 nonce key chacha-const)
                    (bytevector-copy! bv (fix:+ start (fix:* n/64 64))
                                      output 0 rem)
                    (bytevector-zero-explicit! output))))
            (bytevector-zero-explicit! key))))))

(define (random-bytevector n #!optional state)
  (let ((bytes (allocate-bytevector n)))
    (random-bytevector! bytes 0 n state)
    bytes))

;;;; Integers

(define (%random-integer n state)
  (if (fix:fixnum? n)
      ;; 2^k mod n = 1 + (2^k - 1 - n) mod n = 1 + (L - n) mod n where
      ;; L = 2^k - 1 is (fix:largest-value).
      (let ((m (fix:remainder (fix:+ 1 (fix:- (fix:largest-value) n)) n)))
	(let loop ()
	  (let ((r (%random-nonnegative-fixnum state)))
	    (if (fix:< r m)
		(loop)
		(fix:remainder r n)))))
      (let* ((nbytes (quotient (integer-length n) 8))
	     (m (remainder (- (shift-left 1 (* 8 nbytes)) n) n)))
	(let loop ()
	  (let ((r (%random-integer-of-length-in-bytes nbytes state)))
	    (if (< r m)
		(loop)
		(int:remainder r n)))))))

;;; Assumes 6-bit type codes, 1 bit for sign.

(define-integrable high-fixnum-byte-mask #x1)

(select-on-bytes-per-word
 ;; 4
 (define (%random-nonnegative-fixnum state)
   (let ((bv (random-bytevector 4 state)))
     (let ((b0 (bytevector-u8-ref bv 0))
	   (b1 (bytevector-u8-ref bv 1))
	   (b2 (bytevector-u8-ref bv 2))
	   (b3 (fix:and (bytevector-u8-ref bv 3) high-fixnum-byte-mask)))
       (declare (integrate b0 b1 b2 b3))
       (begin0 (fix:or (fix:or b0 (fix:lsh b1 8))
		       (fix:or (fix:lsh b2 16) (fix:lsh b3 24)))
	 (bytevector-zero-explicit! bv)))))
 ;; 8
 (define (%random-nonnegative-fixnum state)
   (let ((bv (random-bytevector 8 state)))
     (let ((b0 (bytevector-u8-ref bv 0))
	   (b1 (bytevector-u8-ref bv 1))
	   (b2 (bytevector-u8-ref bv 2))
	   (b3 (bytevector-u8-ref bv 3))
	   (b4 (bytevector-u8-ref bv 4))
	   (b5 (bytevector-u8-ref bv 5))
	   (b6 (bytevector-u8-ref bv 6))
	   (b7 (fix:and (bytevector-u8-ref bv 7) high-fixnum-byte-mask)))
       (declare (integrate b0 b1 b2 b3 b4 b5 b6 b7))
       (begin0 (fix:or
		(fix:or (fix:or b0 (fix:lsh b1 8))
			(fix:or (fix:lsh b2 16) (fix:lsh b3 24)))
		(fix:or (fix:or (fix:lsh b4 32) (fix:lsh b5 40))
			(fix:or (fix:lsh b6 48) (fix:lsh b7 56))))
	 (bytevector-zero-explicit! bv))))))

(define (%random-integer-of-length-in-bytes nbytes state)
  (let ((bv (random-bytevector nbytes state)))
    (define-integrable (byte i) (bytevector-u8-ref bv i))
    (begin0 (do ((i 0 (fix:+ i 1))
		 (n 0 (bitwise-ior n (shift-left (byte i) (fix:* 8 i)))))
		((fix:>= i nbytes) n))
      (bytevector-zero-explicit! bv))))

;;;; Count leading zeros in 16-bit and 32-bit words

(select-on-bytes-per-word
 ;; 4
 (begin
   (declare (integrate-operator fix:bitcount16))
   (define (fix:bitcount16 x)
     (let* ((x1 (fix:- x (fix:and (fix:lsh x -1) #x5555)))
	    (x2 (fix:+ (fix:and (fix:lsh x1 -2) #x3333) (fix:and x1 #x3333)))
	    (x3 (fix:and (fix:+ x2 (fix:lsh x2 -4)) #x0f0f))
	    (c0 x3)
	    (c1 (fix:lsh x3 -8))
	    (s (fix:+ c0 c1)))
       (declare (integrate x1 x2 x3 x4 c0 c1 s))
       (fix:and s #x1f)))

   (declare (integrate-operator fix:clz16))
   (define (fix:clz16 x)
     (let* ((x2 (fix:or x (fix:lsh x -1)))
	    (x4 (fix:or x2 (fix:lsh x2 -2)))
	    (x8 (fix:or x4 (fix:lsh x4 -4)))
	    (x16 (fix:or x8 (fix:lsh x8 -8))))
       (declare (integrate x2 x4 x8 x16))
       (fix:- 16 (fix:bitcount16 x16))))

   (define (%random-16 state)
     (let ((bv (random-bytevector 2 state)))
       (let ((b0 (bytevector-u8-ref bv 0))
	     (b1 (bytevector-u8-ref bv 1)))
	 (declare (integrate b0 b1))
	 (begin0 (fix:or b0 (fix:lsh b1 8))
	   (bytevector-zero-explicit! bv))))))

 ;; 8
 (begin
   (declare (integrate-operator fix:bitcount32))
   (define (fix:bitcount32 x)
     (let* ((x1 (fix:- x (fix:and (fix:lsh x -1) #x55555555)))
	    (x2
	     (fix:+ (fix:and (fix:lsh x1 -2) #x33333333) (fix:and x1 #x33333333)))
	    (x3 (fix:and (fix:+ x2 (fix:lsh x2 -4)) #x0f0f0f0f))
	    (c0 x3)
	    (c1 (fix:lsh x3 -8))
	    (c2 (fix:lsh x3 -16))
	    (c3 (fix:lsh x3 -24))
	    (s (fix:+ (fix:+ c0 c1) (fix:+ c2 c3))))
       (declare (integrate x1 x2 x3 x4 c0 c1 c2 c3 s))
       (fix:and s #xff)))

   (declare (integrate-operator fix:clz32))
   (define (fix:clz32 x)
     (let* ((x2 (fix:or x (fix:lsh x -1)))
	    (x4 (fix:or x2 (fix:lsh x2 -2)))
	    (x8 (fix:or x4 (fix:lsh x4 -4)))
	    (x16 (fix:or x8 (fix:lsh x8 -8)))
	    (x32 (fix:or x16 (fix:lsh x16 -8))))
       (declare (integrate x2 x4 x8 x16 x32))
       (fix:- 32 (fix:bitcount32 x32))))

   (define (%random-32 state)
     (let ((bv (random-bytevector 4 state)))
       (let ((b0 (bytevector-u8-ref bv 0))
	     (b1 (bytevector-u8-ref bv 1))
	     (b2 (bytevector-u8-ref bv 2))
	     (b3 (bytevector-u8-ref bv 3)))
	 (declare (integrate b0 b1 b2 b3))
	 (begin0 (fix:or (fix:or b0 (fix:lsh b1 8))
			 (fix:or (fix:lsh b2 16) (fix:lsh b3 24)))
	   (bytevector-zero-explicit! bv))))))
)

;;;; Uniform [0,1] sampler and uniform (2^{emin - p - 1}, 1 - ulp(1)/4) sampler

(select-on-bytes-per-word
 ;; 4
 (begin
   (define (flo:random-unit-closed state)
     (define (exponent)
       (let loop ((z 0))
	 (if (fix:>= z 1088)
	     z
	     (let ((x (%random-16 state)))
	       (if (fix:= x 0)
		   (loop (fix:+ z 16))
		   (fix:+ z (fix:clz16 x)))))))
     (define (significand-64)
       (let ((s0 (int:->flonum (fix:or (%random-16 state) #x0001)))
	     (s1 (int:->flonum (%random-16 state)))
	     (s2 (int:->flonum (%random-16 state)))
	     (s3 (int:->flonum (fix:or (%random-16 state) #x8000))))
	 (let ((lo (flo:+ (flo:* 65536. s1) s0))
	       (hi (flo:+ (flo:* 65536. s3) s2)))
	   (declare (integrate lo hi))
	   (flo:+ (flo:* 4294967296. hi) lo))))
     (flo:ldexp (significand-64) (fix:- -64 (exponent))))

   (define (flo:random-unit-open state)
     (define (exponent)
       (let loop ((z 0))
	 (if (fix:>= z 1088)
	     z
	     (let ((x (%random-16 state)))
	       (if (fix:= x 0)
		   (loop (fix:+ z 16))
		   (fix:+ z (fix:clz16 x)))))))
     (define (significand-53)
       (let ((s0 (int:->flonum (%random-16 state)))
	     (s1 (int:->flonum (%random-16 state)))
	     (s2 (int:->flonum (%random-16 state)))
	     (s3
	      (int:->flonum (fix:or #x10 (fix:and (%random-16 state) #xf)))))
	 (let ((lo (flo:+ (flo:* 65536. s1) s0))
	       (hi (flo:+ (flo:* 65536. s3) s2)))
	   (declare (integrate lo hi))
	   (flo:+ (flo:* 4294967296. hi) lo))))
     (flo:max
      flo:smallest-positive-subnormal	;paranoia
      (flo:ldexp (significand-53) (fix:- -53 (exponent))))))

 ;; 8
 (begin
   (define (flo:random-unit-closed state)
     (define (exponent)
       (let loop ((z 0))
	 (if (fix:>= z 1088)
	     z
	     (let ((x (%random-32 state)))
	       (if (fix:= x 0)
		   (loop (fix:+ z 32))
		   (fix:+ z (fix:clz32 x)))))))
     (define (significand-64)
       (let ((lo (int:->flonum (fix:or (%random-32 state) #x00000001)))
	     (hi (int:->flonum (fix:or (%random-32 state) #x80000000))))
	 (flo:+ (flo:* 4294967296. hi) lo)))
     (flo:ldexp (significand-64) (fix:- -64 (exponent))))

   (define (flo:random-unit-open state)
     (define (exponent)
       (let loop ((z 0))
	 (if (fix:>= z 1088)
	     z
	     (let ((x (%random-32 state)))
	       (if (fix:= x 0)
		   (loop (fix:+ z 32))
		   (fix:+ z (fix:clz32 x)))))))
     (define (significand-53)
       (let ((lo (int:->flonum (%random-32 state)))
	     (hi
	      (int:->flonum
	       (fix:or #x100000 (fix:and (%random-32 state) #xfffff)))))
	 (flo:+ (flo:* 4294967296. hi) lo)))
     (flo:max
      flo:smallest-positive-subnormal	;paranoia
      (flo:ldexp (significand-53) (fix:- -53 (exponent))))))
)

;;;; Initialization

(define *random-state* #f)
(define default-random-source)
(define default-random-source-mutex)
(define random-integer)
(define random-real)
(define zero16)
(define chacha-const)

(define (initialize-package!)
  (set! zero16 (make-bytevector 16 0))
  (set! default-random-source-mutex (make-thread-mutex))
  (set! default-random-source (make-random-source))
  (random-source-randomize! default-random-source)
  (set! random-integer (random-source-make-integers default-random-source))
  (set! random-real (random-source-make-reals default-random-source))
  (set! chacha-const (allocate-bytevector 16))
  (let ((c "expand 32-byte k"))
    (do ((i 0 (fix:+ i 1)))
	((fix:>= i 16))
      (bytevector-u8-set! chacha-const i (char->integer (string-ref c i)))))
  unspecific)

(define (finalize-random-state-type!)
  (add-event-receiver! event:after-restart
    (lambda ()
      (random-source-randomize! default-random-source)))
  (named-structure/set-tag-description! random-state-tag
    (new-make-define-structure-type 'vector
				    'random-state
				    '#(key)
				    '#(1)
				    (make-vector 1 (lambda () #f))
				    random-state-tag
				    2)))