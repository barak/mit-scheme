;;;; Pseudo-Random number generator for scheme.
;;; Copyright (C) 1991 Aubrey Jaffer.
;;; From slib1c4, modified for MIT Scheme by cph.
;;; $Id: random.scm,v 14.4 1993/01/13 08:48:34 cph Exp $
;;;
;;;   (random n)					procedure
;;;   (random n state)					procedure
;;; 
;;; Accepts a positive integer or real n and returns a number of the
;;; same type between zero (inclusive) and n (exclusive).  The values
;;; returned have a uniform distribution.
;;;
;;; The optional argument state must be of the type produced by
;;; (make-random-state).  It defaults to the value of the variable
;;; *random-state*.  This object is used to maintain the state of the
;;; pseudo-random-number generator and is altered as a side effect of the
;;; RANDOM operation.
;;;
;;;   *random-state*					variable
;;;
;;; Holds a data structure that encodes the internal state of the
;;; random-number generator that RANDOM uses by default.  The nature of
;;; this data structure is implementation-dependent.  It may be printed
;;; out and successfully read back in, but may or may not function
;;; correctly as a random-number state object in another implementation.
;;;
;;;   (make-random-state)				procedure
;;;   (make-random-state state)				procedure
;;;
;;; Returns a new object of type suitable for use as the value of the
;;; variable *random-state* and as second argument to RANDOM.  If argument
;;; state is given, a copy of it is returned.  Otherwise a copy of
;;; *random-state* is returned.
;;;------------------------------------------------------------------

(define random
  (let ((state-tap-1 24)
	(state-size 55)
	(chunk-size 24)
	(chunk-sup #x1000000))

    (define (get-bits n state)
      (let loop ((n n))
	(let ((p (vector-ref state state-size)))
	  (let ((i (fix:modulo (fix:- p state-tap-1) state-size))
		(chunk (vector-ref state p)))
	    (vector-set! state p (fix:xor (vector-ref state i) chunk))
	    (vector-set! state state-size (fix:modulo (fix:- p 1) state-size))
	    (cond ((fix:= n chunk-size)
		   chunk)
		  ((fix:< n chunk-size)
		   (fix:and chunk (fix:- (fix:lsh 1 n) 1)))
		  (else
		   (+ chunk (* chunk-sup (loop (fix:- n chunk-size))))))))))

    (define (fix:modulo n d)
      ;; Specialized for nonnegative D.
      (let ((r (fix:remainder n d)))
	(if (or (fix:= r 0)
		(not (fix:< n 0)))
	    r
	    (fix:+ r d))))

    (lambda (modulus #!optional state)
      (if (not (and (exact-integer? modulus) (> modulus 0)))
	  (error:wrong-type-argument modulus "exact positive integer" 'RANDOM))
      (let ((state (if (default-object? state) *random-state* state)))
	(if (not (random-state? state))
	    (error:wrong-type-argument state
				       "random number state"
				       'MAKE-RANDOM-STATE))
	(let ((ilen (exact-nonnegative-integer-length modulus))
	      (state (random-state-bits state)))
	  (do ((r (get-bits ilen state)
		  (get-bits ilen state))) ;this could be improved.
	      ((< r modulus) r)))))))

(define-structure (random-state (constructor %make-random-state))
  (bits false read-only true))

(define (make-random-state #!optional state)
  (let ((state (if (default-object? state) *random-state* state)))
    (if (not (random-state? state))
	(error:wrong-type-argument state
				   "random number state"
				   'MAKE-RANDOM-STATE))
    (%make-random-state (vector-copy (random-state-bits state)))))

(define *random-state*
  (%make-random-state
   (vector #xd909ef #xfd330a #xe33f78 #x76783f #xf3675f
	   #xb54ef8 #x0be455 #xa67946 #x0bcd56 #xfabcde
	   #x9cbd3e #x3fd3ef #xe064ef #xdddecc #x344442
	   #x854444 #x4c5192 #xc03662 #x547345 #x70abcd
	   #x1bbdac #x616c5a #xa982ef #x105996 #x5f0ccc
	   #x1ea055 #xfe2acd #x1891c1 #xe66902 #x6912bc
	   #x2678e1 #x612222 #x907abc #x4ad682 #x9cdd14
	   #x577988 #x5b8924 #x871c9c #xd1e67b #x8b0a32
	   #x578ef2 #x28274e #x823ef5 #x845678 #xe67890
	   #x5890ab #x851fa9 #x13efa1 #xb12278 #xdaf805
	   #xa0befc #x0068a7 #xe024fd #xa7b690 #x27f357
	   0)))

(define exact-nonnegative-integer-length
  (let ((powers-of-two
	 (let loop ((n 1))
	   (cons n (delay (loop (* 2 n)))))))
    (lambda (n)
      (let loop ((powers-of-two powers-of-two) (e 0))
	(if (< n (car powers-of-two))
	    e
	    (loop (force (cdr powers-of-two)) (fix:+ e 1)))))))