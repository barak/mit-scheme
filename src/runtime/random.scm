#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;; A "subtract-with-borrow" RNG, based on the algorithm from "A New
;;; Class of Random Number Generators", George Marsaglia and Arif
;;; Zaman, The Annals of Applied Probability, Vol. 1, No. 3, 1991.

;;; The basic algorithm produces a sequence of values x[n] by
;;;      let t = (x[n-s] - x[n-r] - borrow[n-1])
;;;      if (t >= 0)
;;;        then x[n] = t, borrow[n] = 0
;;;        else x[n] = t + b, borrow[n] = 1
;;; where the constants R, S, and B are chosen according to some
;;; constraints that are explained in the article.  Finding
;;; appropriate constants is compute-intensive; the constants used
;;; here are taken from the article and are claimed to represent
;;; "hundreds of hours" of compute time.  The period of this generator
;;; is (- (EXPT B R) (EXPT B S)), which is approximately (EXPT 10 414).

;;;; Core algorithm

(define-integrable r 43)
(define-integrable s 22)
(define-integrable b 4294967291 #|(- (expt 2 32) 5)|#)
(define-integrable b. 4294967291. #|(exact->inexact b)|#)

(define (flo:random-element state)
  (let ((mask ((ucode-primitive set-interrupt-enables!) interrupt-mask/gc-ok)))
    (let ((index (random-state-index state))
	  (vector (random-state-vector state)))
      (let ((element (flo:vector-ref vector index)))
	(let ((difference
	       (flo:- (flo:- (flo:vector-ref vector
					     (if (fix:< (fix:- index s) 0)
						 (fix:+ (fix:- index s) r)
						 (fix:- index s)))
			     element)
		      (random-state-borrow state))))
	  (if (flo:< difference 0.)
	      (begin
		(flo:vector-set! vector index (flo:+ difference b.))
		(set-random-state-borrow! state 1.))
	      (begin
		(flo:vector-set! vector index difference)
		(set-random-state-borrow! state 0.)))
	  (set-random-state-index! state
				   (if (fix:= (fix:+ index 1) r)
				       0
				       (fix:+ index 1))))
	((ucode-primitive set-interrupt-enables!) mask)
	element))))

(define-integrable (int:random-element state)
  (flo:truncate->exact (flo:random-element state)))

;;;; Integer scaling

(define (%random-integer m state)
  (if (int:> m b)
      (large-random-integer m state)
      (small-random-integer m state)))

(define (small-random-integer m state)
  ;; This uses the rejection method to select a uniformly-distributed
  ;; subset of the generated elements.  The trick with SCALE-FACTOR
  ;; limits the number of samples required to find an element in the
  ;; subset, which is otherwise on the order of B for small M.
  (let ((m. (int:->flonum m)))
    (let ((scale-factor (flo:truncate (flo:/ b. m.))))
      (flo:truncate->exact
       (flo:/ (let ((limit (flo:* scale-factor m.)))
		(let loop ()
		  (let ((elt (flo:random-element state)))
		    (if (flo:< elt limit)
			elt
			(loop)))))
	      scale-factor)))))

(define (large-random-integer m state)
  ;; This also uses the rejection method, but this time to select a
  ;; subset of B^N where N is the smallest integer s.t. (<= M B^N).
  (receive (n b^n)
      (let loop ((n 2) (b^n (int:* b b)))
	(if (int:<= m b^n)
	    (values n b^n)
	    (loop (fix:+ n 1) (int:* b^n b))))
    (let ((scale-factor (int:quotient b^n m)))
      (int:quotient (let ((limit (int:* scale-factor m)))
		      (let loop ()
			(let ((elt (int:large-random-element state n)))
			  (if (int:< elt limit)
			      elt
			      (loop)))))
		    scale-factor))))

(define (int:large-random-element state n)
  (let loop ((i 1) (elt (int:random-element state)))
    (if (fix:< i n)
	(loop (fix:+ i 1)
	      (int:+ (int:* elt b) (int:random-element state)))
	elt)))

;;;; Operations producing random values

(define (random modulus #!optional state)
  (let ((state (guarantee-random-state state 'RANDOM)))
    ;; Kludge: an exact integer modulus means that result is an exact
    ;; integer.  Otherwise, the result is a real number.
    (cond ((int:integer? modulus)
	   (if (int:> modulus 0)
	       (%random-integer modulus state)
	       (error:bad-range-argument modulus 'RANDOM)))
	  ((flo:flonum? modulus)
	   (if (flo:> modulus 0.)
	       (flo:* (flo:random-unit state) modulus)
	       (error:bad-range-argument modulus 'RANDOM)))
	  ((real? modulus)
	   ;; I can't think of the correct thing to do here.  The old
	   ;; code scaled a random element into the appropriate range,
	   ;; which gave one of B evenly-distributed values.  But this
	   ;; is arbitrary and not necessarily what the caller wants.
	   ;; If you have an idea what should happen here, let me
	   ;; know.  -- cph
	   (error "Unsupported modulus:" modulus))
	  (else
	   (error:wrong-type-argument modulus "real number" 'RANDOM)))))

(define (flo:random-unit state)
  ;; Guarantee that (< 0 returned-value 1)
  (flo:/ (flo:+ (int:->flonum (%random-integer (int:- flimit 1) state))
		1.)
	 flimit.))

(define (random-byte-vector n #!optional state)
  (let ((state (guarantee-random-state state 'RANDOM-BYTE-VECTOR))
	(s (make-string n)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (vector-8b-set! s i (small-random-integer 256 state)))
    s))

(define (random-source-make-integers source)
  (guarantee-random-state source 'RANDOM-SOURCE-MAKE-INTEGERS)
  (lambda (modulus)
    (if (int:> modulus 0)
	(%random-integer modulus source)
	(error:bad-range-argument modulus #f))))

(define (random-source-make-reals source #!optional unit)
  (guarantee-random-state source 'RANDOM-SOURCE-MAKE-REALS)
  (let ((unit
	 (if (default-object? unit)
	     .5
	     (begin
	       (if (not (and (real? unit) (< 0 unit 1)))
		   (error:wrong-type-argument unit
					      "real unit"
					      'RANDOM-SOURCE-MAKE-REALS))
	       unit))))
    (if (flo:flonum? unit)
	;; Ignore UNIT and return maximum precision.
	(lambda () (flo:random-unit source))
	;; Limit the maximum size of UNIT to avoid problems.
	(let ((m (- (truncate (/ 1 (min 1/65536 unit))) 1)))
	  (lambda ()
	    (* unit (%random-integer m source)))))))

;;;; Operations on state

(define (make-random-state #!optional state)
  (if (or (eq? #t state) (int:integer? state))
      ;; Use good random source if available
      (if (file-readable? "/dev/urandom")
	  (call-with-input-file "/dev/urandom"
	    (lambda (port)
	      (initial-random-state
	       (lambda (b)
		 (let outer ()
		   (let inner
		       ((m #x100)
			(n (char->integer (read-char port))))
		     (cond ((< m b)
			    (inner (* m #x100)
				   (+ (* n #x100)
				      (char->integer (read-char port)))))
			   ((< n b) n)
			   (else (outer)))))))))
	  (simple-random-state))
      (copy-random-state
       (guarantee-random-state state 'MAKE-RANDOM-STATE))))

(define (simple-random-state)
  (initial-random-state
   (congruential-rng
    (int:+ ((ucode-primitive real-time-clock 0))
	   (int:* 100000 ((ucode-primitive system-clock 0)))))))

(define (make-random-source)
  (initial-random-state (congruential-rng 0)))

(define (random-source-state-set! source v)
  (copy-random-state! (import-random-state v) source))

(define (random-source-randomize! source)
  (copy-random-state! (make-random-state #t) source))

(define (random-source-pseudo-randomize! source i j)
  source i j
  (error "Unimplemented procedure:" 'RANDOM-SOURCE-PSEUDO-RANDOMIZE!))

(define (initial-random-state generate-random-seed)
  ;; The numbers returned by GENERATE-RANDOM-SEED are not critical.
  ;; Except for the explicitly disallowed sequences, all other
  ;; sequences produce reasonable results, although some sequences
  ;; might require a small number of initial generation steps to get
  ;; them into the main cycle.  (See the article for details.)
  (let ((seeds (flo:vector-cons r)))
    (let fill ()
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i r))
	(flo:vector-set! seeds i (int:->flonum (generate-random-seed b))))
      ;; Disallow cases with all seeds either 0 or b-1, since they can
      ;; get locked in trivial cycles.
      (if (or (let loop ((i 0))
		(or (fix:= i r)
		    (and (flo:= (flo:vector-ref seeds i) 0.)
			 (loop (fix:+ i 1)))))
	      (let ((b-1 (flo:- b. 1.)))
		(let loop ((i 0))
		  (or (fix:= i r)
		      (and (flo:= (flo:vector-ref seeds i) b-1)
			   (loop (fix:+ i 1)))))))
	  (fill)))
    (%make-random-state 0 0. seeds)))

(define (congruential-rng seed)
  (let ((a 16807 #|(expt 7 5)|#)
	(m 2147483647 #|(- (expt 2 31) 1)|#))
    (let ((m-1 (int:- m 1)))
      (let ((seed (int:+ (int:remainder seed m-1) 1)))
	(lambda (b)
	  (let ((n (int:remainder (int:* a seed) m)))
	    (set! seed n)
	    (int:quotient (int:* (int:- n 1) b) m-1)))))))

;;;; External representation of state

(define-integrable ers:tag 'RANDOM-STATE-V1)
(define-integrable ers:length (fix:+ r 3))

(define (export-random-state state)
  (if (not (random-state? state))
      (error:wrong-type-argument state "random state" 'EXPORT-RANDOM-STATE))
  (let ((v (make-vector ers:length)))
    (vector-set! v 0 ers:tag)
    (vector-set! v 1 (random-state-index state))
    (vector-set! v 2 (inexact->exact (random-state-borrow state)))
    (let ((v* (random-state-vector state)))
      (do ((i 0 (fix:+ i 1))
	   (j 3 (fix:+ j 1)))
	  ((fix:= i r))
	(vector-set! v j (inexact->exact (flo:vector-ref v* i)))))
    v))

(define (import-random-state v)
  (let ((lose
	 (lambda ()
	   (error:wrong-type-argument v
				      "external random state"
				      'IMPORT-RANDOM-STATE))))
    (if (not (and (vector? v)
		  (fix:= (vector-length v) ers:length)
		  (eq? (vector-ref v 0) ers:tag)))
	(lose))
    (let ((index (vector-ref v 1))
	  (borrow (vector-ref v 2))
	  (v* (flo:vector-cons r)))
      (if (not (and (index-fixnum? index)
		    (fix:< index r)
		    (index-fixnum? borrow)
		    (fix:< borrow 2)))
	  (lose))
      (do ((i 3 (fix:+ i 1))
	   (j 0 (fix:+ j 1)))
	  ((fix:= j r))
	(let ((n (vector-ref v i)))
	  (if (not (and (exact-nonnegative-integer? n)
			(< n b)))
	      (lose))
	  (flo:vector-set! v* j (exact->inexact n))))
      (%make-random-state index (exact->inexact borrow) v*))))

;;;; State abstraction

;;; The RANDOM-STATE data abstraction must be built by hand because
;;; the random-number generator is needed in order to build the record
;;; abstraction.

(define-integrable (%make-random-state i b v)
  (vector random-state-tag i b v))

(define (random-state? object)
  (and (vector? object)
       (fix:= (vector-length object) 4)
       (eq? (vector-ref object 0) random-state-tag)))

(define-integrable random-state-tag
  '|#[(runtime random-number)random-state]|)

(define-integrable (random-state-index s) (vector-ref s 1))
(define-integrable (set-random-state-index! s x) (vector-set! s 1 x))

(define-integrable (random-state-borrow s) (vector-ref s 2))
(define-integrable (set-random-state-borrow! s x) (vector-set! s 2 x))

(define-integrable (random-state-vector s) (vector-ref s 3))

(define (copy-random-state state)
  (%make-random-state (random-state-index state)
		      (random-state-borrow state)
		      (flo:vector-copy (random-state-vector state))))

(define (flo:vector-copy vector)
  (let ((n (flo:vector-length vector)))
    (let ((result (flo:vector-cons n)))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(flo:vector-set! result i (flo:vector-ref vector i)))
      result)))

(define (copy-random-state! source target)
  (without-interrupts
   (lambda ()
     (set-random-state-index! target (random-state-index source))
     (set-random-state-borrow! target (random-state-borrow source))
     (let ((vs (random-state-vector source))
	   (vt (random-state-vector target)))
       (do ((i 0 (fix:+ i 1)))
	   ((fix:= i r))
	 (flo:vector-set! vt i (flo:vector-ref vs i)))))))

(define (guarantee-random-state state procedure)
  (if (if (default-object? state) #f state)
      (begin
	(if (not (random-state? state))
	    (error:wrong-type-argument state "random state" procedure))
	state)
      (let ((state *random-state*))
	(if (not (random-state? state))
	    (error "Invalid *random-state*:" state))
	state)))

;;;; Initialization

(define *random-state*)
(define flimit.)
(define flimit)
(define default-random-source)
(define random-integer)
(define random-real)

(define (initialize-package!)
  (set! *random-state* (simple-random-state))
  (set! flimit.
	(let loop ((x 1.))
	  (if (flo:= (flo:+ x 1.) 1.)
	      (flo:/ 1. x)
	      (loop (flo:/ x 2.)))))
  (set! flimit (flo:truncate->exact flimit.))
  (set! default-random-source *random-state*)
  (set! random-integer (random-source-make-integers default-random-source))
  (set! random-real (random-source-make-reals default-random-source))
  unspecific)

(define (finalize-random-state-type!)
  (add-event-receiver! event:after-restart
    (lambda ()
      (random-source-randomize! *random-state*)
      (if (not (eq? default-random-source *random-state*))
	  (random-source-randomize! default-random-source))))
  (named-structure/set-tag-description! random-state-tag
    (make-define-structure-type 'VECTOR
				'RANDOM-STATE
				'#(INDEX BORROW VECTOR)
				'#(1 2 3)
				(make-vector 3 (lambda () #f))
				(standard-unparser-method 'RANDOM-STATE #f)
				random-state-tag
				4)))