#| -*-Scheme-*-

$Id: random.scm,v 14.27 2003/03/11 05:00:48 cph Exp $

Copyright 1988,1989,1993,1994,1995,1996 Massachusetts Institute of Technology
Copyright 1998,1999,2000,2001,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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

(define-integrable r 43)
(define-integrable s 22)
(define-integrable b 4294967291 #|(- (expt 2 32) 5)|#)
(define-integrable b. 4294967291. #|(exact->inexact b)|#)

(define (random modulus #!optional state)
  (let ((element
	 (flo:random-unit
	  (guarantee-random-state (if (default-object? state) #f state)
				  'RANDOM))))
    ;; Kludge: an exact integer modulus means that result is an exact
    ;; integer.  Otherwise, the result is a real number.
    (cond ((and (flo:flonum? modulus) (flo:< 0. modulus))
	   (flo:* element modulus))
	  ((and (int:integer? modulus) (int:< 0 modulus))
	   (flo:truncate->exact (flo:* element (int:->flonum modulus))))
	  ((and (real? modulus) (< 0 modulus))
	   (* (inexact->exact element) modulus))
	  (else
	   (error:wrong-type-argument modulus "positive real" 'RANDOM)))))

(define (flo:random-unit state)
  (let ((mask (set-interrupt-enables! interrupt-mask/gc-ok)))
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
	(set-interrupt-enables! mask)
	(flo:/ element b.)))))

(define (random-byte-vector n #!optional state)
  (let ((state (if (default-object? state) #f state))
	(s (make-string n)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n))
      (vector-8b-set! s i (random 256 state)))
    s))

(define (make-random-state #!optional state)
  (let ((state (if (default-object? state) #f state)))
    (if (or (eq? #t state) (int:integer? state))
	;; Use good random source if available
	(if (file-exists? "/dev/urandom")
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
	 (guarantee-random-state state 'MAKE-RANDOM-STATE)))))

(define (simple-random-state)
  (initial-random-state
   (congruential-rng (+ ((ucode-primitive real-time-clock)) 123456789))))

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
    (let ((m-1 (- m 1)))
      (let ((seed (+ (int:remainder seed m-1) 1)))
	(lambda (b)
	  (let ((n (int:remainder (* a seed) m)))
	    (set! seed n)
	    (int:quotient (* (- n 1) b) m-1)))))))

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
  ((ucode-primitive string->symbol) "#[(runtime random-number)random-state]"))

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

(define (guarantee-random-state state procedure)
  (if state
      (begin
	(if (not (random-state? state))
	    (error:wrong-type-argument state "random state" procedure))
	state)
      (let ((state *random-state*))
	(if (not (random-state? state))
	    (error "Invalid *random-state*:" state))
	state)))

(define *random-state*)

(define (initialize-package!)
  (set! *random-state* (simple-random-state))
  unspecific)

(define (finalize-random-state-type!)
  (add-event-receiver! event:after-restore
		       (lambda ()
			 (set! *random-state* (make-random-state #t))
			 unspecific))
  (named-structure/set-tag-description! random-state-tag
    (make-define-structure-type 'VECTOR
				'RANDOM-STATE
				'(INDEX BORROW VECTOR)
				'(1 2 3)
				#f
				(standard-unparser-method 'RANDOM-STATE #f))))