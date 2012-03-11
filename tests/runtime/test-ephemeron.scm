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

;;;; Test of ephemerons

(declare (usual-integrations))

;;;; Utilities

(define (assert-unbroken ephemeron key datum)
  (assert-equal (ephemeron-key ephemeron) key)
  (assert-equal (ephemeron-datum ephemeron) datum)
  (assert-false (ephemeron-broken? ephemeron)))

(define (assert-broken ephemeron)
  (assert-eqv (ephemeron-key ephemeron) #f)
  (assert-eqv (ephemeron-datum ephemeron) #f)
  (assert-true (ephemeron-broken? ephemeron)))

(define (repeat procedure)
  (gc-flip)
  (procedure)
  (gc-flip)
  (procedure)
  (gc-flip)
  (procedure))

(define (finally procedure)
  (gc-flip)
  (procedure))

(define-test 'NO-GC
  ;; Not really no GC; it's just that the ephemeron is never broken
  ;; because its key is never GC'd.
  (lambda ()
    (let ((key (list 'KEY)) (datum (list 'DATUM)))
      (let ((ephemeron (make-ephemeron key datum)))
	(define (check) (assert-unbroken ephemeron '(KEY) '(DATUM)))
	(repeat check)
	(finally check)
	(reference-barrier (list key datum))))))

(define-test 'GC-KEY
  (lambda ()
    (let ((key (list 'KEY)) (datum (list 'DATUM)))
      (let ((ephemeron (make-ephemeron key datum)))
	(repeat (lambda () (assert-unbroken ephemeron '(KEY) '(DATUM))))
	(reference-barrier key)
	(set! key 0)
	(finally (lambda () (assert-broken ephemeron)))
	(reference-barrier datum)))))

(define-test 'GC-DATUM
  (lambda ()
    (let ((key (list 'KEY)) (datum (list 'DATUM)))
      (let ((ephemeron (make-ephemeron key datum)))
	(repeat (lambda () (assert-unbroken ephemeron '(KEY) '(DATUM))))
	(reference-barrier datum)
	(set! datum 0)
	(finally (lambda () (assert-unbroken ephemeron '(KEY) '(DATUM))))
	(reference-barrier key)))))

(define-test 'GC-KEY-AND-DATUM
  (lambda ()
    (let ((key (list 'KEY)) (datum (list 'DATUM)))
      (let ((ephemeron (make-ephemeron key datum)))
	(repeat (lambda () (assert-unbroken ephemeron '(KEY) '(DATUM))))
	(reference-barrier (list key datum))
	(set! key 0)
	(set! datum 0)
	(finally (lambda () (assert-broken ephemeron)))))))

(define-test 'EPHEMERON-AND-WEAK-PAIR
  (lambda ()
    (let ((key (list 'KEY)) (datum (list 'DATUM)))
      (let ((ephemeron (make-ephemeron key datum))
	    (weak-pair (weak-cons datum 0)))
	(define (check)
	  (assert-unbroken ephemeron '(KEY) '(DATUM))
	  (assert-equal (weak-car weak-pair) '(DATUM))
	  (assert-eqv (weak-car weak-pair) (ephemeron-datum ephemeron)))
	(repeat check)
	(reference-barrier datum)
	(set! datum 0)
	(repeat check)
	(reference-barrier key)
	(set! key 0)
	(finally (lambda ()
		   (assert-broken ephemeron)
		   (assert-false (weak-pair/car? weak-pair))))))))

(define-test 'MANY-EPHEMERONS
  (lambda ()
    (let ((n 100))
      (let* ((frobs (make-initialized-vector n (lambda (i) (cons i i))))
	     (ephemerons
	      (make-initialized-vector n
		(lambda (i) (make-ephemeron (vector-ref frobs i) i)))))
	(define (frob i) (vector-ref frobs i))
	(define (ephemeron i) (vector-ref ephemerons i))
	(define (unbroken i) (assert-unbroken (ephemeron i) (frob i) i))
	(define (broken i) (assert-broken (ephemeron i)))
	(repeat (lambda () (do ((i 0 (+ i 1))) ((= i n)) (unbroken i))))
	(do ((i 0 (+ i 2))) ((>= i n)) (vector-set! frobs i #f))
	(finally (lambda ()
		   (do ((i 0 (+ i 1))) ((= i n))
		     (if (even? i) (broken i) (unbroken i)))))))))

(define-test 'SIMPLE-EPHEMERON-CYCLE
  (lambda ()
    (let ((p (list 'P)) (q (list 'Q)))
      (let ((a (make-ephemeron p q))
	    (b (make-ephemeron q p)))
	(define (check)
	  (assert-unbroken a '(P) '(Q))
	  (assert-unbroken b '(Q) '(P)))
	(repeat check)
	(reference-barrier p)
	(set! p 0)
	(repeat check)
	(reference-barrier q)
	(set! q 0)
	(finally (lambda () (assert-broken a) (assert-broken b)))))))

(define (random-cyclic-permutation n)
  (let ((permutation (make-initialized-vector n identity-procedure)))
    ;; Does this give a uniform distribution?
    (let loop ((i n))
      (if (< 1 i)
	  (let ((i* (- i 1)))
	    (vector-exchange! permutation i* (random-integer i*))
	    (loop i*))))
    permutation))

(define (cyclic-permutation? object)
  (and (vector? object)
       (let loop ((i 0))
	 (or (>= i (vector-length object))
	     (and (let ((vi (vector-ref object i)))
		    (and (integer? vi)
			 (exact? vi)
			 (<= 0 vi)
			 (< vi (vector-length object))
			 (not (= vi (vector-ref object vi)))))
		  (loop (+ i 1)))))))

(define (vector-exchange! v i j)
  (let ((t (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j t)))

(define-test 'RANDOM-EPHEMERON-CYCLES
  (lambda ()
    (let ((n 10))
      (do ((i 0 (+ i 1))) ((= i n))
	(let ((permutation (random-cyclic-permutation n))
	      (frobs (make-initialized-vector n list)))
	  (define (permute i) (vector-ref permutation i))
	  (define (frob i) (vector-ref frobs i))
	  (let ((ephemerons
		 (make-initialized-vector n
		   (lambda (i) (make-ephemeron (frob i) (frob (permute i)))))))
	    (define (ephemeron i) (vector-ref ephemerons i))
	    (define (check)
	      (do ((i 0 (+ i 1))) ((= i n))
		(assert-unbroken (ephemeron i) (list i) (list (permute i)))))
	    (repeat check)
	    (do ((i 1 (+ i 1))) ((= i n)) (vector-set! frobs (permute i) #f))
	    (repeat check)
	    (reference-barrier frobs)
	    (set! frobs 0)
	    (finally (lambda ()
		       (do ((i 0 (+ i 1))) ((= i n))
			 (assert-broken (ephemeron i)))))))))))

(define-test 'TWO-RANDOM-EPHEMERON-CYCLES
  (lambda ()
    (let* ((n 10) (n/2 (quotient n 2)))
      (do ((i 0 (+ i 1))) ((= i n))
	(let ((permutation-a (random-cyclic-permutation n/2))
	      (permutation-b (random-cyclic-permutation (- n n/2)))
	      (frobs (make-initialized-vector n list)))
	  (define (permute i)
	    (if (< i n/2)
		(vector-ref permutation-a i)
		(+ n/2 (vector-ref permutation-b (- i n/2)))))
	  (define (frob i) (vector-ref frobs i))
	  (let ((ephemerons
		 (make-initialized-vector n
		   (lambda (i) (make-ephemeron (frob i) (frob (permute i)))))))
	    (define (ephemeron i) (vector-ref ephemerons i))
	    (define (unbroken start end)
	      (do ((i start (+ i 1))) ((= i end))
		(assert-unbroken (ephemeron i) (list i) (list (permute i)))))
	    (define (unbroken-a) (unbroken 0 n/2))
	    (define (unbroken-b) (unbroken n/2 n))
	    (define (broken start end)
	      (do ((i start (+ i 1))) ((= i end))
		(assert-broken (ephemeron i))))
	    (define (broken-a) (broken 0 n/2))
	    (define (broken-b) (broken n/2 n))
	    (repeat (lambda () (unbroken-a) (unbroken-b)))
	    (reference-barrier frobs)
	    (do ((i 1 (+ i 1))) ((= i n/2)) (vector-set! frobs (permute i) #f))
	    (repeat (lambda () (unbroken-a) (unbroken-b)))
	    (reference-barrier frobs)
	    (do ((i (+ n/2 1) (+ i 1))) ((= i n))
	      (vector-set! frobs (permute i) #f))
	    (repeat (lambda () (unbroken-a) (unbroken-b)))
	    (reference-barrier frobs)
	    (vector-set! frobs (permute 0) #f)
	    (repeat (lambda () (broken-a) (unbroken-b)))
	    (reference-barrier frobs)
	    (vector-set! frobs (permute n/2) #f)
	    (repeat (lambda () (broken-a) (broken-b)))))))))

(define-test 'FORCE-EPHEMERON-QUEUE
  ;; This test forces the garbage-collector to discover an ephemeron
  ;; during the ephemeron-scanning phase, whose key it can't prove live
  ;; upon discovery.  Assumes the garbage-collector processes earlier
  ;; elements in vectors before later ones.
  (lambda ()
    (let ((p (list 'P)) (q (list 'Q)) (r (list 'R)))
      (let ((e (make-ephemeron p (vector (make-ephemeron q r) (list q))))
	    (wp (weak-cons r '())))
	(define (check)
	  (assert-equal '(R) (weak-car wp))
	  (assert-equal '(P) (ephemeron-key e))
	  (let ((datum (ephemeron-datum e)))
	    (define (v i) (vector-ref datum i))
	    (assert-true (vector? datum))
	    (assert-true (ephemeron? (v 0)))
	    (assert-unbroken (v 0) '(Q) '(R))
	    (assert-eqv (ephemeron-datum (v 0)) (weak-car wp))
	    (assert-equal (v 1) '((Q))))
	  (assert-true (weak-pair/car? wp))
	  (assert-false (ephemeron-broken? e)))
	(repeat check)
	(reference-barrier r)
	(set! r 0)
	(repeat check)
	(reference-barrier q)
	(set! q 0)
	(repeat check)
	(reference-barrier p)
	(set! p 0)
	(finally (lambda ()
		   (assert-broken e)
		   (assert-false (weak-pair/car? wp))))))))

(define-test 'SET-EPHEMERON-DATUM-WITHOUT-GC
  (lambda ()
    (let ((p (list 'P)) (q (list 'Q)) (r (list 'R)))
      (let ((e (make-ephemeron p q)))
	(repeat (lambda () (assert-unbroken e '(P) '(Q))))
	(set-ephemeron-datum! e r)
	(finally (lambda () (assert-unbroken e '(P) '(R))))
	(reference-barrier (list p q r))))))

(define-test 'SET-EPHEMERON-DATUM-BEFORE-GC
  (lambda ()
    (let ((p (list 'P)) (q (list 'Q)) (r (list 'R)))
      (let ((e (make-ephemeron p q)))
	(repeat (lambda () (assert-unbroken e '(P) '(Q))))
	(set-ephemeron-datum! e r)
	(repeat (lambda () (assert-unbroken e '(P) '(R))))
	(reference-barrier p)
	(set! p 0)
	(finally (lambda () (assert-broken e)))))))

(define-test 'SET-EPHEMERON-DATUM-AFTER-GC
  (lambda ()
    (let ((p (list 'P)) (q (list 'Q)) (r (list 'R)))
      (let ((e (make-ephemeron p q)))
	(repeat (lambda () (assert-unbroken e '(P) '(Q))))
	(set! p 0)
	(reference-barrier p)
	(repeat (lambda () (assert-broken e)))
	(set-ephemeron-datum! e r)
	(assert-equal (ephemeron-datum e) #f)
	(finally (lambda () (assert-broken e)))))))

#|
;;; Cute idea, but doesn't work very well -- the timings are too
;;; imprecise and noisy.

(define (check-time-complexity ephemerons key datum)
  (define (initialize i)
    (vector-set! ephemerons i (make-ephemeron (key i) (datum i))))
  (define (measure-time i)
    ;; Don't let other threads interfere with our timing by consing.
    (with-thread-timer-stopped
      (lambda ()
	(gc-flip)
	;; It's tempting to time the initialization too, but
	;; MAKE-EPHEMERON runs in constant amortized time, not constant
	;; worst-case time, so timing individual runs does no good.
	(initialize i)
	(gc-flip)
	(let ((start-time (real-time-clock)))
	  (gc-flip)
	  (- (real-time-clock) start-time)))))
  (let loop ((i 0) (times '()))
    (if (< i (vector-length ephemerons))
	(loop (+ i 1)
	      (if (zero? (modulo i 100))
		  (cons (measure-time i) times)
		  (begin (initialize i) times)))
	(begin
	  ;; (assert-false (fits-to-parabola? times))
	  (assert-true (fits-to-line? times))))))

(define (fits-to-line? times)
  (define (sum data) (reduce + 0 data))
  (define (dot u v) (sum (map * u v)))
  (define (distance^2 a b) (square (- a b)))
  (define (mean data) (/ (sum data) (length data)))
  (define (normalize data)
    (let ((mean (mean data)))
      (map (lambda (datum) (- datum mean)) data)))
  (let ((n (length times)))
    (let ((times (normalize times))
	  (indices (normalize (iota n))))
      (let ((slope (/ (dot indices times) (dot indices indices))))
	(let ((times* (map (lambda (i) (* slope i)) indices)))
	  (>= 1 (mean (map distance^2 times times*))))))))

(define-test 'LINEAR-TIME-COMPLEXITY-WITHOUT-REFERENCES
  (lambda ()
    (let* ((n 10000) (ephemerons (make-vector n #f)))
      (check-time-complexity ephemerons (lambda (i) i 0) (lambda (i) i 0)))))

(define-test 'LINEAR-TIME-COMPLEXITY-WITH-KEYS
  (lambda ()
    (let ((n 10000))
      (let ((cells (make-initialized-vector n make-cell))
	    (ephemerons (make-vector n #f)))
	(check-time-complexity ephemerons
          (lambda (i) (vector-ref cells i))
	  (lambda (i) i 0))))))

(define-test 'LINEAR-TIME-COMPLEXITY-WITH-SOME-KEYS
  (lambda ()
    (let ((n 10000))
      (define (make-even-cell i) (and (even? i) (make-cell i)))
      (let ((cells (make-initialized-vector n make-even-cell))
	    (ephemerons (make-vector n #f)))
	(check-time-complexity ephemerons
          (lambda (i) (vector-ref cells i))
	  (lambda (i) i 0))))))

(define-test 'LINEAR-TIME-COMPLEXITY-WITH-EPHEMERON-KEYS
  (lambda ()
    (let* ((n 10000) (ephemerons (make-vector n #f)))
      (check-time-complexity ephemerons
        (lambda (i) (if (zero? i) 0 (vector-ref ephemerons (- i 1))))
	(lambda (i) i 0)))))
|#

(define-test 'FASL-PRESERVES-EPHEMERONS
  (lambda ()
    (call-with-temporary-file-pathname
      (lambda (pathname)
	(let* ((pair (cons 0 0))
	       (ephemeron (make-ephemeron pair 0)))
	  (fasdump (vector ephemeron pair) pathname))
	(let ((object (fasload pathname)))
	  (assert-true (vector? object))
	  (assert-eqv (vector-length object) 2)
	  (let ((ephemeron (vector-ref object 0))
		(pair (vector-ref object 1)))
	    (assert-equal pair '(0 . 0))
	    (assert-true (ephemeron? ephemeron))
	    (assert-unbroken ephemeron pair 0)
	    (assert-eqv (ephemeron-key ephemeron) pair)))))))

;;; Commented out because the fasdumper does not, in fact, break
;;; ephemerons whose keys are not strongly referenced in the fasl.

#|
(define-test 'FASL-BREAKS-EPHEMERONS
  (lambda ()
    (call-with-temporary-file-pathname
      (lambda (pathname)
	(fasdump (make-ephemeron (cons 0 0) 0) pathname)
	(let ((ephemeron (fasload pathname)))
	  (assert-true (ephemeron? ephemeron))
	  (assert-broken ephemeron))))))
|#

;;; Assumption: CONSTANT-PROCEDURE yields compiled closures.

(define-test 'COMPILED-KEY
  (lambda ()
    (let ((key (constant-procedure 0)) (datum (list 'DATUM)))
      (let ((e (make-ephemeron key datum)))
	(repeat (lambda ()
		  (assert-true (procedure? (ephemeron-key e)))
		  (assert-eqv ((ephemeron-key e)) 0)
		  (assert-equal (ephemeron-datum e) '(DATUM))
		  (assert-false (ephemeron-broken? e))))
	(reference-barrier key)
	(set! key 0)
	(finally (lambda () (assert-broken e)))))))

(define-test 'COMPILED-DATUM
  (lambda ()
    (let ((key (list 'KEY)) (datum (constant-procedure 0)))
      (let ((e (make-ephemeron key datum)))
	(repeat (lambda ()
		  (assert-equal (ephemeron-key e) '(KEY))
		  (assert-true (procedure? (ephemeron-datum e)))
		  (assert-eqv ((ephemeron-datum e)) 0)
		  (assert-false (ephemeron-broken? e))))
	(reference-barrier key)
	(set! key 0)
	(finally (lambda () (assert-broken e)))))))

(define-test 'COMPILED-KEY&DATUM
  (lambda ()
    (let ((key (constant-procedure 0)) (datum (constant-procedure 1)))
      (let ((e (make-ephemeron key datum)))
	(repeat (lambda ()
		  (assert-true (procedure? (ephemeron-key e)))
		  (assert-true (procedure? (ephemeron-datum e)))
		  (assert-eqv ((ephemeron-key e)) 0)
		  (assert-eqv ((ephemeron-datum e)) 1)
		  (assert-false (ephemeron-broken? e))))
	(reference-barrier key)
	(set! key 0)
	(finally (lambda () (assert-broken e)))))))
