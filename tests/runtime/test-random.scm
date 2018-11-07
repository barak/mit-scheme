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

;;;; Tests of random number generator

(declare (usual-integrations))

(define-test 'random-state
  (lambda ()
    (assert-true (random-state? (make-random-state)))))

(define-test 'random-source
  (lambda ()
    (assert-true (random-source? (make-random-source)))))

(define-test 'random-state-fresh
  (lambda ()
    (let* ((s0 (make-random-state #t))
	   (s1 (make-random-state #t)))
      (assert-!= ((random-source-make-integers s0) (expt 2 32))
		 ((random-source-make-integers s1) (expt 2 32))))))

(define-test 'random-source-randomize!
  (lambda ()
    (let* ((s0 (make-random-state #t))
	   (s1 (make-random-state s0)))
      (random-source-randomize! s0)
      (assert-!= ((random-source-make-integers s0) (expt 2 32))
		 ((random-source-make-integers s1) (expt 2 32))))))

(define (define-random-test name procedure)
  (define-test name
    (lambda ()
      ;; XXX Should make the seed more compact than just the original
      ;; complete state.
      (let ((state (make-random-state)))
	(with-test-properties
	    (lambda ()
	      ;; Ensure we don't accidentally use the global state.
	      (fluid-let ((*random-state* 'loser-state)
			  (default-random-source 'loser-source))
		(procedure state)))
	  'seed (export-random-state state))))))

(define-random-test 'random-state-derived
  (lambda (state)
    (let* ((s0 (make-random-state state))
	   (s1 (make-random-state s0)))
      (assert-true (random-state? s0))
      (assert-true (random-state? s1))
      (assert-= ((random-source-make-integers s0) (expt 2 64))
		((random-source-make-integers s1) (expt 2 64))))))

(define-random-test 'random-source-ref/set!
  (lambda (state)
    (let* ((s0 state)
	   (s1 (make-random-state #t)))
      (random-source-state-set! s1 (random-source-state-ref s0))
      (assert-= ((random-source-make-integers s0) (expt 2 64))
		((random-source-make-integers s1) (expt 2 64))))))

(define-random-test 'import/export
  (lambda (state)
    (let* ((s0 (make-random-state state))
	   (s1 (import-random-state (export-random-state s0))))
      (assert-= ((random-source-make-integers s0) (expt 2 64))
		((random-source-make-integers s1) (expt 2 64))))))

(define-random-test 'random/integer
  (lambda (state)
    (let ((ok (make-vector 3)))
      (do ((i 0 (+ i 1)))
	  ((>= i 300))
	(vector-set! ok (random 3 state) #t))
      (do ((i 0 (+ i 1)))
	  ((>= i 3))
	(assert-true (vector-ref ok i))))))

(define-random-test 'random/float
  (lambda (state)
    (do ((i 0 (+ i 1)))
	((>= i 64))
      (let ((x (random 0.25 state)))
	((predicate-assertion flo:flonum? "flonum") x)
	(assert-true (<= 0 x 0.25))))))

(define-random-test 'random/rational
  (lambda (state)
    (assert-error (lambda () (random 1/4 state)))))

;;; Stochastic tests

(define NSAMPLES 100000)
(define NTRIALS 2)
(define NPASSES-MIN 1)

(define PSI-DF 100)			;degrees of freedom
(define PSI-CRITICAL 135.807)		;critical value, alpha = .01

(define (psi-test counts logps n)
  (let loop ((i 0) (psi 0.) (c 0.))
    (if (< i PSI-DF)
	(let ((count (vector-ref counts i))
	      (logp (vector-ref logps i)))
	  (if (= count 0)
	      (loop (+ i 1) psi c)
	      (let* ((t (* count (- (log (/ count n)) logp)))
		     (t* (- t c))
		     (psi* (+ psi t*))
		     (c* (- (- psi* psi) t*)))
		(loop (+ i 1) psi* c*))))
	(<= (* 2 psi) PSI-CRITICAL))))

(define (count-sample nsamples procedure)
  (let ((counts (make-vector PSI-DF 0)))
    (do ((i 0 (+ i 1)))
	((>= i nsamples))
      (let ((bin (procedure)))
	(vector-set! counts bin (+ 1 (vector-ref counts bin)))))
    counts))

(define (assert-psi-test nsamples logps procedure)
  ;; Square the false positive rate for each test case (alpha=.01 --->
  ;; alpha=.0001) so that the false positive rate for the whole test
  ;; suite isn't too high.
  (assert-true
   (or (psi-test (count-sample nsamples procedure) logps nsamples)
       (psi-test (count-sample nsamples procedure) logps nsamples))))

(define-random-test 'uniform-float01
  (lambda (state)
    (let ((logps (make-vector PSI-DF (log (/ 1 PSI-DF)))))
      (assert-psi-test NSAMPLES logps
	(lambda ()
	  (min (- PSI-DF 1)
	       (floor->exact (* PSI-DF (flo:random-unit state)))))))))

(define-random-test 'uniform-integer
  (lambda (state)
    (let ((random-integer (random-source-make-integers state))
	  (logps (make-vector PSI-DF (log (/ 1 PSI-DF)))))
      (assert-psi-test NSAMPLES logps
	(lambda ()
	  (random-integer PSI-DF))))))

;;; Confirm that this simple-minded psi test has adequate statistical
;;; power to detect a modulo bias.

(define-random-test 'nonuniform-integer
  (lambda (state)
    (let ((random-integer (random-source-make-integers state))
	  (logps (make-vector PSI-DF (log (/ 1 PSI-DF)))))
      (define (sampler)
	(modulo (random-integer (+ 1 (* 2 PSI-DF))) PSI-DF))
      (assert-false
       (psi-test (count-sample nsamples sampler) logps nsamples)))))

(define-random-test 'geometric-1/2
  (lambda (state)
    (let ((random-integer (random-source-make-integers state))
	  (logps
	   (make-initialized-vector PSI-DF
	     (lambda (k)
	       ;; p = 1/2, so p = 1 - p, so (1 - p)^{k - 1} * p = p^k.
	       (* k (log 1/2))))))
      (assert-psi-test NSAMPLES logps
	(lambda ()
	  (min (- PSI-DF 1)
	       ;; Probability of zero is 1/2^1000 = never.
	       (first-set-bit (random-integer (shift-left 1 1000)))))))))
