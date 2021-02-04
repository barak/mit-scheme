#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Test of promises

(declare (usual-integrations))

(define-test 'delay-force-loop
  (lambda ()
    (assert-error
     (lambda ()
       (carefully (lambda ()
                    (define p (delay-force p))
                    (force p))
                  (lambda () 'stack-overflow)
                  (lambda () 'timeout))))))

(define-test 'force-force-delay-delay
  (lambda ()
    (assert-eqv (force (force (delay (delay 0)))) 0)))

;; Adapted from SRFI 45.

(define-test 'memoization-1
  (lambda ()
    (let* ((c 0)
           (s (delay (begin (set! c (+ c 1)) 'ok))))
      (assert-eqv (force s) 'ok)
      (assert-eqv c 1))))

(define-test 'memoization-2
  (lambda ()
    (let* ((c 0)
           (s (delay (begin (set! c (+ c 1)) 42))))
      (assert-eqv (+ (force s) (force s)) 84)
      (assert-eqv c 1))))

(define-test 'memoization-3
  (lambda ()
    (let* ((c 0)
           (r (delay (begin (set! c (+ c 1)) 'ok)))
           (s (delay-force r))
           (t (delay-force s)))
      (assert-eqv (force t) 'ok)
      (assert-eqv (force s) 'ok)
      (assert-eqv c 1))))

(define-test 'memoization-4
  (lambda ()
    (define (stream-drop s i)
      (delay-force
       (if (zero? i)
           s
           (stream-drop (cdr (force s)) (- i 1)))))
    (define c 0)
    (define (count-from n)
      (delay (begin (set! c (+ c 1)) (cons n (count-from (+ n 1))))))
    (define s0 (count-from 0))
    (assert-eqv (car (force (stream-drop s0 4))) 4)
    (assert-eqv (car (force (stream-drop s0 4))) 4)
    (assert-eqv c 5)
    (assert-eqv (car (force (stream-drop (count-from 0) 4))) 4)
    (assert-eqv c 10)))

(define-test 'reentrancy-1
  (lambda ()
    (let ((c 0) (x 5))
      (define p
        (delay
          (begin (set! c (+ c 1))
                 (if (> c x)
                     c
                     (force p)))))
      (assert-eqv (force p) 6)
      (set! x 10)
      (assert-eqv (force p) 6))))

(define-test 'reentrancy-2
  (lambda ()
    (let ((first? #t))
      (define p
        (delay
          (if first?
              (begin
                (set! first? #f)
                (force p))
              'second)))
      (assert-true first?)
      (assert-eqv (force p) 'second)
      (assert-false first?))))

(define-test 'reentrancy-3
  (lambda ()
    (let ((c 5))
      (define p
        (delay
          (if (<= c 0)
              c
              (begin
                (set! c (- c 1))
                (force p)
                (set! c (+ c 2))
                c))))
      (assert-eqv c 5)
      (assert-eqv (force p) 0)
      (assert-eqv c 10))))

(define (words-in-heap)
  (let ((status (gc-space-status)))
    (let ((heap-start (vector-ref status 4))
          (heap-end (vector-ref status 7)))
      (let ((n-words (- heap-end heap-start)))
        (if keep-it-fast!?
            (quotient n-words 100)
            n-words)))))

(define-test 'leak-1
  (lambda ()
    (define (count-down n)
      (delay-force (if (zero? n) (delay 0) (count-down (- n 1)))))
    (force (count-down (words-in-heap)))))

(define-test 'leak-2
  (lambda ()
    (define (count-down n)
      (delay-force (if (zero? n) (delay 0) (count-down (- n 1)))))
    (let ((p (count-down (words-in-heap))))
      (force p)
      (reference-barrier p))))

(define-test 'leak-3
  (lambda ()
    (define (count-from n)
      (delay (cons n (count-from (+ n 1)))))
    (define (stream-ref s i)
      (delay-force
       (if (zero? i)
           (delay (car (force s)))
           (stream-ref (cdr (force s)) (- i 1)))))
    (let ((n (words-in-heap)))
      (assert-eqv (force (stream-ref (count-from 0) n)) n))))

;; Tests 4, 5, and 6 aren't terribly interesting.

(define-test 'leak-7
  (lambda ()
    (define (count-from n)
      (delay (cons n (count-from (+ n 1)))))
    (define (stream-ref s i)
      (delay-force
       (if (zero? i)
           (delay (car (force s)))
           (stream-ref (cdr (force s)) (- i 1)))))
    (define (stream-filter f s)
      (delay-force
       (if (pair? (force s))
           (let ((x (car (force s)))
                 (s* (delay-force (stream-filter f (cdr (force s))))))
             (if (f x)
                 (delay (cons x s*))
                 s*))
           (delay '()))))
    (define ((divisible-by? d) n)
      (zero? (modulo n d)))
    (define (times3 n)
      (force (stream-ref (stream-filter (divisible-by? n) (count-from 0)) 3)))
    (let ((n (quotient (words-in-heap) 3)))
      (assert-eqv (times3 n) (* 3 n)))))
