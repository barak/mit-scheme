#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

#| Original copyrights from which these tests were derived:

SRFI 221:
Copyright (C) 2020 John Cowan (text), Arvydas Silanskas (implementation).

SRFI 158:
Copyright (C) 2015 Shiro Kawai, John Cowan, Thomas Gilray. All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice (including the next
paragraph) shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

|#

;;;; Tests of generators

(declare (usual-integrations))

(define (for-each-digit proc n)
  (when (> n 0)
    (let-values (((div rem) (truncate/ n 10)))
      (proc rem)
      (for-each-digit proc div))))

(define (small? x)
  (< x 3))

(define-test 'constructors
  (lambda ()
    (assert-equal (generator->list (generator))
		  '())
    (assert-equal (generator->list (generator 1 2 3))
		  '(1 2 3))
    (assert-equal (generator->list (circular-generator 1 2 3) 5)
		  '(1 2 3 1 2))
    (assert-equal (generator->list (make-iota-generator 3 8))
		  '(8 9 10))
    (assert-equal (generator->list (make-iota-generator 3 8 2))
		  '(8 10 12))
    (assert-equal (generator->list (make-range-generator 3) 4)
		  '(3 4 5 6))
    (assert-equal (generator->list (make-range-generator 3 8))
		  '(3 4 5 6 7))
    (assert-equal (generator->list (make-range-generator 3 8 2))
		  '(3 5 7))

    (let ((g
	   (make-coroutine-generator
	    (lambda (yield)
	      (let loop ((i 0))
		(when (< i 3)
		  (yield i)
		  (loop (+ i 1))))))))
      (assert-equal (generator->list g)
		    '(0 1 2)))

    (assert-equal (generator->list (list->generator '(1 2 3 4 5)))
		  '(1 2 3 4 5))
    (assert-equal (generator->list (vector->generator '#(1 2 3 4 5)))
		  '(1 2 3 4 5))
    (assert-equal (let ((v (make-vector 5 0)))
		    (generator->vector! v 2 (generator 1 2 4))
		    v)
		  '#(0 0 1 2 4))
    (assert-equal (generator->list (reverse-vector->generator '#(1 2 3 4 5)))
		  '(5 4 3 2 1))
    (assert-equal (generator->list (string->generator "abcde"))
		  '(#\a #\b #\c #\d #\e))
    (assert-equal (generator->list
		   (bytevector->generator (bytevector 10 20 30)))
		  '(10 20 30))

    (assert-equal (generator->list
		   (make-for-each-generator for-each-digit 12345))
		  '(5 4 3 2 1))
    (assert-equal (generator->list
                   (make-unfold-generator
                    (lambda (s) (> s 5))
                    (lambda (s) (* s 2))
                    (lambda (s) (+ s 1))
                    0))
		  '(0 2 4 6 8 10))))

(define-test 'operators
  (lambda ()
    (assert-equal (generator->list (gcons* 'a 'b (make-range-generator 0 2)))
		  '(a b 0 1))
    (assert-equal (generator->list (gappend (make-range-generator 0 3)
					    (make-range-generator 0 2)))
		  '(0 1 2 0 1))
    (assert-equal (generator->list (gappend))
		  '())

    (assert-equal (generator->list
		   (gcombine (lambda args
			       (values (apply + args) (apply + args)))
			     10
			     (generator 1 2 3)
			     (generator 4 5 6 7)))
		  '(15 22 31))

    (assert-equal (generator->list (gfilter odd? (make-range-generator 1 11)))
		  '(1 3 5 7 9))
    (assert-equal (generator->list (gremove odd? (make-range-generator 1 11)))
		  '(2 4 6 8 10))

    (let ((g (make-range-generator 1 5)))
      (assert-equal (generator->list (gtake g 3))
		    '(1 2 3))
      (assert-equal (generator->list g)
		    '(4)))

    (assert-equal (generator->list (gtake (make-range-generator 1 3) 3))
		  '(1 2))
    (assert-equal (generator->list (gtake (make-range-generator 1 3) 3 0))
		  '(1 2 0))
    (assert-equal (generator->list (gdrop (make-range-generator 1 5) 2))
		  '(3 4))

    (assert-equal (generator->list
		   (gtake-while small? (make-range-generator 1 5)))
		  '(1 2))
    (assert-equal (generator->list
		   (gdrop-while small? (make-range-generator 1 5)))
		  '(3 4))

    (assert-equal (generator->list
		   (gdrop-while (lambda args (declare (ignore args)) #t)
				(generator 1 2 3)))
		  '())
    (assert-equal (generator->list
		   (gdelete 1
			    (generator 0.0 1.0 0 1 2)))
		  '(0.0 1.0 0 2))
    (assert-equal (generator->list
		   (gdelete 1
			    (generator 0.0 1.0 0 1 2)
			    =))
		  '(0.0 0 2))
    (assert-equal (generator->list
		   (gindex (list->generator '(a b c d e f))
			   (list->generator '(0 2 4))))
		  '(a c e))
    (assert-equal (generator->list
		   (gselect (list->generator '(a b c d e f))
			    (list->generator '(#t #f #f #t #t #f))))
		  '(a d e))
    (assert-equal (generator->list
		   (gdelete-neighbor-dups (generator 1 1 2 3 3 3)
					  =))
		  '(1 2 3))
    (assert-equal (generator->list
		   (gdelete-neighbor-dups (generator 1 2 3)
					  (lambda args
					    (declare (ignore args))
					    #t)))
		  '(1))
    (assert-equal (generator->list
		   (gflatten (generator '(1 2 3) '(a b c))))
		  '(1 2 3 a b c))
    (assert-equal (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3))
		  '((1 2 3) (4 5 6) (7 8)))
    (assert-equal (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3 0))
		  '((1 2 3) (4 5 6) (7 8 0)))
    (assert-equal (generator->list (gmerge < (generator 1 2 3)))
		  '(1 2 3))
    (assert-equal (generator->list
		   (gmerge < (generator 1 2 3) (generator 4 5 6)))
		  '(1 2 3 4 5 6))
    (assert-equal (generator->list (gmerge <
					   (generator 1 2 4 6)
					   (generator)
					   (generator 3 4 5)))
		  '(1 2 3 4 4 5 6))
    (assert-equal (generator->list (gmerge <
					   (generator 1 10 11)
					   (generator 2 9 12)
					   (generator 3 8 13)
					   (generator 4 7 14)
					   (generator 5 6 15)))
		  '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    ;; check the tie-break rule
    (assert-equal (generator->list (gmerge (lambda (x y) (< (car x) (car y)))
					   (generator '(1 a) '(1 e))
					   (generator '(1 b))
					   (generator '(1 c) '(1 d))))
		  '((1 a) (1 e) (1 b) (1 c) (1 d)))

    (assert-equal (generator->list (gmap - (generator 1 2 3 4 5)))
		  '(-1 -2 -3 -4 -5))
    (assert-equal (generator->list (gmap +
					 (generator 1 2 3 4 5)
					 (generator 6 7 8 9)))
		  '(7 9 11 13))
    (assert-equal (generator->list (gmap *
					 (generator 1 2 3 4 5)
					 (generator 6 7 8)
					 (generator 9 10 11 12 13)))
		  '(54 140 264))
    (assert-equal (generator->list
		   (gstate-filter
		    (lambda (item state)
		      (declare (ignore item))
		      (values (even? state) (+ 1 state)))
		    0
		    (generator 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
		  '(a c e g i))))

(define-test 'consumers
  (lambda ()
    ;; no test for plain generator->list (used throughout)
    (assert-equal (generator->list (generator 1 2 3 4 5) 3)
		  '(1 2 3))
    (assert-equal (generator->reverse-list (generator 1 2 3 4 5))
		  '(5 4 3 2 1))
    (assert-equal (generator->vector (generator 1 2 3 4 5))
		  '#(1 2 3 4 5))
    (assert-equal (generator->vector (generator 1 2 3 4 5) 3)
		  '#(1 2 3))
    (assert-equal (generator->string (generator #\a #\b #\c))
		  "abc")
    (assert-equal (call-with-input-string "a b c d e"
		    (lambda (port)
		      (generator-fold cons 'z (lambda () (read port)))))
		  '(e d c b a . z))

    (let ((n))
      (generator-for-each (lambda values
			    (set! n (apply + values))
			    unspecific)
			  (generator 1)
			  (generator 2)
			  (generator 3))
      (assert-equal n 6))

    (assert-equal (generator-map->list (lambda values (apply + values))
				       (generator 1 4)
				       (generator 2 5)
				       (generator 3 6))
		  '(6 15))
    (assert-equal (generator-find (lambda (x) (> x 2))
				  (make-range-generator 1 5))
		  3)
    (assert-equal (generator-find (lambda (x) (> x 10))
				  (make-range-generator 1 5))
		  #f)
    (assert-equal (generator-count odd? (make-range-generator 1 5))
		  2)
    (let ((g (make-range-generator 2 5)))
      (assert-equal (generator-any odd? g)
		    #t)
      (assert-equal (generator->list g)
		    '(4)))
    (assert-equal (generator-any (lambda (x) (and (odd? x) x))
				 (make-range-generator 2 5))
		  3)
    (let ((g (make-range-generator 2 5)))
      (assert-equal (generator-every odd? g)
		    #f)
      (assert-equal (generator->list g)
		    '(3 4)))
    (let ((g (make-range-generator 2 5)))
      (assert-equal (generator-every (lambda (x) (and (> x 1) x)) g)
		    4)
      (assert-equal (generator->list g)
		    '()))
    (assert-equal (generator-unfold
		   (make-for-each-generator string-for-each "abc")
		   unfold)
		  '(#\a #\b #\c))))

(define-test 'accumulators
  (lambda ()

    (define (run-accum a . vals)
      (for-each a vals)
      (a (eof-object)))

    (define (accum-test a expected)
      (assert-equal (run-accum a 1 2 4)
		    expected))

    (accum-test (make-accumulator * 1 -) -8)

    (accum-test (count-accumulator)
		3)

    (accum-test (list-accumulator)
		'(1 2 4))

    (accum-test (reverse-list-accumulator)
		'(4 2 1))

    (accum-test (vector-accumulator)
		'#(1 2 4))

    (accum-test (vector-accumulator! (vector 0 0 0 0 0) 2)
		'#(0 0 1 2 4))

    (accum-test (bytevector-accumulator! (bytevector 0 0 0 0 0) 2)
		'#u8(0 0 1 2 4))

    (accum-test (reverse-vector-accumulator)
		'#(4 2 1))

    (assert-equal (run-accum (string-accumulator) #\a #\b #\c)
		  "abc")

    (accum-test (bytevector-accumulator)
		'#u8(1 2 4))

    (accum-test (sum-accumulator)
		7)

    (accum-test (product-accumulator)
		8)))