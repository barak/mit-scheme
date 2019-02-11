#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Test of LETREC and LETREC*

(declare (usual-integrations))

(define-test 'even?/odd?-definition
  (lambda ()
    (define (even? x) (or (zero? x) (odd? (- x 1))))
    (define (odd? x) (and (not (zero? x)) (even? (- x 1))))
    (assert-true (even? 4))
    (assert-false (odd? 4))))

(define-test 'even?/odd?-letrec
  (lambda ()
    (letrec ((even? (lambda (x) (or (zero? x) (odd? (- x 1)))))
	     (odd? (lambda (x) (and (not (zero? x)) (even? (- x 1))))))
      (assert-true (even? 4))
      (assert-false (odd? 4)))))

(define-test 'even?/odd?-letrec*
  (lambda ()
    (letrec* ((even? (lambda (x) (or (zero? x) (odd? (- x 1)))))
	      (odd? (lambda (x) (and (not (zero? x)) (even? (- x 1))))))
      (assert-true (even? 4))
      (assert-false (odd? 4)))))

(define-test 'letrec-cwcc
  (lambda ()
    (let ((k))
      ;; The continuation k0 WILL assign x.
      (letrec ((x 1)
	       (y
		(call-with-current-continuation
		  (lambda (k0)
		    (set! k k0)
		    #t))))
	(if y
	    (begin (set! x 2) (k #f))
	    (assert-eqv x 1))))))

(define-test 'letrec*-cwcc
  (lambda ()
    (let ((k))
      ;; The continuation k0 WILL NOT assign x.
      (letrec* ((x 1)
		(y
		 (call-with-current-continuation
		   (lambda (k0)
		     (set! k k0)
		     #t))))
	(if y
	    (begin (set! x 2) (k #f))
	    (assert-eqv x 2))))))
