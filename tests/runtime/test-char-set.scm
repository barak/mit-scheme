#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Test of character-set abstraction

(declare (usual-integrations))

(define-test 'scalar-value-list
  (lambda ()
    (list (run-random-svl-tests 0 1)
	  (map (lambda (i)
		 (run-random-svl-tests i 100))
	       (iota 4 1))
	  (run-random-svl-tests 100 100))))

(define (run-random-svl-tests n-ranges n-iter)
  (map (lambda (i)
	 i
	 (run-random-svl-test n-ranges))
       (iota n-iter)))

(define (run-random-svl-test n-ranges)
  (let ((svl (make-random-svl n-ranges)))
    (guarantee-well-formed-scalar-value-list svl)
    (let ((svl1 (%canonicalize-scalar-value-list svl))
	  (svl2 (char-set->scalar-values (scalar-values->char-set svl))))
      (list (assert-true `(canonical-svl? ,svl1)
			 (canonical-svl? svl1))
	    (assert-true `(canonical-svl? ,svl2)
			 (canonical-svl? svl2))
	    (assert-equal svl1 svl2)))))

(define (make-random-svl n-ranges)
  ;; Random modulus must exceed %LOW-LIMIT.
  (let ((modulus #x1000))
    (make-initialized-list n-ranges
      (lambda (i)
	(let loop ()
	  (let ((n (random (- char-code-limit modulus))))
	    (let ((m (random modulus)))
	      (if (= m 0)
		  n
		  (cons n (+ n m 1))))))))))

(define (canonical-svl? items)
  (and (list-of-type? items
	 (lambda (item)
	   (if (pair? item)
	       (and (exact-nonnegative-integer? (car item))
		    (exact-nonnegative-integer? (cdr item))
		    (< (car item) (cdr item))
		    (<= (cdr item) char-code-limit))
	       (and (exact-nonnegative-integer? item)
		    (< item char-code-limit)))))
       (every-tail (lambda (tail)
		     (if (and (pair? tail)
			      (pair? (cdr tail)))
			 (< (let ((a (car tail)))
			      (if (pair? a)
				  (cdr a)
				  (+ a 1)))
			    (let ((b (cadr tail)))
			      (if (pair? b)
				  (car b)
				  b)))
			 #t))
		   items)))

(define (every-tail pred items)
  (if (pair? items)
      (and (pred items)
	   (every-tail pred (cdr items)))
      (pred items)))