#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Quick Sort
;;; package: (runtime quick-sort)

(declare (usual-integrations))

(define (quick-sort vector predicate)
  (if (vector? vector)
      (quick-sort! (vector-copy vector) predicate)
      (vector->list (quick-sort! (list->vector vector) predicate))))

(define (quick-sort! vector predicate)
  (define (outer-loop l r)
    (if (fix:> r l)
	(if (fix:= r (fix:+ l 1))
	    (if (predicate (vector-ref vector r)
			   (vector-ref vector l))
		(exchange! l r))
	    (let ((lth-element (vector-ref vector l)))

	      (define (increase-i i)
		(if (or (fix:> i r)
			(predicate lth-element (vector-ref vector i)))
		    i
		    (increase-i (fix:+ i 1))))

	      (define (decrease-j j)
		(if (or (fix:<= j l)
			(not (predicate lth-element (vector-ref vector j))))
		    j
		    (decrease-j (fix:- j 1))))

	      (define (inner-loop i j)
		(if (fix:< i j)		;used to be <=
		    (begin
		      (exchange! i j)
		      (inner-loop (increase-i (fix:+ i 1))
				  (decrease-j (fix:- j 1))))
		    (begin
		      (if (fix:> j l)
			  (exchange! j l))
		      (outer-loop (fix:+ j 1) r)
		      (outer-loop l (fix:- j 1)))))

	      (inner-loop (increase-i (fix:+ l 1))
			  (decrease-j r))))))

  (define-integrable (exchange! i j)
    (let ((ith-element (vector-ref vector i)))
      (vector-set! vector i (vector-ref vector j))
      (vector-set! vector j ith-element)))

  (if (not (vector? vector))
      (error:wrong-type-argument vector "vector" 'QUICK-SORT!))
  (outer-loop 0 (fix:- (vector-length vector) 1))
  vector)