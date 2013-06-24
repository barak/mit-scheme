#| -*-Scheme-*-

$Id: qsort.scm,v 14.5 2001/12/18 18:39:47 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
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