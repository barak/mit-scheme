#| -*-Scheme-*-

$Id: qsort.scm,v 14.3 1998/04/30 18:05:09 cph Exp $

Copyright (c) 1988-98 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Quick Sort
;;; package: ()

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