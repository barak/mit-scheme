#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/qsort.scm,v 14.1 1988/06/13 11:50:22 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

(define (sort vector predicate)
  (if (vector? vector)
      (sort! (vector-copy vector) predicate)
      (vector->list (sort! (list->vector vector) predicate))))

(define (sort! vector predicate)

  (define (outer-loop l r)
    (if (> r l)
	(if (= r (1+ l)) 
	    (if (predicate (vector-ref vector r)
			   (vector-ref vector l))
		(exchange! l r))
	    (let ((lth-element (vector-ref vector l)))

	      (define (increase-i i)
		(if (or (> i r)
			(predicate lth-element (vector-ref vector i)))
		    i
		    (increase-i (1+ i))))

	      (define (decrease-j j)
		(if (or (<= j l)
			(not (predicate lth-element (vector-ref vector j))))
		    j
		    (decrease-j (-1+ j))))

	      (define (inner-loop i j)
		(if (< i j)		;used to be <=
		    (begin (exchange! i j)
			   (inner-loop (increase-i (1+ i))
				       (decrease-j (-1+ j))))
		    (begin (if (> j l)
			       (exchange! j l))
			   (outer-loop (1+ j) r)
			   (outer-loop l (-1+ j)))))

	      (inner-loop (increase-i (1+ l))
			  (decrease-j r))))))

  (define-integrable (exchange! i j)
    (let ((ith-element (vector-ref vector i)))
      (vector-set! vector i (vector-ref vector j))
      (vector-set! vector j ith-element)))

  (if (not (vector? vector))
      (error "SORT! works on vectors only" vector))
  (outer-loop 0 (-1+ (vector-length vector)))
  vector)