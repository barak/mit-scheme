;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/qsort.scm,v 13.41 1987/01/23 00:18:12 jinx Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Quick Sort

(declare (usual-integrations))

(define (sort obj pred)
  (if (vector? obj)
      (sort! (vector-copy obj) pred)
      (vector->list (sort! (list->vector obj) pred))))

(define sort!
  (let ()

    (define (exchange! vec i j)
      ;; Speedup hack uses value of VECTOR-SET!.
      (vector-set! vec j (vector-set! vec i (vector-ref vec j))))

    (named-lambda (sort! obj pred)
      (define (sort-internal! vec l r)
	(cond
	 ((<= r l)
	  vec)
	 ((= r (1+ l)) 
	  (if (pred (vector-ref vec r)
		    (vector-ref vec l))
	      (exchange! vec l r)
	      vec))
	 (else
	  (quick-merge vec l r))))

      (define (quick-merge vec l r)
	(let ((first (vector-ref vec l)))
	  (define (increase-i i)
	    (if (or (> i r)
		    (pred first (vector-ref vec i)))
		i
		(increase-i (1+ i))))
	  (define (decrease-j j)
	    (if (or (<= j l)
		    (not (pred first (vector-ref vec j))))
		j
		(decrease-j (-1+ j))))
	  (define (loop i j)
	    (if (< i j)					;* used to be <=
		(begin (exchange! vec i j)
		       (loop (increase-i (1+ i)) (decrease-j (-1+ j))))
		(begin (if (> j l)
			   (exchange! vec j l))
		       (sort-internal! vec (1+ j) r)
		       (sort-internal! vec l (-1+ j)))))
	  (loop (increase-i (1+ l))
		(decrease-j r))))

      (if (vector? obj)
	  (begin (sort-internal! obj 0 (-1+ (vector-length obj)))
		 obj)
	  (error "SORT! works on vectors only" obj)))))
