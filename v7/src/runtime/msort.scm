;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/msort.scm,v 13.41 1987/01/23 00:15:59 jinx Rel $
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

;;;; Merge Sort

(declare (usual-integrations))

;; Functional and unstable but fairly fast

(define (sort the-list p)
  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
	(split l '() '())
	l))

  (define (split l one two)
    (if (pair? l)
	(split (cdr l) two (cons (car l) one))
	(merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
	  ((p (car two) (car one))
	   (cons (car two)
		 (merge (cdr two) one)))
	  (else
	   (cons (car one)
		 (merge (cdr one) two)))))

  (loop the-list))
    
;; In-place and stable, fairly slow

#|

(define (sort! vector p)
  (define (merge! source target low1 high1 low2 high2 point)
    (define (loop low1 high1 low2 high2 point)
      (cond ((= low1 high1) (transfer! source target low2 high2 point))
	    ((p (vector-ref source low2) (vector-ref source low1))
	     (vector-set! target point (vector-ref source low2))
	     (loop (1+ low2) high2 low1 high1 (1+ point)))
	    (else
	     (vector-set! target point (vector-ref source low1))
	     (loop (1+ low1) high1 low2 high2 (1+ point)))))
    (loop low1 high1 low2 high2 point))
  (define (transfer! from to low high where)
    (if (= low high)
	'DONE
	(begin (vector-set! to where (vector-ref from low))
	       (transfer! from to (1+ low) high (1+ where)))))
  (define (split! source target low high)
    (let ((bound (ceiling (/ (+ low high) 2))))
      (transfer! source target low bound low)
      (transfer! source target bound high bound)
      (do! target source low bound)
      (do! target source bound high)
      (merge! target source low bound bound high low)))
  (define (do! source target low high)
    (if (< high (+ low 2))
	'DONE
	(split! source target low high)))
  (let ((size (vector-length vector)))
    (do! vector (vector-cons size '()) 0 size)
    vector))
|#
