;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/stream.scm,v 13.41 1987/01/23 00:20:30 jinx Rel $
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

;;;; Stream Utilities

(declare (usual-integrations))

;;;; General Streams

(define (nth-stream n s)
  (cond ((empty-stream? s)
	 (error "Empty stream -- NTH-STREAM" n))
	((= n 0)
	 (head s))
	(else
	 (nth-stream (- n 1) (tail s)))))

(define (accumulate combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (combiner (head stream)
		(accumulate combiner
			    initial-value
			    (tail stream)))))

(define (filter pred stream)
  (cond ((empty-stream? stream)
	 the-empty-stream)
	((pred (head stream))
	 (cons-stream (head stream)
		      (filter pred (tail stream))))
	(else
	 (filter pred (tail stream)))))

(define (map-stream proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (head stream))
		   (map-stream proc (tail stream)))))

(define (map-stream-2 proc s1 s2)
  (if (or (empty-stream? s1)
	  (empty-stream? s2))
      the-empty-stream
      (cons-stream (proc (head s1) (head s2))
		   (map-stream-2 proc (tail s1) (tail s2)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
		   (append-streams (tail s1) s2))))

(define (enumerate-fringe tree)
  (if (pair? tree)
      (append-streams (enumerate-fringe (car tree))
		      (enumerate-fringe (cdr tree)))
      (cons-stream tree the-empty-stream)))

;;;; Numeric Streams

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream (+ (head s1) (head s2))
		      (add-streams (tail s1) (tail s2))))))

(define (scale-stream c s)
  (map-stream (lambda (x) (* c x)) s))

(define (enumerate-interval n1 n2)
  (if (> n1 n2)
      the-empty-stream
      (cons-stream n1 (enumerate-interval (1+ n1) n2))))

(define (integers-from n)
  (cons-stream n (integers-from (1+ n))))

(define integers
  (integers-from 0))

;;;; Some Hairier Stuff

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
	 (let ((h1 (head s1))
	       (h2 (head s2)))
	   (cond ((< h1 h2)
		  (cons-stream h1
			       (merge (tail s1)
				      s2)))
		 ((> h1 h2)
		  (cons-stream h2
			       (merge s1
				      (tail s2))))
		 (else
		  (cons-stream h1
			       (merge (tail s1)
				      (tail s2)))))))))

;;;; Printing

(define print-stream
  (let ()
    (define (iter s)
      (if (empty-stream? s)
	  (write-string "}")
	  (begin (write-string " ")
		 (write (head s))
		 (iter (tail s)))))
    (lambda (s)
      (newline)
      (write-string "{")
      (if (empty-stream? s)
	  (write-string "}")
	  (begin (write (head s))
		 (iter (tail s)))))))

;;;; Support for COLLECT

(define (flatmap f s)
  (flatten (map-stream f s)))

(define (flatten stream)
  (accumulate-delayed interleave-delayed
		      the-empty-stream
		      stream))

(define (accumulate-delayed combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (combiner (head stream)
		(delay (accumulate-delayed combiner
					   initial-value
					   (tail stream))))))

(define (interleave-delayed s1 delayed-s2)
  (if (empty-stream? s1)
      (force delayed-s2)
      (cons-stream (head s1)
		   (interleave-delayed (force delayed-s2)
				       (delay (tail s1))))))

(define ((spread-tuple procedure) tuple)
  (apply procedure tuple))
