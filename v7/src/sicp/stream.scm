#| -*-Scheme-*-

$Id: stream.scm,v 1.5 2007/01/05 15:33:10 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990, 1999 Massachusetts Institute of Technology

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
  (integers-from 1))

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