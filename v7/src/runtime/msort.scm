#| -*-Scheme-*-

$Id: msort.scm,v 14.6 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Merge Sort
;;; package: ()

(declare (usual-integrations))

;; Functional and unstable

(define (merge-sort obj pred)
  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
	(split l '() '())
	l))

  (define (split l one two)
    (if (pair? l)
	(split (cdr l) two (cons (car l) one))
	(begin
	  (if (not (null? l)) (lose))
	  (merge (loop one) (loop two)))))

  (define (merge one two)
    (cond ((null? one)
	   two)
	  ((pred (car two) (car one))
	   (cons (car two)
		 (merge (cdr two) one)))
	  (else
	   (cons (car one)
		 (merge (cdr one) two)))))

  (define (lose)
    (error:wrong-type-argument obj "list or vector" 'MERGE-SORT))

  (if (vector? obj)
      (merge-sort! (vector-copy obj) pred)
      (begin
	(if (pair? obj)
	    (if (pair? (cdr obj))
		(loop obj)
		(begin
		  (if (not (null? (cdr obj))) (lose))
		  ;; Must return newly allocated list.
		  (list (car obj))))
	    (begin
	      (if (not (null? obj)) (lose))
	      '())))))

;; This merge sort is stable for partial orders (for predicates like
;; <=, rather than like <).

(define (merge-sort! v pred)
  (define (sort-internal! vec temp low high)
    (if (fix:< low high)
	(let* ((middle (quotient (fix:+ low high) 2))
	       (next (fix:+ middle 1)))
	  (sort-internal! temp vec low middle)
	  (sort-internal! temp vec next high)
	  (let loop ((p low) (p1 low) (p2 next))
	    (if (not (fix:> p high))
		(cond ((fix:> p1 middle)
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (fix:+ p 1) p1 (fix:+ p2 1)))
		      ((or (fix:> p2 high)
			   (pred (vector-ref temp p1)
				 (vector-ref temp p2)))
		       (vector-set! vec p (vector-ref temp p1))
		       (loop (fix:+ p 1) (fix:+ p1 1) p2))
		      (else
		       (vector-set! vec p (vector-ref temp p2))
		       (loop (fix:+ p 1) p1 (fix:+ p2 1)))))))))

  (if (not (vector? v))
      (error:wrong-type-argument v "vector" 'MERGE-SORT!))
  (sort-internal! v
		  (vector-copy v)
		  0
		  (fix:- (vector-length v) 1))
  v)

(define sort merge-sort)
(define sort! merge-sort!)