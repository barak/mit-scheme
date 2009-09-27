#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Tests of hash-table implementation

(declare (usual-integrations))

(define-structure (implementation (conc-name implementation/))
  (make #f read-only #t)
  (insert! #f read-only #t)
  (delete! #f read-only #t)
  (lookup #f read-only #t)
  (->alist #f read-only #t))

(define-structure (sequence-specification
		   (conc-name sequence-specification/))
  (length #f read-only #t)
  (key-radix #f read-only #t)
  (insert-fraction #f read-only #t)
  (delete-fraction #f read-only #t))

(define (run-test sequence-specification implementation)
  (run-sequence (sequence-specification->sequence sequence-specification)
		implementation))

(define (run-sequence s implementation)
  (let ((start-time (process-time-clock)))
    (run-test-sequence s implementation)
    (cons (- (process-time-clock) start-time)
	  (test-sequence-overhead s))))

(define (make-sequence . arguments)
  (sequence-specification->sequence
   (apply make-sequence-specification arguments)))

(define (sequence-specification->sequence sequence-specification)
  (generate-test-sequence (make-random-state #t)
			  sequence-specification))

(define (generate-test-sequence state sequence-specification)
  (let ((state (make-random-state state))
	(length (sequence-specification/length sequence-specification))
	(key-radix (sequence-specification/key-radix sequence-specification))
	(insert-fraction
	 (exact->inexact
	  (sequence-specification/insert-fraction sequence-specification)))
	(delete-fraction
	 (exact->inexact
	  (sequence-specification/delete-fraction sequence-specification)))
	(tree (make-rb-tree fix:= fix:<)))
    (let ((delete-break (+ insert-fraction delete-fraction)))
      (let loop ((n 0) (s '()))
	(if (fix:= n length)
	    s
	    (loop (fix:+ n 1)
		  (cons (cons (let ((x (random 1. state)))
				(cond ((< x insert-fraction) 'INSERT)
				      ((< x delete-break) 'DELETE)
				      (else 'LOOKUP)))
			      (let ((key (random key-radix state)))
				(or (rb-tree/lookup tree key #f)
				    (let ((pointer (cons key '())))
				      (rb-tree/insert! tree key pointer)
				      pointer))))
			s)))))))

(define (run-test-sequence s implementation)
  (let ((table ((implementation/make implementation)))
	(insert! (implementation/insert! implementation))
	(delete! (implementation/delete! implementation))
	(lookup (implementation/lookup implementation)))
    (do ((s s (cdr s)))
	((null? s))
      (cond ((eq? 'INSERT (caar s))
	     (insert! table (cdar s) #f))
	    ((eq? 'DELETE (caar s))
	     (delete! table (cdar s)))
	    (else
	     (lookup table (cdar s) #f))))
    table))

(define (test-sequence-overhead s)
  (let ((start-time (process-time-clock)))
    (run-test-sequence s dummy-implementation)
    (let ((end-time (process-time-clock)))
      (- end-time start-time))))

(define dummy-implementation
  (make-implementation
   (lambda () unspecific)
   (lambda (table key datum) table key datum unspecific)
   (lambda (table key) table key unspecific)
   (lambda (table key default) table key default unspecific)
   (lambda (table) table unspecific)))

(load-option 'HASH-TABLE)

(define htq
  (make-implementation make-eq-hash-table
		       hash-table/put!
		       hash-table/remove!
		       hash-table/get
		       (lambda (table)
			 (sort (hash-table->alist table)
			       (lambda (x y) (fix:< (caar x) (caar y)))))))

(define htv
  (make-implementation make-eqv-hash-table
		       hash-table/put!
		       hash-table/remove!
		       hash-table/get
		       (lambda (table)
			 (sort (hash-table->alist table)
			       (lambda (x y) (fix:< (caar x) (caar y)))))))

(define ht
  (make-implementation make-equal-hash-table
		       hash-table/put!
		       hash-table/remove!
		       hash-table/get
		       (lambda (table)
			 (sort (hash-table->alist table)
			       (lambda (x y) (fix:< (caar x) (caar y)))))))

(load-option 'RB-TREE)

(define (make-pointer-tree)
  (make-rb-tree (lambda (x y) (fix:= (car x) (car y)))
		(lambda (x y) (fix:< (car x) (car y)))))

(define rbt
  (make-implementation make-pointer-tree
		       rb-tree/insert!
		       rb-tree/delete!
		       rb-tree/lookup
		       rb-tree->alist))

(define (test-correctness s implementation)
  (let ((table ((implementation/make implementation)))
	(insert! (implementation/insert! implementation))
	(delete! (implementation/delete! implementation))
	(lookup (implementation/lookup implementation))
	(tree (make-pointer-tree)))
    (do ((s s (cdr s)))
	((null? s))
      (let ((operation (caar s))
	    (key (cdar s)))
	(cond ((eq? 'INSERT operation)
	       (rb-tree/insert! tree key #t)
	       (insert! table key #t))
	      ((eq? 'DELETE operation)
	       (rb-tree/delete! tree key)
	       (delete! table key))
	      (else
	       (let ((result (lookup table key #f)))
		 (if (not (eq? result (rb-tree/lookup tree key #f)))
		     (error "Incorrect lookup result:" result key)))))))
    (let loop
	((alist ((implementation/->alist implementation) table))
	 (check (rb-tree->alist tree)))
      (if (null? alist)
	  (if (not (null? check))
	      (error "Table has too few elements:" check))
	  (begin
	    (if (null? check)
		(error "Table has too many elements:" alist))
	    (if (not (and (eq? (caar alist) (caar check))
			  (eq? (cdar alist) (cdar check))))
		(error "Alist element incorrect:" (car alist) (car check)))
	    (loop (cdr alist) (cdr check)))))))