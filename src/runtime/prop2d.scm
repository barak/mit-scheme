#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Two Dimensional Property Tables
;;; package: (runtime 2D-property)

(declare (usual-integrations))

(define (initialize-package!)
  (set! system-properties '())
  (set! delete-invalid-hash-numbers! (list-deletor! filter-bucket!))
  (set! delete-invalid-y! (list-deletor! filter-entry!))
  (add-secondary-gc-daemon! gc-system-properties!))

(define system-properties)

(define (2D-put! x y value)
  (let ((x-hash (object-hash x))
	(y-hash (object-hash y)))
    (let ((bucket (assq x-hash system-properties)))
      (if bucket
	  (let ((entry (assq y-hash (cdr bucket))))
	    (if entry
		(set-cdr! entry value)
		(set-cdr! bucket
			  (cons (cons y-hash value)
				(cdr bucket)))))
	  (set! system-properties
		(cons (cons x-hash
			    (cons (cons y-hash value)
				  '()))
		      system-properties))))))

(define (2D-get x y)
  (let ((bucket (assq (object-hash x) system-properties)))
    (and bucket
	 (let ((entry (assq (object-hash y) (cdr bucket))))
	   (and entry
		(cdr entry))))))

;;; Returns TRUE iff an entry was removed.
;;; Removes the bucket if the entry removed was the only entry.

(define (2D-remove! x y)
  (let ((bucket (assq (object-hash x) system-properties)))
    (and bucket
	 (begin (set-cdr! bucket
			  (del-assq! (object-hash y)
				     (cdr bucket)))
		(if (null? (cdr bucket))
		    (set! system-properties
			  (del-assq! (object-hash x)
				     system-properties)))
		true))))

;;; This clever piece of code removes all invalid entries and buckets,
;;; and also removes any buckets which [subsequently] have no entries.

(define (gc-system-properties!)
  (set! system-properties (delete-invalid-hash-numbers! system-properties)))

(define (filter-bucket! bucket)
  (or (not (valid-hash-number? (car bucket)))
      (begin (set-cdr! bucket (delete-invalid-y! (cdr bucket)))
	     (null? (cdr bucket)))))

(define (filter-entry! entry)
  (not (valid-hash-number? (car entry))))

(define delete-invalid-hash-numbers!)
(define delete-invalid-y!)

(define (2D-get-alist-x x)
  (let ((bucket (assq (object-hash x) system-properties)))
    (if bucket
	(let loop ((rest (cdr bucket)))
	  (cond ((null? rest) '())
		((valid-hash-number? (caar rest))
		 (cons (cons (object-unhash (caar rest))
			     (cdar rest))
		       (loop (cdr rest))))
		(else (loop (cdr rest)))))
	'())))

(define (2D-get-alist-y y)
  (let ((y-hash (object-hash y)))
    (let loop ((rest system-properties))
      (cond ((null? rest) '())
	    ((valid-hash-number? (caar rest))
	     (let ((entry (assq y-hash (cdar rest))))
	       (if entry
		   (cons (cons (object-unhash (caar rest))
			       (cdr entry))
			 (loop (cdr rest)))
		   (loop (cdr rest)))))
	    (else (loop (cdr rest)))))))