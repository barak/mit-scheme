#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/prop2d.scm,v 14.2 1988/06/13 11:50:17 cph Rel $

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