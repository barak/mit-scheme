;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
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

;;;; Object Hashing

(declare (usual-integrations))

((make-primitive-procedure 'INITIALIZE-OBJECT-HASH) 313)
(add-gc-daemon! (make-primitive-procedure 'REHASH-GC-DAEMON))
(add-event-receiver! event:after-restore gc-flip)

(define object-hash (make-primitive-procedure 'OBJECT-HASH))
(define object-unhash (make-primitive-procedure 'OBJECT-UNHASH))

(define hash-of-false (object-hash #!FALSE))
(define hash-of-false-number (primitive-datum hash-of-false))

(define (hash object)
  (primitive-datum (object-hash object)))

(define (unhash n)
  (if (= n hash-of-false-number)
      #!FALSE
      (or (object-unhash (make-non-pointer-object n))
	  (error "Not a valid hash number" 'UNHASH n))))

(define (valid-hash-number? n)
  (if (eq? n hash-of-false)
      #!TRUE
      (object-unhash n)))

;;;; Populations
;;;
;;;  A population is a collection of objects.  This collection
;;;  has the property that if one of the objects in the collection
;;;  is reclaimed as garbage, then it is no longer an element of
;;;  the collection.

(define make-population)
(define population?)

(let ((population-tag '(POPULATION)))

(define population-of-populations
  (cons population-tag '()))

(set! make-population
(named-lambda (make-population)
  (let ((population (cons population-tag '())))
    (add-to-population! population-of-populations population)
    population)))

(set! population?
(named-lambda (population? object)
  (and (pair? object)
       (eq? (car object) population-tag))))

(define (gc-population! population)
  (set-cdr! population (delete-invalid-hash-numbers! (cdr population))))

(define delete-invalid-hash-numbers!
  (list-deletor!
   (lambda (hash-number)
     (not (valid-hash-number? hash-number)))))

(define (gc-all-populations!)
  (gc-population! population-of-populations)
  (map-over-population population-of-populations gc-population!))

(add-secondary-gc-daemon! gc-all-populations!)

)

(define (add-to-population! population object)
  (let ((n (object-hash object)))
    (if (not (memq n (cdr population)))
	(set-cdr! population (cons n (cdr population))))))

(define (remove-from-population! population object)
  (set-cdr! population
	    (delq! (object-hash object)
		   (cdr population))))

;;; Population Mappings
;;; These have the effect of doing a GC-POPULATION! every time it is
;;; called, since the cost of doing so is very small.

(define (map-over-population population procedure)
  (let loop ((previous population)
	     (rest (cdr population)))
    (if (null? rest)
	'()
	(let ((unhash (object-unhash (car rest))))
	  (if (or (eq? hash-of-false (car rest))
		  unhash)
	      (cons (procedure unhash)
		    (loop rest (cdr rest)))
	      (begin (set-cdr! previous (cdr rest))
		     (loop previous (cdr rest))))))))

(define (map-over-population! population procedure)
  (let loop ((previous population)
	     (rest (cdr population)))
    (if (not (null? rest))
	(let ((unhash (object-unhash (car rest))))
	  (if (or (eq? hash-of-false (car rest))
		  unhash)
	      (begin (procedure unhash)
		     (loop rest (cdr rest)))
	      (begin (set-cdr! previous (cdr rest))
		     (loop previous (cdr rest))))))))

(define (for-all-inhabitants? population predicate)
  (let loop ((previous population)
	     (rest (cdr population)))
    (or (null? rest)
	(let ((unhash (object-unhash (car rest))))
	  (if (or (eq? hash-of-false (car rest))
		  unhash)
	      (and (predicate unhash)
		   (loop rest (cdr rest)))
	      (begin (set-cdr! previous (cdr rest))
		     (loop previous (cdr rest))))))))

(define (exists-an-inhabitant? population predicate)
  (let loop ((previous population)
	     (rest (cdr population)))
    (and (not (null? rest))
	 (let ((unhash (object-unhash (car rest))))
	   (if (or (eq? hash-of-false (car rest))
		   unhash)
	       (or (predicate unhash)
		   (loop rest (cdr rest)))
	       (begin (set-cdr! previous (cdr rest))
		      (loop previous (cdr rest))))))))

;;;; Properties

(define 2D-put!)
(define 2D-get)
(define 2D-remove!)
(define 2D-get-alist-x)
(define 2D-get-alist-y)

(let ((system-properties '()))

(set! 2D-put!
      (named-lambda (2D-put! x y value)
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
			    system-properties)))))))

(set! 2D-get
      (named-lambda (2D-get x y)
	(let ((bucket (assq (object-hash x) system-properties)))
	  (and bucket
	       (let ((entry (assq (object-hash y) (cdr bucket))))
		 (and entry
		      (cdr entry)))))))

;;; Returns TRUE iff an entry was removed.
;;; Removes the bucket if the entry removed was the only entry.

(set! 2D-remove!
      (named-lambda (2D-remove! x y)
	(let ((bucket (assq (object-hash x) system-properties)))
	  (and bucket
	       (begin (set-cdr! bucket
				(del-assq! (object-hash y)
					   (cdr bucket)))
		      (if (null? (cdr bucket))
			  (set! system-properties
				(del-assq! (object-hash x)
					   system-properties)))
		      #!TRUE)))))

;;; This clever piece of code removes all invalid entries and buckets,
;;; and also removes any buckets which [subsequently] have no entries.

(define (gc-system-properties!)
  (set! system-properties (delete-invalid-hash-numbers! system-properties)))

(define delete-invalid-hash-numbers!
  (list-deletor!
   (lambda (bucket)
     (or (not (valid-hash-number? (car bucket)))
	 (begin (set-cdr! bucket (delete-invalid-y! (cdr bucket)))
		(null? (cdr bucket)))))))

(define delete-invalid-y!
  (list-deletor!
   (lambda (entry)
     (not (valid-hash-number? (car entry))))))

(add-secondary-gc-daemon! gc-system-properties!)

(set! 2D-get-alist-x
      (named-lambda (2D-get-alist-x x)
	(let ((bucket (assq (object-hash x) system-properties)))
	  (if bucket
	      (let loop ((rest (cdr bucket)))
		(cond ((null? rest) '())
		      ((valid-hash-number? (caar rest))
		       (cons (cons (object-unhash (caar rest))
				   (cdar rest))
			     (loop (cdr rest))))
		      (else (loop (cdr rest)))))
	      '()))))

(set! 2D-get-alist-y
      (named-lambda (2D-get-alist-y y)
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
		  (else (loop (cdr rest))))))))

