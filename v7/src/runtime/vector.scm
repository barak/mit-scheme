;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Operations on Vectors

(declare (usual-integrations))

;;; Standard Procedures

(in-package system-global-environment
(let-syntax ()
  (define-macro (define-primitives . names)
    `(BEGIN ,@(map (lambda (name)
		     `(DEFINE ,name ,(make-primitive-procedure name)))
		   names)))
  (define-primitives
   vector-length vector-ref vector-set!
   list->vector vector-cons subvector->list)))

(let-syntax ()
  (define-macro (define-type-predicate name type-name)
    `(DEFINE (,name OBJECT)
       (PRIMITIVE-TYPE? ,(microcode-type type-name) OBJECT)))
  (define-type-predicate vector? vector))

(define (make-vector size #!optional fill)
  (if (unassigned? fill) (set! fill #!FALSE))
  (vector-cons size fill))

(define (vector . elements)
  (list->vector elements))

(define (vector->list vector)
  (subvector->list vector 0 (vector-length vector)))

(define (vector-fill! vector value)
  (subvector-fill! vector 0 (vector-length vector) value))

;;; Nonstandard Primitives

(let-syntax ((check-type
	      (let ((type (microcode-type 'VECTOR)))
		(macro (object)
		  `(IF (NOT (PRIMITIVE-TYPE? ,type ,object))
		       (ERROR "Wrong type argument" ,object)))))
	     (check-target
	      (macro (object index)
		`(BEGIN (CHECK-TYPE ,object)
			(IF (NOT (AND (NOT (NEGATIVE? ,index))
				      (<= ,index (VECTOR-LENGTH ,object))))
			    (ERROR "Index out of range" ,index)))))
	     (check-subvector
	      (macro (object start end)
		`(BEGIN (CHECK-TYPE ,object)
			(IF (NOT (AND (NOT (NEGATIVE? ,start))
				      (<= ,start ,end)
				      (<= ,end (VECTOR-LENGTH ,object))))
			    (ERROR "Indices out of range" ,start ,end))))))

(define (subvector-move-right! vector1 start1 end1 vector2 start2)
  (define (loop index1 index2)
    (if (<= start1 index1)
	(begin (vector-set! vector2 index2 (vector-ref vector1 index1))
	       (loop (-1+ index1) (-1+ index2)))))
  (check-subvector vector1 start1 end1)
  (check-target vector2 start2)
  (loop (-1+ end1) (-1+ (+ start2 (- end1 start1)))))

(define (subvector-move-left! vector1 start1 end1 vector2 start2)
  (define (loop index1 index2)
    (if (< index1 end1)
	(begin (vector-set! vector2 index2 (vector-ref vector1 index1))
	       (loop (1+ index1) (1+ index2)))))
  (check-subvector vector1 start1 end1)
  (check-target vector2 start2)
  (loop start1 start2))

(define (subvector-fill! vector start end value)
  (define (loop index)
    (if (< index end)
	(begin (vector-set! vector index value)
	       (loop (1+ index)))))
  (check-subvector vector start end)
  (loop start))

)

;;; Nonstandard Procedures

(define (vector-copy vector)
  (let ((length (vector-length vector)))
    (let ((new-vector (make-vector length)))
      (subvector-move-right! vector 0 length new-vector 0)
      new-vector)))

(define (make-initialized-vector length initialization)
  (let ((vector (make-vector length)))
    (define (loop n)
      (if (= n length)
	  vector
	  (begin (vector-set! vector n (initialization n))
		 (loop (1+ n)))))
    (loop 0)))

(define (vector-map vector procedure)
  (let ((length (vector-length vector)))
    (if (zero? length)
	vector
	(let ((result (make-vector length)))
	  (define (loop i)
	    (vector-set! result i (procedure (vector-ref vector i)))
	    (if (zero? i)
		result
		(loop (-1+ i))))
	  (loop (-1+ length))))))

(define (vector-grow vector length)
  (let ((new-vector (make-vector length)))
    (subvector-move-right! vector 0 (vector-length vector) new-vector 0)
    new-vector))

(define (vector-first vector) (vector-ref vector 0))
(define (vector-second vector) (vector-ref vector 1))
(define (vector-third vector) (vector-ref vector 2))
(define (vector-fourth vector) (vector-ref vector 3))
(define (vector-fifth vector) (vector-ref vector 4))
(define (vector-sixth vector) (vector-ref vector 5))
(define (vector-seventh vector) (vector-ref vector 6))
(define (vector-eighth vector) (vector-ref vector 7))