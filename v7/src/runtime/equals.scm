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

;;;; Equality

(declare (usual-integrations))

(let-syntax ((type?
	      ;; Use PRIMITIVE-TYPE? for everything because the
	      ;; compiler can optimize it well.
	      (macro (name object)
		`(PRIMITIVE-TYPE? ,(microcode-type name) ,object))))

(define (eqv? x y)
  ;; EQV? is officially supposed to work on booleans, characters, and
  ;; numbers specially, but it turns out that EQ? does the right thing
  ;; for everything but numbers, so we take advantage of that.
  (if (eq? x y)
      #T
      (and (primitive-type? (primitive-type x) y)
	   (or (type? big-fixnum y)
	       (type? big-flonum y))
	   (= x y))))

(define (equal? x y)
  (if (eq? x y)
      #T
      (and (primitive-type? (primitive-type x) y)
	   (cond ((or (type? big-fixnum y)
		      (type? big-flonum y))
		  (= x y))
		 ((type? list y)
		  (and (equal? (car x) (car y))
		       (equal? (cdr x) (cdr y))))
		 ((type? vector y)
		  (let ((size (vector-length x)))
		    (define (loop index)
		      (or (= index size)
			  (and (equal? (vector-ref x index)
				       (vector-ref y index))
			       (loop (1+ index)))))
		    (and (= size (vector-length y))
			 (loop 0))))
		 ((type? cell y)
		  (equal? (cell-contents x) (cell-contents y)))
		 ((type? character-string y)
		  (string=? x y))
		 ((type? vector-1b y)
		  (bit-string=? x y))
		 (else false)))))

