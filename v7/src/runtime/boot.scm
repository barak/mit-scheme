;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/boot.scm,v 13.43 1987/04/17 00:58:33 cph Rel $
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
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Boot Utilities

(declare (usual-integrations))

;;; The utilities in this file are the first thing loaded into the
;;; world after the type tables.  They can't depend on anything else
;;; except those tables.

;;;; Primitive Operators

(let-syntax ((define-global-primitives
	      (macro names
		`(BEGIN
		  ,@(map (lambda (name)
			   `(DEFINE ,name ,(make-primitive-procedure name)))
			 names)))))
  (define-global-primitives
   SCODE-EVAL FORCE WITH-THREADED-CONTINUATION
   SET-INTERRUPT-ENABLES! WITH-INTERRUPTS-REDUCED
   WITH-INTERRUPT-MASK
   GET-FIXED-OBJECTS-VECTOR WITH-HISTORY-DISABLED
   PRIMITIVE-PROCEDURE-ARITY NOT FALSE?
   UNSNAP-LINKS!

   ;; Environment
   LEXICAL-REFERENCE LEXICAL-ASSIGNMENT LOCAL-ASSIGNMENT
   LEXICAL-UNASSIGNED? LEXICAL-UNBOUND? LEXICAL-UNREFERENCEABLE?

   ;; Pointers
   EQ?
   PRIMITIVE-SET-TYPE MAKE-NON-POINTER-OBJECT
   PRIMITIVE-TYPE? PRIMITIVE-TYPE PRIMITIVE-DATUM
   OBJECT-DANGEROUS? MAKE-OBJECT-SAFE MAKE-OBJECT-DANGEROUS

   ;; List Operations
   ;; (these appear here for the time being because the compiler
   ;; couldn't handle the `in-package' required to put them in
   ;; `list.scm'.  They should be moved back when that is fixed.
   CONS PAIR? NULL? LENGTH CAR CDR SET-CAR! SET-CDR!
   GENERAL-CAR-CDR MEMQ ASSQ

   ;; System Compound Datatypes
   MAKE-CELL CELL? CELL-CONTENTS SET-CELL-CONTENTS!

   SYSTEM-PAIR-CONS SYSTEM-PAIR?
   SYSTEM-PAIR-CAR SYSTEM-PAIR-SET-CAR!
   SYSTEM-PAIR-CDR SYSTEM-PAIR-SET-CDR!

   SYSTEM-HUNK3-CXR0 SYSTEM-HUNK3-SET-CXR0!
   SYSTEM-HUNK3-CXR1 SYSTEM-HUNK3-SET-CXR1!
   SYSTEM-HUNK3-CXR2 SYSTEM-HUNK3-SET-CXR2!

   SYSTEM-LIST-TO-VECTOR SYSTEM-SUBVECTOR-TO-LIST SYSTEM-VECTOR?
   SYSTEM-VECTOR-SIZE SYSTEM-VECTOR-REF SYSTEM-VECTOR-SET!
   )
;;; end of DEFINE-GLOBAL-PRIMITIVES scope.
)

;;;; Potpourri

(define *the-non-printing-object* '(*THE-NON-PRINTING-OBJECT*))
(define (identity-procedure x) x)
(define false #F)
(define true #T)

(define (null-procedure . args) '())
(define (false-procedure . args) #F)
(define (true-procedure . args) #T)

(define (without-interrupts thunk)
  (with-interrupts-reduced interrupt-mask-gc-ok
    (lambda (old-mask)
      (thunk))))

(define apply
  (let ((primitive (make-primitive-procedure 'APPLY)))
    (named-lambda (apply f . args)
      (primitive f
		 (if (null? args)
		     '()
		     (let loop
			 ((first-element (car args))
			  (rest-elements (cdr args)))
		       (if (null? rest-elements)
			   first-element
			   (cons first-element
				 (loop (car rest-elements)
				       (cdr rest-elements))))))))))

(define system-hunk3-cons
  (let ((hunk3-cons (make-primitive-procedure 'HUNK3-CONS)))
    (named-lambda (system-hunk3-cons type cxr0 cxr1 cxr2)
      (primitive-set-type type (hunk3-cons cxr0 cxr1 cxr2)))))

(define (symbol-hash symbol)
  (string-hash (symbol->string symbol)))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define (boolean? object)
  (or (eq? object #F)
      (eq? object #T)))