#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/rulflo.scm,v 4.33 1991/10/25 12:29:54 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Flonum rules

(declare (usual-integrations))

(define (flonum-source! register)
  (float-register->fpr (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (float-register->fpr (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (float-register->fpr (allocate-temporary-register! 'FLOAT)))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (flonum-source! source))
	(temp (standard-temporary!)))
    (let ((target (standard-target! target)))
      (LAP ; (STW () 0 (OFFSET 0 0 21))	; make heap parsable forwards
	   (DEPI () #b100 31 3 21)	; quad align
	   (COPY () 21 ,target)
	   ,@(deposit-type (ucode-type flonum) target)
	   ,@(load-non-pointer (ucode-type manifest-nm-vector) 2 temp)
	   (STWM () ,temp (OFFSET 4 0 21))
	   (FSTDS (MA) ,source (OFFSET 8 0 21))))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-move-to-temporary! source)))
    (LAP ,@(object->address source)
	 (FLDDS () (OFFSET 4 0 ,source) ,(flonum-target! target)))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  (let ((source (flonum-source! source)))
    ((flonum-1-arg/operator operation) (flonum-target! target) source)))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

;;; Notice the weird ,', syntax here.
;;; If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name opcode)
	 `(define-arithmetic-method ',primitive-name flonum-methods/1-arg
	    (lambda (target source)
	      (LAP (,opcode (DBL) ,',source ,',target)))))))
  (define-flonum-operation flonum-abs FABS)
  (define-flonum-operation flonum-sqrt FSQRT)
  (define-flonum-operation flonum-round FRND))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    #|
    ;; No zero on the floating-point co-processor.  Need to create one.
    (let ((temp (if (= target source) (flonum-temporary!) target)))
      (LAP (FSUB (DBL) ,temp ,temp ,temp)
	   (FSUB (DBL) ,temp ,source ,target)))
    |#
    ;; The status register (fr0) reads as 0 for non-store instructions.
    (LAP (FSUB (DBL) 0 ,source ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  (let ((source1 (flonum-source! source1))
	(source2 (flonum-source! source2)))
    ((flonum-2-args/operator operation) (flonum-target! target)
					source1
					source2)))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name opcode)
	 `(define-arithmetic-method ',primitive-name flonum-methods/2-args
	    (lambda (target source1 source2)
	      (LAP (,opcode (DBL) ,',source1 ,',source2 ,',target)))))))
  (define-flonum-operation flonum-add fadd)
  (define-flonum-operation flonum-subtract fsub)
  (define-flonum-operation flonum-multiply fmpy)
  (define-flonum-operation flonum-divide fdiv)
  (define-flonum-operation flonum-remainder frem))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  #|
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((temp (flonum-temporary!)))
    (LAP (FSUB (DBL) ,temp ,temp ,temp)
	 ,@(flonum-compare
	    (case predicate
	      ((FLONUM-ZERO?) '=)
	      ((FLONUM-NEGATIVE?) '<)
	      ((FLONUM-POSITIVE?) '>)
	      (else (error "unknown flonum predicate" predicate)))
	    (flonum-source! source)
	    temp)))
  |#
  ;; The status register (fr0) reads as 0 for non-store instructions.
  (flonum-compare (case predicate
		    ((FLONUM-ZERO?) '=)
		    ((FLONUM-NEGATIVE?) '<)
		    ((FLONUM-POSITIVE?) '>)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source)
		  0))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (flonum-compare (case predicate
		    ((FLONUM-EQUAL?) '=)
		    ((FLONUM-LESS?) '<)
		    ((FLONUM-GREATER?) '>)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source1)
		  (flonum-source! source2)))

(define (flonum-compare cc r1 r2)
  (set-current-branches!
   (lambda (label)
     (LAP (B (N) (@PCR ,label))))
   (lambda (label)
     (LAP (SKIP (TR))
	  (B (N) (@PCR ,label)))))
  (LAP (FCMP (,(invert-condition cc) DBL) ,r1 ,r2)
       (FTEST ())))