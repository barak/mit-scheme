#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rulflo.scm,v 1.2 1992/02/01 20:08:47 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/rules1.scm,v 4.36 1991/10/25 06:49:58 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-integrable (->sti reg)
  (- reg fr0))

(define (flonum-source! register)
  (->sti (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (->sti (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (allocate-temporary-register! 'FLOAT))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let* ((source (register-alias source 'FLOAT))
	 (target (target-register-reference target)))
    (LAP (MOV W (@R ,regnum:free-pointer)
	      (&U ,(make-non-pointer-literal
		    (ucode-type manifest-nm-vector)
		    2)))
	 ,@(if (not source)
	       ;; Value is in memory home
	       (let ((off (pseudo-register-offset source))
		     (temp (temporary-register-reference)))
		 (LAP (MOV W ,target (@RO ,regnum:regs-pointer ,off))
		      (MOV W ,temp (@RO ,regnum:regs-pointer ,(+ 4 off)))
		      (MOV W (@RO ,regnum:free-pointer 4) ,target)
		      (MOV W (@RO ,regnum:free-pointer 8) ,temp)))
	       (let ((sti (->sti source)))
		 (if (zero? sti)
		     (LAP (FST D (@RO ,regnum:free-pointer 4)))
		     (LAP (FLD D (ST ,(->sti source)))
			  (FSTP D (@RO ,regnum:free-pointer 4))))))
	 (LEA ,target
	      (@RO ,regnum:free-pointer
		   ,(make-non-pointer-literal (ucode-type flonum) 0)))
	 (ADD W (R ,regnum:free-pointer) (& 12)))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (move-to-temporary-register! source 'GENERAL))
	 (target (flonum-target! target)))
    (LAP ,@(object->address source)
	 (FLD D (@RO ,source 4))
	 (FSTP D (ST ,(1+ target))))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  (flonum-1-arg target source operation))

(define (flonum-1-arg target source operation)
  (let* ((source (flonum-source! source))
	 (target (flonum-target! target)))
    ((flonum-1-arg/operator operation) target source)))

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
	      (if (and (zero? target) (zero? source))
		  (,opcode)
		  (LAP (FLD D (ST ,', source))
		       (,opcode)
		       (FSTP D (ST ,',(1+ target))))))))))
  (define-flonum-operation flonum-negate FCHS)
  (define-flonum-operation flonum-abs FABS)
  (define-flonum-operation flonum-sin FSIN)
  (define-flonum-operation flonum-cos FCOS)
  (define-flonum-operation flonum-sqrt FSQRT)
  (define-flonum-operation flonum-round FRND))

;; **** Missing: ****
;; flonum-tan flonum-asin flonum-acos flonum-atan
;; flonum-exp flonum-log flonum-truncate
;; Most of the above can be done in a couple of instructions

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  ((flonum-2-args/operator operation) target source1 source2))

(define ((flonum-binary-operation operate) target source1 source2)
  (let ((default
	  (lambda ()
	    (let* ((sti1 (flonum-source! source1))
		   (sti2 (flonum-source! source2)))
	      (operate (flonum-target! target) sti1 sti2)))))
    (cond ((pseudo-register? target)
	   (reuse-pseudo-register-alias
	    source1 target-type
	    (lambda (alias)
	      (let* ((sti1 (->sti alias))
		     (sti2 (if (= source1 source2)
			       sti1
			       (flonum-source! source2))))
		(delete-register! alias)
		(delete-dead-registers!)
		(add-pseudo-register-alias! target alias)
		(operate sti1 sti1 sti2)))
	    (lambda ()
	      (reuse-pseudo-register-alias
	       source2 target-type
	       (lambda (alias2)
		 (let ((sti1 (flonum-source! source1))
		       (sti2 (->sti alias2)))
		   (delete-register! alias2)
		   (delete-dead-registers!)
		   (add-pseudo-register-alias! target alias2)
		   (operate sti2 sti1 sti2)))
	       default))))
	  ((not (eq? target-type (register-type target)))
	   (error "flonum-2-args: Wrong type register"
		  target target-type))
	  (else
	   (default)))))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name op1%2 op1%2p op2%1 op2%1p)
	 `(define-arithmetic-method ',primitive-name flonum-methods/2-args
	    (flonum-binary-operation
	     (lambda (target source1 source2)
	       (cond ((= target source1)
		      (cond ((zero? target)
			     (LAP (,op1%2 D (ST) (ST ,',source2))))
			    ((zero? source2)
			     (LAP (,op2%1 D (ST ,',target) (ST))))
			    (else
			     (LAP (FLD D (ST ,',source2))
				  (,op2%1p D (ST ,',(1+ target)) (ST))))))
		     ((= target source2)
		      (cond ((zero? target)
			     (LAP (,op2%1 D (ST) (ST ,',source1))))
			    ((zero? source1)
			     (LAP (,op1%2 D (ST ,',target) (ST))))
			    (else
			     (LAP (FLD D (ST ,',source1))
				  (,op1%2p D (ST ,',(1+ target)) (ST))))))
		     (else
		      (LAP (FLD D (ST ,',source1))
			   (,op1%2 D (ST) (ST ,',(1+ source2)))
			   (FSTP D (ST ,',(1+ target))))))))))))
  (define-flonum-operation flonum-add fadd faddp fadd faddp)
  (define-flonum-operation flonum-subtract fsub fsubp fsubr fsubpr)
  (define-flonum-operation flonum-multiply fmul fmulp fmul fmulp)
  (define-flonum-operation flonum-divide fdiv fdivp fdivr fdivpr))

;; **** Missing: (define-flonum-operation flonum-remainder fprem1) ***

;;;; Flonum Predicates

;; **** Here ****

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