#| -*-Scheme-*-

$Id: rulflo.scm,v 1.27 2003/02/14 18:28:03 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; LAP Generation Rules: Flonum rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;; ****
;; Missing: 2 argument operations and predicates with non-trivial
;; constant arguments.
;; Also missing with (OBJECT->FLOAT (REGISTER ...)) operands.
;; ****

(define (flonum-source! register)
  (floreg->sti (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (floreg->sti (allocate-alias-register! pseudo-register 'FLOAT)))

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
		 (LAP (MOV W ,target
			   ,(offset-reference regnum:regs-pointer off))
		      (MOV W ,temp
			   ,(offset-reference regnum:regs-pointer (1+ off)))
		      (MOV W (@RO B ,regnum:free-pointer 4) ,target)
		      (MOV W (@RO B ,regnum:free-pointer 8) ,temp)))
	       (store-float (floreg->sti source)
			    (INST-EA (@RO B ,regnum:free-pointer 4))))
	 (LEA ,target
	      (@RO UW ,regnum:free-pointer
		   ,(make-non-pointer-literal (ucode-type flonum) 0)))
	 (ADD W (R ,regnum:free-pointer) (& 12)))))

#|
(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (move-to-temporary-register! source 'GENERAL))
	 (target (flonum-target! target)))
    (LAP ,@(object->address (register-reference source))
	 ,@(load-float (INST-EA (@RO B ,source 4)) target))))
|#

(define-rule statement
  ;; Convert a flonum object to a floating-point number.  Unlike the
  ;; version above which has an implicits OBJECT->ADDRESS, this one
  ;; uses the addressing mode to remove the type-code.  Saves a cycle
  ;; and maybe a register spill if SOURCE is live after instruction.
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (source-register source))
	 (target (flonum-target! target)))
    (object->float source target)))

(define (object->float source-register target)
  (let ((untagging+offset
	 (- 4 (make-non-pointer-literal (ucode-type flonum) 0))))
    (load-float (INST-EA (@RO W ,source-register ,untagging+offset)) target)))

;;;; Floating-point vector support.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:simple-float-offset?))
  (let* ((source (float-offset->reference! expression))
	 (target (flonum-target! target)))
    (load-float source target)))

(define-rule statement
  (ASSIGN (? expression rtl:simple-float-offset?) (REGISTER (? source)))
  (let ((source (flonum-source! source))
	(target (float-offset->reference! expression)))
    (store-float source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? expression rtl:detagged-float-offset?))
  (with-detagged-float-location expression
    (lambda (temp)
      (load-float temp target))))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-float-offset?)
	  (REGISTER (? source)))
  (with-detagged-float-location expression
    (lambda (temp)
      (store-float (flonum-source! source) temp))))

(define (with-detagged-float-location rtl-expression recvr)
  ;; Never needs to protect a register because it is a float register!
  (with-decoded-detagged-float-offset rtl-expression
    (lambda (base index w-offset)
      (with-indexed-address base index 8 (* 4 w-offset) false recvr))))

(define (rtl:detagged-float-offset? expression)
  (and (rtl:float-offset? expression)
       (let ((base (rtl:float-offset-base expression))
	     (offset (rtl:float-offset-offset expression)))
	 (and (rtl:offset-address? base)
	      (rtl:machine-constant? (rtl:offset-address-offset base))
	      (rtl:detagged-index? (rtl:offset-address-base base)
				   offset)))
       expression))

(define (with-decoded-detagged-float-offset expression recvr)
  (let ((base (rtl:float-offset-base expression))
	(index (rtl:float-offset-offset expression)))
    (let ((base* (rtl:offset-address-base base)))
      (recvr (rtl:register-number (if (rtl:register? base*)
				      base*
				      (rtl:object->address-expression base*)))
	     (rtl:register-number (if (rtl:register? index)
				      index
				      (rtl:object->datum-expression index)))
	     (rtl:machine-constant-value (rtl:offset-address-offset base))))))

(define (load-float ea sti)
  (LAP (FLD D ,ea)
       (FSTP (ST ,(1+ sti)))))

(define (store-float sti ea)
  (if (zero? sti)
      (LAP (FST D ,ea))
      (LAP (FLD (ST ,sti))
	   (FSTP D ,ea))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  ((flonum-1-arg/operator operation) target source))

(define ((flonum-unary-operation/general operate) target source)
  (define (default)
    (let* ((source (flonum-source! source))
	   (target (flonum-target! target)))
      (operate target source)))
  ;; Attempt to reuse source for target if it is in ST(0).
  ;; Otherwise we will target ST(0) by sorting the machine registers.
  (cond ((and (pseudo-register? target) (pseudo-register? source)
	      (eqv? fr0 (pseudo-register-alias *register-map* 'FLOAT source)))
	 (reuse-pseudo-register-alias
	  source 'FLOAT
	  (lambda (alias)
	    (let* ((sti (floreg->sti alias)))
	      (delete-register! alias)
	      (delete-dead-registers!)
	      (add-pseudo-register-alias! target alias)
	      (operate sti sti)))
	  default))
	(else (default))))

'(define ((flonum-unary-operation/general operate) target source)
  (define (default)
    (let* ((source (flonum-source! source))
	   (target (flonum-target! target)))
      (operate target source)))
  ;; Attempt to reuse source for target.  This works well when the
  ;; source is ST(0).  We try to arrange this by sorting the registers
  ;; to give allocation preference to ST(0).
  (cond ((pseudo-register? target)
	 (reuse-pseudo-register-alias
	  source 'FLOAT
	  (lambda (alias)
	    (let* ((sti (floreg->sti alias)))
	      (delete-register! alias)
	      (delete-dead-registers!)
	      (add-pseudo-register-alias! target alias)
	      (operate sti sti)))
	  default))
	(else (default))))

'(define ((flonum-unary-operation/general operate) target source)
  (define (default)
    (let* ((source (flonum-source! source))
	   (target (flonum-target! target)))
      (operate target source)))
  ;; Attempt to reuse source for target.  This works well when the
  ;; source is ST(0).  We try to arrange this by sorting the registers
  ;; to give allocation preference to ST(0).
  (cond ((pseudo-register? target)
	 (let ((alias
		(and (dead-register? source)
		     (pseudo-register-alias *register-map* 'FLOAT source))))
	   (if alias
	       (default)))
	
	(reuse-pseudo-register-alias
	  source 'FLOAT
	  (lambda (alias)
	    (let* ((sti (floreg->sti alias)))
		(delete-register! alias)
		(delete-dead-registers!)
		(add-pseudo-register-alias! target alias)
		(operate sti sti)))
	  default))
	(else (default))))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

;;; Notice the weird ,', syntax here.
;;; If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((primitive-name (cadr form))
		(opcode (caddr form)))
	    `(define-arithmetic-method ',primitive-name flonum-methods/1-arg
	       (flonum-unary-operation/general
		(lambda (target source)
		  (if (and (zero? target) (zero? source))
		      (LAP (,opcode))
		      (LAP (FLD (ST ,', source))
			   (,opcode)
			   (FSTP (ST ,',(1+ target)))))))))))))
  (define-flonum-operation FLONUM-NEGATE FCHS)
  (define-flonum-operation FLONUM-ABS FABS)
  (define-flonum-operation FLONUM-SIN FSIN)
  (define-flonum-operation FLONUM-COS FCOS)
  (define-flonum-operation FLONUM-SQRT FSQRT)
  (define-flonum-operation FLONUM-ROUND FRNDINT))

;; These (and FLONUM-ROUND above) presume that the default rounding mode
;; is round-to-nearest/even

(define (define-rounding prim-name mode)
  (define-arithmetic-method prim-name flonum-methods/1-arg
    (flonum-unary-operation/general
     (lambda (target source)
       (let ((temp (temporary-register-reference)))
	 (LAP (FSTCW (@R ,regnum:free-pointer))
	      ,@(if (and (zero? target) (zero? source))
		    (LAP)
		    (LAP (FLD (ST ,source))))
	      (MOV B ,temp (@RO B ,regnum:free-pointer 1))
	      (OR B (@RO B ,regnum:free-pointer 1) (&U ,mode))
	      (FNLDCW (@R ,regnum:free-pointer))
	      (FRNDINT)
	      (MOV B (@RO B ,regnum:free-pointer 1) ,temp)
	      ,@(if (and (zero? target) (zero? source))
		    (LAP)
		    (LAP (FSTP (ST ,(1+ target)))))
	      (FNLDCW (@R ,regnum:free-pointer))))))))

(define-rounding 'FLONUM-CEILING #x08)
(define-rounding 'FLONUM-FLOOR #x04)
(define-rounding 'FLONUM-TRUNCATE #x0c)

;; This is used in order to avoid using two stack locations for
;; the remainder unary operations.

(define ((flonum-unary-operation/stack-top operate) target source)
  (define (finish source->top)
    ;; Perhaps this can be improved?
    (rtl-target:=machine-register! target fr0)
    (LAP ,@source->top
	 ,@(operate)))

  (if (or (machine-register? source)
	  (not (is-alias-for-register? fr0 source))
	  (not (dead-register? source)))
      (finish (load-machine-register! source fr0))
      (begin
	(delete-dead-registers!)
	(finish (LAP)))))

(define-arithmetic-method 'FLONUM-LOG flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLDLN2)
	  (FLD (ST ,(1+ source)))
	  (FYL2X)
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FLDLN2)
	  (FXCH (ST 0) (ST 1))
	  (FYL2X)))))

(define-arithmetic-method 'FLONUM-EXP flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLD (ST ,source))
	  (FLDL2E)
	  (FMULP (ST 1) (ST 0))
	  (F2XM1)
	  (FLD1)
	  (FADDP (ST 1) (ST 0))
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FLDL2E)
	  (FMULP (ST 1) (ST 0))
	  (F2XM1)
	  (FLD1)
	  (FADDP (ST 1) (ST 0))))))

(define-arithmetic-method 'FLONUM-TAN flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLD (ST ,source))
	  (FPTAN)
	  (FSTP (ST 0))			; FPOP
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FPTAN)
	  (FSTP (ST 0))			; FPOP
	  ))))

(define-arithmetic-method 'FLONUM-ATAN flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLD (ST ,source))
	  (FLD1)
	  (FPATAN)
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FLD1)
	  (FPATAN)))))

;; For now, these preserve values in memory
;; in order to avoid flushing a stack location.

(define-arithmetic-method 'FLONUM-ACOS flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLD (ST ,source))
	  (FMUL (ST 0) (ST 0))
	  (FLD1)
	  (F%SUBP (ST 1) (ST 0))
	  (FSQRT)
	  (FLD (ST ,(1+ source)))
	  (FPATAN)
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FST D (@R ,regnum:free-pointer))
	  (FMUL (ST 0) (ST 0))
	  (FLD1)
	  (F%SUBP (ST 1) (ST 0))
	  (FSQRT)
	  (FLD D (@R ,regnum:free-pointer))
	  (FPATAN)))))

(define-arithmetic-method 'FLONUM-ASIN flonum-methods/1-arg
  (flonum-unary-operation/stack-top
   (lambda ()
     #|
     (LAP (FLD (ST ,source))
	  (FMUL (ST 0) (ST 0))
	  (FLD1)
	  (F%SUBP (ST 1) (ST 0))
	  (FSQRT)
	  (FLD (ST ,(1+ source)))
	  (FXCH (ST 0) (ST 1))
	  (FPATAN)
	  (FSTP (ST ,(1+ target))))
     |#
     (LAP (FST D (@R ,regnum:free-pointer))
	  (FMUL (ST 0) (ST 0))
	  (FLD1)
	  (F%SUBP (ST 1) (ST 0))
	  (FSQRT)
	  (FLD D (@R ,regnum:free-pointer))
	  (FXCH (ST 0) (ST 1))
	  (FPATAN)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  ((flonum-2-args/operator operation) target source1 source2))

;; Binary instructions all use ST(0), and are of the forms
;;   Fop ST(0),ST(i)
;;   Fop ST(i),ST(0)
;;   FopP ST(i),ST(0)
;;   Fop ST(0),memory
;;
;; If possible, we like to target ST(0) since it is likely to be the
;; source of a subsequent operation.  Failing that, it is good to
;; reuse one of the source aliases.

(define ((flonum-binary-operation operate) target source1 source2)
  (define (default)
    (let* ((sti1 (flonum-source! source1))
	   (sti2 (flonum-source! source2)))
      (operate (flonum-target! target) sti1 sti2)))
  (define (try-reuse-1 if-cannot)
    (reuse-pseudo-register-alias
     source1 'FLOAT
     (lambda (alias1)
       (let* ((sti1 (floreg->sti alias1))
	      (sti2 (if (= source1 source2)
			sti1
			(flonum-source! source2))))
	 (delete-register! alias1)
	 (delete-dead-registers!)
	 (add-pseudo-register-alias! target alias1)
	 (operate sti1 sti1 sti2)))
     if-cannot))
  (define (try-reuse-2 if-cannot)
    (reuse-pseudo-register-alias
     source2 'FLOAT
     (lambda (alias2)
       (let* ((sti2 (floreg->sti alias2))
	      (sti1 (if (= source1 source2)
			sti2
			(flonum-source! source1))))
	 (delete-register! alias2)
	 (delete-dead-registers!)
	 (add-pseudo-register-alias! target alias2)
	 (operate sti2 sti1 sti2)))
     if-cannot))
  (cond ((pseudo-register? target)
	 (if (is-alias-for-register? fr0 source1)
	     (try-reuse-1 (lambda () (try-reuse-2 default)))
	     (try-reuse-2 (lambda () (try-reuse-1 default)))))
	((not (eq? (register-type target) 'FLOAT))
	 (error "flonum-2-args: Wrong type register" target 'FLOAT))
	(else (default))))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define (flonum-1-arg%1/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg%1))

(define flonum-methods/1-arg%1
  (list 'FLONUM-METHODS/1-ARG%1))

(define (flonum-1%1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1%1-arg))

(define flonum-methods/1%1-arg
  (list 'FLONUM-METHODS/1%1-ARG))

(define (binary-flonum-arithmetic? operation)
  (memq operation '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE)))

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((primitive-name (list-ref form 1))
		(op1%2 (list-ref form 2))
		(op1%2p (list-ref form 3))
		(op2%1 (list-ref form 4))
		(op2%1p (list-ref form 5)))
	    `(begin
	       (define-arithmetic-method ',primitive-name flonum-methods/2-args
		 (flonum-binary-operation
		  (lambda (target source1 source2)
		    (cond ((= target source1)
			   (cond ((zero? target)
				  (LAP (,op1%2 (ST 0) (ST ,',source2))))
				 ((zero? source2)
				  (LAP (,op2%1 (ST ,',target) (ST 0))))
				 (else
				  (LAP (FLD (ST ,',source2))
				       (,op2%1p (ST ,',(1+ target)) (ST 0))))))
			  ((= target source2)
			   (cond ((zero? target)
				  (LAP (,op2%1 (ST 0) (ST ,',source1))))
				 ((zero? source1)
				  (LAP (,op1%2 (ST ,',target) (ST 0))))
				 (else
				  (LAP (FLD (ST ,',source1))
				       (,op1%2p (ST ,',(1+ target)) (ST 0))))))
			  (else
			   (LAP (FLD (ST ,',source1))
				(,op1%2 (ST 0) (ST ,',(1+ source2)))
				(FSTP (ST ,',(1+ target)))))))))

	       (define-arithmetic-method ',primitive-name
		 flonum-methods/1%1-arg
		 (flonum-unary-operation/general
		  (lambda (target source)
		    (if (= source target)
			(LAP (FLD1)
			     (,op1%2p (ST ,',(1+ target)) (ST 0)))
			(LAP (FLD1)
			     (,op1%2 (ST 0) (ST ,',(1+ source)))
			     (FSTP (ST ,',(1+ target))))))))

	       (define-arithmetic-method ',primitive-name
		 flonum-methods/1-arg%1
		 (flonum-unary-operation/general
		  (lambda (target source)
		    (if (= source target)
			(LAP (FLD1)
			     (,op2%1p (ST ,',(1+ target)) (ST 0)))
			(LAP (FLD1)
			     (,op2%1 (ST 0) (ST ,',(1+ source)))
			     (FSTP (ST ,',(1+ target))))))))))))))

  (define-flonum-operation FLONUM-ADD FADD FADDP FADD FADDP)
  (define-flonum-operation FLONUM-SUBTRACT F%SUB F%SUBP F%SUBR F%SUBPR)
  (define-flonum-operation FLONUM-MULTIPLY FMUL FMULP FMUL FMULP)
  (define-flonum-operation FLONUM-DIVIDE F%DIV F%DIVP F%DIVR F%DIVPR))

(define-arithmetic-method 'FLONUM-ATAN2 flonum-methods/2-args
  (lambda (target source1 source2)
    (if (and (not (machine-register? source1))
	     (is-alias-for-register? fr0 source1)
	     (dead-register? source1))
	(let ((source2 (flonum-source! source2)))
	  (delete-dead-registers!)
	  (rtl-target:=machine-register! target fr0)
	  (LAP (FLD (ST ,source2))
	       (FPATAN)))
	(begin
	  (prefix-instructions! (load-machine-register! source1 fr0))
	  (need-register! fr0)
	  (let ((source2
		 (if (= source2 source1) fr0 (flonum-source! source2))))
	    (delete-dead-registers!)
	    (rtl-target:=machine-register! target fr0)
	    (LAP (FLD (ST ,source2))
		 (FPATAN)))))))

(define-arithmetic-method 'FLONUM-REMAINDER flonum-methods/2-args
  (flonum-binary-operation
   (lambda (target source1 source2)
     (if (zero? source2)
	 (LAP (FLD (ST ,source1))
	      (FPREM1)
	      (FSTP (ST ,(1+ target))))
	 #|
	 ;; This sequence is one cycle shorter than the one below,
	 ;; but needs two spare stack locations instead of one.
	 ;; Since FPREM1 is a variable, very slow instruction,
	 ;; the difference in time will hardly be noticeable
	 ;; but the availability of an extra "register" may be.
	 (LAP (FLD (ST ,source2))
	      (FLD (ST ,source1))
	      (FPREM1)
	      (FSTP (ST ,(+ target 2)))
	      (FSTP (ST 0)))		; FPOP
	 |#
	 (LAP (FXCH (ST 0) (ST ,source2))
	      (FLD (ST ,(if (zero? source1) source2 source1)))
	      (FPREM1)
	      (FSTP (ST ,(1+ (if (= target source2)
				 0
				 target))))
	      (FXCH (ST 0) (ST ,source2)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS FLONUM-SUBTRACT
			 (OBJECT->FLOAT (CONSTANT 0.))
			 (REGISTER (? source))
			 (? overflow?)))
  overflow?				;ignore
  ((flonum-unary-operation/general
    (lambda (target source)
      (if (and (zero? target) (zero? source))
	  (LAP (FCHS))
	  (LAP (FLD (ST ,source))
	       (FCHS)
	       (FSTP (ST ,(1+ target)))))))
   target source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FLOAT (CONSTANT 1.))
			 (? overflow?)))
  (QUALIFIER (binary-flonum-arithmetic? operation))
  overflow?				;ignore
  ((flonum-1-arg%1/operator operation) target source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (OBJECT->FLOAT (CONSTANT 1.))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (binary-flonum-arithmetic? operation))
  overflow?				;ignore
  ((flonum-1%1-arg/operator operation) target source))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (flonum-compare-zero predicate source))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (let* ((st1 (flonum-source! source1))
	 (st2 (flonum-source! source2)))
    (cond ((zero? st1)
	   (flonum-branch! predicate
			   (LAP (FCOM (ST 0) (ST ,st2)))))
	  ((zero? st2)
	   (flonum-branch! (commute-flonum-predicate predicate)
			   (LAP (FCOM (ST 0) (ST ,st1)))))
	  (else
	   (flonum-branch! predicate
			   (LAP (FLD (ST ,st1))
				(FCOMP (ST 0) (ST ,(1+ st2)))))))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FLOAT (CONSTANT 0.)))
  (flonum-compare-zero predicate source))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FLOAT (CONSTANT 0.))
		      (REGISTER (? source)))
  (flonum-compare-zero (commute-flonum-predicate predicate) source))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FLOAT (CONSTANT 1.)))
  (flonum-compare-one predicate source))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FLOAT (CONSTANT 1.))
		      (REGISTER (? source)))
  (flonum-compare-one (commute-flonum-predicate predicate) source))

(define (flonum-compare-zero predicate source)
  (let ((sti (flonum-source! source)))
    (if (zero? sti)
	(flonum-branch! predicate
			(LAP (FTST)))
	(flonum-branch! (commute-flonum-predicate predicate)
			(LAP (FLDZ)
			     (FCOMP (ST 0) (ST ,(1+ sti))))))))

(define (flonum-compare-one predicate source)
  (let ((sti (flonum-source! source)))
    (flonum-branch! (commute-flonum-predicate predicate)
		    (LAP (FLD1)
			 (FCOMP (ST 0) (ST ,(1+ sti)))))))

(define (commute-flonum-predicate pred)
  (case pred
    ((FLONUM-EQUAL? FLONUM-ZERO?) 'FLONUM-EQUAL?)
    ((FLONUM-LESS? FLONUM-NEGATIVE?) 'FLONUM-GREATER?)
    ((FLONUM-GREATER? FLONUM-POSITIVE?) 'FLONUM-LESS?)
    (else
     (error "commute-flonum-predicate: Unknown predicate" pred))))

(define (flonum-branch! predicate prefix)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-ZERO?)
     (set-current-branches! (lambda (label)
			      (let ((unordered (generate-label 'UNORDERED)))
				(LAP (JP (@PCR ,unordered))
				     (JE (@PCR ,label))
				     (LABEL ,unordered))))
			    (lambda (label)
			      (LAP (JNE (@PCR ,label))
				   (JP (@PCR ,label))))))
    ((FLONUM-LESS? FLONUM-NEGATIVE?)
     (set-current-branches! (lambda (label)
			      (let ((unordered (generate-label 'UNORDERED)))
				(LAP (JP (@PCR ,unordered))
				     (JB (@PCR ,label))
				     (LABEL ,unordered))))
			    (lambda (label)
			      (LAP (JAE (@PCR ,label))
				   (JP (@PCR ,label))))))
    ((FLONUM-GREATER? FLONUM-POSITIVE?)
     (set-current-branches! (lambda (label)
			      (LAP (JA (@PCR ,label))))
			    (lambda (label)
			      (LAP (JBE (@PCR ,label))))))
    (else
     (error "flonum-branch!: Unknown predicate" predicate)))
  (flush-register! eax)
  (LAP ,@prefix
       (FSTSW (R ,eax))
       (SAHF)))

;; This is endianness dependent!

(define (flonum-value->data-decl value)
  (let ((high (make-bit-string 32 false))
	(low (make-bit-string 32 false)))
    (read-bits! value 32 high)
    (read-bits! value 64 low)
    (LAP ,@(lap:comment `(FLOAT ,value))
	 (LONG U ,(bit-string->unsigned-integer high))
	 (LONG U ,(bit-string->unsigned-integer low)))))

(define (flo:32-bit-representation-exact? value)
  ;; Returns unsigned long representation if 32 bit representation
  ;; exists, i.e. if all `1' significant mantissa bits fit in the 32
  ;; bit format and the exponent is within range.
  (let ((mant-diff (make-bit-string (- 52 23) false)))
    (read-bits! value (+ 32 0) mant-diff)
    (and (bit-string-zero? mant-diff)
	 (let ((expt64 (make-bit-string 11 false)))
	   (read-bits! value (+ 32 52) expt64)
	   (let ((expt (- (bit-string->unsigned-integer expt64) 1022)))
	     (and (<= -127 expt 127)
		  (let ((sign (make-bit-string 1  false))
			(mant32 (make-bit-string 23 false)))
		    (read-bits! value (+ 32 52 11) sign)
		    (read-bits! value (+ 32 52 -23) mant32)
		    (bit-string->unsigned-integer
		     (bit-string-append
		      (bit-string-append
		       mant32
		       (unsigned-integer->bit-string 8 (+ 126 expt)))
		      sign)))))))))

(define (flonum->label value block-name alignment offset data)
  (let* ((block
	  (or (find-extra-code-block block-name)
	      (let ((block (declare-extra-code-block! block-name
						      'ANYWHERE
						      '())))
		(add-extra-code!
		 block
		 (LAP (PADDING ,offset ,alignment ,padding-string)))
		block)))
	 (pairs (extra-code-block/xtra block))
	 (place (assoc value pairs)))
    (if place
	(cdr place)
	(let ((label (generate-label block-name)))
	  (set-extra-code-block/xtra!
	   block
	   (cons (cons value label) pairs))
	  (add-extra-code! block
			   (LAP (LABEL ,label)
				,@data))
	  label))))

(define (double-flonum->label fp-value)
  (flonum->label fp-value 'DOUBLE-FLOATS 8 0
		 (flonum-value->data-decl fp-value)))

(define (single-flonum->label fp-value)
  (flonum->label fp-value 'SINGLE-FLOATS 4 0
		 (LAP ,@(lap:comment `(SINGLE-FLOAT ,fp-value))
		      (LONG U ,(flo:32-bit-representation-exact? fp-value)))))
				     
(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (CONSTANT (? fp-value))))
  (cond ((not (flo:flonum? fp-value))
	 (error "OBJECT->FLOAT: Not a floating-point value" fp-value))
	((flo:= fp-value 0.0)
	 (let ((target (flonum-target! target)))
	   (LAP (FLDZ)
		(FSTP (ST ,(1+ target))))))
	((flo:= fp-value 1.0)
	 (let ((target (flonum-target! target)))
	   (LAP (FLD1)
		(FSTP (ST ,(1+ target))))))
	(compiler:cross-compiling?
	 (let* ((temp (allocate-temporary-register! 'GENERAL))
		(target (flonum-target! target)))
	   (LAP ,@(load-constant (register-reference temp) fp-value)
		,@(object->float temp target))))
	(else
	 (let ((target (flonum-target! target)))
	   (with-pcr-float fp-value
	      (lambda (ea size)
		(LAP (FLD ,size ,ea)
		     (FSTP (ST ,(1+ target))))))))))

(define (with-pcr-float fp-value receiver)
  (define (generate-ea label-expr size)
    (with-pc
     (lambda (pc-label pc-register)
       (receiver (INST-EA (@RO W ,pc-register (- ,label-expr ,pc-label)))
		 size))))
  (if (flo:32-bit-representation-exact? fp-value)
      (generate-ea (single-flonum->label fp-value) 'S)
      (generate-ea (double-flonum->label fp-value) 'D)))
