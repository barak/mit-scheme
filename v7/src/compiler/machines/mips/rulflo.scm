#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/rulflo.scm,v 1.2 1990/07/22 20:28:36 jinx Rel $
$MC68020-Header: rules1.scm,v 4.33 90/05/03 15:17:28 GMT jinx Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

(define (store-flonum offset base source)
  (fp-store-doubleword offset base
		       (fpr->float-register source)))

(define (load-flonum offset base target)
  (fp-load-doubleword offset base
		      (fpr->float-register target)
		      #t))		; Output NOP

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (flonum-source! source)))
    (let ((target (standard-target! target)))
      (LAP
       ; (SW 0 (OFFSET 0 ,regnum:free))	; make heap parsable forwards
       (SRL ,regnum:free ,regnum:free 3)
       (SLL ,regnum:free ,regnum:free 3)
       (ORI ,regnum:free ,regnum:free #b100) ; Align to odd quad byte
       (ADD ,target 0 ,regnum:free)	; Result is this address
       ,@(deposit-type (ucode-type flonum) target)
       ,@(load-non-pointer
	  (ucode-type manifest-nm-vector) 2 regnum:assembler-temp) 
       (SW ,regnum:assembler-temp (OFFSET 0 ,regnum:free))
       ,@(store-flonum 4 regnum:free source)
       (ADDI ,regnum:free ,regnum:free 12)))))

(define-rule statement
  ;; convert a flonum object address to a floating-point number
  (ASSIGN (REGISTER (? target)) (@ADDRESS->FLOAT (REGISTER (? source))))
  (let ((source (standard-source! source)))
    (let ((target (flonum-target! target)))
      (load-flonum 4 source target))))

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
	      (LAP (,opcode DOUBLE ,',target ,',source)))))))
  (define-flonum-operation flonum-abs FABS)
  (define-flonum-operation flonum-negate FNEG))

; Well, I thought this would work, but the fine print in the manual
; says that CVT.D only works with a source type of single precision.
; *Sigh*

; (define-arithmetic-method 'FLONUM-ROUND flonum-methods/1-arg
;   (lambda (target source)
;     (let ((temp (standard-temporary!)))
;       (LAP (CFC1 ,regnum:assembler-temp 31)      ; Status register
; 	   (ORI  ,temp ,regnum:assembler-temp 3) ; Rounding Mode <-
; 	   (XORI ,temp ,temp 3)			 ;; 0 (nearest)
; 	   (CTC1 ,temp 31)			 ; Store mode back
; 	   (CVT.D DOUBLE ,target ,source)	 ; Move & round
; 	   (CTC1 ,regnum:assembler-temp 31)))))  ; Restore status
 
; (define-arithmetic-method 'FLONUM-TRUNCATE flonum-methods/1-arg
;   (lambda (target source)
;     (let ((temp (standard-temporary!)))
;       (LAP (CFC1 ,regnum:assembler-temp 31)      ; Status register
; 	   (ORI ,temp ,regnum:assembler-temp 3)	 ; Rounding Mode <-
; 	   (XORI  ,temp ,temp 2)                 ;; 1 (toward zero)
; 	   (CTC1 ,temp 31)			 ; Store mode back
; 	   (CVT.D DOUBLE ,target ,source)	 ; Move & round
; 	   (CTC1 ,regnum:assembler-temp 31)))))	 ; Restore status

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
	      (LAP (,opcode DOUBLE ,',target ,',source1 ,',source2)))))))
  (define-flonum-operation flonum-add FADD)
  (define-flonum-operation flonum-subtract FSUB)
  (define-flonum-operation flonum-multiply FMUL)
  (define-flonum-operation flonum-divide FDIV)
;  (define-flonum-operation flonum-remainder frem)
  )

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((temp (flonum-temporary!))
	(source (flonum-source! source)))
    (LAP (FSUB DOUBLE ,temp ,source ,source)
	 ,@(flonum-compare
	    (case predicate
	      ((FLONUM-ZERO?) 'C.EQ)
	      ((FLONUM-NEGATIVE?) 'C.LT)
	      ((FLONUM-POSITIVE?) 'C.GT)
	      (else (error "unknown flonum predicate" predicate)))
	    source temp))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (flonum-compare (case predicate
		    ((FLONUM-EQUAL?) 'C.EQ)
		    ((FLONUM-LESS?) 'C.LT)
		    ((FLONUM-GREATER?) 'C.GT)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source1)
		  (flonum-source! source2)))

(define (flonum-compare cc r1 r2)
  (set-current-branches!
   (lambda (label)
     (LAP (BC1T (@PCR ,label)) (NOP)))
   (lambda (label)
     (LAP (BC1F (@PCR ,label)) (NOP))))
  (if (eq? cc 'C.GT)
      (LAP (C.LT DOUBLE ,r2 ,r1) (NOP))
      (LAP (,cc DOUBLE ,r1 ,r2) (NOP))))