#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules1.scm,v 4.30 1989/12/05 20:52:00 jinx Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Data Transfers

(declare (usual-integrations))

;;;; Transfers to Registers

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (machine-register? target))
  (LAP (MOV L
	    ,(standard-register-reference source false true)
	    ,(register-reference target))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (LEA ,(indirect-reference! source offset) (A 7))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (increment-machine-register 15 n))

(define-rule statement
  (ASSIGN (REGISTER 12) (OFFSET-ADDRESS (REGISTER 15) (? offset)))
  (LAP (LEA (@AO 7 ,(* 4 offset)) (A 4))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (LEA ,(indirect-reference! source offset) (A 4))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? source))
  (let ((temp (move-to-temporary-register! source 'DATA)))
    (LAP (AND L ,mask-reference ,temp)
	 (MOV L ,temp (A 4)))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OBJECT->ADDRESS (POST-INCREMENT (REGISTER 15) 1)))
  (let ((temp (reference-temporary-register! 'DATA)))
    (LAP (MOV L (@A+ 7) ,temp)
	 (AND L ,mask-reference ,temp)
	 (MOV L ,temp (A 4)))))

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  However, it is
;;; necessary to derive the effective address of the source
;;; expression(s) before deleting the dead registers.  Otherwise any
;;; source expression containing dead registers might refer to aliases
;;; which have been reused.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (QUALIFIER (pseudo-word? target))
  (load-static-link target source n false))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (QUALIFIER (pseudo-word? target))
  (load-static-link target source n
    (lambda (target)
      (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define (load-static-link target source n suffix)
  (let ((non-reusable
	 (lambda ()
	   (let ((source (indirect-reference! source n)))
	     (delete-dead-registers!)
	     (if suffix
		 (let ((temp (reference-temporary-register! 'ADDRESS)))
		   (let ((target (reference-target-alias! target 'DATA)))
		     (LAP (LEA ,source ,temp)
			  (MOV L ,temp ,target)
			  ,@(suffix target))))
		 (LAP (LEA ,source
			   ,(reference-target-alias! target 'ADDRESS))))))))
    (if (machine-register? source)
	(non-reusable)
	(reuse-pseudo-register-alias! source 'DATA
	  (lambda (reusable-alias)
	    (delete-dead-registers!)
	    (add-pseudo-register-alias! target reusable-alias)
	    (LAP ,@(increment-machine-register reusable-alias n)
		 ,@(if suffix
		       (suffix (register-reference reusable-alias))
		       (LAP))))
	  non-reusable))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-constant source (standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@PCR ,(free-reference-label name))
	    ,(reference-target-alias! target 'ADDRESS))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@PCR ,(free-assignment-label name))
	    ,(reference-target-alias! target 'ADDRESS))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-word? target))
  (move-to-alias-register! source 'DATA target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-float? target))
  (move-to-alias-register! source 'FLOAT target)
  (LAP))

(define (convert-object/constant->register target constant conversion)
  (delete-dead-registers!)
  (let ((target (reference-target-alias! target 'DATA)))
    (if (non-pointer-object? constant)
	(LAP ,(load-non-pointer 0 (careful-object-datum constant) target))
	(LAP ,(load-constant constant target)
	     ,@(conversion target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (CONSTANT (? constant)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant address->fixnum))

(define-integrable (convert-object/register->register target source conversion)
  ;; `conversion' often expands into multiple references to `target'.
  (let ((target (move-to-alias-register! source 'DATA target)))
    (conversion target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source object->type))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source address->fixnum))

(define (convert-object/offset->register target address offset conversion)
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (MOV L ,source ,target)
	   ,@(conversion target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->ADDRESS (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (OFFSET (REGISTER (? address))
						    (? offset)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (LAP (MOV L ,source ,(standard-target-reference target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L (@A+ 7) ,(standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (and (pseudo-register? target) (machine-register? datum)))
  (let ((target (reference-target-alias! target 'DATA)))
    (LAP (MOV L ,(register-reference datum) ,target)
	 (OR UL (& ,(make-non-pointer-literal type 0)) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (and (pseudo-register? target) (pseudo-register? datum)))
  (let ((target (move-to-alias-register! datum 'DATA target)))
    (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (UNASSIGNED))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-non-pointer (ucode-type unassigned)
			  0
			  (standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (CONSTANT (? datum))))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-non-pointer type datum (standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (QUALIFIER (pseudo-register? target))
  (let ((temp (reference-temporary-register! 'ADDRESS)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
		,temp)
	   (MOV L ,temp ,target)
	   (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (load-fixnum-constant constant (reference-target-alias! target 'DATA)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source object->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->fixnum))    

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source fixnum->object))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source fixnum->address))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (LAP ,(load-constant object (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned)
			  0
			  (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (LAP (MOV L
	    ,(standard-register-reference r false true)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (let ((target (indirect-reference! address offset)))
    (LAP (MOV L ,(standard-register-reference datum 'DATA true) ,target)
	 ,(memory-set-type type target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (indirect-reference! address offset)))
    (LAP (LEA ,(indirect-reference! source n) ,temp)
	 (MOV L ,temp ,target)
	 ,(memory-set-type type target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (indirect-reference! address offset)))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,temp)
	 (MOV L ,temp ,target)
	 ,(memory-set-type type target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (if (and (= a0 a1) (= n0 n1))
      (LAP)
      (let ((source (indirect-reference! a1 n1)))
	(LAP (MOV L ,source ,(indirect-reference! a0 n0))))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (FIXNUM->OBJECT (REGISTER (? source))))
  (let ((target (indirect-reference! a n)))
    (let ((temporary (move-to-temporary-register! source 'DATA)))
      (LAP ,@(fixnum->object temporary)
	   (MOV L ,temporary ,target)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (CONS-POINTER (CONSTANT (? type)) (CONSTANT (? datum))))
  (LAP ,(load-non-pointer type datum (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (QUALIFIER (pseudo-word? r))
  (LAP (MOV L ,(standard-register-reference r false true) (@A+ 5))))

#|
;; This seems like a fossil.  Removed by Jinx.

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (QUALIFIER (pseudo-float? r))
  (LAP (FMOVE D ,(machine-register-reference r 'FLOAT) (@A+ 5))))
|#

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (move-to-temporary-register! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@A+ 5)))))

(define-rule statement
  ;; This pops the top of stack into the heap
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) (@A+ 5))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (INST-EA (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  (LAP (MOV L ,(standard-register-reference r false true) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (LAP (MOV L ,(standard-register-reference datum 'DATA true) (@-A 7))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (LAP (PEA (@PCR ,(rtl-procedure/external-label (label->object label))))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:CONTINUATION (? label))))
  (LAP (PEA (@PCR ,label))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (ENTRY:CONTINUATION (? label)))
  (LAP (PEA (@PCR ,label))
       ,(memory-set-type (ucode-type compiled-entry) (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? r)) (? n))))
  (LAP (PEA ,(indirect-reference! r n))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (move-to-temporary-register! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@-A 7)))))

;;;; Fixnum Operations

(define-rule statement
  (ASSIGN (? target) (FIXNUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source)))
  overflow?				; ignored
  (reuse-and-load-machine-target! 'DATA
				  target
				  source
				  (fixnum-1-arg/operate operator)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source1)
		  (pseudo-register? source2)))
  overflow?				; ignored
  (two-arg-register-operation (fixnum-2-args/operate operator)
			      (fixnum-2-args/commutative? operator)
			      'DATA
			      (standard-fixnum-source operator)
			      (lambda (source)
				(standard-register-reference source
							     'DATA
							     true))
			      target
			      source1
			      source2))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source)))
  overflow?				; ignored
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source)))
  overflow?				; ignored
  (if (fixnum-2-args/commutative? operator)
      (fixnum-2-args/register*constant operator target source constant)
      (fixnum-2-args/constant*register operator target constant source)))

(define (fixnum-2-args/register*constant operator target source constant)
  (reuse-and-load-machine-target! 'DATA target source
    (lambda (target)
      ((fixnum-2-args/operate-constant operator) target constant))))

(define (fixnum-2-args/constant*register operator target constant source)
  (reuse-and-operate-on-machine-target! 'DATA target
    (lambda (target)
      (LAP ,@(load-fixnum-constant constant target)
	   ,@((fixnum-2-args/operate operator)
	      target
	      ((standard-fixnum-source operator) source))))))

(define (standard-fixnum-source operator)
  (let ((alternate-types?
	 (not (memq operator
		    '(MULTIPLY-FIXNUM FIXNUM-DIVIDE FIXNUM-REMAINDER)))))
    (lambda (source)
      (standard-register-reference source 'DATA alternate-types?))))

;;; The maximum value for a shift constant is 8, so these rules can
;;; only be used when the type width is 6 bits or less.

(if (<= scheme-type-width 6)
    (begin

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source)))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-register? source)))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target r n))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target r n))

;;; end (IF (<= SCHEME-TYPE-WIDTH 6) ...)
))

;;; It doesn't hurt for these to be defined when the above rules are
;;; not in use.

(define (convert-index->fixnum/register target source)
  (reuse-and-load-machine-target! 'DATA target source
    (lambda (target)
      (LAP (AS L L (& ,(+ scheme-type-width 2)) ,target)))))

(define (convert-index->fixnum/offset target address offset)
  (let ((source (indirect-reference! address offset)))
    (reuse-and-operate-on-machine-target! 'DATA target
      (lambda (target)
	(LAP (MOV L ,source ,target)
	     (AS L L (& ,(+ scheme-type-width 2)) ,target))))))

;;;; Flonum Operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (QUALIFIER (pseudo-float? source))
  (let ((target (reference-target-alias! target 'DATA)))
    (LAP (MOV L (A 5) ,target)
	 (OR L (& ,(make-non-pointer-literal (ucode-type flonum) 0)) ,target)
	 ,(load-non-pointer (ucode-type manifest-nm-vector)
			    flonum-size
			    (INST-EA (@A+ 5)))
	 (FMOVE D
		,(machine-register-reference source 'FLOAT)
		(@A+ 5)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (@ADDRESS->FLOAT (REGISTER (? source))))
  (QUALIFIER (pseudo-float? target))
  (LAP (FMOVE D
	      ,(indirect-reference! source 1)
	      ,(reference-target-alias! target 'FLOAT))))

(define-rule statement
  (ASSIGN (? target)
	  (FLONUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-float? source)))
  overflow?				; ignored
  (let ((operate-on-target
	 (lambda (target)
	   ((flonum-1-arg/operate operator)
	    (standard-register-reference source 'FLOAT false)
	    target))))
    (reuse-machine-target! 'FLOAT target
      (lambda (target)
	(operate-on-target (reference-target-alias! target 'FLOAT)))
      operate-on-target)))

(define-rule statement
  (ASSIGN (? target)
	  (FLONUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (and (machine-operation-target? target)
		  (pseudo-float? source1)
		  (pseudo-float? source2)))
  overflow?				; ignored
  (let ((source-reference
	 (lambda (source) (standard-register-reference source 'FLOAT false))))
    (two-arg-register-operation (flonum-2-args/operate operator)
				(flonum-2-args/commutative? operator)
				'FLOAT
				source-reference
				source-reference
				target
				source1
				source2)))

;;;; CHAR->ASCII/BYTE-OFFSET

(define (load-char-into-register type source target)
  (let ((target (reference-target-alias! target 'DATA)))
    (delete-dead-registers!)
    (LAP ,(load-non-pointer type 0 target)
	 (MOV B ,source ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (load-char-into-register 0
			   (indirect-char/ascii-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((source (machine-register-reference source 'DATA)))
    (delete-dead-registers!)
    (LAP (BFEXTU ,source (& 24) (& 8)
		 ,(reference-target-alias! target 'DATA)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (load-char-into-register 0
			   (indirect-byte-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type))
			(BYTE-OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (load-char-into-register type
			   (indirect-byte-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (CONSTANT (? character))))
  (LAP (MOV B
	    (& ,(char->signed-8-bit-immediate character))
	    ,(indirect-byte-reference! address offset))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (REGISTER (? source)))
  (let ((source (coerce->any/byte-reference source)))
    (let ((target (indirect-byte-reference! address offset)))
      (LAP (MOV B ,source ,target)))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (coerce->any/byte-reference source)))
    (let ((target (indirect-byte-reference! address offset)))
      (LAP (MOV B ,source ,target)))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? target)) (? target-offset))
	  (CHAR->ASCII (OFFSET (REGISTER (? source)) (? source-offset))))
  (let ((source (indirect-char/ascii-reference! source source-offset)))
    (LAP (MOV B ,source ,(indirect-byte-reference! target target-offset)))))