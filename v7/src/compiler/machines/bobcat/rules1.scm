#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules1.scm,v 4.22 1989/04/27 20:06:32 cph Rel $

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
	    ,(standard-register-reference source false)
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
  (QUALIFIER (and (pseudo-register? target) (machine-register? source)))
  (let ((source (indirect-reference! source n)))
    (delete-dead-registers!)
    (LAP (LEA ,source ,(reference-target-alias! target 'ADDRESS)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (QUALIFIER (and (pseudo-register? target) (pseudo-register? source)))
  (reuse-pseudo-register-alias! source 'DATA
    (lambda (reusable-alias)
      (delete-dead-registers!)
      (add-pseudo-register-alias! target reusable-alias)
      (increment-machine-register reusable-alias n))
    (lambda ()
      (let ((source (indirect-reference! source n)))
	(delete-dead-registers!)
	(LAP (LEA ,source ,(reference-target-alias! target 'ADDRESS)))))))

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
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  (LAP))

(define (convert-object/constant->register target constant conversion)
  (delete-dead-registers!)
  (let ((target (reference-target-alias! target 'DATA)))
    (if (non-pointer-object? constant)
	(LAP ,(load-non-pointer 0 (object-datum constant) target))
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
	 (OR L (& ,(make-non-pointer-literal type 0)) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (and (pseudo-register? target) (pseudo-register? datum)))
  (let ((target (move-to-alias-register! datum 'DATA target)))
    (LAP (OR L (& ,(make-non-pointer-literal type 0)) ,target))))

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
	   (OR L (& ,(make-non-pointer-literal type 0)) ,target)))))

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
	    ,(standard-register-reference r false)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (let ((target (indirect-reference! address offset)))
    (LAP (MOV L ,(standard-register-reference datum 'DATA) ,target)
	 (MOV B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (indirect-reference! address offset)))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,temp)
	 (MOV L ,temp ,target)
	 (MOV B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (LAP (MOV L ,source ,(indirect-reference! a0 n0)))))

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
  (LAP (MOV L ,(standard-register-reference r false) (@A+ 5))))

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
  (LAP (MOV L ,(standard-register-reference r false) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (LAP (MOV L ,(standard-register-reference datum 'DATA) (@-A 7))
       (MOV B (& ,type) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (LAP (PEA (@PCR ,(rtl-procedure/external-label (label->object label))))
       (MOV B (& ,type) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (ENTRY:CONTINUATION (? label)))
  (LAP (PEA (@PCR ,label))
       (MOV B (& ,(ucode-type compiled-entry)) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (move-to-temporary-register! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@-A 7)))))

;;;; Fixnum Operations

(define-rule statement
  (ASSIGN (? target) (FIXNUM-1-ARG (? operator) (REGISTER (? source))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (reuse-and-load-fixnum-target! target
				 source
				 (fixnum-1-arg/operate operator)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (if (fixnum-2-args/commutative? operator)
      (fixnum-2-args/register*constant operator target source constant)
      (fixnum-2-args/constant*register operator target constant source)))

(define (fixnum-2-args/register*constant operator target source constant)
  (reuse-and-load-fixnum-target! target source
    (lambda (target)
      ((fixnum-2-args/operate-constant operator) target constant))))

(define (fixnum-2-args/constant*register operator target constant source)
  (reuse-and-operate-on-fixnum-target! target
    (lambda (target)
      (LAP ,@(load-fixnum-constant constant target)
	   ,@((fixnum-2-args/operate operator)
	      target
	      (if (eq? operator 'MULTIPLY-FIXNUM)
		  (standard-multiply-source source)
		  (standard-register-reference source 'DATA)))))))

(define (reuse-and-operate-on-fixnum-target! target operate-on-target)
  (reuse-fixnum-target! target
    (lambda (target)
      (operate-on-target (reference-target-alias! target 'DATA)))
    operate-on-target))

#|

;;; This code would have been a nice idea except that 10 is not a
;;; valid value as a shift constant.

(define (convert-index->fixnum/register target source)
  (reuse-and-load-fixnum-target! target source
    (lambda (target)
      (LAP (LS L L (& 10) ,target)))))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (convert-index->fixnum/register target source))

(define (convert-index->fixnum/offset target address offset)
  (let ((source (indirect-reference! address offset)))
    (reuse-and-operate-on-fixnum-target! target
      (lambda (target)
	(LAP (MOV L ,source ,target)
	     (LS L L (& 10) ,target))))))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))))
  (QUALIFIER (fixnum-operation-target? target))
  (convert-index->fixnum/offset target r n))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (OBJECT->FIXNUM (CONSTANT 4))))
  (QUALIFIER (fixnum-operation-target? target))
  (convert-index->fixnum/offset target r n))

|#
(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (QUALIFIER (and (fixnum-operation-target? target)
		  (pseudo-register? source1)
		  (pseudo-register? source2)))
  (let ((worst-case
	 (lambda (target source1 source2)
	   (LAP (MOV L ,source1 ,target)
		,@((fixnum-2-args/operate operator) target source2))))
	(source-reference
	 (if (eq? operator 'MULTIPLY-FIXNUM)
	     standard-multiply-source
	     (lambda (source) (standard-register-reference source 'DATA)))))
    (reuse-fixnum-target! target
      (lambda (target)
	(reuse-pseudo-register-alias! source1 'DATA
	  (lambda (alias)
	    (let ((source2 (if (= source1 source2)
			       (register-reference alias)
			       (source-reference source2))))
	      (delete-dead-registers!)
	      (add-pseudo-register-alias! target alias)
	      ((fixnum-2-args/operate operator) (register-reference alias)
						source2)))
	  (lambda ()
	    (let ((new-target-alias!
		   (lambda (source1 source2)
		     (delete-dead-registers!)
		     (worst-case (reference-target-alias! target 'DATA)
				 source1
				 source2))))
	      (reuse-pseudo-register-alias source2 'DATA
		(lambda (alias)
		  (let ((source1 (source-reference source1))
			(source2 (register-reference alias)))
		    (let ((use-source2-alias!
			   (lambda ()
			     (delete-machine-register! alias)
			     (delete-dead-registers!)
			     (add-pseudo-register-alias! target alias)
			     ((fixnum-2-args/operate operator) source2
							       source1))))
		      (cond ((fixnum-2-args/commutative? operator)
			     (use-source2-alias!))
			    ((effective-address/data-register? source1)
			     (LAP (EXG ,source2 ,source1)
				  ,@(use-source2-alias!)))
			    (else
			     (new-target-alias! source1 source2))))))
		(lambda ()
		  (new-target-alias!
		   (standard-register-reference source1 'DATA)
		   (source-reference source2))))))))      (lambda (target)
	(worst-case target
		    (standard-register-reference source1 'DATA)
		    (source-reference source2))))))

(define (standard-multiply-source register)
  (let ((alias (register-alias register 'DATA)))
    (cond (alias
	   (register-reference alias))
	  ((register-saved-into-home? register)
	   (pseudo-register-home register))
	  (else
	   (reference-alias-register! register 'DATA)))))
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