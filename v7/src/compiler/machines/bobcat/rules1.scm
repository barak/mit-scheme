#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules1.scm,v 4.7 1988/05/09 19:57:17 mhwu Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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
  (ASSIGN (REGISTER 15) (REGISTER (? source)))
  (LAP (MOV L ,(coerce->any source) (A 7))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (LEA ,(indirect-reference! source offset) (A 7))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (increment-machine-register 15 n))

(define-rule statement
  (ASSIGN (REGISTER 12) (REGISTER 15))
  (LAP (MOV L (A 7) (A 4))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OFFSET-ADDRESS (REGISTER 15) (? offset)))
  (LAP (LEA (@AO 7 ,(* 4 offset)) (A 4))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (LEA ,(indirect-reference! source offset) (A 4))))

;;; The following rule always occurs immediately after an instruction
;;; of the form
;;;
;;; (ASSIGN (REGISTER (? source)) (POST-INCREMENT (REGISTER 15) 1))
;;;
;;; in which case it could be implemented very efficiently using the
;;; sequence
;;;
;;; (LAP (CLR (@A 7)) (MOV L (@A+ 7) (A 4)))
;;;
;;; but unfortunately we have no mechanism to take advantage of this.

(define-rule statement
  (ASSIGN (REGISTER 12) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? source))
  (reuse-pseudo-register-alias! source 'DATA
    (lambda (reusable-alias)
      (let ((source (register-reference reusable-alias)))
	(LAP (AND L ,mask-reference ,source)
	     (MOV L ,source (A 4)))))
    (lambda ()
      (let ((temp (reference-temporary-register! 'DATA)))
	(LAP (MOV L ,(coerce->any source) ,temp)
	     (AND L ,mask-reference ,temp)
	     (MOV L ,temp (A 4)))))))

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (QUALIFIER (pseudo-register? target))
  (reuse-pseudo-register-alias! source 'DATA
    (lambda (reusable-alias)
      (add-pseudo-register-alias! target reusable-alias false)
      (increment-machine-register reusable-alias n))
    (lambda ()
      (let ((source (indirect-reference! source n)))
	(delete-dead-registers!)
	(LAP (LEA ,source
		  ,(register-reference
		    (allocate-alias-register! target 'ADDRESS))))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-constant source (coerce->any target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@PCR ,(free-reference-label name))
	    ,(register-reference
	      (allocate-alias-register! target 'ADDRESS)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@PCR ,(free-assignment-label name))
	    ,(register-reference
	      (allocate-alias-register! target 'ADDRESS)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (AND L ,mask-reference ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (RO L L (& 8) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    ;; The fact that the target register here is a data register is a
    ;; heuristic that works reasonably well since if the value is a
    ;; pointer, we will probably want to dereference it, which
    ;; requires that we first mask it.
    (LAP (MOV L
	      ,source
	      ,(register-reference
		(allocate-alias-register! target 'DATA))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@A+ 7)
	    ,(register-reference
	      (allocate-alias-register! target 'DATA)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (pseudo-register? target))
  (let ((datum (coerce->any datum)))
    (delete-dead-registers!)
    (let ((target* (coerce->any target)))
      (if (register-effective-address? target*)
	  (LAP (MOV L ,datum ,reg:temp)
	       (MOV B (& ,type) ,reg:temp)
	       (MOV L ,reg:temp ,target*))
	  (LAP (MOV L ,datum ,target*)
	       (MOV B (& ,type) ,target*))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (QUALIFIER (pseudo-register? target))
  (let ((temp (reference-temporary-register! 'ADDRESS)))
    (delete-dead-registers!)
    (let ((target* (coerce->any target)))
      (if (register-effective-address? target*)
	  (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
		    ,temp)
	       (MOV L ,temp ,reg:temp)
	       (MOV B (& ,type) ,reg:temp)
	       (MOV L ,reg:temp ,target*))
	  (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
		    ,temp)
	       (MOV L ,temp ,target*)
	       (MOV B (& ,type) ,target*))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? datum))))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (let ((target-ref (register-reference (allocate-alias-register! target 'DATA))))
    (load-fixnum-constant datum target-ref)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target-ref (move-to-alias-register! source 'DATA target)))
    (LAP ,(remove-type-from-fixnum target-ref))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    (let ((target-ref (register-reference (allocate-alias-register! target 'DATA))))
      (LAP (MOV L ,source ,target-ref)
	   ,(remove-type-from-fixnum target-ref)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator) (? operand1) (? operand2)))
  (QUALIFIER (pseudo-register? target))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (let ((operation
	   (LAP ,@(fixnum-do-2-args! operator operand1 operand2 temp-reg)
		,@(put-type-in-ea (ucode-type fixnum) (register-reference temp-reg)))))
      (delete-dead-registers!)
      (add-pseudo-register-alias! target temp-reg false)
      operation)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operator) (? operand)))
  (QUALIFIER (pseudo-register? target))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (let ((operation
	   (LAP ,@(fixnum-do-1-arg! operator operand temp-reg)
		,@(put-type-in-ea (ucode-type fixnum) (register-reference temp-reg)))))
      (delete-dead-registers!)
      (add-pseudo-register-alias! target temp-reg false)
      operation)))

;;;; OBJECT->DATUM rules.  Assignment is always to a register.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? datum))))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (let ((target-ref
	 (register-reference (allocate-alias-register! target 'DATA))))
    (load-constant-datum datum target-ref)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target-ref (move-to-alias-register! source 'DATA target)))
    (LAP ,(scheme-object->datum target-ref))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    (let ((target-ref
	   (register-reference (allocate-alias-register! target 'DATA))))
      (LAP (MOV L ,source ,target-ref)
	   ,(scheme-object->datum target-ref)))))


;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (byte-offset->register (indirect-char/ascii-reference! address offset)
			 (indirect-register address)
			 target))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CHAR->ASCII (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (if (machine-register? source)
      (LAP (BFEXTU ,(register-reference source)
		   (& 0) (& 8)
		   ,(register-reference (allocate-alias-register! target 'DATA))))
      (byte-offset->register
       (indirect-char/ascii-reference! regnum:regs-pointer
				       (pseudo-register-offset source))
       (indirect-register regnum:regs-pointer)
       target)))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (coerce->any/byte-reference source)))
    (let ((target (indirect-byte-reference! address offset)))
      (LAP (MOV B ,source ,target)))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (CONSTANT (? character))))
  (LAP (MOV B (& ,(char->signed-8-bit-immediate character))
	    ,(indirect-byte-reference! address offset))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (BYTE-OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (byte-offset->register (indirect-byte-reference! address offset)
			 (indirect-register address)
			 target))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? target)) (? target-offset))
	  (CHAR->ASCII (OFFSET (REGISTER (? source)) (? source-offset))))
  (let ((source (indirect-char/ascii-reference! source source-offset)))
    (LAP (MOV B ,source ,(indirect-byte-reference! target target-offset)))))


;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (LAP ,(load-constant object (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (LAP (MOV L
	    ,(coerce->any r)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L
	    (@A+ 7)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (let ((target (indirect-reference! a n)))
    (LAP (MOV L ,(coerce->any r) ,target)
	 (MOV B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (let* ((target (indirect-reference! a n))
	 (temp (reference-temporary-register! 'ADDRESS)))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,temp)
	 (MOV L ,temp ,target)
	 (MOV B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (LAP (MOV L
	      ,source
	      ,(indirect-reference! a0 n0)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (FIXNUM-2-ARGS (? operator) (? operand1) (? operand2)))
  (let ((temp-reg (allocate-temporary-register! 'DATA))
	(target-ref (indirect-reference! a n)))
    (LAP ,@(fixnum-do-2-args! operator operand1 operand2 temp-reg)
	 (MOV L ,(register-reference temp-reg) ,target-ref)
	 ,@(put-type-in-ea (ucode-type fixnum) target-ref))))


(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (FIXNUM-1-ARG (? operator) (? operand)))
  (let ((temp-reg (allocate-temporary-register! 'DATA))
	(target-ref (indirect-reference! a n)))
    (LAP ,@(fixnum-do-1-arg! operator operand temp-reg)
	 (MOV L ,(register-reference temp-reg) ,target-ref)
	 ,@(put-type-in-ea (ucode-type fixnum) target-ref))))

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
  (LAP (MOV L ,(coerce->any r) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (FIXNUM-2-ARGS (? operator) (? operand1) (? operand2)))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (LAP ,@(fixnum-do-2-args! operator operand1 operand2 temp-reg)
	 (MOV L ,(register-reference temp-reg) (@A+ 5))
	 ,@(put-type-in-ea (ucode-type fixnum) (INST-EA (@A 5))))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (FIXNUM-1-ARG (? operator) (? operand)))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (LAP ,@(fixnum-do-1-arg! operator operand temp-reg)
	 (MOV L ,(register-reference temp-reg) (@A+ 5))
	 ,@(put-type-in-ea (ucode-type fixnum) (INST-EA (@A 5))))))

;; This pops the top of stack into the heap

(define-rule statement
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
  (LAP (MOV L ,(coerce->any r) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (LAP (MOV L ,(coerce->any r) (@-A 7))
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
	  (FIXNUM-2-ARGS (? operator) (? operand1) (? operand2)))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (LAP ,@(fixnum-do-2-args! operator operand1 operand2 temp-reg)
	 (MOV L ,(register-reference temp-reg) (@-A 7))
	 ,@(put-type-in-ea (ucode-type fixnum) (INST-EA (@A 7))))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) 
	  (FIXNUM-1-ARG (? operator) (? operand)))
  (let ((temp-reg (allocate-temporary-register! 'DATA)))
    (LAP ,@(fixnum-do-1-arg! operator operand temp-reg)
	 (MOV L ,(register-reference temp-reg) (@-A 7))
	 ,@(put-type-in-ea (ucode-type fixnum) (INST-EA (@A 7))))))
