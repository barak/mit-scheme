#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rules1.scm,v 4.6 1989/05/21 03:55:50 jinx Rel $
$MC68020-Header: rules1.scm,v 4.22 89/04/27 20:06:32 GMT cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Data Transfers.  DEC VAX version.
;;; Note: All fixnum code has been moved to rulfix.scm.

(declare (usual-integrations))

;;;; Transfers to Registers

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (machine-register? target))
  (LAP (MOV L
	    ,(standard-register-reference source false)
	    ,(register-reference target))))

(define-rule statement
  (ASSIGN (REGISTER 14) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (MOVA L ,(indirect-reference! source offset) (R 14))))

(define-rule statement
  (ASSIGN (REGISTER 14) (OFFSET-ADDRESS (REGISTER 14) (? n)))
  (increment-rn 14 n))

(define-rule statement
  (ASSIGN (REGISTER 10) (OFFSET-ADDRESS (REGISTER 14) (? offset)))
  (let ((real-offset (* 4 offset)))
    (LAP (MOVA L (@RO ,(datum-size real-offset) 14 ,real-offset) (R 10)))))

(define-rule statement
  (ASSIGN (REGISTER 10) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (MOVA L ,(indirect-reference! source offset) (R 10))))

(define-rule statement
  (ASSIGN (REGISTER 10) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? source))
  (let ((source (preferred-register-reference source)))
    (LAP (BIC L ,mask-reference ,source (R 10)))))

(define-rule statement
  (ASSIGN (REGISTER 10) (OBJECT->ADDRESS (POST-INCREMENT (REGISTER 14) 1)))
  (LAP (BIC L ,mask-reference (@R+ 14) (R 10))))

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
    (LAP (MOVA L ,source ,(standard-target-reference target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (QUALIFIER (and (pseudo-register? target) (pseudo-register? source)))
  (reuse-pseudo-register-alias! source 'GENERAL
    (lambda (reusable-alias)
      (delete-dead-registers!)
      (add-pseudo-register-alias! target reusable-alias)
      (increment-rn reusable-alias n))
    (lambda ()
      ;; *** This could use an add instruction. ***
      (let ((source (indirect-reference! source n)))
	(LAP (MOVA L ,source ,(standard-target-reference target)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-constant source (standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L
	    (@PCR ,(free-reference-label name))
	    ,(standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L
	    (@PCR ,(free-assignment-label name))
	    ,(standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'GENERAL target)
  (LAP))

(define (object->address source reg-ref)
  (if (eq? source reg-ref)
      (LAP (BIC L ,mask-reference ,reg-ref))
      (LAP (BIC L ,mask-reference ,source ,reg-ref))))

(define-integrable (ct/object->address object target)
  (LAP ,(load-immediate (object-datum object) target)))

(define (object->datum source reg-ref)
  (if (eq? source reg-ref)
      (LAP (BIC L ,mask-reference ,reg-ref))
      (LAP (BIC L ,mask-reference ,source ,reg-ref))))

(define-integrable (ct/object->datum object target)
  (LAP ,(load-immediate (object-datum object) target)))

(define-integrable (object->type source reg-ref)
  (LAP (ROTL (S 8) ,source ,reg-ref)))

(define-integrable (ct/object->type object target)
  (LAP ,(load-immediate (object-type object) target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant
				     object->datum
				     ct/object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant
				     object->address
				     ct/object->address))

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
	  (OBJECT->DATUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->ADDRESS (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (LAP (MOV L ,source ,(standard-target-reference target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 14) 1))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L (@R+ 14) ,(standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (and (pseudo-register? target) (machine-register? datum)))
  (let ((target (standard-target-reference target)))
    (LAP (BIS L (& ,(make-non-pointer-literal type 0))
	      ,(register-reference datum) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (and (pseudo-register? target) (pseudo-register? datum)))
  (with-register-copy-alias! datum 'GENERAL target
    (lambda (target)
      (LAP (BIS L (& ,(make-non-pointer-literal type 0)) ,target)))
    (lambda (source target)
      (LAP (BIS L (& ,(make-non-pointer-literal type 0)) ,source ,target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (CONSTANT (? datum))))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-non-pointer type datum (standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (standard-target-reference target)))
    (LAP (MOVA B
	       (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,target)
	 (BIS L (& ,(make-non-pointer-literal type 0)) ,target))))

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

;; 1,3,4,5 of the following may need to do a delete-dead-registers!

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (let ((target (indirect-reference! a n)))
    (LAP (MOV L
	      ,(standard-register-reference r false)
	      ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 14) 1))
  (LAP (MOV L (@R+ 14) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (let ((target (indirect-reference! address offset)))
    (LAP (BIS L ,(make-immediate (make-non-pointer-literal type 0))
	      ,(standard-register-reference datum false)
	      ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (let ((temp (reference-temporary-register! 'GENERAL))
	(target (indirect-reference! address offset)))
    (LAP (MOVA B (@PCR ,(rtl-procedure/external-label (label->object label)))
	       ,temp)
	 (BIS L ,(make-immediate (make-non-pointer-literal type 0))
	      ,temp ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (LAP (MOV L ,source ,(indirect-reference! a0 n0)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@R+ 12)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1)
	  (CONS-POINTER (CONSTANT (? type)) (CONSTANT (? datum))))
  (LAP ,(load-non-pointer type datum (INST-EA (@R+ 12)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1) (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (INST-EA (@R+ 12)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1) (REGISTER (? r)))
  (LAP (MOV L ,(standard-register-reference r false) (@R+ 12))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@R+ 12))))

(define-rule statement
  ;; This pops the top of stack into the heap
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1) (POST-INCREMENT (REGISTER 14) 1))
  (LAP (MOV L (@R+ 14) (@R+ 12))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1) (CONSTANT (? object)))
  (LAP ,(push-constant object)))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1) (UNASSIGNED))
  (LAP ,(push-non-pointer (ucode-type unassigned) 0)))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1) (REGISTER (? r)))
  (LAP (PUSHL ,(standard-register-reference r false))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (LAP (PUSHL ,(standard-register-reference datum 'GENERAL))
       (MOV B (S ,type) (@RO B 14 3))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1)
	  (CONS-POINTER (CONSTANT (? type)) (ENTRY:PROCEDURE (? label))))
  (LAP (PUSHA B (@PCR ,(rtl-procedure/external-label (label->object label))))
       (MOV B (S ,type) (@RO B 14 3))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (PUSHL ,(indirect-reference! r n))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1) (ENTRY:CONTINUATION (? label)))
  (LAP (PUSHA B (@PCR ,label))
       (MOV B (S ,(ucode-type compiled-entry)) (@RO B 14 3))))

;;;; CHAR->ASCII/BYTE-OFFSET

(define (load-char-into-register type source target)
  (let ((target (standard-target-reference target)))
    (if (not (zero? type))
	(LAP ,(load-non-pointer type 0 target)
	     (MOV B ,source ,target))
	(LAP (MOVZ B L ,source ,target)))))    

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
  (let ((source (machine-register-reference source 'GENERAL)))
    (load-char-into-register 0 source target)))

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
	    ,(make-immediate (char->signed-8-bit-immediate character))
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