#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rules1.scm,v 1.2 1992/01/24 03:57:46 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/rules1.scm,v 4.36 1991/10/25 06:49:58 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

MOVB	vs.	MOVW
ADDB	vs.	ADDW
they make, so that these may be included in future releases; and (b)
The assembler assumes that it is always running in 32-bit mode.
It matters for immediate operands, displacements in addressing modes, and displacements in pc-relative jump  instructions.

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  However, it is
;;; necessary to derive the effective address of the source
;;; expression(s) before deleting the dead registers.  Otherwise any
;;; source expression containing dead registers might refer to aliases
;;; which have been reused.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (assign-register->register target source))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (load-displaced-register target source (* 4 n)))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (load-displaced-register/typed target source type (* 4 n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (load-displaced-register target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (load-displaced-register/typed target source type n))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (object->type (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let ((temp (standard-move-to-temporary! type)))
    (LAP (ROR W ,temp (&U ,scheme-type-width))
	 (OR W ,(standard-move-to-target! datum target) ,temp))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (if (zero? type)
      (assign-register->register target datum)
      (LAP (OR W
	       ,(standard-move-to-target! datum target)
	       (&U ,(make-non-pointer-literal type 0))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (object->datum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (object->address (standard-move-to-target! source target)))

;;;; Loading Constants

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant source (target-register-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? n)))
  (load-immediate n (target-register-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (target-register-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address
   target
   (rtl-procedure/external-label (label->object label))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address target label))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (load-pc-relative-address/typed target
				  type
				  (rtl-procedure/external-label
				   (label->object label))))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (load-pc-relative-address/typed target type label))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative target (free-reference-label name)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative target (free-assignment-label name)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (convert-object/constant->register target constant object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (CONSTANT (? constant))))
  (convert-object/constant->register target constant object->address))

;;;; Transfers from Memory

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (let ((source (source-indirect-reference! address offset)))
    (LAP (MOV W ,(target-register-reference target) ,source))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 4) 1))
  (LAP (POP ,(target-register-reference target))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n)) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV W
	    ,(target-indirect-reference! a n)
	    ,(source-register-reference r))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (BYTE-OFFSET-ADDRESS (OFFSET (REGISTER (? address)) (? offset))
			       (? n)))
  (if (zero? n)
      (LAP)
      (LAP (ADD W ,(target-indirect-reference! address offset) (& ,n)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 7) 1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV W (@R 7) ,(source-register-reference r))
       (ADD W (R 7) (& 4))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (PUSH W ,(source-register-reference r))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (LAP (PUSH W (& ,(make-non-pointer-literal type datum)))))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (load-char-into-register 0
			   (indirect-char/ascii-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (load-char-into-register 0
			   (source-register-reference source)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address)) (? offset)))
  (load-char-into-register 0
			   (indirect-byte-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET (REGISTER (? address)) (? offset))))
  (load-char-into-register type
			   (indirect-byte-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (CONSTANT (? character))))
  (LAP (MOV B
	    ,(indirect-byte-reference! address offset)
	    (& ,(char->signed-8-bit-immediate character)))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (REGISTER (? source)))
  (let ((source (source-register-reference source)))
    (let ((target (indirect-byte-reference! address offset)))
      (LAP (MOV B ,target ,source)))))

(define-rule statement
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (source-register-reference source)))
    (let ((target (indirect-byte-reference! address offset)))
      (LAP (MOV B ,target ,source)))))

;;;; Utilities specific to rules1 (others in lapgen)

(define (assign-register->register target source)
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define (convert-object/constant->register target constant conversion)
  (delete-dead-registers!)
  (let ((target (target-register-reference target)))
    (if (non-pointer-object? constant)
	;; Is this correct if conversion is object->address ?
	(load-non-pointer 0 (careful-object-datum constant) target)
	(LAP ,@(load-constant constant target)
	     ,@(conversion target)))))

(define (load-displaced-register target source n)
  (if (zero? n)
      (assign-register->register target source)
      (let* ((source (source-indirect-reference! source n))
	     (target (target-register-reference target)))
	(LAP (LEA ,target ,source)))))

(define (load-displaced-register/typed target source type n)
  (load-displaced-register target
			   source
			   (if (zero? type)
			       n
			       (+ (make-non-pointer-literal type 0) n))))

(define (load-pc-relative target label)
  (with-pc-relative-address
    (lambda (pc-label pc-register)
      (let ((target (target-register-reference target)))
	(LAP (MOV W ,target (@RO ,pc-register (- ,label ,pc-label))))))))

(define (load-pc-relative-address target label)
  (with-pc-relative-address
    (lambda (pc-label pc-register)
      (let ((target (target-register-reference target)))
	(LAP (LEA ,target (@RO ,pc-register (- ,label ,pc-label))))))))

(define (load-pc-relative-address/typed target type label)
  (with-pc-relative-address
    (lambda (pc-label pc-register)
      (let ((target (target-register-reference target)))
	(LAP (LEA ,target (@RO ,pc-register
			       (+ ,(make-non-pointer-literal type 0)
				  (- ,label ,pc-label)))))))))

(define (load-char-into-register type source target)
  (let ((target (target-register-reference target)))
    (cond #|
          ;; According to i486 appendix on optimization, the following
	  ;; instruction is no faster, and typically slower.
	  ;; It takes one less byte, however.
          ((zero? type)
	   (LAP (MOVZX ,target ,source)))
	  |#
	  (else
	   (LAP ,@(load-non-pointer type 0 target)
		(MOV B ,target ,source))))))