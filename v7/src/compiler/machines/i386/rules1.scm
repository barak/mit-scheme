#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rules1.scm,v 1.14 1992/02/18 01:53:26 jinx Exp $
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

;;;; LAP Generation Rules: Data Transfers.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register Assignments

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
      (let ((literal (make-non-pointer-literal type 0)))
	(define (three-arg source)
	  (let ((target (target-register-reference target)))
	    (LAP (LEA ,target (@RO UW ,source ,literal)))))

	(define (two-arg target)
	  (LAP (OR W ,target (&U ,literal))))

	(let ((alias (register-alias datum 'GENERAL)))
	  (cond ((not alias)
		 (two-arg (standard-move-to-target! datum target)))
		((register-copy-if-available datum 'GENERAL target)
		 =>
		 (lambda (get-tgt)
		   (two-arg (get-tgt))))
		(else
		 (three-arg alias)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (object->datum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (object->address (standard-move-to-target! source target)))

;;;; Loading Constants

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant (target-register-reference target) source))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? n)))
  (load-immediate (target-register-reference target) n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer (target-register-reference target) type datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address
   (target-register-reference target)
   (rtl-procedure/external-label (label->object label))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address (target-register-reference target) label))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (load-pc-relative-address/typed (target-register-reference target)
				  type
				  (rtl-procedure/external-label
				   (label->object label))))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (load-pc-relative-address/typed (target-register-reference target)
				  type label))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative (target-register-reference target)
		    (free-reference-label name)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (target-register-reference target)
		    (free-assignment-label name)))

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
  (let ((source (source-register-reference r)))
    (LAP (MOV W
	      ,(target-indirect-reference! a n)
	      ,source))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n)) (CONSTANT (? value)))
  (QUALIFIER (non-pointer-object? value))
  (LAP (MOV W ,(target-indirect-reference! a n)
	    (&U ,(non-pointer->literal value)))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (LAP (MOV W ,(target-indirect-reference! a n)
	    (&U ,(make-non-pointer-literal type datum)))))

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
  (LAP (PUSH ,(source-register-reference r))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1) (CONSTANT (? value)))
  (QUALIFIER (non-pointer-object? value))
  (LAP (PUSH W (&U ,(non-pointer->literal value)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (LAP (PUSH W (&U ,(make-non-pointer-literal type datum)))))

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

(define (char->signed-8-bit-immediate character)
  (let ((ascii (char->ascii character)))
    (if (< ascii 128) ascii (- ascii 256))))

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

;;;; Utilities specific to rules1

(define (load-displaced-register/internal target source n signed?)
  (cond ((zero? n)
	 (assign-register->register target source))
	((and (= target source)
	      (= target esp))
	 (if signed?
	     (LAP (ADD W (R ,esp) (& ,n)))
	     (LAP (ADD W (R ,esp) (&U ,n)))))
	(signed?
	 (let* ((source (indirect-byte-reference! source n))
		(target (target-register-reference target)))
	   (LAP (LEA ,target ,source))))
	(else
	 (let* ((source (indirect-unsigned-byte-reference! source n))
		(target (target-register-reference target)))
	   (LAP (LEA ,target ,source))))))

(define-integrable (load-displaced-register target source n)
  (load-displaced-register/internal target source n true))

(define-integrable (load-displaced-register/typed target source type n)
  (load-displaced-register/internal target
				    source
				    (if (zero? type)
					n
					(+ (make-non-pointer-literal type 0)
					   n))
				    false))

(define (load-pc-relative-address/typed target type label)
  (with-pc
    (lambda (pc-label pc-register)
      (LAP (LEA ,target (@RO UW
			     ,pc-register
			     (+ ,(make-non-pointer-literal type 0)
				(- ,label ,pc-label))))))))

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
	   (LAP ,@(load-non-pointer target type 0)
		(MOV B ,target ,source))))))

(define (indirect-char/ascii-reference! register offset)
  (indirect-byte-reference! register (* offset 4)))

(define (indirect-byte-reference! register offset)
  (byte-offset-reference (allocate-indirection-register! register) offset))

(define (indirect-unsigned-byte-reference! register offset)
  (byte-unsigned-offset-reference (allocate-indirection-register! register)
				  offset))