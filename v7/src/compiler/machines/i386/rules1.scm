#| -*-Scheme-*-

$Id: rules1.scm,v 1.18 1993/07/16 19:27:52 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (REGISTER (? index))))
  (load-indexed-register target source index 4))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? n))))
  (load-displaced-register target source (* 4 n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (REGISTER (? index))))
  (load-indexed-register target source index 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? n))))
  (load-displaced-register target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(REGISTER (? index))))
  (load-indexed-register target source index 8))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? n))))
  (load-displaced-register target source (* 8 n)))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source))
					(MACHINE-CONSTANT (? n)))))
  (load-displaced-register/typed target source type (* 4 n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source))
					     (MACHINE-CONSTANT (? n)))))
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
  (ASSIGN (REGISTER (? target)) (? expression rtl:simple-offset?))
  (let ((source (offset->reference! expression)))
    (LAP (MOV W ,(target-register-reference target) ,source))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 4) 1))
  (LAP (POP ,(target-register-reference target))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (let ((source (source-register-reference r)))
    (LAP (MOV W
	      ,(offset->reference! expression)
	      ,source))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?) (CONSTANT (? value)))
  (QUALIFIER (non-pointer-object? value))
  (LAP (MOV W ,(offset->reference! expression)
	    (&U ,(non-pointer->literal value)))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (LAP (MOV W ,(offset->reference! expression)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (BYTE-OFFSET-ADDRESS (? expression)
			       (MACHINE-CONSTANT (? n))))
  (if (zero? n)
      (LAP)
      (LAP (ADD W ,(offset->reference! expression) (& ,n)))))

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
	  (CHAR->ASCII (? expression rtl:simple-offset?)))
  (load-char-into-register 0
			   (offset->reference! expression)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (load-char-into-register 0
			   (source-register-reference source)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:simple-byte-offset?))
  (load-char-into-register 0
			   (byte-offset->reference! expression)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(? expression rtl:simple-byte-offset?)))
  (load-char-into-register type
			   (byte-offset->reference! expression)
			   target))

(define-rule statement
  (ASSIGN (? expression rtl:simple-byte-offset?)
	  (CHAR->ASCII (CONSTANT (? character))))
  (LAP (MOV B
	    ,(byte-offset->reference! expression)
	    (& ,(char->signed-8-bit-immediate character)))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-byte-offset?)
	  (REGISTER (? source)))
  (let* ((source (source-register-reference source))
	 (target (byte-offset->reference! expression)))
    (LAP (MOV B ,target ,source))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-byte-offset?)
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (source-register-reference source))
	(target (byte-offset->reference! expression)))
    (LAP (MOV B ,target ,source))))

(define (char->signed-8-bit-immediate character)
  (let ((ascii (char->ascii character)))
    (if (< ascii 128) ascii (- ascii 256))))

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

(define (load-indexed-register target source index scale)
  (let* ((source (indexed-ea source index scale 0))
	 (target (target-register-reference target)))
    (LAP (LEA ,target ,source))))  

(define (load-pc-relative-address/typed target type label)
  (with-pc
    (lambda (pc-label pc-register)
      (LAP (LEA ,target (@RO UW
			     ,pc-register
			     (+ ,(make-non-pointer-literal type 0)
				(- ,label ,pc-label))))))))

(define (load-char-into-register type source target)
  (let ((target (target-register-reference target)))
    (cond ((zero? type)
	   ;; No faster, but smaller
	   (LAP (MOVZX B ,target ,source)))
	  (else
	   (LAP ,@(load-non-pointer target type 0)
		(MOV B ,target ,source))))))

(define (indirect-unsigned-byte-reference! register offset)
  (byte-unsigned-offset-reference (allocate-indirection-register! register)
				  offset))

;;;; Improved vector and string references

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? expression rtl:detagged-offset?))
  (with-detagged-vector-location expression false
    (lambda (temp)
      (LAP (MOV W ,(target-register-reference target) ,temp)))))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-offset?)
	  (REGISTER (? source)))
  (QUALIFIER (register-value-class=word? source))
  (with-detagged-vector-location expression source
    (lambda (temp)
      (LAP (MOV W ,temp ,(source-register-reference source))))))

(define (with-detagged-vector-location rtl-expression protect recvr)
  (with-decoded-detagged-offset rtl-expression
    (lambda (base index offset)
      (with-indexed-address base index 4 (* 4 offset) protect recvr))))

(define (rtl:detagged-offset? expression)
  (and (rtl:offset? expression)
       (rtl:machine-constant? (rtl:offset-offset expression))
       (let ((base (rtl:offset-base expression)))
	 (and (rtl:offset-address? base)
	      (rtl:detagged-index? (rtl:offset-address-base base)
				   (rtl:offset-address-offset base))))
       expression))

(define (with-decoded-detagged-offset expression recvr)
  (let ((base (rtl:offset-base expression)))
    (let ((base* (rtl:offset-address-base base))
	  (index (rtl:offset-address-offset base)))
      (recvr (rtl:register-number (if (rtl:register? base*)
				      base*
				      (rtl:object->address-expression base*)))
	     (rtl:register-number (if (rtl:register? index)
				      index
				      (rtl:object->datum-expression index)))
	     (rtl:machine-constant-value (rtl:offset-offset expression))))))

;;;; Improved string references

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:detagged-byte-offset?))
  (load-char-indexed/detag 0 target expression))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(? expression rtl:detagged-byte-offset?)))
  (load-char-indexed/detag type target expression))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-byte-offset?)
	  (REGISTER (? source)))
  (store-char-indexed/detag expression
			    source
			    (source-register-reference source)))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-byte-offset?)
	  (CHAR->ASCII (REGISTER (? source))))
  (store-char-indexed/detag expression
			    source
			    (source-register-reference source)))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-byte-offset?)
	  (CHAR->ASCII (CONSTANT (? character))))
  (store-char-indexed/detag expression
			    false
			    (INST-EA (& ,(char->signed-8-bit-immediate
					  character)))))

(define (load-char-indexed/detag tag target rtl-source-expression)
  (with-detagged-string-location rtl-source-expression false
    (lambda (temp)
      (load-char-into-register tag temp target))))

(define (store-char-indexed/detag rtl-target-expression protect source)
  (with-detagged-string-location rtl-target-expression protect
    (lambda (temp)
      (LAP (MOV B ,temp ,source)))))

(define (with-detagged-string-location rtl-expression protect recvr)
  (with-decoded-detagged-byte-offset rtl-expression
    (lambda (base index offset)
      (with-indexed-address base index 1 offset protect recvr))))

(define (rtl:detagged-byte-offset? expression)
  (and (rtl:byte-offset? expression)
       (rtl:machine-constant? (rtl:byte-offset-offset expression))
       (let ((base (rtl:byte-offset-base expression)))
	 (and (rtl:byte-offset-address? base)
	      (rtl:detagged-index? (rtl:byte-offset-address-base base)
				   (rtl:byte-offset-address-offset base))))
       expression))

(define (with-decoded-detagged-byte-offset expression recvr)
  (let ((base (rtl:byte-offset-base expression)))
    (let ((base* (rtl:byte-offset-address-base base))
	  (index (rtl:byte-offset-address-offset base)))
      (recvr (rtl:register-number (if (rtl:register? base*)
				      base*
				      (rtl:object->address-expression base*)))
	     (rtl:register-number (if (rtl:register? index)
				      index
				      (rtl:object->datum-expression index)))
	     (rtl:machine-constant-value
	      (rtl:byte-offset-offset expression))))))