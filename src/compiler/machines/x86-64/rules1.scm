#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
  (load-indexed-register target source index address-units-per-object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? n))))
  (load-displaced-register target source n address-units-per-object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (REGISTER (? index))))
  (load-indexed-register target source index 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? n))))
  (load-displaced-register target source n 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(REGISTER (? index))))
  (load-indexed-register target source index address-units-per-float))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? n))))
  (load-displaced-register target source n address-units-per-float))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source))
					(MACHINE-CONSTANT (? n)))))
  (load-displaced-register/typed target source type n
				 address-units-per-object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source))
					     (MACHINE-CONSTANT (? n)))))
  (load-displaced-register/typed target source type n 1))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (object->type (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let ((temp (standard-move-to-temporary! type)))
    (LAP (ROR Q ,temp (&U ,scheme-type-width))
	 (OR Q ,(standard-move-to-target! datum target) ,temp))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (if (zero? type)
      (assign-register->register target datum)
      (let* ((datum (source-register-reference datum))
	     (target (target-register-reference target)))
	(LAP (MOV Q ,target (&U ,(make-non-pointer-literal type 0)))
	     (OR Q ,target ,datum)))))

#| This doesn't work because immediate operands aren't big enough to
   fit the type tag.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (if (zero? type)
      (assign-register->register target datum)
      (let ((literal (make-non-pointer-literal type 0)))
	(define (three-arg source)
	  (let ((target (target-register-reference target)))
	    (LAP (LEA Q ,target (@RO ,source ,literal)))))

	(define (two-arg target)
	  (LAP (OR Q ,target (&U ,literal))))

	(let ((alias (register-alias datum 'GENERAL)))
	  (cond ((not alias)
		 (two-arg (standard-move-to-target! datum target)))
		((register-copy-if-available datum 'GENERAL target)
		 =>
		 (lambda (get-tgt)
		   (two-arg (get-tgt))))
		(else
		 (three-arg alias)))))))
|#

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (object->datum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (object->address (standard-move-to-target! source target)))

;;;; Loading Constants

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? object)))
  (load-constant (target-register-reference target) object))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? n)))
  (load-signed-immediate (target-register-reference target) n))

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
  (load-converted-constant target constant object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (CONSTANT (? constant))))
  (load-converted-constant target constant object->address))

;;;; Transfers from Memory

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:simple-offset?))
  (let ((source (offset->reference! expression)))
    (LAP (MOV Q ,(target-register-reference target) ,source))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 4) 1))
  (LAP (POP Q ,(target-register-reference target))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (let ((source (source-register-reference r)))
    (LAP (MOV Q ,(offset->reference! expression) ,source))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?) (CONSTANT (? object)))
  (QUALIFIER (non-pointer-object? object))
  (store-non-pointer-constant expression object))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (store-non-pointer expression type datum))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (BYTE-OFFSET-ADDRESS (? expression)
			       (MACHINE-CONSTANT (? n))))
  (if (zero? n)
      (LAP)
      (with-signed-immediate-operand n
	(lambda (operand)
	  (LAP (ADD Q ,(offset->reference! expression) ,operand))))))

;;;; Consing

;;; rdi = 7, regnum:free-pointer

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 7) 1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV Q (@R 7) ,(source-register-reference r))
       (ADD Q (R 7) (&U ,address-units-per-object))))

;;;; Pushes

;;; rsp = 4, regnum:stack-pointer

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (PUSH Q ,(source-register-reference r))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1) (CONSTANT (? value)))
  (QUALIFIER (non-pointer-object? value))
  (with-unsigned-immediate-operand (non-pointer->literal value)
    (lambda (operand)
      (LAP (PUSH Q ,operand)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 4) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (with-unsigned-immediate-operand (make-non-pointer-literal type datum)
    (lambda (operand)
      (LAP (PUSH Q ,operand)))))

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

(define (load-displaced-register/internal target source n scale signed?)
  (cond ((zero? n)
	 (assign-register->register target source))
	((and (= target source)
	      ;; Why this condition?
	      (= target rsp))
	 ((if signed?
	      with-signed-immediate-operand
	      with-unsigned-immediate-operand)
	  (* n scale)
	  (lambda (operand)
	    (LAP (ADD Q (R ,rsp) ,operand)))))
	(else
	 (receive (reference! referenceable?)
	     (if signed?
		 (values indirect-byte-reference! byte-offset-referenceable?)
		 (values indirect-unsigned-byte-reference!
			 byte-unsigned-offset-referenceable?))
	   (let ((n-scaled (* n scale)))
	     (if (referenceable? n-scaled)
		 (let* ((source (reference! source n-scaled))
			(target (target-register-reference target)))
		   (LAP (LEA Q ,target ,source)))
		 (let ((temp (allocate-temporary-register! 'GENERAL))
		       (source (allocate-indirection-register! source)))
		   (let ((target (target-register-reference target)))
		     (LAP (MOV Q (R ,temp)
			       ,(if signed?
				    (INST-EA (& ,n))
				    (INST-EA (&U ,n))))
			  ;++ Check that SCALE is a valid SIB scale.
			  (LEA Q ,target (@RI ,source ,temp ,scale)))))))))))

(define-integrable (load-displaced-register target source n scale)
  (load-displaced-register/internal target source n scale #t))

(define (load-displaced-register/typed target source type n scale)
  (if (zero? type)
      (load-displaced-register/internal target source n scale #f)
      (load-displaced-register/internal target
					source
					(+ (make-non-pointer-literal type 0)
					   (* n scale))
					1
					#f)))

(define (load-indexed-register target source index scale)
  (let* ((source (indexed-ea source index scale 0))
	 (target (target-register-reference target)))
    (LAP (LEA Q ,target ,source))))  

(define (load-pc-relative-address/typed target type label)
  ;++ This is pretty horrid, especially since it happens for every
  ;++ continuation pushed!  None of the alternatives is much good.
  ;; Twenty bytes, but only three instructions and no extra memory.
  (let ((temp (temporary-register-reference)))
    (LAP (MOV Q ,temp (&U ,(make-non-pointer-literal type 0)))
	 (LEA Q ,target (@PCR ,label))
	 (OR Q ,target ,temp)))
  #|
  ;; Nineteen bytes, but rather complicated (and needs syntax for an
  ;; addressing mode not presently supported).
  (cond ((zero? type)
	 (LAP (LEA Q ,target (@PCR ,label))))
	((zero? (remainder type 8))
	 (receive (type-divisor scale scale-log)
	     (cond ((not (zero? (remainder type #x10))) (values 8 8 3))
		   ((not (zero? (remainder type #x20))) (values #x10 4 2))
		   ((not (zero? (remainder type #x40))) (values #x20 2 1))
		   (else (error "Type too large:" type)))
	   (let ((offset (quotient type type-divisor)))
	     (LAP (LEA Q ,target (@PCR ,label))
		  (LEA Q ,target (@OI ,offset ,target ,scale))
		  (ROR Q ,target (&U ,scale-log))))))
	(else ...))
  |#
  #|
  ;; This would be brilliant, except that it needs (PC * 2^6)-relative
  ;; addressing, rather than PC-relative addressing.
  (let* ((reference-point (generate-label 'PC))
	 (offset
	  `(+ ,type
	      (* ,(expt 2 scheme-type-width) (- ,label ,reference-point)))))
    (LAP (LEA Q ,target (@PCO ,offset))
	 (LABEL ,reference-point)
	 (ROR Q ,target (&U ,scheme-type-width))))
  |#
  #|
  ;; Nineteen bytes and no temporaries, but four instructions.
  (LAP (LEA Q ,target (@PCR ,label))
       (SHL Q ,target (&U ,scheme-type-width))
       (OR Q ,target (&U ,type))
       (ROR Q ,target (&U ,scheme-type-width)))
  |#
  #|
  ;; Seventeen bytes, but this requires reading eight bytes of memory.
  (let ((temp (temporary-register-reference))
	(literal (make-non-pointer-literal type 0)))
    (LAP (MOV Q ,temp (@PCR ,(allocate-unsigned-quad-label literal)))
	 (LEA Q ,target (@PCR ,label))
	 (OR Q ,target ,temp)))
  |#
  #|
  ;; Fourteen bytes and no temporaries, but this requires reading
  ;; eight bytes of memory.
  (let* ((reference-point (generate-label 'REFERENCE-POINT))
	 (expression
	  `(+ ,(make-non-pointer-literal type 0) (- ,label ,reference-point))))
    (LAP (LABEL ,reference-point)
	 (LEA Q ,target (@PCR ,reference-point))
	 (ADD Q ,target (@PCR ,(allocate-unsigned-quad-label expression)))))
  |#)

(define (load-char-into-register type source target)
  (let ((target (target-register-reference target)))
    (cond ((zero? type)
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
      (LAP (MOV Q ,(target-register-reference target) ,temp)))))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-offset?)
	  (REGISTER (? source)))
  (QUALIFIER (register-value-class=word? source))
  (with-detagged-vector-location expression source
    (lambda (temp)
      (LAP (MOV Q ,temp ,(source-register-reference source))))))

(define (with-detagged-vector-location rtl-expression protect recvr)
  (with-decoded-detagged-offset rtl-expression
    (lambda (base index offset)
      (with-indexed-address base index address-units-per-object
	  (* address-units-per-object offset)
	  protect
	recvr))))

(define (rtl:detagged-offset? expression)
  (and (rtl:offset? expression)
       (rtl:machine-constant? (rtl:offset-offset expression))
       (let ((base (rtl:offset-base expression)))
	 (and (rtl:offset-address? base)
	      (rtl:detagged-index? (rtl:offset-address-base base)
				   (rtl:offset-address-offset base))))
       expression))

(define (with-decoded-detagged-offset expression receiver)
  (let ((base (rtl:offset-base expression)))
    (let ((base* (rtl:offset-address-base base))
	  (index (rtl:offset-address-offset base)))
      (receiver
       (rtl:register-number (if (rtl:register? base*)
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