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

;;;; LAP Generation Rules: Data Transfers
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

(define (assign-register->register target source)
  (standard-move-to-target! source (register-type target) target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (REGISTER (? index))))
  (load-indexed-address target base index 4 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (REGISTER (? index))))
  (load-indexed-address target base index 1 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? index))))
  (load-indexed-address target base index 8 0))

(define-integrable (->areg reg)
  (- reg 8))

(define (load-indexed-address target base index scale offset)
  (let ((load-address
	 (lambda (get-target-reference)
	   (let ((ea (indexed-ea base index scale offset)))
	     (LAP (LEA ,ea ,(get-target-reference)))))))
    (cond ((or (not (machine-register? target))
	       (eq? (register-type target) 'ADDRESS))
	   (load-address
	    (lambda ()
	      (target-register-reference target 'ADDRESS))))
	  ((eq? (register-type target) 'DATA)
	   (let ((temp
		  (register-reference
		   (allocate-temporary-register! 'ADDRESS))))
	     (LAP ,@(load-address (lambda () temp))
		  (MOV L ,temp ,(register-reference target)))))
	  (else
	   (error "load-indexed-address: Unknown register type"
		  target)))))

(define (target-register-reference target type)
  (delete-dead-registers!)
  (register-reference
   (or (register-alias target type)
       (allocate-alias-register! target type))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? n))))
  (load-static-link target source (* 4 n) false))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? n))))
  (load-static-link target source n false))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? n))))
  (load-static-link target source (* 8 n) false))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source))
					(MACHINE-CONSTANT (? n)))))
  (load-static-link target source (* 4 n)
    (lambda (target)
      (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source))
					     (MACHINE-CONSTANT (? n)))))
  (load-static-link target source n
    (lambda (target)
      (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define (load-static-link target source n suffix)
  (cond ((and (not suffix) (zero? n))
	 (assign-register->register target source))
	((machine-register? target)
	 (let ((do-data
		(lambda (target)
		  (let ((source
			 (standard-register-reference source false true)))
		    (LAP (MOV L ,source ,target)
			 ,@(ea+=constant target n)
			 ,@(if suffix
			       (suffix target)
			       (LAP)))))))
	   (case (register-type target)
	     ((ADDRESS)
	      (if (not suffix)
		  (let ((source (allocate-indirection-register! source)))
		    (LAP (LEA ,(byte-offset-reference source n)
			      ,(register-reference target))))
		  (let ((temp (reference-temporary-register! 'DATA)))
		    (LAP ,(do-data temp)
			 (MOV L ,temp ,(register-reference target))))))
	     ((DATA)
	      (do-data (register-reference target)))
	     (else
	      (error "load-static-link: Unknown register type"
		     (register-type target))))))

	(else
	 (let ((non-reusable
		(cond ((not suffix)
		       (lambda ()
			 (let ((source
				(allocate-indirection-register! source)))
			   (delete-dead-registers!)
			   (let ((target (allocate-alias-register! target
								   'ADDRESS)))
			     (if (eqv? source target)
				 (increment-machine-register target n)
				 (LAP (LEA ,(byte-offset-reference source n)
					   ,(register-reference target))))))))
		      ((<= -128 n 127)
		       (lambda ()
			 (let ((source (register-reference source)))
			   (delete-dead-registers!)
			   (let ((target
				  (reference-target-alias! target 'DATA)))
			     (LAP (MOVEQ (& ,n) ,target)
				  (ADD L ,source ,target)
				  ,@(suffix target))))))
		      (else
		       (lambda ()
			 (let ((source (indirect-byte-reference! source n)))
			   (delete-dead-registers!)
			   (let ((temp
				  (reference-temporary-register! 'ADDRESS)))
			     (let ((target (reference-target-alias! target
								    'DATA)))
			       (LAP (LEA ,source ,temp)
				    (MOV L ,temp ,target)
				    ,@(suffix target))))))))))
	   (if (machine-register? source)
	       (non-reusable)
	       (reuse-pseudo-register-alias!
		source 'DATA
		(lambda (reusable-alias)
		  (delete-dead-registers!)
		  (add-pseudo-register-alias! target reusable-alias)
		  (LAP ,@(increment-machine-register reusable-alias n)
		       ,@(if suffix
			     (suffix (register-reference reusable-alias))
			     (LAP))))
		non-reusable))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  ;; See if we can reuse a source alias, because `object->type' can
  ;; sometimes do a slightly better job when the source and target are
  ;; the same register.
  (let ((no-reuse
	 (lambda ()
	   (let ((source (standard-register-reference source 'DATA false)))
	     (delete-dead-registers!)
	     (object->type source (reference-target-alias! target 'DATA))))))
    (if (machine-register? target)
	(no-reuse)
	(reuse-pseudo-register-alias! source 'DATA
	  (lambda (source)
	    (delete-dead-registers!)
	    (add-pseudo-register-alias! target source)
	    (let ((source (register-reference source)))
	      (object->type source source)))
	  no-reuse))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let ((temp (standard-move-to-temporary! type 'DATA)))
    (LAP (RO R L (& ,scheme-type-width) ,temp)
	 (OR L ,temp ,(standard-move-to-target! datum 'DATA target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (LAP (OR UL
	   (& ,(make-non-pointer-literal type 0))
	   ,(standard-move-to-target! datum 'DATA target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (object->datum (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (object->address (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (address->fixnum (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (object->fixnum (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (address->fixnum (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (fixnum->object (standard-move-to-target! source 'DATA target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (fixnum->address (standard-move-to-target! source 'DATA target)))

;;;; Loading Constants

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant source (standard-target-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? n)))
  (load-machine-constant n (standard-target-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (standard-target-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address
   target
   (rtl-procedure/external-label (label->object label))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address target label))

(define (load-pc-relative-address target label)
  (delete-dead-registers!)
  (LAP (LEA (@PCR ,label) ,(reference-target-alias! target 'ADDRESS))))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (load-pc-relative-address-with-type
   target
   type
   (rtl-procedure/external-label (label->object label))))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (load-pc-relative-address-with-type target type label))

(define (load-pc-relative-address-with-type target type label)
  (delete-dead-registers!)
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (reference-target-alias! target 'DATA)))
    (LAP (LEA (@PCR ,label) ,temp)
	 (MOV L ,temp ,target)
	 (OR UL (& ,(make-non-pointer-literal type 0)) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative target (free-reference-label name)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative target (free-assignment-label name)))

(define (load-pc-relative target label)
  (delete-dead-registers!)
  (LAP (MOV L (@PCR ,label) ,(reference-target-alias! target 'ADDRESS))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (convert-object/constant->register target constant object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (CONSTANT (? constant))))
  (convert-object/constant->register target constant object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (CONSTANT (? constant)))))
  (convert-object/constant->register target constant address->fixnum))

(define (convert-object/constant->register target constant conversion)
  (delete-dead-registers!)
  (let ((target (reference-target-alias! target 'DATA)))
    (if (non-pointer-object? constant)
	(load-non-pointer 0 (careful-object-datum constant) target)
	(LAP ,@(load-constant constant target)
	     ,@(conversion target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (delete-dead-registers!)
  (load-fixnum-constant constant (reference-target-alias! target 'DATA)))

;;;; Transfers from Memory

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? expression rtl:simple-offset?))
  (let ((source (offset->reference! expression)))
    (LAP (MOV L ,source ,(standard-target-reference target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(standard-target-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->TYPE (? expression rtl:simple-offset?)))
  (let ((source (offset->reference! expression)))
    (delete-dead-registers!)
    (object->type source (reference-target-alias! target 'DATA))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (? expression rtl:simple-offset?)))
  (convert-object/offset->register target expression object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->ADDRESS (? expression rtl:simple-offset?)))
  (convert-object/offset->register target expression object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM
	   (OBJECT->ADDRESS (? expression rtl:simple-offset?))))
  (convert-object/offset->register target expression address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (? expression rtl:simple-offset?)))
  (convert-object/offset->register target expression object->fixnum))

(define (convert-object/offset->register target expression conversion)
  (let ((source (offset->reference! expression)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (MOV L ,source ,target)
	   ,@(conversion target)))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV L
	    ,(standard-register-reference r false true)
	    ,(offset->reference! expression))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(offset->reference! expression))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONSTANT (? object)))
  (load-constant object (offset->reference! expression)))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (offset->reference! expression)))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(REGISTER (? datum))))
  (let ((target (offset->reference! expression)))
    (LAP (MOV L ,(standard-register-reference datum 'DATA true) ,target)
	 ,@(memory-set-type type target))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source))
					(MACHINE-CONSTANT (? n)))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (offset->reference! expression)))
    (LAP (LEA ,(indirect-reference! source n) ,temp)
	 (MOV L ,temp ,target)
	 ,@(memory-set-type type target))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source))
					     (MACHINE-CONSTANT (? n)))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (offset->reference! expression)))
    (LAP (LEA ,(indirect-byte-reference! source n) ,temp)
	 (MOV L ,temp ,target)
	 ,@(memory-set-type type target))))

;; Common case that can be done cheaply:

(define-rule statement
  (ASSIGN (? expression0 rtl:simple-offset?)
	  (BYTE-OFFSET-ADDRESS (? expression rtl:simple-offset?)
			       (MACHINE-CONSTANT (? n))))
  (QUALIFIER (equal? expression0 expression))
  (if (zero? n)
      (LAP)
      (let ((target (offset->reference! expression)))
	(cond ((<= 1 n 8)
	       (LAP (ADDQ L (& ,n) ,target)))
	      ((<= -8 n -1)
	       (LAP (SUBQ L (& ,(- n)) ,target)))
	      ((<= -128 n 127)
	       (let ((temp (reference-temporary-register! 'DATA)))
		 (LAP (MOVEQ (& ,n) ,temp)
		      (ADD L ,temp ,target))))
	      (else
	       (LAP (ADD L (& ,n) ,target)))))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (offset->reference! expression)))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,temp)
	 (MOV L ,temp ,target)
	 ,@(memory-set-type type target))))

#|
;; This is no better than assigning to a register and then assigning
;; from the register

(define-rule statement
  (ASSIGN (? expression rtl:simple-offset?)
	  (FIXNUM->OBJECT (REGISTER (? source))))
  (let ((target (offset->reference! expression)))
    (let ((temporary (standard-move-to-temporary! source 'DATA)))
      (LAP ,@(fixnum->object temporary)
	   (MOV L ,temporary ,target)))))
|#

(define-rule statement
  (ASSIGN (? expression0 rtl:simple-offset?)
	  (? expression1 rtl:simple-offset?))
  (if (equal? expression0 expression1)
      (LAP)
      (LAP (MOV L ,(offset->reference! expression1)
		,(offset->reference! expression0)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  (load-constant object (INST-EA (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (INST-EA (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV L ,(standard-register-reference r false true) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (? expression rtl:simple-offset?))
  (LAP (MOV L ,(offset->reference! expression) (@A+ 5))))

#|
;; This is no better than assigning to a register and then assigning
;; from the register

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (standard-move-to-temporary! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@A+ 5)))))
|#

(define-rule statement
  ;; This pops the top of stack into the heap
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) (@A+ 5))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV L ,(standard-register-reference r false true) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  (load-constant object (INST-EA (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (LAP (MOV L ,(standard-register-reference datum 'DATA true) (@-A 7))
       ,@(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (INST-EA (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (LAP (PEA (@PCR ,(rtl-procedure/external-label (label->object label))))
       ,@(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (LAP (PEA (@PCR ,label))
       ,@(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? r))
					(MACHINE-CONSTANT (? n)))))
  (LAP (PEA ,(indirect-reference! r n))
       ,@(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? r))
					     (MACHINE-CONSTANT (? n)))))
  (LAP (PEA ,(indirect-byte-reference! r n))
       ,@(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (? expression rtl:simple-offset?))
  (LAP (MOV L ,(offset->reference! expression) (@-A 7))))

#|
;; This is no better than assigning to a register and then assigning
;; from the register

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (standard-move-to-temporary! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@-A 7)))))
|#

;;;; Fixnum Operations

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
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
  (QUALIFIER (machine-operation-target? target))
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
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
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
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (? expression rtl:simple-offset?))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target expression))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (? expression rtl:simple-offset?))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target expression))

;;; end (IF (<= SCHEME-TYPE-WIDTH 6) ...)
))

;;; It doesn't hurt for these to be defined when the above rules are
;;; not in use.

(define (convert-index->fixnum/register target source)
  (reuse-and-load-machine-target! 'DATA target source
    (lambda (target)
      (LAP (AS L L (& ,(+ scheme-type-width 2)) ,target)))))

(define (convert-index->fixnum/offset target expression)
  (let ((source (offset->reference! expression)))
    (reuse-and-operate-on-machine-target! 'DATA target
      (lambda (target)
	(LAP (MOV L ,source ,target)
	     (AS L L (& ,(+ scheme-type-width 2)) ,target))))))

;;;; Flonum Operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (reference-alias-register! source 'FLOAT)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (MOV L (A 5) ,target)
	   (OR L (& ,(make-non-pointer-literal (ucode-type flonum) 0)) ,target)
	   ,@(load-non-pointer (ucode-type manifest-nm-vector)
			       2
			       (INST-EA (@A+ 5)))
	   (FMOVE D ,source (@A+ 5))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-move-to-temporary! source 'DATA))
	(temp (allocate-temporary-register! 'ADDRESS)))
    (LAP ,@(object->address source)
	 (MOV L ,source ,(register-reference temp))
	 (FMOVE D
		,(offset-reference temp 1)
		,(target-float-reference target)))))  

(define-rule statement
  (ASSIGN (? target)
	  (FLONUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
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
  (QUALIFIER (machine-operation-target? target))
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
  (delete-dead-registers!)
  (let ((target (reference-target-alias! target 'DATA)))
    (LAP ,@(load-non-pointer type 0 target)
	 (MOV B ,source ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (load-char-into-register 0
			   (reference-alias-register! source 'DATA)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (? expression rtl:simple-offset?)))
  (load-char-into-register 0
			   (offset->reference!/char expression)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? expression rtl:simple-byte-offset?))
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
	  (REGISTER (? source)))
  (LAP (MOV B ,(coerce->any/byte-reference source)
	    ,(byte-offset->reference! expression))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-byte-offset?)
	  (CHAR->ASCII (CONSTANT (? character))))
  (LAP (MOV B (& ,(char->signed-8-bit-immediate character))
	    ,(byte-offset->reference! expression))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-byte-offset?)
	  (CHAR->ASCII (REGISTER (? source))))
  (LAP (MOV B ,(coerce->any/byte-reference source)
	    ,(byte-offset->reference! expression))))

(define-rule statement
  (ASSIGN (? expression0 rtl:simple-byte-offset?)
	  (CHAR->ASCII (? expression1 rtl:simple-offset?)))
  (LAP (MOV B ,(offset->reference!/char expression1)
	    ,(byte-offset->reference! expression0))))

(define-rule statement
  (ASSIGN (? expression0 rtl:simple-byte-offset?)
	  (? expression1 rtl:simple-byte-offset?))
  (LAP (MOV B ,(byte-offset->reference! expression1)
	    ,(byte-offset->reference! expression0))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? expression rtl:simple-float-offset?))
  (let ((ea (float-offset->reference! expression)))
    (LAP (FMOVE D ,ea ,(target-float-reference target)))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-float-offset?)
	  (REGISTER (? source)))
  (LAP (FMOVE D ,(source-float-reference source)
	      ,(float-offset->reference! expression))))

(define (target-float-reference target)
  (delete-dead-registers!)
  (reference-target-alias! target 'FLOAT))

(define (source-float-reference source)
  (register-reference
   (or (register-alias source 'FLOAT)
       (allocate-alias-register! source 'FLOAT))))