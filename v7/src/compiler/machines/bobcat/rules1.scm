#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules1.scm,v 4.34 1991/01/23 21:34:30 jinx Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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
  (load-static-link target source (* 4 n) false))

(define-rule statement
  ;; This is an intermediate rule -- not intended to produce code.
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (load-static-link target source (* 4 n)
    (lambda (target)
      (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? n)))
  (load-static-link target source n false))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (load-static-link target source n
    (lambda (target)
      (LAP (OR UL (& ,(make-non-pointer-literal type 0)) ,target)))))

(define (load-static-link target source n suffix)
  (if (and (zero? n) (not suffix))
      (assign-register->register target source)
      (let ((non-reusable
	     (cond ((not suffix)
		    (lambda ()
		      (let ((source (allocate-indirection-register! source)))
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
			(let ((target (reference-target-alias! target 'DATA)))
			  (LAP (MOVEQ (& ,n) ,target)
			       (ADD L ,source ,target))))))
		   (else
		    (lambda ()
		      (let ((source (indirect-byte-reference! source n)))
			(delete-dead-registers!)
			(let ((temp (reference-temporary-register! 'ADDRESS)))
			  (let ((target (reference-target-alias! target
								 'DATA)))
			    (LAP (LEA ,source ,temp)
				 (MOV L ,temp ,target)
				 ,@(suffix target))))))))))
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
	      non-reusable)))))

(define (assign-register->register target source)
  (standard-move-to-target! source (register-type target) target)
  (LAP))

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
	  (OBJECT->TYPE (OFFSET (REGISTER (? address)) (? offset))))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    (object->type source (reference-target-alias! target 'DATA))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (OFFSET (REGISTER (? address)) (? offset))))
  (convert-object/offset->register target address offset object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->ADDRESS (OFFSET (REGISTER (? address)) (? offset))))
  (convert-object/offset->register target address offset object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM
	   (OBJECT->ADDRESS (OFFSET (REGISTER (? address)) (? offset)))))
  (convert-object/offset->register target address offset address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (OFFSET (REGISTER (? address)) (? offset))))
  (convert-object/offset->register target address offset object->fixnum))

(define (convert-object/offset->register target address offset conversion)
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (MOV L ,source ,target)
	   ,@(conversion target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (let ((source (indirect-reference! address offset)))
    (LAP (MOV L ,source ,(standard-target-reference target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(standard-target-reference target))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (load-constant object (indirect-reference! a n)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (indirect-reference! a n)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n)) (REGISTER (? r)))
  (QUALIFIER (register-value-class=word? r))
  (LAP (MOV L
	    ,(standard-register-reference r false true)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L (@A+ 7) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? datum))))
  (let ((target (indirect-reference! address offset)))
    (LAP (MOV L ,(standard-register-reference datum 'DATA true) ,target)
	 ,(memory-set-type type target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (indirect-reference! address offset)))
    (LAP (LEA ,(indirect-reference! source n) ,temp)
	 (MOV L ,temp ,target)
	 ,(memory-set-type type target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? n))))
  (let ((temp (reference-temporary-register! 'ADDRESS))
	(target (indirect-reference! address offset)))
    (LAP (LEA ,(indirect-byte-reference! source n) ,temp)
	 (MOV L ,temp ,target)
	 ,(memory-set-type type target))))

;; Common case that can be done cheaply:

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (BYTE-OFFSET-ADDRESS (OFFSET (REGISTER (? address)) (? offset))
			       (? n)))
  (if (zero? n)
      (LAP)
      (let ((target (indirect-reference! address offset)))
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
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
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
    (let ((temporary (standard-move-to-temporary! source 'DATA)))
      (LAP ,@(fixnum->object temporary)
	   (MOV L ,temporary ,target)))))

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
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (standard-move-to-temporary! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@A+ 5)))))

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
       ,(memory-set-type type (INST-EA (@A 7)))))

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
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (LAP (PEA (@PCR ,label))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(OFFSET-ADDRESS (REGISTER (? r)) (? n))))
  (LAP (PEA ,(indirect-reference! r n))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(BYTE-OFFSET-ADDRESS (REGISTER (? r)) (? n))))
  (LAP (PEA ,(indirect-byte-reference! r n))
       ,(memory-set-type type (INST-EA (@A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (let ((temporary (standard-move-to-temporary! r 'DATA)))
    (LAP ,@(fixnum->object temporary)
	 (MOV L ,temporary (@-A 7)))))

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
  (let ((source (reference-alias-register! source 'FLOAT)))
    (delete-dead-registers!)
    (let ((target (reference-target-alias! target 'DATA)))
      (LAP (MOV L (A 5) ,target)
	   (OR L (& ,(make-non-pointer-literal (ucode-type flonum) 0)) ,target)
	   ,@(load-non-pointer (ucode-type manifest-nm-vector)
			       flonum-size
			       (INST-EA (@A+ 5)))
	   (FMOVE D ,source (@A+ 5))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (@ADDRESS->FLOAT (REGISTER (? source))))
  (let ((source (indirect-reference! source 1)))
    (delete-dead-registers!)
    (LAP (FMOVE D ,source ,(reference-target-alias! target 'FLOAT)))))

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
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (load-char-into-register 0
			   (indirect-char/ascii-reference! address offset)
			   target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (load-char-into-register 0
			   (reference-alias-register! source 'DATA)
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