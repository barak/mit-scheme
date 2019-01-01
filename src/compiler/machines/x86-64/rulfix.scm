#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Fixnum operations.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Making and examining fixnums

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (address->fixnum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (object->fixnum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (address->fixnum (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (fixnum->object (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (fixnum->address (standard-move-to-target! source target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (CONSTANT (? constant)))))
  (load-converted-constant target constant address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-fixnum-constant constant (target-register-reference target)))

;;;; Fixnum Operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  overflow?				; ignored
  (fixnum-1-arg target source (fixnum-1-arg/operate operator)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  ((fixnum-2-args/operate operator) target source1 source2 overflow?))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER (or (and (not (eq? operator 'FIXNUM-QUOTIENT))
		      (not (eq? operator 'FIXNUM-REMAINDER)))
		 (integer-power-of-2? (abs constant))))
  (fixnum-2-args/register*constant operator target source constant overflow?))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-QUOTIENT
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? d)))
			 (? overflow?)))
  (QUALIFIER (not (or (zero? d) (integer-power-of-2? d))))
  overflow?				;ignore
  (fixnum-quotient/constant target source d))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-REMAINDER
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? d)))
			 (? overflow?)))
  (QUALIFIER (not (or (zero? d) (integer-power-of-2? d))))
  overflow?				;ignore
  (fixnum-remainder/constant target source d))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (fixnum-2-args/commutative? operator))
  (fixnum-2-args/register*constant operator target source constant overflow?))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT 0))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (not (fixnum-2-args/commutative? operator)))
  overflow?				; ignored
  (if (eq? operator 'MINUS-FIXNUM)
      (fixnum-1-arg target source (fixnum-1-arg/operate 'FIXNUM-NEGATE))
      (load-fixnum-constant 0 (target-register-reference target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f))
  (fixnum-1-arg target source
   (lambda (target)
     (multiply-fixnum-constant target (* n fixnum-1) #f))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (fixnum-1-arg target source
   (lambda (target)
     (multiply-fixnum-constant target (* n fixnum-1) #f))))

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (fixnum-branch! (fixnum-predicate/unary->binary predicate))
  (LAP (CMP Q ,(source-register-reference register) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OBJECT->FIXNUM (REGISTER (? register))))
  (QUALIFIER (or (eq? predicate 'NEGATIVE-FIXNUM?)
		 (eq? predicate 'ZERO-FIXNUM?)))
  (fixnum-branch! predicate)
  (object->fixnum (standard-move-to-temporary! register)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (? expression rtl:simple-offset?))
  (fixnum-branch! (fixnum-predicate/unary->binary predicate))
  (LAP (CMP Q ,(offset->reference! expression) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (REGISTER (? register-2)))
  (fixnum-branch! predicate)
  (compare/register*register register-1 register-2))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (? expression rtl:simple-offset?))
  (fixnum-branch! predicate)
  (LAP (CMP Q ,(source-register-reference register)
	    ,(offset->reference! expression))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (? expression rtl:simple-offset?)
		      (REGISTER (? register)))
  (fixnum-branch! predicate)
  (LAP (CMP Q ,(offset->reference! expression)
	    ,(source-register-reference register))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-branch! predicate)
  (compare/reference*fixnum (source-register-reference register) constant))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? register)))
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (compare/reference*fixnum (source-register-reference register) constant))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (? expression rtl:simple-offset?)
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-branch! predicate)
  (compare/reference*fixnum (offset->reference! expression) constant))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (? expression rtl:simple-offset?))
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (compare/reference*fixnum (offset->reference! expression) constant))

(define (compare/reference*fixnum reference fixnum)
  (with-signed-immediate-operand (* fixnum fixnum-1)
    (lambda (operand)
      (LAP (CMP Q ,reference ,operand)))))

;; This assumes that the immediately preceding instruction sets the
;; condition code bits correctly.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches! (lambda (label) (LAP (JO (@PCR ,label))))
			 (lambda (label) (LAP (JNO (@PCR ,label)))))
  (LAP))

;;;; Utilities

(define (object->fixnum target)
  (LAP (SAL Q ,target (&U ,scheme-type-width))))

(define (fixnum->object target)
  (LAP (OR Q ,target (&U ,(ucode-type FIXNUM)))
       (ROR Q ,target (&U ,scheme-type-width))))

(define (address->fixnum target)
  (LAP (SAL Q ,target (&U ,scheme-type-width))))

(define (fixnum->address target)
  (LAP (SHR Q ,target (&U ,scheme-type-width))))

(define-integrable fixnum-1 64)		; (expt 2 scheme-type-width) ***

(define-integrable fixnum-bits-mask
  (-1+ fixnum-1))

(define (word->fixnum target)
  (LAP (AND Q ,target (& ,(fix:not fixnum-bits-mask)))))

(define (integer-power-of-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) #f)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define (load-fixnum-constant constant target)
  (load-signed-immediate target (* constant fixnum-1)))

(define (add-fixnum-constant target constant overflow?)
  (let ((value (* constant fixnum-1)))
    (cond ((and (zero? value) (not overflow?))
	   (LAP))
	  ((and (not (fits-in-signed-byte? value))
		(fits-in-signed-byte? (- value)))
	   (LAP (SUB Q ,target (& ,(- value)))))
	  (else
	   (with-signed-immediate-operand value
	     (lambda (operand)
	       (LAP (ADD Q ,target ,operand))))))))

(define (multiply-fixnum-constant target constant overflow?)
  (cond ((zero? constant)
	 (load-fixnum-constant 0 target))
	((= constant 1)
	 (if (not overflow?)
	     (LAP)
	     (add-fixnum-constant target 0 overflow?)))
	((= constant -1)
	 (LAP (NEG Q ,target)))
	((and (not overflow?)
	      (integer-power-of-2? (abs constant)))
	 =>
	 (lambda (expt-of-2)
	   (if (negative? constant)
	       (LAP (SAL Q ,target (&U ,expt-of-2))
		    (NEG Q ,target))
	       (LAP (SAL Q ,target (&U ,expt-of-2))))))
	;; It is tempting to use WITH-SIGNED-IMMEDIATE-OPERAND here to
	;; get an operand for an otherwise common IMUL instruction,
	;; but ternary IMUL takes a 32-bit immediate, whereas binary
	;; IMUL takes an r/m and not an immediate, so these really
	;; must be different cases.
	((fits-in-signed-long? constant)
	 ;; target must be a register!
	 (LAP (IMUL Q ,target ,target (& ,constant))))
	(else
	 (let ((temp (temporary-register-reference)))
	   (LAP (MOV Q ,temp (& ,constant))
		(IMUL Q ,target ,temp))))))

;;;; Operation tables

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-integrable (fixnum-1-arg/operate operator)
  (lookup-arithmetic-method operator fixnum-methods/1-arg))

(define-integrable (fixnum-1-arg target source operation)
  (operation (standard-move-to-target! source target)))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-integrable (fixnum-2-args/operate operator)
  (lookup-arithmetic-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args-constant
  (list 'FIXNUM-METHODS/2-ARGS-CONSTANT))

(define-integrable (fixnum-2-args/operate-constant operator)
  (lookup-arithmetic-method operator fixnum-methods/2-args-constant))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))

(define ((fixnum-2-args/standard commutative? operate) target source1
						       source2 overflow?)
  overflow?				; ignored
  (binary-register-operation operate commutative? 'GENERAL
			     (lambda (target source)
			       (LAP (MOV Q ,target ,source)))
			     target source1 source2))

(define (fixnum-2-args/register*constant operator target
					 source constant overflow?)
  (fixnum-1-arg
   target source
   (lambda (target)
     ((fixnum-2-args/operate-constant operator) target constant overflow?))))

;;;; Arithmetic operations

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target 1 #f)))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target -1 #f)))

(define-arithmetic-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (target)
    (LAP (NOT Q ,target)
	 ,@(word->fixnum target))))

(define-arithmetic-method 'FIXNUM-NEGATE fixnum-methods/1-arg
  (lambda (target)
    (LAP (NEG Q ,target))))

(let-syntax
    ((binary-operation
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((name (list-ref form 1))
	       (instr (list-ref form 2))
	       (commutative? (list-ref form 3))
	       (idempotent? (list-ref form 4)))
	   `(define-arithmetic-method ',name fixnum-methods/2-args
	      (fixnum-2-args/standard
	       ,commutative?
	       (lambda (target source2)
		 (if (and ,idempotent? (equal? target source2))
		     (LAP)
		     (LAP (,instr Q ,',target ,',source2)))))))))))

  #| (binary-operation PLUS-FIXNUM ADD #t #f) |#
  (binary-operation MINUS-FIXNUM SUB #f #f)
  (binary-operation FIXNUM-AND AND #t #t)
  (binary-operation FIXNUM-OR OR #t #t)
  (binary-operation FIXNUM-XOR XOR #t #f))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (let* ((operate
	  (lambda (target source2)
	    (LAP (ADD Q ,target ,source2))))
	 (standard (fixnum-2-args/standard #t operate)))

  (lambda (target source1 source2 overflow?)
    (if overflow?
	(standard target source1 source2 overflow?)
	(let ((one (register-alias source1 'GENERAL))
	      (two (register-alias source2 'GENERAL)))
	  (cond ((not (and one two))
		 (standard target source1 source2 overflow?))
		((register-copy-if-available source1 'GENERAL target)
		 =>
		 (lambda (get-tgt)
		   (operate (get-tgt) (register-reference two))))
		((register-copy-if-available source2 'GENERAL target)
		 =>
		 (lambda (get-tgt)
		   (operate (get-tgt) (register-reference one))))
		(else
		 (let ((target (target-register-reference target)))
		   (LAP (LEA Q ,target (@RI ,one ,two 1)))))))))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args
  (fixnum-2-args/standard
   #f
   (lambda (target source2)
     (if (equal? target source2)
	 (load-fixnum-constant 0 target)
	 (let ((temp (temporary-register-reference)))
	   (LAP ,@(if (equal? temp source2)
		      (LAP)
		      (LAP (MOV Q ,temp ,source2)))
		(NOT Q ,temp)
		(AND Q ,target ,temp)))))))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (fixnum-2-args/standard
   #f
   (lambda (target source2)
     (cond ((not (equal? target source2))
	    (LAP (SAR Q ,target (&U ,scheme-type-width))
		 (IMUL Q ,target ,source2)))
	   ((even? scheme-type-width)
	    (LAP (SAR Q ,target (&U ,(quotient scheme-type-width 2)))
		 (IMUL Q ,target ,target)))
	   (else
	    (let ((temp (temporary-register-reference)))
	      (LAP (MOV Q ,temp ,target)
		   (SAR Q ,target (&U ,scheme-type-width))
		   (IMUL Q ,target ,temp))))))))

;;; This calls an out-of-line assembly hook because it requires a lot
;;; of hair to deal with shift counts that exceed the datum width, and
;;; with negative arguments.

(define-arithmetic-method 'FIXNUM-LSH fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    overflow?				;ignore
    ;++ This is suboptimal in the cases when SOURCE1 is stored only in
    ;++ rcx or when SOURCE2 is stored only in rax, and either one is
    ;++ dead (which is often the case).  In such cases, this generates
    ;++ code to needlessly save the dead pseudo-registers into their
    ;++ homes simply because they were stored in rax and rcx.  It'd be
    ;++ nice to have a variant of LOAD-MACHINE-REGISTER! for multiple
    ;++ sources and targets, which would compute a parallel assignment
    ;++ using machine registers if available for temporaries, or the
    ;++ homes of pseudo-registers if not.
    (let* ((load-rax (load-machine-register! source1 rax))
	   (load-rcx (load-machine-register! source2 rcx)))
      (delete-dead-registers!)
      (rtl-target:=machine-register! target rax)
      (LAP ,@load-rax
	   ,@load-rcx
	   ;; Clearing the map is not necessary because the hook uses
	   ;; only rax and rcx.  If the hook were changed, it would be
	   ;; necessary to clear the map first.
	   ,@(invoke-hook/subroutine entry:compiler-fixnum-shift)))))

(define (do-division target source1 source2 result-reg)
  (prefix-instructions! (load-machine-register! source1 rax))
  (need-register! rax)
  (require-register! rdx)
  (rtl-target:=machine-register! target result-reg)
  (let ((source2 (any-reference source2)))
    ;; Before IDIV, the high (most significant) half of the 128-bit
    ;; dividend is in RDX, and the low (least significant) half is in
    ;; RAX.  After, the quotient is in RAX, and the remainder in RDX.
    ;; First we fill RDX with the sign of RAX.
    (LAP (CSE Q (R ,rdx) (R ,rax))
	 (IDIV Q ((R ,rdx) : (R ,rax)) ,source2))))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    overflow?				; ignored
    (if (= source2 source1)
	(load-fixnum-constant 1 (target-register-reference target))
	(LAP ,@(do-division target source1 source2 rax)
	     (SAL Q (R ,rax) (&U ,scheme-type-width))))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    overflow?				; ignored
    (if (= source2 source1)
	(load-fixnum-constant 0 (target-register-reference target))
	(do-division target source1 source2 rdx))))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (add-fixnum-constant target n overflow?)))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (add-fixnum-constant target (- 0 n) overflow?)))

(define-arithmetic-method 'FIXNUM-OR fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant -1 target))
	  (else
	   (with-signed-immediate-operand (* n fixnum-1)
	     (lambda (operand)
	       (LAP (OR Q ,target ,operand))))))))

(define-arithmetic-method 'FIXNUM-XOR fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (LAP (NOT Q ,target)
		,@(word->fixnum target)))
	  (else
	   (with-signed-immediate-operand (* n fixnum-1)
	     (lambda (operand)
	       (LAP (XOR Q ,target ,operand))))))))

(define-arithmetic-method 'FIXNUM-AND fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (load-fixnum-constant 0 target))
	  ((= n -1)
	   (LAP))
	  (else
	   (with-signed-immediate-operand (* n fixnum-1)
	     (lambda (operand)
	       (LAP (AND Q ,target ,operand))))))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant 0 target))
	  (else
	   (with-signed-immediate-operand (* (- -1 n) fixnum-1)
	     (lambda (operand)
	       (LAP (AND Q ,target ,operand))))))))

(define-arithmetic-method 'FIXNUM-LSH fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((not (<= (- 0 scheme-datum-width) n scheme-datum-width))
	   (load-fixnum-constant 0 target))
	  ((not (negative? n))
	   (LAP (SHL Q ,target (&U ,n))))
	  (else
	   (LAP (SHR Q ,target (&U ,(- 0 n)))
		,@(word->fixnum target))))))

;; (+ x y <const>)

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (REGISTER (? source1))
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 #f))
  (QUALIFIER (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (lea-addition target source1 source2 1 addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 (REGISTER (? source1))
			 #f))
  (QUALIFIER (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (lea-addition target source1 source2 1 addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (REGISTER (? source1))
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  #f)))
  (QUALIFIER (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (lea-addition->object target source1 source2 1 addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  (REGISTER (? source1))
			  #f)))
  (QUALIFIER (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (lea-addition->object target source1 source2 1 addend))

;; (+ x (* y {1,2,4,8}))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (REGISTER (? source1))
			 (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? scale)))
					#f)
			 #f))
  (QUALIFIER (memv scale '(1 2 4 8)))
  (lea-addition target source1 source2 scale 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? scale)))
					#f)
			 (REGISTER (? source1))
			 #f))
  (QUALIFIER (memv scale '(1 2 4 8)))
  (lea-addition target source1 source2 scale 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (REGISTER (? source1))
			  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? scale)))
					 #f)
			  #f)))
  (QUALIFIER (memv scale '(1 2 4 8)))
  (lea-addition->object target source1 source2 scale 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? scale)))
					 #f)
			  (REGISTER (? source1))
			  #f)))
  (QUALIFIER (memv scale '(1 2 4 8)))
  (lea-addition->object target source1 source2 scale 0))

;; (+ x (lsh y {0,1,2,3}))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (REGISTER (? source1))
			 (FIXNUM-2-ARGS FIXNUM-LSH
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? power)))
					#f)
			 #f))
  (QUALIFIER (memv power '(0 1 2 3)))
  (lea-addition target source1 source2 (expt 2 power) 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS FIXNUM-LSH
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? power)))
					#f)
			 (REGISTER (? source1))
			 #f))
  (QUALIFIER (memv power '(0 1 2 3)))
  (lea-addition target source1 source2 (expt 2 power) 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (REGISTER (? source1))
			  (FIXNUM-2-ARGS FIXNUM-LSH
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? power)))
					 #f)
			  #f)))
  (QUALIFIER (memv power '(0 1 2 3)))
  (lea-addition->object target source1 source2 (expt 2 power) 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS FIXNUM-LSH
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? power)))
					 #f)
			  (REGISTER (? source1))
			  #f)))
  (QUALIFIER (memv power '(0 1 2 3)))
  (lea-addition->object target source1 source2 (expt 2 power) 0))

;; (+ x (* y {1,2,4,8}) <const>)

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source1))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? scale)))
					#f)
			 #f))
  (QUALIFIER
   (and (memv scale '(1 2 4 8))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition target source1 source2 scale addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? scale)))
					#f)
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source1))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 #f))
  (QUALIFIER
   (and (memv scale '(1 2 4 8))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition target source1 source2 scale addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source1))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? scale)))
					 #f)
			  #f)))
  (QUALIFIER
   (and (memv scale '(1 2 4 8))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition->object target source1 source2 scale addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? scale)))
					 #f)
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source1))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  #f)))
  (QUALIFIER
   (and (memv scale '(1 2 4 8))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition->object target source1 source2 scale addend))

;; (+ x (lsh y {0,1,2,3}) <const>)

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source1))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 (FIXNUM-2-ARGS FIXNUM-LSH
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? power)))
					#f)
			 #f))
  (QUALIFIER
   (and (memv power '(0 1 2 3))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition target source1 source2 (expt 2 power) addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS PLUS-FIXNUM
			 (FIXNUM-2-ARGS FIXNUM-LSH
					(REGISTER (? source2))
					(OBJECT->FIXNUM (CONSTANT (? power)))
					#f)
			 (FIXNUM-2-ARGS PLUS-FIXNUM
					(REGISTER (? source1))
					(OBJECT->FIXNUM (CONSTANT (? addend)))
					#f)
			 #f))
  (QUALIFIER
   (and (memv power '(0 1 2 3))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition target source1 source2 (expt 2 power) addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source1))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  (FIXNUM-2-ARGS FIXNUM-LSH
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? power)))
					 #f)
			  #f)))
  (QUALIFIER
   (and (memv power '(0 1 2 3))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition->object target source1 source2 (expt 2 power) addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (FIXNUM-2-ARGS FIXNUM-LSH
					 (REGISTER (? source2))
					 (OBJECT->FIXNUM (CONSTANT (? power)))
					 #f)
			  (FIXNUM-2-ARGS PLUS-FIXNUM
					 (REGISTER (? source1))
					 (OBJECT->FIXNUM (CONSTANT (? addend)))
					 #f)
			  #f)))
  (QUALIFIER
   (and (memv power '(0 1 2 3))
	(fits-in-signed-long? (shift-left addend scheme-type-width))))
  (lea-addition->object target source1 source2 (expt 2 power) addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (REGISTER (? source1))
			  (REGISTER (? source2))
			  #f)))
  (lea-addition->object target source1 source2 1 0))

(define (lea-addition target source1 source2 scale addend)
  (assert (memv scale '(1 2 4 8)))
  (assert (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (let ((offset (shift-left addend scheme-type-width)))
    (with-lea-addition target source1 offset source2 scale
      (lambda (target addition)
	target				;ignore
	addition))))

(define (lea-addition->object target source1 source2 scale addend)
  (assert (memv scale '(1 2 4 8)))
  (assert (fits-in-signed-long? (shift-left addend scheme-type-width)))
  (let ((offset (+ (shift-left addend scheme-type-width) type-code:fixnum)))
    (with-lea-addition target source1 offset source2 scale
      (lambda (target addition)
	(LAP ,@addition
	     (ROR Q ,target (&U ,scheme-type-width)))))))

(define (with-lea-addition target source1 offset source2 scale operate)
  (assert (memv scale '(1 2 4 8)))
  (assert (fits-in-signed-long? offset))
  (define (two-way target source)
    (operate
     (register-reference target)
     (cond ((and (zero? offset) (= scale 1))
	    (LAP (ADD Q (R ,target) (R ,source))))
	   ((zero? offset)
	    (LAP (LEA Q (R ,target) (@RI ,target ,source ,scale))))
	   (else
	    (LAP (LEA Q (R ,target) (@ROI ,target ,offset ,source ,scale)))))))
  (define (three-way target source1 source2)
    (operate
     (register-reference target)
     (if (zero? offset)
	 (LAP (LEA Q (R ,target) (@RI ,source1 ,source2 ,scale)))
	 (LAP (LEA Q (R ,target) (@ROI ,source1 ,offset ,source2 ,scale))))))
  (let* ((alias1 (register-alias source1 'GENERAL))
	 (alias2 (register-alias source2 'GENERAL)))
    ;; If one of the sources has no aliases, just load from its home
    ;; into a new alias for the target.
    (cond ((not alias1)
	   (let* ((source (or alias2 (load-alias-register! source2 'GENERAL)))
		  (target (move-to-alias-register! source1 'GENERAL target)))
	     (two-way target source)))
	  ((not alias2)
	   (let* ((source alias1)
		  (target (move-to-alias-register! source2 'GENERAL target)))
	     (two-way target source)))
	  ;; If one of the sources has an extra alias, let that one
	  ;; become an alias for the target instead.
	  ((register-copy-if-available source1 'GENERAL target)
	   => (lambda (get-target)
		(two-way (reference->register (get-target)) alias2)))
	  ((register-copy-if-available source2 'GENERAL target)
	   => (lambda (get-target)
		(two-way (reference->register (get-target)) alias1)))
	  ;; No copies of the sources available, so allocate an alias
	  ;; for the target and use the three-operand version.
	  (else
	   (three-way (target-register target) alias1 alias2)))))

(define (reference->register reference)
  (if (not (and (pair? reference)
		(eq? 'R (car reference))
		(pair? (cdr reference))
		(null? (cddr reference))))
      (error "Invalid register reference:" reference))
  (cadr reference))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (add-immediate-and-tag target source n #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS PLUS-FIXNUM
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (REGISTER (? source))
			  #f)))
  (add-immediate-and-tag target source n #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-1-ARG ONE-PLUS-FIXNUM
			 (REGISTER (? source))
			 #f)))
  (add-immediate-and-tag target source 1 #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS MINUS-FIXNUM
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (add-immediate-and-tag target source (- n) #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS MINUS-FIXNUM
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (REGISTER (? source))
			  #f)))
  (add-immediate-and-tag target source n #t))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-1-ARG MINUS-ONE-PLUS-FIXNUM
			 (REGISTER (? source))
			 #f)))
  (add-immediate-and-tag target source -1 #f))

(define (add-immediate-and-tag target source n negate?)
  (fixnum-1-arg target source
    (lambda (target)
      (let ((n (+ (shift-left n scheme-type-width) type-code:fixnum)))
	(define (add) (LAP (ADD Q ,target (& ,n))))
	(define (sub) (LAP (SUB Q ,target (& ,(- n)))))
	(define (general)
	  (with-signed-immediate-operand n
	    (lambda (operand)
	      (LAP (ADD Q ,target ,operand)))))
	(LAP ,@(if negate? (LAP (NEG Q ,target)) (LAP))
	     ,@(cond ((fits-in-signed-byte? n) (add))
		     ((fits-in-signed-byte? (- n)) (sub))
		     ((fits-in-signed-long? n) (add))
		     ((fits-in-signed-long? (- n)) (sub))
		     (else (general)))
	     (ROR Q ,target (&U ,scheme-type-width)))))))

;;; NOT tricks

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG FIXNUM-NOT (OBJECT->FIXNUM (REGISTER (? source))) #f))
  ;; Save a word->fixnum instruction: complement the word first before
  ;; shifting off the tag, so that we'll get zeros rather than ones in
  ;; the low order bits, as we want.
  (let ((target (standard-move-to-target! source target)))
    (LAP (NOT Q ,target)
	 (SAL Q ,target (&U ,scheme-type-width)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT (FIXNUM-1-ARG FIXNUM-NOT (REGISTER (? source)) #f)))
  ;; Use XOR with a sign-extended immediate operand to simultaneously
  ;; (a) complement the value of the fixnum, and (b) set the tag bits.
  (let ((target (standard-move-to-target! source target))
	(magic-bits
	 (+ (* -1 (expt 2 scheme-type-width)) type-code:fixnum)))
    (assert (< magic-bits 0))		;Sign-extend with 1 to complement.
    (LAP (XOR Q ,target (& ,magic-bits))
	 (ROR Q ,target (&U ,scheme-type-width)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-1-ARG FIXNUM-NOT
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  ;; XOR with the datum mask to complement the value of the fixnum
  ;; without affecting its tag.
  (let ((target (standard-move-to-target! source target)))
    (LAP (XOR Q ,target (R ,regnum:datum-mask)))))

;;; OR tricks, part 1: mostly intermediate rules en route to good stuff.

;; XXX Can we arrange to sort operands into a canonical order (say,
;; constant first, then register) so that we don't need to duplicate
;; every rule for commutative operations?

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-OR
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 (REGISTER (? untagged-source))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-ior target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-OR
			 (REGISTER (? untagged-source))
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-ior target tagged-source untagged-source))

(define (detag-and-ior target tagged-source untagged-source)
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((untagged-source (source-register-reference untagged-source))
	 (target (standard-move-to-target! tagged-source target)))
    (LAP ,@(object->fixnum target)
	 (OR Q ,target ,untagged-source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-OR
			 (OBJECT->FIXNUM (REGISTER (? source1)))
			 (OBJECT->FIXNUM (REGISTER (? source2)))
			 #f))
  ;; Save a shift by ORing the tagged data first.
  ((lambda (operate)
     ((fixnum-2-args/standard #t operate) target source1 source2 #f))
   (lambda (target source)
     (LAP (OR Q ,target ,source)
	  ,@(object->fixnum target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-OR
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (detag-and-ior-immediate target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-OR
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f))
  (detag-and-ior-immediate target source n))

(define (detag-and-ior-immediate target source n)
  ;; Take advantage of the facts that we don't care what happens to the
  ;; tag bits (which are all set to 1 if n is negative) and that
  ;; unshifted operands are smaller.
  (let ((target (standard-move-to-target! source target)))
    (if (= n 0)
	(object->fixnum target)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (OR Q ,target ,operand)
		 ;; If n is negative, then we can use a short
		 ;; sign-extended operand, but the tag will not be
		 ;; intact so we can't rightly use OBJECT->FIXNUM --
		 ;; the peephole optimizer might get confused by it.
		 ,@(if (< n 0)
		       (LAP (SAL Q ,target (&U ,scheme-type-width)))
		       (object->fixnum target))))))))

;;; OR tricks, part 2: detag/tag on registers.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  (REGISTER (? untagged-source))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-ior-tag target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (REGISTER (? untagged-source))
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-ior-tag target tagged-source untagged-source))

(define (detag-ior-tag target tagged-source untagged-source)
  ;; Take advantage of the tag in the tagged source to fill the
  ;; zero-filled bits from shifting the untagged source into position.
  (assert compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((tagged-source (source-register-reference tagged-source))
	 (target (standard-move-to-target! untagged-source target)))
    (LAP (SHR Q ,target (&U ,scheme-type-width))
	 (OR Q ,target ,tagged-source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (OBJECT->FIXNUM (REGISTER (? source1)))
			  (OBJECT->FIXNUM (REGISTER (? source2)))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  ;; OR on two fixnum tags conveniently preserves them.
  ((lambda (operate)
     ((fixnum-2-args/standard #t operate) target source1 source2 #f))
   (lambda (target source)
     (LAP (OR Q ,target ,source)))))

;;; OR tricks, part 3: taking advantage of constants.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (ior-immediate-and-tag target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (REGISTER (? source))
			  #f)))
  (ior-immediate-and-tag target source n))

(define (ior-immediate-and-tag target source n)
  ;; Use the low-order bits of the immediate with OR to set the tag
  ;; before rotating it into place.
  (let ((target (standard-move-to-target! source target)))
    (with-signed-immediate-operand
	(bitwise-ior (shift-left n scheme-type-width) type-code:fixnum)
      (lambda (operand)
	(LAP (OR Q ,target ,operand)
	     (ROR Q ,target (&U ,scheme-type-width)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(<= 0 n)))			;Sign-extension must not set tag bits.
  (ior-immediate-in-place target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-OR
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(<= 0 n)))			;Sign-extension must not set tag bits.
  (ior-immediate-in-place target source n))

(define (ior-immediate-in-place target source n)
  (assert compiler:assume-safe-fixnums?)
  (assert (<= 0 n))		 ;Sign-extension must not set tag bits.
  ;; OR preserves the tag, as long as the operand is not sign-extended
  ;; into setting all the tag bits.
  (let ((target (standard-move-to-target! source target)))
    (if (= n 0)
	(LAP)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (OR Q ,target ,operand)))))))

;;; XOR tricks, part 1

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-XOR
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 (REGISTER (? untagged-source))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-xor target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-XOR
			 (REGISTER (? untagged-source))
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-xor target tagged-source untagged-source))

(define (detag-and-xor target tagged-source untagged-source)
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((untagged-source (source-register-reference untagged-source))
	 (target (standard-move-to-target! tagged-source target)))
    (LAP ,@(object->fixnum target)
	 (XOR Q ,target ,untagged-source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  (REGISTER (? untagged-source))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-xor-tag target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (REGISTER (? untagged-source))
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-xor-tag target tagged-source untagged-source))

(define (detag-xor-tag target tagged-source untagged-source)
  ;; Take advantage of the tag in the tagged source and the
  ;; zero-filling of shr in the untagged source, tag xor 0 = tag, to
  ;; save some shifts, or's, and rotates.
  (assert compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((tagged-source (source-register-reference tagged-source))
	 (target (standard-move-to-target! untagged-source target)))
    (LAP (SHR Q ,target (&U ,scheme-type-width))
	 (XOR Q ,target ,tagged-source))))

;;; XOR tricks, part 2

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-XOR
			 (OBJECT->FIXNUM (REGISTER (? source1)))
			 (OBJECT->FIXNUM (REGISTER (? source2)))
			 #f))
  ;; Save a shift by XORing the tagged data first.  This zeros the tag,
  ;; but we don't care because we're going to drop it anyway.
  ((lambda (operate)
     ((fixnum-2-args/standard #t operate) target source1 source2 #f))
   (lambda (target source)
     (LAP (XOR Q ,target ,source)
	  ,@(object->fixnum target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-XOR
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (detag-and-xor-immediate target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-XOR
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f))
  (detag-and-xor-immediate target source n))

(define (detag-and-xor-immediate target source n)
  ;; Take advantage of the facts that we don't care what happens to the
  ;; tag bits (which are all toggled if n is negative) and that
  ;; unshifted operands are smaller.
  (let ((target (standard-move-to-target! source target)))
    (if (= n 0)
	(object->fixnum target)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (XOR Q ,target ,operand)
		 ;; If n is negative, then we can use a short
		 ;; sign-extended operand, but the tag will not be
		 ;; intact so we can't rightly use OBJECT->FIXNUM --
		 ;; the peephole optimizer might get confused by it.
		 ,@(if (< n 0)
		       (LAP (SAL Q ,target (&U ,scheme-type-width)))
		       (object->fixnum target))))))))

;;; XOR tricks, part 3

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (xor-immediate-and-tag target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (REGISTER (? source))
			  #f)))
  (xor-immediate-and-tag target source n))

(define (xor-immediate-and-tag target source n)
  ;; Use one XOR to do the user's bidding and set the tag, since the
  ;; low bits are guaranteed to be zero at this point; then it's just a
  ;; matter of a single rotate to turn it into a tagged object.
  (let ((target (standard-move-to-target! source target)))
    (with-signed-immediate-operand
	(bitwise-xor (shift-left n scheme-type-width) type-code:fixnum)
      (lambda (operand)
	(LAP (XOR Q ,target ,operand)
	     (ROR Q ,target (&U ,scheme-type-width)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(<= 0 n)))			;Must not toggle tag.
  (xor-immediate-in-place target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-XOR
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(<= 0 n)))			;Must not toggle tag.
  (xor-immediate-in-place target source n))

(define (xor-immediate-in-place target source n)
  (assert compiler:assume-safe-fixnums?)
  (assert (<= 0 n))		   ;Sign-extension must not toggle tag.
  ;; Take advantage of the facts that positive xor with any
  ;; fixnum-sized integer will preserve the tag, and that unshifted
  ;; operands are smaller.
  (let ((target (standard-move-to-target! source target)))
    (if (= n 0)
	(LAP)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (XOR Q ,target ,operand)))))))

;;; AND tricks, part 1: mostly intermediate rules en route to good stuff.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-AND
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 (REGISTER (? untagged-source))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-and target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-AND
			 (REGISTER (? untagged-source))
			 (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			 #f))
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-and target tagged-source untagged-source))

(define (detag-and-and target tagged-source untagged-source)
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((untagged-source (source-register-reference untagged-source))
	 (target (standard-move-to-target! tagged-source target)))
    (LAP ,@(object->fixnum target)
	 (AND Q ,target ,untagged-source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-AND
			 (OBJECT->FIXNUM (REGISTER (? source1)))
			 (OBJECT->FIXNUM (REGISTER (? source2)))
			 #f))
  ;; Save a shift by ANDing the tagged data first.
  ((lambda (operate)
     ((fixnum-2-args/standard #t operate) target source1 source2 #f))
   (lambda (target source)
     (LAP (AND Q ,target ,source)
	  ,@(object->fixnum target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-AND
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (detag-and-and-immediate target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-AND
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f))
  (detag-and-and-immediate target source n))

(define (detag-and-and-immediate target source n)
  ;; Take advantage of the facts that we don't care what happens to the
  ;; tag bits (which are all cleared if n is nonnegative) and that
  ;; unshifted operands are smaller.
  (let ((target (standard-move-to-target! source target)))
    (if (= n -1)
	(object->fixnum target)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (AND Q ,target ,operand)
		 ;; If n is nonnegative, the tag will not be intact, so
		 ;; we can't rightly use OBJECT->FIXNUM -- the peephole
		 ;; optimizer might get confused by it.
		 ,@(if (<= 0 n)
		       (LAP (SAL Q ,target (&U ,scheme-type-width)))
		       (object->fixnum target))))))))

;;; AND tricks, part 2: detag/tag on registers.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  (REGISTER (? untagged-source))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-tag target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (REGISTER (? untagged-source))
			  (OBJECT->FIXNUM (REGISTER (? tagged-source)))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (detag-and-tag target tagged-source untagged-source))

(define (detag-and-tag target tagged-source untagged-source)
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert compiler:assume-safe-fixnums?)
  (assert (not (eqv? tagged-source untagged-source)))
  (let* ((untagged-source (source-register-reference untagged-source))
	 (target (standard-move-to-target! tagged-source target)))
    (LAP ,@(object->fixnum target)
	 (AND Q ,target ,untagged-source)
	 ,@(fixnum->object target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (OBJECT->FIXNUM (REGISTER (? source1)))
			  (OBJECT->FIXNUM (REGISTER (? source2)))
			  #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  ;; AND on two fixnum tags conveniently preserves them.
  ((lambda (operate)
     ((fixnum-2-args/standard #t operate) target source1 source2 #f))
   (lambda (target source)
     (LAP (AND Q ,target ,source)))))

;;; AND tricks, part 3: taking advantage of constants.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n))))))
  (and-immediate-and-tag target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (REGISTER (? source)))))
  (and-immediate-and-tag target source n))

(define (and-immediate-and-tag target source n)
  ;; Intermediate rule -- no advantage but paves way to composition.
  (let ((target (standard-move-to-target! source target)))
    (if (= n -1)
	(fixnum->object target)
	(with-signed-immediate-operand (shift-left n scheme-type-width)
	  (lambda (operand)
	    (LAP (AND Q ,target ,operand)
		 ,@(fixnum->object target)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
        (< n 0)))		    ;Sign-extension must not clear tag.
  (and-immediate-in-place target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-AND
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
        (< n 0)))		    ;Sign-extension must not clear tag.
  (and-immediate-in-place target source n))

(define (and-immediate-in-place target source n)
  (assert (< n 0))		    ;Sign-extension must not clear tag.
  ;; OR preserves the tag, as long as the operand is not sign-extended
  ;; into clearing all the tag bits.
  (let ((target (standard-move-to-target! source target)))
    (if (= n -1)
	(LAP)
	(with-signed-immediate-operand n
	  (lambda (operand)
	    (LAP (AND Q ,target ,operand)))))))

;;; ANDC tricks

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-ANDC
			 (OBJECT->FIXNUM (REGISTER (? source1)))
			 (REGISTER (? source2))
			 #f))
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert (not (eqv? source1 source2)))
  (let* ((source2 (standard-move-to-temporary! source2))
	 (target (standard-move-to-target! source1 target)))
    (LAP ,@(object->fixnum target)
	 (NOT Q ,source2)
	 (AND Q ,target ,source2))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-ANDC
			 (REGISTER (? source2))
			 (OBJECT->FIXNUM (REGISTER (? source1)))
			 #f))
  ;; Intermediate rule -- no advantage but paves way to composition.
  (assert (not (eqv? source1 source2)))
  (let* ((source1 (standard-move-to-temporary! source1))
	 (target (standard-move-to-target! source2 target)))
    (LAP ,@(object->fixnum source1)
	 (NOT Q ,source1)
	 (AND Q ,target ,source1))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-ANDC
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (detag-and-and-immediate target source (bitwise-not n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS FIXNUM-ANDC
                          (REGISTER (? source))
                          (OBJECT->FIXNUM (CONSTANT (? n)))
                          #f)))
  (and-immediate-and-tag target source (bitwise-not n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-ANDC
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #f))
  ;; Intermediate rule -- no advantage but paves way to composition.
  (let ((target (standard-move-to-target! source target)))
    (with-signed-immediate-operand n
      (lambda (operand)
	(LAP (NOT Q ,target)
	     (AND Q ,target ,operand)
	     ,@(if (<= 0 n)
		   ;; If n is nonnegative, the tag will not be intact,
		   ;; so we can't rightly use OBJECT->FIXNUM -- the
		   ;; peephole optimizer might get confused by it.
		   (LAP (SAL Q ,target (&U ,scheme-type-width)))
		   (object->fixnum target)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-ANDC
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(<= 0 n)))			;Sign-extension must not clear tag.
  (and-immediate-in-place target source (bitwise-not n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-ANDC
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  #f)))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
	(< n 0)))			;Sign-extension must not clear tag.
  (let ((target (standard-move-to-target! source target)))
    (with-signed-immediate-operand n
      (lambda (operand)
	(LAP (XOR Q ,target (R ,regnum:datum-mask))
	     (AND Q ,target ,operand))))))

;;; I don't think this rule is ever hit.  In any case, it does nothing
;;; useful over the other rules; formerly, it used a single OR to
;;; affix the type tag, since the two SHR's (one for the program, one
;;; to make room for the type tag) could be merged by adding the
;;; shift, but OR doesn't take 64-bit immediates, so that no longer
;;; works.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS FIXNUM-LSH
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? n)))
			  #f)))
  (QUALIFIER (and (exact-integer? n) (< (- scheme-datum-width) n 0)))
  (fixnum-1-arg target source
    (lambda (target)
      (LAP (SHR Q ,target (&U ,(- scheme-type-width n)))
	   (SHL Q ,target (&U ,scheme-type-width))
	   (OR Q ,target (&U ,(ucode-type FIXNUM)))
	   (ROR Q ,target (&U ,scheme-type-width))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-LSH
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (QUALIFIER (and (exact-integer? n) (< 0 n scheme-datum-width)))
  (fixnum-1-arg target source
    (lambda (target)
      (LAP (SHL Q ,target (&U ,(+ scheme-type-width n)))))))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (multiply-fixnum-constant target n overflow?)))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((= n 1)
	   (LAP))
	  ((= n -1)
	   (LAP (NEG Q ,target)))
	  ((integer-power-of-2? (if (negative? n) (- 0 n) n))
	   =>
	   (lambda (expt-of-2)
	     (let ((if-negative (generate-label 'QUO-NEGATIVE))
		   (merge (generate-label 'QUO-SHIFT)))
	       (LAP (CMP Q ,target (& 0))
		    ;; Forward branch for the negative case so the
		    ;; static prediction is not-taken.
		    (JL B (@PCR ,if-negative))
		    (JMP (@PCR ,merge))
		   (LABEL ,if-negative)
		    ,@(with-unsigned-immediate-operand
			  (* (- (abs n) 1) fixnum-1)
			(lambda (operand)
			  (LAP (ADD Q ,target ,operand))))
		   (LABEL ,merge)
		    (SAR Q ,target (&U ,expt-of-2))
		    ,@(word->fixnum target)
		    ,@(if (negative? n)
			  (LAP (NEG Q ,target))
			  (LAP))))))
	  (else
	   (error "fixnum-quotient: Bad value:" n)))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    ;; (remainder x y) is 0 or has the sign of x.
    ;; Thus we can always "divide" by (abs y) to make things simpler.
    overflow?				; ignored
    (let ((n (if (negative? n) (- 0 n) n)))
      (cond ((= n 1)
	     (load-fixnum-constant 0 target))
	    ((integer-power-of-2? n)
	     (let ((sign (temporary-register-reference))
		   (if-negative (generate-label 'REM-NEGATIVE))
		   (merge (generate-label 'REM-MERGE)))
	       ;; There is some hair here to deal with immediates that
	       ;; don't fit in 32 bits, and reusing a temporary
	       ;; register to store them.
	       (receive (temp prefix:n-1 operand:n-1)
		   (unsigned-immediate-operand (* (- n 1) fixnum-1)
					       temporary-register-reference)
		 (receive (temp prefix:-n operand:-n)
		     (signed-immediate-operand
		      (* n (- 0 fixnum-1))
		      (lambda ()
			(or temp (temporary-register-reference))))
		   temp			;ignore
		   ;; This may produce a branch to a branch, but a
		   ;; peephole optimizer should be able to fix this.
		   (LAP (MOV Q ,sign ,target)
			,@prefix:n-1
			(AND Q ,target ,operand:n-1)
			(JNZ B (@PCR ,if-negative))
			(JMP (@PCR ,merge))
		       (LABEL ,if-negative)
			(SAR Q ,sign (&U ,(-1+ scheme-object-width)))
			,@prefix:-n
			(AND Q ,sign ,operand:-n)
			(OR Q ,target ,sign)
		       (LABEL ,merge))))))
	    (else
	     (error "fixnum-remainder: Bad value:" n))))))

;;;; Fast division by multiplication

(define (fast-divide-prepare divisor width)
  (let ((zeros (integer-length (- divisor 1))))
    (values
     (+ 1
	(quotient (shift-left (- (shift-left 1 zeros) divisor) width) divisor))
     (if (> zeros 1) 1 zeros)
     (if (= zeros 0) 0 (- zeros 1)))))

;;; For reference, this is what the code generated below computes.

(define (fast-quotient n d width multiplier s1 s2)
  d					;ignore
  (let ((t (shift-right (* n multiplier) width)))
    (shift-right (+ t (shift-right (- n t) s1)) s2)))

(define (fast-remainder n d width multiplier s1 s2)
  (- n (* d (fast-quotient n d width multiplier s1 s2))))

(define (fast-divide-prepare/signed divisor width)
  (fast-divide-prepare (abs divisor) width))

(define (fast-quotient/signed n d width multiplier s1 s2)
  (define (do-unsigned n d)
    (fast-quotient n d width multiplier s1 s2))
  (if (negative? d)
      (if (negative? n)
	  (do-unsigned (- 0 n) (- 0 d))
	  (- 0 (do-unsigned n (- 0 d))))
      (if (negative? n)
	  (- 0 (do-unsigned (- 0 n) d))
	  (do-unsigned n d))))

(define (fast-remainder/signed n d width multiplier s1 s2)
  (- n (* d (fast-quotient/signed n d width multiplier s1 s2))))

(define (fast-division target* source* d finish)
  (flush-register! rax)
  (need-register! rax)
  (flush-register! rdx)
  (need-register! rdx)
  (receive (multiplier s1 s2) (fast-divide-prepare (abs d) scheme-object-width)
    (let* ((if-negative1 (generate-label 'QUO-NEGATIVE-1))
	   (if-negative2 (generate-label 'QUO-NEGATIVE-2))
	   (merge1 (generate-label 'QUO-MULTIPLY))
	   (merge2 (generate-label 'QUO-RESULT))
	   (source (source-register-reference source*))
	   (target (target-register-reference target*)))
      (LAP (MOV Q ,target ,source)
	   ;; Divide by 2^t so that factor doesn't mess us up.
	   (SAR Q ,target (&U ,scheme-type-width))
	   ;; No need to CMP; SAR sets the SF bit for us to detect
	   ;; whether the input is negative.
	   (JS B (@PCR ,if-negative1))
	   (JMP (@PCR ,merge1))
	  (LABEL ,if-negative1)
	   (NEG Q ,target)
	  (LABEL ,merge1)
	   ;; MUL takes argument in rax, so put it there.
	   (MOV Q (R ,rax) ,target)
	   ;; Load the multiplier into rdx, which is free until we MUL.
	   (MOV Q (R ,rdx) (&U ,multiplier))
	   ;; Compute the 128-bit product rax * multiplier, storing the
	   ;; high 64 bits in rdx and the low 64 bits in rax.  We are
	   ;; not interested in the low 64 bits, so rax is now free for
	   ;; reuse.
	   (MUL Q ((R ,rdx) : (R ,rax)) (R ,rdx))
	   ;; Compute ((((n - p) >> s1) + p) >> s2) where p is the high
	   ;; 64 bits of the product, in rdx.
	   (SUB Q ,target (R ,rdx))
	   (SHR Q ,target (&U ,s1))
	   (ADD Q ,target (R ,rdx))
	   (SHR Q ,target (&U ,s2))
	   ;; Reapply the sign.
	   (CMP Q ,source (& 0))
	   (JL B (@PCR ,if-negative2))
	   ,@(if (negative? d) (LAP (NEG Q ,target)) (LAP))
	   (JMP (@PCR ,merge2))
	  (LABEL ,if-negative2)
	   ,@(if (negative? d) (LAP) (LAP (NEG Q ,target)))
	  (LABEL ,merge2)
	   ;; Convert back to fixnum representation with low zero bits.
	   (SAL Q ,target (&U ,scheme-type-width))
	   ,@(finish target source (INST-EA (R ,rax)))))))

(define (fixnum-quotient/constant target source d)
  (fast-division target source d
    (lambda (quotient numerator temp)
      (declare (ignore quotient numerator temp))
      (LAP))))

(define (fixnum-remainder/constant target source d)
  (fast-division target source d
    (lambda (quotient numerator temp)
      ;; Compute n - d q.
      (LAP (MOV Q ,temp (& ,(- d)))
	   (IMUL Q ,quotient ,temp)
	   (ADD Q ,quotient ,numerator)))))

(define (fixnum-predicate/unary->binary predicate)
  (case predicate
    ((ZERO-FIXNUM?) 'EQUAL-FIXNUM?)
    ((NEGATIVE-FIXNUM?) 'LESS-THAN-FIXNUM?)
    ((POSITIVE-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    (else
     (error "fixnum-predicate/unary->binary: Unknown unary predicate"
	    predicate))))

(define (commute-fixnum-predicate predicate)
  (case predicate
    ((EQUAL-FIXNUM?) 'EQUAL-FIXNUM?)
    ((LESS-THAN-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    ((GREATER-THAN-FIXNUM?) 'LESS-THAN-FIXNUM?)
    ((UNSIGNED-LESS-THAN-FIXNUM?) 'UNSIGNED-GREATER-THAN-FIXNUM?)
    ((UNSIGNED-GREATER-THAN-FIXNUM?) 'UNSIGNED-LESS-THAN-FIXNUM?)
    (else
     (error "commute-fixnum-predicate: Unknown predicate"
	    predicate))))

(define (fixnum-branch! predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?)
     (set-equal-branches!))
    ((LESS-THAN-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JL (@PCR ,label))))
			    (lambda (label)
			      (LAP (JGE (@PCR ,label))))))
    ((GREATER-THAN-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JG (@PCR ,label))))
			    (lambda (label)
			      (LAP (JLE (@PCR ,label))))))
    ((UNSIGNED-LESS-THAN-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JB (@PCR ,label))))
			    (lambda (label)
			      (LAP (JAE (@PCR ,label))))))
    ((UNSIGNED-GREATER-THAN-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JA (@PCR ,label))))
			    (lambda (label)
			      (LAP (JBE (@PCR ,label))))))
    ((NEGATIVE-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JS (@PCR ,label))))
			    (lambda (label)
			      (LAP (JNS (@PCR ,label))))))
    ((POSITIVE-FIXNUM?)
     (error "fixnum-branch!: Cannot handle directly" predicate))
    (else
     (error "fixnum-branch!: Unknown predicate" predicate))))