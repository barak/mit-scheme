#| -*-Scheme-*-

$Id: rulfix.scm,v 1.7 2008/01/30 20:01:53 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Fixnum Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Conversions

(define-rule statement
  ;; convert a fixnum object to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target object->fixnum))

(define-rule statement
  ;; load a fixnum constant as a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-immediate (standard-target! target) (* constant fixnum-1) #T))

(define-rule statement
  ;; convert a memory address to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target address->fixnum))

(define-rule statement
  ;; convert an object's address to a "fixnum integer"
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (standard-unary-conversion source target object->fixnum))

(define-rule statement
  ;; convert a "fixnum integer" to a fixnum object
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (standard-unary-conversion source target fixnum->object))

(define-rule statement
  ;; convert a "fixnum integer" to a memory address
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source target fixnum->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #F))
  (standard-unary-conversion source target object->index-fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 #F))
  (standard-unary-conversion source target object->index-fixnum))

;; This is a patch for the time being.  Probably only one of these pairs
;; of rules is needed.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (REGISTER (? source))
			 #F))
  (standard-unary-conversion source target fixnum->index-fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 #F))
  (standard-unary-conversion source target fixnum->index-fixnum))

;; "Fixnum" in this context means an integer left shifted so that
;; the sign bit is the leftmost bit of the word, i.e., the datum
;; has been left shifted by scheme-type-width bits.

(define-integrable (fixnum->index-fixnum src tgt)
  ; Shift left 2 bits
  (LAP (SLL ,tgt ,src 2)))

(define-integrable (object->fixnum src tgt)
  ; Shift left by scheme-type-width
  (LAP (SLL ,tgt ,src ,scheme-type-width)))

(define-integrable (object->index-fixnum src tgt)
  ; Shift left by scheme-type-width+2
  (LAP (SLL ,tgt ,src ,(+ scheme-type-width 2))))

(define-integrable (address->fixnum src tgt)
  ; Strip off type bits, just like object->fixnum
  (LAP (SLL ,tgt ,src ,scheme-type-width)))

(define-integrable (fixnum->object src tgt)
  ; Move right by type code width and put on fixnum type code
  (LAP (SRL ,tgt ,src ,scheme-type-width)
       ,@(deposit-type-datum (ucode-type fixnum) tgt tgt)))

(define (fixnum->address src tgt)
  ; Move right by type code width and put in address bits
  (LAP (SRL ,tgt ,src ,scheme-type-width)
       (OR ,tgt ,tgt ,regnum:quad-bits)))

(define-integrable fixnum-1
  (expt 2 scheme-type-width))

(define-integrable -fixnum-1
  (- fixnum-1))

(define (no-overflow-branches!)
  (set-current-branches!
   (lambda (if-overflow)
     if-overflow
     (LAP))
   (lambda (if-no-overflow)
     (LAP (BA (@PCR ,if-no-overflow))
	  (NOP)))))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

;;;; Arithmetic Operations

(define-rule statement
  ;; execute a unary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      ((fixnum-1-arg/operator operation) target source overflow?))))

(define (fixnum-1-arg/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/1-arg))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (fixnum-add-constant tgt src 1 overflow?)))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (fixnum-add-constant tgt src -1 overflow?)))

(define (fixnum-add-constant tgt src constant overflow?)
  (let ((constant (* fixnum-1 constant)))
    (cond ((not overflow?)
	   (add-immediate constant src tgt))
	  ((= constant 0)
	   (no-overflow-branches!)
	   (LAP (ADDIU ,tgt ,src 0)))
	  (else
	   (let ((bcc (if (> constant 0) 'BLE 'BGE)))
	     (let ((prefix
		    (if (fits-in-16-bits-signed? constant)
			(lambda (label)
			  (LAP (SUBCCI ,regnum:assembler-temp 0 ,src)
			       (,bcc ,regnum:assembler-temp (@PCR ,label))
			       (ADDIU ,tgt ,src ,constant)))
			(with-values (lambda () (immediate->register constant))
			  (lambda (prefix alias)
			    (lambda (label)
			      (LAP ,@prefix
				   (,bcc ,src (@PCR ,label))
				   (ADDU ,tgt ,src ,alias))))))))
	       (if (> constant 0)
		   (set-current-branches!
		    (lambda (if-overflow)
		      (let ((if-no-overflow (generate-label)))
			(LAP ,@(prefix if-no-overflow)
			     (SUBCCI ,regnum:assembler-temp 0 ,tgt)			   
			     (BLT ,tgt (@PCR ,if-overflow))
			     (NOP)
			     (LABEL ,if-no-overflow))))
		    (lambda (if-no-overflow)
		      (LAP ,@(prefix if-no-overflow)
			   (SUBCCI ,regnum:assembler-temp 0 ,tgt)			   
			   (BGE ,tgt (@PCR ,if-no-overflow))
			   (NOP))))
		   (set-current-branches!
		    (lambda (if-overflow)
		      (let ((if-no-overflow (generate-label)))
			(LAP ,@(prefix if-no-overflow)
			     (SUBCCI ,regnum:assembler-temp 0 ,tgt)			   
			     (BGE ,tgt (@PCR ,if-overflow))
			     (NOP)
			     (LABEL ,if-no-overflow))))
		    (lambda (if-no-overflow)
		      (LAP ,@(prefix if-no-overflow)
			   (BLTZ ,tgt (@PCR ,if-no-overflow))
			   (NOP)))))))
	   (LAP)))))

(define-rule statement
  ;; execute a binary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (standard-binary-conversion source1 source2 target
    (lambda (source1 source2 target)
      ((fixnum-2-args/operator operation) target source1 source2 overflow?))))

(define (fixnum-2-args/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(do-overflow-addition tgt src1 src2)
	(LAP (ADDU ,tgt ,src1 ,src2)))))

;;; Use of REGNUM:ASSEMBLER-TEMP is OK here, but only because its
;;; value is not used after the branch instruction that tests it.
;;; The long form of the @PCR branch will test it correctly, but
;;; clobbers it after testing.

(define (do-overflow-addition tgt src1 src2)
  (cond ((not (= src1 src2))
	 (set-current-branches!
	  (lambda (if-overflow)
	    (let ((if-no-overflow (generate-label)))
	      (LAP (XOR  ,regnum:assembler-temp ,src1 ,src2)
		   (BLTZ ,regnum:assembler-temp (@PCR ,if-no-overflow))
		   (ADDU ,tgt ,src1 ,src2)
		   (XOR  ,regnum:assembler-temp
			 ,tgt
			 ,(if (= tgt src1) src2 src1))
		   (BLTZ ,regnum:assembler-temp (@PCR ,if-overflow))
		   (NOP)
		   (LABEL ,if-no-overflow))))
	  (lambda (if-no-overflow)
	    (LAP (XOR  ,regnum:assembler-temp ,src1 ,src2)
		 (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		 (BLT ,regnum:assembler-temp (@PCR ,if-no-overflow))
		 (ADDU ,tgt ,src1 ,src2)
		 (XOR  ,regnum:assembler-temp
		       ,tgt
		       ,(if (= tgt src1) src2 src1))
		 (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		 (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow))
		 (NOP)))))
	((not (= tgt src1))
	 (set-current-branches!
	  (lambda (if-overflow)
	    (LAP (ADDU ,tgt ,src1 ,src1)
		 (XOR  ,regnum:assembler-temp ,tgt ,src1)
		 (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		 (BLT ,regnum:assembler-temp (@PCR ,if-overflow))
		 (NOP)))
	  (lambda (if-no-overflow)
	    (LAP (ADDU ,tgt ,src1 ,src1)
		 (XOR  ,regnum:assembler-temp ,tgt ,src1)
		 (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		 (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow))
		 (NOP)))))
	(else
	 (let ((temp (standard-temporary!)))
	   (set-current-branches!
	    (lambda (if-overflow)
	      (LAP (ADDU ,temp ,src1 ,src1)
		   (XOR  ,regnum:assembler-temp ,temp ,src1)
		   (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		   (BLT ,regnum:assembler-temp (@PCR ,if-overflow))
		   (ADD  ,tgt 0 ,temp)))
	    (lambda (if-no-overflow)
	      (LAP (ADDU ,temp ,src1 ,src1)
		   (XOR  ,regnum:assembler-temp ,temp ,src1)
		   (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		   (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow))
		   (ADD  ,tgt 0 ,temp)))))))
  (LAP))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(if (= src1 src2)		;probably won't ever happen.
	    (begin
	      (no-overflow-branches!)
	      (LAP (SUBU ,tgt ,src1 ,src1)))
	    (do-overflow-subtraction tgt src1 src2))
	(LAP (SUB ,tgt ,src1 ,src2)))))

(define (do-overflow-subtraction tgt src1 src2)
  (set-current-branches!
   (lambda (if-overflow)
     (let ((if-no-overflow (generate-label)))
       (LAP (XOR  ,regnum:assembler-temp ,src1 ,src2)
	    (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
	    (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow))
	    (SUBU ,tgt ,src1 ,src2)
	    ,@(if (not (= tgt src1))
		  (LAP (XOR  ,regnum:assembler-temp ,tgt ,src1)
		       (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		       (BLT ,regnum:assembler-temp (@PCR ,if-overflow)))
		  (LAP (XOR  ,regnum:assembler-temp ,tgt ,src2)
		       (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		       (BGE ,regnum:assembler-temp (@PCR ,if-overflow))))
	    (NOP)
	    (LABEL ,if-no-overflow))))
   (lambda (if-no-overflow)
     (LAP (XOR  ,regnum:assembler-temp ,src1 ,src2)
	  (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
	  (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow))
	  (SUBU ,tgt ,src1 ,src2)
	  ,@(if (not (= tgt src1))
		(LAP (XOR  ,regnum:assembler-temp ,tgt ,src1)
		     (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0)		   
		     (BGE ,regnum:assembler-temp (@PCR ,if-no-overflow)))
		(LAP (XOR  ,regnum:assembler-temp ,tgt ,src2)
		     (SUBCCI ,regnum:assembler-temp ,regnum:assembler-temp 0g)		   
		     (BLT ,regnum:assembler-temp (@PCR ,if-no-overflow))))
	  (NOP))))
  (LAP))

(define (do-multiply tgt src1 src2 overflow?)
  (if overflow?
      (let ((temp (standard-temporary!)))
	(set-current-branches!
	 (lambda (if-overflow)
	   (LAP (MFHI ,temp)
		(SRA  ,regnum:assembler-temp ,tgt 31)
		(BNE  ,temp ,regnum:assembler-temp
		      (@PCR ,if-overflow))
		(NOP)))
	 (lambda (if-no-overflow)
	   (LAP (MFHI ,temp)
		(SRA  ,regnum:assembler-temp ,tgt 31)
		(BEQ  ,temp ,regnum:assembler-temp
		      (@PCR ,if-no-overflow))
		(NOP))))))
  (LAP (SRA  ,regnum:assembler-temp ,src1 ,scheme-type-width)
       (MULT ,regnum:assembler-temp ,src2)
       (MFLO ,tgt)))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args do-multiply)

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      ((fixnum-2-args/operator/register*constant operation)
       target source constant overflow?))))

(define-rule statement
  ;; execute binary fixnum operation with constant first arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      (if (fixnum-2-args/commutative? operation)
	  ((fixnum-2-args/operator/register*constant operation)
	   target source constant overflow?)
	  ((fixnum-2-args/operator/constant*register operation)
	   target constant source overflow?)))))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM MULTIPLY-FIXNUM)))

(define (fixnum-2-args/operator/register*constant operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args/register*constant))

(define fixnum-methods/2-args/register*constant
  (list 'FIXNUM-METHODS/2-ARGS/REGISTER*CONSTANT))

(define (fixnum-2-args/operator/constant*register operation)
  (lookup-arithmetic-method operation
			    fixnum-methods/2-args/constant*register))

(define fixnum-methods/2-args/constant*register
  (list 'FIXNUM-METHODS/2-ARGS/CONSTANT*REGISTER))

(define-arithmetic-method 'PLUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (fixnum-add-constant tgt src constant overflow?)))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (fixnum-add-constant tgt src (- constant) overflow?)))

(define-arithmetic-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (cond ((zero? constant)
	   (if overflow? (no-overflow-branches!))
	   (LAP (ADDI ,tgt 0 0)))
	  ((= constant 1) 
	   (if overflow? (no-overflow-branches!))
	   (LAP (ADD ,tgt 0 ,src)))
	  ((let loop ((n constant))
	     (and (> n 0)
		  (if (= n 1)
		      0
		      (and (even? n)
			   (let ((m (loop (quotient n 2))))
			     (and m
				  (+ m 1)))))))
	   =>
	   (lambda (power-of-two)
	     (if overflow?
		 (do-left-shift-overflow tgt src power-of-two)
		 (LAP (SLL ,tgt ,src ,power-of-two)))))
	  (else
	   (with-values (lambda () (immediate->register (* constant fixnum-1)))
	     (lambda (prefix alias)
	       (LAP ,@prefix
		    ,@(do-multiply tgt src alias overflow?))))))))

(define (do-left-shift-overflow tgt src power-of-two)
  (if (= tgt src)
      (let ((temp (standard-temporary!)))
	(set-current-branches!
	 (lambda (if-overflow)
	   (LAP (SLL  ,temp ,src ,power-of-two)
		(SRA  ,regnum:assembler-temp ,temp ,power-of-two)
		(BNE  ,regnum:assembler-temp ,src (@PCR ,if-overflow))
		(ADD  ,tgt 0 ,temp)))
	 (lambda (if-no-overflow)
	   (LAP (SLL  ,temp ,src ,power-of-two)
		(SRA  ,regnum:assembler-temp ,temp ,power-of-two)
		(BEQ  ,regnum:assembler-temp ,src (@PCR ,if-no-overflow))
		(ADD  ,tgt 0 ,temp)))))
      (set-current-branches!
       (lambda (if-overflow)
	 (LAP (SLL  ,tgt ,src ,power-of-two)
	      (SRA  ,regnum:assembler-temp ,tgt ,power-of-two)
	      (BNE  ,regnum:assembler-temp ,src (@PCR ,if-overflow))
	      (NOP)))
       (lambda (if-no-overflow)
	 (LAP (SLL  ,tgt ,src ,power-of-two)
	      (SRA  ,regnum:assembler-temp ,tgt ,power-of-two)
	      (BEQ  ,regnum:assembler-temp ,src (@PCR ,if-no-overflow))
	      (NOP)))))
  (LAP))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/constant*register
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (with-values (lambda () (immediate->register (* constant fixnum-1)))
      (lambda (prefix alias)
	(LAP ,@prefix
	     ,@(if overflow?
		   (do-overflow-subtraction tgt alias src)
		   (LAP (SUB ,tgt ,alias ,src))))))))

;;;; Predicates

(define-rule predicate
  (OVERFLOW-TEST)
  ;; The RTL code generate guarantees that this instruction is always
  ;; immediately preceded by a fixnum operation with the OVERFLOW?
  ;; flag turned on.  Furthermore, it also guarantees that there are
  ;; no other fixnum operations with the OVERFLOW? flag set.  So all
  ;; the processing of overflow tests has been moved into the fixnum
  ;; operations.
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare-immediate (fixnum-pred-1->cc predicate)
		     0
		     (standard-source! source)))

(define (fixnum-pred-1->cc predicate)
  (case predicate
    ((ZERO-FIXNUM?) '=)
    ((NEGATIVE-FIXNUM?) '>)
    ((POSITIVE-FIXNUM?) '<)
    (else (error "unknown fixnum predicate" predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (compare (fixnum-pred-2->cc predicate)
	   (standard-source! source1)
	   (standard-source! source2)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (compare-fixnum/constant*register (invert-condition-noncommutative
				     (fixnum-pred-2->cc predicate))
				    constant
				    (standard-source! source)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? source)))
  (compare-fixnum/constant*register (fixnum-pred-2->cc predicate)
				    constant
				    (standard-source! source)))

(define-integrable (compare-fixnum/constant*register cc n r)
  (guarantee-signed-fixnum n)
  (compare-immediate cc (* n fixnum-1) r))

(define (fixnum-pred-2->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM?) '=)
    ((LESS-THAN-FIXNUM?) '<)
    ((GREATER-THAN-FIXNUM?) '>)
    (else (error "unknown fixnum predicate" predicate))))