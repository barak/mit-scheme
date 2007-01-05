#| -*-Scheme-*-

$Id: rulfix.scm,v 1.8 2007/01/05 15:33:03 cph Exp $

Copyright (c) 1992-1999, 2001 Massachusetts Institute of Technology

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
;; Package: (compiler lap-syntaxer)

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
			 (OBJECT->FIXNUM (CONSTANT (? value)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #F))
  (QUALIFIER (power-of-2 value))
  (standard-unary-conversion source target (object-scaler value)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? value)))
			 #F))
  (QUALIFIER (power-of-2 value))
  (standard-unary-conversion source target (object-scaler value)))

;; "Fixnum" in this context means an integer left shifted so that
;; the sign bit is the leftmost bit of the word, i.e., the datum
;; has been left shifted by scheme-type-width bits.

(define (power-of-2 value)
  (and (positive? value)
       (let loop ((n value)
		  (exp 0))
	 (if (= n 1)
	     exp
	     (let ((division (integer-divide n 2)))
	       (and (zero? (integer-divide-remainder division))
		    (loop (integer-divide-quotient division)
			  (+ exp 1))))))))

(define-integrable (object-scaler value)
  (lambda (source target)
    (scaled-object->fixnum (power-of-2 value) source target)))

(define-integrable (datum->fixnum src tgt)
  ; Shift left by scheme-type-width
  (LAP (SLL ,src (& ,scheme-type-width) ,tgt)))

(define-integrable (fixnum->datum src tgt)
  (LAP (SRL ,src (& ,scheme-type-width) ,tgt)))

(define-integrable (object->fixnum src tgt)
  (datum->fixnum src tgt))

(define-integrable (scaled-object->fixnum shift src tgt)
  (LAP (SLL ,src (& ,(+ shift scheme-type-width)) ,tgt)))

(define-integrable (address->fixnum src tgt)
  ; Strip off type bits, just like object->fixnum
  (datum->fixnum src tgt))

(define-integrable (fixnum->object src tgt)
  ; Move right by type code width and put on fixnum type code
  (LAP ,@(fixnum->datum src tgt)
       ,@(deposit-type-datum (ucode-type fixnum) tgt tgt)))

(define (fixnum->address src tgt)
  ; Move right by type code width; no address bits
  (fixnum->datum src tgt))

(define-integrable fixnum-1
  (expt 2 scheme-type-width))

(define-integrable -fixnum-1
  (- fixnum-1))

(define (no-overflow-branches!)
  (set-current-branches!
   (lambda (if-overflow)
     if-overflow			; ignored
     (LAP))
   (lambda (if-no-overflow)
     (LAP (BR ,regnum:came-from (@PCR ,if-no-overflow))))))

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

(define-arithmetic-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (if overflow?
	(error "FIXNUM-NOT: overflow test requested"))
    (LAP (EQV ,src (& ,(-1+ fixnum-1)) ,tgt))))

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
	  ((zero? constant)
	   (no-overflow-branches!)
	   (LAP (COPY ,src ,tgt)))
	  (else
	   (with-values
	       (lambda ()
		 (cond
		  ((fits-in-16-bits-signed? constant)
		   (values (LAP)
			   (lambda (target)
			     (LAP (LDA ,target (OFFSET ,constant ,src))))))
		  ((top-16-of-32-bits-only? constant)
		   (values (LAP)
			   (lambda (target)
			     (LAP (LDAH ,target (OFFSET ,constant ,src))))))
		  (else
		   (with-values (lambda () (immediate->register constant))
		     (lambda (prefix alias)
		       (values prefix
			       (lambda (target)
				 (LAP (ADDQ ,src ,alias ,target)))))))))
	     (lambda (prefix add-code)
	       (let ((temp (new-temporary! src)))
		 (cond
		  ((positive? constant)
		   (begin
		     (set-current-branches!
		      (lambda (overflow-label)
			(LAP (BLT ,temp (@PCR ,overflow-label))))
		      (lambda (no-overflow-label)
			(LAP (BGE ,temp (@PCR ,no-overflow-label)))))
		     (LAP ,@prefix
			  ,@(add-code temp)   ; Add, result to temp
			  (CMOVLT ,src ,regnum:zero ,temp)
					      ; sgn(src) != sgn(const) ->
					      ; no overflow
			  ,@(add-code tgt)    ; Real result
			  ; (BLT ,temp (@PCR ,overflow-label))
			  )))
		  ((not (= src tgt))
		   (set-current-branches!
		    (lambda (overflow-label)
		      (LAP (BLT ,temp (@PCR ,overflow-label))))
		    (lambda (no-overflow-label)
		      (LAP (BGE ,temp (@PCR ,no-overflow-label)))))
		   (LAP ,@prefix
			,@(add-code tgt)      ; Add, result to target
			(XOR ,src ,tgt ,temp) ; Compare result and source sign
			(CMOVGE ,src ,regnum:zero ,temp)
					      ; sgn(src) != sgn(const) ->
					      ; no overflow
			; (BLT ,temp (@PCR ,overflow-label))
			))
		  (else
		   (set-current-branches!
		    (lambda (overflow-label)
		      (LAP (BGE ,temp (@PCR ,overflow-label))))
		    (lambda (no-overflow-label)
		      (LAP (BLT ,temp (@PCR ,no-overflow-label)))))
		   (with-values
		       (lambda () (immediate->register -1))
		     (lambda (prefix2 reg:minus-1)
		       (LAP ,@prefix
			    ,@prefix2
			    ,@(add-code temp) ; Add, result to temp
			    (CMOVGE ,src ,reg:minus-1 ,temp)
					      ; sgn(src) != sgn(const) ->
					      ; no overflow
			    ,@(add-code tgt)  ; Add, result to target
			    ; (BGE ,temp (@PCR ,overflow-label))
			    ))))))))))))

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

(define-arithmetic-method 'FIXNUM-AND fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(error "FIXNUM-AND: overflow test requested"))
    (LAP (AND ,src1 ,src2 ,tgt))))

(define-arithmetic-method 'FIXNUM-OR fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(error "FIXNUM-OR: overflow test requested"))
    (LAP (BIS ,src1 ,src2 ,tgt))))

(define-arithmetic-method 'FIXNUM-XOR fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(error "FIXNUM-XOR: overflow test requested"))
    (LAP (XOR ,src1 ,src2 ,tgt))))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(do-overflow-addition tgt src1 src2)
	(LAP (ADDQ ,src1 ,src2 ,tgt)))))

(define (do-overflow-addition tgt src1 src2)
  (let ((temp1 (new-temporary! src1 src2)))
    (set-current-branches!
     (lambda (overflow-label)
       (LAP (BLT ,temp1 (@PCR ,overflow-label))))
     (lambda (no-overflow-label)
       (LAP (BGE ,temp1 (@PCR ,no-overflow-label)))))
    (cond ((not (= src1 src2))
	   (let ((temp2 (new-temporary! src1 src2))
		 (src (if (= src1 tgt) src2 src1))) ; Non-clobbered source
	     (LAP (XOR ,src1 ,src2 ,temp2)    ; Sign compare sources
		  (ADDQ ,src1 ,src2 ,tgt)     ; Add them ...
		  (XOR ,src ,tgt ,temp1)      ; Result sign OK?
		  (CMOVLT ,temp2 ,regnum:zero ,temp1)
					      ; Looks like sgn(result)=sgn(src)
					      ; if sgn(src1) != sgn(src2)
		  ; (BLT ,temp1 (@PCR ,overflow-label))
					      ; Sign differs -> overflow
		  )))
	((not (= src1 tgt))
	 (LAP (ADDQ ,src1 ,src2 ,tgt)	      ; Add
	      (XOR ,src1 ,tgt ,temp1)))	      ; Sign compare result
	(else				      ; Don't test source signs
	 (LAP (ADDQ ,src1 ,src2 ,temp1)	      ; Interim sum
	      (XOR ,src1 ,temp1 ,temp1)	      ; Compare result & source signs
	      (ADDQ ,src1 ,src2 ,tgt)	      ; Final addition
	      ; (BLT ,temp1 (@PCR ,overflow-label))
					      ; Sign differs -> overflow
	      )))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(error "FIXNUM-ANDC: overflow test requested"))
    (LAP (BIC ,src1 ,src2 ,tgt))))

(define (with-different-source-and-target src tgt handler)
  (if (not (= tgt src))
      (handler src tgt)
      (let ((temp (standard-temporary!)))
	(LAP (COPY ,src ,temp)
	     ,@(handler temp tgt)))))

(define-arithmetic-method 'FIXNUM-LSH fixnum-methods/2-args
  (lambda (tgt value shift-amount overflow?)
    (if overflow?
	(error "FIXNUM-LSH: overflow test requested"))
    (let* ((temp (standard-temporary!))
	   (temp-right (standard-temporary!)))
      (with-different-source-and-target
       value tgt
       (lambda (value tgt)
	 (LAP (SRA ,shift-amount (& ,scheme-type-width) ,temp)
	      (SLL ,value ,temp ,tgt)
	      (SUBQ ,regnum:zero ,temp ,temp-right)
	      (SRL ,value ,temp-right ,temp-right)
	      (BIC ,temp-right (& ,(-1+ fixnum-1)) ,temp-right)
	      (CMOVLT ,shift-amount ,temp-right ,tgt)))))))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(if (= src1 src2)		;probably won't ever happen.
	    (begin
	      (no-overflow-branches!)
	      (LAP (SUBQ ,src1 ,src1 ,tgt)))
	    (do-overflow-subtraction tgt src1 src2))
	(LAP (SUBQ ,src1 ,src2 ,tgt)))))

(define (do-overflow-subtraction tgt src1 src2)
  ; Requires src1 != src2
  (let ((temp1 (new-temporary! src1 src2))
	(temp2 (new-temporary! src1 src2)))
    (set-current-branches!
     (lambda (overflow-label)
       (LAP (BLT ,temp1 (@PCR ,overflow-label))))
     (lambda (no-overflow-label)
       (LAP (BGE ,temp1 (@PCR ,no-overflow-label)))))
    (LAP (XOR ,src1 ,src2 ,temp2)	      ; Compare source signs
	 (SUBQ ,src1 ,src2 ,tgt)	      ; Subtract
	 ,@(if (= src1 tgt)		      ; Compare result and source sign
	       (LAP (EQV ,src2 ,tgt ,temp1))
	       (LAP (XOR ,src1 ,tgt ,temp1)))
	 (CMOVGE ,temp2 ,regnum:zero ,temp1)  ; Same source signs ->
					      ;   no overflow
	 ; (BLT ,temp1 (@PCR ,overflow-label))
	 )))

(define (do-multiply tgt src1 src2 overflow?)
  (let ((temp (new-temporary! src1 src2)))
    (LAP (SRA ,src1 (& ,scheme-type-width) ,temp) ; unscale source 1
	 ,@(if overflow?
	       (let ((abs1 (new-temporary! src1 src2))
		     (abs2 (new-temporary! src1 src2))
		     (oflow? (new-temporary! src1 src2)))
		 (set-current-branches!
		  (lambda (overflow-label)
		    (LAP (BNE ,oflow? (@PCR ,overflow-label))))
		  (lambda (no-overflow-label)
		    (LAP (BEQ ,oflow? (@PCR ,no-overflow-label)))))
		 (LAP
		  (SUBQ ,regnum:zero ,temp ,abs1) ; ABS(unscaled(source1))
		  (CMOVGE ,temp ,temp ,abs1)	   ;  ""
		  (SUBQ ,regnum:zero ,src2 ,abs2) ; ABS(source2)
		  (CMOVGE ,src2 ,src2 ,abs2)	   ;  ""
						   ; high of abs(source2)*
		  (UMULH ,abs1 ,abs2 ,oflow?)	   ;  abs(unscaled(source1))
		  (MULQ ,abs1 ,abs2 ,abs1)	   ; low of same
		  (CMOVLT ,abs1 ,src2 ,oflow?)	   ; If low end oflowed, make
						   ;  sure that high end <> 0
		  ;; (BNE ,oflow? (@PCR overflow-label))
						   ; If high end <> 0 oflow
		  ))
	       (LAP))
	 (MULQ ,temp ,src2 ,tgt))))	           ; Compute result

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args do-multiply)

;;;; Division operations, unknown arguments

#| ; This doesn't work because we get physical register numbers, not
   ; rtl register numbers.

(define (special-binary-operation operation hook end-code)
  (lambda (target source1 source2 ovflw?)
    (define (->machine-register source machine-reg)
      (let ((code (load-machine-register! source machine-reg)))
	;; Prevent it from being allocated again.
	(need-register! machine-reg)
	code))
    (require-register! r23)
    (let* ((load-1 (->machine-register source1 r24))
	   (load-2 (->machine-register source2 r25))
	   (target (standard-target! target)))
      (LAP ,@load-1
	   ,@load-2
	   (LDQ ,r23 ,hook)
	   (JSR ,r23 ,r23 (@PCO 0))
	   ,@(end-code ovflw? r24 target)))))
|#

(define (special-binary-operation operation hook end-code)
  (lambda (target source1 source2 ovflw?)
    (if (not (= target r23)) (require-register! r23))
    (if (not (= target r24)) (require-register! r24))
    (if (not (= target r25)) (require-register! r25))
    (if (not (= target r27)) (require-register! r27))
    (LAP
     ,@(cond ((and (= source1 r25) (= source2 r24))
	      (LAP (COPY ,r24 ,r23)
		   (COPY ,r25 ,r24)
		   (COPY ,r23 ,r25)))
	     ((= source1 r25)
	      (LAP (COPY ,r25 ,r24)
		   ,@(copy source2 r25)))
	     (else
	      (LAP ,@(copy source2 r25)
		   ,@(copy source1 r24))))
     (LDQ ,r27 ,hook)
     (JSR ,r23 ,r27 (@PCO 0))
     ,@(end-code ovflw? r27 target))))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (special-binary-operation
   'FIXNUM-QUOTIENT
   reg:divq
   (lambda (overflow? source target)
     (if (not overflow?)
	 (LAP (SLL ,source (& ,scheme-type-width) ,target))
	 (with-different-source-and-target
	  source target
	  (lambda (source target)
	    (let ((temp (standard-temporary!)))
	      (set-current-branches!
	       (lambda (if-overflow)
		 (LAP (BEQ ,temp (@PCR ,if-overflow))))
	       (lambda (if-no-overflow)		 
		 (LAP (BNE ,temp (@PCR ,if-no-overflow)))))
	      (LAP (SLL ,source (& ,scheme-type-width) ,target)
		   (SRA ,target (& ,scheme-type-width) ,temp)
		   (CMPEQ ,temp ,target ,temp)))))))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (special-binary-operation 'FIXNUM-REMAINDER reg:remq
			    (lambda (overflow? src tgt)
			      (if overflow? (no-overflow-branches!))
			      (copy src tgt))))

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER (case operation
	       ((FIXNUM-AND FIXNUM-OR FIXNUM-ANDC FIXNUM-XOR)
		#F)
	       ((FIXNUM-REMAINDER)
		(power-of-2 (abs constant)))
	       (else #T)))
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
  (QUALIFIER (not (memq operation
			'(FIXNUM-AND FIXNUM-OR FIXNUM-ANDC
			  FIXNUM-XOR FIXNUM-LSH FIXNUM-REMAINDER
			  FIXNUM-QUOTIENT))))
  (standard-unary-conversion source target
    (lambda (source target)
      (if (fixnum-2-args/commutative? operation)
	  ((fixnum-2-args/operator/register*constant operation)
	   target source constant overflow?)
	  ((fixnum-2-args/operator/constant*register operation)
	   target constant source overflow?)))))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))

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

(define-arithmetic-method 'FIXNUM-LSH
  fixnum-methods/2-args/register*constant
  (lambda (tgt source constant-shift-amount overflow?)
    (if overflow?
	(error "FIXNUM-LSH: overflow test requested"))
    (guarantee-signed-fixnum constant-shift-amount)
    (let ((nbits (abs constant-shift-amount)))
      (cond ((zero? constant-shift-amount)
	     (copy source tgt))
	    ((>= nbits scheme-datum-width)
	     (LAP (COPY ,regnum:zero ,tgt)))
	    ((negative? constant-shift-amount)
	     (LAP (SRL ,source (& ,(fix:and nbits 63)) ,tgt)
		  (BIC ,tgt (& ,(-1+ fixnum-1)) ,tgt)))
	    (else
	     (LAP (SLL ,source (& ,(fix:and nbits 63)) ,tgt)))))))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (fixnum-add-constant tgt src (- constant) overflow?)))

;;;; Division operators with constant denominator

(define-arithmetic-method 'FIXNUM-QUOTIENT
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant ovflw?)
    (guarantee-signed-fixnum constant)
    (case constant
      ((0) (error "FIXNUM-QUOTIENT: Divide by zero"))
      ((1) (if ovflw? (no-overflow-branches!)) (copy src tgt))
      ((-1) (if (not ovflw?)
		(LAP (SUBQ ,regnum:zero ,src ,tgt))
		(let ((temp (standard-temporary!)))
		  (set-current-branches!
		   (lambda (if-overflow)
		     (LAP (BNE ,temp (@PCR ,if-overflow))))
		   (lambda (if-no-overflow)
		     (LAP (BEQ ,temp (@PCR ,if-no-overflow)))))
		  (with-different-source-and-target
		   src tgt
		   (lambda (src tgt)
		     (LAP (SUBQ ,regnum:zero ,src ,tgt)
			  (CMPEQ ,src ,tgt ,temp)
			  (CMOVEQ ,src ,regnum:zero ,temp)))))))
      (else
       (if ovflw? (no-overflow-branches!)) 
       (let* ((factor (abs constant))
	      (xpt (power-of-2 factor)))
	 (cond ((> factor signed-fixnum/upper-limit)
		(copy regnum:zero tgt))
	       (xpt			; A power of 2
		(let ((temp (standard-temporary!)))
		  (LAP ,@(add-immediate (* (-1+ factor) fixnum-1) src temp)
		       (CMOVGE ,src ,src ,temp)
		       (SRA ,temp (& ,xpt) ,tgt)
		       (BIC ,tgt (& ,(-1+ fixnum-1)) ,tgt)
		       ,@(if (negative? constant)
			     (LAP (SUBQ ,regnum:zero ,tgt ,tgt))
			     (LAP)))))
	       (else
		(with-different-source-and-target
		 src tgt
		 (lambda (src tgt)
		   (define max-word (expt 2 scheme-object-width))
		   (define (find-shift denom recvr)
		     (let loop ((shift 1)
				(factor (ceiling (/ max-word denom))))
		       (let ((next
			      (ceiling
			       (/ (expt 2 (+ scheme-object-width shift))
				  denom))))
			 (if (>= next max-word)
			     (normalize (-1+ shift) factor recvr)
			     (loop (1+ shift) next)))))
		   (define (normalize shift factor recvr)
		     (do ((shift shift (-1+ shift))
			  (factor factor (quotient factor 2)))
			 ((or (zero? shift) (odd? factor))
			  (recvr shift factor))))
		   (let ((abs-val (standard-temporary!)))
		     (find-shift factor
		       (lambda (shift multiplier)
			 (with-values
			     (lambda () (immediate->register multiplier))
			   (lambda (prefix temp)
			     (LAP
			      ,@prefix
			      (SUBQ ,regnum:zero ,src ,abs-val)
			      (CMOVGE ,src ,src ,abs-val)
			      (SRL ,abs-val (& ,scheme-type-width) ,abs-val)
			      (UMULH ,abs-val ,temp ,abs-val)
			      ,@(if (= shift 0)
				    (LAP)
				    (LAP (SRL ,abs-val (& ,shift) ,abs-val)))
			      (SLL ,abs-val (& ,scheme-type-width) ,abs-val)
			      (SUBQ ,regnum:zero ,abs-val ,tgt)
			      ,@(if (positive? constant)
				    (LAP (CMOVGE ,src ,abs-val ,tgt))
				    (LAP
				     (CMOVLT ,src
					     ,abs-val
					     ,tgt))))))))))))))))))

(define-arithmetic-method 'FIXNUM-REMAINDER
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant ovflw?)
    (guarantee-signed-fixnum constant)
    (if ovflw? (no-overflow-branches!))
    (case constant
      ((1 -1) (copy regnum:zero tgt))
      (else
       (let* ((keep-bits (+ scheme-type-width (power-of-2 (abs constant))))
	      (flush-bits (- scheme-object-width keep-bits))
	      (temp (standard-temporary!))
	      (sign (standard-temporary!)))
	 (LAP (SLL ,src (& ,flush-bits) ,temp)
	      (SRA ,src (& ,(- scheme-object-width 1)) ,sign)
	      (SRL ,temp (& ,flush-bits) ,temp)
	      (SLL ,sign (& ,keep-bits) ,sign)
	      (BIS ,sign ,temp ,tgt)
	      (CMOVEQ ,temp ,regnum:zero ,tgt)))))))

;;;; Other operators with constant second argument

(define-arithmetic-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (cond ((zero? constant)
	   (if overflow? (no-overflow-branches!))
	   (LAP (COPY ,regnum:zero ,tgt)))
	  ((= constant 1) 
	   (if overflow? (no-overflow-branches!))
	   (LAP (COPY ,src ,tgt)))
	  ((power-of-2 constant)
	   => (lambda (power-of-two)
		(if overflow?
		    (do-left-shift-overflow tgt src power-of-two)
		    (LAP (SLL ,src (& ,power-of-two) ,tgt)))))
	  (else
	   (with-values (lambda () (immediate->register (* constant fixnum-1)))
	     (lambda (prefix alias)
	       (LAP ,@prefix
		    ,@(do-multiply tgt src alias overflow?))))))))

(define (do-left-shift-overflow tgt src power-of-two)
  (let ((temp (new-temporary! src)))
    (set-current-branches!
     (lambda (overflow-label)
       (LAP (BEQ ,temp (@PCR ,overflow-label))))
     (lambda (no-overflow-label)
       (LAP (BNE ,temp (@PCR ,no-overflow-label)))))
    (with-different-source-and-target
     src tgt
     (lambda (src tgt)
       (LAP (SLL ,src (& ,power-of-two) ,tgt)
	    (SRA ,tgt (& ,power-of-two) ,temp)
	    (CMPEQ ,src ,temp ,temp))))))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/constant*register
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (with-values (lambda () (immediate->register (* constant fixnum-1)))
      (lambda (prefix alias)
	(LAP ,@prefix
	     ,@(if overflow?
		   (do-overflow-subtraction tgt alias src)
		   (LAP (SUBQ ,alias ,src ,tgt))))))))

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
  (compare (fixnum-pred-1->cc predicate)
	   (standard-source! source)
	   regnum:zero))

(define (fixnum-pred-1->cc predicate)
  (case predicate
    ((ZERO-FIXNUM?) '=)
    ((NEGATIVE-FIXNUM?) '<)
    ((POSITIVE-FIXNUM?) '>)
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
