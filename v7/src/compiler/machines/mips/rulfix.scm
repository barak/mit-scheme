#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/rulfix.scm,v 1.1 1990/05/07 04:17:20 jinx Rel $
$MC68020-Header: rules1.scm,v 4.32 90/01/18 22:43:54 GMT cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Fixnum Rules

(declare (usual-integrations))

;;;; Conversions

(define-rule statement
  ;; convert a fixnum object to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target object->fixnum))

(define-rule statement
  ;; load a fixnum constant as a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-fixnum-constant constant (standard-target! target)))

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

; "Fixnum" in this context means an integer left shifted 6 bits

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
       ,@(put-type (ucode-type fixnum) tgt)))

(define (fixnum->address src tgt)
  ; Move right by type code width and put in address bits
  (LAP (SRL ,tgt ,src ,scheme-type-width)
       ,@(put-address-bits tgt)))

(define (load-fixnum-constant constant target)
  (load-immediate (* constant fixnum-1) target))

(define-integrable fixnum-1
  (expt 2 scheme-type-width))

(define-integrable -fixnum-1
  (- fixnum-1))

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

; Assumption: overflow sets or clears register regnum:assembler-temp,
; and this code is followed immediately by a branch on overflow

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (if overflow?
	(let ((label-1 (generate-label))
	      (label-2 (generate-label)))
	  (LAP (BLTZ ,src (@PCR ,label-1))
	       (ADDI ,regnum:assembler-temp 0 0)
	       (ADDIU ,regnum:first-arg ,src ,fixnum-1)
	       (BGEZ ,regnum:assembler-temp (@PCR ,label-2))
	       (ADDIU ,tgt ,src ,fixnum-1)
	       (ADDI ,regnum:assembler-temp 0 1)
             (LABEL ,label-1)
	       (ADDIU ,tgt ,src ,fixnum-1)
	     (LABEL ,label-2)))
	(LAP (ADDIU ,tgt ,src ,fixnum-1)))))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM
  fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (if overflow?
	(let ((label-1 (generate-label))
	      (label-2 (generate-label)))
	  (LAP (BGEZ ,src (@PCR ,label-1))		      ; Can't overflow if >0
	         (ADDI ,regnum:assembler-temp 0 0)	      ; Clear o'flow flag
	       (ADDIU ,regnum:assembler-temp ,src ,-fixnum-1) ; Do subtraction into temp
	       (BGEZ ,regnum:assembler-temp (@PCR ,label-2))  ; Overflow? -> label-2
	         (ADDIU ,regnum:assembler-temp 0 1)	      ; Set overflow flag
 	       (ADDI ,regnum:assembler-temp 0 0)	      ; Clear overflow flag
             (LABEL ,label-1)
	       (ADDIU ,tgt ,src ,-fixnum-1)		     ; Do subtraction
	     (LABEL ,label-2)))
	(LAP (ADDIU ,tgt ,src ,-fixnum-1)))))

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

(define (do-overflow-addition tgt src1 src2)
  (let ((label-1 (generate-label))
	(label-2 (generate-label)))
    (LAP (ADDI ,regnum:assembler-temp 0 0)
	 (XOR  ,regnum:first-arg ,src1 ,src2)
	 (BLTZ ,regnum:first-arg (@PCR ,label-1))
	 (ADDU ,regnum:first-arg ,src1 ,src2)
	 (XOR  ,regnum:first-arg ,src1 ,regnum:first-arg)
	 (BGEZ ,regnum:first-arg (@PCR ,label-2))
	 (ADDU ,tgt ,src1 ,src2)
	 (ADDI ,regnum:assembler-temp 0 1)
       (LABEL ,label-1)
         (ADDU ,tgt ,src1 ,src2)
       (LABEL ,label-2))))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(do-overflow-addition tgt src1 src2)
	(LAP (ADDU ,tgt ,src1 ,src2)))))

(define (do-overflow-subtraction tgt src1 src2)
  (let ((label-1 (generate-label))
	(label-2 (generate-label)))
    (LAP (ADDI ,regnum:assembler-temp 0 0)
	 (XOR  ,regnum:first-arg ,src1 ,src2)
	 (BGEZ ,regnum:first-arg (@PCR ,label-1))
	 (SUBU ,regnum:first-arg ,src1 ,src2)
	 (XOR  ,regnum:first-arg ,regnum:first-arg ,src1)
	 (BGEZ ,regnum:first-arg (@PCR ,label-2))
	 (SUBU ,tgt ,src1 ,src2)
	 (ADDI ,regnum:assembler-temp 0 1)
       (LABEL ,label-1)
	 (SUBU ,tgt ,src1 ,src2)
       (LABEL ,label-2))))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(do-overflow-subtraction tgt src1 src2)
	(LAP (SUB ,tgt ,src1 ,src2)))))

(define (do-multiply tgt src1 src2 overflow?)
  (if overflow?
      (let ((temp (standard-temporary!))
	    (label-1 (generate-label)))
	(LAP (SRL  ,regnum:first-arg ,src1 6)	; Unshift 1st arg.
	     (MULT ,regnum:first-arg ,src2)	; Result is left justified
	     (MFLO ,temp)
	     (SRA  ,temp ,temp 31)		; Get sign bit only
	     (MFHI ,regnum:first-arg)		; Should match the sign
	     (BNE  ,regnum:first-arg ,temp (@pcr ,label-1))
 	       (ADDI ,regnum:assembler-temp 0 1) ; Set overflow flag
	     (ADDI ,regnum:assembler-temp 0 0)	; Clear overflow flag
	     (MFLO ,tgt)
           (LABEL ,label-1)))
      (LAP (SRL  ,regnum:assembler-temp ,src1 6)
	   (MULT ,regnum:assembler-temp ,src2)
	   (MFLO ,tgt))))

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

(define-arithmetic-method 'PLUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (if overflow?
	(if (zero? constant)
	    (LAP (ADDI ,regnum:assembler-temp 0 0))
	    (let ((temp (standard-temporary!)))
	      (LAP ,@(load-fixnum-constant constant temp)
		   ,@(do-overflow-addition tgt src temp))))
	(add-immediate (* fixnum-1 constant) src tgt))))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (if overflow?
	(if (zero? constant)
	    (LAP (ADDI ,regnum:assembler-temp 0 0)
		 (ADD ,tgt 0 ,src))
	    (let ((temp (standard-temporary!)))
	      (LAP ,@(load-fixnum-constant constant temp)
		   ,@(do-overflow-subtraction tgt src temp))))
	(add-immediate (- (* constant fixnum-1)) src tgt))))

(define-arithmetic-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (define (power-of-two? integer)
      (cond ((<= integer 0) #F)
	    ((= integer 1) 0)
	    ((odd? integer) #F)
	    ((power-of-two? (quotient integer 2)) => 1+)
	    (else #F)))
    (define (multiply-by-power-of-two what-power)
      (if overflow?
	  (let ((label-1 (generate-label)))
	    (LAP (SLL  ,regnum:first-arg ,src ,what-power)
		 (SRA  ,regnum:assembler-temp ,regnum:first-arg ,what-power)
		 (BNE  ,regnum:assembler-temp ,src (@pcr ,label-1))
	           (ADDI ,regnum:assembler-temp 0 1)
		 (ADDI ,regnum:assembler-temp 0 0)
		 (SLL  ,tgt ,src ,what-power)
	       (LABEL ,label-1)))
	  (LAP (SLL ,tgt ,src ,what-power))))
    (cond ((zero? constant)
	   (LAP ,@(if overflow?
		      (LAP (ADDI ,regnum:assembler-temp 0 0))
		      '())
		(ADDI ,tgt 0 0)))
	  ((= constant 1) 
	   (LAP ,@(if overflow?
		      (LAP (ADDI ,regnum:assembler-temp 0 0))
		      '())
		(ADD ,tgt 0 ,src)))
	   ((power-of-two? constant) => multiply-by-power-of-two)
	   (else
	    (let ((temp (standard-temporary!)))
	      (LAP ,@(load-fixnum-constant constant temp)
		   ,@(do-multiply tgt src temp overflow?)))))))

(define (fixnum-2-args/operator/constant*register operation)
  (lookup-arithmetic-method operation
			    fixnum-methods/2-args/constant*register))

(define fixnum-methods/2-args/constant*register
  (list 'FIXNUM-METHODS/2-ARGS/CONSTANT*REGISTER))

(define-arithmetic-method 'MINUS-FIXNUM
  fixnum-methods/2-args/constant*register
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (let ((temp (standard-temporary!)))
      (LAP ,@(load-fixnum-constant constant temp)
	   ,@(if overflow?
		 (do-overflow-subtraction tgt temp src)
		 (LAP (SUB ,tgt ,temp ,src)))))))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

;;;; Predicates

;;; This is a kludge.  It assumes that the last instruction of the
;;; arithmetic operation that may cause an overflow condition will
;;; have set regnum:assembler-temp to 0 if there is no overflow.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches!
   (lambda (label)
     (LAP (BNE ,regnum:assembler-temp 0 (@PCR ,label)) (NOP)))
   (lambda (label)
     (LAP (BEQ ,regnum:assembler-temp 0 (@PCR ,label)) (NOP))))
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (fixnum-pred-1->cc predicate)
	   (standard-source! source)
	   0))

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