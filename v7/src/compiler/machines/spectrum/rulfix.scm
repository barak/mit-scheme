#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/rulfix.scm,v 4.42 1992/08/19 13:36:01 jinx Exp $

Copyright (c) 1989-1992 Massachusetts Institute of Technology

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

#|
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
|#

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT (? value)))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #F))
  (QUALIFIER (integer-log-base-2? value))
  (standard-unary-conversion source target
			     (make-scaled-object->fixnum value)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? value)))
			 #F))
  (QUALIFIER (integer-log-base-2? value))
  (standard-unary-conversion source target
			     (make-scaled-object->fixnum value)))

#|
;; Superseded by code below

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
|#

(define-integrable (fixnum->index-fixnum src tgt)
  (LAP (SHD () ,src 0 30 ,tgt)))

(define-integrable (object->fixnum src tgt)
  (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt)))

#|
(define-integrable (object->index-fixnum src tgt)
  (LAP (SHD () ,src 0 ,(- scheme-datum-width 2) ,tgt)))
|#

(define (make-scaled-object->fixnum factor)
  (let ((shift (integer-log-base-2? factor)))
    (cond ((not shift)
	   (error "make-scaled-object->fixnum: Not a power of 2" factor))
	  ((> shift scheme-datum-width)
	   (error "make-scaled-object->fixnum: shift too large" shift))
	  (else
	   (lambda (src tgt)
	     (LAP (SHD () ,src 0 ,(- scheme-datum-width shift) ,tgt)))))))

(define-integrable (address->fixnum src tgt)
  (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt)))

(define-integrable (fixnum->object src tgt)
  (LAP ,@(load-immediate (ucode-type fixnum) regnum:addil-result)
       (SHD () ,regnum:addil-result ,src ,scheme-type-width ,tgt)))

(define (fixnum->address src tgt)
  (LAP (SHD () ,regnum:quad-bitmask ,src ,scheme-type-width ,tgt)))

(define (load-fixnum-constant constant target)
  (load-immediate (* constant fixnum-1) target))

(define-integrable fixnum-1
  ;; (expt 2 scheme-type-width) ***
  64)

;;;; Arithmetic Operations

(define-rule statement
  ;; execute a unary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (QUALIFIER (fixnum-1-arg/operator? operation))
  (standard-unary-conversion source target
    (lambda (source target)
      ((fixnum-1-arg/operator operation) target source overflow?))))

(define-integrable (fixnum-1-arg/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/1-arg))

(define-integrable (fixnum-1-arg/operator? operation)
  (arithmetic-method? operation fixnum-methods/1-arg))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-rule statement
  ;; execute a binary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (fixnum-2-args/operator? operation))
  (standard-binary-conversion source1 source2 target
    (lambda (source1 source2 target)
      ((fixnum-2-args/operator operation) target source1 source2 overflow?))))

(define-integrable (fixnum-2-args/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args))

(define-integrable (fixnum-2-args/operator? operation)
  (arithmetic-method? operation fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

;; Some operations are too long to do in-line.
;; Use out-of-line utilities.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (fixnum-2-args/special-operator? operation))
  (special-binary-operation
   operation
   (fixnum-2-args/special-operator operation)
   target source1 source2 overflow?))

(define-integrable (fixnum-2-args/special-operator operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args/special))

(define-integrable (fixnum-2-args/special-operator? operation)
  (arithmetic-method? operation fixnum-methods/2-args/special))

(define fixnum-methods/2-args/special
  (list 'FIXNUM-METHODS/2-ARGS/SPECIAL))

;; Note: Bit-wise operations never overflow, therefore they always
;; skip the branch (cond = TR).  Perhaps they should error?

;; Note: The commas in the macros do not follow normal QUASIQUOTE patterns.
;; This is due to a bad interaction between QUASIQUOTE and LAP!

(let-syntax
    ((unary-fixnum
      (macro (name instr nsv fixed-operand)
	`(define-arithmetic-method ',name fixnum-methods/1-arg
	   (lambda (tgt src overflow?)
	     (if overflow?
		 (LAP (,instr (,nsv) ,fixed-operand ,',src ,',tgt))
		 (LAP (,instr () ,fixed-operand ,',src ,',tgt)))))))

     (binary-fixnum
      (macro (name instr nsv)
	`(define-arithmetic-method ',name fixnum-methods/2-args
	   (lambda (tgt src1 src2 overflow?)
	     (if overflow?
		 (LAP (,instr (,nsv) ,',src1 ,',src2 ,',tgt))
		 (LAP (,instr () ,',src1 ,',src2 ,',tgt)))))))

     (binary-out-of-line
      (macro (name . regs)
	`(define-arithmetic-method ',name fixnum-methods/2-args/special
	   (cons ,(symbol-append 'HOOK:COMPILER- name)
		 (lambda ()
		   ,(if (null? regs)
			`(LAP)
			`(require-registers! ,@regs))))))))

  (unary-fixnum ONE-PLUS-FIXNUM ADDI NSV ,fixnum-1)
  (unary-fixnum MINUS-ONE-PLUS-FIXNUM ADDI NSV ,(- fixnum-1))
  (unary-fixnum FIXNUM-NOT SUBI TR ,(- fixnum-1))

  (binary-fixnum PLUS-FIXNUM ADD NSV)
  (binary-fixnum MINUS-FIXNUM SUB NSV)
  (binary-fixnum FIXNUM-AND AND TR)
  (binary-fixnum FIXNUM-ANDC ANDCM TR)
  (binary-fixnum FIXNUM-OR OR TR)
  (binary-fixnum FIXNUM-XOR XOR TR)

  (binary-out-of-line MULTIPLY-FIXNUM fp4 fp5)
  (binary-out-of-line FIXNUM-QUOTIENT fp4 fp5)
  (binary-out-of-line FIXNUM-REMAINDER fp4 fp5 regnum:addil-result)
  (binary-out-of-line FIXNUM-LSH))

;;; Out of line calls.

;; Arguments are passed in regnum:first-arg and regnum:second-arg.
;; Result is returned in regnum:first-arg, and a boolean is returned
;; in regnum:second-arg indicating wheter there was overflow.

(define (special-binary-operation operation hook target source1 source2 ovflw?)
  (define (->machine-register source machine-reg)
    (let ((code (load-machine-register! source machine-reg)))
      ;; Prevent it from being allocated again.
      (need-register! machine-reg)
      code))

  (if (not (pair? hook))
      (error "special-binary-operation: Unknown operation" operation))

  (let* ((extra ((cdr hook)))
	 (load-1 (->machine-register source1 regnum:first-arg))		      
	 (load-2 (->machine-register source2 regnum:second-arg)))
    ;; Make regnum:first-arg the only alias for target
    (delete-register! target)
    (add-pseudo-register-alias! target regnum:first-arg)
    (LAP ,@extra
	 ,@load-1
	 ,@load-2
	 ;; Hopefully a peep-hole optimizer will switch this instruction
	 ;; and the preceding one, and remove the nop.
	 (BLE () (OFFSET ,(car hook) 4 ,regnum:scheme-to-interface-ble))
	 (NOP ())
	 ,@(if (not ovflw?)
	       (LAP)
	       (LAP (COMICLR (=) 0 ,regnum:second-arg 0))))))

;;; Binary operations with one argument constant.

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER
   (fixnum-2-args/operator/register*constant? operation constant overflow?))
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
  (QUALIFIER
   (fixnum-2-args/operator/constant*register? operation constant overflow?))
  (standard-unary-conversion source target
    (lambda (source target)
      (if (fixnum-2-args/commutative? operation)
	  ((fixnum-2-args/operator/register*constant operation)
	   target source constant overflow?)
	  ((fixnum-2-args/operator/constant*register operation)
	   target constant source overflow?)))))

(define (define-arithconst-method name table qualifier code-gen)
  (define-arithmetic-method name table
    (cons code-gen qualifier)))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))

(define-integrable (fixnum-2-args/operator/register*constant operation)
  (car (lookup-arithmetic-method operation
				 fixnum-methods/2-args/register*constant)))

(define (fixnum-2-args/operator/register*constant? operation constant ovflw?)
  (let ((handler (arithmetic-method? operation
				     fixnum-methods/2-args/register*constant)))
    (and handler
	 ((cddr handler) constant ovflw?))))

(define fixnum-methods/2-args/register*constant
  (list 'FIXNUM-METHODS/2-ARGS/REGISTER*CONSTANT))

(define-integrable (fixnum-2-args/operator/constant*register operation)
  (car (lookup-arithmetic-method operation
				 fixnum-methods/2-args/constant*register)))

(define (fixnum-2-args/operator/constant*register? operation constant ovflw?)
  (let ((handler (arithmetic-method? operation
				     fixnum-methods/2-args/constant*register)))
    (or (and handler
	     ((cddr handler) constant ovflw?))
	(and (fixnum-2-args/commutative? operation)
	     (fixnum-2-args/operator/register*constant? operation
							constant ovflw?)))))

(define fixnum-methods/2-args/constant*register
  (list 'FIXNUM-METHODS/2-ARGS/CONSTANT*REGISTER))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

(define-arithconst-method 'PLUS-FIXNUM fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?					; ignored
    (fits-in-11-bits-signed? (* constant fixnum-1)))
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (* constant fixnum-1)))
      (if overflow?
	  (cond ((zero? constant)
		 (LAP (ADD (TR) ,src 0 ,tgt)))
		((fits-in-11-bits-signed? value)
		 (LAP (ADDI (NSV) ,value ,src ,tgt)))
		(else
		 (let ((temp (standard-temporary!)))
		   (LAP ,@(load-fixnum-constant constant temp)
			(ADD (NSV) ,src ,temp ,tgt)))))
	  (load-offset value src tgt)))))

(define-arithconst-method 'MINUS-FIXNUM fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?					; ignored
    (fits-in-11-bits-signed? (* constant fixnum-1)))
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (- (* constant fixnum-1))))
      (if overflow?
	  (cond ((zero? constant)
		 (LAP (SUB (TR) ,src 0 ,tgt)))
		((fits-in-11-bits-signed? value)
		 (LAP (ADDI (NSV) ,value ,src ,tgt)))
		(else
		 (let ((temp (standard-temporary!)))
		   (LAP ,@(load-fixnum-constant constant temp)
			(SUB (NSV) ,src ,temp ,tgt)))))
	  (load-offset value src tgt)))))

(define-arithconst-method 'MINUS-FIXNUM fixnum-methods/2-args/constant*register
  (lambda (constant ovflw?)
    ovflw?					; ignored
    (fits-in-11-bits-signed? (* constant fixnum-1)))
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (* constant fixnum-1)))
      (if (fits-in-11-bits-signed? value)
	  (if overflow?
	      (LAP (SUBI (NSV) ,value ,src ,tgt))
	      (LAP (SUBI () ,value ,src ,tgt)))
	  (let ((temp (standard-temporary!)))
	    (LAP ,@(load-fixnum-constant constant temp)
		 ,@(if overflow?
		       (LAP (SUB (NSV) ,temp ,src ,tgt))
		       (LAP (SUB () ,temp ,src ,tgt)))))))))

(define-arithconst-method 'FIXNUM-LSH fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    constant ovflw?				; ignored
    true)
  (lambda (tgt src shift overflow?)
    ;; What does overflow mean for a logical shift?
    ;; The code commented out below corresponds to arithmetic shift
    ;; overflow conditions.
    (guarantee-signed-fixnum shift)
    (cond ((zero? shift)
	   (cond ((not overflow?)
		  (copy src tgt))
		 ((= src tgt)
		  (LAP (SKIP (TR))))
		 (else
		  (LAP (COPY (TR) ,src ,tgt)))))
	  ((negative? shift)
	   ;; Right shift
	   (let ((shift (- shift)))
	     (cond ((< shift scheme-datum-width)
		    (LAP (SHD () 0 ,src ,shift ,tgt)
			 ;; clear shifted bits
			 (DEP (,(if overflow? 'TR 'NV))
			       0 31 ,scheme-type-width ,tgt)))
		   ((not overflow?)
		    (copy 0 tgt))
		   (else
		    (LAP (COPY (TR) 0 ,tgt))))))
	  (else
	   ;; Left shift
	   (if (>= shift scheme-datum-width)
	       (if (not overflow?)
		   (copy 0 tgt)
		   #| (LAP (COMICLR (=) 0 ,src ,tgt)) |#
		   (LAP (COMICLR (TR) 0 ,src ,tgt)))
	       (let ((nbits (- 32 shift)))
		 (if overflow?
		     #|
		     ;; Arithmetic overflow condition accomplished
		     ;; by skipping all over the place.
		     ;; Another possibility is to use the shift-and-add
		     ;; instructions, which compute correct signed overflow
		     ;; conditions.
		     (let ((nkept (- 32 shift))
			   (temp (standard-temporary!)))
		       (LAP (ZDEP () ,src ,(- nkept 1) ,nkept ,tgt)
			    (EXTRS (=) ,src ,(- shift 1) ,shift ,temp)
			    (COMICLR (<>) -1 ,temp 0)
			    (SKIP (TR))))
		     |#
		     (LAP (ZDEP (TR) ,src ,(- nbits 1) ,nbits ,tgt))
		     (LAP (ZDEP () ,src ,(- nbits 1) ,nbits ,tgt)))))))))

(define-integrable (divisible? m n)
  (zero? (remainder m n)))

(define (integer-log-base-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define-arithconst-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    (let ((factor (abs constant)))
      (or (integer-log-base-2? factor)
	  (and (<= factor 64)
	       (or (not ovflw?)
		   (<= factor (expt 2 scheme-type-width)))))))

  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (let ((skip (if overflow? 'NSV 'NV)))
      (case constant
	((0)
	 (if overflow?
	     (LAP (COPY (TR) 0 ,tgt))
	     (LAP (COPY () 0 ,tgt))))
	((1)
	 (if overflow?
	     (LAP (COPY (TR) ,src ,tgt))
	     (copy src tgt)))
	((-1)
	 (LAP (SUB (,skip) 0 ,src ,tgt)))
	(else
	 (let* ((factor (abs constant))
		(src+ (if (negative? constant) tgt src))
		(xpt (integer-log-base-2? factor)))
	   (cond ((not overflow?)
		  (LAP ,@(if (negative? constant)
			     (LAP (SUB () 0 ,src ,tgt))
			     (LAP))
		       ,@(if xpt
			     (LAP (SHD () ,src+ 0 ,(- 32 xpt) ,tgt))
			     (expand-factor tgt src+ factor false 'NV
					    (lambda ()
					      (LAP))))))
		 ((and xpt (> xpt 6))
		  (let* ((high (standard-temporary!))
			 (low (if (or (= src tgt) (negative? constant))
				  (standard-temporary!)
				  src))
			 (nbits (- 32 xpt))
			 (core
			  (LAP (SHD () ,low 0 ,nbits ,tgt)
			       (SHD (=) ,high ,low ,(-1+ nbits) ,high)
			       (COMICLR (<>) -1 ,high 0)
			       (SKIP (TR)))))
		    (if (negative? constant)
			(LAP (EXTRS () ,src 0 1 ,high)
			     (SUB () 0 ,src ,low)
			     (SUBB () 0 ,high ,high)
			     ,@core)
			(LAP ,@(if (not (= src low))
				   (LAP (COPY () ,src ,low))
				   (LAP))
			     (EXTRS () ,low 0 1 ,high)
			     ,@core))))
		 (else
		  (LAP ,@(if (negative? constant)
			     (LAP (SUB (SV) 0 ,src ,tgt))
			     (LAP))
		       ,@(expand-factor tgt src+ factor (negative? constant)
					'NSV
					(lambda ()
					  (LAP (SKIP (TR))))))))))))))

(define (expand-factor tgt src factor skipping? condition skip)
  (define (sh3add condition src1 src2 tgt)
    (LAP (SH3ADD (,condition) ,src1 ,src2 ,tgt)))

  (define (sh2add condition src1 src2 tgt)
    (LAP (SH2ADD (,condition) ,src1 ,src2 ,tgt)))

  (define (sh1add condition src1 src2 tgt)
    (LAP (SH1ADD (,condition) ,src1 ,src2 ,tgt)))

  (define (handle factor fixed)
    (define (wrap instr next value)
      (let ((code? (car next))
	    (result-reg (cadr next))
	    (temp-reg (caddr next))
	    (code (cadddr next)))
	(list true
	      tgt
	      temp-reg
	      (LAP ,@code
		   ,@(if code?
			 (skip)
			 (LAP))
		   ,@(instr condition result-reg value tgt)))))

    (cond ((zero? factor) (list false 0 fixed (LAP)))
	  ((= factor 1) (list false fixed fixed (LAP)))
	  ((divisible? factor 8)
	   (wrap sh3add (handle (/ factor 8) fixed) 0))
	  ((divisible? factor 4)
	   (wrap sh2add (handle (/ factor 4) fixed) 0))
	  ((divisible? factor 2)
	   (wrap sh1add (handle (/ factor 2) fixed) 0))
	  (else
	   (let* ((f1 (-1+ factor))
		  (fixed (if (or (not (= fixed src))
				 (not (= src tgt))
				 (and (integer-log-base-2? f1)
				      (< f1 16)))
			   fixed
			   (standard-temporary!))))
	     (cond ((divisible? f1 8)
		    (wrap sh3add (handle (/ f1 8) fixed) fixed))
		   ((divisible? f1 4)
		    (wrap sh2add (handle (/ f1 4) fixed) fixed))
		   (else
		    (wrap sh1add (handle (/ f1 2) fixed) fixed)))))))

  (let ((result (handle factor src)))
    (let ((result-reg (cadr result))
	  (temp-reg (caddr result))
	  (code (cadddr result)))
      
      (LAP ,@(cond ((= temp-reg src)
		    (LAP))
		   ((not skipping?)
		    (LAP (COPY () ,src ,temp-reg)))
		   (else
		    (LAP (COPY (TR) ,src ,temp-reg)
			 ,@(skip))))
	   ,@code
	   ,@(cond ((= result-reg tgt)
		    (LAP))
		   ((eq? condition 'NV)
		    (LAP (COPY () ,result-reg ,tgt)))
		   (else
		    (LAP (COPY (TR) ,result-reg ,tgt)
			 ,@(skip))))))))

;;;; Division

(define-arithconst-method 'FIXNUM-QUOTIENT
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?				; ignored
    (integer-log-base-2? (abs constant)))
  (lambda (tgt src constant ovflw?)
    (guarantee-signed-fixnum constant)
    (case constant
      ((1)
       (if ovflw?
	   (LAP (COPY (TR) ,src ,tgt))
	   (copy src tgt)))
      ((-1)
       (let ((skip (if ovflw? 'NSV 'TR)))
	 (LAP (SUB (,skip) 0 ,src ,tgt))))
      (else
       (let* ((factor (abs constant))
	      (xpt (integer-log-base-2? factor)))
	 (cond ((not xpt)
		(error "fixnum-quotient: Inconsistency" constant))
	       ((>= xpt scheme-datum-width)
		(if ovfwl?
		    (LAP (COPY (TR) 0 ,tgt))
		    (copy 0 tgt)))
	       (else
		;; Note: The following cannot overflow because we are
		;; dividing by a constant whose absolute value is
		;; strictly greater than 1.  However, we need to
		;; negate after shifting, not before, because negating
		;; the input can overflow (if it is -0).
		;; This unfortunately implies an extra instruction in the
		;; case of negative constants because if this weren't the
		;; case, we could substitute the first ADD instruction for
		;; a SUB for negative constants, and eliminate the SUB later.
		(let* ((posn (- 32 (+ xpt scheme-type-width)))
		       (delta (* (-1+ factor) fixnum-1))
		       (fits? (fits-in-11-bits-signed? delta))
		       (temp (and (not fits?) (standard-temporary!))))
	 
		  (LAP ,@(if fits?
			     (LAP)
			     (load-immediate delta temp))
		       (ADD (>=) 0 ,src ,tgt)
		       ,@(if (fits-in-11-bits-signed? delta)
			     (LAP (ADDI () ,delta ,tgt ,tgt))
			     (LAP (ADD () ,temp ,tgt ,tgt)))
		       (EXTRS () ,tgt ,(-1+ posn) ,posn ,tgt)
		       ,@(if (negative? constant)
			     (LAP (SUB () 0 ,tgt ,tgt))
			     (LAP))
		       ,@(if ovflw?
			     (LAP
			      (DEP (TR) 0 31 ,scheme-type-width ,tgt))
			     (LAP
			      (DEP () 0 31 ,scheme-type-width ,tgt))))))))))))

(define-arithconst-method 'FIXNUM-REMAINDER
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?				; ignored
    (integer-log-base-2? (abs constant)))
  (lambda (tgt src constant ovflw?)
    (guarantee-signed-fixnum constant)
    (case constant
      ((1 -1)
       (if ovflw?
	   (LAP (COPY (TR) 0 ,tgt))
	   (LAP (COPY () 0 ,tgt))))
      (else
       (let ((sign (standard-temporary!))
	     (len (let ((xpt (integer-log-base-2? (abs constant))))
		    (and xpt (+ xpt scheme-type-width)))))
	 (let ((sgn-len (- 32 len)))
	   (if (not len)
	       (error "fixnum-remainder: Inconsistency" constant ovflw?))
	   (LAP (EXTRS () ,src 0 1 ,sign)
		(EXTRU (=) ,src 31 ,len ,tgt)
		(DEP () ,sign ,(- sgn-len 1) ,sgn-len ,tgt)
		,@(if ovflw?
		      (LAP (SKIP (TR)))
		      (LAP)))))))))

;;;; Predicates

;; This is a kludge.  It assumes that the last instruction of the
;; arithmetic operation that may cause an overflow condition will skip
;; the following instruction if there was no overflow, ie., the last
;; instruction will nullify using NSV (or TR if overflow is
;; impossible).  The code for the alternative is a real kludge because
;; we can't force the arithmetic instruction that precedes this code
;; to use the inverted condition.  Hopefully a peep-hole optimizer
;; will fix this.  The linearizer attempts to use the "good" branch.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches!
   (lambda (label)
     (LAP (B (N) (@PCR ,label))))
   (lambda (label)
     (LAP (SKIP (TR))
	  (B (N) (@PCR ,label)))))
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (fixnum-pred->cc predicate)
	   (standard-source! source)
	   0))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (compare (fixnum-pred->cc predicate)
	   (standard-source! source1)
	   (standard-source! source2)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (compare-fixnum/constant*register (invert-condition-noncommutative
				     (fixnum-pred->cc predicate))
				    constant
				    (standard-source! source)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? source)))
  (compare-fixnum/constant*register (fixnum-pred->cc predicate)
				    constant
				    (standard-source! source)))

(define-integrable (compare-fixnum/constant*register cc n r)
  (guarantee-signed-fixnum n)
  (compare-immediate cc (* n fixnum-1) r))

(define (fixnum-pred->cc predicate)
  (case predicate
    ((ZERO-FIXNUM? EQUAL-FIXNUM?) '=)
    ((NEGATIVE-FIXNUM? LESS-THAN-FIXNUM?) '<)
    ((POSITIVE-FIXNUM? GREATER-THAN-FIXNUM?) '>)
    (else
     (error "fixnum-pred->cc: unknown predicate" predicate))))