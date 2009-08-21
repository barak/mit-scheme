#| -*-Scheme-*-

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

(define-integrable (fixnum->index-fixnum src tgt)
  (LAP (SHD () ,src 0 30 ,tgt)))

(define-integrable (object->fixnum src tgt)
  (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt)))

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

(define (fixnum->datum src tgt)
  (LAP (SHD () 0 ,src ,scheme-type-width ,tgt)))

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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FIXNUM-METHODS/1-ARG
	    (LAMBDA (TGT SRC OVERFLOW?)
	      (IF OVERFLOW?
		  (LAP (,(caddr form) (,(cadddr form))
				      ,(list-ref form 4) ,',SRC ,',TGT))
		  (LAP (,(caddr form) () ,fixed-operand ,',SRC ,',TGT))))))))

     (binary-fixnum
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS
	    (LAMBDA (TGT SRC1 SRC2 OVERFLOW?)
	      (IF OVERFLOW?
		  (LAP (,(caddr form) (,(cadddr form)) ,',SRC1 ,',SRC2 ,',TGT))
		  (LAP (,(caddr form) () ,',SRC1 ,',SRC2 ,',TGT))))))))

     (binary-out-of-line
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS/SPECIAL
	    (CONS ,(symbol-append 'HOOK:COMPILER- (cadr form))
		  (LAMBDA ()
		    ,(if (null? (cddr form))
			 `(LAP)
			 `(REQUIRE-REGISTERS! ,@(cddr form))))))))))

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
  (if (not (pair? hook))
      (error "special-binary-operation: Unknown operation" operation))

  (let* ((extra ((cdr hook)))
	 (load-1 (->machine-register source1 regnum:first-arg))		      
	 (load-2 (->machine-register source2 regnum:second-arg)))
    ;; Make regnum:first-arg the only alias for target
    (delete-register! target)
    (delete-dead-registers!)
    (add-pseudo-register-alias! target regnum:first-arg)
    (LAP ,@extra
	 ,@load-1
	 ,@load-2
	 ,@(invoke-hook (car hook))
	 ,@(if (not ovflw?)
	       (LAP)
	       (LAP (COMICLR (=) 0 ,regnum:second-arg 0))))))

(define (->machine-register source machine-reg)
  (let ((code (load-machine-register! source machine-reg)))
    ;; Prevent it from being allocated again.
    (need-register! machine-reg)
    code))

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
      #|
      (or (integer-log-base-2? factor)
	  (and (<= factor 64)
	       (or (not ovflw?)
		   (<= factor (expt 2 scheme-type-width)))))
      |#
      (or (not ovflw?)
	  (<= factor 64)
	  (integer-log-base-2? factor))))

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
       (let ((skip (if ovflw? 'NSV 'NV)))
	 (LAP (SUB (,skip) 0 ,src ,tgt))))
      (else
       (let* ((factor (abs constant))
	      (xpt (integer-log-base-2? factor)))
	 (cond ((not xpt)
		(error "fixnum-quotient: Inconsistency" constant))
	       ((>= xpt scheme-datum-width)
		(if ovflw?
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
 		(let* ((posn (- 32 xpt))
		       (delta (* (-1+ factor) fixnum-1))
		       (fits? (fits-in-11-bits-signed? delta))
		       (temp (and (not fits?) (standard-temporary!))))
		  (LAP ,@(if fits? (LAP) (load-immediate delta temp))
		       (ADD (>=) 0 ,src ,tgt)
		       ,@(if fits?
			     (LAP (ADDI () ,delta ,tgt ,tgt))
			     (LAP (ADD () ,temp ,tgt ,tgt)))
		       (EXTRS () ,tgt ,(-1+ posn) ,posn ,tgt)
		       ,@(let ((skip (if ovflw? 'TR 'NV)))
			   (if (negative? constant)
			       (LAP (DEP () 0 31 ,scheme-type-width ,tgt)
				    (SUB (,skip) 0 ,tgt ,tgt))
			       (LAP
				(DEP (,skip) 0 31 ,scheme-type-width
				     ,tgt)))))))))))))

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
    ((UNSIGNED-LESS-THAN-FIXNUM?) '<<)
    ((UNSIGNED-GREATER-THAN-FIXNUM?) '>>)
    (else
     (error "fixnum-pred->cc: unknown predicate" predicate))))

;;;; New "optimizations"

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (FIXNUM->OBJECT (REGISTER (? source)))))
  (standard-unary-conversion source target fixnum->datum))

(define (constant->additive-operand operation constant)
  (case operation
    ((PLUS-FIXNUM ONE-PLUS-FIXNUM) constant)
    ((MINUS-FIXNUM MINUS-ONE-PLUS-FIXNUM) (- constant))
    (else
     (error "constant->additive-operand: Unknown operation"
	    operation))))

(define (guarantee-fixnum-result target)
  (let ((default
	  (lambda ()
	    (deposit-immediate (ucode-type fixnum)
			       (-1+ scheme-type-width)
			       scheme-type-width
			       target))))
    #|
    ;; Unsafe at sign crossings until the tags are changed.
    (if compiler:assume-safe-fixnums?
	(LAP)
	(default))
    |#
    (default)))

(define (obj->fix-of-reg*obj->fix-of-const operation target source constant)
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(load-offset (constant->additive-operand operation constant)
			source temp)
	 ,@(object->fixnum temp target))))

(define (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target
						       source constant)
  (let* ((source (standard-source! source))
	 (target (standard-target! target)))
    (LAP ,@(load-offset (constant->additive-operand operation constant)
			source target)
	 ,@(guarantee-fixnum-result target))))

(define (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
	 operation target source constant)
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(load-offset (constant->additive-operand operation constant)
			source temp)
	 ,@(object->datum temp target))))

(define (fix->obj-of-reg*obj->fix-of-const operation target source constant)
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(load-offset
	    (constant->additive-operand operation (* constant fixnum-1))
	    source temp)
	 ,@(fixnum->object temp target))))

(define (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
	 operation target source constant)
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(load-offset
	    (constant->additive-operand operation (* constant fixnum-1))
	    source temp)
	 ,@(fixnum->datum temp target))))

(define (incr-or-decr? operation)
  (and (memq operation '(ONE-PLUS-FIXNUM MINUS-ONE-PLUS-FIXNUM))
       operation))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation incr-or-decr?)
			(OBJECT->FIXNUM (REGISTER (? source)))
			#F))
  (obj->fix-of-reg*obj->fix-of-const operation target source 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-1-ARG (? operation incr-or-decr?)
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #F)))
  (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target source 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM
	   (FIXNUM->OBJECT
	    (FIXNUM-1-ARG (? operation incr-or-decr?)
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  #F))))
  (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
   operation target source 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-1-ARG (? operation incr-or-decr?)
			 (REGISTER (? source))
			 #F)))
  (fix->obj-of-reg*obj->fix-of-const operation target source 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM
	   (FIXNUM->OBJECT
	    (FIXNUM-1-ARG (? operation incr-or-decr?)
			  (REGISTER (? source))
			  #F))))
  (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
   operation target source 1))

(define (plus-or-minus? operation)
  (and (memq operation '(PLUS-FIXNUM MINUS-FIXNUM))
       operation))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 #F))
  (obj->fix-of-reg*obj->fix-of-const operation target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS (? operation plus-or-minus?)
			  (OBJECT->FIXNUM (REGISTER (? source)))
			  (OBJECT->FIXNUM (CONSTANT (? constant)))
			  #F)))
  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
  (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target
						 source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM
	   (FIXNUM->OBJECT
	    (FIXNUM-2-ARGS (? operation plus-or-minus?)
			   (OBJECT->FIXNUM (REGISTER (? source)))
			   (OBJECT->FIXNUM (CONSTANT (? constant)))
			   #F))))
  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
  (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
   operation target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS (? operation plus-or-minus?)
			  (REGISTER (? source))
			  (OBJECT->FIXNUM (CONSTANT (? constant)))
			  #F)))
  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
  (fix->obj-of-reg*obj->fix-of-const operation target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM
	   (FIXNUM->OBJECT
	    (FIXNUM-2-ARGS (? operation plus-or-minus?)
			   (REGISTER (? source))
			   (OBJECT->FIXNUM (CONSTANT (? constant)))
			   #F))))
  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
  (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
   operation target source constant))

(define (additive-operate operation target source-1 source-2)
  (case operation
    ((PLUS-FIXNUM)
     (LAP (ADD () ,source-1 ,source-2 ,target)))
    ((MINUS-FIXNUM)
     (LAP (SUB () ,source-1 ,source-2 ,target)))
    (else
     (error "constant->additive-operand: Unknown operation"
	    operation))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
			 (REGISTER (? source-1))
			 (OBJECT->FIXNUM (REGISTER (? source-2)))
			 #F))
  (let* ((source-1 (standard-source! source-1))
	 (source-2 (standard-source! source-2))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(object->fixnum source-2 temp)
	 ,@(additive-operate operation target source-1 temp))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
			 (OBJECT->FIXNUM (REGISTER (? source-1)))
			 (REGISTER (? source-2))
			 #F))
  (let* ((source-1 (standard-source! source-1))
	 (source-2 (standard-source! source-2))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(object->fixnum source-1 temp)
	 ,@(additive-operate operation target temp source-2))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
			 (OBJECT->FIXNUM (REGISTER (? source-1)))
			 (OBJECT->FIXNUM (REGISTER (? source-2)))
			 #F))
  (let* ((source-1 (standard-source! source-1))
	 (source-2 (standard-source! source-2))
	 (temp (standard-temporary!))
	 (target (standard-target! target)))
    (LAP ,@(additive-operate operation temp source-1 source-2)
	 ,@(object->fixnum temp target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT
	   (FIXNUM-2-ARGS (? operation plus-or-minus?)
			  (OBJECT->FIXNUM (REGISTER (? source-1)))
			  (OBJECT->FIXNUM (REGISTER (? source-2)))
			  #F)))
  (let* ((source-1 (standard-source! source-1))
	 (source-2 (standard-source! source-2))
	 (target (standard-target! target)))
    (LAP ,@(additive-operate operation target source-1 source-2)
	 ,@(guarantee-fixnum-result target))))