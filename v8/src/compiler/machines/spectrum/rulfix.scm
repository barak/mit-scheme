#| -*-Scheme-*-

$Id: d3bae7b39bb455947139b348116dbcf280e467f6 $

Copyright (c) 1989-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP Generation Rules: Fixnum Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Conversions

;;  NOTE(1):  This file used to work for either tagged or untagged fixnums.
;;  This is no longer true.  As the 8.0 compiler developed, it stopped
;;  being convenient to test both, and bugs have crept into the tagged
;;  code.  It seemed simplest to relegate the tagged code to RCS
;;  history and clean up this file.
;;
;;  NOTE(2):  The 8.0 compiler does not generate the conversion operations
;;  OBJECT->FIXNUM, FIXNUM->OBJECT and OBJECT->UNSIGNED-FIXNUM.
;;
;;  NOTE(3):  The new rtl generator  never generates overflow codes.
;;
;;  NOMENCLATURE:
;;  OBJECT  means an object represented in standard Scheme form
;;  ADDRESS means a hardware pointer to an address; on the PA this
;;          means it has the quad bits set correctly
;;  FIXNUM  means a value without type code, in a form suitable for
;;          machine arithmetic.  If UNTAGGED-FIXNUMS? is #T (i.e.
;;          POSITIVE-FIXNUM is type code 0, NEGATIVE-FIXNUM is type
;;          code -1), then we simply use the standard hardware
;;          representation of integers.  Otherwise, we shift the
;;          integer so that the Scheme fixnum sign bit is stored in the
;;          hardware sign bit: i.e. left shifted by typecode-width (6)
;;          bits.  The tagged version is no longer working(see NOTE 1)

(if (not untagged-fixnums?)
    (error "RULFIX: no longer works for tagged fixnums."))

;;;; Arithmetic Operations

(define-rule statement
  ;; execute a unary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (QUALIFIER (fixnum-1-arg/operator? operation))
  (standard-unary-conversion
   source
   target
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
				((fixnum-2-args/operator operation)
				 target source1 source2 overflow?))))

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
	     (if overflow?  (no-overflow-branches!))
	     (LAP (,instr () ,fixed-operand ,',src ,',tgt))))))

     (binary-fixnum
      (macro (name instr nsv)
	`(define-arithmetic-method ',name fixnum-methods/2-args
	   (lambda (tgt src1 src2 overflow?)
	     (if overflow?  (no-overflow-branches!))
	     (LAP (,instr () ,',src1 ,',src2 ,',tgt))))))

     (binary-out-of-line
      (macro (name . regs)
	`(define-arithmetic-method ',name fixnum-methods/2-args/special
	   (cons ,(symbol-append 'HOOK:COMPILER- name)
		 (lambda ()
		   ,(if (null? regs)
			`(LAP)
			`(require-registers! ,@regs))))))))

  (unary-fixnum ONE-PLUS-FIXNUM ADDI NSV 1)
  (unary-fixnum MINUS-ONE-PLUS-FIXNUM ADDI NSV -1)
  (unary-fixnum FIXNUM-NOT SUBI TR -1)  ;;?? XOR?

  (binary-fixnum PLUS-FIXNUM  ADD NSV)
  (binary-fixnum MINUS-FIXNUM SUB NSV)
  (binary-fixnum FIXNUM-AND   AND TR)
  (binary-fixnum FIXNUM-ANDC  ANDCM TR)
  (binary-fixnum FIXNUM-OR    OR TR)
  (binary-fixnum FIXNUM-XOR   XOR TR)

  (binary-out-of-line MULTIPLY-FIXNUM fp4 fp5)
  (binary-out-of-line FIXNUM-QUOTIENT fp4 fp5)
  (binary-out-of-line FIXNUM-REMAINDER fp4 fp5 regnum:addil-result)
  (binary-out-of-line FIXNUM-LSH))

;;; Out of line calls.

;; Arguments are passed in regnum:first-arg and regnum:second-arg.
;; Result is returned in regnum:first-arg, and a boolean is returned
;; in regnum:second-arg indicating wheter there was overflow.

;; This version fixes the problem with the previous that a reduction merge 
;; like (if ... (fix:remainder x y) 0) would never assign target (=r2)

(define (special-binary-operation operation hook target source1 source2 ovflw?)
  (if (not (pair? hook))
      (error "special-binary-operation: Unknown operation" operation))

  (let* ((extra ((cdr hook)))
	 (load-1 (->machine-register source1 regnum:first-arg))		      
	 (load-2 (->machine-register source2 regnum:second-arg)))
    (let ((core
	   (lambda (extra-2)
	     (if ovflw? (error "RULFIX: overflow branches obsolete"))
	     (LAP ,@extra
		  ,@load-1
		  ,@load-2
		  ,@(invoke-hook (car hook))
		  ,@extra-2
		  ,@(if (not ovflw?)
			(LAP)
			(LAP (COMICLR (=) 0 ,regnum:second-arg 0)))))))
      (if (machine-register? target)
	  (begin
	    (standard-target! target)
	    (delete-dead-registers!)
	    (core (copy regnum:first-arg target)))
	  (begin
	    (delete-register! target)
	    (delete-dead-registers!)
	    (add-pseudo-register-alias! target regnum:first-arg)
	    (core (LAP)))))))

;;; Binary operations with one argument constant.

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (CONSTANT (? constant))
			 (? overflow?)))
  (QUALIFIER
   (fixnum-2-args/operator/register*constant? operation constant overflow?))
  (standard-unary-conversion
   source target
   (lambda (source target)
     ((fixnum-2-args/operator/register*constant operation)
      target source constant overflow?))))

(define-rule statement
  ;; execute binary fixnum operation with constant first arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (CONSTANT (? constant))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER
   (fixnum-2-args/operator/constant*register? operation constant overflow?))
  (standard-unary-conversion
   source target
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

;; precondition for considering a constant for fixnum operations:

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

;;;; The following are for special case handling where one argument is
;;;; a compile-time constant.  Each has a predicate to see if the
;;;; constant is of the form required for the open coding to work.

(define-integrable (divisible? m n)
  (zero? (remainder m n)))

(define (integer-log-base-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define-arithconst-method 'PLUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?
    ;; ignored because success of generic arithmetic pretest
    ;; guarantees it won't overflow
    (fits-in-14-bits-signed? constant))
  (lambda (tgt src constant overflow?)
    (if overflow? (no-overflow-branches!))
    (load-offset constant src tgt)))

(define-arithconst-method 'MINUS-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?
    ;; ignored because success of generic arithmetic pretest
    ;; guarantees it won't overflow
    (and (signed-fixnum? constant)
	 (fits-in-14-bits-signed? (- constant))))
  (lambda (tgt src constant overflow?)
    (if overflow? (no-overflow-branches!))
    (let ((value (- constant)))
      (load-offset value src tgt))))

(define-arithconst-method 'MINUS-FIXNUM
  fixnum-methods/2-args/constant*register
  (lambda (constant ovflw?)
    ovflw?				; ignored
    (fits-in-11-bits-signed? constant))
  (lambda (tgt constant src overflow?)
    (if overflow? (no-overflow-branches!))
    (LAP (SUBI () ,constant ,src ,tgt))))

(define-arithconst-method 'FIXNUM-AND
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?				; ignored because can never happen
    (and (signed-fixnum? constant)
	 (integer-log-base-2? (+ constant 1))))
  (lambda (tgt src constant overflow?)
    (if overflow? (no-overflow-branches!))
    (let ((bits (integer-log-base-2? (+ constant 1))))
      (LAP (EXTRU () ,src 31 ,bits ,tgt)))))

(define-arithconst-method 'FIXNUM-LSH
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    (if ovflw? (error "RULFIX: FIXNUM-LSH with overflow check requested"))
    (signed-fixnum? constant))
  (lambda (tgt src shift overflow?)
    (if overflow? (error "RULFIX: FIXNUM-LSH with overflow check requested"))
    (cond ((zero? shift)
	   (copy src tgt))
	  ((negative? shift)
	   ;; Right shift
	   (let ((shift (- shift)))
	     (if (>= shift scheme-datum-width)
		 (copy 0 tgt)
		 (LAP (SHD () 0 ,src ,shift ,tgt)))))
	  (else
	   ;; Left shift
	   (if (>= shift scheme-datum-width)
	       (copy 0 tgt)
	       (LAP (SHD () ,src 0 ,(- 32 shift) ,tgt)))))))

(define (no-overflow-branches!)
  (error "RULFIX: overflow branches obsolete!")
  (set-current-branches!
   (lambda (if-overflow)
     if-overflow
     (LAP))
   (lambda (if-no-overflow)
     (LAP (B (N) (@PCR ,if-no-overflow))
	  (NOP ())))))

(define (untagged-fixnum-sign-extend source target)
  (let ((len (+ 1 scheme-datum-width)))
    (LAP (EXTRS () ,source 31 ,len ,target))))

(define (fix:fixnum?-overflow-branches! register)
  (error "RULFIX: overflow branches obsolete")
  (let ((temp (standard-temporary!)))
    (set-current-branches!
     (lambda (if-overflow)
       (LAP ,@(untagged-fixnum-sign-extend register temp)
	    (COMBN (<>) ,register ,temp (@PCR ,if-overflow))))
     (lambda (if-no-overflow)
       (LAP ,@(untagged-fixnum-sign-extend register temp)
	    (COMBN (=) ,register ,temp (@PCR ,if-no-overflow)))))))

(define (expand-factor tgt src factor skipping? condition skip)
  (define (sh3add condition src1 src2 tgt)
    (LAP (SH3ADD ,condition ,src1 ,src2 ,tgt)))

  (define (sh2add condition src1 src2 tgt)
    (LAP (SH2ADD ,condition ,src1 ,src2 ,tgt)))

  (define (sh1add condition src1 src2 tgt)
    (LAP (SH1ADD ,condition ,src1 ,src2 ,tgt)))

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
		   ((or (null? condition)
			(memq 'NV condition))
		    (LAP (COPY () ,result-reg ,tgt)))
		   (else
		    (LAP (COPY (TR) ,result-reg ,tgt)
			 ,@(skip))))))))
					; end of EXPAND-FACTOR

(define-arithconst-method 'MULTIPLY-FIXNUM
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    (and (signed-fixnum? constant)
	 (let ((factor (abs constant)))
	   (or (not ovflw?)
	       (< factor 64)		; Can't overflow out of 32-bit word
	       (and
		(< (abs factor) (expt 2 (-1+ scheme-datum-width)))
		(integer-log-base-2? factor))))))

  (lambda (tgt src constant overflow?)
    (let* ((factor (abs constant))
	   (xpt (integer-log-base-2? factor)))
      (case constant
	((0) (if overflow? (no-overflow-branches!))
	     (LAP (COPY () 0 ,tgt)))
	((1) (if overflow? (no-overflow-branches!))
	     (copy src tgt))
	((-1) (if overflow? (fix:fixnum?-overflow-branches! tgt))
	      (LAP (SUB () 0 ,src ,tgt)))
	((and overflow? xpt (> xpt 6))
	 (let ((true-src (if (negative? constant) tgt src))
	       (temp     (standard-temporary!)))
	   (set-current-branches!
	    (lambda (if-oflow)
	      (LAP (COMBN (<>) ,true-src ,temp ,if-oflow)
		   (SHD ,true-src 0 ,(- 32 xpt) ,tgt)))
	    (lambda (if-no-oflow)
	      (LAP (COMB (=) ,true-src ,temp ,if-no-oflow)
		   (SHD ,true-src 0 ,(- 32 xpt) ,tgt))))
	   (LAP ,@(if (negative? constant)
		      (LAP (SUB () 0 ,src ,true-src))
		      (LAP))
		(EXTRS () ,true-src 31
		       ,(- 31 (+ xpt scheme-type-width))
		       ,temp))))
	(else
	 ;; No overflow, or small constant
	 (if overflow? (fix:fixnum?-overflow-branches! tgt))
	 (let ((src+ (if (negative? constant) tgt src)))
	   (LAP ,@(if (negative? constant)
		      (LAP (SUB () 0 ,src ,tgt))
		      (LAP))
		,@(if xpt
		      (LAP (SHD () ,src+ 0 ,(- 32 xpt) ,tgt))
		      (expand-factor tgt src+ factor false '()
				     (lambda () (LAP)))))))))))

;;;; Division

(define-arithconst-method 'FIXNUM-QUOTIENT
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?				; ignored
    (and (signed-fixnum? constant)
	 (integer-log-base-2? (abs constant))))
  (lambda (tgt src constant ovflw?)
    (case constant
      ((1) (if ovflw? (no-overflow-branches!))
	   (copy src tgt))
      ((-1)
       (if ovflw? (fix:fixnum?-overflow-branches!))
       (LAP (SUB () 0 ,src ,tgt)))
      (else
       (let* ((factor (abs constant))
	      (xpt (integer-log-base-2? factor)))
	 (cond ((not xpt)
		(error "fixnum-quotient: Inconsistency" constant))
	       ((>= xpt scheme-datum-width)
		(if ovflw? (no-overflow-branches!))
		(copy 0 tgt))
	       (else
		;; Note: The following cannot overflow because we are
		;; dividing by a constant whose absolute value is
		;; strictly greater than 1.
		(if ovflw? (no-overflow-branches!))
		(let* ((posn (- 32 xpt))
		       (delta (- factor 1))
		       (fits? (fits-in-11-bits-signed? delta))
		       (temp (and (not fits?) (standard-temporary!))))
		  (LAP ,@(if fits?
			     (LAP)
			     (load-immediate delta temp))
		       (ADD (>=) 0 ,src ,tgt) ; Copy to tgt & test
					; negative dividend
		       ,@(if fits?	; For negative dividend ONLY
			     (LAP (ADDI () ,delta ,tgt ,tgt))
			     (LAP (ADD () ,temp ,tgt ,tgt)))
		       (EXTRS () ,tgt ,(-1+ posn) ,posn ,tgt)
		       ,@(if (negative? constant)
			     (LAP (SUB () 0 ,tgt ,tgt))
			     (LAP)))))))))))

(define-arithconst-method 'FIXNUM-REMAINDER
  fixnum-methods/2-args/register*constant
  (lambda (constant ovflw?)
    ovflw?				; ignored
    (and (signed-fixnum? constant)
	 (integer-log-base-2? (abs constant))))
  (lambda (tgt src constant ovflw?)
    (if ovflw? (no-overflow-branches!))
    (case constant
      ((1 -1)
       (LAP (COPY () 0 ,tgt)))
      (else
       (let ((sign (standard-temporary!))
	     (len  (integer-log-base-2? (abs constant))))
	 (let ((sgn-len (- 32 len)))
	   (LAP (EXTRS () ,src 0 1 ,sign)
		(EXTRU (=) ,src 31 ,len ,tgt)
		(DEP () ,sign ,(- sgn-len 1) ,sgn-len ,tgt))))))))

;;;; Predicates

(define-rule predicate
  (OVERFLOW-TEST)
  ;; Overflow test handling for untagged-fixnums is embedded in the
  ;; code for the operator.
  (error "RULFIX: Overflow test obsolete"))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (QUALIFIER (memq predicate '(ZERO-FIXNUM? EQUAL-FIXNUM?
			       NEGATIVE-FIXNUM? LESS-THAN-FIXNUM?
			       POSITIVE-FIXNUM? GREATER-THAN-FIXNUM?)))
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
		      (CONSTANT (? constant))) 
  (QUALIFIER (signed-fixnum? constant))
  (compare-fixnum/constant*register (invert-condition-noncommutative
				     (fixnum-pred->cc predicate))
				    constant
				    (standard-source! source)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (CONSTANT (? constant))
		      (REGISTER (? source)))
  (QUALIFIER (signed-fixnum? constant))
  (compare-fixnum/constant*register (fixnum-pred->cc predicate)
				    constant
				    (standard-source! source)))

(define-integrable (compare-fixnum/constant*register cc n r)
  (compare-immediate cc n r))

(define (fixnum-pred->cc predicate)
  (case predicate
    ((ZERO-FIXNUM? EQUAL-FIXNUM?) '=)
    ((NEGATIVE-FIXNUM? LESS-THAN-FIXNUM?) '<)
    ((POSITIVE-FIXNUM? GREATER-THAN-FIXNUM?) '>)
    (else
     (error "fixnum-pred->cc: unknown predicate" predicate))))

;; This recognises the pattern for flo:vector-length:

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT 0)
			(FIXNUM-2-ARGS FIXNUM-LSH
				       (OBJECT->DATUM (REGISTER (? source)))
				       (CONSTANT (? constant))
				       #F)))
  (QUALIFIER (and (exact-integer? constant)
		  (<= (- 1 scheme-datum-width) constant -1)))
  (let* ((source  (standard-source! source))
	 (target  (standard-target! target)))
    (LAP (EXTRU () ,source ,(+ 31 constant) ,(+ scheme-datum-width constant)
		,target))))

;; Intermediate patterns of above:

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS FIXNUM-LSH
			 (OBJECT->DATUM (REGISTER (? source)))
			 (CONSTANT (? constant))
			 #F))
  (QUALIFIER (and (exact-integer? constant)
		  (<= (- 1 scheme-datum-width) constant -1)))
  (let* ((source  (standard-source! source))
	 (target  (standard-target! target)))
    (LAP (EXTRU () ,source ,(+ 31 constant) ,(+ scheme-datum-width constant)
		,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT 0)
			    (FIXNUM-2-ARGS FIXNUM-LSH
					   (REGISTER (? source))
					   (CONSTANT (? constant))
					   #F)))
  (QUALIFIER (and (exact-integer? constant)
		  (<= (- 1 scheme-datum-width) constant -1)))
  (let* ((source  (standard-source! source))
	 (target  (standard-target! target)))
    (LAP ;; Without OBJECT->DATUM the high order bits could be anything and
         ;; some could creep into the result.
         (EXTRU () ,source ,(+ 31 constant) ,(+ 32 constant) ,target)
	 (DEPI () 0 ,(- scheme-type-width 1) ,scheme-type-width ,target))))
  