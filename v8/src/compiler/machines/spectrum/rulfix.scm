#| -*-Scheme-*-

$Id: rulfix.scm,v 1.1 1994/11/19 02:08:04 adams Exp $

Copyright (c) 1989-1994 Massachusetts Institute of Technology

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

;;; NOTE: The **only** part of the compiler that currently (12/28/93)
;;; generates (OBJECT->FIXNUM ...) is opncod.scm and it guarantees
;;; that these are either preceded by a type check for fixnum or the
;;; user has open-coded a fixnum operation indicating that type
;;; checking isn't necessary.  So we don't bother to clear type bits
;;; if untagged-fixnums? is #T.

;;; NOTE(2):  rulrew.scm removes all the occurences of
;;;  OBJECT->FIXNUM, FIXNUM->OBJECT and OBJECT->UNSIGNED-FIXNUM
;;;  as these are no-ops when using untagged fixnums

;;; NOMENCLATURE:
;;; OBJECT means an object represented in standard Scheme form
;;; ADDRESS means a hardware pointer to an address; on the PA this
;;;         means it has the quad bits set correctly
;;; FIXNUM means a value without type code, in a form suitable for
;;;        machine arithmetic.  If UNTAGGED-FIXNUMS? is #T (i.e.
;;;        POSITIVE-FIXNUM is type code 0, NEGATIVE-FIXNUM is type
;;;        code -1), then we simply use the standard hardware
;;;        representation of integers.  Otherwise, we shift the
;;;        integer so that the Scheme fixnum sign bit is stored in the
;;;        hardware sign bit: i.e. left shifted by typecode-width (6)
;;;        bits.

;(define (copy-instead-of-object->fixnum source target)
;  (standard-move-to-target! source target)
;  (LAP))

;(define (copy-instead-of-fixnum->object source target)
;  (standard-move-to-target! source target)
;   (LAP))

;(define-rule statement
;  ;; convert a fixnum object to a "fixnum integer"
;  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
;  (if untagged-fixnums?
;      (copy-instead-of-object->fixnum source target)
;      (standard-unary-conversion source target object->fixnum)))

;(define-rule statement
;  ;; load a fixnum constant as a "fixnum integer"
;  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
;  (load-fixnum-constant constant (standard-target! target)))

(define-rule statement
  ;; convert a memory address to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target address->fixnum))

(define-rule statement
  ;; convert an object's address to a "fixnum integer"
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (if untagged-fixnums?
      (standard-unary-conversion source target object->datum)
      ;;(standard-unary-conversion source target object->fixnum)
      ))

;(define-rule statement
;  ;; convert a "fixnum integer" to a fixnum object
;  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
;  (standard-move-to-target! source target)
;  (LAP (COMMENT (elided (object->fixnum (register ,source))))))
; ;;  (standard-unary-conversion source target fixnum->object)

(define-rule statement
  ;; convert a "fixnum integer" to a memory address
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source target fixnum->address))

(let ((make-scaled-object->fixnum
       (lambda (factor)
	 (let ((shift (integer-log-base-2? factor)))
	   (cond ((not shift)
		  (error "make-scaled-object->fixnum: Not a power of 2"
			 factor))
		 ((> shift scheme-datum-width)
		  (error "make-scaled-object->fixnum: shift too large" shift))
		 (else
		  (lambda (src tgt)
		    (if untagged-fixnums?
			(LAP (SHD () ,src 0 ,(- 32 shift) ,tgt))
			(LAP (SHD () ,src 0 ,(- scheme-datum-width shift)
				  ,tgt))))))))))

  (define-rule statement
    (ASSIGN (REGISTER (? target))
	    (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			   (CONSTANT (? value))
			   (REGISTER (? source))
			   #F))
    (QUALIFIER (integer-log-base-2? value))
    (standard-unary-conversion source target
			       (make-scaled-object->fixnum value)))

  (define-rule statement
    (ASSIGN (REGISTER (? target))
	    (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			   (REGISTER (? source))
			   (CONSTANT (? value))
			   #F))
    (QUALIFIER (integer-log-base-2? value))
    (standard-unary-conversion source target
			       (make-scaled-object->fixnum value))))

(define-integrable (fixnum->index-fixnum src tgt)
  ;; Takes a register containing a FIXNUM representing an index in
  ;; units of Scheme object units and generates the
  ;; corresponding FIXNUM for the byte offset: it multiplies by 4.
  ;;! (if untagged-fixnums? 'nothing-different)
  (LAP (SHD () ,src 0 30 ,tgt)))

;(define-integrable (object->fixnum src tgt)
;  ;; With untagged-fixnums this is called *only* when we are not
;  ;; treating the src as containing a signed fixnum -- i.e. when we
;  ;; have a pointer and want to do integer arithmetic on it.  In this
;  ;; case it is OK to generate positive numbers in all cases.  Notice
;  ;; that we *also* choose, in this case, to have "fixnums" be
;  ;; unshifted, while with tagged-fixnums we shift to put the Scheme
;  ;; sign bit in the hardware sign bit, and unshift later.
;  (if untagged-fixnums?
;      (begin
;	(warn "object->fixum: " src tgt)
;       ;; This is wrong!
;	;;(deposit-type 0 (standard-move-to-target! src tgt))
;	(LAP ,@(copy src tgt)
;	     ,@(deposit-type 0 tgt)))
;      (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt))))

(define-integrable (address->fixnum src tgt)
  ;; This happens to be the same as object->fixnum
  ;; With untagged-fixnums we need to clear the quad bits, With single tag
  ;; fixnums shift the sign into the machine sign, shifting out the
  ;; quad bits.
  (if untagged-fixnums?
      (deposit-type 0 (standard-move-to-target! src tgt))
      (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt))))

;(define-integrable (fixnum->object src tgt)
;  (if untagged-fixnums?
;      ;;B?(copy-instead-of-fixnum->object src tgt)
;      (untagged-fixnum-sign-extend src tgt)
;      (LAP ,@(load-immediate (ucode-type positive-fixnum) regnum:addil-result)
;	   (SHD () ,regnum:addil-result ,src ,scheme-type-width ,tgt))))

(define (fixnum->address src tgt)
  (if untagged-fixnums?
      (LAP (DEP () ,regnum:quad-bitmask ,(-1+ scheme-type-width)
		,scheme-type-width ,tgt))
      (LAP (SHD () ,regnum:quad-bitmask ,src ,scheme-type-width ,tgt))))

(define (fixnum->datum src tgt)
  (if untagged-fixnums?
      (deposit-type 0 (standard-move-to-target! src tgt))
      (LAP (SHD () 0 ,src ,scheme-type-width ,tgt))))

(define (load-fixnum-constant constant target)
  (load-immediate (* constant fixnum-1) target))

(define #|-integrable|# fixnum-1
  ;; (expt 2 scheme-type-width) ***
  (if untagged-fixnums? 1 64))

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

;(define-rule statement
;  ;; execute a binary fixnum operation
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM->OBJECT
;	   (FIXNUM-2-ARGS (? operation)
;			  (OBJECT->FIXNUM (REGISTER (? source1)))
;			  (OBJECT->FIXNUM (REGISTER (? source2)))
;			  (? overflow?))))
;  (QUALIFIER (fixnum-2-args/operator? operation))
;  (standard-binary-conversion source1 source2 target
;			      (lambda (source1 source2 target)
;				((fixnum-2-args/operator operation)
;                                target source1 source2 overflow?))))

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
	     (if untagged-fixnums?
		 (begin
		   (if overflow?  (no-overflow-branches!))
		   (LAP (,instr () ,fixed-operand ,',src ,',tgt)))
		 (if overflow?
		     (LAP (,instr (,nsv) ,fixed-operand ,',src ,',tgt))
		     (LAP (,instr () ,fixed-operand ,',src ,',tgt))))))))

     (binary-fixnum
      (macro (name instr nsv)
	`(define-arithmetic-method ',name fixnum-methods/2-args
	   (lambda (tgt src1 src2 overflow?)
	     (if untagged-fixnums?
		 (begin
		   (if overflow?  (no-overflow-branches!))
		   (LAP (,instr () ,',src1 ,',src2 ,',tgt)))
		 (if overflow?
		     (LAP (,instr (,nsv) ,',src1 ,',src2 ,',tgt))
		     (LAP (,instr () ,',src1 ,',src2 ,',tgt))))))))

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
  (unary-fixnum FIXNUM-NOT SUBI TR ,(- fixnum-1));;?? XOR?

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
#|
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
    (if (and untagged-fixnums? ovflw?)
	(overflow-branch-if-not-nullified!))
    (LAP ,@extra
	 ,@load-1
	 ,@load-2
	 ,@(invoke-hook (car hook))
	 ,@(if (not ovflw?)
	       (LAP)
	       (LAP (COMICLR (=) 0 ,regnum:second-arg 0))))))
|#

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
	     (if (and untagged-fixnums? ovflw?)
		 (overflow-branch-if-not-nullified!))
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

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

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

(if untagged-fixnums?

    (define-arithconst-method 'PLUS-FIXNUM
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?
	;; ignored because success of generic arithmetic pretest
	;; guarantees it won't overflow
	(fits-in-14-bits-signed? (* constant fixnum-1)))
      (lambda (tgt src constant overflow?)
	(guarantee-signed-fixnum constant)
	(if overflow? (no-overflow-branches!))
	(let ((value (* constant fixnum-1)))
	  (load-offset value src tgt))))

    (define-arithconst-method 'PLUS-FIXNUM
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?				; ignored
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
    )

(if untagged-fixnums?

    (define-arithconst-method 'MINUS-FIXNUM
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?
	;; ignored because success of generic arithmetic pretest
	;; guarantees it won't overflow
	(fits-in-14-bits-signed? (- (* constant fixnum-1))))
      (lambda (tgt src constant overflow?)
	(guarantee-signed-fixnum constant)
	(if overflow? (no-overflow-branches!))
	(let ((value (- (* constant fixnum-1))))
	  (load-offset value src tgt))))

    (define-arithconst-method 'MINUS-FIXNUM
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?				; ignored
	(fits-in-11-bits-signed? (- (* constant fixnum-1))))
      (lambda (tgt src constant overflow?)
	(guarantee-signed-fixnum constant)
	(let ((value (- (* constant fixnum-1))))
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
    )

(if untagged-fixnums?
    (define-arithconst-method 'MINUS-FIXNUM
      fixnum-methods/2-args/constant*register
      (lambda (constant ovflw?)
	ovflw?				; ignored
	(fits-in-11-bits-signed? (* constant fixnum-1)))
      (lambda (tgt constant src overflow?)
	(guarantee-signed-fixnum constant)
	(if overflow? (no-overflow-branches!))
	(let ((value (* constant fixnum-1)))
	  (if (fits-in-11-bits-signed? value)
	      (LAP (SUBI () ,value ,src ,tgt))
	      (error "MINUS-FIXNUM <c>*<r> with bad constant" value)))))

    (define-arithconst-method 'MINUS-FIXNUM
      fixnum-methods/2-args/constant*register
      (lambda (constant ovflw?)
	ovflw?				; ignored
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
    )


(if untagged-fixnums?
    (define-arithconst-method 'FIXNUM-AND
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?
	;; ignored because can never happen
	(integer-log-base-2? (+ constant 1)))
      (lambda (tgt src constant overflow?)
	(guarantee-signed-fixnum constant)
	(if overflow? (no-overflow-branches!))
	(let ((bits (integer-log-base-2? (+ constant 1))))
	  (LAP (EXTRU () ,src 31 ,bits ,tgt))))))

(if untagged-fixnums?
    (define-arithconst-method 'FIXNUM-LSH
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	(if ovflw? (error "RULFIX: FIXNUM-LSH with overflow check requested"))
	constant			; ignored
	true)
      ;; OVERFLOW? should never be set, because there is no generic
      ;; LSH operation and only generics cause overflow detection
      (lambda (tgt src shift overflow?)
	(if overflow?
	    (error "RULFIX: FIXNUM-LSH with overflow check requested"))
	(guarantee-signed-fixnum shift)
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

    (define-arithconst-method 'FIXNUM-LSH
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	constant ovflw?			; ignored
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
    )

(define (no-overflow-branches!)
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
  (let ((temp (standard-temporary!)))
    (set-current-branches!
     (lambda (if-overflow)
       (LAP ,@(untagged-fixnum-sign-extend register temp)
	    (COMBN (<>) ,register ,temp (@PCR ,if-overflow))))
     (lambda (if-no-overflow)
       (LAP ,@(untagged-fixnum-sign-extend register temp)
	    (COMBN (=) ,register ,temp (@PCR ,if-no-overflow)))))))

(define (overflow-branch-if-not-nullified!)
  (set-current-branches!
   (lambda (if-overflow)
     (LAP (B (N) (@PCR ,if-overflow))))
   (lambda (if-no-overflow)
     (LAP (SKIP (TR))
	  (B (N) (@PCR ,if-no-overflow))))))

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

(if untagged-fixnums?
    (define-arithconst-method 'MULTIPLY-FIXNUM
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	(let ((factor (abs constant)))
	  (or (not ovflw?)
	      (< factor 64)		; Can't overflow out of 32-bit word
	      (and
	       (< (abs factor) (expt 2 (-1+ scheme-datum-width)))
	       (integer-log-base-2? factor)))))

      (lambda (tgt src constant overflow?)
	(guarantee-signed-fixnum constant)
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
				 (expand-factor tgt src+ factor false '()
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
			   ,@(expand-factor tgt src+ factor
					    (negative? constant)
					    '(NSV)
					    (lambda ()
					      (LAP (SKIP (TR))))))))))))))
    )

;;;; Division

(if untagged-fixnums?
    (define-arithconst-method 'FIXNUM-QUOTIENT
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?				; ignored
	(integer-log-base-2? (abs constant)))
      (lambda (tgt src constant ovflw?)
	(guarantee-signed-fixnum constant)
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
			   (delta (* (-1+ factor) fixnum-1))
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

		      (LAP ,@(if fits?
				 (LAP)
				 (load-immediate delta temp))
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
    )

(if untagged-fixnums?
    (define-arithconst-method 'FIXNUM-REMAINDER
      fixnum-methods/2-args/register*constant
      (lambda (constant ovflw?)
	ovflw?				; ignored
	(integer-log-base-2? (abs constant)))
      (lambda (tgt src constant ovflw?)
	(guarantee-signed-fixnum constant)
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
    )

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
  ;; Overflow test handling for untagged-fixnums is embedded in the
  ;; code for the operator.
  (if (not untagged-fixnums?)
      (overflow-branch-if-not-nullified!))
  (LAP))

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

;(define-rule predicate
;  (FIXNUM-PRED-2-ARGS (? predicate)
;		      (OBJECT->FIXNUM (REGISTER (? source1)))
;		      (OBJECT->FIXNUM (REGISTER (? source2))))
;  (compare (fixnum-pred->cc predicate)
;	   (standard-source! source1)
;	   (standard-source! source2)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (CONSTANT (? constant)))
  (compare-fixnum/constant*register (invert-condition-noncommutative
				     (fixnum-pred->cc predicate))
				    constant
				    (standard-source! source)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (CONSTANT (? constant))
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

;;;; New "optimizations"

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (OBJECT->DATUM (FIXNUM->OBJECT (REGISTER (? source)))))
;  (standard-unary-conversion source target fixnum->datum))

(define (constant->additive-operand operation constant)
  (case operation
    ((PLUS-FIXNUM ONE-PLUS-FIXNUM) constant)
    ((MINUS-FIXNUM MINUS-ONE-PLUS-FIXNUM) (- constant))
    (else
     (error "constant->additive-operand: Unknown operation"
	    operation))))

(define (guarantee-fixnum-result target)
  (if untagged-fixnums?
      (if compiler:assume-safe-fixnums?
	  (LAP)
	  (untagged-fixnum-sign-extend target target))
      (let ((default
	      (lambda ()
		(deposit-immediate (ucode-type positive-fixnum)
				   (-1+ scheme-type-width)
				   scheme-type-width
				   target))))
	#|
	;; Unsafe at sign crossings until the tags are changed.
	(if compiler:assume-safe-fixnums?
	    (LAP)
	    (default))
	|#
	(default))))

;(define (obj->fix-of-reg*obj->fix-of-const operation target source constant)
;  (let* ((source (standard-source! source))
;	 (temp   (standard-temporary!))
;	 (target (standard-target! target)))
;    (pp (list 'obj->fix-of-reg*obj->fix-of-const operation target source constant))
;    (LAP ,@(load-offset (constant->additive-operand operation constant)
;			source temp)
;	 ,@(if untagged-fixnums?
;	       ;;B? (copy-instead-of-object->fixnum temp target)
;	       (object->fixnum temp target)
;	       (object->fixnum temp target)))))
;
;(define (obj->fix-of-reg*obj->fix-of-const operation target source constant)
;  (let* ((source (standard-source! source))
;	 (target (standard-target! target)))
;    (LAP ,@(load-offset (constant->additive-operand operation constant)
;			source target)
;	 ,@(guarantee-fixnum-result target))))


;(define (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target
;						       source constant)
;  (let* ((source (standard-source! source))
;	 (target (standard-target! target)))
;    (LAP ,@(load-offset (constant->additive-operand operation constant)
;			source target)
;	 ,@(guarantee-fixnum-result target))))
;
;(define (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
;	 operation target source constant)
;  (let* ((source (standard-source! source))
;	 (temp   (standard-temporary!))
;	 (target (standard-target! target)))
;    (LAP ,@(load-offset (constant->additive-operand operation constant)
;			source temp)
;	 ,@(object->datum temp target))))
;
;(define (fix->obj-of-reg*obj->fix-of-const operation target source constant)
;  (let* ((source (standard-source! source))
;	 (temp (standard-temporary!))
;	 (target (standard-target! target)))
;    (LAP ,@(load-offset
;	    (constant->additive-operand operation (* constant fixnum-1))
;	    source temp)
;	 ,@(fixnum->object temp target))))
;
;(define (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
;	 operation target source constant)
;  (let* ((source (standard-source! source))
;	 (temp (standard-temporary!))
;	 (target (standard-target! target)))
;    (LAP ,@(load-offset
;	    (constant->additive-operand operation (* constant fixnum-1))
;	    source temp)
;	 ,@(fixnum->datum temp target))))

;(define (incr-or-decr? operation)
;   (and (memq operation '(ONE-PLUS-FIXNUM MINUS-ONE-PLUS-FIXNUM))
;	operation))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-1-ARG (? operation incr-or-decr?)
;			(OBJECT->FIXNUM (REGISTER (? source)))
;			#F))
;  (obj->fix-of-reg*obj->fix-of-const operation target source 1))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-1-ARG (? operation incr-or-decr?)
;			(OBJECT->FIXNUM (REGISTER (? source)))
;			#F))
;  (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target source 1))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (OBJECT->DATUM
;	   (FIXNUM->OBJECT
;	    (FIXNUM-1-ARG (? operation incr-or-decr?)
;			  (OBJECT->FIXNUM (REGISTER (? source)))
;			  #F))))
;  (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
;   operation target source 1))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-1-ARG (? operation incr-or-decr?)
;			(REGISTER (? source))
;			#F))
;  (fix->obj-of-reg*obj->fix-of-const operation target source 1))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (OBJECT->DATUM
;	   (FIXNUM->OBJECT
;	    (FIXNUM-1-ARG (? operation incr-or-decr?)
;			  (REGISTER (? source))
;			  #F))))
;  (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
;   operation target source 1))

(define (plus-or-minus? operation)
  (and (memq operation '(PLUS-FIXNUM MINUS-FIXNUM))
       operation))

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			 (OBJECT->FIXNUM (REGISTER (? source)))
;			 (OBJECT->FIXNUM (CONSTANT (? constant)))
;			 #F))
;  (obj->fix-of-reg*obj->fix-of-const operation target source constant))

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM->OBJECT
;	   (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			  (OBJECT->FIXNUM (REGISTER (? source)))
;			  (OBJECT->FIXNUM (CONSTANT (? constant)))
;			  #F)))
;  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
;  (fix->obj-of-obj->fix-of-reg*obj->fix-of-const operation target
;						 source constant))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (OBJECT->DATUM
;	   (FIXNUM->OBJECT
;	    (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			   (OBJECT->FIXNUM (REGISTER (? source)))
;			   (OBJECT->FIXNUM (CONSTANT (? constant)))
;			   #F))))
;  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
;  (obj->dat-of-fix->obj-of-obj->fix-of-reg*obj->fix-of-const
;   operation target source constant))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM->OBJECT
;	   (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			  (REGISTER (? source))
;			  (OBJECT->FIXNUM (CONSTANT (? constant)))
;			  #F)))
;  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
;  (fix->obj-of-reg*obj->fix-of-const operation target source constant))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (OBJECT->DATUM
;	   (FIXNUM->OBJECT
;	    (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			   (REGISTER (? source))
;			   (OBJECT->FIXNUM (CONSTANT (? constant)))
;			   #F))))
;  (QUALIFIER (memq operation '(PLUS-FIXNUM MINUS-FIXNUM)))
;  (obj->dat-of-fix->obj-of-reg*obj->fix-of-const
;   operation target source constant))

;(define (additive-operate operation target source-1 source-2)
;  (case operation
;    ((PLUS-FIXNUM)
;     (LAP (ADD () ,source-1 ,source-2 ,target)))
;    ((MINUS-FIXNUM)
;     (LAP (SUB () ,source-1 ,source-2 ,target)))
;    (else
;     (error "constant->additive-operand: Unknown operation"
;	    operation))))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			 (REGISTER (? source-1))
;			 (REGISTER (? source-2))
;			 #F))
;  (let* ((source-1 (standard-source! source-1))
;	 (source-2 (standard-source! source-2))
;	 (target (standard-target! target)))
;    (LAP ,@(additive-operate operation target source-1 source-2))))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			 (REGISTER (? source-1))
;			 (REGISTER (? source-2))
;			 #F))
;  (let* ((source-1 (standard-source! source-1))
;	 (source-2 (standard-source! source-2))
;	 (target (standard-target! target)))
;    (LAP ,@(additive-operate operation target source-1 source-2))))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			 (REGISTER (? source-1))
;			 (REGISTER (? source-2))
;			 #F))
;  (let* ((source-1 (standard-source! source-1))
;	 (source-2 (standard-source! source-2))
;	 (target (standard-target! target)))
;    (LAP ,@(additive-operate operation target source-1 source-2))))
;
;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FIXNUM-2-ARGS (? operation plus-or-minus?)
;			 (REGISTER (? source-1))
;			 (REGISTER (? source-2))
;			 #F))
;  (let* ((source-1 (standard-source! source-1))
;	 (source-2 (standard-source! source-2))
;	 (target (standard-target! target)))
;    (LAP ,@(additive-operate operation target source-1 source-2)
;	 ,@(guarantee-fixnum-result target))))


;; This recognises the pattern for flo:vector-length:

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT 0)
			(FIXNUM-2-ARGS FIXNUM-LSH
				       (OBJECT->DATUM (REGISTER (? source)))
				       (CONSTANT (? constant))
				       #F)))
  (QUALIFIER (and (integer? constant)
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
  (QUALIFIER (and (integer? constant)
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
  (QUALIFIER (and (integer? constant)
		  (<= (- 1 scheme-datum-width) constant -1)))
  (let* ((source  (standard-source! source))
	 (target  (standard-target! target)))
    (LAP ;; Without OBJECT->DATUM the high order bits could be anything and
         ;; some could creep into the result.
         (EXTRU () ,source ,(+ 31 constant) ,(+ 32 constant) ,target)
	 (DEPI () 0 ,(- scheme-type-width 1) ,scheme-type-width ,target))))
  
