#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rulfix.scm,v 1.5 1992/01/27 04:24:27 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/rules1.scm,v 4.36 1991/10/25 06:49:58 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

MOVB	vs.	MOVW
ADDB	vs.	ADDW
they make, so that these may be included in future releases; and (b)
The assembler assumes that it is always running in 32-bit mode.
It matters for immediate operands, displacements in addressing modes, and displacements in pc-relative jump  instructions.

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
  (convert-object/constant->register target constant address->fixnum))

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
  overflow?				; ignored
  (case operator
    ((FIXNUM-LSH)
     (require-register! ecx))		; CL used as shift count
    ((FIXNUM-QUOTIENT FIXNUM-REMAINDER)
     (require-register! eax)		; dividend low/quotient
     (require-register! edx)))		; dividend high/remainder
  (fixnum-2-args target source1 source2 (fixnum-2-args/operate operator)))

(define (require-register! machine-reg)
  (flush-register! machine-reg)
  (need-register! machine-reg))

(define-integrable (flush-register! machine-reg)
  (prefix-instructions! (clear-registers! machine-reg)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER (or (and (not (eq? operator 'FIXNUM-QUOTIENT))
		      (not (eq? operator 'FIXNUM-REMAINDER)))
		 (integer-power-of-2? (abs constant))))
  overflow?				; ignored
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (fixnum-2-args/commutative? operator))
  overflow?				; ignored
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (? overflow?)))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OBJECT->FIXNUM (REGISTER (? register))))
  (fixnum-branch! predicate)
  (let ((temp (standard-move-to-temporary! register)))
    (object->fixnum temp)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(predicate/memory-operand-reference memory) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (REGISTER (? register-2)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register-1)
	     ,(source-register-reference register-2))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (REGISTER (? register)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register)
	    ,(predicate/memory-operand-reference memory))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (? memory) (REGISTER (? register)))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(predicate/memory-operand-reference memory)
	    ,(source-register-reference register))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register)
	    (& ,(fixnum-object->fixnum-word constant)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? register)))
  (fixnum-branch/commuted! predicate)
  (LAP (CMP W ,(source-register-reference register)
	    (& ,(fixnum-object->fixnum-word constant)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (? memory)
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(predicate/memory-operand-reference memory)
	    (& ,(fixnum-object->fixnum-word constant)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-branch/commuted! predicate)
  (LAP (CMP W ,(predicate/memory-operand-reference memory)
	    (& ,(fixnum-object->fixnum-word constant)))))

;; This assumes that the last instruction sets the condition code bits
;; correctly.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches! (lambda (label) (LAP (JO (@PCR ,label))))
			 (lambda (label) (LAP (JNO (@PCR ,label)))))
  (LAP))

;;;; Utilities

(define (object->fixnum target)
  (SAL W ,target (& ,scheme-type-width)))

(define (fixnum->object target)
  (LAP (OR W ,target (& ,(ucode-type fixnum)))
       (ROR W ,target (& ,scheme-type-width))))

(define (address->fixnum target)
  (SAL W ,target (& ,scheme-type-width)))

(define (fixnum->address target)
  (SHR W ,target (& ,scheme-type-width)))

(define-integrable fixnum-1 64)		; (expt 2 scheme-type-width) ***

(define-integrable fixnum-bits-mask
  (-1+ fixnum-1))

(define (load-fixnum-constant constant target)
  (if (zero? constant)
      (LAP (XOR W ,target ,target))
      (LAP (MOV W ,target (& ,(* constant fixnum-1))))))

(define (convert-index->fixnum/register target source)
  (fixnum-1-arg target source
   (lambda (target)
     (LAP (SAL W ,target (& ,(+ scheme-type-width 2)))))))

;;;; Fixnum operation dispatch

(define (define-fixnum-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-fixnum-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-integrable (fixnum-1-arg/operate operator)
  (lookup-fixnum-method operator fixnum-methods/1-arg))

(define-integrable (fixnum-1-arg target source operation)
  (operation (standard-move-to-target! source)))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-integrable (fixnum-2-args/operate operator)
  (lookup-fixnum-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args-constant
  (list 'FIXNUM-METHODS/2-ARGS-CONSTANT))

(define-integrable (fixnum-2-args/operate-constant operator)
  (lookup-fixnum-method operator fixnum-methods/2-args-constant))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))
	     
(define (fixnum-2-args target source1 source2 operation)
  (two-arg-register-operation (fixnum-2-args/operate operator)
			      (fixnum-2-args/commutative? operator)
			      'GENERAL
			      any-reference
			      any-reference
			      target
			      source1
			      source2))

(define (two-arg-register-operation
	 operate commutative?
	 target-type source-reference alternate-source-reference
	 target source1 source2)
  (let ((worst-case
	 (lambda (target source1 source2)
	   (LAP ,@(if (eq? target-type 'FLOAT)
		      (load-float-register source1 target)
		      (LAP (MOV W ,target ,source1)))
		,@(operate target source2)))))
    (reuse-machine-target! target-type target
      (lambda (target)
	(reuse-pseudo-register-alias source1 target-type
	  (lambda (alias)
	    (let ((source2 (if (= source1 source2)
			       (register-reference alias)
			       (source-reference source2))))
	      (delete-register! alias)
	      (delete-dead-registers!)
	      (add-pseudo-register-alias! target alias)
	      (operate (register-reference alias) source2)))
	  (lambda ()
	    (let ((new-target-alias!
		   (lambda ()
		     (let ((source1 (alternate-source-reference source1))
			   (source2 (source-reference source2)))
		       (delete-dead-registers!)
		       (worst-case (reference-target-alias! target target-type)
				   source1
				   source2)))))
	      (if commutative?
		  (reuse-pseudo-register-alias source2 target-type
		    (lambda (alias2)
		      (let ((source1 (source-reference source1)))
			(delete-register! alias2)
			(delete-dead-registers!)
			(add-pseudo-register-alias! target alias2)
			(operate (register-reference alias2) source1)))
		    new-target-alias!)
		  (new-target-alias!))))))
      (lambda (target)
	(worst-case target
		    (alternate-source-reference source1)
		    (source-reference source2))))))

(define (fixnum-2-args/register*constant operator target source constant)
  (fixnum-1-arg
   target source
   (lambda (target)
     ((fixnum-2-args/operate-constant operator) target constant))))

;;;; Arithmetic operations

(define (integer-power-of-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define (word->fixnum target)
  (LAP (AND W ,target (& ,(fix:not fixnum-bits-mask)))))

(define (add-fixnum-constant target constant)
  (if (zero? constant)
      (LAP)
      (LAP (ADD W ,target (& ,(* constant fixnum-1))))))

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target 1)))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target -1)))

(define-fixnum-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (target)
    (LAP (NOT W ,target)
	 ,@(word->fixnum target))))

(let-syntax
    ((binary-operation
      (macro (name instr idempotent?)
	`(define-fixnum-method ',name fixnum-methods/2-args
	   (lambda (target source2)
	     (if (and ,idempotent? (equal? target source2))
		 (LAP)
		 (LAP (,instr W ,',target ,',source2))))))))

  (binary-operation PLUS-FIXNUM ADD false)
  (binary-operation MINUS-FIXNUM SUB false)
  (binary-operation FIXNUM-AND AND true)
  (binary-operation FIXNUM-OR OR true)
  (binary-operation FIXNUM-XOR XOR false))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args
  (lambda (target source2)
    (if (equal? target source2)
	(load-fixnum-constant 0 target)
	(let ((temp (temporary-register-reference)))
	  (LAP ,@(if (equal? temp source2)
		     (LAP)
		     (LAP (MOV W ,temp ,source2)))
	       (NOT W ,temp)
	       (AND W ,target ,temp))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source2)
    (cond ((not (equal? target source2))
	   (LAP (SAR W ,target (& ,scheme-type-width))
		(IMUL W ,target ,source2)))
	  ((even? scheme-type-width)
	   (LAP (SAR W ,target (& ,(quotient scheme-type-width 2)))
		(IMUL W ,target ,target)))
	  (else
	   (let ((temp (temporary-register-reference)))
	     (LAP (MOV W ,temp ,target)
		  (SAR W ,target (& ,scheme-type-width))
		  (IMUL W ,target ,temp)))))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args
  (lambda (target source2)
    ;; SOURCE2 is guaranteed not to be ECX because of the
    ;; require-register! used in the rule.
    ;; TARGET can be ECX only if the rule has machine register
    ;; ECX as the target, unlikely, but it must be handled!
    (let ((with-target
	    (lambda (target)
	      (let ((jlabel (generate-label 'SHIFT-JOIN))
		    (slabel (generate-label 'SHIFT-NEGATIVE)))
		(LAP (MOV W (R ,ecx) ,source2)
		     (SAR W (R ,ecx) (& ,scheme-type-width))
		     (JS (@PCR ,slabel))
		     (SHL W ,target (R ,ecx))
		     (JMP (@PCR ,jlabel))
		     (LABEL ,slabel)
		     (NEG W (R ,ecx))
		     (SHR W ,target (R ,ecx))
		     ,@(word->fixnum target)
		     (LABEL ,jlabel))))))

      (if (not (equal? target (INST-EA (R ,ecx))))
	  (with-target target)
	  (let ((temp (temporary-register-reference)))
	    (LAP (MOV W ,temp ,target)
		 ,@(with-target temp)
		 (MOV W ,target ,temp)))))))

;; ***** These should be rewritten.  Rather than use the standard 2 arg
;; register allocator, they should use their own to specify that the result
;; is in eax or edx after the rule.  This avoids unnecessary moves! ****

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source2)
    (if (equal? target source2)
	(load-fixnum-constant 1 target)
	(let ((do-divide
	       (lambda ()
		 (LAP (MOV W (R ,edx) (R ,eax))
		      (SAR W (R ,edx) (& 31))
		      (IDIV W (R ,eax) ,source2)
		      (SAL W (R ,eax) (& ,scheme-type-width))))))
	  (if (equal? target (INST-EA (R ,eax)))
	      (do-divide)
	      (LAP (MOV W (R ,eax) ,target)
		   ,@(do-divide)
		   (MOV W ,target (R ,eax))))))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (ea/same? source1 source2)
	(load-fixnum-constant 0 target)
	(LAP ,@(if (not (equal? target (INST-EA (R ,eax))))
		   (MOV W (R ,eax) ,target)
		   (LAP))
	     (MOV W (R ,edx) (R ,eax))
	     (SAR W (R ,edx) (& 31))
	     (IDIV W (R ,eax) ,source2)
	     (SAL W (R ,edx) (& ,scheme-type-width))
	     ,@(if (not (equal? target (INST-EA (R ,edx))))
		   (MOV W ,target (R ,edx))
		   (LAP))))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (add-fixnum-constant target n)))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (add-fixnum-constant target (- 0 n))))

(define-fixnum-method 'FIXNUM-OR fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant -1 target))
	  (else
	   (LAP (OR W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-XOR fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (LAP (NOT W ,target)
		,@(word->fixnum target)))
	  (else
	   (LAP (XOR W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-AND fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (load-fixnum-constant 0 target))
	  ((= n -1)
	   (LAP))
	  (else
	   (LAP (AND W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant 0 target))
	  (else
	   (LAP (AND W ,target (& ,(* (fix:not n) fixnum-1))))))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (LAP))
	  ((not (<= (- 0 scheme-datum-width) n scheme-datum-width))
	   (load-fixnum-constant 0 target))
	  ((not (negative? n))
	   (LAP (SHL W ,target (& ,n))))
	  (else
	   (LAP (SHR W ,target (& ,(- 0 n)))
		,@(word->fixnum target))))))

;; **** Overflow not set by SAL instruction!
;; also (LAP) leaves condition codes as before, while they should
;; clear the overflow flag! ****

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (load-fixnum-constant 0 target))
	  ((= n 1)
	   (LAP))
	  ((= n -1)
	   (LAP (NEG W ,target)))
	  ((integer-power-of-2? (if (negative? n) (- 0 n) n))
	   =>
	   (lambda (expt-of-2)
	     (if (negative? n)
		 (LAP (SAL W ,target (& ,expt-of-2))
		      (NEG W ,target))
		 (LAP (SAL W ,target (& ,expt-of-2))))))
	  (else
	   (LAP (IMUL W ,target (& ,n)))))))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((= n 1)
	   (LAP))
	  ((= n -1)
	   (NEG W ,target))
	  ((integer-power-of-2? (if (negative? n) (- 0 n) n))
	   =>
	   (lambda (expt-of-2)
	     (let ((label (generate-label 'QUO-SHIFT))
		   (absn (if (negative? n) (- 0 n) n)))
	       (LAP (CMP W ,target (& 0))
		    (JGE (@PCR ,label))
		    (ADD W ,target (& ,(* (-1+ absn) fixnum-1)))
		    (LABEL ,label)
		    (SAR W ,target (& ,expt-of-2))
		    ,@(word->fixnum ,target)
		    ,@(if (negative? n)
			  (LAP (NEG W ,target))
			  (LAP))))))
	  (else
	   (error "Fixnum-quotient/constant: Bad value" n)))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args-constant
  (lambda (target n)
    ;; (remainder x y) is 0 or has the sign of x.
    ;; Thus we can always "divide" by (abs y) to make things simpler.
    (let ((n (if (negative? n) (- 0 n) n)))
      (cond ((= n 1)
	     (load-fixnum-constant 0 target))
	    ((integer-power-of-2? n)
	     =>
	     (lambda (expt-of-2)
	       (let ((sign (temporary-register-reference))
		     (label (generate-label 'REM-MERGE))
		     (mask (-1+ (expt 2 nbits))))
		  ;; This may produce a branch to a branch, but a
		  ;; peephole optimizer should be able to fix this.
		 (LAP (MOV W ,sign ,target)
		      (SAR W ,sign (& ,(-1+ scheme-object-width)))
		      (XOR W ,sign (& ,mask))
		      (AND W ,target (& ,mask))
		      (JZ (@PCR ,label))
		      (OR W ,target ,sign)
		      (LABEL ,label)))))
	    (else
	     (error "Fixnum-remainder/constant: Bad value" n))))))

;;;; Predicate utilities

;; **** Here ****

(define (signed-fixnum? n)
  (and (integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

(define (unsigned-fixnum? n)
  (and (integer? n)
       (not (negative? n))
       (< n unsigned-fixnum/upper-limit)))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (guarantee-unsigned-fixnum n)
  (if (not (unsigned-fixnum? n)) (error "Not a unsigned fixnum" n))
  n)

(define (fixnum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQL)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LSS)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GTR)
    (else
     (error "FIXNUM-PREDICATE->CC: Unknown predicate" predicate))))

(define-integrable (test-fixnum/ea ea)
  (LAP (TST L ,ea)))

(define (fixnum-predicate/register*constant register constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (if (zero? constant)
      (test-fixnum/ea (any-register-reference register))
      (LAP (CMP L ,(any-register-reference register)
		,(make-immediate (* constant fixnum-1))))))

(define (fixnum-predicate/memory*constant memory constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (if (zero? constant)
      (test-fixnum/ea memory)
      (LAP (CMP L ,memory ,(make-immediate (* constant fixnum-1))))))