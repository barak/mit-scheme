#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rulfix.scm,v 1.15 1992/02/13 06:40:36 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/rules1.scm,v 4.36 1991/10/25 06:49:58 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
  ((fixnum-2-args/operate operator) target source1 source2))

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
     (multiply-fixnum-constant target (* n fixnum-1) false))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT (? n)))
			 #f))
  (fixnum-1-arg target source
   (lambda (target)
     (multiply-fixnum-constant target (* n fixnum-1) false))))

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OBJECT->FIXNUM (REGISTER (? register))))
  (fixnum-branch! predicate)
  (object->fixnum (standard-move-to-temporary! register)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OFFSET (REGISTER (? address)) (? offset)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-indirect-reference! address offset) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (REGISTER (? register-2)))
  (fixnum-branch! predicate)
  (compare/register*register register-1 register-2))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (OFFSET (REGISTER (? address)) (? offset)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-register-reference register)
	    ,(source-indirect-reference! address offset))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OFFSET (REGISTER (? address)) (? offset))
		      (REGISTER (? register)))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-indirect-reference! address offset)
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
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (LAP (CMP W ,(source-register-reference register)
	    (& ,(fixnum-object->fixnum-word constant)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OFFSET (REGISTER (? address)) (? offset))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-branch! predicate)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (& ,(fixnum-object->fixnum-word constant)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (OFFSET (REGISTER (? address)) (? offset)))
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (& ,(fixnum-object->fixnum-word constant)))))

;; This assumes that the immediately preceding instruction sets the
;; condition code bits correctly.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches!
   (lambda (label)
     (LAP (JO (@PCR ,label))))
   (lambda (label)
     (LAP (JNO (@PCR ,label)))))
  (LAP))

;;;; Utilities

(define (object->fixnum target)
  (LAP (SAL W ,target (& ,scheme-type-width))))

(define (fixnum->object target)
  (LAP (OR W ,target (& ,(ucode-type fixnum)))
       (ROR W ,target (& ,scheme-type-width))))

(define (address->fixnum target)
  (LAP (SAL W ,target (& ,scheme-type-width))))

(define (fixnum->address target)
  (LAP (SHR W ,target (& ,scheme-type-width))))

(define-integrable fixnum-1 64)		; (expt 2 scheme-type-width) ***

(define-integrable fixnum-bits-mask
  (-1+ fixnum-1))

(define (word->fixnum target)
  (LAP (AND W ,target (& ,(fix:not fixnum-bits-mask)))))

(define (integer-power-of-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define (load-fixnum-constant constant target)
  (if (zero? constant)
      (LAP (XOR W ,target ,target))
      (LAP (MOV W ,target (& ,(* constant fixnum-1))))))

(define (add-fixnum-constant target constant overflow?)
  (let ((value (* constant fixnum-1)))
    (cond ((and (zero? value) (not overflow?))
	   (LAP))
	  ((and (not (fits-in-signed-byte? value))
		(fits-in-signed-byte? (- value)))
	   (LAP (SUB W ,target (& ,(- value)))))
	  (else
	   (LAP (ADD W ,target (& ,value)))))))

(define (multiply-fixnum-constant target constant overflow?)
  (cond ((zero? constant)
	 (load-fixnum-constant 0 target))
	((= constant 1)
	 (if (not overflow?)
	     (LAP)
	     (add-fixnum-constant target 0 overflow?)))
	((= constant -1)
	 (LAP (NEG W ,target)))
	((and (not overflow?)
	      (integer-power-of-2? (abs constant)))
	 =>
	 (lambda (expt-of-2)
	   (if (negative? constant)
	       (LAP (SAL W ,target (& ,expt-of-2))
		    (NEG W ,target))
	       (LAP (SAL W ,target (& ,expt-of-2))))))
	(else
	 (LAP (IMUL W ,target (& ,constant))))))

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
  (operation (standard-move-to-target! source target)))

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
	     
(define ((fixnum-2-args/standard commutative? operate) target source1 source2)
  (two-arg-register-operation operate
			      commutative?
			      target
			      source1
			      source2))

(define (two-arg-register-operation operate commutative?
				    target source1 source2)
  (let* ((worst-case
	  (lambda (target source1 source2)
	    (LAP (LAP (MOV W ,target ,source1))
		 ,@(operate target source2))))
	 (new-target-alias!
	  (lambda ()
	    (let ((source1 (any-reference source1))
		  (source2 (any-reference source2)))
	      (delete-dead-registers!)
	      (worst-case (target-register-reference target)
			  source1
			  source2)))))
    (cond ((pseudo-register? target)
	   (reuse-pseudo-register-alias
	    source1 'GENERAL
	    (lambda (alias)
	      (let ((source2 (if (= source1 source2)
				 (register-reference alias)
				 (any-reference source2))))
		(delete-register! alias)
		(delete-dead-registers!)
		(add-pseudo-register-alias! target alias)
		(operate (register-reference alias) source2)))
	    (lambda ()
	      (if commutative?
		  (reuse-pseudo-register-alias
		   source2 'GENERAL
		   (lambda (alias2)
		     (let ((source1 (any-reference source1)))
		       (delete-register! alias2)
		       (delete-dead-registers!)
		       (add-pseudo-register-alias! target alias2)
		       (operate (register-reference alias2) source1)))
		   new-target-alias!)
		  (new-target-alias!)))))
	  ((not (eq? (register-type target) 'GENERAL))
	   (error "two-arg-register-operation: Wrong type register"
		  target 'GENERAL))
	  (else
	   (worst-case (register-reference target)
		       (any-reference source1)
		       (any-reference source2))))))

(define (fixnum-2-args/register*constant operator target
					 source constant overflow?)
  (fixnum-1-arg
   target source
   (lambda (target)
     ((fixnum-2-args/operate-constant operator) target constant overflow?))))

;;;; Arithmetic operations

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target 1 false)))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target)
    (add-fixnum-constant target -1 false)))

(define-fixnum-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (target)
    (LAP (NOT W ,target)
	 ,@(word->fixnum target))))

(define-fixnum-method 'FIXNUM-NEGATE fixnum-methods/1-arg
  (lambda (target)
    (LAP (NEG W ,target))))

(let-syntax
    ((binary-operation
      (macro (name instr commutative? idempotent?)
	`(define-fixnum-method ',name fixnum-methods/2-args
	   (fixnum-2-args/standard
	    ,commutative?
	    (lambda (target source2)
	      (if (and ,idempotent? (equal? target source2))
		  (LAP)
		  (LAP (,instr W ,',target ,',source2)))))))))

  (binary-operation PLUS-FIXNUM ADD true false)
  (binary-operation MINUS-FIXNUM SUB false false)
  (binary-operation FIXNUM-AND AND true true)
  (binary-operation FIXNUM-OR OR true true)
  (binary-operation FIXNUM-XOR XOR true false))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args
  (fixnum-2-args/standard
   false
   (lambda (target source2)
     (if (equal? target source2)
	 (load-fixnum-constant 0 target)
	 (let ((temp (temporary-register-reference)))
	   (LAP ,@(if (equal? temp source2)
		      (LAP)
		      (LAP (MOV W ,temp ,source2)))
		(NOT W ,temp)
		(AND W ,target ,temp)))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (fixnum-2-args/standard
   false
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
		   (IMUL W ,target ,temp))))))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args
  (let ((operate
	 (lambda (target source2)
	   ;; SOURCE2 is guaranteed not to be ECX because of the
	   ;; require-register! used below.
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
			(MOV W ,target ,temp))))))))
    (lambda (target source1 source2)
      (require-register! ecx)
      (two-arg-register-operation operate
				  false
				  target
				  source1
				  source2))))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (= source2 source1)
	(load-fixnum-constant 1 (target-register-reference target))
	(let ((load-dividend (load-machine-register! source1 eax)))
	  (require-register! edx)
	  (let ((source2 (any-reference source2)))
	    (rtl-target:=machine-register! eax)
	    (LAP ,@load-dividend
		 (MOV W (R ,edx) (R ,eax))
		 (SAR W (R ,edx) (& 31))
		 (IDIV W (R ,eax) ,source2)
		 (SAL W (R ,eax) (& ,scheme-type-width))))))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (= source2 source1)
	(load-fixnum-constant 0 (target-register-reference target))
	(let ((load-dividend (load-machine-register! source1 eax)))
	  (require-register! edx)
	  (let ((source2 (any-reference source2)))
	    (rtl-target:=machine-register! edx)
	    (LAP ,@load-dividend
		 (MOV W (R ,edx) (R ,eax))
		 (SAR W (R ,edx) (& 31))
		 (IDIV W (R ,eax) ,source2)
		 (SAL W (R ,edx) (& ,scheme-type-width))))))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (add-fixnum-constant target n overflow?)))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (add-fixnum-constant target (- 0 n) overflow?)))

(define-fixnum-method 'FIXNUM-OR fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant -1 target))
	  (else
	   (LAP (OR W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-XOR fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (LAP (NOT W ,target)
		,@(word->fixnum target)))
	  (else
	   (LAP (XOR W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-AND fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (load-fixnum-constant 0 target))
	  ((= n -1)
	   (LAP))
	  (else
	   (LAP (AND W ,target (& ,(* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((= n -1)
	   (load-fixnum-constant 0 target))
	  (else
	   (LAP (AND W ,target (& ,(* (fix:not n) fixnum-1))))))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((zero? n)
	   (LAP))
	  ((not (<= (- 0 scheme-datum-width) n scheme-datum-width))
	   (load-fixnum-constant 0 target))
	  ((not (negative? n))
	   (LAP (SHL W ,target (& ,n))))
	  (else
	   (LAP (SHR W ,target (& ,(- 0 n)))
		,@(word->fixnum target))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    (multiply-fixnum-constant target n overflow?)))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-constant
  (lambda (target n overflow?)
    overflow?				; ignored
    (cond ((= n 1)
	   (LAP))
	  ((= n -1)
	   (LAP (NEG W ,target)))
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
  (lambda (target n overflow?)
    ;; (remainder x y) is 0 or has the sign of x.
    ;; Thus we can always "divide" by (abs y) to make things simpler.
    overflow?				; ignored
    (let ((n (if (negative? n) (- 0 n) n)))
      (cond ((= n 1)
	     (load-fixnum-constant 0 target))
	    ((integer-power-of-2? n)
	     (let ((sign (temporary-register-reference))
		   (label (generate-label 'REM-MERGE))
		   (mask (-1+ (* n fixnum-1))))
	       ;; This may produce a branch to a branch, but a
	       ;; peephole optimizer should be able to fix this.
	       (LAP (MOV W ,sign ,target)
		    (AND W ,target (& ,mask))
		    (JZ (@PCR ,label))
		    (SAR W ,sign (& ,(-1+ scheme-object-width)))
		    (XOR W ,sign (& ,mask))
		    (OR W ,target ,sign)
		    (LABEL ,label))))
	    (else
	     (error "Fixnum-remainder/constant: Bad value" n))))))

(define (commute-fixnum-predicate predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQUAL-FIXNUM?)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'LESS-THAN-FIXNUM?)
    (else
     (error "commute-fixnum-predicate: Unknown predicate"
	    predicate))))

(define (fixnum-branch! predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?)
     (set-equal-branches!))
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JL (@PCR ,label))))
			    (lambda (label)
			      (LAP (JGE (@PCR ,label))))))
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?)
     (set-current-branches! (lambda (label)
			      (LAP (JG (@PCR ,label))))
			    (lambda (label)
			      (LAP (JLE (@PCR ,label))))))
    (else
     (error "FIXNUM-BRANCH!: Unknown predicate" predicate))))