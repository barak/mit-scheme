#| -*-Scheme-*-

$Id: rulflo.scm,v 1.4 1995/08/15 00:12:17 adams Exp $

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

;;;; LAP Generation Rules: Flonum rules
;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define (flonum-source! register)
  (float-register->fpr (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (float-register->fpr (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (float-register->fpr (allocate-temporary-register! 'FLOAT)))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (flonum-source! source))
	(temp   (standard-temporary!)))
    (let ((target (standard-target! target)))
      ;; With this ordering of STANDARD-TEMPORARY! and STANDARD-TARGET! it is
      ;; possible that TEMP = TARGET.  The sequence below works for this case.
      (LAP
       ;; make heap parsable forwards
       ;; (STW () 0 (OFFSET 0 0 ,regnum:free-pointer))	
       (DEPI () #b100 31 3 ,regnum:free-pointer)		; quad align
       ,@(load-non-pointer (ucode-type manifest-nm-vector) 2 temp)
       (STWS (MA C) ,temp (OFFSET 4 0 ,regnum:free-pointer))
       (LDO () (OFFSET -4 0 ,regnum:free-pointer) ,target)
       (FSTDS (MA) ,source (OFFSET 8 0 ,regnum:free-pointer))
       ,@(deposit-type (ucode-type flonum) target)
       ))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-move-to-temporary! source)))
    (LAP ,@(object->address source)
	 (FLDDS () (OFFSET 4 0 ,source) ,(flonum-target! target)))))

;; This is endianness dependent!

(define (flonum-value->data-decl value)
  (let ((high (make-bit-string 32 false))
	(low (make-bit-string 32 false)))
    (read-bits! value 32 high)
    (read-bits! value 64 low)
    (LAP ,@(lap:comment `(FLOAT ,value))
	 (UWORD () ,(bit-string->unsigned-integer high))
	 (UWORD () ,(bit-string->unsigned-integer low)))))

(define (flonum->label value)
  (let* ((block
	  (or (find-extra-code-block 'FLOATING-CONSTANTS)
	      (let ((block (declare-extra-code-block! 'FLOATING-CONSTANTS
						      'ANYWHERE
						      '())))
		(add-extra-code!
		 block
		 (LAP (PADDING ,(- 0 *initial-dword-offset*) 8)))
		block)))
	 (pairs (extra-code-block/xtra block))
	 (place (assoc value pairs)))
    (if place
	(cdr place)
	(let ((label (generate-label)))
	  (set-extra-code-block/xtra!
	   block
	   (cons (cons value label) pairs))
	  (add-extra-code! block
			   (LAP (LABEL ,label)
				,@(flonum-value->data-decl value)))
	  label))))	 
				     
#|
(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (CONSTANT 0.)))
  (LAP (FCPY (DBL) 0 ,(flonum-target! target))))
|#

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (CONSTANT (? fp-value))))
  (cond ((not (flo:flonum? fp-value))
	 (error "OBJECT->FLOAT: Not a floating-point value" fp-value))
	(compiler:cross-compiling?
	 (let ((temp (standard-temporary!)))
	   (LAP ,@(load-constant fp-value temp)
		,@(object->address temp)
		(FLDDS () (OFFSET 4 0 ,temp) ,(flonum-target! target)))))
	((flo:= fp-value 0.0)
	 (LAP (FCPY (DBL) 0 ,(flonum-target! target))))
	(else
	 (let* ((temp (standard-temporary!))
		(target (flonum-target! target)))
	   (LAP ,@(load-pc-relative-address (flonum->label fp-value)
					    temp
					    'CONSTANT)
		(FLDDS () (OFFSET 0 0 ,temp) ,target))))))  

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset))))
  (float-load/offset target base (* 8 offset)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset))))
  (float-load/offset target base (+ (* 4 w-offset) (* 8 f-offset))))	  

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (float-store/offset base (* 8 offset) source))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset)))
	  (REGISTER (? source)))
  (float-store/offset base (+ (* 4 w-offset) (* 8 f-offset)) source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index))))
  (let* ((base (standard-source! base))
	 (index (standard-source! index))
	 (target (flonum-target! target)))
    (LAP (FLDDX (S) (INDEX ,index 0 ,base) ,target))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((source (flonum-source! source))
	(base (standard-source! base))
	(index (standard-source! index)))
    (LAP (FSTDX (S) ,source (INDEX ,index 0 ,base)))))

(define (float-load/offset target base offset)
  (let ((base (standard-source! base)))
    (%float-load/offset (flonum-target! target)
			base
			offset)))

(define (float-store/offset base offset source)
  (%float-store/offset (standard-source! base)
		       offset
		       (flonum-source! source)))

(define (%float-load/offset target base offset)
  (if (<= -16 offset 15)
      (LAP (FLDDS () (OFFSET ,offset 0 ,base) ,target))
      (let ((base* (standard-temporary!)))
	(LAP (LDO () (OFFSET ,offset 0 ,base) ,base*)
	     (FLDDS () (OFFSET 0 0 ,base*) ,target)))))

(define (%float-store/offset base offset source)
  (if (<= -16 offset 15)
      (LAP (FSTDS () ,source (OFFSET ,offset 0 ,base)))
      (let ((base* (standard-temporary!)))
	(LAP (LDO () (OFFSET ,offset 0 ,base) ,base*)
	     (FSTDS () ,source (OFFSET 0 0 ,base*))))))

;;;; Optimized floating-point references

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset))))
  (let ((b-offset (+ (* 4 w-offset) (* 8 f-offset))))
    (reuse-pseudo-register-alias!
     base 'GENERAL
     (lambda (base)
       (let ((target (flonum-target! target)))
	 (LAP ,@(object->address base)
	      ,@(%float-load/offset target base b-offset))))
     (lambda ()
       (let* ((base (standard-source! base))
	      (base* (standard-temporary!))
	      (target (flonum-target! target)))
	 (LAP (LDO () (OFFSET ,b-offset 0 ,base) ,base*)
	      ,@(object->address base*)
	      (FLDDS () (OFFSET 0 0 ,base*) ,target)))))))

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FLOAT-OFFSET (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
;					(MACHINE-CONSTANT (? offset)))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index)))))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!)))
;    (let ((target (flonum-target! target)))
;      (LAP (SH3ADDL () ,index ,base ,temp)
;	   ,@(object->address temp)
;	   ,@(%float-load/offset target temp (* 4 offset))))))

;(define-rule statement
;  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
;					(MACHINE-CONSTANT (? offset)))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index))))
;	  (REGISTER (? source)))
;  (let ((source (flonum-source! source))
;	(base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!)))
;    (LAP (SH3ADDL () ,index ,base ,temp)
;	 ,@(object->address temp)
;	 ,@(%float-store/offset temp (* 4 offset) source))))

;;;; Intermediate rules needed to generate the above.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
			  (MACHINE-CONSTANT (? offset))))
  (let* ((base (standard-source! base))
	 (target (standard-target! target)))
    (LAP (LDO () (OFFSET ,(* 4 offset) 0 ,base) ,target)
	 ,@(object->address target))))	

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
;					(MACHINE-CONSTANT (? offset)))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index)))))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!)))
;    (let ((target (flonum-target! target)))
;      (LAP ;; ,@(object->datum index temp)
;	   ;; (SH3ADDL () ,temp ,base ,temp)
;	   (SH3ADDL () ,index ,base ,temp)
;	   ,@(%float-load/offset target temp (* 4 offset))))))


;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FLOAT-OFFSET (REGISTER (? base))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index)))))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!)))
;    (let ((target (flonum-target! target)))
;      (LAP ,@(object->datum index temp)
;	   (FLDDX (S) (INDEX ,temp 0 ,base) ,target)))))

;(define-rule statement
;  (ASSIGN (REGISTER (? target))
;	  (FLOAT-OFFSET (REGISTER (? base))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index)))))
;  (let ((base (standard-source! base))
;	(index (standard-source! index)))
;    (let ((target (flonum-target! target)))
;      (LAP (FLDDX (S) (INDEX ,index 0 ,base) ,target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? offset)))
			(REGISTER (? index))))
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!)))
    (let ((target (flonum-target! target)))
      (LAP (SH3ADDL () ,index ,base ,temp)
	   ,@(%float-load/offset target temp (* 4 offset))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
					(MACHINE-CONSTANT (? offset)))
			(REGISTER (? index))))
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!)))
    (let ((target (flonum-target! target)))
      (LAP (SH3ADDL () ,index ,base ,temp)
	   ,@(object->address temp)
	   ,@(%float-load/offset target temp (* 4 offset))))))

;(define-rule statement
;  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
;					(MACHINE-CONSTANT (? offset)))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index))))
;	  (REGISTER (? source)))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!))
;	(source (flonum-source! source)))
;    (LAP ;; ,@(object->datum index temp)
;	 ;; (SH3ADDL () ,temp ,base ,temp)
;	 (SH3ADDL () ,index ,base ,temp)
;	 ,@(%float-store/offset temp (* 4 offset) source))))

;(define-rule statement
;  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index))))
;	  (REGISTER (? source)))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(temp (standard-temporary!))
;	(source (flonum-source! source)))
;    (LAP ,@(object->datum index temp)
;	 (FSTDX (S) ,source (INDEX ,temp 0 ,base)))))

;(define-rule statement
;  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
;			(OBJECT->UNSIGNED-FIXNUM (REGISTER (? index))))
;	  (REGISTER (? source)))
;  (let ((base (standard-source! base))
;	(index (standard-source! index))
;	(source (flonum-source! source)))
;    (LAP (FSTDX (S) ,source (INDEX ,index 0 ,base)))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? offset)))
			(REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!))
	(source (flonum-source! source)))
    (LAP (SH3ADDL () ,index ,base ,temp)
	 ,@(%float-store/offset temp (* 4 offset) source))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
					(MACHINE-CONSTANT (? offset)))
			(REGISTER (? index)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!))
	(source (flonum-source! source)))
    (LAP (SH3ADDL () ,index ,base ,temp)
	 ,@(object->address temp)
	 ,@(%float-store/offset temp (* 4 offset) source))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (arithmetic-method? operation flonum-methods/1-arg))
  overflow?				;ignore
  (let ((source (flonum-source! source)))
    ((flonum-1-arg/operator operation) (flonum-target! target) source)))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

;;; Notice the weird ,', syntax here.
;;; If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name opcode)
	 `(define-arithmetic-method ',primitive-name flonum-methods/1-arg
	    (lambda (target source)
	      (LAP (,opcode (DBL) ,',source ,',target)))))))
  (define-flonum-operation FLONUM-ABS FABS)
  (define-flonum-operation FLONUM-SQRT FSQRT)
  (define-flonum-operation FLONUM-ROUND FRND))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    ;; The status register (fr0) reads as 0 for non-store instructions.
    (LAP (FSUB (DBL) 0 ,source ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (arithmetic-method? operation flonum-methods/1-arg/special))
  overflow?				;ignore
  (flonum/1-arg/special
   (lookup-arithmetic-method operation flonum-methods/1-arg/special)
   target source))

(define flonum-methods/1-arg/special
  (list 'FLONUM-METHODS/1-ARG/SPECIAL))

(let-syntax ((define-out-of-line
	       (macro (name)
		 `(define-arithmetic-method ',name flonum-methods/1-arg/special
		    ,(symbol-append 'HOOK:COMPILER- name)))))
  (define-out-of-line FLONUM-SIN)
  (define-out-of-line FLONUM-COS)
  (define-out-of-line FLONUM-TAN)
  (define-out-of-line FLONUM-ASIN)
  (define-out-of-line FLONUM-ACOS)
  (define-out-of-line FLONUM-ATAN)
  (define-out-of-line FLONUM-EXP)
  (define-out-of-line FLONUM-LOG)
  (define-out-of-line FLONUM-TRUNCATE)
  (define-out-of-line FLONUM-CEILING)
  (define-out-of-line FLONUM-FLOOR))

(define caller-saves-registers
  (list
   ;; g1 g19 g20 g21 g22		; Not available for allocation
   g23 g24 g25 g26 g28 g29 g31
   ;; fp0 fp1 fp2 fp3			; Not real registers
   fp4 fp5 fp6 fp7 fp8 fp9 fp10 fp11))

(define registers-to-preserve-around-special-calls
  (append (list g14 g15 g16 g17)
	  caller-saves-registers))

(define (flonum/1-arg/special hook target source)
  (let ((load-arg (->machine-register source fp5)))
    (delete-register! target)
    (delete-dead-registers!)
    (let ((clear-regs
	   (apply clean-registers!
		  registers-to-preserve-around-special-calls)))
      (add-pseudo-register-alias! target fp4)
      (LAP ,@load-arg
	   ,@clear-regs
	   ,@(invoke-hook hook)))))

;; Missing operations

#|
;; Return integers
(define-out-of-line FLONUM-ROUND->EXACT)
(define-out-of-line FLONUM-TRUNCATE->EXACT)
(define-out-of-line FLONUM-FLOOR->EXACT)
(define-out-of-line FLONUM-CEILING->EXACT)

;; Returns a pair
(define-out-of-line FLONUM-NORMALIZE)

;; Two arguments
(define-out-of-line FLONUM-DENORMALIZE) ; flo*int
|#

;;;; Two arg operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS FLONUM-SUBTRACT
			 (OBJECT->FLOAT (CONSTANT 0.))
			 (REGISTER (? source))
			 (? overflow?)))
  overflow?				; ignore
  (let ((source (flonum-source! source)))
    (LAP (FSUB (DBL) 0 ,source ,(flonum-target! target)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (arithmetic-method? operation flonum-methods/2-args))
  overflow?				;ignore
  (let ((source1 (flonum-source! source1))
	(source2 (flonum-source! source2)))
    ((flonum-2-args/operator operation) (flonum-target! target)
					source1
					source2)))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name opcode)
	 `(define-arithmetic-method ',primitive-name flonum-methods/2-args
	    (lambda (target source1 source2)
	      (LAP (,opcode (DBL) ,',source1 ,',source2 ,',target)))))))
  (define-flonum-operation flonum-add fadd)
  (define-flonum-operation flonum-subtract fsub)
  (define-flonum-operation flonum-multiply fmpy)
  (define-flonum-operation flonum-divide fdiv)
  (define-flonum-operation flonum-remainder frem))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS FLONUM-ATAN2
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  (let* ((load-arg-1 (->machine-register source1 fp5))
	 (load-arg-2 (->machine-register source2 fp7)))
    (delete-register! target)
    (delete-dead-registers!)
    (let ((clear-regs
	   (apply clean-registers!
		  registers-to-preserve-around-special-calls)))
      (add-pseudo-register-alias! target fp4)
      (LAP ,@load-arg-1
	   ,@load-arg-2
	   ,@clear-regs
	   ,@(invoke-hook hook:compiler-flonum-atan2)))))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  #|
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((temp (flonum-temporary!)))
    (LAP (FSUB (DBL) ,temp ,temp ,temp)
	 ,@(flonum-compare
	    (case predicate
	      ((FLONUM-ZERO?) '=)
	      ((FLONUM-NEGATIVE?) '<)
	      ((FLONUM-POSITIVE?) '>)
	      (else (error "unknown flonum predicate" predicate)))
	    (flonum-source! source)
	    temp)))
  |#
  ;; The status register (fr0) reads as 0 for non-store instructions.
  (flonum-compare (case predicate
		    ((FLONUM-ZERO?) '=)
		    ((FLONUM-NEGATIVE?) '<)
		    ((FLONUM-POSITIVE?) '>)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source)
		  0))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (flonum-compare (case predicate
		    ((FLONUM-EQUAL?) '=)
		    ((FLONUM-LESS?) '<)
		    ((FLONUM-GREATER?) '>)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source1)
		  (flonum-source! source2)))

(define (flonum-compare cc r1 r2)
  (set-current-branches!
   (lambda (true-label)
     (LAP (FCMP (,(invert-float-condition cc) DBL) ,r1 ,r2)
	  (FTEST ())
	  (B (N) (@PCR ,true-label))))
   (lambda (false-label)
     (LAP (FCMP (,cc DBL) ,r1 ,r2)
	  (FTEST ())
	  (B (N) (@PCR ,false-label)))))
  (LAP))

;; invert-float-condition makes sure that NaNs are taken care of
;; correctly.

(define (invert-float-condition cc)
  (let ((place (assq cc float-inversion-table)))
    (if (not place)
	(error "invert-float-condition: Unknown condition"
	       cc)
	(cadr place))))

(define float-inversion-table
  ;; There are many others, but only these are used here.
  '((> !>)
    (< !<)
    (= !=)))