#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 4.5 1988/05/03 01:04:25 mhwu Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; RTL Rules for 68020.  Part 1

(declare (usual-integrations))

;;;; Basic machine instructions

(define (register->register-transfer source target)
  (LAP ,(machine->machine-register source target)))

(define (home->register-transfer source target)
  (LAP ,(pseudo->machine-register source target)))

(define (register->home-transfer source target)
  (LAP ,(machine->pseudo-register source target)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

(define-integrable (machine->machine-register source target)
  (INST (MOV L
	     ,(register-reference source)
	     ,(register-reference target))))

(define-integrable (machine-register->memory source target)
  (INST (MOV L
	     ,(register-reference source)
	     ,target)))

(define-integrable (memory->machine-register source target)
  (INST (MOV L
	     ,source
	     ,(register-reference target))))

(define (offset-reference register offset)
  (if (zero? offset)
      (if (< register 8)
	  (INST-EA (@D ,register))
	  (INST-EA (@A ,(- register 8))))
      (if (< register 8)
	  (INST-EA (@DO ,register ,(* 4 offset)))
	  (INST-EA (@AO ,(- register 8) ,(* 4 offset))))))

(define (load-dnw n d)
  (cond ((zero? n)
	 (INST (CLR W (D ,d))))
	((<= -128 n 127)
	 (INST (MOVEQ (& ,n) (D ,d))))
	(else
	 (INST (MOV W (& ,n) (D ,d))))))

(define (test-dnw n d)
  (if (zero? n)
      (INST (TST W (D ,d)))
      (INST (CMPI W (& ,n) (D ,d)))))

(define (increment-machine-register register n)
  (let ((target (register-reference register)))
    (case n
      ((0) (LAP))
      ((1 2) (LAP (ADDQ L (& ,(* 4 n)) ,target)))
      ((-1 -2) (LAP (SUBQ L (& ,(* -4 n)) ,target)))
      ((< register 8) (LAP (ADD L (& ,(* 4 n)) ,target)))
      (else (LAP (LEA (@AO ,(- register 8) ,(* 4 n)) ,target))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      (INST (MOV L
		 (@PCR ,(constant->label constant))
		 ,target))))

(define (load-fixnum-constant constant register-ref)
  (if (non-pointer-object? constant)
      (INST (MOV L (& ,(fixnum-constant constant)) ,register-ref))
      (LAP  (MOV L
		 (@PCR ,(constant->label constant))
		 ,register-ref)
	    ,(remove-type-from-fixmum register-ref))))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 (INST (MOV L
		    (& ,(make-non-pointer-literal type datum))
		    ,target)))
	((and (zero? datum)
	      (memq (lap:ea-keyword target)
		    '(D @D @A @A+ @-A @AO @DO @AOX W L)))
	 (INST (CLR L ,target)))
	((and (<= -128 datum 127) (eq? (lap:ea-keyword target) 'D))
	 (INST (MOVEQ (& ,datum) ,target)))
	(else (INST (MOV L (& ,datum) ,target)))))

(define (test-byte n effective-address)
  (if (and (zero? n) (TSTable-effective-address? effective-address))
      (INST (TST B ,effective-address))
      (INST (CMPI B (& ,n) ,effective-address))))

(define (test-non-pointer type datum effective-address)
  (if (and (zero? type) (zero? datum)
	   (TSTable-effective-address? effective-address))
      (INST (TST L ,effective-address))
      (INST (CMPI L
		  (& ,(make-non-pointer-literal type datum))
		  ,effective-address))))

(define (test-fixnum effective-address)
  (if (TSTable-effective-address? effective-address)
      (INST (TST L ,effective-address))
      (INST (CMPI L (& 0) ,effective-address))))
 
(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc cc) (@PCR ,label))))))

(define (invert-cc cc)
  (cdr (or (assq cc
		 '((T . F) (F . T)
		   (HI . LS) (LS . HI)
		   (HS . LO) (LO . HS)
		   (CC . CS) (CS . CC)
		   (NE . EQ) (EQ . NE)
		   (VC . VS) (VS . VC)
		   (PL . MI) (MI . PL)
		   (GE . LT) (LT . GE)
		   (GT . LE) (LE . GT)
		   ))
	   (error "INVERT-CC: Not a known CC" cc))))

(define (fixnum-pred->cc fixnum-predicate)
  (case fixnum-predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQ)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LT)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GT)
    (else
     (error "fixnum-pred->cc: Unknown fixnum predicate" fixnum-predicate))))

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER)
	      (coerce->target (cadr expression) register))
	     ((OFFSET)
	      (LAP
	       (MOV L
		    ,(indirect-reference! (cadadr expression)
					  (caddr expression))
		    ,target)))
	     ((CONSTANT)
	      (LAP ,(load-constant (cadr expression) target)))
	     ((UNASSIGNED)
	      (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define-integrable (TSTable-effective-address? effective-address)
  (memq (lap:ea-keyword effective-address)
	'(D @D @A @A+ @-A @DO @AO @AOX W L)))

(define-integrable (register-effective-address? effective-address)
  (memq (lap:ea-keyword effective-address) '(A D)))

(define (indirect-reference! register offset)
  (offset-reference
   (if (machine-register? register)
       register
       (or (register-alias register false)
	   ;; This means that someone has written an address out
	   ;; to memory, something that should happen only when the
	   ;; register block spills something.
	   (begin (warn "Needed to load indirect register!" register)
		  ;; Should specify preference for ADDRESS but will
		  ;; accept DATA if no ADDRESS registers available.
		  (load-alias-register! register 'ADDRESS))))
   offset))

(define (coerce->any register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (pseudo-register-home register)))))

(define (coerce->machine-register register)
  (if (machine-register? register)
      (register-reference register)
      (reference-alias-register! register false)))

(define (coerce->target source register)
  (if (is-alias-for-register? register source)
      (LAP)
      (LAP (MOV L ,(coerce->any source)
		,(register-reference register)))))

(define (code-object-label-initialize code-object)
  false)

(define (generate-n-times n limit instruction-gen with-counter)
  (if (> n limit)
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,(load-dnw (-1+ n) counter)
		(LABEL ,loop)
		,(instruction-gen)
		(DB F (D ,counter) (@PCR ,loop))))))
      (let loop ((n n))
	(if (zero? n)
	    (LAP)
	    (LAP ,(instruction-gen)
		 ,@(loop (-1+ n)))))))


;;; this fixnum stuff will be moved to fixlap.scm after we can include
;;; fixlap.scm's dependencies in decls.scm

(define (expression->fixnum-register! expression register)
;;; inputs:
;;;   - an rtl expression
;;;   - a register into which the produced code should place the
;;;     result of evaluating the expression.
;;; output: the lap code to move the expression into the register.
  (let ((target (register-reference register)))
    (case (rtl:expression-type expression)
      ((REGISTER)
       (LAP ,(coerce->target (rtl:register-number expression) register)))
      ((OFFSET)
       (LAP
	(MOV L
	     ,(indirect-reference! (rtl:register-number (rtl:offset-register expression))
				   (rtl:offset-number expression))
	     ,target)))
      ((CONSTANT)
       (LAP (MOV L (& ,(fixnum-constant (rtl:constant-value expression))) ,target)))
      ((UNASSIGNED)
       (LAP ,(load-non-pointer type-code:unassigned 0 target)))
      (else
       (error "expression->fixnum-register!:Unknown expression type" (expression))))))

(define (remove-type-from-fixnum register-reference)
;;; input: a register reference of a register  containing some fixnum
;;;        with a type-code
;;; output: the lap code to get rid of the type-code and sign extend
  (LAP (LS L L (& 8) ,register-reference)
       (AS R L (& 8) ,register-reference)))

(define (put-type-in-ea type-code effective-address)
;;; inputs:
;;;   - a type-code
;;;   - an effective address
;;; output: the lap code to stick the type in the top byte of the register
  (if (register-effective-address? effective-address)
      (LAP (AND L ,mask-reference ,effective-address)
	   (OR L (& ,(make-non-pointer-literal type-code 0))
		 ,effective-address))
      (INST (MOV B (& ,type-code) ,effective-address))))
	     
(define (fixnum-constant x)
  (if (<= (abs x) maximum-positive-fixnum)
      x
      (error "Not a fixnum" x)))

(define (fixnum-expression? expression)
;;; input: an rtl expression
;;; output: true, if the expression is of some fixnum type. false, otherwise
  (eq? (rtl:expression-type expression) 'FIXNUM))


(define (fixnum-do-2-args! operator operand-1 operand-2 register)
;;; inputs: 
;;;    - a fixnum operator
;;;    - an operand
;;;    - another operand
;;;    - the register into which the generated code should place the
;;;      result of the calculation 
;;; output: the lap code to calculate the fixnum expression
;;;
;;; Note that the final placement of the type-code in the result is
;;; not done here. It must be done in the caller.
  (LAP ,(expression->fixnum-register! operand-1 register)
       ,((fixnum-code-gen operator) operand-2 register)))


(define (fixnum-do-1-arg! operator operand register)
;;; inputs: 
;;;    - a fixnum operator
;;;    - an operand
;;;    - the register into which the generated code should place the
;;;      result of the calculation 
;;; output: the lap code to calculate the fixnum expression
;;;
;;; Note that the final placement of the type-code in the result is
;;; not done here. It must be done in the caller.
  (LAP ,(expression->fixnum-register! operand register)
       ,((fixnum-code-gen operator) register)))

(define fixnum-plus-gen
;;;   inputs:
;;;     - an rtl expression representing the addend
;;;     - a register to which the addend will be added
;;;   output: lap code to add the addend to the register
  (lambda (addend register)
    (let ((target (register-reference register)))
      (case (rtl:expression-type addend)
	((REGISTER)
	 (INST (ADD L ,(coerce->any (rtl:register-number addend)) ,target)))
	((OFFSET)
	 (INST (ADD L
		   ,(indirect-reference!
		     (rtl:register-number (rtl:offset-register addend))
		     (rtl:offset-number addend))
		   ,target)))
	((CONSTANT)
	 (INST (ADD L (& ,(fixnum-constant (rtl:constant-number addend))) ,target)))
	((UNASSIGNED)			; this needs to be looked at
	 (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	(else
	 (error "fixnum-plus-gen: Unknown expression type"  addend))))))

(define fixnum-multiply-gen
;;;   inputs:
;;;     - an rtl expression representing the multiplicand
;;;     - a register to which the multiplicand will be multiplied
;;;   output: lap code to add the multiplicand to the register
  (lambda (multiplicand register)
    (let ((target (register-reference register)))
      (case (rtl:expression-type multiplicand)
	((REGISTER)
	 (INST (MUL S L ,(coerce->any (rtl:register-number multiplicand)) ,target)))
	((OFFSET)
	 (INST (MUL S L
		   ,(indirect-reference!
		     (rtl:register-number (rtl:offset-register multiplicand))
		     (rtl:offset-number multiplicand))
		   ,target)))
	((CONSTANT)
	 (INST (MUL S L (& ,(fixnum-constant (rtl:constant-number multiplicand))) ,target)))
	((UNASSIGNED)			; this needs to be looked at
	 (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	(else
	 (error "fixnum-multiply-gen: Unknown expression type"  multiplicand))))))

(define fixnum-minus-gen
;;;   inputs:
;;;     - an rtl expression representing the subtrahend
;;;     - a register to which the subtrahend will be subtracted
;;;   output: lap code to add the subtrahend to the register
  (lambda (subtrahend register)
    (let ((target (register-reference register)))
      (case (rtl:expression-type subtrahend)
	((REGISTER)
	 (INST (SUB L ,(coerce->any (rtl:register-number subtrahend)) ,target)))
	((OFFSET)
	 (INST (SUB L
		   ,(indirect-reference!
		     (rtl:register-number (rtl:offset-register subtrahend))
		     (rtl:offset-number subtrahend))
		   ,target)))
	((CONSTANT)
	 (INST (SUB L (& ,(fixnum-constant (rtl:constant-number subtrahend))) ,target)))
	((UNASSIGNED)			; this needs to be looked at
	 (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	(else
	 (error "fixnum-minus-gen: Unknown expression type"  subtrahend))))))

(define fixnum-one-plus-gen
;;;   inputs:
;;;     - a register to be incremented
;;;   output: lap code to add one to the register
  (lambda (register)
    (INST (ADDQ  L (& 1) ,(register-reference register)))))

(define fixnum-minus-one-plus-gen
;;;   inputs:
;;;     - a register to be deccremented
;;;   output: lap code to subtract one from the register
  (lambda (register)
    (INST (SUBQ  L (& 1) ,(register-reference register)))))

(define (fixnum-code-gen operator)
;;; input: a fixnum operator
;;; output: a procedure with the following behavior
;;;           inputs:
;;;             - an operand to a fixnum expression
;;;             - a register which already should contain the other
;;;               operand to the fixnum expression
;;;           output: the lap code to apply the operator to the
;;;                   operand and register, putting the result in the register
  (case operator
    ((PLUS-FIXNUM) fixnum-plus-gen)
    ((MULTIPLY-FIXNUM) fixnum-multiply-gen)
    ((MINUS-FIXNUM) fixnum-minus-gen)
    ((ONE-PLUS-FIXNUM) fixnum-one-plus-gen)
    ((MINUS-ONE-PLUS-FIXNUM) fixnum-minus-one-plus-gen)
    ))


(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))
(define-integrable (lap:ea-keyword expression)
  (car expression))

(define-export (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define-export (lap:make-unconditional-branch label)
  (INST (BRA (@PCR ,label))))

(define-export (lap:make-entry-point label block-start-label)
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'ENTRY:COMPILER-
						(car names))
				(INST-EA (@AO 6 ,index)))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names start)))))
  (define-entries #x012c
    link error apply
    lexpr-apply primitive-apply primitive-lexpr-apply
    cache-reference-apply lookup-apply
    interrupt-continuation interrupt-ic-procedure
    interrupt-procedure interrupt-closure
    lookup safe-lookup set! access unassigned? unbound? define
    reference-trap safe-reference-trap assignment-trap unassigned?-trap
    &+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?))

(define-integrable reg:compiled-memtop (INST-EA (@A 6)))
(define-integrable reg:environment (INST-EA (@AO 6 #x000C)))
(define-integrable reg:temp (INST-EA (@AO 6 #x0010)))
(define-integrable reg:enclose-result (INST-EA (@AO 6 #x0014)))
(define-integrable reg:lexpr-primitive-arity (INST-EA (@AO 6 #x001C)))

(define-integrable popper:apply-closure (INST-EA (@AO 6 #x0168)))
(define-integrable popper:apply-stack (INST-EA (@AO 6 #x01A8)))
(define-integrable popper:value (INST-EA (@AO 6 #x01E8)))
