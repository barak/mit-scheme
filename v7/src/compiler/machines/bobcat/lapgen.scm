;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; RTL Rules for 68020

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 1.144 1987/01/01 19:41:39 cph Exp $

(declare (usual-integrations))
(using-syntax (access lap-generator-syntax-table compiler-package)

;;;; Basic machine instructions

(define (register->register-transfer source target)
  `(,(machine->machine-register source target)))

(define (home->register-transfer source target)
  `(,(pseudo->machine-register source target)))

(define (register->home-transfer source target)
  `(,(machine->pseudo-register source target)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

(define-integrable (machine->machine-register source target)
  `(MOVE L ,(register-reference source) ,(register-reference target)))

(define-integrable (machine-register->memory source target)
  `(MOVE L ,(register-reference source) ,target))

(define-integrable (memory->machine-register source target)
  `(MOVE L ,source ,(register-reference target)))

(define (offset-reference register offset)
  (if (zero? offset)
      (if (< register 8)
	  `(@D ,register)
	  `(@A ,(- register 8)))
      (if (< register 8)
	  `(@DO ,register ,(* 4 offset))
	  `(@AO ,(- register 8) ,(* 4 offset)))))

(define (load-dnw n d)
  (cond ((zero? n) `(CLR W (D ,d)))
	((<= -128 n 127) `(MOVEQ (& ,n) (D ,d)))
	(else `(MOVE W (& ,n) (D ,d)))))

(define (test-dnw n d)
  (if (zero? n)
      `(TST W (D ,d))
      `(CMP W (& ,n) (D ,d))))

(define (increment-anl an n)
  (case n
    ((0) '())
    ((1 2) `((ADDQ L (& ,(* 4 n)) (A ,an))))
    ((-1 -2) `((SUBQ L (& ,(* -4 n)) (A ,an))))
    (else `((LEA (@AO ,an ,(* 4 n)) (A ,an))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      `(MOVE L (@PCR ,(constant->label constant)) ,target)))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 `(MOVE L (& ,(make-non-pointer-literal type datum)) ,target))
	((and (zero? datum)
	      (memq (car target) '(D @D @A @A+ @-A @AO @DO @AOX W L)))
	 `(CLR L ,target))
	((and (<= -128 datum 127) (eq? (car target) 'D))
	 `(MOVEQ (& ,datum) ,target))
	(else
	 `(MOVE L (& ,datum) ,target))))

(define (test-byte n expression)
  (if (and (zero? n) (TSTable-expression? expression))
      `(TST B ,expression)
      `(CMP B (& ,n) ,expression)))

(define (test-non-pointer type datum expression)
  (if (and (zero? type) (zero? datum) (TSTable-expression? expression))
      `(TST L ,expression)
      `(CMP L (& ,(make-non-pointer-literal type datum)) ,expression)))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* type type-scale-factor) datum))))

(define (set-standard-branches! cc)
  (set-current-branches! (lambda (label)
			   `((B ,cc L (@PCR ,label))))
			 (lambda (label)
			   `((B ,(invert-cc cc) L (@PCR ,label))))))

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

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER) `((MOVE L ,(coerce->any (cadr expression)) ,target)))
	     ((OFFSET)
	      `((MOVE L ,(indirect-reference! (cadadr expression)
					      (caddr expression))
		      ,target)))
	     ((CONSTANT) `(,(load-constant (cadr expression) target)))
	     (else (error "Bad expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define-integrable (TSTable-expression? expression)
  (memq (car expression) '(D @D @A @A+ @-A @DO @AO @AOX W L)))

(define-integrable (register-expression? expression)
  (memq (car expression) '(A D)))

(define (indirect-reference! register offset)
  (offset-reference
   (if (machine-register? register)
       register
       (or (register-alias register false)
	   ;; This means that someone has written an address out
	   ;; to memory, something that should never happen.
	   (error "Needed to load indirect register!" register)))
   offset))

(define (coerce->any register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (pseudo-register-home register)))))

(define (code-object-label-initialize code-object)
  false)

(define (generate-n-times n limit instruction with-counter)
  (if (<= n limit)
      (let loop ((n n))
	(if (zero? n)
	    '()
	    `(,instruction
	      ,@(loop (-1+ n)))))
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   `(,(load-dnw (-1+ n) counter)
	     (LABEL ,loop)
	     ,instruction
	     (DB F (D ,counter) (@PCR ,loop))))))))

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro names
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE ,(symbol-append 'ENTRY:COMPILER-
						      (car names))
				'(@AO 6 ,index))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names #x00F0)))))
  (define-entries apply error wrong-number-of-arguments interrupt-procedure
    interrupt-continuation lookup-apply lookup access unassigned? unbound?
    set! define primitive-apply enclose setup-lexpr setup-ic-procedure))

(define reg:temp '(@AO 6 #x0010))
(define reg:enclose-result '(@AO 6 #x0014))
(define reg:compiled-memtop '(@A 6))

(define popper:apply-closure '(@AO 6 #x0168))
(define popper:apply-stack '(@AO 6 #x01C8))
(define popper:value '(@AO 6 #x0228))

;;;; Transfers to Registers

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (increment-anl 7 n))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  `(,(load-constant source (coerce->any target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  '())

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    `((AND L ,mask-reference ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    `((RO L L (& 8) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    ;; The fact that the target register here is a data register is a
    ;; heuristic that works reasonably well since if the value is a
    ;; pointer, we will probably want to dereference it, which
    ;; requires that we first mask it.
    `((MOVE L ,source
	    ,(register-reference (allocate-alias-register! target 'DATA))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (let ((target* (coerce->any target)))
    (if (pseudo-register? target)
	(delete-dead-registers!))
    `((MOVE L (@A+ 7) ,target*))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (let ((target* (coerce->any target))
	(datum (coerce->any datum)))
    (if (pseudo-register? target)
	(delete-dead-registers!))
    (if (register-expression? target*)
	`((MOVE L ,datum ,reg:temp)
	  (MOVE B (& ,type) ,reg:temp)
	  (MOVE L ,reg:temp ,target*))
	`((MOVE L ,datum ,target*)
	  (MOVE B (& ,type) ,target*)))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  `(,(load-constant object (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  `((MOVE L ,(coerce->any r) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  `((MOVE L (@A+ 7) ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (let ((target (indirect-reference! a n)))
    `((MOVE L ,(coerce->any r) ,target)
      (MOVE B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    `((MOVE L ,source ,(indirect-reference! a0 n0)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  `(,(load-constant object '(@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  `((MOVE L ,(coerce->any r) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  `((MOVE L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (ENTRY:PROCEDURE (? procedure)))
  (let ((temporary
	 (register-reference (allocate-temporary-register! 'ADDRESS))))
    `((LEA (@PCR ,(procedure-external-label procedure)) ,temporary)
      (MOVE L ,temporary (@A+ 5))
      (MOVE B (& ,type-code:return-address) (@AO 5 -4)))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  `(,(load-constant object '(@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (UNASSIGNED))
  `(,(load-non-pointer type-code:unassigned 0 '(@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  `((MOVE L ,(coerce->any r) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  `((MOVE L ,(coerce->any r) (@-A 7))
    (MOVE B (& ,type) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  `((MOVE L ,(indirect-reference! r n) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (OFFSET-ADDRESS (REGISTER 15) (? n)))
  `((PEA ,(offset-reference a7 n))
    (MOVE B (& ,type-code:stack-environment) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (ENTRY:CONTINUATION (? continuation)))
  `((PEA (@PCR ,(continuation-label continuation)))
    (MOVE B (& ,type-code:return-address) (@A 7))))

;;;; Predicates

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NE)
  `(,(test-non-pointer (ucode-type false) 0 (coerce->any register))))

(define-rule predicate
  (TRUE-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'NE)
  `(,(test-non-pointer (ucode-type false) 0
		       (indirect-reference! register offset))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  `(,(test-byte type
		(register-reference (load-alias-register! register 'DATA)))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    `((RO L L (& 8) ,reference)
      ,(test-byte type reference))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQ)
  `(,(test-non-pointer (ucode-type unassigned) 0 (coerce->any register))))

(define-rule predicate
  (UNASSIGNED-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'EQ)
  `(,(test-non-pointer (ucode-type unassigned) 0
		       (indirect-reference! register offset))))

;;;; Invocations

(define-rule statement
  (INVOCATION:APPLY (? number-pushed) (? prefix) (? continuation))
  `(,@(generate-invocation-prefix prefix)
    ,(load-dnw number-pushed 0)
    (JMP ,entry:compiler-apply)))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-CLOSURE (? frame-size) (? receiver-offset))
		   (? continuation) (? procedure))
  `(,@(clear-map!)
    ,@(apply-closure-sequence frame-size receiver-offset
			      (procedure-label procedure))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-STACK (? frame-size) (? receiver-offset)
				(? n-levels))
		   (? continuation) (? procedure))
  `(,@(clear-map!)
    ,@(apply-stack-sequence frame-size receiver-offset n-levels
			    (procedure-label procedure))))

(define-rule statement
  (INVOCATION:JUMP (? number-pushed) (? prefix) (? continuation) (? procedure))
  (QUALIFIER (not (memq (car prefix) '(APPLY-CLOSURE APPLY-STACK))))
  `(,@(generate-invocation-prefix prefix)
    (BRA L (@PCR ,(procedure-label procedure)))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? procedure))
  `(,@(generate-invocation-prefix prefix)
    ,(load-dnw number-pushed 0)
    (BRA L (@PCR ,(procedure-label procedure)))))

(define-rule statement
  (INVOCATION:LOOKUP (? number-pushed) (? prefix) (? continuation)
		     (? environment) (? name))
  (let ((set-environment (expression->machine-register! environment d4)))
    (delete-dead-registers!)
    `(,@set-environment
      ,@(generate-invocation-prefix prefix)
      ,(load-constant name '(D 5))
      (MOVE W (& ,(1+ number-pushed)) (D 0))
      (JMP ,entry:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? number-pushed) (? prefix) (? continuation)
			(? primitive))
  `(,@(generate-invocation-prefix prefix)
    ,@(if (eq? primitive compiled-error-procedure)
	  `(,(load-dnw (1+ number-pushed) 0)
	    (JMP ,entry:compiler-error))
	  `(,(load-dnw (primitive-datum primitive) 6)
	    (JMP ,entry:compiler-primitive-apply)))))

(define-rule statement
  (RETURN)
  `(,@(clear-map!)
    (CLR B (@A 7))
    (RTS)))

(define (generate-invocation-prefix prefix)
  `(,@(clear-map!)
    ,@(case (car prefix)
	((NULL) '())
	((MOVE-FRAME-UP)
	 (apply generate-invocation-prefix:move-frame-up (cdr prefix)))
	((APPLY-CLOSURE)
	 (apply generate-invocation-prefix:apply-closure (cdr prefix)))
	((APPLY-STACK)
	 (apply generate-invocation-prefix:apply-stack (cdr prefix)))
	(else (error "GENERATE-INVOCATION-PREFIX: bad prefix type" prefix)))))

(define (generate-invocation-prefix:move-frame-up frame-size how-far)
  (cond ((or (zero? frame-size) (zero? how-far)) '())
	((= frame-size 1)
	 `((MOVE L (@A+ 7) ,(offset-reference a7 (-1+ how-far)))))
	((= frame-size 2)
	 (if (= how-far 1)
	     `((MOVE L ,(offset-reference a7 1) ,(offset-reference a7 2))
	       (MOVE L (@A+ 7) (@A 7)))
	     (let ((i `(MOVE L (@A+ 7) ,(offset-reference a7 (-1+ how-far)))))
	       `(,i ,i ,@(increment-anl 7 (- how-far 2))))))
	(else
	 (let ((temp-0 (allocate-temporary-register! 'ADDRESS))
	       (temp-1 (allocate-temporary-register! 'ADDRESS)))
	   `((LEA ,(offset-reference a7 frame-size)
		  ,(register-reference temp-0))
	     (LEA ,(offset-reference a7 (+ frame-size how-far))
		  ,(register-reference temp-1))
	     ,@(generate-n-times frame-size 5
				 `(MOVE L
					(@-A ,(- temp-0 8))
					(@-A ,(- temp-1 8)))
		 (lambda (generator)
		   (generator (allocate-temporary-register! 'DATA))))
	     (MOVE L ,(register-reference temp-1) (A 7)))))))

(define (generate-invocation-prefix:apply-closure frame-size receiver-offset)
  (let ((label (generate-label)))
    `(,@(apply-closure-sequence frame-size receiver-offset label)
      (LABEL ,label))))

(define (generate-invocation-prefix:apply-stack frame-size receiver-offset
						n-levels)
  (let ((label (generate-label)))
    `(,@(apply-stack-sequence frame-size receiver-offset n-levels label)
      (LABEL ,label))))

;;;; Interpreter Calls

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? environment) (? name))
  (lookup-call entry:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? environment) (? name))
  (lookup-call entry:compiler-lookup environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? environment) (? name))
  (lookup-call entry:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? environment) (? name))
  (lookup-call entry:compiler-unbound? environment name))

(define (lookup-call entry environment name)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((clear-map (clear-map!)))
      `(,@set-environment
	,@clear-map
	,(load-constant name '(A 1))
	(JSR ,entry)
	,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:ENCLOSE (? number-pushed))
  `((MOVE L (A 5) ,reg:enclose-result)
    (MOVE B (& ,(ucode-type vector)) ,reg:enclose-result)
    ,(load-non-pointer (ucode-type manifest-vector) number-pushed
		       '(@A+ 5))
    ,@(generate-n-times number-pushed 5 '(MOVE L (@A+ 7) (@A+ 5))
	(lambda (generator)
	  `(,@(clear-registers! d0)
	    ,@(generator 0)))))
#| Alternate sequence which minimizes code size.
  `(,@(clear-registers! a0 a1 d0)
    (MOVE W (& ,number-pushed) (D 0))
    (JSR ,entry:compiler-enclose))|#
  )

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-set! environment name value))

(define (assignment-call:default entry environment name value)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((set-value (expression->machine-register! value a2)))
      (let ((clear-map (clear-map!)))
	`(,@set-environment
	  ,@set-value
	  ,@clear-map
	  ,(load-constant name '(A 1))
	  (JSR ,entry)
	  ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name)
			   (CONS-POINTER (CONSTANT (? type))
					 (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-define environment name type
				datum))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name)
			   (CONS-POINTER (CONSTANT (? type))
					 (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-set! environment name type
				datum))

(define (assignment-call:cons-pointer entry environment name type datum)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	`(,@set-environment
	  (MOVE L ,datum ,reg:temp)
	  (MOVE B (& ,type) ,reg:temp)
	  ,@clear-map
	  (MOVE L ,reg:temp (A 2))
	  ,(load-constant name '(A 1))
	  (JSR ,entry)
	  ,@(make-external-label (generate-label)))))))

;;;; Procedure/Continuation Entries

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.

(define-rule statement
  (PROCEDURE-HEAP-CHECK (? procedure))
  (let ((gc-label (generate-label)))
    `(,@(procedure-header procedure gc-label)
      (CMP L ,reg:compiled-memtop (A 5))
      (B GE S (@PCR ,gc-label)))))

(define-rule statement
  (SETUP-CLOSURE-LEXPR (? procedure))
  (lexpr-header procedure 1))

(define-rule statement
  (SETUP-STACK-LEXPR (? procedure))
  (lexpr-header procedure 0))

;;; Note: do not change the MOVE.W in the setup-lexpr call to a MOVEQ.
;;; The setup-lexpr code assumes a fixed calling sequence to compute
;;; the GC address if that is needed.

(define (lexpr-header procedure extra)
  `(,@(procedure-header procedure false)
    (MOVE W
	  (& ,(+ (length (procedure-required procedure))
		 (length (procedure-optional procedure))
		 extra))
	  (D 1))
    (MOVEQ (& ,(if (procedure-rest procedure) 1 0)) (D 2))
    (JSR , entry:compiler-setup-lexpr)))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? continuation))
  (let ((gc-label (generate-label))
	(internal-label (continuation-label continuation)))
    `((LABEL ,gc-label)
      (JSR ,entry:compiler-interrupt-continuation)
      ,@(make-external-label internal-label)
      (CMP L ,reg:compiled-memtop (A 5))
      (B GE S (@PCR ,gc-label)))))

(define (procedure-header procedure gc-label)
  (let ((internal-label (procedure-label procedure)))
    (append! (if (closure-procedure? procedure)
		 (let ((required (1+ (length (procedure-required procedure))))
		       (optional (length (procedure-optional procedure)))
		       (label (procedure-external-label procedure)))
		   (if (and (procedure-rest procedure)
			    (zero? required))
		       (begin (set-procedure-external-label! procedure
							     internal-label)
			      `((ENTRY-POINT ,internal-label)))
		       `((ENTRY-POINT ,label)
    			 ,@(make-external-label label)
			 ,(test-dnw required 0)
			 ,@(cond ((procedure-rest procedure)
				  `((B GE S (@PCR ,internal-label))))
				 ((zero? optional)
				  `((B EQ S (@PCR ,internal-label))))
				 (else
				  (let ((wna-label (generate-label)))
				    `((B LT S (@PCR ,wna-label))
				      ,(test-dnw (+ required optional) 0)
				      (B LE S (@PCR ,internal-label))
				      (LABEL ,wna-label)))))
			 (JMP ,entry:compiler-wrong-number-of-arguments))))
		 '())
	     (if gc-label
		 `((LABEL ,gc-label)
		   (JSR ,entry:compiler-interrupt-procedure))
		 '())
	     `(,@(make-external-label internal-label)))))

(define (make-external-label label)
  `((DC W (- ,label ,*block-start-label*))
    (LABEL ,label)))

;;;; Poppers

(define-rule statement
  (MESSAGE-RECEIVER:CLOSURE (? frame-size))
  `(;; Push (JSR (@AO 0 #x0000))
    (MOVE L (& #x4EA80000) (@-A 7))
    ;; Push (PEA (@PCO ,(+ 6 (* 4 frame-size))))
    (MOVE L (& ,(+ #x487A0000 (+ 6 (* 4 frame-size)))) (@-A 7))))

(define-rule statement
  (MESSAGE-RECEIVER:STACK (? frame-size))
  `(;; Push (JSR (@AO 0 #x0020))
    (MOVE L (& #x4EA80020) (@-A 7))
    ;; Push (DB F (D 0) (@PCO ,(+ 6 (* 4 frame-size))))
    (MOVE L (& ,(+ #x51C80000 (+ 6 (* 4 frame-size)))) (@-A 7))))

(define-rule statement
  (MESSAGE-RECEIVER:SUBPROBLEM (? continuation))
  `((PEA (@PCR ,(continuation-label continuation)))
    (MOVE B (& ,type-code:return-address) (@A 7))
    ;; Push (JSR (@AO 0 #x0040))
    (MOVE L (& #x4EA80040) (@-A 7))))

(define-rule statement
  (MESSAGE-SENDER:VALUE (? receiver-offset))
  `(,@(clear-map!)
    (MOVEQ (& -1) (D 0))
    (LEA ,popper:value (A 0))
    (JMP (@AO 7 ,(* receiver-offset 4)))))

(define (apply-closure-sequence frame-size receiver-offset label)
  `((MOVEQ (& -1) (D 0))
    ,(load-dnw frame-size 1)
    (LEA ,popper:apply-closure (A 0))
    (LEA (@PCR ,label) (A 1))
    (JMP (@AO 7 ,(* receiver-offset 4)))))

(define (apply-stack-sequence frame-size receiver-offset n-levels label)
  `((MOVEQ (& ,n-levels) (D 0))
    ,(load-dnw frame-size 1)
    (LEA ,popper:apply-stack (A 0))
    (LEA (@PCR ,label) (A 1))
    (JMP (@AO 7 ,(* receiver-offset 4)))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access lap-generator-package compiler-package)
;;; Scheme Syntax Table: (access lap-generator-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
(define popper:value '(@AO 6 #x01E8))