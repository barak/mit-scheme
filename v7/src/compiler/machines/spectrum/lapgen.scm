#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/lapgen.scm,v 4.28 1990/04/02 15:28:32 jinx Exp $
$MC68020-Header: lapgen.scm,v 4.31 90/04/01 22:26:01 GMT jinx Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; RTL Rules for HPPA.  Shared utilities.

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define (register->register-transfer source target)
  (if (not (register-types-compatible? source target))
      (error "Moving between incompatible register types" source target))
  (case (register-type source)
    ((GENERAL) (copy source target))
    ((FLOAT) (fp-copy source target))
    (else (error "unknown register type" source))))

(define (home->register-transfer source target)
  (memory->register-transfer (pseudo-register-displacement source)
			     regnum:regs-pointer
			     target))

(define (register->home-transfer source target)
  (register->memory-transfer source
			     (pseudo-register-displacement target)
			     regnum:regs-pointer))

(define (reference->register-transfer source target)
  (case (ea/mode source)
    ((GR)
     (copy (register-ea/register source) target))
    ((FPR)
     (fp-copy (fpr->float-register (register-ea/register source)) target))
    ((OFFSET)
     (memory->register-transfer (offset-ea/offset source)
				(offset-ea/register source)
				target))
    (else
     (error "unknown effective-address mode" source))))

(define (pseudo-register-home register)
  ;; Register block consists of 16 4-byte registers followed by 256
  ;; 8-byte temporaries.
  (INST-EA (OFFSET ,(pseudo-register-displacement register)
		   0
		   ,regnum:regs-pointer)))

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  ;; g1 removed from this list since it is the target of ADDIL,
  ;; needed to expand some rules.  g31 may want to be removed
  ;; too.
  (list
   ;; g0 g1 g2 g3 g4 g5
   g6 g7 g8 g9 g10 g11 g12 g13 g14 g15 g16 g17 g18
   ;; g19 g20 g21 g22
   g23 g24 g25 g26
   ;; g27
   g28 g29
   ;; g30
   g31
   ;; fp0 fp1 fp2 fp3
   fp4 fp5 fp6 fp7 fp8 fp9 fp10 fp11 fp12 fp13 fp14 fp15
   ))

(define-integrable (float-register? register)
  (eq? (register-type register) 'FLOAT))

(define-integrable (general-register? register)
  (eq? (register-type register) 'GENERAL))

(define-integrable (word-register? register)
  (eq? (register-type register) 'GENERAL))
      
(define (register-types-compatible? type1 type2)
  (boolean=? (eq? type1 'FLOAT) (eq? type2 'FLOAT)))

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT)
	  register))
	((register-value-class=word? register) 'GENERAL)
	((register-value-class=float? register) 'FLOAT)
	(else (error "unable to determine register type" register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (let loop ((register 0))
      (if (< register 32)
	  (begin
	    (vector-set! references register (INST-EA (GR ,register)))
	    (loop (1+ register)))))
    (let loop ((register 32) (fpr 0))
      (if (< register 48)
	  (begin
	    (vector-set! references register (INST-EA (FPR ,fpr)))
	    (loop (1+ register) (1+ fpr)))))
    (lambda (register)
      (vector-ref references register))))

;;;; Useful Cliches

(define (memory->register-transfer offset base target)
  (case (register-type target)
    ((GENERAL) (load-word offset base target))
    ((FLOAT) (fp-load-doubleword offset base target))
    (else (error "unknown register type" target))))

(define (register->memory-transfer source offset base)
  (case (register-type source)
    ((GENERAL) (store-word source offset base))
    ((FLOAT) (fp-store-doubleword source offset base))
    (else (error "unknown register type" source))))

(define (load-constant constant target)
  ;; Load a Scheme constant into a machine register.
  (if (non-pointer-object? constant)
      (load-immediate (non-pointer->literal constant) target)
      (load-pc-relative (constant->label constant) target)))

(define (load-non-pointer type datum target)
  ;; Load a Scheme non-pointer constant, defined by type and datum,
  ;; into a machine register.
  (load-immediate (make-non-pointer-literal type datum) target))

(define (non-pointer->literal constant)
  (make-non-pointer-literal (object-type constant)
			    (careful-object-datum constant)))

(define-integrable (make-non-pointer-literal type datum)
  (+ (* type type-scale-factor) datum))

(define-integrable type-scale-factor
  ;; (expt 2 scheme-datum-width) ***
  64)

(define-integrable (deposit-type type target)
  (deposit-immediate type (-1+ scheme-type-width) scheme-type-width target))

;;;; Regularized Machine Instructions

(define (copy r t)
  (if (= r t)
      (LAP)
      (LAP (COPY () ,r ,t))))

(define-integrable ldil-scale
  ;; (expt 2 11) ***
  2048)

(define (load-immediate i t)
  (if (fits-in-14-bits-signed? i)
      (LAP (LDI () ,i ,t))
      (let ((split (integer-divide i ldil-scale)))
	(LAP (LDIL () ,(integer-divide-quotient split) ,t)
	     ,@(let ((r%i (integer-divide-remainder split)))
		 (if (zero? r%i)
		     (LAP)
		     (LAP (LDO () (OFFSET ,r%i 0 ,t) ,t))))))))

(define (deposit-immediate i p len t)
  (if (fits-in-5-bits-signed? i)
      (LAP (DEPI () ,i ,p ,len ,t))
      (LAP ,@(load-immediate i regnum:addil-result)
	   (DEP () ,regnum:addil-result ,p ,len ,t))))

(define (load-offset d b t)
  (cond ((and (zero? d) (= b t))
	 (LAP))
	((fits-in-14-bits-signed? d)
	 (LAP (LDO () (OFFSET ,d 0 ,b) ,t)))
	(else
	 (let ((split (integer-divide d ldil-scale)))
	   (LAP (ADDIL () ,(integer-divide-quotient split) ,b)
		(LDO () (OFFSET ,(integer-divide-remainder split) 0 1) ,t))))))

(define (load-word d b t)
  (if (fits-in-14-bits-signed? d)
      (LAP (LDW () (OFFSET ,d 0 ,b) ,t))
      (let ((split (integer-divide d ldil-scale)))
	(LAP (ADDIL () ,(integer-divide-quotient split) ,b)
	     (LDW () (OFFSET ,(integer-divide-remainder split) 0 1) ,t)))))

(define (load-byte d b t)
  (if (fits-in-14-bits-signed? d)
      (LAP (LDB () (OFFSET ,d 0 ,b) ,t))
      (let ((split (integer-divide d ldil-scale)))
	(LAP (ADDIL () ,(integer-divide-quotient split) ,b)
	     (LDB () (OFFSET ,(integer-divide-remainder split) 0 1) ,t)))))

(define (store-word b d t)
  (if (fits-in-14-bits-signed? d)
      (LAP (STW () ,b (OFFSET ,d 0 ,t)))
      (let ((split (integer-divide d ldil-scale)))
	(LAP (ADDIL () ,(integer-divide-quotient split) ,t)
	     (STW () ,b (OFFSET ,(integer-divide-remainder split) 0 1))))))

(define (store-byte b d t)
  (if (fits-in-14-bits-signed? d)
      (LAP (STB () ,b (OFFSET ,d 0 ,t)))
      (let ((split (integer-divide d ldil-scale)))
	(LAP (ADDIL () ,(integer-divide-quotient split) ,t)
	     (STB () ,b (OFFSET ,(integer-divide-remainder split) 0 1))))))

(define (fp-copy r t)
  (if (= r t)
      (LAP)
      (LAP (FCPY (DBL) ,(float-register->fpr r) ,(float-register->fpr t)))))

(define (fp-load-doubleword d b t)
  (let ((t (float-register->fpr t)))
    (if (fits-in-5-bits-signed? d)
	(LAP (FLDDS () (OFFSET ,d 0 ,b) ,t))
	(LAP ,@(load-offset d b regnum:addil-result)
	     (FLDDS () (OFFSET 0 0 ,regnum:addil-result) ,t)))))

(define (fp-store-doubleword r d b)
  (let ((r (float-register->fpr r)))
    (if (fits-in-5-bits-signed? d)
	(LAP (FSTDS () ,r (OFFSET ,d 0 ,b)))
	(LAP ,@(load-offset d b regnum:addil-result)
	     (FSTDS () ,r (OFFSET 0 0 ,regnum:addil-result))))))

(define (load-pc-relative label target)
  ;; Load a pc-relative location's contents into a machine register.
  ;; This assumes that the offset fits in 14 bits!
  ;; We should have a pseudo-op for LDW that does some "branch" tensioning.
  (LAP (BL () ,regnum:addil-result (@PCO 0))
       ;; Clear the privilege level, making this a memory address.
       (DEP () 0 31 2 ,regnum:addil-result)
       (LDW () (OFFSET (- ,label *PC*) 0 ,regnum:addil-result) ,target)))

(define (load-pc-relative-address label target)
  ;; Load a pc-relative address into a machine register.
  ;; This assumes that the offset fits in 14 bits!
  ;; We should have a pseudo-op for LDO that does some "branch" tensioning.
  (LAP (BL () ,regnum:addil-result (@PCO 0))
       ;; Clear the privilege level, making this a memory address.
       (DEP () 0 31 2 ,regnum:addil-result)
       (LDO () (OFFSET (- ,label *PC*) 0 ,regnum:addil-result) ,target)))

;; NOPs are inserted since conditional nullification only nullifies
;; depending on the sign of the branch offset, which is unknown at
;; this point.  Linearizer can be changed, fairly easily, to tell us
;; which direction the branch goes so we can decide whether the NOP is
;; needed.

(define (compare-immediate cc i r2)
  (cond ((zero? i)
	 (compare cc 0 r2))
	((fits-in-5-bits-signed? i)
	 (let* ((inverted? (memq cc '(TR <> >= > >>= >> NSV EV
					 LTGT GTEQ GT GTGTEQ GTGT)))
		(cc (if inverted? (invert-condition cc) cc))
		(set-branches!
		 (lambda (if-true if-false)
		   (if inverted?
		       (set-current-branches! if-false if-true)
		       (set-current-branches! if-true if-false)))))
	
	   (set-branches!
	    (lambda (label)
	      (LAP (COMIBT (,cc) ,i ,r2 (@PCR ,label))
		   (NOP ())))
	    (lambda (label)
	      (LAP (COMIBF (,cc) ,i ,r2 (@PCR ,label))
		   (NOP ()))))
	   (LAP)))
	((fits-in-11-bits-signed? i)
	 (set-current-branches!
	  (lambda (label)
	    (LAP (COMICLR (,(invert-condition cc)) ,i ,r2 0)
		 (B (N) (@PCR ,label))))
	  (lambda (label)
	    (LAP (COMICLR (,cc) ,i ,r2 0)
		 (B (N) (@PCR ,label)))))
	 (LAP))
	(else
	 (let ((temp (standard-temporary!)))
	   (LAP ,@(load-immediate i temp)
		,@(compare cc temp r2))))))

(define (compare condition r1 r2)
  (set-current-branches!
   (lambda (label)
     (LAP (COMB (,condition) ,r1 ,r2 (@PCR ,label))
	  (NOP ())))
   (lambda (label)
     (LAP (COMB (,(invert-condition condition)) ,r1 ,r2 (@PCR ,label))
	  (NOP ()))))
  (LAP))

;;;; Conditions

(define (invert-condition condition)
  (let ((place (assq condition condition-inversion-table)))
    (if (not place)
	(error "unknown condition" condition))
    (cadr place)))

(define (invert-condition-noncommutative condition)
  (let ((place (assq condition condition-inversion-table)))
    (if (not place)
	(error "unknown condition" condition))
    (caddr place)))

(define condition-inversion-table
  '((=		<>		=)
    (<		>=		>)
    (>		<=		<)
    (NUV	UV		NUV)
    (TR		NV		TR)
    (<<		>>=		>>)
    (>>		<<=		<<)
    (<>		=		<>)
    (<=		>		>=)
    (>=		<		<=)
    (<<=	>>		>>=)
    (>>=	<<		<<=)
    (NV		TR		NV)
    (EQ		LTGT		EQ)
    (LT		GTEQ		GT)
    (SBZ	NBZ		SBZ)
    (LTEQ	GT		GTEQ)
    (SHZ	NHZ		SHZ)
    (LTLT	GTGTEQ		GTGT)
    (SDC	NDC		SDC)
    (LTLTEQ	GTGT		GTGTEQ)
    (ZNV	VNZ		ZNV)
    (SV		NSV		SV)
    (SBC	NBC		SBC)
    (OD		EV		OD)
    (SHC	NHC		SHC)
    (LTGT	EQ		LTGT)
    (GTEQ	LT		LTEQ)
    (NBZ	SBZ		NBZ)
    (GT		LTEQ		LT)
    (NHZ	SHZ		NHZ)
    (GTGTEQ	LTLT		LTLTEQ)
    (UV		NUV		UV)
    (NDC	SDC		NDC)
    (GTGT	LTLTEQ		LTLT)
    (VNZ	ZNV		NVZ)
    (NSV	SV		NSV)
    (NBC	SBC		NBC)
    (EV		OD		EV)
    (NHC	SHC		NHC)))

;;;; Miscellaneous

(define-integrable (object->datum src tgt)
  (LAP (ZDEP () ,src 31 ,scheme-datum-width ,tgt)))

(define-integrable (object->address reg)
  (LAP (DEP ()
	    ,regnum:quad-bitmask
	    ,(-1+ scheme-type-width)
	    ,scheme-type-width
	    ,reg)))

(define-integrable (object->type src tgt)
  (LAP (EXTRU () ,src ,(-1+ scheme-type-width) ,scheme-type-width ,tgt)))

(define (standard-unary-conversion source target conversion)
  ;; `source' is any register, `target' a pseudo register.
  (let ((source (standard-source! source)))
    (conversion source (standard-target! target))))

(define (standard-binary-conversion source1 source2 target conversion)
  (let ((source1 (standard-source! source1))
	(source2 (standard-source! source2)))
    (conversion source1 source2 (standard-target! target))))

(define (standard-source! register)
  (load-alias-register! register (register-type register)))

(define (standard-target! register)
  (delete-dead-registers!)
  (allocate-alias-register! register (register-type register)))

(define-integrable (standard-temporary!)
  (allocate-temporary-register! 'GENERAL))

(define (standard-move-to-target! source target)
  (move-to-alias-register! source (register-type source) target))

(define (standard-move-to-temporary! source)
  (move-to-temporary-register! source (register-type source)))

(define (register-expression expression)
  (case (rtl:expression-type expression)
    ((REGISTER)
     (rtl:register-number expression))
    ((CONSTANT)
     (let ((object (rtl:constant-value expression)))
       (and (zero? (object-type object))
	    (zero? (object-datum object))
	    0)))
    ((CONS-POINTER)
     (and (let ((type (rtl:cons-pointer-type expression)))
	    (and (rtl:machine-constant? type)
		 (zero? (rtl:machine-constant-value type))))
	  (let ((datum (rtl:cons-pointer-datum expression)))
	    (and (rtl:machine-constant? datum)
		 (zero? (rtl:machine-constant-value datum))))
	  0))
    (else false)))

(define (define-arithmetic-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-arithmetic-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define (fits-in-5-bits-signed? value)
  (<= #x-10 value #xF))

(define (fits-in-11-bits-signed? value)
  (<= #x-400 value #x3FF))

(define (fits-in-14-bits-signed? value)
  (<= #x-2000 value #x1FFF))

(define-integrable (ea/mode ea) (car ea))
(define-integrable (register-ea/register ea) (cadr ea))
(define-integrable (offset-ea/offset ea) (cadr ea))
(define-integrable (offset-ea/space ea) (caddr ea))
(define-integrable (offset-ea/register ea) (cadddr ea))

(define (pseudo-register-displacement register)
  ;; Register block consists of 16 4-byte registers followed by 256
  ;; 8-byte temporaries.
  (+ (* 4 16) (* 8 (register-renumber register))))

(define-integrable (float-register->fpr register)
  ;; Float registers are represented by 32 through 47 in the RTL,
  ;; corresponding to registers 0 through 15 in the machine.
  (- register 32))

(define-integrable (fpr->float-register register)
  (+ register 32))

(define-integrable reg:memtop
  (INST-EA (OFFSET #x0000 0 ,regnum:regs-pointer)))

(define-integrable reg:environment
  (INST-EA (OFFSET #x000C 0 ,regnum:regs-pointer)))

(define-integrable reg:lexpr-primitive-arity
  (INST-EA (OFFSET #x001C 0 ,regnum:regs-pointer)))

(define (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (B (N) (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

;;;; Codes and Hooks

(let-syntax ((define-codes
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'CODE:COMPILER-
						(car names))
				,index)
			     (loop (cdr names) (1+ index)))))
		 `(BEGIN ,@(loop names start)))))
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply))

(define-integrable (invoke-interface-ble code)
  ;; Jump to scheme-to-interface-ble
  (LAP (BLE () (OFFSET 0 4 ,regnum:scheme-to-interface-ble))
       (LDI () ,code 28)))

;;; trampoline-to-interface uses (OFFSET 4 4 ,regnum:scheme-to-interface-ble)

(define-integrable (invoke-interface code)
  ;; Jump to scheme-to-interface
  (LAP (BLE () (OFFSET 12 4 ,regnum:scheme-to-interface-ble))
       (LDI () ,code 28)))

(let-syntax ((define-hooks
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'HOOK:COMPILER-
						(car names))
				,index)
			     (loop (cdr names) (+ 8 index)))))
		 `(BEGIN ,@(loop names start)))))
  (define-hooks 100
    store-closure-code))

(define (load-interface-args! first second third fourth)
  (let ((clear-regs
	 (apply clear-registers!
		(append (if first (list first) '())
			(if second (list second) '())
			(if third (list third) '())
			(if fourth (list fourth) '()))))
	(load-reg
	 (lambda (reg arg)
	   (if reg (load-machine-register! reg arg) (LAP)))))
    (let ((load-regs
	   (LAP ,@(load-reg first regnum:first-arg)
		,@(load-reg second regnum:second-arg)
		,@(load-reg third regnum:third-arg)
		,@(load-reg fourth regnum:fourth-arg))))
      (LAP ,@clear-regs
	   ,@load-regs
	   ,@(clear-map!)))))