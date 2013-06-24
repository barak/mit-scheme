#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; RTL Rules for HPPA.  Shared utilities.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define (register->register-transfer source target)
  (guarantee-registers-compatible source target)
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

;; ***
;; Note: fp16-fp31 only exist on PA-RISC 1.1 or later.
;; If compiling for PA-RISC 1.0, truncate this
;; list after fp15.
;; ***

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
   fp12 fp13 fp14 fp15
   fp4 fp5 fp6 fp7 fp8 fp9 fp10 fp11
   ;; The following are only available on newer processors
   fp16 fp17 fp18 fp19 fp20 fp21 fp22 fp23
   fp24 fp25 fp26 fp27 fp28 fp29 fp30 fp31
   ))

(define-integrable (float-register? register)
  (eq? (register-type register) 'FLOAT))

(define-integrable (general-register? register)
  (eq? (register-type register) 'GENERAL))

(define-integrable (word-register? register)
  (eq? (register-type register) 'GENERAL))

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT
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
      (if (< register 64)
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
      (load-pc-relative (constant->label constant) target 'CONSTANT)))

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
  #x4000000)

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

#|
(define (load-pc-relative label target type)
  type					; ignored
  ;; Load a pc-relative location's contents into a machine register.
  ;; This assumes that the offset fits in 14 bits!
  ;; We should have a pseudo-op for LDW that does some "branch" tensioning.
  (LAP (BL () ,regnum:addil-result (@PCO 0))
       ;; Clear the privilege level, making this a memory address.
       (DEP () 0 31 2 ,regnum:addil-result)
       (LDW () (OFFSET (- ,label *PC*) 0 ,regnum:addil-result) ,target)))

(define (load-pc-relative-address label target type)
  type					; ignored
  ;; Load a pc-relative address into a machine register.
  ;; This assumes that the offset fits in 14 bits!
  ;; We should have a pseudo-op for LDO that does some "branch" tensioning.
  (LAP (BL () ,regnum:addil-result (@PCO 0))
       ;; Clear the privilege level, making this a memory address.
       (DEP () 0 31 2 ,regnum:addil-result)
       (LDO () (OFFSET (- ,label *PC*) 0 ,regnum:addil-result) ,target)))
|#

;; These versions of load-pc-... remember what they obtain, to avoid
;; doing the sequence multiple times.
;; In addition, they assume that the code is running in the least
;; privilege, and avoid the DEP in the sequences above.

(define-integrable *privilege-level* 3)

(define-integrable (close? label label*)
  ;; Heuristic
  label label*				; ignored
  compiler:compile-by-procedures?)

(define (load-pc-relative label target type)
  (load-pc-relative-internal label target type
			     (lambda (offset base target)
			       (LAP (LDW () (OFFSET ,offset 0 ,base)
					 ,target)))))

(define (load-pc-relative-address label target type)
  (load-pc-relative-internal label target type
			     (lambda (offset base target)
			       (LAP (LDO () (OFFSET ,offset 0 ,base)
					 ,target)))))

(define (load-pc-relative-internal label target type gen)
  (with-values (lambda () (get-typed-label type))
    (lambda (label* alias type*)
      (define (closer label* alias)
	(let ((temp (standard-temporary!)))
	  (set-typed-label! type label temp)
	  (LAP (LDO () (OFFSET (- ,label ,label*) 0 ,alias) ,temp)
	       ,@(gen 0 temp target))))

      (cond ((not label*)
	     (let ((temp (standard-temporary!))
		   (here (generate-label)))
	       (let ((value `(+ ,here ,(+ 8 *privilege-level*))))
		 (set-typed-label! 'CODE value temp)
		 (LAP (LABEL ,here)
		      (BL () ,temp (@PCO 0))
		      ,@(if (or (eq? type 'CODE) (close? label label*))
			    (gen (INST-EA (- ,label ,value)) temp target)
			    (closer value temp))))))
	    ((or (eq? type* type) (close? label label*))
	     (gen (INST-EA (- ,label ,label*)) alias target))
	    (else
	     (closer label* alias))))))

;;; Typed labels provide further optimization.  There are two types,
;;; CODE and CONSTANT, that say whether the label is located in the
;;; code block or the constants block of the output.  Statistically,
;;; a label is likely to be closer to another label of the same type
;;; than to a label of the other type.

(define (get-typed-label type)
  (let ((entries (register-map-labels *register-map* 'GENERAL)))
    (let loop ((entries* entries))
      (cond ((null? entries*)
	     ;; If no entries of the given type, use any entry that is
	     ;; available.
	     (let loop ((entries entries))
	       (cond ((null? entries)
		      (values false false false))
		     ((pair? (caar entries))
		      (values (cdaar entries) (cadar entries) (caaar entries)))
		     (else
		      (loop (cdr entries))))))
	    ((and (pair? (caar entries*))
		  (eq? type (caaar entries*)))
	     (values (cdaar entries*) (cadar entries*) type))
	    (else
	     (loop (cdr entries*)))))))

(define (set-typed-label! type label alias)
  (set! *register-map*
	(set-machine-register-label *register-map* alias (cons type label)))
  unspecific)

;; COMIBTN, COMIBFN, and COMBN are pseudo-instructions that nullify
;; the following instruction when the branch is taken.  Since COMIBT,
;; etc. nullify according to the sign of the displacement, the branch
;; tensioner inserts NOPs as necessary (backward branches).

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
	      (LAP (COMIBTN (,cc) ,i ,r2 (@PCR ,label))))
	    (lambda (label)
	      (LAP (COMIBFN (,cc) ,i ,r2 (@PCR ,label)))))
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
     (LAP (COMBN (,condition) ,r1 ,r2 (@PCR ,label))))
   (lambda (label)
     (LAP (COMBN (,(invert-condition condition)) ,r1 ,r2 (@PCR ,label)))))
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
  ;; The sources are any register, `target' a pseudo register.
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

(define-integrable (arithmetic-method? operator methods)
  (assq operator (cdr methods)))  

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
  ;; Float registers are represented by 32 through 47/63 in the RTL,
  ;; corresponding to registers 0 through 15/31 in the machine.
  (- register 32))

(define-integrable (fpr->float-register register)
  (+ register 32))

(define-integrable reg:memtop
  (INST-EA (OFFSET #x0000 0 ,regnum:regs-pointer)))

(define-integrable reg:environment
  (INST-EA (OFFSET #x000C 0 ,regnum:regs-pointer)))

(define-integrable reg:lexpr-primitive-arity
  (INST-EA (OFFSET #x001C 0 ,regnum:regs-pointer)))

(define-integrable reg:stack-guard
  (INST-EA (OFFSET #x002C 0 ,regnum:regs-pointer)))

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (B (N) (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

;;;; Codes and Hooks

(let-syntax ((define-codes
	       (sc-macro-transformer
		(lambda (form environment)
		  environment
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (index (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'CODE:COMPILER-
						      (car names))
				      ,index)
				   (loop (cdr names) (+ index 1)))
			     '())))))))
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply primitive-error
    quotient remainder modulo
    reflect-to-interface interrupt-continuation-2
    compiled-code-bkpt compiled-closure-bkpt))

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
	       (sc-macro-transformer
		(lambda (form environment)
		  environment
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (index (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'HOOK:COMPILER-
						      (car names))
				      ,index)
				   (loop (cdr names) (+ index 8)))
			     '())))))))
  (define-hooks 100
    store-closure-code
    store-closure-entry			; newer version of store-closure-code.
    multiply-fixnum
    fixnum-quotient
    fixnum-remainder
    fixnum-lsh
    &+
    &-
    &*
    &/
    &=
    &<
    &>
    1+
    -1+
    zero?
    positive?
    negative?
    shortcircuit-apply
    shortcircuit-apply-1
    shortcircuit-apply-2
    shortcircuit-apply-3
    shortcircuit-apply-4
    shortcircuit-apply-5
    shortcircuit-apply-6
    shortcircuit-apply-7
    shortcircuit-apply-8
    stack-and-interrupt-check
    invoke-primitive
    vector-cons
    string-allocate
    floating-vector-cons
    flonum-sin
    flonum-cos
    flonum-tan
    flonum-asin
    flonum-acos
    flonum-atan
    flonum-exp
    flonum-log
    flonum-truncate
    flonum-ceiling
    flonum-floor
    flonum-atan2
    compiled-code-bkpt
    compiled-closure-bkpt
    copy-closure-pattern
    copy-multiclosure-pattern))

;; There is a NOP here because otherwise the return address would have 
;; to be adjusted by the hook code.  This gives more flexibility to the
;; compiler since it may be able to eliminate the NOP by moving an
;; instruction preceding the BLE to the delay slot.

(define (invoke-hook hook)
  (LAP (BLE () (OFFSET ,hook 4 ,regnum:scheme-to-interface-ble))
       (NOP ())))

;; This is used when not returning.  It uses BLE instead of BE as a debugging
;; aid.  The hook gets a return address pointing to the caller, even
;; though the code will not return.

(define (invoke-hook/no-return hook)
  (LAP (BLE (N) (OFFSET ,hook 4 ,regnum:scheme-to-interface-ble))))

(define (require-registers! . regs)
  (let ((code (apply clear-registers! regs)))
    (need-registers! regs)
    code))

(define (load-interface-args! first second third fourth)
  (let ((clear-regs
	 (apply clear-registers!
		(append (if first (list regnum:first-arg) '())
			(if second (list regnum:second-arg) '())
			(if third (list regnum:third-arg) '())
			(if fourth (list regnum:fourth-arg) '()))))
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


(define (pre-lapgen-analysis rgraphs)
  rgraphs
  unspecific)