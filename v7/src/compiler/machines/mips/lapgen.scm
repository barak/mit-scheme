#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/lapgen.scm,v 1.4 1991/06/17 21:21:40 cph Exp $
$MC68020-Header: lapgen.scm,v 4.26 90/01/18 22:43:36 GMT cph Exp $

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

;;;; RTL Rules for MIPS.  Shared utilities.

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
		   ,regnum:regs-pointer)))

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list
   ;; g0 g1 g2 g3 g4
   ;; g8 g9 g10 g11
   g12 g13 g14 g15 g16 g17 g18 g19
   ;; g20 g21 g22
   g23 g24
   ;; g26 g27 g28 g29
   g30
   g5 g6 g7 g25				; Allocate last
   ;; g31
   fp0 fp2 fp4 fp6 fp8 fp10 fp12 fp14
   fp16 fp18 fp20 fp22 fp24 fp26 fp28 fp30
   ;; fp1 fp3 fp5 fp7 fp9 fp11 fp13 fp15
   ;; fp17 fp19 fp21 fp23 fp25 fp27 fp29 fp31
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
      (if (< register 48)
	  (begin
	    (vector-set! references register (INST-EA (FPR ,fpr)))
	    (loop (1+ register) (1+ fpr)))))
    (lambda (register)
      (vector-ref references register))))

;;;; Useful Cliches

(define (memory->register-transfer offset base target)
  (case (register-type target)
    ((GENERAL) (LAP (LW ,target (OFFSET ,offset ,base))
		    (NOP)))
    ((FLOAT) (fp-load-doubleword offset base target #T))
    (else (error "unknown register type" target))))

(define (register->memory-transfer source offset base)
  (case (register-type source)
    ((GENERAL) (LAP (SW ,source (OFFSET ,offset ,base))))
    ((FLOAT) (fp-store-doubleword source offset base))
    (else (error "unknown register type" source))))

(define (load-constant constant target #!optional delay-slot?)
  ;; Load a Scheme constant into a machine register.
  (let ((delay-slot? (and (not (default-object? delay-slot?)) delay-slot?)))
    (if (non-pointer-object? constant)
	(load-immediate (non-pointer->literal constant) target)
	(LAP ,@(load-pc-relative (constant->label constant) target)
	     ,@(if delay-slot? '((NOP)) '())))))

(define (load-non-pointer type datum target)
  ;; Load a Scheme non-pointer constant, defined by type and datum,
  ;; into a machine register.
  (load-immediate (make-non-pointer-literal type datum) target))

(define (non-pointer->literal constant)
  (make-non-pointer-literal (object-type constant)
			    (careful-object-datum constant)))

(define-integrable (make-non-pointer-literal type datum)
  (+ (* type (expt 2 scheme-datum-width)) datum))

(define-integrable (deposit-type type-num target-reg)
  (if (= target-reg regnum:assembler-temp)
      (error "deposit-type: into register 1"))
  (LAP (AND ,target-reg ,target-reg ,regnum:address-mask)
       ,@(put-type type-num target-reg)))

(define-integrable (put-type type-num target-reg)
  ; Assumes that target-reg has 0 in type bits
  (LAP (LUI ,regnum:assembler-temp
	    ,(* type-scale-factor #x100 type-num))
       (OR  ,target-reg ,regnum:assembler-temp ,target-reg)))

;;;; Regularized Machine Instructions

(define (copy r t)
  (if (= r t)
      (LAP)
      (LAP (ADD ,t 0 ,r))))

(define-integrable (long->bits long)
  ((if (negative? long)
       signed-integer->bit-string
       unsigned-integer->bit-string) 32 long))

(define (adjusted:high long)
  (let ((n (long->bits long)))
    (+ (extract n 16 32)
       (if (> (extract n 0 16) #x7FFF)
	   1 0))))

(define (adjusted:low long)
  (extract-signed (long->bits long) 0 16))

(define (top-16-bits long)
  (extract (long->bits long) 16 32))

(define (add-immediate value source dest)
  (cond
   ((fits-in-16-bits-signed? value)
    (LAP (ADDI ,dest ,source ,value)))
   ((top-16-bits-only? value)
    (LAP (LUI ,regnum:assembler-temp ,(top-16-bits value))
	 (ADD ,dest ,regnum:assembler-temp ,source)))
   (else
    (LAP (ADDIU ,dest ,source ,(adjusted:low value))
	 (LUI ,regnum:assembler-temp ,(adjusted:high value))
	 (ADD ,dest ,dest ,regnum:assembler-temp)))))

(define (load-immediate value dest)
  (cond
   ((fits-in-16-bits-signed? value)
    (LAP (ADDI ,dest 0 ,value)))
   ((top-16-bits-only? value)
    (LAP (LUI ,dest ,(top-16-bits value))))
   ((fits-in-16-bits-unsigned? value)
    (LAP (ORI ,dest 0 ,value)))
   (else
    (LAP
     (LUI ,regnum:assembler-temp ,(adjusted:high value))
     (ADDIU ,dest ,regnum:assembler-temp ,(adjusted:low value))))))

(define (fp-copy from to)
  (if (= to from)
      (LAP)
      (LAP (FMOV DOUBLE ,(float-register->fpr to)
		        ,(float-register->fpr from)))))

;; Handled by VARIABLE-WIDTH in instr1.scm

(define (fp-load-doubleword offset base target NOP?)
  (let* ((least (float-register->fpr target))
	 (most (+ least 1)))
    (if (eq? endianness 'LITTLE)
	(LAP (LWC1 ,least (OFFSET ,offset ,base))
	     (LWC1 ,most (OFFSET ,(+ offset 4) ,base))
	     ,@(if NOP? (LAP (NOP)) (LAP)))
	(LAP (LWC1 ,least (OFFSET ,(+ offset 4) ,base))
	     (LWC1 ,most (OFFSET ,offset ,base))
	     ,@(if NOP? (LAP (NOP)) (LAP))))))

(define (fp-store-doubleword offset base source)
  (let* ((least (float-register->fpr source))
	 (most (+ least 1)))
    (if (eq? endianness 'LITTLE)
	(LAP (SWC1 ,least (OFFSET ,offset ,base))
	     (SWC1 ,most (OFFSET ,(+ offset 4) ,base)))
	(LAP (SWC1 ,least (OFFSET ,(+ offset 4) ,base))
	     (SWC1 ,most (OFFSET ,offset ,base))))))

(define (load-pc-relative label target)
  ;; Load a pc-relative location's contents into a machine register.
  (LAP (LW ,target (@PCR ,label))))

(define (load-pc-relative-address label target)
  ;; Load address of a pc-relative location into a machine register.
  (LAP (PC-RELATIVE-OFFSET ,target (@PCR ,label))))

(define (branch-generator! cc = < > <> >= <=)
  (let ((forward
	 (case cc
	   ((=)   =) ((<)  <)  ((>)  >)
	   ((<>) <>) ((>=) >=) ((<=) <=)))
	(inverse
	 (case cc
	   ((=)  <>) ((<)  >=) ((>)  <=)
	   ((<>) =)  ((>=) <)  ((<=) >))))
    (set-current-branches!
     (lambda (label)
       (LAP (,@forward (@PCR ,label)) (NOP)))
     (lambda (label)
       (LAP (,@inverse (@PCR ,label)) (NOP))))))

(define (compare-immediate comp i r2)
  ; Branch if immediate <comp> r2
  (let ((cc (invert-condition-noncommutative comp)))
    ;; This machine does register <op> immediate; you can
    ;; now think of cc in this way
    (if (zero? i)
	(begin
	  (branch-generator! cc
	    `(BEQ 0 ,r2) `(BLTZ ,r2) `(BGTZ ,r2)
	    `(BNE 0 ,r2) `(BGEZ ,r2) `(BLEZ ,r2))
	  (LAP))
      (let ((temp (standard-temporary!)))
	(if (fits-in-16-bits-signed? i)
	    (begin
	      (branch-generator! cc
	        `(BEQ ,temp ,r2) `(BNE 0 ,temp) `(BEQ 0 ,temp)
		`(BNE ,temp ,r2) `(BEQ 0 ,temp) `(BNE 0 ,temp))
	      (case cc
		((= <>) (LAP (ADDI ,temp 0 ,i)))
		((< >=) (LAP (SLTI ,temp ,r2 ,i)))
		((> <=) (LAP (SLTI ,temp ,r2 ,(+ i 1))))))
	    (LAP ,@(load-immediate i temp)
		 ,@(compare comp temp r2)))))))

(define (compare condition r1 r2)
  ; Branch if r1 <cc> r2
  (let ((temp (if (memq condition '(< > <= >=))
		  (standard-temporary!)
		  '())))
    (branch-generator! condition
      `(BEQ ,r1 ,r2) `(BNE ,temp 0) `(BNE ,temp 0)
      `(BNE ,r1 ,r2) `(BEQ ,temp 0) `(BEQ ,temp 0))
    (case condition
      ((= <>) (LAP))
      ((< >=) (LAP (SLT ,temp ,r1 ,r2)))
      ((> <=) (LAP (SLT ,temp ,r2 ,r1))))))

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
  ; A OP B  NOT (A OP B)      B OP A
  ;           invert      invert non-comm.
  '((=		<>		=)
    (<		>=		>)
    (>		<=		<)
    (<>		=		<>)
    (<=		>		>=)
    (>=		<		<=)))

;;;; Miscellaneous

(define-integrable (object->datum src tgt)
  ; Zero out the type field; don't put in the quad bits
  (LAP (AND ,tgt ,regnum:address-mask ,src)))

(define-integrable (object->address reg)
  ; Drop in the segment bits 
  (LAP (AND ,reg ,regnum:address-mask ,reg)
       ,@(put-address-bits reg)))

(define-integrable (put-address-bits reg)
  ; Drop in the segment bits, assuming they are currently 0
  (LAP (OR ,reg ,reg ,regnum:quad-bits)))

(define-integrable (object->type src tgt)
  ; Type extraction
  (LAP (SRL ,tgt ,src ,(- 32 scheme-type-width))))

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

(define (fits-in-16-bits-signed? value)
  (<= #x-8000 value #x7FFF))

(define (fits-in-16-bits-unsigned? value)
  (<= #x0 value #xFFFF))

(define (top-16-bits-only? value)
  (zero? (remainder value #x10000)))

(define-integrable (ea/mode ea) (car ea))
(define-integrable (register-ea/register ea) (cadr ea))
(define-integrable (offset-ea/offset ea) (cadr ea))
(define-integrable (offset-ea/register ea) (caddr ea))

(define (pseudo-register-displacement register)
  ;; Register block consists of 16 4-byte registers followed by 256
  ;; 8-byte temporaries.
  (+ (* 4 16) (* 8 (register-renumber register))))

(define-integrable (float-register->fpr register)
  ;; Float registers are represented by 32 through 47 in the RTL,
  ;; corresponding to even registers 0 through 30 in the machine.
  (- register 32))

(define-integrable (fpr->float-register register)
  (+ register 32))

(define-integrable reg:memtop
  (INST-EA (OFFSET #x0000 ,regnum:regs-pointer)))

(define-integrable reg:environment
  (INST-EA (OFFSET #x000C ,regnum:regs-pointer)))

(define-integrable reg:lexpr-primitive-arity
  (INST-EA (OFFSET #x001C ,regnum:regs-pointer)))

(define (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (BEQ 0 0 (@PCR ,label))
       (NOP)))

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

(define-integrable (link-to-interface code)
  ;; Jump, with link in 31, to link_to_interface
  (LAP (ADDI ,regnum:assembler-temp ,regnum:scheme-to-interface -100)
       (JALR ,regnum:linkage ,regnum:assembler-temp)
       (ADDI ,regnum:interface-index 0 ,(* 4 code))))

(define-integrable (link-to-trampoline code)
  ;; Jump, with link in 31, to trampoline_to_interface
  (LAP (ADDI ,regnum:assembler-temp ,regnum:scheme-to-interface -96)
       (JALR ,regnum:linkage ,regnum:assembler-temp)
       (ADDI ,regnum:interface-index 0 ,(* 4 code))))

(define-integrable (invoke-interface code)
  ;; Jump to scheme-to-interface
  (LAP (JR ,regnum:scheme-to-interface)
       (ADDI ,regnum:interface-index 0 ,(* 4 code))))

(define (load-interface-args! first second third fourth)
  (let ((clear-regs
	 (apply clear-registers!
		(append (if first (list regnum:first-arg) '())
			(if second (list regnum:second-arg) '())
			(if third (list regnum:third-arg) '()))))
	(load-reg
	 (lambda (reg arg)
	   (if reg (load-machine-register! reg arg) (LAP)))))
    (let ((load-regs
	   (LAP ,@(load-reg first regnum:second-arg)
		,@(load-reg second regnum:third-arg)
		,@(load-reg third regnum:fourth-arg)
		,@(if fourth
		      (let ((temp (standard-temporary!)))
			(LAP
			 ,@(load-machine-register! fourth temp)
			 (SW ,temp
			     (OFFSET 16 ,regnum:C-stack-pointer))))
		      (LAP)))))
      (LAP ,@clear-regs
	   ,@load-regs
	   ,@(clear-map!)))))