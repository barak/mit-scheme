#| -*-Scheme-*-

$Id: lapgen.scm,v 1.21 2004/07/01 01:19:58 cph Exp $

Copyright 1990,1991,1992,1993,1997,1998 Massachusetts Institute of Technology
Copyright 2001,2002,2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; RTL Rules for MIPS.  Shared utilities.
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
		   ,regnum:regs-pointer)))

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list
   ;; g0 g1 g2 g3
   ;; g8 g9 g10 g11
   g12 g13 g14 g15 g16 g17 g18
   ;; g19 g20 g21 g22 g23
   g24
   ;; g26 g27 g28 g29
   g30
   g7 g6 g5 g4 g25			; Allocate last
   ;; g31				; could be available if handled right
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
    ((GENERAL) (LAP (LW ,target (OFFSET ,offset ,base)) (NOP)))
    ((FLOAT) (fp-load-doubleword offset base target #T))
    (else (error "unknown register type" target))))

(define (register->memory-transfer source offset base)
  (case (register-type source)
    ((GENERAL) (LAP (SW ,source (OFFSET ,offset ,base))))
    ((FLOAT) (fp-store-doubleword offset base source))
    (else (error "unknown register type" source))))

(define (load-constant target constant delay-slot? record?)
  ;; Load a Scheme constant into a machine register.
  (if (non-pointer-object? constant)
      (load-immediate target (non-pointer->literal constant) record?)
      (load-pc-relative target
			'CONSTANT
			(constant->label constant)
			delay-slot?)))

(define (deposit-type-address type source target)
  (deposit-type-datum (fix:xor (quotient #x10 type-scale-factor) type)
		      source
		      target))

(define (deposit-type-datum type source target)
  (with-values
      (lambda ()
	(immediate->register (make-non-pointer-literal type 0)))
    (lambda (prefix alias)
      (LAP ,@prefix
	   (XOR ,target ,alias ,source)))))

(define (non-pointer->literal constant)
  (make-non-pointer-literal (object-type constant)
			    (careful-object-datum constant)))

(define-integrable (make-non-pointer-literal type datum)
  (+ (* type (expt 2 scheme-datum-width)) datum))

;;;; Regularized Machine Instructions

(define (adjusted:high n)
  (if (fits-in-16-bits-signed? n)
      0
      (let ((n (->unsigned n)))
	(if (< (remainder n #x10000) #x8000)
	    (quotient n #x10000)
	    (+ (quotient n #x10000) 1)))))

(define (adjusted:low n)
  (if (fits-in-16-bits-signed? n)
      n
      (let ((remainder (remainder (->unsigned n) #x10000)))
	(if (< remainder #x8000)
	    remainder
	    (- remainder #x10000)))))

(define-integrable (top-16-bits n)
  (quotient (->unsigned n) #x10000))

(define-integrable (bottom-16-bits n)
  (remainder (->unsigned n) #x10000))

(define (->unsigned n)
  (if (negative? n) (+ #x100000000 n) n))

(define-integrable (fits-in-16-bits-signed? value)
  (<= #x-8000 value #x7fff))

(define-integrable (fits-in-16-bits-unsigned? value)
  (<= #x0 value #xffff))

(define-integrable (top-16-bits-only? value)
  (zero? (bottom-16-bits value)))

(define (copy r t)
  (if (= r t)
      (LAP)
      (LAP (ADD ,t 0 ,r))))

(define (fp-copy from to)
  (if (= to from)
      (LAP)
      (LAP (MOV.D ,(float-register->fpr to)
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

;;;; PC-relative addresses

(define (load-pc-relative target type label delay-slot?)
  ;; Load a pc-relative location's contents into a machine register.
  ;; Optimization: if there is a register that contains the value of
  ;; another label, use that register as the base register.
  ;; Otherwise, allocate a temporary and load it with the value of the
  ;; label, then use the temporary as the base register.  This
  ;; strategy of loading a temporary wins if the temporary is used
  ;; again, but loses if it isn't, since loading the temporary takes
  ;; two instructions in addition to the LW instruction, while doing a
  ;; pc-relative LW instruction takes only two instructions total.
  ;; But pc-relative loads of various kinds are quite common, so this
  ;; should almost always be advantageous.
  (with-values (lambda () (get-typed-label type))
    (lambda (label* alias)
      (if label*
	  (LAP (LW ,target (OFFSET (- ,label ,label*) ,alias))
	       ,@(if delay-slot? (LAP (NOP)) (LAP)))
	  (let ((temporary (standard-temporary!)))
	    (set-typed-label! type label temporary)
	    (LAP ,@(%load-pc-relative-address temporary label)
		 (LW ,target (OFFSET 0 ,temporary))
		 ,@(if delay-slot? (LAP (NOP)) (LAP))))))))

(define (load-pc-relative-address target type label)
  ;; Load address of a pc-relative location into a machine register.
  ;; Optimization: if there is another register that contains the
  ;; value of another label, add the difference between the labels to
  ;; that register's contents instead.  The ADDI takes one
  ;; instruction, while the %LOAD-PC-RELATIVE-ADDRESS takes two, so
  ;; this is always advantageous.
  (let ((instructions
	 (with-values (lambda () (get-typed-label type))
	   (lambda (label* alias)
	     (if label*
		 (LAP (ADDI ,target ,alias (- ,label ,label*)))
		 (%load-pc-relative-address target label))))))
    (set-typed-label! type label target)
    instructions))

(define (%load-pc-relative-address target label)
  (let ((label* (generate-label)))
    (LAP (BGEZAL 0 (@PCO 4))
	 (LABEL ,label*)
	 (ADDI ,target 31 (- ,label (+ ,label* 4))))))

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
		      (values false false))
		     ((pair? (caar entries))
		      (values (cdaar entries) (cadar entries)))
		     (else
		      (loop (cdr entries))))))
	    ((and (pair? (caar entries*))
		  (eq? type (caaar entries*)))
	     (values (cdaar entries*) (cadar entries*)))
	    (else
	     (loop (cdr entries*)))))))

(define (set-typed-label! type label alias)
  (set! *register-map*
	(set-machine-register-label *register-map* alias (cons type label)))
  unspecific)

(define (immediate->register immediate)
  (let ((register (get-immediate-alias immediate)))
    (if register
	(values (LAP) register)
	(let ((temporary (standard-temporary!)))
	  (set! *register-map*
		(set-machine-register-label *register-map*
					    temporary
					    immediate))
	  (values (%load-immediate temporary immediate) temporary)))))

(define (get-immediate-alias immediate)
  (let loop ((entries (register-map-labels *register-map* 'GENERAL)))
    (cond ((null? entries)
	   false)
	  ((eqv? (caar entries) immediate)
	   (cadar entries))
	  (else
	   (loop (cdr entries))))))

(define (load-immediate target immediate record?)
  (let ((registers (get-immediate-aliases immediate)))
    (if (memv target registers)
	(LAP)
	(begin
	  (if record?
	      (set! *register-map*
		    (set-machine-register-label *register-map*
						target
						immediate)))
	  (if (not (null? registers))
	      (LAP (ADD ,target 0 ,(car registers)))
	      (%load-immediate target immediate))))))

(define (get-immediate-aliases immediate)
  (let loop ((entries (register-map-labels *register-map* 'GENERAL)))
    (cond ((null? entries)
	   '())
	  ((eqv? (caar entries) immediate)
	   (append (cdar entries) (loop (cdr entries))))
	  (else
	   (loop (cdr entries))))))

(define (%load-immediate target immediate)
  (cond ((fits-in-16-bits-signed? immediate)
	 (LAP (ADDIU ,target 0 ,immediate)))
	((fits-in-16-bits-unsigned? immediate)
	 (LAP (ORI ,target 0 ,immediate)))
	((top-16-bits-only? immediate)
	 (LAP (LUI ,target ,(top-16-bits immediate))))
	(else
	 (LAP (LUI ,target ,(top-16-bits immediate))
	      (ORI ,target ,target ,(bottom-16-bits immediate))))))

(define (add-immediate immediate source target)
  (if (fits-in-16-bits-signed? immediate)
      (LAP (ADDIU ,target ,source ,immediate))
      (with-values (lambda () (immediate->register immediate))
	(lambda (prefix alias)
	  (LAP ,@prefix
	       (ADDU ,target ,source ,alias))))))

;;;; Comparisons

(define (compare-immediate comp immediate source)
  ; Branch if immediate <comp> source
  (let ((cc (invert-condition-noncommutative comp)))
    ;; This machine does register <op> immediate; you can
    ;; now think of cc in this way.
    (if (zero? immediate)
	(let ((use-cc
	       (lambda (cc)
		 (branch-generator! cc
		   `(BEQ 0 ,source) `(BLTZ ,source) `(BGTZ ,source)
		   `(BNE 0 ,source) `(BGEZ ,source) `(BLEZ ,source))
		 (LAP))))
	  (case cc
	    ((<<) (compare-false))
	    ((>>=) (compare-true))
	    ((<<=) (use-cc '=))
	    ((>>) (use-cc '<>))
	    (else (use-cc cc))))
	(with-values (lambda () (immediate->register immediate))
	  (lambda (prefix alias)
	    (LAP ,@prefix
		 ,@(compare comp alias source)))))))

(define (compare condition r1 r2)
  ; Branch if r1 <cc> r2
  (if (= r1 r2)
      (if (memq condition '(< > <> << >>))
	  (compare-false)
	  (compare-true))
      (let ((temp
	     (and (memq condition '(< > <= >= << >> <<= >>=))
		  (standard-temporary!))))
	(branch-generator! condition
	  `(BEQ ,r1 ,r2) `(BNE ,temp 0) `(BNE ,temp 0)
	  `(BNE ,r1 ,r2) `(BEQ ,temp 0) `(BEQ ,temp 0))
	(case condition
	  ((= <>) (LAP))
	  ((< >=) (LAP (SLT ,temp ,r1 ,r2)))
	  ((> <=) (LAP (SLT ,temp ,r2 ,r1)))
	  ((<< >>=) (LAP (SLTU ,temp ,r1 ,r2)))
	  ((>> <<=) (LAP (SLTU ,temp ,r2 ,r1)))))))

(define (compare-true)
  (set-current-branches!
   (lambda (label) (LAP (BGEZ 0 (@PCR ,label)) (NOP)))
   (lambda (label) label (LAP)))
  (LAP))

(define (compare-false)
  (set-current-branches!
   (lambda (label) label (LAP))
   (lambda (label) (LAP (BGEZ 0 (@PCR ,label)) (NOP))))
  (LAP))

(define (branch-generator! cc = < > <> >= <=)
  (let ((forward
	 (case cc
	   ((=) =)
	   ((< <<) <)
	   ((> >>) >)
	   ((<>) <>)
	   ((>= >>=) >=)
	   ((<= <<=) <=)))
	(inverse
	 (case cc
	   ((=) <>)
	   ((< <<) >=)
	   ((> >>) <=)
	   ((<>) =)
	   ((>= >>=) <)
	   ((<= <<=) >))))
    (set-current-branches!
     (lambda (label)
       (LAP (,@forward (@PCR ,label)) (NOP)))
     (lambda (label)
       (LAP (,@inverse (@PCR ,label)) (NOP))))))

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
    (>=		<		<=)
    (<<		>>=		>>)
    (>>		<<=		<<)
    (<<=	>>		>>=)
    (>>=	<<		<<=)))

;;;; Miscellaneous

(define-integrable (object->type source target)
  ; Type extraction
  (LAP (SRL ,target ,source ,(- 32 scheme-type-width))))

(define-integrable (object->datum source target)
  ; Zero out the type field; don't put in the quad bits
  (LAP (AND ,target ,source ,regnum:address-mask)))

(define (object->address source target)
  ; Drop in the segment bits 
  (LAP (AND ,target ,source ,regnum:address-mask)
       (OR ,target ,target ,regnum:quad-bits)))

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
    ((CONS-NON-POINTER)
     (and (let ((type (rtl:cons-non-pointer-type expression)))
	    (and (rtl:machine-constant? type)
		 (zero? (rtl:machine-constant-value type))))
	  (let ((datum (rtl:cons-non-pointer-datum expression)))
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

(define-integrable reg:closure-limit
  (INST-EA (OFFSET #x0024 ,regnum:regs-pointer)))

(define-integrable reg:stack-guard
  (INST-EA (OFFSET #x002C ,regnum:regs-pointer)))

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (BEQ 0 0 (@PCR ,label))
       (NOP)))

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

(define (require-register! machine-reg)
  (flush-register! machine-reg)
  (need-register! machine-reg))

(define-integrable (flush-register! machine-reg)
  (prefix-instructions! (clear-registers! machine-reg)))

(define (rtl-target:=machine-register! rtl-reg machine-reg)
  (if (machine-register? rtl-reg)
      (begin
	(require-register! machine-reg)
	(if (not (= rtl-reg machine-reg))
	    (suffix-instructions!
	     (register->register-transfer machine-reg rtl-reg))))
      (begin
	(delete-register! rtl-reg)
	(flush-register! machine-reg)
	(add-pseudo-register-alias! rtl-reg machine-reg))))


(define (pre-lapgen-analysis rgraphs)
  rgraphs
  unspecific)