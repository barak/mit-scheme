#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; LAP Generation for AArch64
;;; package: (compiler)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define available-machine-registers
  (list
   r0
   r1
   r2
   r3
   r4
   r5
   r6
   r7
   r8
   r9
   r10
   r11
   r12
   r13
   r14
   r15
   ;r16 - PLT scratch; we'll use for branch tensioning
   ;r17 - PLT scratch; we'll use for branch tensioning
   ;r18 - platform ABI register
   ;r19 - interpreter register block
   ;r20 - free pointer
   ;r21 - dynamic link
   ;r22 - memtop
   ;r23 - scheme-to-interface
   r24
   r25
   r26
   r27
   r28
   ;r29 - C frame pointer, callee-saved and left alone by Scheme
   ;r30 - link register (could maybe allocate)
   ;r31 - stack pointer or zero register, depending on instruction
   ;      XXX could pick another one for our stack and leave this alone?
   ;; Vector registers, always available.
   v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
   v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31))

(define (sort-machine-registers registers)
  registers)

(define (register-type register)
  (cond ((machine-register? register)
         (if (< register 32) 'GENERAL 'FLOAT))
        ((register-value-class=word? register) 'GENERAL)
        ((register-value-class=float? register) 'FLOAT)
        (else (error "Unknown register type:" register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (do ((register 0 (+ register 1)))
        ((>= register 32))
      (vector-set! references register (INST-EA (R ,register))))
    (do ((register 32 (+ register 1)))
        ((>= register 64))
      (vector-set! references register (INST-EA (V ,(- register 32)))))
    (named-lambda (register-reference register)
      (vector-ref references register))))

(define (register=? a b)
  (= a b))

(define (register->register-transfer source target)
  (guarantee-registers-compatible source target)
  (if (register=? source target)
      (LAP)
      (case (register-type source)
        ((GENERAL)
         (if (or (= source rsp) (= target rsp))
             (LAP (ADD X ,target ,source (&U 0)))
             (LAP (ORR X ,target ,source (&U 0)))))
        ((FLOAT)
         (LAP (FMOV D ,target ,source)))
        (else
         (error "Unknown register type:" source target)))))

(define (pseudo-register-home register)
  (INST-EA (OFFSET ,regnum:regs-pointer ,(register-renumber register))))

(define (home->register-transfer source target)
  (memory->register-transfer regnum:regs-pointer
                             (pseudo-register-byte-offset source)
                             target))

(define (register->home-transfer source target)
  (register->memory-transfer source
                             regnum:regs-pointer
                             (pseudo-register-byte-offset target)))

(define (reference->register-transfer source target)
  (case (ea/mode source)
    ((R) (register->register-transfer (register-ea/register source) target))
    ((V) (register->register-transfer (vector-ea/register source) target))
    ((OFFSET)
     (memory->register-transfer (offset-ea/offset source)
                                (offset-ea/register source)
                                target))
    (else
     (error "Unknown effective address mode:" source target))))

(define (memory->register-transfer offset base target)
  (case (register-type target)
    ((GENERAL)
     (LAP (LDR X ,target (OFFSET ,base ,offset))))
    ((FLOAT)
     (LAP (LDR D ,target (OFFSET ,base ,offset))))
    (else
     (error "Unknown register type:" target))))

(define (register->memory-transfer source offset base)
  (case (register-type source)
    ((GENERAL)
     (LAP (STR X ,source (OFFSET ,base ,offset))))
    ((FLOAT)
     (LAP (STR D ,source (OFFSET ,base ,offset))))
    (else
     (error "Unknown register type:" source))))

;;; References, for machine register allocator.

(define (ea/mode ea) (car ea))

(define (offset-reference register offset)
  (INST-EA (OFFSET ,register ,offset)))

(define (offset-ea? ea)
  (eq? 'OFFSET (ea/mode ea)))

(define (offset-ea/register ea)
  (guarantee offset-ea? ea)
  (cadr ea))

(define (offset-ea/offset ea)
  (guarantee offset-ea? ea)
  (caddr ea))

(define (register-ea? ea)
  (eq? 'R (ea/mode ea)))

(define (register-ea/register ea)
  (guarantee register-ea? ea)
  (cadr ea))

(define (vector-ea? ea)
  (eq? 'V (ea/mode ea)))

(define (vector-ea/register ea)
  (guarantee vector-ea? ea)
  (cadr ea))

;;; Utilities

(define (standard-source! register)
  (if (eq? register 'Z)
      register
      (load-alias-register! register (register-type register))))

(define (standard-target! register)
  (assert (not (eq? register 'Z)))
  (delete-dead-registers!)
  (allocate-alias-register! register (register-type register)))

(define (standard-move-to-temporary! source)
  (if (eq? source 'Z)
      (let ((temp (allocate-temporary-register! 'GENERAL)))
        (prefix-instructions! (LAP (MOVZ X ,temp (&U 0))))
        temp)
      (move-to-temporary-register! source (register-type source))))

(define (assign-register->register target source)
  (move-to-alias-register! source (register-type source) target)
  (LAP))

(define (require-register! machine-reg)
  (flush-register! machine-reg)
  (need-register! machine-reg))

(define (flush-register! machine-reg)
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

(define (register-expression expression)
  (case (rtl:expression-type expression)
    ((REGISTER)
     (rtl:register-number expression))
    ((CONSTANT)
     (let ((object (rtl:constant-value expression)))
       (and (zero? (back-end:object-type object))
            (zero? (back-end:object-datum object))
            'Z)))
    ((MACHINE-CONSTANT)
     (and (zero? (rtl:machine-constant-value expression))
          'Z))
    ((CONS-POINTER)
     (let ((type (rtl:cons-pointer-type expression))
           (datum (rtl:cons-pointer-datum expression)))
       (cond ((rtl:machine-constant? type)
              (and (zero? (rtl:machine-constant-value type))
                   (register-expression datum)))
             ((rtl:machine-constant? datum)
              (and (zero? (rtl:machine-constant-value datum))
                   (register-expression type)))
             (else #f))))
    (else #f)))

(define (standard-unary target source operate)
  (let* ((source (standard-source! source))
         (target (standard-target! target)))
    (operate target source)))

(define (standard-binary target source1 source2 operate)
  (let* ((source1 (standard-source! source1))
         (source2 (standard-source! source2))
         (target (standard-target! target)))
    (operate target source1 source2)))

(define (standard-binary-effect source1 source2 operate)
  (let* ((source1 (standard-source! source1))
         (source2 (standard-source! source2)))
    (operate source1 source2)))

(define (standard-ternary-effect source1 source2 source3 operate)
  (let* ((source1 (standard-source! source1))
         (source2 (standard-source! source2))
         (source3 (standard-source! source3)))
    (operate source1 source2 source3)))

(define (pop register)
  (LAP (LDR X ,register
            (POST+ ,regnum:stack-pointer ,address-units-per-object))))

(define (push register)
  (LAP (STR X ,register
            (PRE- ,regnum:stack-pointer ,address-units-per-object))))

(define (pop2 reg1 reg2)
  ;; (LAP ,@(pop reg1) ,@(pop reg2))
  (LAP (LDRP X ,reg1 ,reg2
             (POST+ ,regnum:stack-pointer
                    ,(* 2 address-units-per-object)))))

(define (push2 reg1 reg2)
  ;; (LAP ,@(push reg2) ,@(push reg1))
  (LAP (STRP X ,reg2 ,reg1
             (PRE- ,regnum:stack-pointer ,(* 2 address-units-per-object)))))

(define (fits-in-unsigned-12? x)
  (<= 0 x #xfff))

(define (fits-in-unsigned-16? x)
  (<= 0 x #xffff))

(define (fits-in-unsigned-32? x)
  (<= 0 x #xffffffff))

(define (fits-in-unsigned-48? x)
  (<= 0 x #xffffffffffff))

;; XXX doesn't belong here

(define-integrable type-code:fixnum #x1a)
(define-integrable type-code:manifest-closure #x0d)

(define (scale->shift scale)
  (case scale
    ((1) 0)
    ((2) 1)
    ((4) 2)
    ((8) 4)
    (else (error "Invalid scale:" scale))))

(define (load-displaced-address target base offset scale)
  (standard-unary target base
    (lambda (target base)
      (add-immediate target base (* offset scale)))))

(define (load-indexed-address target base offset scale)
  (standard-binary target base offset
    (lambda (target base offset)
      (LAP (ADD X ,target ,base (LSL ,offset ,(scale->shift scale)))))))

(define (load-signed-immediate target imm)
  (load-unsigned-immediate target (bitwise-and imm #xffffffffffffffff)))

(define (load-unsigned-immediate target imm)
  (define (try-shift imm shift)
    (and (zero? (bitwise-and imm (bit-mask shift 0)))
         (fits-in-unsigned-16? (shift-right imm shift))
         shift))
  (define (find-shift imm)
    (or (try-shift imm 0)
        (try-shift imm 16)
        (try-shift imm 32)
        (try-shift imm 48)))
  (define (chunk16 pos)
    (bitwise-and (shift-right imm 16) pos))
  (cond ((find-shift imm)
         => (lambda (shift)
              (LAP (MOVZ X ,target (LSL (&U ,imm) ,shift)))))
        ((find-shift (bitwise-not imm))
         => (lambda (shift)
              (LAP (MOVN X ,target (LSL (&U ,(bitwise-not imm)) ,shift)))))
        ((logical-immediate? imm)
         (LAP (ORR X ,target Z (&U ,imm))))
        ;; XXX try splitting in halves, quarters
	#;
        ((let ((lo (extract-bit-field 32 0 imm))
               (hi (extract-bit-field 32 32 imm)))
           (let ((lo-shift (find-shift lo))
                 (hi-shift (find-shift hi)))
             (and lo-shift hi-shift (cons lo-shift hi-shift))))
         => (lambda))
        ((fits-in-unsigned-16? (bitwise-not imm))
         (LAP (MOVN X ,target (&U ,(bitwise-not imm)))))
	(else
	 ;; XXX give up
	 (LAP (MOVZ X ,target (&U ,(chunk16 0)))
	      (MOVK X ,target (LSL (&U ,(chunk16 16)) 16))
	      (MOVK X ,target (LSL (&U ,(chunk16 32)) 32))
	      (MOVK X ,target (LSL (&U ,(chunk16 48)) 48))))))

(define (load-pc-relative-address target label)
  ;; XXX What happens if label is >1 MB away?
  (LAP (ADR X ,target (@PCR ,label))))

(define (load-pc-relative target label)
  (LAP ,@(load-pc-relative-address target label)
       (LDR X ,target ,target)))

(define (load-tagged-immediate target type datum)
  (load-unsigned-immediate target (make-non-pointer-literal type datum)))

(define (load-constant target object)
  (if (non-pointer-object? object)
      (load-unsigned-immediate target (non-pointer->literal object))
      (load-pc-relative target (constant->label object))))

(define (add-immediate target source imm)
  (define (add addend) (LAP (ADD X ,target ,source ,addend)))
  (define (sub addend) (LAP (SUB X ,target ,source ,addend)))
  (immediate-addition imm add sub))

(define (add-immediate-with-flags target source imm)
  (define (adds addend) (LAP (ADDS X ,target ,source ,addend)))
  (define (subs addend) (LAP (SUBS X ,target ,source ,addend)))
  (immediate-addition imm adds subs))

(define (cmp-immediate source imm)
  ;; Same as above but with zero destination.
  (define (cmp operand) (LAP (CMP X ,source ,operand)))
  (define (cmn operand) (LAP (CMN X ,source ,operand)))
  (immediate-addition imm cmp cmn))

(define (immediate-addition imm add sub)
  ;; XXX Use INST-EA instead of quasiquote?  Dunno...
  (cond ((fits-in-unsigned-12? imm)
         (add `(&U ,imm)))
        ((and (zero? (bitwise-and imm (bit-mask 12 0)))
              (fits-in-unsigned-12? (shift-right immediate 12)))
         (add `(&U ,imm LSL 12)))
        ((fits-in-unsigned-12? (- immediate))
         (sub `(&U ,(- immediate))))
        ((and (zero? (bitwise-and imm (bit-mask 12 0)))
              (fits-in-unsigned-12? (shift-right (- immediate) 12)))
         (sub `(&U ,(- immediate) LSL 12)))
        (else
         (let ((temp (allocate-temporary-register! 'GENERAL)))
           (LAP ,@(load-unsigned-immediate temp immediate)
                ,@(add temp))))))

(define (affix-type target type datum)
  ;; Note: This must NOT use regnum:scratch-0 or regnum:scratch-1!
  ;; This is used by closure headers to tag the incoming entry.
  (assert (<= scheme-type-width 16))
  (assert (<= 48 scheme-datum-width))
  (cond ((zero? type)
         (assign-register->register target datum))
        ((logical-immediate? (make-non-pointer-literal type 0))
         ;; Works for tags with only contiguous one bits, including
         ;; tags with only one bit set.
         (LAP (ORR ,target ,datum (&U ,(make-non-pointer-literal type 0)))))
        ((fits-in-unsigned-12?
          (shift-left type (- scheme-datum-width 48)))
         ;; Works for 2-bit tags.
         (let ((imm (shift-left type (- scheme-datum-width 48)))
               (shift 48))
           (LAP (ADD ,target ,datum (LSL (&U ,imm) ,shift)))))
        (else
         ;; Works for all tags up to 16 bits, but costs two
         ;; instructions.
         ;;
         ;; XXX If we know the top few bits of the datum are zero, we
         ;; could use a single MOVK instruction.
         (let ((imm (shift-left type (- 16 scheme-type-width)))
               (shift 48))
           (LAP (MOVZ ,target (LSL (&U ,imm) ,shift))
                (ORR ,target ,target ,datum))))))

(define (object->type target source)
  (let ((lsb scheme-datum-width)
        (width scheme-type-width))
    (LAP (UBFX X ,target ,source (&U ,lsb) (&U ,width)))))

(define (object->datum target source)
  (let ((lsb 0)
        (width scheme-datum-width))
    ;; Alternatively, use BFC to clear the top scheme-type-width bits.
    (LAP (UBFX X ,target ,source (&U ,lsb) (&U ,width)))))

(define (object->address target source)
  (object->datum target source))

;;;; Linearizer interface

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (B (@PCR ,label ,regnum:scratch-0))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

(define (make-external-label type/arity label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (PADDING 32 64 0)
       (EXTERNAL-LABEL ,type/arity ,label)
       (DATA 64 U 0)
       (LABEL ,label)))

(define (make-code-word min max)
  (+ (* #x100 min) max))

(define expression-code-word
  (make-code-word #xff #xff))

;;;; Named registers, codes, and entries

(define reg:memtop
  (offset-reference regnum:regs-pointer
                    register-block/memtop-offset))

(define reg:environment
  (offset-reference regnum:regs-pointer
                    register-block/environment-offset))

(define reg:lexpr-primitive-arity
  (offset-reference regnum:regs-pointer
                    register-block/lexpr-primitive-arity-offset))

(define reg:stack-guard
  (offset-reference regnum:regs-pointer
                    register-block/stack-guard-offset))

(define reg:int-mask
  (offset-reference regnum:regs-pointer
                    register-block/int-mask-offset))

(define reg:int-code
  (offset-reference regnum:regs-pointer
                    register-block/int-code-offset))

(define reg:reflect-to-interface
  (offset-reference regnum:regs-pointer
                    register-block/reflect-to-interface-offset))

(define-syntax define-codes
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
        ,@(let loop ((names (cddr form)) (index (cadr form)))
            (if (pair? names)
                (cons `(DEFINE-INTEGRABLE ,(symbol 'CODE:COMPILER- (car names))
                         ,index)
                      (loop (cdr names) (+ index 1)))
                '()))))))

;; Must match utility_table in cmpint.c.
(define-codes #x012
  primitive-apply
  primitive-lexpr-apply
  apply
  error
  lexpr-apply
  link
  interrupt-closure
  interrupt-dlink
  interrupt-procedure
  interrupt-continuation
  interrupt-ic-procedure
  assignment-trap
  cache-reference-apply
  reference-trap
  safe-reference-trap
  unassigned?-trap
  -1+
  &/
  &=
  &>
  1+
  &<
  &-
  &*
  negative?
  &+
  positive?
  zero?
  access
  lookup
  safe-lookup
  unassigned?
  unbound?
  set!
  define
  lookup-apply
  primitive-error
  quotient
  remainder
  modulo)

(define-syntax define-entries
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
        ,@(let loop ((names (cddr form)) (index (cadr form)))
            (if (pair? names)
                (cons `(DEFINE-INTEGRABLE
                           ,(symbol 'ENTRY:COMPILER- (car names))
                         ,index)
                      (loop (cdr names) (+ index 1)))
                '()))))))

;; Must match aarch64_reset_hook in cmpintmd/aarch64.c.
(define-entries 16
  scheme-to-interface                   ; Main entry point (only one necessary)
  interrupt-procedure
  interrupt-continuation
  interrupt-continuation-2
  interrupt-closure
  interrupt-dlink
  primitive-apply
  primitive-lexpr-apply
  assignment-trap
  reference-trap
  safe-reference-trap
  link
  error
  primitive-error
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
  quotient
  remainder
  modulo
  fixnum-shift
  apply-setup
  apply-setup-size-1
  apply-setup-size-2
  apply-setup-size-3
  apply-setup-size-4
  apply-setup-size-5
  apply-setup-size-6
  apply-setup-size-7
  apply-setup-size-8
  set-interrupt-enables!)

(define-integrable (invoke-hook entry)
  (LAP (LDR X ,regnum:scratch-0 (+ ,regnum:regs-pointer (&U (* 8 ,entry))))
       (BR ,regnum:scratch-0)))

;; Invoke a hook that will return to the address in the link register
;; with RET.  To be used for super-cheap assembly hooks that never fail
;; but are a little too large to copy in every caller.

(define-integrable (invoke-hook/subroutine entry)
  (LAP (LDR X ,regnum:scratch-0 (+ ,regnum:regs-pointer (&U (* 8 ,entry))))
       (BLR ,regnum:scratch-0)))

;; Invoke a hook that expects an untagged compiled return address in
;; the link register, may examine it, and will eventually pop it and
;; return to it with RET.  It is worthwhile to use paired BL/RET here
;; because in the fast path, non-error case, the hook will just return
;; to Scheme; only in error or complicated cases will it return to C.
;; To be used for compiler utilities that are usually cheap but may
;; have error cases and may call back into C.

(define-integrable (invoke-hook/call entry label)
  (LAP ,@(invoke-hook/subroutine entry)
       (B (@PCR ,label ,regnum:scratch-0))))

;; Invoke a hook that expects a compiled entry address as the first
;; utility argument, and will later jump to it with BR.  It is not
;; worthwhile to use paired BL/RET here because the microcode will RET
;; back into C code on the C stack to handle it, which wrecks the
;; return address branch target predictor anyway.  To be used for,
;; e.g., interrupts, which are assumed to be always expensive.

(define-integrable (invoke-hook/reentry entry label)
  (LAP (ADR X ,regnum:utility-arg0 (@PCR ,label ,regnum:scratch-0))
       ,@(invoke-hook entry)))

(define-integrable (invoke-interface code)
  (LAP (MOVZ X ,regnum:utility-index (&U ,code))
       (BR ,regnum:scheme-to-interface)))

(define-integrable (invoke-interface/call code label)
  (LAP (MOVZ X ,regnum:utility-index (&U ,code))
       (BLR ,regnum:scheme-to-interface)
       (B (@PCR ,label ,regnum:scratch-0))))

(define-integrable (invoke-interface/reentry code label)
  (LAP (ADR X ,regnum:utility-arg0 (@PCR ,label ,regnum:scratch-0))
       ,@(invoke-interface code)))

;; Operation tables

(define (define-arithmetic-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
        (set-cdr! entry method)
        (set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-arithmetic-method operator methods)
  (cdr (or (assq operator (cdr methods))
           (error "Unknown operator" operator))))

(define (pre-lapgen-analysis rgraphs)
  (for-each (lambda (rgraph)
              (for-each (lambda (edge)
                          (determine-interrupt-checks (edge-right-node edge)))
                        (rgraph-entry-edges rgraph)))
            rgraphs))

;; XXX

(define (back-end:object-type object)
  (object-type object))

(define (back-end:object-datum object)
  (object-datum object))
