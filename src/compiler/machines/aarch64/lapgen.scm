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
   r22 ;XXX memtop?
   ;r23 - scheme-to-interface
   r24
   r25
   r26
   r27
   ;r28 - Scheme stack pointer, not r31 so we can use CMP for interrupt checks
   ;r29 - C frame pointer, callee-saved and left alone by Scheme
   ;r30 - link register (could maybe allocate)
   ;r31 - C stack pointer or zero register, depending on instruction
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

;;; References, for machine register allocator.  Not used by LAP
;;; syntax.

;; Following assumes objects and floats have the same indexing.
(assert (= scheme-object-width float-width 64))

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

(define (pseudo-register-home register)
  (let ((number (register-renumber register)))
    (assert number)
    (INST-EA (HOME ,number))))

(define (ea/mode ea) (car ea))

(define (home-ea? ea)
  (eq? 'HOME (ea/mode ea)))

(define (home-ea/index ea)
  (guarantee home-ea? ea)
  (cadr ea))

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
             (LAP (ORR X ,target Z ,source))))
        ((FLOAT)
         (LAP (FMOV D ,target ,source)))
        (else
         (error "Unknown register type:" source target)))))

(define (spill-ea index)
  ;; XXX fix register block indexing
  (regblock-ea (+ 16 index)))

(define (home->register-transfer register alias)
  (load-register alias (spill-ea (register-renumber register))))

(define (register->home-transfer alias register)
  (store-register alias (spill-ea (register-renumber register))))

(define (reference->register-transfer source target)
  (case (ea/mode source)
    ((R) (register->register-transfer (register-ea/register source) target))
    ((V) (register->register-transfer (vector-ea/register source) target))
    ((HOME) (load-register target (spill-ea (home-ea/index source))))
    (else
     (error "Unknown effective address mode:" source target))))

(define (load-register register ea)
  (case (register-type register)
    ((GENERAL) (LAP (LDR X ,register ,ea)))
    ((FLOAT) (LAP (LDR D ,register ,ea)))
    (else (error "Unknown register type:" register))))

(define (store-register register ea)
  (case (register-type register)
    ((GENERAL) (LAP (STR X ,register ,ea)))
    ((FLOAT) (LAP (STR D ,register ,ea)))
    (else (error "Unknown register type:" register))))

;;; Utilities

(define (standard-source! register)
  (if (eq? register 'Z)
      register
      (load-alias-register! register (register-type register))))

(define (standard-target! register)
  (assert (not (eq? register 'Z)))
  (delete-dead-registers!)
  (allocate-alias-register! register (register-type register)))

(define (general-temporary!)
  (allocate-temporary-register! 'GENERAL))

(define (standard-move-to-temporary! source)
  (if (eq? source 'Z)
      ;; XXX What about float?  Should maybe rename this to
      ;; GENERAL-MOVE-TO-TEMPORARY!.
      (let ((temp (general-temporary!)))
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
            (POST+ ,regnum:stack-pointer
		   (& ,(* address-units-per-object 1))))))

(define (push register)
  (LAP (STR X ,register
            (PRE+ ,regnum:stack-pointer
		  (& ,(* address-units-per-object -1))))))

(define (pop2 reg1 reg2)
  ;; (LAP ,@(pop reg1) ,@(pop reg2))
  (LAP (LDP X ,reg1 ,reg2
	    (POST+ ,regnum:stack-pointer
		   (& (* ,address-units-per-object 2))))))

(define (push2 reg1 reg2)
  ;; (LAP ,@(push reg1) ,@(push reg2))
  (LAP (STP X ,reg2 ,reg1
	    (PRE+ ,regnum:stack-pointer
		  (& (* ,address-units-per-object -2))))))

(define (scale->shift scale)
  (case scale
    ((1) 0)
    ((2) 1)
    ((4) 2)
    ((8) 3)
    (else (error "Invalid scale:" scale))))

(define (load-displaced-address target base offset scale)
  (standard-unary target base
    (lambda (target base)
      (add-immediate target base (* offset scale) general-temporary!))))

(define (load-indexed-address target base offset scale)
  (standard-binary target base offset
    (lambda (target base offset)
      (LAP (ADD X ,target ,base (LSL ,offset ,(scale->shift scale)))))))

(define (load-pc-relative-address target label)
  (LAP (ADR X ,target (@PCR ,label ,regnum:scratch-0))))

(define (load-pc-relative target label)
  (LAP ,@(load-pc-relative-address target label)
       (LDR X ,target ,target)))

(define (load-tagged-immediate target type datum)
  (load-unsigned-immediate target (make-non-pointer-literal type datum)))

(define (load-constant target object)
  (if (non-pointer-object? object)
      (load-unsigned-immediate target (non-pointer->literal object))
      (load-pc-relative target (constant->label object))))

(define (load-signed-immediate target imm)
  (assert (<= (bit-antimask 63 0) imm (bit-mask 63 0)))
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
  (define (mov-chunk16 pos)
    (let ((chunk (bitwise-and (shift-right imm pos) #xffff))
          (below (bitwise-and imm (bit-mask pos 0))))
      (cond ((zero? chunk) (LAP))
            ((zero? pos) (LAP (MOVZ X ,target (&U ,chunk))))
            ((zero? below) (LAP (MOVZ X ,target (LSL (&U ,chunk) ,pos))))
            (else (LAP (MOVK X ,target (LSL (&U ,chunk) ,pos)))))))
  (assert (<= 0 imm (bit-mask 64 0)))
  (cond ((find-shift imm)
         => (lambda (shift)
              (LAP (MOVZ X ,target
                         (LSL (&U ,(shift-right imm shift)) ,shift)))))
        ((find-shift (bitwise-andc1 imm #xffffffffffffffff))
         => (lambda (shift)
              (let ((imm (bitwise-andc1 imm #xffffffffffffffff)))
                (LAP (MOVN X ,target
                           (LSL (&U ,(shift-right imm shift)) ,shift))))))
        ((logical-imm-u64 imm)
         (LAP (ORR X ,target Z (&U ,imm))))
        ;; XXX try splitting in halves, quarters
        #;
        ((let ((lo (extract-bit-field 32 0 imm))
               (hi (extract-bit-field 32 32 imm)))
           (let ((lo-shift (find-shift lo))
                 (hi-shift (find-shift hi)))
             (and lo-shift hi-shift (cons lo-shift hi-shift))))
         => (lambda))
        (else
         ;; XXX give up
         (LAP ,@(mov-chunk16 0)
              ,@(mov-chunk16 16)
              ,@(mov-chunk16 32)
              ,@(mov-chunk16 48)))))

(define (add-immediate target source imm get-temporary)
  (define (add addend) (LAP (ADD X ,target ,source ,addend)))
  (define (sub addend) (LAP (SUB X ,target ,source ,addend)))
  (immediate-addition imm add sub get-temporary))

(define (add-immediate-with-flags target source imm get-temporary)
  (define (adds addend) (LAP (ADDS X ,target ,source ,addend)))
  (define (subs addend) (LAP (SUBS X ,target ,source ,addend)))
  (immediate-addition imm adds subs get-temporary))

(define (cmp-immediate source imm get-temporary)
  ;; Same as above but with zero destination.
  (define (cmp operand) (LAP (CMP X ,source ,operand)))
  (define (cmn operand) (LAP (CMN X ,source ,operand)))
  (immediate-addition imm cmp cmn get-temporary))

(define (immediate-addition imm add sub get-temporary)
  ;; XXX Use INST-EA instead of quasiquote?  Dunno...
  (cond ((fits-in-unsigned-12? imm)
         (add `(&U ,imm)))
        ((and (zero? (bitwise-and imm (bit-mask 12 0)))
              (fits-in-unsigned-12? (shift-right imm 12)))
         (add `(LSL (&U ,imm) 12)))
        ((fits-in-unsigned-12? (- imm))
         (sub `(&U ,(- imm))))
        ((and (zero? (bitwise-and (- imm) (bit-mask 12 0)))
              (fits-in-unsigned-12? (shift-right (- imm) 12)))
         (sub `(LSL (&U ,(- imm)) 12)))
        (else
         (let ((temp (get-temporary)))
           (LAP ,@(load-unsigned-immediate temp imm)
                ,@(add temp))))))

(define (affix-type target type datum get-temporary)
  (assert (<= scheme-type-width 16))
  (assert (<= 48 scheme-datum-width))
  (cond ((zero? type)
         (assign-register->register target datum))
        ((logical-imm-u64 (make-non-pointer-literal type 0))
         ;; Works for tags with only contiguous one bits, including
         ;; tags with only one bit set.
         (LAP (ORR X ,target ,datum (&U ,(make-non-pointer-literal type 0)))))
        (else
         ;; Works for all tags up to 16 bits, but costs two
         ;; instructions.
         ;;
         ;; XXX If we know the top few bits of the datum are zero, we
         ;; could use a single MOVK instruction.
         (let ((temp (if (= target datum) (get-temporary) target))
               (imm (shift-left type (- 16 scheme-type-width)))
               (shift 48))
           (LAP (MOVZ X ,temp (LSL (&U ,imm) ,shift))
                (ORR X ,target ,temp ,datum))))))

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

(define entry-padding-bit-string
  (unsigned-integer->bit-string 32 0))

(define (make-external-label type/arity label)
  (set! *external-labels* (cons label *external-labels*))
  ;; Pad to 4 modulo 8 bytes so the PC offset is 8-byte-aligned.
  (LAP (PADDING 4 8 ,entry-padding-bit-string)
       (EXTERNAL-LABEL ,type/arity ,label)
       (DATA 64 U 0)                    ;PC offset
       (LABEL ,label)))

(define (make-code-word min max)
  (+ (* #x100 min) max))

(define expression-code-word
  (make-code-word #xff #xff))

;;;; Named registers, codes, and entries

(define (regblock-ea offset)
  ;; LDR/STR operand.
  (INST-EA (+ ,regnum:regs-pointer (&U (* 8 ,offset)))))

(define reg:memtop (regblock-ea register-block/memtop-offset))
(define reg:environment (regblock-ea register-block/environment-offset))
(define reg:lexpr-primitive-arity
  (regblock-ea register-block/lexpr-primitive-arity-offset))
(define reg:stack-guard (regblock-ea register-block/stack-guard-offset))
(define reg:int-mask (regblock-ea register-block/int-mask-offset))
(define reg:int-code (regblock-ea register-block/int-code-offset))
(define reg:reflect-to-interface
  (regblock-ea register-block/reflect-to-interface-offset))

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
  primitive-apply                       ;12
  primitive-lexpr-apply                 ;13
  apply                                 ;14
  error                                 ;15
  lexpr-apply                           ;16
  link                                  ;17
  interrupt-closure                     ;18
  interrupt-dlink                       ;19
  interrupt-procedure                   ;1a
  interrupt-continuation                ;1b
  interrupt-ic-procedure                ;1c
  assignment-trap                       ;1d
  cache-reference-apply                 ;1e
  reference-trap                        ;1f
  safe-reference-trap                   ;20
  unassigned?-trap                      ;21
  -1+                                   ;22
  &/                                    ;23
  &=                                    ;24
  &>                                    ;25
  1+                                    ;26
  &<                                    ;27
  &-                                    ;28
  &*                                    ;29
  negative?                             ;2a
  &+                                    ;2b
  positive?                             ;2c
  zero?                                 ;2d
  access                                ;2e (obsolete)
  lookup                                ;2f (obsolete)
  safe-lookup                           ;30 (obsolete)
  unassigned?                           ;31 (obsolete)
  unbound?                              ;32 (obsolete)
  set!                                  ;33 (obsolete)
  define                                ;34 (obsolete)
  lookup-apply                          ;35 (obsolete)
  primitive-error                       ;36
  quotient                              ;37
  remainder                             ;38
  modulo                                ;39
  reflect-to-interface                  ;3a
  interrupt-continuation-2              ;3b
  compiled-code-bkpt                    ;3c
  compiled-closure-bkpt                 ;3d
  )

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

;; Must match hooks in cmpauxmd/aarch64.m4.
(define-entries 0
  scheme-to-interface                   ;00 Main entry point (only necessary)
  &+                                    ;01
  &-                                    ;02
  &*                                    ;03
  &/                                    ;04
  &=                                    ;05
  &<                                    ;06
  &>                                    ;07
  1+                                    ;08
  -1+                                   ;09
  zero?                                 ;0a
  positive?                             ;0b
  negative?                             ;0c
  quotient                              ;0d
  remainder                             ;0e
  modulo                                ;0f
  fixnum-shift                          ;10
  apply-setup                           ;11
  apply-setup-size-1                    ;12
  apply-setup-size-2                    ;13
  apply-setup-size-3                    ;14
  apply-setup-size-4                    ;15
  apply-setup-size-5                    ;16
  apply-setup-size-6                    ;17
  apply-setup-size-7                    ;18
  apply-setup-size-8                    ;19
  set-interrupt-enables!                ;1a
  )

;; Jump to an assembly hook.  No link register setup or anything.  May
;; clobber r16, r17.

(define-integrable (hook-offset entry)
  ;; Four instructions per hook, four bytes per instruction.
  (* 16 entry))

(define (invoke-hook entry)
  (if (zero? entry)                     ;scheme-to-interface
      (LAP (BR ,regnum:hooks))
      (LAP (ADD X ,regnum:scratch-0 ,regnum:hooks (&U ,(hook-offset entry)))
           (BR ,regnum:scratch-0))))

;; Invoke a hook that will return to the address in the link register
;; with RET.  To be used for super-cheap assembly hooks that never fail
;; but are a little too large to copy in every caller.

(define-integrable (invoke-hook/subroutine entry)
  (LAP (ADD X ,regnum:scratch-0 ,regnum:hooks (&U ,(hook-offset entry)))
       (BLR ,regnum:scratch-0)))

;; Invoke a hook that expects an untagged compiled return address in
;; the link register, may examine it, and will eventually pop it and
;; return to it with RET.  It is worthwhile to use paired BL/RET here
;; because in the fast path, non-error case, the hook will just return
;; to Scheme; only in error or complicated cases will it return to C.
;; To be used for compiler utilities that are usually cheap but may
;; have error cases and may call back into C.

(define-integrable (invoke-hook/call entry continuation)
  (LAP ,@(invoke-hook/subroutine entry)
       (B (@PCR ,continuation ,regnum:scratch-0))))

;; Invoke a hook that expects a compiled entry address as the first
;; utility argument, and will later jump to it with BR.  It is not
;; worthwhile to use paired BL/RET here because the microcode will RET
;; back into C code on the C stack to handle it, which wrecks the
;; return address branch target predictor anyway.  To be used for,
;; e.g., interrupts, which are assumed to be always expensive.

(define-integrable (invoke-hook/reentry entry label)
  (LAP (ADR X ,regnum:utility-arg1 (@PCR ,label ,regnum:scratch-0))
       ,@(invoke-hook entry)))

(define-integrable (invoke-interface code)
  (LAP (MOVZ X ,regnum:utility-index (&U ,code))
       ,@(invoke-hook entry:compiler-scheme-to-interface)))

(define (invoke-interface/shared name code)
  (share-instruction-sequence! name
    (lambda (label) (LAP (B (@PCR ,label ,regnum:scratch-0))))
    (lambda (label)
      (LAP (LABEL ,label)
           ,@(invoke-interface code)))))

;; If this assumption is violated, then the definition below is silly
;; and should be replaced.
(assert (not (= rlr regnum:utility-arg1)))

(define (invoke-interface/call code continuation)
  (define (invoke subroutine)
    (LAP (MOVZ X ,regnum:utility-index (&U ,code))
         (BL (@PCR ,subroutine ,regnum:scratch-0))
         (B (@PCR ,continuation ,regnum:scratch-0))))
  (share-instruction-sequence! 'SCHEME-TO-INTERFACE/CALL
    (lambda (subroutine) (invoke subroutine))
    (lambda (subroutine)
      (LAP ,@(invoke subroutine)
           (LABEL ,subroutine)
           ,@(register->register-transfer rlr regnum:utility-arg1)
           ,@(invoke-hook entry:compiler-scheme-to-interface)))))

(define-integrable (invoke-interface/reentry code label)
  (LAP (ADR X ,regnum:utility-arg1 (@PCR ,label ,regnum:scratch-0))
       ,@(invoke-interface code)))

(define (invoke-interface/shared-reentry name code label)
  (LAP (ADR X ,regnum:utility-arg1 (@PCR ,label ,regnum:scratch-0))
       ,@(share-instruction-sequence! name
           (lambda (label)
             (LAP (B (@PCR ,label ,regnum:scratch-0))))
           (lambda (label)
             (LAP (LABEL ,label)
                  ,@(invoke-interface code))))))

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
