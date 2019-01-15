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

;;;; Machine Model for AArch64
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define (target-fasl-format)
  (case endianness
    ((BIG) fasl-format:aarch64be)
    ((LITTLE) fasl-format:aarch64le)
    (else (error "Unknown endianness:" endianness))))

(define use-pre/post-increment? #t)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 64)
(define-integrable scheme-type-width 6) ;or 8

;; NOTE: expt is not being constant-folded now.
;; For the time being, some of the parameters below are
;; pre-computed and marked with ***
;; There are similar parameters in lapgen.scm
;; Change them if any of the parameters above change.

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable float-width 64)
(define-integrable float-alignment 64)

(define-integrable address-units-per-float
  (quotient float-width addressing-granularity))

;;; It is currently required that both packed characters and objects
;;; be integrable numbers of address units.  Furthermore, the number
;;; of address units per object must be an integral multiple of the
;;; number of address units per character.  This will cause problems
;;; on a machine that is word addressed: we will have to rethink the
;;; character addressing strategy.

(define-integrable address-units-per-object
  (quotient scheme-object-width addressing-granularity))

(define-integrable address-units-per-packed-char 1)

(define-integrable signed-fixnum/upper-limit
  ;; (expt 2 (-1+ scheme-datum-width)) ***
  #x0200000000000000)

(define-integrable signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable unsigned-fixnum/upper-limit
  (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)

;;;; Closure format

;;; See microcode/cmpintmd/aarch64.h for a description of the layout.

(define-integrable closure-entry-size 2) ;units of objects

(define-integrable address-units-per-closure-manifest address-units-per-object)
(define-integrable address-units-per-entry-format-code 4)
(define-integrable address-units-per-closure-entry-count 4)
(define-integrable address-units-per-closure-padding -4)

(define-integrable address-units-per-closure-pc-offset 8)
(define-integrable address-units-per-closure-entry-padding 4)

(define-integrable address-units-per-closure-entry
  (+ address-units-per-entry-format-code
     address-units-per-closure-pc-offset
     address-units-per-closure-entry-padding))

;;; Note:
;;;
;;; (= address-units-per-closure-entry #| 16 |#
;;;    (* closure-entry-size #| 2 |# address-units-per-object #| 8 |#))

;;; Given the number of entries in a closure, and the index of an
;;; entry, return the number of words from that entry's closure
;;; pointer to the location of the storage for the closure's first
;;; free variable.  In this case, the closure pointer is the same as
;;; the compiled entry pointer into the entry instructions.  This is
;;; different from the i386, where the entry instructions are not all
;;; object-aligned, and thus the closure pointer is adjusted to point
;;; to the first entry in the closure block, which is always aligned.
;;;
;;; When there are zero entries, the `closure' is just a vector, and
;;; represented by a tagged pointer to a manifest, following which are
;;; the free variables.  In this case, the first offset is one object
;;; past the manifest's address.

(define (closure-first-offset nentries entry)
  (if (zero? nentries)
      1
      (* (- nentries entry 1) closure-entry-size)))

;;; Given the number of entry points in a closure, return the distance
;;; in objects from the address of the manifest closure to the address
;;; of the first free variable.

(define (closure-object-first-offset nentries)
  (if (zero? nentries)
      1                                 ;One vector manifest.
      ;; One object for the closure manifest, half an object for the
      ;; leading entry count, and minus half an object for the trailing
      ;; non-padding.
      (+ 1 (* nentries closure-entry-size))))

;;; Given the number of entries in a closure, and the indices of two
;;; entries, return the number of bytes separating the two entries.

(define (closure-entry-distance nentries entry entry*)
  nentries                              ;ignore
  (* (- entry* entry) address-units-per-closure-entry))

;;; Given the number of entries in a closure, and the index of an
;;; entry, return the number of bytes to add to a possibly misaligned
;;; closure pointer to obtain a `canonical' entry point, which is
;;; aligned on an object boundary.  Since all closure entry points are
;;; aligned thus on this machine, we need adjust nothing.

(define (closure-environment-adjustment nentries entry)
  nentries entry                        ;ignore
  0)

;;;; Machine registers

;;; 64-bit general purpose registers, variously named Wn or Xn in the
;;; ARM assembler depending on the operand size, 32-bit or 64-bit.
;;; We'll name the operand size separately.
;;;
;;; XXX To allocate: regnum:apply-pc, regnum:apply-target

;; register             Scheme purpose          C purpose
(define-integrable r0 0) ;result, temporary     first argument, result
(define-integrable r1 1) ;temporary, utilarg0   second argument
(define-integrable r2 2) ;temporary, utilarg1   third argument
(define-integrable r3 3) ;temporary, utilarg2   fourth argument
(define-integrable r4 4) ;temporary, utilarg3   fifth argument
(define-integrable r5 5) ;temporary, utilarg4   sixth argument
(define-integrable r6 6) ;temporary, utilarg6   seventh argument
(define-integrable r7 7) ;temporary, utilarg6   eighth argument
(define-integrable r8 8) ;temporary             indirect result location
(define-integrable r9 9) ;temporary             temporary
(define-integrable r10 10) ;temporary           temporary
(define-integrable r11 11) ;temporary           temporary
(define-integrable r12 12) ;temporary           temporary
(define-integrable r13 13) ;temporary           temporary
(define-integrable r14 14) ;temporary           temporary
(define-integrable r15 15) ;temporary           temporary
(define-integrable r16 16) ;temporary,          first PLT scratch register
                           ;  indirect jump callee,
                           ;  scheme-to-interface code
(define-integrable r17 17) ;temporary,          second PLT scratch register
                           ;  indirect jump pc
(define-integrable r18 18) ;reserved            platform ABI register
(define-integrable r19 19) ;interpreter regs    callee-saved
(define-integrable r20 20) ;free pointer        callee-saved
(define-integrable r21 21) ;dynamic link        callee-saved
(define-integrable r22 22) ;memtop (XXX why?)   callee-saved
(define-integrable r23 23) ;scheme-to-interface callee-saved
(define-integrable r24 24) ;temporary           callee-saved
(define-integrable r25 25) ;temporary           callee-saved
(define-integrable r26 26) ;temporary           callee-saved
(define-integrable r27 27) ;temporary           callee-saved
(define-integrable r28 28) ;stack pointer       callee-saved
(define-integrable r29 29) ;C frame pointer     frame pointer
(define-integrable rlr 30) ;link register       link register
(define-integrable rsp 31) ;C stack pointer     stack pointer

;; Note: Register 31 is alternately the stack pointer or the zero
;; register, depending on instruction.

;;; 128-bit vector registers for SIMD or floating-point instructions,
;;; variously called Bn, Hn, Sn, Dn, Qn, Vn.8B, Vn.16B, Vn.4H, Vn.8H,
;;; Vn.2S in the ARM assembler depending on how they are being used.
;;; No special purpose.

(define-integrable v0 32)
(define-integrable v1 33)
(define-integrable v2 34)
(define-integrable v3 35)
(define-integrable v4 36)
(define-integrable v5 37)
(define-integrable v6 38)
(define-integrable v7 39)
(define-integrable v8 40)
(define-integrable v9 41)
(define-integrable v10 42)
(define-integrable v11 43)
(define-integrable v12 44)
(define-integrable v13 45)
(define-integrable v14 46)
(define-integrable v15 47)
(define-integrable v16 48)
(define-integrable v17 49)
(define-integrable v18 50)
(define-integrable v19 51)
(define-integrable v20 52)
(define-integrable v21 53)
(define-integrable v22 54)
(define-integrable v23 55)
(define-integrable v24 56)
(define-integrable v25 57)
(define-integrable v26 58)
(define-integrable v27 59)
(define-integrable v28 60)
(define-integrable v29 61)
(define-integrable v30 62)
(define-integrable v31 63)

(define-integrable number-of-machine-registers 64)
(define-integrable number-of-temporary-registers 256)

;; Draw various fixed-function registers from the callee-saved section,
;; so we don't have to worry about saving and restoring them ourselves
;; in the transition to and from C.

(define-integrable regnum:value-register r0)
(define-integrable regnum:utility-arg0 r1)
(define-integrable regnum:utility-arg1 r2)
(define-integrable regnum:utility-arg2 r3)
(define-integrable regnum:utility-arg3 r4)
(define-integrable regnum:utility-arg4 r5)
(define-integrable regnum:utility-arg5 r6)
(define-integrable regnum:utility-arg6 r7)
(define-integrable regnum:scratch-0 r16)
(define-integrable regnum:scratch-1 r17)
(define-integrable regnum:regs-pointer r19)
(define-integrable regnum:free-pointer r20)
(define-integrable regnum:dynamic-link r21) ;Pointer to parent stack frame.
;; (define-integrable regnum:memtop r22)
(define-integrable regnum:scheme-to-interface r23)
(define-integrable regnum:stack-pointer r27)
(define-integrable regnum:c-frame-pointer r29)
(define-integrable regnum:link-register rlr) ;Return address.
(define-integrable regnum:c-stack-pointer rsp)

;; XXX Maybe we're playing a dangerous game to use the scratch registers for
;; these.
(define-integrable regnum:apply-target regnum:scratch-0)
(define-integrable regnum:apply-pc regnum:scratch-1)
(define-integrable regnum:utility-index regnum:scratch-1)

(define-integrable (machine-register-known-value register)
  register                              ;ignore
  #f)

(define machine-register-value-class
  (let ((classes (make-vector 64)))
    ;; Fill in defaults.
    (do ((i 0 (+ i 1)))
        ((>= i 32))
      (vector-set! classes i value-class=object))
    (do ((i 32 (+ i 1)))
        ((>= i 64))
      (vector-set! classes i value-class=float))
    (vector-set! classes regnum:scratch-0 value-class=unboxed)
    (vector-set! classes regnum:scratch-1 value-class=unboxed)
    (vector-set! classes regnum:regs-pointer value-class=address)
    (vector-set! classes regnum:free-pointer value-class=address)
    (vector-set! classes regnum:dynamic-link value-class=address)
    ;; (vector-set! classes regnum:memtop value-class=address)
    (vector-set! classes regnum:stack-pointer value-class=address)
    (vector-set! classes regnum:c-frame-pointer value-class=address)
    (vector-set! classes regnum:link-register value-class=address)
    (vector-set! classes regnum:c-stack-pointer value-class=address)
    (named-lambda (machine-register-value-class register)
      (assert (<= 0 register))
      (assert (< register number-of-machine-registers))
      (vector-ref classes register))))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
(define-integrable register-block/stack-guard-offset 11)
(define-integrable register-block/int-code-offset 12)
(define-integrable register-block/reflect-to-interface-offset 13)

(define-integrable (interpreter-value-register)
  (rtl:make-machine-register regnum:value-register))

(define (interpreter-value-register? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:value-register)))

(define-integrable (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:regs-pointer))

(define (interpreter-regs-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:regs-pointer)))

(define-integrable (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free-pointer))

(define (interpreter-free-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:free-pointer)))

(define-integrable (interpreter-dynamic-link)
  (rtl:make-machine-register regnum:dynamic-link))

(define (interpreter-dynamic-link? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:dynamic-link)))

(define-integrable (interpreter-stack-pointer)
  (rtl:make-machine-register regnum:stack-pointer))

(define (interpreter-stack-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:stack-pointer)))

(define (interpreter-register:access)
  (rtl:make-machine-register r0))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register r0))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register r0))

(define (interpreter-register:lookup)
  (rtl:make-machine-register r0))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register r0))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register r0))

(define (rtl:machine-register? register-name)
  (case register-name
    ((DYNAMIC-LINK) (interpreter-dynamic-link))
    ((FREE) (interpreter-free-pointer))
    ;; ((MEMORY-TOP) (rtl:make-machine-register regnum:memtop))
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((VALUE) (interpreter-value-register))
    ((INTERPRETER-CALL-RESULT:ACCESS)
     (interpreter-register:access))
    ((INTERPRETER-CALL-RESULT:CACHE-REFERENCE)
     (interpreter-register:cache-reference))
    ((INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?)
     (interpreter-register:cache-unassigned?))
    ((INTERPRETER-CALL-RESULT:LOOKUP)
     (interpreter-register:lookup))
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?)
     (interpreter-register:unassigned?))
    ((INTERPRETER-CALL-RESULT:UNBOUND?)
     (interpreter-register:unbound?))
    (else #f)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((INT-MASK) register-block/int-mask-offset)
    ((ENVIRONMENT) register-block/environment-offset)
    ((MEMORY-TOP) register-block/memtop-offset)
    (else #f)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown interpreter register:" locative)))

(define (rtl:constant-cost expression)
  ;; Register reference costs 1.  If a cost is less than 1, CSE will
  ;; always choose the constant over the register reference, which may
  ;; not always work and is not always helpful even when it does work.
  ;;
  ;; XXX Justify this by reference to cycle counts, &c.  This really
  ;; depends on which instruction we're talking about -- sometimes
  ;; immediates are cheaper.
  (let ((cost:zero 1)
        (cost:imm16 2)                  ;MOVZ/MOVN
        (cost:imm32 3)                  ;MOVZ/MOVN + 1*MOVK
        (cost:imm48 4)                  ;MOVZ/MOVN + 2*MOVK
        (cost:imm64 5)                  ;MOVZ/MOVN + 3*MOVK
        (cost:add 2)
        (cost:adr 2)
        (cost:ldr 10)
        (cost:bl 3))
    (define (immediate-cost immediate)
      (cond ((zero? immediate)
             cost:zero)
            ((or (fits-in-unsigned-16? immediate)
                 (fits-in-unsigned-16? (- immediate)))
             cost:imm16)
            ((or (fits-in-unsigned-32? immediate)
                 (fits-in-unsigned-32? (- immediate)))
             cost:imm32)
            ;; XXX logical immediate
            ((or (fits-in-unsigned-48? immediate)
                 (fits-in-unsigned-48? (- immediate)))
             cost:imm48)
            (else
             cost:imm64)))
    (define (tagged-immediate-cost tag datum)
      (immediate-cost (make-non-pointer-literal tag datum)))
    (define (load-pc-relative-address-cost)
      cost:adr)
    (define (load-pc-relative-cost)
      (+ (load-pc-relative-address-cost) cost:ldr))
    (define (branch-and-link-cost)
      cost:bl)
    (define (offset-cost base offset scale)
      (let ((base-cost (rtl:expression-cost base)))
        (and base-cost
             (+ base-cost
                (if (rtl:machine-constant? offset)
                    (let ((offset
                           (abs
                            (* scale (rtl:machine-constant-value offset)))))
                      (cond ((or (fits-in-unsigned-12? (abs offset))
                                 (and (zero?
                                       (remainder (abs offset) (expt 2 12)))
                                      (fits-in-unsigned-12?
                                       (quotient (abs offset) (expt 2 12)))))
                             cost:add)
                            (else
                             (+ (immediate-cost offset)
                                cost:add))))
                    cost:add)))))

    (case (rtl:expression-type expression)
      ((MACHINE-CONSTANT)
       (immediate-cost (rtl:machine-constant-value expression)))
      ((CONSTANT)
       (let ((value (rtl:constant-value expression)))
         (if (non-pointer-object? value)
             (immediate-cost (non-pointer->literal value))
             (load-pc-relative-cost))))
      ((ENTRY:PROCEDURE)
       (load-pc-relative-address-cost))
      ((ENTRY:CONTINUATION)
       (branch-and-link-cost))
      ((VARIABLE-CACHE ASSIGNMENT-CACHE)
       (load-pc-relative-cost))
      ((OFFSET-ADDRESS)
       (offset-cost (rtl:offset-address-base expression)
                    (rtl:offset-address-offset expression)
                    address-units-per-object))
      ((BYTE-OFFSET-ADDRESS)
       (offset-cost (rtl:byte-offset-address-base expression)
                    (rtl:byte-offset-address-offset expression)
                    1))
      ((FLOAT-OFFSET-ADDRESS)
       (offset-cost (rtl:float-offset-address-base expression)
                    (rtl:float-offset-address-offset expression)
                    address-units-per-float))
      ((CONS-POINTER)
       (let ((type (rtl:cons-pointer-type expression))
             (datum (rtl:cons-pointer-datum expression)))
         (and (rtl:machine-constant? type)
              (rtl:machine-constant? datum)
              (let ((type (rtl:machine-constant-value type))
                    (datum (rtl:machine-constant-value datum)))
                (tagged-immediate-cost type datum)))))
      (else #f))))

(define (fits-in-signed-9? x)
  (<= #x-100 x #xff))

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
(define-integrable type-code:manifest-vector #x00)

;; XXX

(define (non-pointer->literal object)
  (make-non-pointer-literal (back-end:object-type object)
			    (back-end:object-datum object)))

(define (back-end:object-type object)
  (object-type object))

(define (back-end:object-datum object)
  (object-datum object))

(define compiler:open-code-floating-point-arithmetic?
  ;; XXX not yet
  #f)

(define compiler:primitives-with-no-open-coding
  ;; XXX Should really make this a whitelist, not a blacklist.
  '(
    &/                  ;nobody open-codes this
    DIVIDE-FIXNUM       ;nobody open-codes this
    FIXNUM-LSH          ;open-coding not useful without constant operands
    FLOATING-VECTOR-CONS;nobody open-codes this
    FLOATING-VECTOR-REF ;no flonum arithmetic yet
    FLOATING-VECTOR-SET!;no flonum arithmetic yet
    FLONUM-ABS          ;no flonum arithmetic yet
    FLONUM-ACOS         ;not useful to open-code hairy math
    FLONUM-ADD          ;no flonum arithmetic yet
    FLONUM-ASIN         ;not useful to open-code hairy math
    FLONUM-ATAN         ;not useful to open-code hairy math
    FLONUM-ATAN2        ;not useful to open-code hairy math
    FLONUM-CEILING      ;no flonum arithmetic yet
    FLONUM-COS          ;not useful to open-code hairy math
    FLONUM-DIVIDE       ;no flonum arithmetic yet
    FLONUM-EXP          ;not useful to open-code hairy math
    FLONUM-EXPM1        ;not useful to open-code hairy math
    FLONUM-FLOOR        ;no flonum arithmetic yet
    FLONUM-LOG          ;not useful to open-code hairy math
    FLONUM-LOG1P        ;not useful to open-code hairy math
    FLONUM-MULTIPLY     ;no flonum arithmetic yet
    FLONUM-NEGATE       ;no flonum arithmetic yet
    FLONUM-ROUND        ;no flonum arithmetic yet
    FLONUM-SIN          ;not useful to open-code hairy math
    FLONUM-SQRT         ;no flonum arithmetic yet
    FLONUM-SUBTRACT     ;no flonum arithmetic yet
    FLONUM-TAN          ;not useful to open-code hairy math
    FLONUM-TRUNCATE     ;no flonum arithmetic yet
    GCD-FIXNUM          ;nobody open-codes this
    VECTOR-CONS         ;nobody open-codes this
    ))
