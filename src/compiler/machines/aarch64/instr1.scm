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

;;;; AArch64 Instruction Set, part 1
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;; XXX Syntax notes:
;;;
;;; - Should shifted immediates be (* 8 (&U ...)), (&U (* 8 ...)), (LSL
;;;   (&U ...) 3), or (&U (LSL ... 3))?

(define-instruction EXTERNAL-LABEL
  (((? type/arity unsigned-16) (? label))
   (BITS (16 label BLOCK-OFFSET)
         (16 type/arity))))

(define-instruction DATA
  ((32 S (? value))
   (BITS (32 value SIGNED)))
  ((32 U (? value))
   (BITS (32 value UNSIGNED)))
  ((64 S (? value))
   (BITS (64 value SIGNED)))
  ((64 U (? value))
   (BITS (64 value UNSIGNED))))

;;;; Instructions, ordered by sections in ARMv8-A ARM, C3

;;; C3.1.1 Conditional branch

(define-instruction B.
  (((? condition branch-condition) (@PCO (* 4 (? offset signed-19))))
   (BITS (7 #b0101010)
         (1 0)                          ;o1
         (19 offset SIGNED)
         (1 0)                          ;o0
         (4 condition)))
  (((? condition) (@PCR (? target) (? temp register<31)))
   (VARIABLE-WIDTH offset `(/ (- ,target *PC*) 4)
     ;; If it fits in a signed 19-bit displacement, great.
     ((#x-40000 #x3ffff)
      (MACRO 32 (B. ,condition (@PCO (* 4 ,offset)))))
     ;; If not, we have to use ADRP and ADD with a temporary register.
     ;; Preserve forward or backward branches to preserve static branch
     ;; predictions.  The PC relative to which we compute the target
     ;; address is marked with (*) to explain the curious bounds.
     ((0 #x100000001)
      ;; Forward branch.
      (MACRO 32 (B. ,condition (@PCO (* 4 2)))) ;1f
      (MACRO 32 (B (@PCO (* 4 4))))             ;2f
      ;; 1:
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 (- offset 2))))) ;(*)
      (MACRO 32 (BR ,temp))
      ;; 2:
      )
     ((#x-fffffffe -1)
      ;; Backward branch.
      (MACRO 32 (B (@PCO (* 4 4))))             ;1f
      ;; 2:
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 (- offset 2))))) ;(*)
      (MACRO 32 (BR ,temp))
      ;; 1:
      (MACRO 32 (B. ,condition (@PCO (* 4 -3)))) ;2b
      ))))

(let-syntax
    ((define-compare&branch-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rt register-31=z)
                (@PCO (* 4 (? offset signed-19))))
               (BITS (1 sf)
                     (6 #b011010)
                     (1 ,op)
                     (19 offset SIGNED)
                     (5 Rt)))
              (((? sf) (? Rt) (@PCR (? target) (? temp register<31)))
               (VARIABLE-WIDTH offset `(/ (- ,target *PC*) 4)
                 ((#x-40000 #x3ffff)
                  (MACRO 32 (,mnemonic ,',sf ,',Rt (@PCO (* 4 ,',offset)))))
                 ((0 #x40000001)
                  ;; Forward branch.
                  (MACRO 32 (,mnemonic ,',sf ,',Rt (@PCO (* 4 2)))) ;1f
                  (MACRO 32 (B (@PCO (* 4 4)))) ;2f
                  ;; 1:
                  (MACRO 64 (ADRP-ADD X ,',temp
                                      (@PCO ,',(* 4 (- offset 2))))) ;(*)
                  (MACRO 32 (BR ,',temp))
                  ;; 2:
                  )
                 ((#x-3ffffffe -1)
                  ;; Backward branch.
                  (MACRO 32 (B (@PCO (* 4 4)))) ;1f
                  ;; 2:
                  (MACRO 64 (ADRP-ADD X ,',temp
                                      (@PCO ,',(* 4 (- offset 2))))) ;(*)
                  (MACRO 32 (BR ,',temp))
                  ;; 1:
                  (MACRO 32 (,mnemonic ,',sf ,',Rt (@PCO (* 4 -3)))) ;2b
                  )))))))))
  ;; Compare and branch on zero
  (define-compare&branch-instruction CBZ 0)
  ;; Compare and branch on nonzero
  (define-compare&branch-instruction CBNZ 1))

(let-syntax
    ((define-test&branch-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ((W (? Rt register-31=z)
                  (&U (? bit unsigned-5))
                  (@PCO (* 4 (? offset))))
               (BITS (1 0)              ;b5, fifth bit of bit index
                     (6 #b011011)
                     (1 ,op)
                     (5 bit)
                     (14 offset)
                     (5 Rt)))
              ((X (? Rt register-31=z)
                  (&U (? bit unsigned-6))
                  (@PCO (* 4 (? offset))))
               (BITS (1 (shift-right bit 5))
                     (6 #b011011)
                     (1 ,op)
                     (5 (bitwise-and bit #b11111))
                     (14 offset)
                     (5 Rt)))
              (((? sf)
                (? Rt)
                (&U (? bit))
                (@PCR (? target) (? temp register<31)))
               (VARIABLE-WIDTH offset `(/ (- ,target *PC*) 4)
                 ((#x-2000 #x1fff)
                  (MACRO 32
                         (,mnemonic ,',sf ,',Rt (&U ,',bit)
                                    (@PCO (* 4 ,',offset)))))
                 ((0 #x100000001)
                  ;; Forward branch.
                  (MACRO 32 (,mnemonic ,',sf ,',Rt (&U ,',bit)
                                       (@PCO (* 4 2)))) ;1f
                  (MACRO 32 (B (@PCO 4))) ;2f
                  ;; 1:
                  (MACRO 64 (ADRP-ADD X ,',temp
                                      (@PCO ,',(* 4 (- offset 2))))) ;(*)
                  (MACRO 32 (BR ,',temp))
                  ;; 2:
                  )
                 ((#x-fffffffe -1)
                  ;; Backward branch.
                  (MACRO 32 (B (@PCO (* 4 4)))) ;1f
                  ;; 2:
                  (MACRO 64 (ADRP-ADD X ,',temp
                                      (@PCO ,',(* 4 (- offset 2))))) ;(*)
                  (MACRO 32 (BR ,',temp))
                  ;; 1:
                  (MACRO 32 (,mnemonic ,',sf ,',Rt (@PCO (* 4 -3)))) ;2b
                  )))))))))
  ;; Test and branch if zero
  (define-test&branch-instruction TBZ 0)
  ;; Test and branch if nonzero
  (define-test&branch-instruction TBNZ 1))

;;; C3.1.2 Unconditional branch (immediate)

;; Branch unconditional to PC-relative.

(define-instruction B
  (((@PCO (* 4 (? offset signed-26))))
   (BITS (1 0)                          ;no link
         (5 #b00101)
         (26 offset SIGNED)))
  (((@PCR (? target) (? temp register<31)))
   (VARIABLE-WIDTH offset `(/ (- ,target *PC*) 4)
     ((#x-2000000 #x1ffffff)
      (MACRO 32 (B (@PCO (* 4 ,offset)))))
     ((#x-100000000 #xffffffff)
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
      (MACRO 32 (BR ,temp)))))
  (((@PCR (+ (? target) (* 4 (? addend))) (? temp register<31)))
   (VARIABLE-WIDTH offset `(+ (/ (- ,target *PC*) 4) ,addend)
     ((#x-2000000 #x1ffffff)
      (MACRO 32 (B (@PCO (* 4 ,offset)))))
     ((#x-100000000 #xffffffff)
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
      (MACRO 32 (BR ,temp))))))

;; Branch and link unconditional to PC-relative

(define-instruction BL
  (((@PCO (* 4 (? offset))))
   (BITS (1 1)                          ;link
         (5 #b00101)
         (26 offset SIGNED)))
  (((@PCR (? target) (? temp register<31)))
   (VARIABLE-WIDTH offset `(/ (- ,target *PC*) 4)
     ((#x-2000000 #x1ffffff)
      (MACRO 32 (BL (@PCO (* 4 ,offset)))))
     ((#x-100000000 #xffffffff)
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
      (MACRO 32 (BLR ,temp))))))

;;; C.3.1.3 Unconditional branch (register)

;; Unconditional branch to register

(let-syntax
    ((define-branch-to-register-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? Rn register-31=z))
               (BITS (7 #b1101011)
                     (1 0)              ;Z
                     (1 0)
                     (2 ,op)
                     (5 #b11111)
                     (4 #b0000)
                     (1 0)              ;A
                     (1 0)              ;M
                     (5 Rn)
                     (5 0)))))))))
  (define-branch-to-register-instruction BR #b00)
  (define-branch-to-register-instruction BLR #b01))

;; Return (same as BR but with prediction hint and default R31, LR)

(define-instruction RET
  (()
   (BITS (7 #b1101011)
         (1 0)                          ;Z
         (1 0)
         (2 #b10)                       ;op
         (5 #b11111)
         (4 #b0000)
         (1 0)                          ;A
         (1 0)                          ;M
         (5 30)                         ;Rn=30, link register
         (5 0)))
  (((? Rn register-31=z))
   (BITS (7 #b1101011)
         (1 0)                          ;Z
         (1 0)
         (2 #b10)                       ;op
         (5 #b11111)
         (4 #b0000)
         (1 0)                          ;A
         (1 0)                          ;M
         (5 Rn)
         (5 0))))

;;; C3.1.4 Exception generation and return

(let-syntax
    ((define-exception-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (let ((mnemonic (list-ref form 1))
               (opc (list-ref form 2))
               (op2 (list-ref form 3))
               (LL (list-ref form 4)))
           `(define-instruction ,mnemonic
              (((&U (? imm unsigned-16)))
               (BITS (8 #b11010100)
                     (3 ,opc)
                     (16 imm)
                     (3 ,op2)
                     (2 ,LL)))))))))
  ;; Supervisor Call (-> EL1)
  (define-exception-instruction SVC #b000 #b000 #b01)
  ;; Hypervisor Call (non-secure EL1 -> EL2)
  (define-exception-instruction HVC #b000 #b000 #b10)
  ;; Secure Monitor Call (EL>=1 -> EL3)
  (define-exception-instruction SMC #b000 #b000 #b11)
  ;; Breakpoint
  (define-exception-instruction BRK #b001 #b000 #b00)
  ;; Halt
  (define-exception-instruction HLT #b010 #b000 #b00))

;; Exception return

(define-instruction ERET
  (()
   (BITS (7 #b1101011)
         (1 0)
         (3 #b100)
         (5 #b11111)
         (4 #b0000)
         (1 0)                          ;A
         (1 0)                          ;M
         (5 31)                         ;Rn
         (5 0))))                       ;op4

;;; C3.1.5 System register instructions

;; Move to special register

(define-instruction MSR
  ;; Immediate
  (((? psf msr-pstatefield) (&U (? CRm unsigned-4)))
   (BITS (8 #b11010101)
         (5 #b00000)
         (3 psf PSTATEFIELD-OP1)
         (4 #b0100)
         (4 CRm)
         (3 psf PSTATEFIELD-OP2)
         (5 31)))
  ;; Register
  ;; ... XXX
  )

;; XXX MRS

;;; C3.1.6 System instructions

;; XXX SYS, SYSL, IC, DC, AT, TLBI

;;; C3.1.7 Hint instructions, C3.1.8 Barriers and CLREX instructions

;; Generic HINT format.

(define-instruction HINT
  (((&U (? imm unsigned-7)))
   (BITS (8 #b11010101)
         (2 #b00)
         (1 0)
         (2 #b00)
         (3 #b011)
         (4 #b0010)
         (7 imm)
         (5 #b11111))))

;; Common hint and barrier format.

(let-syntax
    ((define-hint/barrier-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (let ((mnemonic (list-ref form 1))
               (CRm (list-ref form 2))
               (op2 (list-ref form 3)))
           `(define-instruction ,mnemonic
              (()
               (BITS (8 #b11010101)
                     (2 #b00)
                     (1 0)
                     (2 #b00)
                     (3 #b011)
                     (4 #b0010)
                     (4 ,CRm)
                     (3 ,op2)
                     (5 #b11111)))))))))
  ;; Hints: CRm=#b0000
  ;;
  ;; No-op
  (define-hint/barrier-instruction NOP #b0000 #b000)
  ;; Yield bus while spinning on spin lock
  (define-hint/barrier-instruction YIELD #b0000 #b001)
  ;; Wait for event, signalled by SEV on any CPU (`PE') or SEVL on this one
  (define-hint/barrier-instruction WFE #b0000 #b010)
  ;; Wait for interrupt
  (define-hint/barrier-instruction WFI #b0000 #b011)
  ;; Send event, waking WFE in all CPUs (`PE') in multiprocessor system
  (define-hint/barrier-instruction SEV #b0000 #b100)
  ;; Send event local, waking WFE on this CPU
  (define-hint/barrier-instruction SEVL #b0000 #b101)
  ;; Barriers: CRm=#b0010
  ;;
  ;; Error synchronization barrier
  (define-hint/barrier-instruction ESB #b0010 #b000)
  ;; Profiling synchronization barrier
  (define-hint/barrier-instruction PSB-CSYNC #b0010 #b001)
  ;; Trace synchronization barrier
  (define-hint/barrier-instruction TSB-CSYNC #b0010 #b010)
  ;; Consumption of speculative data barrier
  (define-hint/barrier-instruction CSDB #b0010 #b100))

;; Clear exclusive: clear local monitor of the executing PE.

(define-instruction CLREX
  (((&U (? CRm unsigned-4)))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b010)                      ;op2
         (5 31))))

;; Data memory barrier

(define-instruction DMB
  (((? CRm dmb-option))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b010)                      ;op2
         (5 31)))
  (((&U (? CRm unsigned-4)))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b010)                      ;op2
         (5 31))))

;; Speculative store bypass barrier (physical address), encoded like DMB.

(define-instruction PSSBB
  (()
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 #b0100)
         (3 #b010)                      ;op2
         (5 31))))

(define-instruction ISB
  (()
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 #b1111)                     ;CRm, full system barrier
         (3 #b110)                      ;op2
         (5 31)))
  (((? CRm isb-option))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b110)                      ;op2
         (5 31)))
  (((&U (? CRm unsigned-4)))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b110)                      ;op2
         (5 31))))

;; Data synchronization barrier

(define-instruction DSB
  (((? CRm dsb-option))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b100)                      ;op2
         (5 31)))
  (((&U (? CRm unsigned-4)))
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 CRm)
         (3 #b100)                      ;op2
         (5 31))))

;; Speculative store bypass barrier (virtual address)

(define-instruction SSBB
  (()
   (BITS (8 #b11010101)
         (8 #b00000011)
         (4 #b0011)
         (4 #b0000)
         (3 #b100)                      ;op2
         (5 31))))

;;; C3.1.9 Pointer authentication instructions

;; XXX pointer authentication instructions

;;; C3.2 Loads and stores

;;; C3.2.1 Load/Store register

(let-syntax
    ((define-load/store-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic load/store . extra) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; LDRB/LDRH/LDR immediate, pre/post-index with signed
              ;; byte offset (C6.2.123, C6.2.125, C6.2.119)
              ;; STRB/STRH/STR immediate, pre/post-index with signed
              ;; byte offset (C6.2.259, C6.2.261, C6.2.257)
              (((? size load/store-size)
                (? Rt register-31=z)
                ((? pre/post load/store-pre/post-index)
                 (? Rn register-31=sp)
                 (& (? offset signed-9))))
               (BITS (2 size)
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 0)
                     (9 offset SIGNED)
                     (2 pre/post)
                     (5 Rn)
                     (5 Rt)))
              ;; LDRB/LDRH/LDR immediate, zero offset
              ;; (C6.2.123, C6.2.125, C6.2.119)
              ;; STRB/STRH/STR immediate, zero offset
              ;; (C6.2.259, C6.2.261, C6.2.257)
              (((? size load/store-size)
                (? Rt register-31=z)
                (? Rn register-31=sp))
               (BITS (2 size)
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 0)             ;offset=0
                     (5 Rn)
                     (5 Rt)))

              ;; LDRB immediate, unsigned byte offset (C6.2.123)
              ;; STRB immediate, unsigned byte offset (C6.2.259)
              ((B (? Rt register-31=z)
                  (+ (? Rn register-31=sp) (&U (? offset unsigned-12))))
               (BITS (2 #b00)           ;size=B, 8-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Rt)))
              ;; LDRB immediate, unsigned byte offset (C6.2.123)
              ;; STRB immediate, unsigned byte offset (C6.2.259)
              ;; [same as above]
              ((B (? Rt register-31=z)
                  (+ (? Rn register-31=sp) (&U (* 1 (? offset unsigned-12)))))
               (BITS (2 #b00)           ;size=B, 8-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Rt)))
              ;; LDRH immediate, unsigned 2-byte offset (C6.2.125)
              ;; STRH immediate, unsigned 2-byte offset (C6.2.259)
              ((H (? Rt register-31=z)
                  (+ (? Rn register-31=sp) (&U (* 2 (? offset unsigned-12)))))
               (BITS (2 #b01)           ;size=H, 16-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Rt)))
              ;; LDR (W) immediate, unsigned 4-byte offset (C6.2.119)
              ;; STR (W) immediate, unsigned 4-byte offset (C6.2.257)
              ((W (? Rt register-31=z)
                  (+ (? Rn register-31=sp) (&U (* 4 (? offset unsigned-12)))))
               (BITS (2 #b10)           ;size=W, 32-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Rt)))
              ;; LDR (X) immediate, unsigned 8-byte offset (C6.2.119)
              ;; STR (X) immediate, unsigned 8-byte offset (C6.2.257)
              ((X (? Rt register-31=z)
                  (+ (? Rn register-31=sp) (&U (* 8 (? offset unsigned-12)))))
               (BITS (2 #b11)           ;size=X, 64-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Rt)))

              ;; LDRB/LDRH/LDR register, no extend
              ;; (C6.2.124, C6.2.126, C6.2.121)
              ;; STRB/STRH/STR register, no extend
              ;; (C6.2.260, C6.2.262, C6.2.258)
              (((? size load/store-size)
                (? Rt register-31=z)
                (+ (? Rn register-31=sp)
                   (? Rm register-31=z)))
               (BITS (2 size)
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 #b011)          ;option=LSL
                     (1 0)              ;shift=0
                     (2 #b10)
                     (5 Rn)
                     (5 Rt)))
              ;; LDRB extended register, 8-bit operand size (C6.2.124)
              ;; STRB extended register, 8-bit operand size (C6.2.260)
              ((B (? Rt register-31=z)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store8-extend-amount))))
               (BITS (2 #b00)           ;size=B, 8-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Rt)))
              ;; LDRH extended register, 16-bit operand size (C6.2.126)
              ;; STRH extended register, 16-bit operand size (C6.2.262)
              ((H (? Rt register-31=z)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store16-extend-amount))))
               (BITS (2 #b01)           ;size=H, 16-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Rt)))

              ;; LDR (W) extended register, 32-bit operand size (C6.2.121)
              ;; STR (W) extended register, 32-bit operand size (C6.2.258)
              ((W (? Rt register-31=z)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store32-extend-amount))))
               (BITS (2 #b10)           ;size=W, 32-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Rt)))
              ;; LDR (X) extended register, 64-bit operand size (C6.2.121)
              ;; STR (X) extended register, 64-bit operand size (C6.2.258)
              ((X (? Rt register-31=z)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store64-extend-amount))))
               (BITS (2 #b11)           ;size=X, 64-bit
                     (3 #b111)
                     (1 0)              ;general
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Rt)))
              ,@extra))))))
  (define-load/store-instruction STR 0)
  (define-load/store-instruction LDR 1
    ;; LDR PC-relative literal (C6.2.120).
    (((? opc ldr-literal-size)
      (? Rt register-31=z)
      (@PCO (* 4 (? offset signed-19))))
     (BITS (2 opc)
           (3 #b011)
           (1 0)                        ;general
           (2 #b00)
           (19 offset SIGNED)
           (5 Rt)))
    (((? size) (? Rt) (@PCR (? label) (? temp register<31)))
     (VARIABLE-WIDTH offset `(/ (- ,label *PC*) 4)
       ((#x-40000 #x3ffff)
        (MACRO 32 (LDR ,size ,Rt (@PCO (* 4 ,offset)))))
       ((#x-100000000 #xffffffff)
        ;; Could maybe use ADRP and LDR with unsigned 8-byte offset,
        ;; but only if the offset is or this instruction is aligned,
        ;; which the assembler can't handle easily.
        (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
        (MACRO 32 (LDR X ,Rt ,temp)))))))

;;; C3.2.3 Load/Store Pair

(let-syntax
    ((define-load/store-pair-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic L) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Zero offset.
              (((? sf sf-size)
                (? Rt1 register-31=z)
                (? Rt2 register-31=z)
                (? Rn register-31=sp))
               (BITS (1 sf)
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (1 0)
                     (2 #b10)
                     (1 ,L)
                     (7 0)              ;offset=0
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))
              ;; Signed offset, 32-bit operand size.
              ((W (? Rt1 register-31=z)
                  (? Rt2 register-31=z)
                  (+ (? Rn register-31=sp)) (& (* 4 (? imm signed-7))))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (1 0)
                     (2 #b10)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))

              ;; Signed offset, 64-bit operand size.
              ((X (? Rt1 register-31=z)
                  (? Rt2 register-31=z)
                  (+ (? Rn register-31=sp)) (& (* 8 (? imm signed-7))))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (1 0)
                     (2 #b10)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))
              ;; Pre/post-index signed offset.
              ((W (? Rt1 register-31=z)
                  (? Rt2 register-31=z)
                  ((? pre/post load/store-pre/post-index)
                   (? Rn register-31=sp)
                   (& (* 4 (? imm signed-7)))))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (1 0)
                     (2 pre/post)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))
              ((X (? Rt1 register-31=z)
                  (? Rt2 register-31=z)
                  ((? pre/post load/store-pre/post-index)
                   (? Rn register-31=sp)
                   (& (* 8 (? imm signed-7)))))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (1 0)
                     (2 pre/post)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))))))))
  (define-load/store-pair-instruction LDP 1)
  (define-load/store-pair-instruction STP 0))

;; Load register signed (i.e., sign-extended).  This does not detect
;; the nonsensical (LDRS W W ...) operand combination -- it will
;; assemble into a possibly different instruction.

(define-instruction LDRS
  ;; Immediate, zero offset (C6.2.127, C6.2.129, C6.2.131)
  (((? size load-signed-size)
    (? opc load-signed-opc)
    (? Rt register-31=z)
    (? Rn register-31=sp))
   (BITS (2 size)
         (3 #b111)
         (1 0)
         (2 #b01)
         (2 opc)
         (12 0)                         ;imm12
         (5 Rn)
         (5 Rt)))
  ;; Immediate, unsigned offset (C6.2.127, C6.2.129, C6.2.131)
  (((? size load-signed-size)
    (? opc load-signed-opc)
    (? Rt register-31=z)
    (+ (? Rn register-31=sp) (&U (? offset unsigned-12))))
   (BITS (2 size)
         (3 #b111)
         (1 0)
         (2 #b01)
         (2 opc)
         (12 offset)                    ;imm12
         (5 Rn)
         (5 Rt)))
  ;; Pre/post-indexed signed offset (C6.2.127, C6.2.129, C6.2.131)
  (((? opc load-signed-opc)
    (? size load-signed-size)
    (? Rt register-31=z)
    ((? pre/post load/store-pre/post-index)
     (? Rn register-31=sp)
     (& (? offset signed-9))))
   (BITS (2 size)
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 opc)
         (1 0)
         (9 offset SIGNED)
         (2 pre/post)
         (5 Rn)
         (5 Rt)))

  ;; Register, no extend (C6.2.128, C6.2.130, C6.2.132)
  (((? size load-signed-size)
    (? opc load-signed-opc)
    (? Rt register-31=z)
    (+ (? Rn register-31=sp)
       (? Rm register-31=z)))
   (BITS (2 size)
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 opc)
         (1 1)
         (5 Rm)
         (3 #b011)                      ;option=LSL
         (1 0)                          ;shift=0
         (2 #b10)
         (5 Rn)
         (5 Rt)))
  ;; Extended register (C6.2.128, C6.2.130, C6.2.132)
  (((? size load-signed-size)
    (? opc load-signed-opc)
    (? Rt register-31=z)
    (+ (? Rn register-31=sp)
       ((? option load/store-extend-type)
        (? Rm register-31=z)
        (? S load/store32-extend-amount))))
   (BITS (2 size)
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 opc)
         (1 1)
         (5 Rm)
         (3 option)
         (1 S)
         (2 #b10)
         (5 Rn)
         (5 Rt)))
  ;; Literal -- only loading W into X (C6.2.132)
  ((X W (? Rt register-31=z) (@PCO (* 4 (? offset signed-19))))
   (BITS (2 #b10)                       ;opc
         (3 #b011)
         (1 0)                          ;general
         (2 #b00)
         (19 offset SIGNED)
         (5 Rt)))
  ((X W (? Rt register-31=z) (@PCR (? label) (? temp register<31)))
   (VARIABLE-WIDTH offset `(/ (- ,label *PC*) 4)
     ((#x-40000 #x3ffff)
      (MACRO 32 (LDRS ,Rt (@PCO (* 4 ,offset)))))
     ((#x-100000000 #xffffffff)
      (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
      (MACRO 32 (LDRS ,Rt ,temp))))))

;;; C3.3 and C3.4: Data processing - immediate and register

;;; C3.3.3 Move (wide immediate)

(let-syntax
    ((define-move-wide-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rd register-31=z)
                (&U (? imm unsigned-16)))
               (BITS (1 sf)
                     (2 ,opc)
                     (1 1)
                     (4 #b0010)
                     (1 1)
                     (2 0)              ;hw shift=0
                     (16 imm)
                     (5 Rd)))
              ((W (? Rd register-31=z)
                  (LSL (&U (? imm unsigned-16)) (? hw hw-shift32)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0010)
                     (1 1)
                     (2 hw)
                     (16 imm)
                     (5 Rd)))
              ((X (? Rd register-31=z)
                  (LSL (&U (? imm unsigned-16)) (? hw hw-shift64)))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0010)
                     (1 1)
                     (2 hw)
                     (16 imm)
                     (5 Rd)))))))))
  ;; Move wide with NOT
  (define-move-wide-instruction MOVN #b00)
  ;; Move wide with zero
  (define-move-wide-instruction MOVZ #b10)
  ;; Move wide with keep
  (define-move-wide-instruction MOVK #b11))

;;; C3.3.5 PC-relative address calculation

(define-instruction ADR
  ((X (? Rd register-31=z) (@PCO (? offset signed-21)))
   (BITS (1 0)                          ;op=ADR, byte offset
         (2 (bitwise-and offset #b11))
         (1 1)
         (4 #b0000)
         (19 (shift-right offset 2) SIGNED)
         (5 Rd)))
  ((X (? Rd) (@PCR (? label) (? temp register<31)))
   (VARIABLE-WIDTH offset `(- ,label *PC*)
     ((#x-100000 #xfffff)               ;21-bit signed _byte_ offset
      (MACRO 32 (ADR X ,Rd (@PCO ,offset))))
     ((#x-100000000 #xffffffff)         ;33-bit signed _byte_ offset
      (MACRO 64 (ADRP-ADD X ,Rd (@PCO ,offset)))))))

(define-instruction ADRP
  ((X (? Rd register-31=z) (@PCO (LSL (? offset signed-21) 12)))
   (BITS (1 1)                          ;op=ADRP, page offset
         (2 (bitwise-and offset #b11))
         (1 1)
         (4 #b0000)
         (19 (shift-right offset 2) SIGNED)
         (5 Rd)))
  ((X (? Rd) (@PCR (? label)))
   (MACRO 32 (ADRP X ,Rd (@PCO ,`(QUOTIENT (- ,label *PC*) #x1000))))))

;; Macro used for assembling variable-width instructions which can't
;; just defer to the variable-width ADR because we prohibit variable
;; width within variable width.
;;
;; XXX OOPS!  This doesn't actually work, because ADRP actually
;; computes PC - (PC mod 2^12) + 2^12*offset, not PC + 2^12*offset.
;; Either we need to know where we are in a page (which can change from
;; GC to GC because compiled blocks are only float-aligned, not
;; page-aligned), or we need to generate extra code to find and add PC
;; mod 2^12, which looks like it'll require another temporary register.
;; Ugh!
;;
;; The way that assemblers usually get around this is either by always
;; relying on relocations or by always aligning code on page boundaries
;; so that we know PC mod 2^12 anyway.

#;
(define-instruction ADRP-ADD
  ((X (? Rd) (@PCO (? offset signed-33)))
   (MACRO 32 (ADRP X ,Rd (@PCO (LSL ,(shift-right offset 12) 12)))) ;(*)
   (MACRO 32 (ADD X ,Rd ,Rd (&U ,(bitwise-and offset #xfff))))))

;;; C3.3.2 Logical (immediate) and C3.4.5 Logical (shifted register)

(let-syntax
    ((define-logical-instruction
       (sc-macro-transformer
        (lambda (form environment)
          environment
          (receive (mnemonic opc register-31=dst Rd) (apply values (cdr form))
            `(define-instruction ,mnemonic
               ;; Immediate, 32-bit operand size
               ((W ,@(if Rd '() `((? Rd ,register-31=dst)))
                   (? Rn register-31=z)
                   (&U (? imm logical-imm-u32)))
                (BITS (1 0)           ;sf=0, 32-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (13 imm)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ((W ,@(if Rd '() `((? Rd ,register-31=dst)))
                   (? Rn register-31=z)
                   (& (? imm logical-imm-s32)))
                (BITS (1 0)           ;sf=0, 32-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (13 imm)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ;; Immediate, 64-bit operand size
               ((X ,@(if Rd '() '((? Rd register-31=sp)))
                   (? Rn register-31=z)
                   (&U (? imm logical-imm-u64)))
                (BITS (1 1)           ;sf=1, 64-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (13 imm)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ((X ,@(if Rd '() '((? Rd register-31=sp)))
                   (? Rn register-31=z)
                   (& (? imm logical-imm-s64)))
                (BITS (1 1)           ;sf=1, 64-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (13 imm)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ;; Shifted register, no shift amount.
               (((? sf sf-size)
                 ,@(if Rd '() '((? Rd register-31=z)))
                 (? Rn register-31=z)
                 (? Rm register-31=z))
                (BITS (1 sf)
                      (2 ,opc)
                      (1 0)
                      (4 #b1010)
                      (2 #b00)        ;shift type=LSL
                      (1 0)           ;N=0
                      (5 Rm)
                      (6 0)           ;shift amount=0
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))

               ;; Shifted register, 32-bit operand size.
               ((W ,@(if Rd '() '((? Rd register-31=z)))
                   (? Rn register-31=z)
                   ((? type logical-shift/rotate-type)
                    (? Rm register-31=z)
                    (? amount unsigned-5)))
                (BITS (1 0)           ;sf=0, 32-bit operand size
                      (2 ,opc)
                      (1 0)
                      (4 #b1010)
                      (2 type)
                      (1 0)           ;N=0
                      (5 Rm)
                      (6 amount)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ;; Shifted register, 64-bit operand size.
               ((X ,@(if Rd '() '((? Rd register-31=z)))
                   (? Rn register-31=z)
                   ((? type logical-shift/rotate-type)
                    (? Rm register-31=z)
                    (? amount unsigned-6)))
                (BITS (1 1)           ;sf=1, 64-bit operand size
                      (2 ,opc)
                      (1 0)
                      (4 #b1010)
                      (2 type)
                      (1 0)           ;N=0
                      (5 Rm)
                      (6 amount)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))))))))
  ;; Logical AND
  (define-logical-instruction AND #b00 register-31=sp #f)
  ;; Logical inclusive OR
  (define-logical-instruction ORR #b01 register-31=sp #f)
  ;; Logical exclusive OR
  (define-logical-instruction EOR #b10 register-31=sp #f)
  ;; Logical AND and set flags
  (define-logical-instruction ANDS #b11 register-31=z #f)
  ;; Test: ANDS(Rd=z)
  (define-logical-instruction TST #b11 register-31=z 31))

;;; C3.4.5 Logical (shifted register)

;; No immediates because these are all with complement, which would be
;; redundant with immediates.

(let-syntax
    ((define-logical-complement-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Shifted register, no shift amount.
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (2 ,opc)
                     (5 #b01010)
                     (2 0)              ;shift type=LSL
                     (1 1)              ;N=1
                     (5 Rm)
                     (6 0)              ;shift amount=0
                     (5 Rn)
                     (5 Rd)))
              ;; Shifted register, 32-bit operand size
              ((W (? Rd register-31=z)
                  (? Rn register-31=z)
                  ((? type logical-shift/rotate-type)
                   (? Rm register-31=z)
                   (? amount unsigned-5)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (5 #b01010)
                     (2 type)
                     (1 1)              ;N=1
                     (5 Rm)
                     (1 0)              ;high 0 bit of 6-bit immediate
                     (5 amount)
                     (5 Rn)
                     (5 Rd)))
              ;; Shifted register, 64-bit operand size
              ((X (? Rd register-31=z)
                  (? Rn register-31=z)
                  ((? type logical-shift/rotate-type)
                   (? Rm register-31=z)
                   (? amount unsigned-6)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (5 #b01010)
                     (2 type)
                     (1 1)              ;N=1
                     (5 Rm)
                     (6 amount)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Bitwise bit clear (AND with complement)
  (define-logical-complement-instruction BIC #b00)
  ;; Bitwise bit clear (AND with complement) and set flags
  (define-logical-complement-instruction BICS #b11)
  ;; Bitwise exclusive OR NOT
  (define-logical-complement-instruction EON #b10)
  ;; Bitwise inclusive OR NOT
  (define-logical-complement-instruction ORN #b01))

;; Bitwise NOT (or, `move with NOT?')

(define-instruction MVN
  (((? sf) (? Rd) (? Rm))
   (MACRO 32 (ORN ,sf ,Rd Z ,Rm))))

;;; C3.3.1, C3.4.1, C3.4.1: Arithmetic (immediate, shifted register,
;;; extended register)

(let-syntax
    ((define-addsub-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op S register-31=dst Rd) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Immediate, shift=0
              (((? sf sf-size)
                ,@(if Rd '() '((? Rd register-31=sp)))
                (? Rn register-31=sp)
                (&U (? imm unsigned-12)))
               (BITS (1 sf)
                     (1 ,op)
                     (1 ,S)
                     (1 1)
                     (4 #b0001)
                     (2 #b00)
                     (12 imm)
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))
              ;; Immediate, shift=12
              (((? sf sf-size)
                ,@(if Rd '() '((? Rd register-31=sp)))
                (? Rn register-31=sp)
                (LSL (&U (? imm unsigned-12)) 12))
               (BITS (1 sf)
                     (1 ,op)
                     (1 ,S)
                     (1 1)
                     (4 #b0001)
                     (2 #b01)
                     (12 imm)
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))
              ;; Shifted register, no shift amount.  Could also be
              ;; encoded by extended register as long as Rm is not the
              ;; zero register.
              (((? sf sf-size)
                ,@(if Rd '() '((? Rd register-31=z)))
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (1 ,op)
                     (1 ,S)
                     (1 0)
                     (4 #b1011)
                     (2 #b00)           ;shift type=LSL
                     (1 0)
                     (5 Rm)
                     (6 0)              ;shift amount=0
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))
              ;; Shifted register, 32-bit
              ((W ,@(if Rd '() '((? Rd register-31=z)))
                  (? Rn register-31=z)
                  ((? type add/sub-shift-type)
                   (? Rm register-31=z)
                   (? amount unsigned-5)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (1 ,op)
                     (1 ,S)
                     (1 0)
                     (4 #b1011)
                     (2 type)
                     (1 0)
                     (5 Rm)
                     (6 amount)
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))

              ;; Shifted register, 64-bit
              ((X ,@(if Rd '() '((? Rd register-31=z)))
                  (? Rn register-31=z)
                  ((? type add/sub-shift-type)
                   (? Rm register-31=z)
                   (? amount unsigned-6)))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (1 ,op)
                     (1 ,S)
                     (1 0)
                     (4 #b1011)
                     (2 type)
                     (1 0)
                     (5 Rm)
                     (6 amount)
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))
              ;; Extended register
              (((? sf sf-size)
                ,@(if Rd '() `((? Rd ,register-31=dst)))
                (? Rn register-31=sp)
                ((? option add/sub-extend-type)
                 (? Rm register-31=z)
                 (? amount unsigned-2)))
               (BITS (1 sf)
                     (1 ,op)
                     (1 ,S)
                     (1 0)
                     (4 #b1011)
                     (2 #b00)
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (3 amount)
                     (5 Rn)
                     (5 ,(or Rd 'Rd))))))))))
  ;; Add
  (define-addsub-instruction ADD 0 0 register-31=sp #f)
  ;; Add and set flags
  (define-addsub-instruction ADDS 0 1 register-31=z #f)
  ;; Compare negation: ADDS(Rd=z)
  (define-addsub-instruction CMN 0 1 #f 31)
  ;; Subtract
  (define-addsub-instruction SUB 1 0 register-31=sp #f)
  ;; Subtract and set flags
  (define-addsub-instruction SUBS 1 1 register-31=z #f)
  ;; Compare: SUBS(Rd=z)
  (define-addsub-instruction CMP 1 1 #f 31))

;; Negate

(define-instruction NEG
  (((? sf) (? Rd) (? Rm))
   (MACRO 32 (SUB ,sf ,Rd Z ,Rm))))

;; Negate and set flags

(define-instruction NEGS
  (((? sf) (? Rd) (? Rm))
   (MACRO 32 (SUBS ,sf ,Rd Z ,Rm))))

;;; C3.4.3 Arithmetic with carry

(let-syntax
    ((define-addsub/carry-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op S) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (1 ,op)
                     (1 ,S)
                     (5 #b11010)
                     (3 #b000)
                     (5 Rm)
                     (6 0)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Add with carry
  (define-addsub/carry-instruction ADC 0 0)
  ;; Add with carry and set flags
  (define-addsub/carry-instruction ADCS 0 1)
  ;; Subtract with borrow
  (define-addsub/carry-instruction SBC 1 0)
  ;; Subtract with borrow and set flags
  (define-addsub/carry-instruction SBCS 1 1))

;;; C3.4.8 Multiply and divide

(let-syntax
    ((define-muladdsub-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op31 o0) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z)
                (? Ra register-31=z))
               (BITS (1 sf)
                     (2 #b00)           ;op54
                     (5 #b11011)
                     (3 ,op31)
                     (5 Rm)
                     (1 ,o0)
                     (5 Ra)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-muladdsub-instruction MADD #b000 0) ;Multiply-Add
  (define-muladdsub-instruction MSUB #b000 1)) ;Multiply-Subtract

(let-syntax
    ((define-widemul-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op31 o0) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ((X (? Rd register-31=z)
                  (? Rn register-31=z)
                  (? Rm register-31=z))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (2 #b00)           ;op54
                     (5 #b11011)
                     (3 ,op31)
                     (5 Rm)
                     (1 ,o0)
                     (5 #b11111)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-widemul-instruction SMADDL #b001 0) ;Signed Mul/Add 32x32->64 lo
  (define-widemul-instruction SMSUBL #b001 1) ;Signed Mul/Sub 32x32->64 lo
  (define-widemul-instruction SMULH #b010 0) ;Signed Mul/Add 64x64->128 hi
  (define-widemul-instruction UMADDL #b101 0) ;Unsigned Mul/Add 32x32->64 lo
  (define-widemul-instruction UMSUBL #b101 1) ;Unsigned Mul/Add 32x32->64 lo
  (define-widemul-instruction UMULH #b110 0)) ;Unsigned Mul/Add 64x64->128 hi

(define-instruction MUL                 ;Multiply
  (((? size) (? Rd) (? Rn) (? Rm))
   (MACRO 32 (MADD ,size ,Rd ,Rn ,Rm Z))))

(define-instruction MNEG                ;Multiply-Negate
  (((? size) (? Rd) (? Rn) (? Rm))
   (MACRO 32 (MSUB ,size ,Rd ,Rn ,Rm Z))))

(define-instruction SMULL               ;Signed Multiply 32x32->64
  ((X (? Rd) (? Rn) (? Rm))
   (MACRO 32 (SMADDL X ,Rd ,Rn ,Rm Z))))

(define-instruction SMNEGL              ;Signed Multiply-Negate 32x32->64
  ((X (? Rd) (? Rn) (? Rm))
   (MACRO 32 (SMSUBL X ,Rd ,Rn ,Rm Z))))

(define-instruction UMULL               ;Unsigned Multiply 32x32->64
  ((X (? Rd) (? Rn) (? Rm))
   (MACRO 32 (SMADDL X ,Rd ,Rn ,Rm Z))))

(define-instruction UMNEGL              ;Unsigned Multiply-Negate 32x32->64
  ((X (? Rd) (? Rn) (? Rm))
   (MACRO 32 (UMSUBL X ,Rd ,Rn ,Rm Z))))

(let-syntax
    ((define-div-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic o1) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (2 #b00)
                     (5 #b11010)
                     (3 #b110)
                     (5 Rm)
                     (5 #b00001)
                     (1 ,o1)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-div-instruction UDIV 0)
  (define-div-instruction SDIV 1))

;;; C3.4.11 Conditional select

(let-syntax
    ((define-conditional-select-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op o2) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? condition branch-condition)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (1 ,op)
                     (1 0)
                     (1 1)
                     (4 #b1010)
                     (3 #b100)
                     (5 Rm)
                     (4 condition)
                     (1 0)
                     (1 ,o2)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Rd := Rn if condition else Rm
  (define-conditional-select-instruction CSEL 0 0)
  ;; Rd := Rn if condition else (Rm + 1)
  (define-conditional-select-instruction CSINC 0 1)
  ;; Rd := Rn if condition else ~Rn
  (define-conditional-select-instruction CSINV 1 0)
  ;; Rd := Rn if condition else -Rn
  (define-conditional-select-instruction CSNEG 1 1))

;; Rd := 1 if condition else 0
(define-instruction CSET
  (((? sf) (? condition) (? Rd))
   (MACRO 32 (CSINC ,sf ,condition ,Rd Z Z))))

;; Rd := -1 if condition else 0
(define-instruction CSETM
  (((? sf) (? condition) (? Rd))
   (MACRO 32 (CSINV ,sf ,condition ,Rd Z Z))))

;; Rd := (Rn + 1) if condition else Rn
(define-instruction CINC
  (((? sf) (? condition) (? Rd) (? Rn))
   (MACRO 32 (CSINC ,sf ,(invert-branch-condition condition) ,Rd ,Rn ,Rn))))

;; Rd := (~Rn) if condition else Rn
(define-instruction CINV
  (((? sf) (? condition) (? Rd) (? Rn))
   (MACRO 32 (CSINV ,sf ,(invert-branch-condition condition) ,Rd ,Rn ,Rn))))

;; Rd := (-Rn) if condition else Rn
(define-instruction CNEG
  (((? sf) (? condition) (? Rd) (? Rn))
   (MACRO 32 (CSNEG ,sf ,(invert-branch-condition condition) ,Rd ,Rn ,Rn))))

;;; Local Variables:
;;; eval: (put 'variable-width 'scheme-indent-function 2)
;;; End:
