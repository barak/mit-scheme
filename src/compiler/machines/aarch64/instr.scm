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

;;;; AArch Instruction Set
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;; Idea for branch tensioning: in every @PCR, allow an optional
;;; temporary register, like (@PCR <label> (T <temp>)); then assemble
;;; into a two-instruction sequence that uses the temporary register.
;;;
;;; Not really necessary: x16 and x17 are for that purpose.
;;;
;;; Syntax notes:
;;;
;;; - Should shifted immediates be (* 8 (&U ...)), (&U (* 8 ...)), (LSL
;;;   (&U ...) 3), or (&U (LSL ... 3))?

;;;; Helpers, for insutl.scm

(define (sf-size size)
  (case size
    ((W) 0)
    ((X) 1)
    (else #f)))

(define (vregister v)
  (and (<= 0 v 31)
       v))

(define (register<31 r)
  (and (<= 0 r 30)
       r))

(define (register-31=z r)
  (cond ((eq? r 'Z) 31)
        ((<= 0 r 30) r)
        (else #f)))

(define (register-31=sp r)
  (cond ((<= 0 r 31) r)
        (else #f)))

(define (msr-pstatefield x)
  (case x
    ((SPSel) #b000101)
    ((DAIFSet) #b011110)
    ((DAIFClr) #b011111)
    ((UAO) #b000011)
    ((PAN) #b000100)
    ((DIT) #b011010)
    (else #f)))

(define (load/store-pre/post-index op)
  (case op
    ((POST+) #b01)
    ((PRE+) #b11)
    (else #f)))

(define (load/store-size sz)
  (case sz
    ((B) #b00)
    ((H) #b01)
    ((W) #b10)
    ((X) #b11)
    (else #f)))

(define (load/store-simd/fp-size sz)
  ;; Returns size(2) || opchi(1).  opclo(1), omitted, is 1 for a load
  ;; and 0 for a store.
  (case sz
    ((B) #b000)
    ((H) #b010)
    ((S) #b100)
    ((D) #b110)
    ((Q) #b001)
    (else #f)))

(define (ldr-simd/fp-size sz)
  (case sz
    ((S) #b00)
    ((D) #b01)
    ((Q) #b10)
    (else #f)))

(define (str-simd/fp-size sz)
  (case sz
    (())))

(define (ldr-literal-size sz)
  (case sz
    ;; No byte or halfword, only word and extended word.
    ((W) #b00)
    ((X) #b01)
    (else #f)))

(define (load/store-extend-type t)
  (case t
    ((UTXW) #b010)
    ((LSL) #b011)
    ((SXTW) #b110)
    ((SXTX) #b111)
    (else #f)))

(define (load/store8-extend-amount amount)
  (case amount
    ((#f) 0)
    ((0) 1)
    (else #f)))

(define (load/store16-extend-amount amount)
  (case amount
    ((0) 0)
    ((1) 1)
    (else #f)))

(define (load/store32-extend-amount amount)
  (case amount
    ((0) 0)
    ((2) 1)
    (else #f)))

(define (load/store64-extend-amount amount)
  (case amount
    ((0) 0)
    ((3) 1)
    (else #f)))

(define (load/store128-extend-amount amount)
  (case amount
    ((0) 0)
    ((4) 1)
    (else #f)))

;;;; Instructions, ordered by sections in ARMv8-A ARM, C3

;;; C3.1.1 Conditional branch

(let-syntax
    ((define-conditional-branch-instruction
      (lambda (form environment)
        environment
        (let ((mnemonic (list-ref form 1))
              (o0 (list-ref form 2))
              (o1 (list-ref form 3))
              (condition (list-ref form 4)))
          `(define-instruction ,mnemonic
             (((@PCR (? target)))
              (BITS (7 #b0101010)
                    (1 ,o1)
                    (19 `(- ,target *PC*) SIGNED)
                    (1 ,o0)
                    (4 ,condition))))))))
  ;; PSTATE condition bits:
  ;; .n = negative
  ;; .z = zero
  ;; .c = carry
  ;; .v = overflow
  ;; Branch if...
  (define-conditional-branch-instruction B.EQ 0 0 #b0000) ;equal
  (define-conditional-branch-instruction B.NE 0 0 #b0001) ;not equal
  (define-conditional-branch-instruction B.CS 0 0 #b0010) ;carry set
  (define-conditional-branch-instruction B.CC 0 0 #b0011) ;carry clear
  (define-conditional-branch-instruction B.MI 0 0 #b0100) ;negative `minus'
  (define-conditional-branch-instruction B.PL 0 0 #b0101) ;nonnegative `plus'
  (define-conditional-branch-instruction B.VS 0 0 #b0110) ;overflow set
  (define-conditional-branch-instruction B.VC 0 0 #b0111) ;overflow clear
  (define-conditional-branch-instruction B.HI 0 0 #b1000) ;carry and nonzero
  (define-conditional-branch-instruction B.LS 0 0 #b1001) ;!carry or zero
  (define-conditional-branch-instruction B.GE 0 0 #b1010) ;greater or equal
                                        ;n = v
  (define-conditional-branch-instruction B.LT 0 0 #b1011) ;less
                                        ;n != v
  (define-conditional-branch-instruction B.GT 0 0 #b1100) ;greater
                                        ;n = v and !z
  (define-conditional-branch-instruction B.LE 0 0 #b1101) ;less or equal
                                        ;n != v or z
  (define-conditional-branch-instruction B.AL 0 0 #b1110) ;always
  #;  ;never?
  (define-conditional-branch-instruction B.<never> 0 0 #b1111))

(let-syntax
    ((define-compare&branch-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size) (? Rt register-31=z) (@PCR (? label)))
               (BITS (1 sf)
                     (6 #b011010)
                     (1 ,op)
                     (19 `(QUOTIENT (- ,label *PC*) 4) SIGNED)
                     (5 Rt)))))))))
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
                  (@PCR (? label)))
               (BITS (1 0)              ;b5, fifth bit of bit index
                     (6 #b011011)
                     (1 ,op)
                     (5 bit)
                     (14 `(- ,label *PC*))
                     (5 Rt)))
              ((X (? Rt register-31=z)
                  (&U (? bit unsigned-6))
                  (@PCR (? label)))
               (BITS (1 bit B5)
                     (6 #b011011)
                     (5 bit B40)
                     (14 `(- ,label *PC*))
                     (5 Rt)))))))))
  ;; Test and branch if zero
  (define-test&branch-instruction TBZ 0)
  ;; Test and branch if nonzero
  (define-test&branch-instruction TBNZ 1))

;;; C3.1.2 Unconditional branch (immediate)

;; Branch unconditional to PC-relative.  Probably no need for
;; variable-width encoding here for a while since there's 26 bits to
;; work with.

(define-instruction B
  (((@PCR (? label)))
   (BITS (1 0)                          ;no link
         (5 #b00101)
         (26 `(- ,label *PC*) SIGNED))))

;; Branch and link unconditional to PC-relative

(define-instruction BL
  (((@PCR (? label)))
   (BITS (1 1)                          ;link
         (5 #b00101)
         (26 `(- ,label *PC*) SIGNED))))

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

(define (dmb-option o)
  (case o
    ((SY) #b1111)
    ((ST) #b1110)
    ((LD) #b1101)
    ((ISH) #b1011)
    ((ISHST) #b1010)
    ((ISHLD) #b1001)
    ((NSH) #b0111)
    ((NSHST) #b0110)
    ((NSHLD) #b0101)
    ((OSH) #b0011)
    ((OSHST) #b0010)
    ((OSHLD) #b0001)
    (else #f)))

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

(define (isb-option o)
  (case o
    ((SY) #b1111)
    (else #f)))

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

(define (dsb-option o)
  (case o
    ((SY) #b1111)
    ((ST) #b1110)
    ((LD) #b1101)
    ((ISH) #b1011)
    ((ISHST) #b1010)
    ((ISHLD) #b1001)
    ((NSH) #b0111)
    ((NSHST) #b0110)
    ((NSHLD) #b0101)
    ((OSH) #b0011)
    ((OSHST) #b0010)
    ((OSHLD) #b0001)
    (else #f)))

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
    (((? opc ldr-literal-size) (? Rt register-31=z) (@PCR (? label)))
     (BITS (2 opc)
           (3 #b011)
           (1 0)                        ;general
           (2 #b00)
           (19 `(QUOTIENT (- ,label *PC*) 4))
           (5 Rt)))))

;;; C3.2.9 Load/Store scalar SIMD and floating-point

(let-syntax
    ((define-simd/fp-load/store-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic load/store . extra) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; LDR immediate, SIMD&FP, pre/post-index with signed
              ;; byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP, pre/post-index with signed
              ;; byte offset (C7.2.315)
              (((? sz load/store-simd/fp-size)
                (? Vt vregister)
                ((? pre/post load/store-pre/post-index)
                 (? Rn register-31=sp)
                 (& (? offset signed-9))))
               (BITS (2 sz LOAD/STORE-SIMD/FP-SIZE)
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 sz LOAD/STORE-SIMD/FP-OPCHI)
                     (1 ,load/store)    ;opc[0]
                     (1 0)
                     (9 offset SIGNED)
                     (2 pre/post)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP, zero offset (C7.2.176)
              ;; STR immediate, SIMD&FP, zero offset (C7.2.315)
              (((? sz load/store-simd/fp-size)
                (? Vt vregister)
                (? Rn register-31=sp))
               (BITS (2 sz LOAD/STORE-SIMD/FP-SIZE)
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 sz LOAD/STORE-SIMD/FP-OPCHI)
                     (1 ,load/store)    ;opc[0]
                     (12 0)             ;offset=0
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (B), unsigned byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (B), unsigned byte offset (C7.2.315)
              ((B (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (? offset unsigned-12))))
               (BITS (2 #b00)           ;size=B, 8-bit
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (B), unsigned byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (B), unsigned byte offset (C7.2.315)
              ;; [same as above]
              ((B (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (* 1 (? offset unsigned-12)))))
               (BITS (2 #b00)           ;size=B, 8-bit
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (H), unsigned 2-byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (H), unsigned 2-byte offset (C7.2.315)
              ((H (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (* 2 (? offset unsigned-12)))))
               (BITS (2 #b01)           ;size=H, 16-bit
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (S), unsigned 4-byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (S), unsigned 4-byte offset (C7.2.315)
              ((S (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (* 4 (? offset unsigned-12)))))
               (BITS (2 #b10)           ;size=S, 32-bit
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (D), unsigned 8-byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (D), unsigned 8-byte offset (C7.2.315)
              ((D (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (* 8 (? offset unsigned-12)))))
               (BITS (2 #b11)           ;size=D, 64-bit
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP (Q), unsigned 16-byte offset (C7.2.176)
              ;; STR immediate, SIMD&FP (Q), unsigned 16-byte offset (C7.2.315)
              ((Q (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     (&U (* 16 (? offset unsigned-12)))))
               (BITS (2 #b00)           ;`size'
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (12 offset)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP, no extend (C7.2.178)
              ;; STR register, SIMD&FP, no extend (C7.3.316)
              (((? sz load/store-simd/fp-size)
                (? Vt vregister)
                (+ (? Rn register-31=sp)
                   (? Rm register-31=z)))
               (BITS (2 sz LOAD/STORE-SIMD/FP-SIZE)
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 sz LOAD/STORE-SIMD/FP-OPCHI)
                     (1 ,load/store)    ;opc[0]
                     (5 Rm)
                     (3 #b011)          ;option=LSL
                     (1 0)              ;shift=0
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP (B), (C7.2.178)
              ;; STR register, SIMD&FP (B), (C7.2.316)
              ((B (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store8-extend-amount))))
               (BITS (2 #b00)           ;size=B
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP (H), (C7.2.178)
              ;; STR register, SIMD&FP (H), (C7.2.316)
              ((H (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store16-extend-amount))))
               (BITS (2 #b01)           ;size=H
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP (S), (C7.2.178)
              ;; STR register, SIMD&FP (S), (C7.2.316)
              ((S (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store32-extend-amount))))
               (BITS (2 #b10)           ;size=H
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP (D), (C7.2.178)
              ;; STR register, SIMD&FP (D), (C7.2.316)
              ((D (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store64-extend-amount))))
               (BITS (2 #b11)           ;size=D
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 0)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR register, SIMD&FP (Q), (C7.2.178)
              ;; STR register, SIMD&FP (Q), (C7.2.316)
              ((Q (? Vt vregister)
                  (+ (? Rn register-31=sp)
                     ((? option load/store-extend-type)
                      (? Rm register-31=z)
                      (? S load/store128-extend-amount))))
               (BITS (2 #b00)           ;size=Q
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 1)              ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 option)
                     (1 S)
                     (2 #b10)
                     (5 Rn)
                     (5 Vt)))
              ,@extra))))))
  ;; The ARM assembler has `LDRB w13,...' for byte-sized load into
  ;; general register 13, and `LDR b13,...' for byte-sized load into
  ;; vector register 13.  We use a separate mnemonic for general
  ;; registers and vector registers.
  (define-simd/fp-load/store-instruction STR.V 0)
  (define-simd/fp-load/store-instruction LDR.V 1
    ;; LDR PC-relative literal, SIMD&FP (C7.2.177)
    (((? opc ldr-literal-simd/fp-size) (? Vt vregister) (@PCR (? label)))
     (BITS (2 opc)
           (3 #b011)
           (1 1)                        ;SIMD/FP
           (2 #b00)
           (19 `(QUOTIENT (- ,label *PC*) 4))
           (5 Vt)))))

;; Load register signed

(define-instruction LDRS
  ;; Immediate, zero unsigned offset
  (((? Rt register-31=z) (? Rn register-31=sp))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b01)
         (2 #b10)                       ;opc
         (12 0)                         ;imm12
         (5 Rn)
         (5 Rt)))
  ;; Immediate, unsigned offset
  (((? Rt register-31=z)
    (+ (? Rn register-31=sp) (&U (? offset unsigned-12))))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b01)
         (2 #b10)                       ;opc
         (12 offset)                    ;imm12
         (5 Rn)
         (5 Rt)))
  ;; Post-indexed signed offset
  (((? Rt register-31=z)
    (POST+ (? Rn register-31=sp) (& (? offset signed-9))))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 #b10)                       ;opc
         (1 0)
         (9 offset SIGNED)
         (2 #b01)                       ;post-index
         (5 Rn)
         (5 Rt)))
  ;; Pre-indexed signed offset
  (((? Rt register-31=z)
    (POST+ (? Rn register-31=sp) (& (? offset signed-9))))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 #b10)                       ;opc
         (1 0)
         (9 offset SIGNED)
         (2 #b11)                       ;pre-index
         (5 Rn)
         (5 Rt)))
  ;; Literal
  (((? Rt register-31=z) (@PCR (? label)))
   (BITS (2 #b10)                       ;opc
         (3 #b011)
         (1 0)                          ;general
         (2 #b00)
         (19 `(QUOTIENT (- ,label *PC*) 4))
         (5 Rt)))
  ;; Register, no extend
  (((? Rt register-31=z) (? Rn register-31=sp) (? Rm register-31=z))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 #b10)                       ;opc
         (1 1)
         (5 Rm)
         (3 #b011)                      ;option=LSL
         (1 0)                          ;shift=0
         (5 Rn)
         (5 Rt)))
  ;; Extended register
  (((? Rt register-31=z)
    (? Rn register-31=sp)
    (? Rm register-31=z)
    (? option ldrsw-extend-type)
    (? S ldrsw-extend-amount))
   (BITS (2 #b10)                       ;size
         (3 #b111)
         (1 0)
         (2 #b00)
         (2 #b10)                       ;opc
         (1 1)
         (5 Rm)
         (3 option)
         (1 S)
         (2 #b10)
         (5 Rn)
         (5 Rt))))

;;; XXX not yet converted to section ordering, need to review syntax

(let-syntax
    ((define-adr-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op divisor) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ((X (? Rd register-31=z) (@PCR ,label))
               (BITS (1 ,op)
                     (2 `(QUOTIENT (- ,label *PC*) ,',divisor) IMMLO)
                     (1 1)
                     (4 #b0000)
                     (19 `(QUOTIENT (- ,label *PC*) ,',divisor) IMMHI)
                     (5 Rd)))))))))
  ;; PC-relative byte address
  (define-adr-instruction ADR 0 1)
  ;; PC-relative page address
  (define-adr-instruction ADRP 1 4096))

(define (extend-type t)
  (case t
    ((UXTB) #b000)
    ((UXTH) #b001)
    ((UXTW) #b010)
    ((UXTX) #b011)
    ((SXTB) #b100)
    ((SXTH) #b101)
    ((SXTW) #b110)
    ((SXTX) #b111)
    (else #f)))

(define (shift-type t)
  (case t
    ((LSL) #b00)
    ((LSR) #b01)
    ((ASR) #b10)
    (else #f)))

(let-syntax
    ((define-addsub-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op S register-31=dst Rd) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Extended register
              (((? sf sf-size)
                ,@(if Rd '() `((? Rd ,register-31=dst)))
                (? Rn register-31=sp)
                (? Rm register-31=z)
                (? option extend-type)
                (&U (? amount unsigned-2)))
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
                     (5 ,(or Rd 'Rd))))
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
                  (? Rm register-31=z)
                  (? type shift-type)
                  (? amount unsigned-5))
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
                  (? Rm register-31=z)
                  (? type shift-type)
                  (? amount unsigned-6))
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

;;; XXX wacky logical bit pattern encoding for immediates

(define (shiftror-type t)
  (case t
    ((LSL) #b00)
    ((LSR) #b01)
    ((ASR) #b10)
    ((ROR) #b11)
    (else #f)))

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
                   (&U (? imm logical-imm-32)))
                (BITS (1 0)           ;sf=0, 32-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (1 0)           ;N=0
                      (6 imm BITMASK32-IMMR)
                      (6 imm BITMASK32-IMMS)
                      (5 Rn)
                      (5 ,(or Rd 'Rd))))
               ;; Immediate, 64-bit operand size
               ((X ,@(if Rd '() '((? Rd register-31=sp)))
                   (? Rn register-31=z)
                   (&U (? imm logical-imm-64)))
                (BITS (1 1)           ;sf=1, 64-bit operand size
                      (2 ,opc)
                      (1 1)
                      (4 #b0010)
                      (1 0)
                      (1 imm BITMASK64-N)
                      (6 imm BITMASK64-IMMR)
                      (6 imm BITMASK64-IMMS)
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
                   (? Rm register-31=z)
                   (? type shiftror-type)
                   (? amount unsigned-5))
                (BITS (1 sf)
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
                   (? Rm register-31=z)
                   (? type shiftror-type)
                   (? amount unsigned-6))
                (BITS (1 sf)
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

(define (hw-shift32 shift)
  (and (exact-nonnegative-integer? shift)
       (let ((q (quotient shift 16))
             (r (remainder shift 16)))
         (and (zero? r)
              (< q 2)
              q))))

(define (hw-shift64 shift)
  (and (exact-nonnegative-integer? shift)
       (let ((q (quotient shift 16))
             (r (remainder shift 16)))
         (and (zero? r)
              (< q 4)
              q))))

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

(let-syntax
    ((define-bitfield-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ((W (? Rd register-31=z)
                  (? Rn register-31=z)
                  (&U (? r unsigned-5))
                  (&U (? s unsigned-5)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 0)              ;N, must match sf
                     (1 0)              ;high bit of r
                     (6 r)
                     (1 0)              ;high bit of s
                     (5 s)
                     (5 Rn)
                     (5 Rd)))
              ((X (? Rd register-31=z)
                  (? Rn register-31=z)
                  (&U (? r unsigned-6))
                  (&U (? s unsigned-6)))
               (BITS (1 0)              ;sf=1, 64-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 1)              ;N, must match sf
                     (6 r)
                     (6 s)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Signed bitfield move
  (define-bitfield-instruction SBFM #b00)
  ;; Bitfield move
  (define-bitfield-instruction BFM #b01)
  ;; Unsigned bitfield move
  (define-bitfield-instruction UBFM #b10))

(let-syntax
    ((define-shift-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc op2) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z)
                (? Rm register-31=z))
               (BITS (1 sf)
                     (1 0)
                     (1 0)
                     (1 1)
                     (4 #b1010)
                     (3 #b110)
                     (5 Rm)
                     (4 #b0010)
                     (2 ,op2)
                     (5 Rn)
                     (5 Rd)))
              ;; Alias for SBFM/UBFM, 32-bit operand size.
              ((W (? Rd register-31=z)
                  (? Rn register-31=z)
                  (&U (? shift unsigned-5)))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 0)              ;N, must match sf
                     (1 0)              ;high bit of r
                     (5 `(REMAINDER (- ,shift) 32))
                     (1 0)              ;high bit of s
                     (5 `(- 31 ,shift))
                     (5 Rn)
                     (5 Rd)))
              ;; Alias for SBFM/UBFM, 64-bit operand size.
              ((X (? Rd register-31=z)
                  (? Rn register-31=z)
                  (&U (? shift unsigned-6)))
               (BITS (1 1)              ;sf=1, 64-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 1)              ;N, must match sf
                     (6 `(REMAINDER (- ,shift) 64))
                     (6 `(- 63 ,shift))
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Arithmetic shift right (replicate sign bit), alias for SBFM
  (define-shift-instruction ASR #b00 #b10)
  ;; Logical shift left, alias for UBFM
  (define-shift-instruction LSL #b10 #b00)
  ;; Logical shift right (fill with zeros), alias for UBFM
  (define-shift-instruction LSR #b10 #b01))

(let-syntax
    ((define-signed-extend-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc r s) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Alias for SBFM with fixed r and s.
              (((? sf sf-size)
                (? Rd register-31=z)
                (? Rn register-31=z))
               (BITS (1 sf)
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 sf)             ;N, must match sf
                     (6 ,r)
                     (6 ,s)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Sign-extend byte (8-bit), alias for SBFM
  (define-signed-extend-instruction SXTB #b00 0 7)
  ;; Sign-extend halfword (16-bit), alias for SBFM
  (define-signed-extend-instruction SXTH #b00 0 15)
  ;; Sign-extend word (32-bit), alias for SBFM
  (define-signed-extend-instruction SXTW #b00 0 31))

(let-syntax
    ((define-unsigned-extend-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc r s) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; Alias for UBFM with fixed r and s.
              ;;
              ;; Limited to 32-bit because the top 32 bits are always
              ;; zero'd anyway.  Not that it would be a problem to
              ;; support this, since the instruction encoding is there,
              ;; but the official assembler syntax doesn't support it
              ;; and maybe it's a mistake if you try to use it.
              ((W (? Rd register-31=z)
                  (? Rn register-31=z))
               (BITS (1 0)              ;sf=0, 32-bit operand size
                     (2 ,opc)
                     (1 1)
                     (4 #b0011)
                     (1 0)
                     (1 0)              ;N, must match sf
                     (6 ,r)
                     (6 ,s)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Unsigned zero-extend byte (8-bit), alias for UBFM
  (define-unsigned-extend-instruction UXTB #b00 0 7)
  ;; Unsigned zero-extend halfword (16-bit), alias for UBFM
  (define-unsigned-extend-instruction UXTH #b00 0 15)
  ;; Unsigned zero-extend word (32-bit), nonexistent because any
  ;; word-sized write to a destination register will zero the high 32
  ;; bits.
  #;
  (define-unsigned-extend-instruction UXTW #b00 0 31))

(let-syntax
    ((define-bitfield-insert/extract-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc r32 r64 s #!optional register-31=src Rn)
                  (apply values (cdr form))
           (define (default def x) (if (default-object? x) def x))
           (let ((register-31=src (default register-31=z register-31=src))
                 (Rn (default #f Rn)))
             `(define-instruction ,mnemonic
                ((W (R (? Rd register-31=z))
                    ,@(if Rn '() `((? Rn ,register-31=src)))
                    (&U (? lsb unsigned-5))
                    (&U (? width unsigned-5+1)))
                 (BITS (1 0)            ;sf=0, 32-bit operand size
                       (2 ,opc)
                       (1 1)
                       (4 #b0011)
                       (1 0)
                       (1 0)            ;N, must match sf
                       (6 ,r32)
                       (6 ,s)
                       (5 ,(or Rn 'Rn))
                       (5 Rd)))
                ((X (? Rd register-31=z)
                    ,@(if Rn '() `((? Rn ,register-31=src)))
                    (&U (? lsb unsigned-5))
                    (&U (? width unsigned-5+1)))
                 (BITS (1 1)            ;sf=1, 32-bit operand size
                       (2 ,opc)
                       (1 1)
                       (4 #b0011)
                       (1 0)
                       (1 1)            ;N, must match sf
                       (6 ,r64)
                       (6 ,s)
                       (5 ,(or Rn 'Rn))
                       (5 Rd))))))))))
  ;; Signed bitfield extract, alias for SBFM
  (define-bitfield-insert/extract-instruction SBFX #b00
    lsb                                 ;r32
    lsb                                 ;r64
    `(- (+ ,lsb ,width) 1))             ;s
  ;; Unsigned bitfield extract, alias for UBFM
  (define-bitfield-insert/extract-instruction UBFX #b10
    lsb                                 ;r32
    lsb                                 ;r64
    `(- (+ ,lsb ,width) 1))             ;s
  ;; Signed bitfield insert in zeros, alias for SBFM
  (define-bitfield-insert/extract-instruction SFBIZ #b00
    `(REMAINDER (- ,lsb) 32)            ;r32
    `(REMAINDER (- ,lsb) 64)            ;r64
    `(- ,width 1))                      ;s
  ;; Bitfield extract and insert low copies
  (define-bitfield-insert/extract-instruction BFXIL #b01
    `(REMAINDER (- ,lsb) 32)            ;r32
    `(REMAINDER (- ,lsb) 64)            ;r64
    (- width 1))                        ;s
  ;; Bitfield insert: copy <width> bits at <lsb> from source
  (define-bitfield-insert/extract-instruction BFI #b01
    `(REMAINDER (- ,lsb) 32)            ;r32
    `(REMAINDER (- ,lsb) 64)            ;r64
    `(- ,width 1)                       ;s
    register<31)                        ;Rn must not be 31
  ;; Bitfield clear: clear <width> bit positions at <lsb>
  (define-bitfield-insert/extract-instruction BFC #b01
    `(REMAINDER (- ,lsb) 32)            ;r32
    `(REMAINDER (- ,lsb) 64)            ;r64
    `(- ,width 1)                       ;s
    #f 31)                              ;Rn is 31
  (define-bitfield-insert/extract-instruction UFBIZ #b10
    `(REMAINDER (- ,lsb) 32)            ;r32
    `(REMAINDER (- ,lsb) 64)            ;r64
    `(- ,width 1)))                     ;s

(let-syntax
    ((define-extract-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (let ((mnemonic (cadr form))
               (op21 (caddr form))
               (o0 (cadddr form))
               (m=n? (and (pair? (cddddr form)) (car (cddddr form)))))
           `(define-instruction ,mnemonic
              ((W (? Rd)
                  (? Rn)
                  ,@(if m=n? '() '((? Rm)))
                  (&U (? s unsigned-5)))
               (BITS (1 0)              ;sf=0
                     (2 ,op21)
                     (1 1)
                     (4 #b0011)
                     (1 1)
                     (1 sf)             ;N, must match sf
                     (1 ,o0)
                     (5 ,(if m=n? 'Rn 'Rm))
                     (1 0)              ;high bit of lsb index, 0 for 32-bit
                     (5 s)
                     (5 Rn)
                     (5 Rd)))
              ((X (? Rd)
                  (? Rn)
                  ,@(if m=n? '() '((? Rm)))
                  (&U (? s unsigned-6)))
               (BITS (1 0)              ;sf=0
                     (2 ,op21)
                     (1 1)
                     (4 #b0011)
                     (1 1)
                     (1 sf)             ;N, must match sf
                     (1 ,o0)
                     (5 ,(if m=n? 'Rn 'Rm))
                     (6 s)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; Extract register from pair of registers at bit offset
  (define-extract-instruction EXTR #b00 0)
  ;; Rotate right
  (define-extract-instruction ROR #b00 0 #t))

;; Carry flag invert

(define-instruction CFINV
  (()
   (BITS (8 #b11010101)
         (8 #b00000000)
         (8 #b01000000)
         (8 #b00011111))))

;; XXX advanced SIMD load/store multiple

(define (signed-7*4 x)
  (and (<= -256 x 252)
       (zero? (remainder x 4))
       (quotient x 4)))

(let-syntax
    ((define-load/store-pair-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic L) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; No write-back, no increment.
              (((? sf sf-size)
                (? Rt1 register-31=z)
                (? Rt2 register-31=z)
                (? Rn register-31=sp))
               (BITS (1 sf)
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (3 #b010)
                     (1 ,L)
                     (7 0)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))
              ;; No write back, signed increment.
              (((? sf sf-size)
                (? Rt1 register-31=z)
                (? Rt2 register-31=z)
                (+ (? Rn register-31=sp)) (& (? imm signed-7*4)))
               (BITS (1 sf)
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (3 #b010)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt1)))
              ;; Pre-index signed offset.
              (((? sf sf-size)
                (? Rt1 register-31=z)
                (? Rt2 register-31=z)
                (PRE+ (? Rn register-31=sp) (& (? imm signed-7*4))))
               (BITS (1 sf)
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (3 #b011)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt)))
              ;; Post-index signed offset.
              (((? sf sf-size)
                (? Rt1 register-31=z)
                (? Rt2 register-31=z)
                (POST+ (? Rn register-31=sp) (& (? imm signed-7*4))))
               (BITS (1 sf)
                     (1 0)              ;opc[1]
                     (3 #b101)
                     (1 0)
                     (3 #b001)
                     (1 ,L)
                     (7 imm SIGNED)
                     (5 Rt2)
                     (5 Rn)
                     (5 Rt)))))))))
  (define-load/store-pair-instruction LDP 1)
  (define-load/store-pair-instruction STP 1))

(define (load/store-size sz)
  (case sz
    ((B) #b00)
    ((H) #b01)
    ((W) #b10)
    ((X) #b11)
    (else #f)))

(let-syntax
    ((define-load/store-exclusive-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic L o2 o1 o0) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? sz load/store-size)
                (? Rs register-31=z)
                (? Rt register-31=z)
                (? Rn register-31=sp))
               (BITS (2 size)
                     (2 #b00)
                     (4 #b1000)
                     (1 ,o2)
                     (1 ,L)
                     (1 ,o1)
                     (5 Rs)
                     (1 ,o0)
                     (5 31)
                     (5 Rn)
                     (5 Rt)))))))))
  ;; Store exclusive register
  (define-load/store-exclusive-instruction STXR 0 0 0 0)
  ;; Store-release exclusive register
  (define-load/store-exclusive-instruction STLXR 0 0 0 1)
  ;; Load exclusive register
  (define-load/store-exclusive-instruction LDXR 1 0 0 0)
  ;; Load-acquire exclusive register
  (define-load/store-exclusive-instruction LDLXR 1 0 0 1)
  ;; Store LORelease register
  (define-load/store-exclusive-instruction STLLR 0 1 0 0)
  ;; Store-release register
  (define-load/store-exclusive-instruction STLR 0 1 0 1)
  ;; Load LOAcquire register
  (define-load/store-exclusive-instruction LDLAR 1 1 0 0)
  ;; Load-acquire register
  (define-load/store-exclusive-instruction LDAR 1 1 0 1))
