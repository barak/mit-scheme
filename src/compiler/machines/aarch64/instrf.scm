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

;;;; AArch64 SIMD and Floating-Point Instruction Set
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

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
              (((? sz:opchi load/store-simd/fp-size)
                (? Vt vregister)
                ((? pre/post load/store-pre/post-index)
                 (? Rn register-31=sp)
                 (& (? offset signed-9))))
               (BITS (2 (fix:lsh sz:opchi -1))
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 (fix:and 1 sz:opchi)) ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 0)
                     (9 offset SIGNED)
                     (2 pre/post)
                     (5 Rn)
                     (5 Vt)))
              ;; LDR immediate, SIMD&FP, zero offset (C7.2.176)
              ;; STR immediate, SIMD&FP, zero offset (C7.2.315)
              (((? sz:opchi load/store-simd/fp-size)
                (? Vt vregister)
                (? Rn register-31=sp))
               (BITS (2 (fix:lsh sz:opchi -1))
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b01)
                     (1 (fix:and 1 sz:opchi)) ;opc[1]
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
              (((? sz:opchi load/store-simd/fp-size)
                (? Vt vregister)
                (+ (? Rn register-31=sp)
                   (? Rm register-31=z)))
               (BITS (2 (fix:lsh sz:opchi -1))
                     (3 #b111)
                     (1 1)              ;SIMD/FP
                     (2 #b00)
                     (1 (fix:and 1 sz:opchi)) ;opc[1]
                     (1 ,load/store)    ;opc[0]
                     (1 1)
                     (5 Rm)
                     (3 #b011)          ;option=LSL
                     (1 0)              ;shift=0
                     (2 #b10)
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
    (((? opc ldr-literal-simd/fp-size)
      (? Vt vregister)
      (@PCO (? offset signed-19)))
     (BITS (2 opc)
           (3 #b011)
           (1 1)                        ;SIMD/FP
           (2 #b00)
           (19 offset SIGNED)
           (5 Vt)))
    (((? size) (? Vt) (@PCR (? label) (? temp register<31)))
     (VARIABLE-WIDTH offset `(/ (- ,label *PC*) 4)
       ((#x-40000 #x3ffff)
        (MACRO 32 (LDR.V ,size ,Vt (@PCO (* 4 ,offset)))))
       ((#x-100000000 #xffffffff)
        (MACRO 64 (ADRP-ADD X ,temp (@PCO ,(* 4 offset)))) ;(*)
        (MACRO 32 (LDR.V X ,Vt ,temp)))))))

(let-syntax
    ((define-aes-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opcode) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? Rd vregister) (? Rn vregister))
               (BITS (8 #b01001110)
                     (2 #b00)           ;size
                     (5 #b10100)
                     (5 ,opcode)
                     (2 #b10)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-aes-instruction AESE   #b00100)
  (define-aes-instruction AESD   #b00101)
  (define-aes-instruction AESMC  #b00110)
  (define-aes-instruction AESIMC #b00111))

(let-syntax
    ((define-sha-3reg-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opcode) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? Rd vregister) (? Rn vregister) (? Rm vregister))
               (BITS (8 #b01011110)
                     (2 #b00)           ;size
                     (1 0)
                     (5 Rm)
                     (1 0)
                     (3 ,opcode)
                     (2 #b00)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-sha-3reg-instruction SHA1C     #b000)
  (define-sha-3reg-instruction SHA1P     #b001)
  (define-sha-3reg-instruction SHA1M     #b010)
  (define-sha-3reg-instruction SHA1SU0   #b011)
  (define-sha-3reg-instruction SHA256H   #b100)
  (define-sha-3reg-instruction SHA256H2  #b101)
  (define-sha-3reg-instruction SHA256SU1 #b110))

(let-syntax
    ((define-sha-2reg-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opcode) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? Rd vregister) (? Rn vregister))
               (BITS (8 #b01011110)
                     (2 #b00)           ;size
                     (5 #b10100)
                     (5 ,opcode)
                     (2 #b10)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-sha-2reg-instruction SHA1H     #b00000)
  (define-sha-2reg-instruction SHA1SU1   #b00001)
  (define-sha-2reg-instruction SHA256SU0 #b00010))

(let-syntax
    ((define-fp-unary-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opcode-s U a opcode-v) (apply values (cdr form))
           `(define-instruction ,mnemonic
              ;; scalar
              (((? type fp-scalar-size) (? Rd vregister) (? Rn vregister))
               (BITS (1 0)              ;M=0
                     (1 0)              ;?
                     (1 0)              ;S=0
                     (5 #b11110)
                     (2 type)
                     (1 1)
                     (6 ,opcode-s)
                     (5 #b10000)
                     (5 Rn)
                     (5 Rd)))
              ;; vector
              (((? sz:Q fp-vector-size)
                (? Rd vregister)
                (? Rn vregister))
               (BITS (1 0)              ;M=0
                     (1 (fix:and sz:Q 1)) ;Q
                     (1 ,U)
                     (5 #b01110)
                     (1 ,a)
                     (6 (fix:lsh sz:Q -1)) ;sz
                     (5 ,opcode-v)
                     (2 #b10)
                     (5 Rn)
                     (5 Rd)))))))))
  ;; arithmetic
  (define-fp-unary-instruction FABS   #b000001 0 1 #b01111)
  (define-fp-unary-instruction FNEG   #b000010 1 1 #b01111)
  (define-fp-unary-instruction FSQRT  #b000011 1 1 #b11111)
  ;; rounding
  (define-fp-unary-instruction FRINTN #b001000 0 0 #b11000)
  (define-fp-unary-instruction FRINTP #b001001 0 1 #b11000)
  (define-fp-unary-instruction FRINTM #b001010 0 0 #b11001)
  (define-fp-unary-instruction FRINTZ #b001011 0 1 #b11001)
  (define-fp-unary-instruction FRINTA #b001100 1 0 #b11000)
  (define-fp-unary-instruction FRINTX #b001110 1 0 #b11001)
  (define-fp-unary-instruction FRINTI #b001111 1 1 #b11001))

(let-syntax
    ((define-fp-binary-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opcode-s U a S opcode-v) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? type fp-scalar-size)
                (? Rd vregister)
                (? Rn vregister)
                (? Rm vregister))
               (BITS (1 0)              ;M
                     (1 0)
                     (1 0)              ;S
                     (5 #b11110)
                     (2 type)
                     (1 1)
                     (5 Rm)
                     (4 ,opcode-s)
                     (2 #b10)
                     (5 Rn)
                     (5 Rd)))
              (((? Q fp16-vector-size)
                (? Rd vregister)
                (? Rn vregister)
                (? Rm vregister))
               (BITS (1 0)
                     (1 Q)
                     (1 ,U)
                     (5 #b01110)
                     (1 ,a)
                     (1 0)
                     (1 1)
                     (5 Rm)
                     (2 #b00)
                     (4 ,opcode-v)
                     (5 Rn)
                     (5 Rd)))
              (((? sz:Q fp32/64-vector-size)
                (? Rd vregister)
                (? Rn vregister)
                (? Rm vregister))
               (BITS (1 0)
                     (1 (fix:and sz:Q 1))  ;Q
                     (1 ,U)
                     (5 #b01110)
                     (1 ,S)
                     (1 (fix:lsh sz:Q -1)) ;sz
                     (1 1)
                     (5 Rm)
                     (2 #b11)
                     (4 ,opcode-v)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-fp-binary-instruction FMUL   #b0000 1 0 0 #b011)
  (define-fp-binary-instruction FDIV   #b0001 1 0 0 #b111)
  (define-fp-binary-instruction FADD   #b0010 0 0 0 #b010)
  (define-fp-binary-instruction FSUB   #b0011 0 1 1 #b010)
  (define-fp-binary-instruction FMAX   #b0100 0 0 0 #b110)
  (define-fp-binary-instruction FMIN   #b0101 0 1 1 #b110)
  (define-fp-binary-instruction FMAXNM #b0110 0 0 0 #b000)
  (define-fp-binary-instruction FMINNM #b0111 0 1 1 #b000))

(let-syntax
    ((define-fp-ternary-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic o1 o0) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? type fp-scalar-size)
                (? Rd vregister)
                (? Rn vregister)
                (? Rm vregister)
                (? Ra vregister))
               (BITS (1 0)
                     (1 0)
                     (1 0)
                     (5 #b11111)
                     (2 type)
                     (1 ,o1)
                     (5 Rm)
                     (1 ,o0)
                     (5 Ra)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-fp-ternary-instruction FMADD 0 0)
  (define-fp-ternary-instruction FMSUB 0 1)
  (define-fp-ternary-instruction FNMADD 1 0)
  (define-fp-ternary-instruction FNMSUB 1 1))

(let-syntax
    ((define-fp-compare-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic opc-hi) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? type fp-scalar-size) (? Rn vregister) Z)
               (BITS (1 0)              ;M
                     (1 0)
                     (1 0)              ;S
                     (5 #b11110)
                     (2 type)
                     (1 1)
                     (5 #b0000)         ;Rm = #0.0
                     (2 #b00)
                     (4 #b1000)
                     (5 Rn)
                     (1 ,opc-hi)
                     (1 1)              ;zero variant
                     (3 #b000)))
              (((? type fp-scalar-size) (? Rn vregister) (? Rm vregister))
               (BITS (1 0)              ;M
                     (1 0)
                     (1 0)              ;S
                     (5 #b11110)
                     (2 type)
                     (1 1)
                     (5 Rm)
                     (2 0)
                     (4 #b1000)
                     (5 Rn)
                     (1 ,opc-hi)
                     (1 0)              ;register variant
                     (3 #b000)))))))))
  ;; quiet compare
  (define-fp-compare-instruction FCMP 0)
  ;; compare and raise exceptions
  (define-fp-compare-instruction FCMPE 1))

(let-syntax
    ((define-fp-conditional-compare-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic op) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? type fp-scalar-size)
                (? Rn vregister)
                (? Rm vregister)
                (&U (? nzcv nzcv-value))
                (? cc branch-condition))
               (BITS (1 0)              ;M
                     (1 0)
                     (1 0)              ;S
                     (5 #b11110)
                     (2 type)
                     (1 1)
                     (5 Rm)
                     (4 cc)
                     (2 #b01)
                     (5 Rn)
                     (1 ,op)
                     (4 nzcv)))))))))
  ;; quiet compare
  (define-fp-conditional-compare-instruction FCCMP 0)
  ;; compare and raise exceptions
  (define-fp-conditional-compare-instruction FCCMPE 1))

(let-syntax
    ((define-simd-byte-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment
         (receive (mnemonic U opc2 opcode) (apply values (cdr form))
           `(define-instruction ,mnemonic
              (((? Q simd-byte-vector-size)
                (? Rd vregister)
                (? Rn vregister)
                (? Rm vregister))
               (BITS (1 0)
                     (1 Q)
                     (1 ,U)
                     (5 #b01110)
                     (2 ,opc2)
                     (1 1)
                     (5 Rm)
                     (5 ,opcode)
                     (1 1)
                     (5 Rn)
                     (5 Rd)))))))))
  (define-simd-byte-instruction BSL 1 #b01 #b00011)
  (define-simd-byte-instruction BIT 1 #b10 #b00011))

(define-instruction FMOV
  ;; vector, immediate (C7.2.122)
  (((? U:op:Q fmov-vector-immediate-size)
    (? Rd vregister)
    (&U (? abc fmov-abc) (? defgh fmov-defgh)))
   (BITS (1 0)
         (1 (fix:and 1 U:op:Q))
         (1 (fix:and 1 (fix:lsh U:op:Q -2)))
         (5 #b01111)
         (5 #b00000)
         (3 abc)
         (4 #b1111)
         (1 (fix:and 1 (fix:lsh U:op:Q -1)))
         (1 1)
         (5 defgh)
         (5 Rd)))
  (((? U:op:Q fmov-vector-immediate-size)
    (? Rd vregister)
    (&. (? abcdefgh fmov-abcdefgh)))
   (BITS (1 0)
         (1 (fix:and 1 U:op:Q))
         (1 (fix:and 1 (fix:lsh U:op:Q -2)))
         (5 #b01111)
         (5 #b00000)
         (3 (fix:lsh abcdefgh -5))
         (4 #b1111)
         (1 (fix:and 1 (fix:lsh U:op:Q -1)))
         (1 1)
         (5 (fix:and #b11111 abcdefgh))
         (5 Rd)))
  ;; register (C7.2.123)
  (((? type fp-scalar-size) (? Rd vregister) (? Rn vregister))
   (BITS (1 0)
         (1 0)
         (1 0)
         (5 #b11110)
         (2 type)
         (1 1)
         (4 #b0000)
         (2 #b00)                       ;opc
         (5 #b10000)
         (5 Rn)
         (5 Rd)))
  ;; general (C7.2.124)
  (((? sf sf-size) (? Rd register-31=z)
    (? rmode:type fmov-general-size) (? Rn vregister))
   (BITS (1 sf)
         (1 0)
         (1 0)
         (5 #b11110)
         (2 (fix:and #b11 rmode:type))
         (1 1)
         (2 (fix:lsh rmode:type -2))
         (3 #b110)                      ;opcode
         (6 #b000000)
         (5 Rn)
         (5 Rd)))
  (((? rmode:type fmov-general-size) (? Rd vregister)
    (? sf sf-size) (? Rn register-31=z))
   (BITS (1 sf)
         (1 0)
         (1 0)
         (5 #b11110)
         (2 (fix:and #b11 rmode:type))
         (1 1)
         (2 (fix:lsh rmode:type -2))
         (3 #b111)                      ;opcode
         (6 #b000000)
         (5 Rn)
         (5 Rd)))
  ;; scalar, immediate (C7.2.125)
  (((? type fp-scalar-size) (? Rd vregister) (&U (? imm8 unsigned-8)))
   (BITS (1 0)
         (1 0)
         (1 0)
         (5 #b11110)
         (2 type)
         (1 1)
         (8 imm8)
         (3 #b100)
         (5 #b00000)
         (5 Rd)))
  (((? type fp-scalar-size) (? Rd vregister) (&. (? imm8 fp-binary8)))
   (BITS (1 0)
         (1 0)
         (1 0)
         (5 #b11110)
         (2 type)
         (1 1)
         (8 imm8)
         (3 #b100)
         (5 #b00000)
         (5 Rd))))
