#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; AMD x86-64 128-bit Media Instruction Set
;;; package: (compiler lap-syntaxer)

;;; The mnemonics here don't entirely match the ones in the AMD
;;; manual, or in your typical x86-64 assembler.  These mnemonics try
;;; to adhere to a convention of treating operand sizes, precisions,
;;; and packed/scalar choices as arguments to a common mnemonic, where
;;; it is sensible for there to be a choice.  Sometimes this is not
;;; entirely clear, such as PSHUF (which works only with longword-size
;;; (32-bit) operands) and PSHUFH/PSHUFL (which work only with
;;; word-size (16-bit) operands).  And sometimes this doesn't work
;;; very well (e.g., MOVQ).  Most instructions for floating-point
;;; arithmetic have F suffixed to their names; e.g., rather than
;;; ADDSS, ADDSD, ADDPS, and ADDPD, there's a single ADDF mnemonic, to
;;; be used as (ADDF S S ...), (ADDF S D ...), &c.
;;;
;;; Would it have been better just to transcribe exactly the mnemonics
;;; in the AMD manual?  Perhaps, and it might have caused fewer errors
;;; in transcription, since there would be fewer different formats
;;; that way.

(declare (usual-integrations))

(let-syntax
    ((define-flop-instruction
      (sc-macro-transformer
       (lambda (form environment)
         environment                    ;ignore
         (let ((mnemonic (cadr form))
               (opcode (caddr form)))
           `(define-instruction ,mnemonic
              (((? p/s float-packed/scalar)
                (? p float-precision)
                (XMM (? target))
                (? source xmm/m-ea))
               (PREFIX (FLOAT p/s p) (ModR/M target source))
               (BITS (8 #x0F)
                     (8 ,opcode))
               (ModR/M target source))))))))
  (define-flop-instruction ADDF #x58)
  (define-flop-instruction DIVF #x5E)
  (define-flop-instruction MAXF #x5F)
  (define-flop-instruction MINF #x5D)
  (define-flop-instruction MULF #x59)
  (define-flop-instruction SQRTF #x51)
  (define-flop-instruction SUBF #x5C))

(let-syntax ((define-packed-flop-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      ((P D (XMM (? target)) (? source xmm/m-ea))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 #xD0))
                       (ModR/M target source))

                      ((P S (XMM (? target)) (? source xmm/m-ea))
                       (BITS (8 #xF2))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 #xD0))
                       (ModR/M target source))))))))
  (define-packed-flop-instruction ADDSUBF       #xD0)
  (define-packed-flop-instruction HADDF         #x7C)
  (define-packed-flop-instruction HSUBF         #x7D))

(let-syntax ((define-packed-bitwise-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      ((P
                        (? p float-precision)
                        (XMM (? target))
                        (? source xmm/m-ea))
                       (PREFIX (FLOAT 'P p) (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M target source))))))))
  (define-packed-bitwise-instruction ANDNF      #x55)
  (define-packed-bitwise-instruction ANDF       #x54)
  (define-packed-bitwise-instruction ORF        #x56)
  (define-packed-bitwise-instruction XORF       #x57)
  ;; Not really bitwise instruction, but these two fit the pattern.
  (define-packed-bitwise-instruction UNPCKHF    #x15)
  (define-packed-bitwise-instruction UNPCKLF    #x15))

(define-instruction CMPF
  (((? comparator float-comparator)
    (? p/s float-packed/scalar)
    (? p float-precision)
    (XMM (? source1))
    (? source2 xmm/m-ea))
   (PREFIX (FLOAT p/s p) (ModR/M source1 source2))
   (BITS (8 #x0F)
         (8 #xC2))
   (ModR/M source1 source2)
   (BITS (8 comparator))))

(let-syntax ((define-un/ordered-compare-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      ((S D (XMM (? source1)) (? source2 xmm/m-ea))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M source1 source2))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M source1 source2))

                      ((S S (XMM (? source1)) (? source2 xmm/m-ea))
                       (PREFIX (ModR/M source1 source2))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M source1 source2))))))))
  ;; What does the `I' stand for?
  (define-un/ordered-compare-instruction COMIF #x2F)
  (define-un/ordered-compare-instruction UCOMIF #x2E))

(let-syntax ((define-conversion-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (rules-definitions (cddr form)))
                   `(define-instruction ,mnemonic
                      ,@(append-map
                         (lambda (rules-definition)
                           (if (not (eq? (car rules-definition) 'DEFINE-RULES))
                               (error "Malformed conversion rules definition:"
                                      rules-definition))
                           (let ((pattern (cadr rules-definition))
                                 (prefix-options (caddr rules-definition))
                                 (rules (cdddr rules-definition)))
                             (map (lambda (rule)
                                    (let ((conversion (car rule))
                                          (pre-prefix
                                           (if (cadr rule)
                                               `((BITS (8 ,(cadr rule))))
                                               '()))
                                          (bytes
                                           (map (lambda (byte) `(8 ,byte))
                                                (cddr rule))))
                                      `((,conversion ,@pattern)
                                        ,@pre-prefix
                                        (PREFIX ,@prefix-options
                                                (ModR/M reg ea))
                                        (BITS ,@bytes)
                                        (ModR/M reg ea))))
                                  rules)))
                         rules-definitions)))))))

  (define-conversion-instruction CVTF
    (define-rules ((XMM (? reg)) (? ea xmm/m-ea))
        ()
      (DQ->PD   #xF3 #x0F #xE6)
      (DQ->PS   #f   #x0F #x5B)
      (PD->DQ   #xF2 #x0F #xE6)
      (PD->PS   #x66 #x0F #x5A)
      (PS->DQ   #x66 #x0F #x5B)
      (PS->PD   #f   #x0F #x5A)
      (SD->SS   #xF2 #x0F #x5A)
      (SS->SD   #xF3 #x0F #x5A))

    ;++ SIZE can be only L or Q, not W.
    (define-rules ((? size operand-size) (R (? reg)) (? ea xmm/m-ea))
        ((OPERAND size))
      (SD->SI   #xF2 #x0F #x2D)
      (SS->SI   #xF3 #x0F #x2D))

    (define-rules ((? size operand-size) (XMM (? reg)) (? ea r/m-ea))
        ((OPERAND size))
      (SI->SD   #xF2 #x0F #x2A)
      (SI->SS   #xF3 #x0F #x2A)))

  (define-conversion-instruction CVTFT  ;Convert Truncated
    (define-rules ((XMM (? reg)) (? ea xmm/m-ea))
        ()
      (PD->DQ   #x66 #x0F #xE6)
      (PS->DQ   #xF3 #x0F #x5B))
    (define-rules ((? size operand-size) (R (? reg)) (? ea xmm/m-ea))
        ()
      (SD->SI   #xF2 #x0F #x2C)
      (SS->SI   #xF3 #x0F #x2C))))

(define-instruction EXTRQ               ;SSE4A only
  (((? target xmm-ea)
    (&U (? size unsigned-5bit))
    (&U (? position unsigned-5bit)))
   (BITS (8 #x66))
   (PREFIX (ModR/M target))
   (BITS (8 #x0F)
         (8 #x78))
   (ModR/M 0 target)
   (BITS (8 size UNSIGNED)
         (8 position UNSIGNED)))

  (((XMM (? target)) (? source xmm-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x79))
   (ModR/M target source)))

(define-instruction INSERTQ             ;SSE4A only
  (((XMM (? target))
    (? source xmm-ea)
    (&U (? size unsigned-5bit))
    (&U (? position unsigned-5bit)))
   (BITS (8 #xF2))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x78))
   (ModR/M target source)
   (BITS (8 size UNSIGNED)
         (8 position UNSIGNED)))

  (((XMM (? target)) (? source xmm-ea))
   (BITS (8 #xF2))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x79))
   (ModR/M target source)))

(define-instruction FXRSTOR
  (((? source m-ea))
   (PREFIX (ModR/M source))
   (BITS (8 #x0F)
         (8 #xAE))
   (ModR/M 1 source)))

(define-instruction FXSAVE
  (((? target m-ea))
   (PREFIX (ModR/M target))
   (BITS (8 #x0F)
         (8 #xAE))
   (ModR/M 0 target)))

;;; How does LDDQU differ from MOVDQU?

(define-instruction LDDQU               ;Load Double Quadword Unaligned
  (((XMM (? target)) (? source m-ea))
   (BITS (8 #xF2))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xF0))
   (ModR/M target source)))

(define-instruction LDMXCSR
  (((? source m-ea))
   (PREFIX (ModR/M source))
   (BITS (8 #x0F)
         (8 #xAE))
   (ModR/M 2 source)))

(define-instruction MASKMOVDQU
  (((@R 7) (XMM (? source1)) (? source2 xmm-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M source1 source2))
   (BITS (8 #x0F)
         (8 #xF7))
   (ModR/M source1 source2)))

(define-instruction MOVAF               ;Aligned
  ((P (? p float-precision) (XMM (? target)) (? source xmm/m-ea))
   (PREFIX (FLOAT 'P p) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x28))
   (ModR/M target source))

  ((P (? p float-precision) (? target xmm/m-ea) (XMM (? source)))
   (PREFIX (FLOAT 'P p) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #x29))
   (ModR/M source target)))

(define-instruction MOVD
  ;++ SIZE can be only L or Q, not W.
  (((? size operand-size) (XMM (? target)) (? source r/m-ea))
   (BITS (8 #x66))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x6E))
   (ModR/M target source))

  (((? size operand-size) (? target r/m-ea) (XMM (? source)))
   (BITS (8 #x66))
   (PREFIX (OPERAND size) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #x7E))
   (ModR/M source target)))

(define-instruction MOVDDUP
  (((XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #xF2))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x12))
   (ModR/M target source)))

(let-syntax ((define-move-dq-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (prefix (caddr form)))

                   `(define-instruction ,mnemonic
                      (((XMM (? target)) (? source xmm/m-ea))
                       (BITS (8 ,prefix))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 #x6F))
                       (ModR/M target source))

                      (((? target xmm/m-ea) (XMM (? source)))
                       (BITS (8 ,prefix))
                       (PREFIX (ModR/M source target))
                       (BITS (8 #x0F)
                             (8 #x7F))
                       (ModR/M source target))))))))
  (define-move-dq-instruction MOVDQA #x66)
  (define-move-dq-instruction MOVDQU #xF3))

(let-syntax ((define-move-high/low-instructions
              (sc-macro-transformer
               (lambda (form environment)
                 (let ((MOVxy (cadr form))
                       (MOVx (caddr form))
                       (opcode1 (cadddr form))
                       (opcode2 (car (cddddr form))))

                   `(begin
                      (define-instruction ,MOVxy
                        ((P S (XMM (? target)) (? source xmm-ea))
                         (PREFIX (ModR/M target source))
                         (BITS (8 #x0F)
                               (8 ,opcode1))
                         (ModR/M target source)))

                      (define-instruction ,MOVx
                        ((P D (XMM (? target)) (? source m-ea))
                         (BITS (8 #x66))
                         (PREFIX (ModR/M target source))
                         (BITS (8 #x0F)
                               (8 ,opcode2))
                         (ModR/M target source))

                        ((P D (? target m-ea) (XMM (? source)))
                         (BITS (8 #x66))
                         (PREFIX (ModR/M source target))
                         (BITS (8 #x0F)
                               (8 ,(+ opcode2 1)))
                         (ModR/M source target))

                        ((P S (XMM (? target)) (? source m-ea))
                         (PREFIX (ModR/M target source))
                         (BITS (8 #x0F)
                               (8 ,opcode2))
                         (ModR/M target source))

                        ((P S (? target m-ea) (XMM (? source)))
                         (PREFIX (ModR/M source target))
                         (BITS (8 #x0F)
                               (8 ,(+ opcode2 1)))
                         (ModR/M source target)))))))))
  ;; Note: (MOVL ...) is very different from (MOV L ...)!
  (define-move-high/low-instructions MOVHL MOVH #x12 #x16)
  (define-move-high/low-instructions MOVLH MOVL #x12 #x16))

(define-instruction MOVMSKF
  ((P (? p float-precision) (R (? target)) (? source xmm-ea))
   (PREFIX (FLOAT 'P p) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x50))
   (ModR/M target source)))

(define-instruction MOVNTDQ
  (((? target m-ea) (XMM (? source)))
   (PREFIX (FLOAT 'P 'D) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #xE7))
   (ModR/M source target)))

(define-instruction MOVNTF
  (((? p/s float-packed/scalar)
    (? p float-precision)
    (? target m-ea)
    (XMM (? source)))
   (PREFIX (FLOAT p/s p) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #x2B))
   (ModR/M source target)))

;; Note: (MOVQ ...) is very different from (MOV Q ...)!
(define-instruction MOVQ
  (((XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #xF3))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x7E))
   (ModR/M target source))

  (((? target xmm/m-ea) (XMM (? source)))
   (BITS (8 #x66))
   (PREFIX (ModR/M source target))
   (BITS (8 #x0F)
         (8 #xD6))
   (ModR/M source target)))

;;; Using the mnemonic MOVF avoids conflict with the general MOVS
;;; instruction.

(define-instruction MOVF
  ((S (? p float-precision) (XMM (? target)) (? source xmm/m-ea))
   (PREFIX (FLOAT 'S p) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x10))
   (ModR/M target source))

  ((S (? p float-precision) (? target xmm/m-ea) (XMM (? source)))
   (PREFIX (FLOAT 'S p) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #x11))
   (ModR/M source target)))

(let-syntax ((define-mov/dup-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      (((XMM (? target)) (? source xmm/m-ea))
                       (BITS (8 #xF3))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M target source))))))))
  (define-mov/dup-instruction MOVSHDUP #x16)
  (define-mov/dup-instruction MOVSLDUP #x12))

(define-instruction MOVUF               ;Unaligned
  ((P (? p float-precision) (XMM (? target)) (? source xmm/m-ea))
   (PREFIX (FLOAT 'P p) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x10))
   (ModR/M target source))
  ((P (? p float-precision) (? target xmm/m-ea) (XMM (? source)))
   (PREFIX (FLOAT 'P p) (ModR/M source target))
   (BITS (8 #x0F)
         (8 #x11))
   (ModR/M source target)))

(let-syntax ((define-packed-sized-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (size/opcode-list (cddr form)))
                   `(define-instruction ,mnemonic
                      ,@(map (lambda (size/opcode)
                               (let ((size (car size/opcode))
                                     (opcode (cadr size/opcode)))
                                 `((,size (XMM (? target)) (? source xmm/m-ea))
                                   (BITS (8 #x66))
                                   (PREFIX (ModR/M target source))
                                   (BITS (8 #x0F)
                                         (8 ,opcode))
                                   (ModR/M target source))))
                             size/opcode-list)))))))
  (define-packed-sized-instruction PACKS (B #x63) (UB #x67) (W #x6B))
  (define-packed-sized-instruction PADD (B #xFC) (W #xFD) (L #xFE) (Q #xD4))
  ;++ Should PADDU be considered a separate instruction from PADDS?
  (define-packed-sized-instruction PADDS (B #xEC) (W #xED) (UB #xDC) (UW #xDD))
  (define-packed-sized-instruction PAVG (B #xE0) (W #xE3))
  (define-packed-sized-instruction PCMPEQ (B #x74) (W #x75) (L #x76))
  (define-packed-sized-instruction PCMPGT (B #x64) (W #x65) (L #x66))
  (define-packed-sized-instruction PMAX (W #xEE) (UB #xDE))
  (define-packed-sized-instruction PMIN (W #xEA) (UB #xDA))
  (define-packed-sized-instruction PSUB (B #xF8) (W #xF9) (L #xFA) (Q #xFB))
  ;++ Should PSUBSU be considered a separate instruction from PSUBS?
  (define-packed-sized-instruction PSUBS (B #xE8) (W #xE9) (UB #xD8) (UW #xD9))
  ;++ Should the size indicate the source or target size?  Right now
  ;++ it indicates the source size.
  (define-packed-sized-instruction PUNPCKH (B #x68) (W #x69) (L #x6A) (Q #x6D))
  (define-packed-sized-instruction PUNPCKL (B #x60) (W #x61) (L #x62) (Q #x6C))
  )

(let-syntax ((define-packed-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      (((XMM (? target)) (? source xmm/m-ea))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M target source))))))))
  (define-packed-instruction PAND #xDB)
  (define-packed-instruction PANDN #xDF)
  (define-packed-instruction PMADDWL #xF5)
  (define-packed-instruction PMULUDQ #xF4)
  (define-packed-instruction POR #xEB)
  (define-packed-instruction PSADBW #xF6)
  (define-packed-instruction PXOR #xEF))

(define-instruction PEXTR
  ((W (R (? target)) (? source xmm-ea) (&U (? position unsigned-3bit)))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xC5))
   (ModR/M target source)
   (BITS (8 position UNSIGNED))))

(define-instruction PINSR
  ((W (XMM (? target)) (? source r-ea) (&U (? position unsigned-3bit)))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xC4))
   (ModR/M target source)
   (BITS (8 position UNSIGNED))))

(define-instruction PMOVMSKB
  (((R (? target)) (? source xmm-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xD7))
   (ModR/M target source)))

(define-instruction PMULH
  ((W (XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xE5))
   (ModR/M target source))

  ((UW (XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xE4))
   (ModR/M target source)))

(define-instruction PMULL
  ((W (XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xD5))
   (ModR/M target source)))

(define-instruction PSHUF
  ((L (XMM (? target)) (? source xmm/m-ea) (&U (? wibblethwop unsigned-byte)))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x70))
   (ModR/M target source)
   (BITS (8 wibblethwop))))

(define-instruction PSHUFH
  ((W (XMM (? target)) (? source xmm/m-ea) (&U (? zob unsigned-byte)))
   (BITS (8 #xF3))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x70))
   (ModR/M target source)
   (BITS (8 zob))))

;;; Note: (PSHUF L ...) is very different from (PSHUFL ...)!  (The
;;; latter must be (PSHUFL W ...) in any case.)

(define-instruction PSHUFL
  ((W (XMM (? target)) (? source xmm/m-ea) (&U (? veeblefitzer unsigned-byte)))
   (BITS (8 #xF3))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 #x70))
   (ModR/M target source)
   (BITS (8 veeblefitzer))))

(let-syntax ((define-shift-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 environment            ;ignore
                 (let ((mnemonic (cadr form))
                       (digit (caddr form))
                       (dq-digit (cadddr form))
                       (opcode (car (cddddr form)))
                       (size/opcode-list (cddddr form)))
                   `(define-instruction ,mnemonic
                      ((DQ (? target xmm-ea) (&U count unsigned-byte))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M target))
                       (BITS (8 #x0F)
                             (8 #x73))
                       (ModR/M ,dq-digit target)
                       (BITS (8 count UNSIGNED)))

                      (((? size operand-size)
                        (? target xmm-ea)
                        (&U (? count unsigned-byte)))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M target))
                       (BITS (8 #x0F)
                             (8 (case size ((Q) #x73) ((L) #x72) ((W) #x71))))
                       (ModR/M ,digit target)
                       (BITS (8 count UNSIGNED)))

                      (((? size operand-size)
                        (XMM (? target))
                        (? source xmm/m-ea))
                       (BITS (8 #x66))
                       (PREFIX (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 (case size
                                  ((Q) ,(+ opcode 1))
                                  ((L) ,(+ opcode 2))
                                  ((W) ,(+ opcode 3)))))
                       (ModR/M target source))))))))
  (define-shift-instruction PSLL 6 7 #xF0)
  (define-shift-instruction PSRL 2 3 #xD0))

(define-instruction PSRA
  ;++ This does not admit an operand size of Q.
  (((? size operand-size) (? target xmm-ea) (&U (? count unsigned-byte)))
   (BITS (8 #x66))
   (PREFIX (ModR/M target))
   (BITS (8 #x0F)
         (8 (case size ((L) #x72) ((W) #x71))))
   (ModR/M 4 target)
   (BITS (8 count UNSIGNED)))

  (((? size operand-size) (XMM (? target)) (? source xmm/m-ea))
   (BITS (8 #x66))
   (PREFIX (ModR/M target source))
   (BITS (8 #x0F)
         (8 (case size ((L) #xE2) ((W) #xE1))))
   (ModR/M target source)))

(let-syntax ((define-reciprocal-instruction
              (sc-macro-transformer
               (lambda (form environment)
                 (let ((mnemonic (cadr form))
                       (opcode (caddr form)))
                   `(define-instruction ,mnemonic
                      (((? p/s float-packed/scalar)
                        S               ;Single-precision only.
                        (XMM (? target))
                        (? source xmm/m-ea))
                       (PREFIX (FLOAT p/s 'S) (ModR/M target source))
                       (BITS (8 #x0F)
                             (8 ,opcode))
                       (ModR/M target source))))))))
  (define-reciprocal-instruction RCPF #x53)
  (define-reciprocal-instruction RSQRTF #x52))

(define-instruction SHUF
  ((P (? p float-precision)
      (XMM (? target))
      (? source xmm/m-ea)
      (&U (? command unsigned-2bit)))
   (PREFIX (FLOAT 'P p) (ModR/M target source))
   (BITS (8 #x0F)
         (8 #xC6))
   (ModR/M target source)
   (BITS (8 command UNSIGNED))))

(define-instruction STMXCSR
  (((? target m-ea))
   (PREFIX (ModR/M target))
   (BITS (8 #x0F)
         (8 #xAE))
   (ModR/M 3 target)))
