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

;;;; AArch64 Instruction Set, part 2
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;; XXX not yet converted to section ordering, need to review syntax

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
                     (5 r)
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
                     (5 (modulo (- 0 shift) 32))
                     (1 0)              ;high bit of s
                     (5 (- 31 shift))
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
                     (6 (modulo (- 0 shift) 64))
                     (6 (- 63 shift))
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
         (define-syntax receive
           (syntax-rules ()
             ((RECEIVE bvl expression body0 body1+ ...)
              (CALL-WITH-VALUES (LAMBDA () expression)
                (LAMBDA bvl body0 body1+ ...)))))
         (receive (mnemonic opc r32 r64 s #!optional register-31=src Rn)
                  (apply values (cdr form))
           (define (default def x) (if (default-object? x) def x))
           (let ((register-31=src (default 'register-31=z register-31=src))
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
                    (&U (? lsb unsigned-6))
                    (&U (? width unsigned-6+1)))
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
    (- (+ lsb width) 1))                ;s
  ;; Unsigned bitfield extract, alias for UBFM
  (define-bitfield-insert/extract-instruction UBFX #b10
    lsb                                 ;r32
    lsb                                 ;r64
    (- (+ lsb width) 1))                ;s
  ;; Signed bitfield insert in zeros, alias for SBFM
  (define-bitfield-insert/extract-instruction SFBIZ #b00
    (modulo (- 0 lsb) 32)               ;r32
    (modulo (- 0 lsb) 64)               ;r64
    (- width 1))                        ;s
  ;; Bitfield extract and insert low copies
  (define-bitfield-insert/extract-instruction BFXIL #b01
    (modulo (- 0 lsb) 32)               ;r32
    (modulo (- 0 lsb) 64)               ;r64
    (- width 1))                        ;s
  ;; Bitfield insert: copy <width> bits at <lsb> from source
  (define-bitfield-insert/extract-instruction BFI #b01
    (modulo (- 0 lsb) 32)               ;r32
    (modulo (- 0 lsb) 64)               ;r64
    (- width 1)                         ;s
    register<31)                        ;Rn must not be 31
  ;; Bitfield clear: clear <width> bit positions at <lsb>
  (define-bitfield-insert/extract-instruction BFC #b01
    (modulo (- 0 lsb) 32)               ;r32
    (modulo (- 0 lsb) 64)               ;r64
    (- width 1)                         ;s
    #f 31)                              ;Rn is 31
  (define-bitfield-insert/extract-instruction UFBIZ #b10
    (modulo (- 0 lsb) 32)               ;r32
    (modulo (- 0 lsb) 64)               ;r64
    (- width 1)))                       ;s

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
                     (1 0)              ;N, must match sf
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
                     (1 0)              ;N, must match sf
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
               (BITS (2 sz)
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

;;; Local Variables:
;;; eval: (put 'variable-width 'scheme-indent-function 2)
;;; End:
