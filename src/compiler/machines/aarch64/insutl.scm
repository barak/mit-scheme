#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; AArch64 Instruction Set, utilities

(declare (usual-integrations))

(define (signed-7 x)
  (and (exact-integer? x)
       (<= #x-40 x #x3f)
       x))

(define (signed-9 x)
  (and (exact-integer? x)
       (<= #x-200 x #x1ff)
       x))

(define (signed-19 x)
  (and (exact-integer? x)
       (<= #x-40000 x #x3ffff)
       x))

(define (signed-21 x)
  (and (exact-integer? x)
       (<= #x-100000 x #xfffff)
       x))

(define (signed-26 x)
  (and (exact-integer? x)
       (<= #x-04000000 x #x03ffffff)
       x))

(define (signed-33 x)
  (and (exact-integer? x)
       (<= #x-100000000 x #xffffffff)
       x))

(define (unsigned-2 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x 3)
       x))

(define (unsigned-3 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x 7)
       x))

(define (unsigned-4 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xf)
       x))

(define (unsigned-5 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #x1f)
       x))

(define (unsigned-5+1 x)
  (and (exact-nonnegative-integer? x)
       (<= 1 x #x20)
       (- x 1)))

(define (unsigned-6 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #x3f)
       x))

(define (unsigned-6+1 x)
  (and (exact-nonnegative-integer? x)
       (<= 1 x #x40)
       x))

(define (unsigned-7 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #x7f)
       x))

(define (unsigned-8 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xff)
       x))

(define (unsigned-12 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xfff)
       x))

(define (unsigned-16 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xffff)
       x))

(define (branch-condition condition)
  ;; PSTATE condition bits:
  ;; .n = negative
  ;; .z = zero
  ;; .c = carry
  ;; .v = overflow
  ;; Branch if...
  (case condition
    ((EQ) #b0000)               ;equal (z)
    ((NE) #b0001)               ;not equal (!z)
    ((CS HS) #b0010)            ;carry set / unsigned higher|same (c)
    ((CC LO) #b0011)            ;carry clear / unsigned lower (!c)
    ((MI) #b0100)               ;negative `minus' (n)
    ((PL) #b0101)               ;nonnegative `plus' (!n)
    ((VS) #b0110)               ;overflow set (v)
    ((VC) #b0111)               ;overflow clear (!v)
    ((HI) #b1000)               ;carry&nonzero / unsigned higher (c & !z)
    ((LS) #b1001)               ;(!carry)|zero / unsigned lower|same (!c | z)
    ((GE) #b1010)               ;greater or equal (n = v)
    ((LT) #b1011)               ;less (n != v)
    ((GT) #b1100)               ;greater ((n = v) & !z)
    ((LE) #b1101)               ;less or equal ((n != v) | z)
    ((AL) #b1110)               ;always
    ;((<never>) #b1111)         ;never?
    (else #f)))

(define (invert-branch-condition condition)
  (case condition
    ((EQ) 'NE)
    ((NE) 'EQ)
    ((CS) 'CC)
    ((CC) 'CS)
    ((MI) 'PL)
    ((PL) 'MI)
    ((VS) 'VC)
    ((VC) 'VS)
    ((HI) 'LS)
    ((LS) 'HI)
    ((GE) 'LT)
    ((LT) 'GE)
    ((GT) 'LE)
    ((LE) 'GT)
    ((AL) 'NV)
    ((NV) 'AL)
    (else #f)))

(define (nzcv-value x)
  (and (exact-integer? x)
       (<= 0 x #xf)
       x))

(define (sf-size size)
  (case size
    ((W) 0)
    ((X) 1)
    (else #f)))

(define (vregister v)
  (and (exact-integer? v)
       (<= 0 v 31)
       v))

(define (register<31 r)
  (and (exact-integer? r)
       (<= 0 r 30)
       r))

(define (register-31=z r)
  (cond ((eq? r 'Z) 31)
        ((and (exact-nonnegative-integer? r) (<= 0 r 30)) r)
        (else #f)))

(define (register-31=sp r)
  (cond ((and (exact-nonnegative-integer? r) (<= 0 r 31)) r)
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

(define (ldr-literal-size sz)
  (case sz
    ;; No byte or halfword, only word and extended word.
    ((W) #b00)
    ((X) #b01)
    (else #f)))

(define (ldr-literal-simd/fp-size t)
  (case t
    ((S) #b00)
    ((D) #b01)
    ((Q) #b10)
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

(define (add/sub-extend-type t)
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

(define (add/sub-shift-type t)
  (case t
    ((LSL) #b00)
    ((LSR) #b01)
    ((ASR) #b10)
    (else #f)))

(define (logical-shift/rotate-type t)
  (case t
    ((LSL) #b00)
    ((LSR) #b01)
    ((ASR) #b10)
    ((ROR) #b11)
    (else #f)))

(define (logical-immediate-signed imm width)
  (and (<= (bit-antimask (- width 1) 0) imm (bit-mask (- width 1) 0))
       (logical-immediate-unsigned (bitwise-and imm (bit-mask width 0))
                                   width)))

(define (logical-immediate-unsigned imm width)
  (define (find-smallest-period)
    ;; Find the smallest candidate period, at least 2 since we need at
    ;; least one 1 and at least one 0.
    (let loop ((p width))
      (let* ((h (quotient p 2))
             (mask (bit-mask h 0)))
        (if (and (= (bitwise-and imm mask)
                    (bitwise-and (shift-right imm h) mask))
                 (> h 2))
            (loop h)
            p))))
  (define (generate period phase count)
    ;; Given the period, phase, and count of bits, compute and encode
    ;; the n, immr, and imms representation.
    (assert (< phase period))
    (let* ((immr (modulo (- period phase) width))
           (nimms (bitwise-ior (shift-left (- period) 1) (- count 1)))
           (n (bitwise-xor 1 (bitwise-and 1 (shift-right nimms 6))))
           (imms (bitwise-and nimms #x3f)))
      (encode n immr imms)))
  (define (encode n immr imms)
    ;; Given the n, immr, and imms fields, encode them as:
    ;; n(1) || immr(6) || imms(6)
    (assert (= n (bitwise-and n 1)))
    (assert (= immr (bitwise-and immr #x3f)))
    (assert (= imms (bitwise-and imms #x3f)))
    (bitwise-ior (shift-left n 12)
                 (bitwise-ior (shift-left immr 6) imms)))
  (define (contiguous-ones? x)
    ;; True if the one bits in x are contiguous.  E.g.,
    ;;
    ;;  00111000 - 1 = 001101111
    ;;  (00111000 - 1) | 00111000 = 00111111
    ;;  00111111 + 1 = 01000000
    ;;  (00111111 + 1) & 00111111 = 0
    (let* ((y (bitwise-ior (- x 1) x))
           (z (bitwise-and (+ y 1) y)))
      (zero? z)))
  (let ((wmask (bit-mask width 0)))
    (and (not (= imm 0))
         (not (= imm wmask))
         (= imm (bitwise-and imm wmask))
         (let* ((period (find-smallest-period))
                (pmask (bit-mask period 0)))
           (let ((imm+ (bitwise-and imm pmask))
                 (imm- (bitwise-andc1 imm pmask)))
             (define (use-phase phase)
               (generate period phase (bit-count imm+)))
             (cond ((contiguous-ones? imm+)
                    ;; E.g.: 00011100 -> phase = 2, count = 3
                    (use-phase (first-set-bit imm+)))
                   ((contiguous-ones? imm-)
                    ;; E.g.: 11100011 -> phase = 5, count = 5
                    ;; Note imm- is the p-bit complement of imm+.
                    (use-phase (integer-length imm-)))
                   (else
                    ;; It is not the replication of the rotation of a
                    ;; contiguous sequence of one bits.
                    #f)))))))

(define (logical-imm-s32 imm)
  (logical-immediate-signed imm 32))

(define (logical-imm-s64 imm)
  (logical-immediate-signed imm 64))

(define (logical-imm-u32 imm)
  (logical-immediate-unsigned imm 32))

(define (logical-imm-u64 imm)
  (logical-immediate-unsigned imm 64))

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

(define (isb-option o)
  (case o
    ((SY) #b1111)
    (else #f)))

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

(define (load-signed-opc size)          ;operand size
  (case size
    ((W) #b11)
    ((X) #b10)
    (else #f)))

(define (load-signed-size sz)           ;memory load size
  (case sz
    ((B) #b00)
    ((H) #b01)
    ((W) #b10)
    (else #f)))

(define (fp-scalar-size t)
  (case t
    ((H) #b11)
    ((S) #b00)
    ((D) #b01)
    (else #f)))

(define (fp-vector-size t)
  ;; Low bit is Q, next five bits are opcode, high bit is sz.
  (case t
    ((H4) #b1111000)
    ((H8) #b1111001)
    ((S2) #b0100000)
    ((S4) #b0100001)
    ((D2) #b1100001)
    (else #f)))

(define (fp16-vector-size t)
  ;; Q
  (case t
    ((H4) 0)
    ((H8) 1)
    (else #f)))

(define (fp32/64-vector-size t)
  (case t
    ((S2) #b00)
    ((S4) #b01)
    ((D2) #b11)
    (else #f)))

(define (simd-byte-vector-size t)
  ;; Q
  (case t
    ((B8) 0)
    ((B16) 1)
    (else #f)))

(define (simd-integer-vector-size t)
  ;; Low bit is Q, high two bits are sz.
  (case t
    ((B8) #b000)
    ((B16) #b001)
    ((H4) #b010)
    ((H8) #b011)
    ((S2) #b100)
    ((S4) #b101)
    (else #f)))

(define (simd-double-integer-vector-size t)
  ;; Low bit is Q, high two bits are sz.
  (case t
    ((B8) #b000)
    ((B16) #b001)
    ((H4) #b010)
    ((H8) #b011)
    ((S2) #b100)
    ((S4) #b101)
    ((D2) #b111)
    (else #f)))

(define (fmov-vector-immediate-size t)
  ;; #b{U}{op}{Q}
  (case t
    ((H4) #b100)
    ((H8) #b101)
    ((S2) #b000)
    ((S4) #b001)
    ((D2) #b010)
    (else #f)))

(define (fmov-general-size t)
  ;; Low two bits are type; high two bits are rmode.
  (case t
    ((H) #b0011)
    ((S) #b0000)
    ((D) #b0001)
    ((D1) #b0110)
    (else #f)))

(define (fmov-abc x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #b111)
       x))

(define (fmov-defgh x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #b11111)
       x))

;; XXX fixed-point
(define (fmov-abcdefgh x)
  x
  #f)

;; XXX 8-bit floating-point
(define (fp-binary8 x)
  x
  #f)
