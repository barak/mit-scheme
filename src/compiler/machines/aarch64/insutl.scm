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

;;;; AArch64 Instruction Set, utilities

(declare (usual-integrations))

(define (signed-9 x)
  (and (exact-integer? x)
       (<= #x-200 x #x1ff)
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

(define (unsigned-7 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #x7f)
       x))

(define (unsigned-12 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xfff)
       x))

(define (unsigned-16 x)
  (and (exact-nonnegative-integer? x)
       (<= 0 x #xffff)
       x))

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

(define (logical-imm-32 imm)
  ;; XXX
  imm
  (error "XXX not yet implemented"))

(define (logical-imm-64 imm)
  ;; XXX
  imm
  (error "XXX not yet implemented"))

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
