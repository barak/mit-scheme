#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/instr4.scm,v 1.1 1987/07/17 15:59:50 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; 68020 Instruction Set Description (in addition to 68000)
;;; Originally from arthur, patterned after GJS's.

(declare (usual-integrations))

;;;; Bit Field Instructions (1)

(let-syntax
    ((define-bitfield-manipulation-1
       (macro (keyword bits ea-mode)
	 `(define-instruction ,keyword
	    (((? ea ,ea-mode) (& (? offset)) (& (? width)) (D (? reg)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (1 #b0)
			     (3 reg)
			     (1 #b0)
			     (5 offset)
			     (1 #b0)
			     (5 width BFWIDTH)))

	    (((? ea ,ea-mode) (& (? offset)) (D (? r-width)) (D (? reg)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (1 #b0)
			     (3 reg)
			     (1 #b0)
			     (5 offset)
			     (3 #b100)
			     (3 r-width)))

	    (((? ea ,ea-mode) (D (? r-offset)) (& (? width)) (D (? reg)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (1 #b0)
			     (3 reg)
			     (3 #b100)
			     (3 r-offset)
			     (1 #b0)
			     (5 width BFWIDTH)))

	    (((? ea ,ea-mode) (D (? r-offset)) (D (? r-width)) (D (? reg)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (1 #b0)
			     (3 reg)
			     (3 #b100)
			     (3 r-offset)
			     (3 #b100)
			     (3 r-width)))))))

  (define-bitfield-manipulation-1 BFEXTS #b1011 ea-d/c)
  (define-bitfield-manipulation-1 BFEXTU #b1001 ea-d/c)
  (define-bitfield-manipulation-1 BFFFO  #b1101 ea-d/c)
  (define-bitfield-manipulation-1 BFINS  #b1111 ea-d/c&a))

;;;; Bit Field Instructions (2)

(let-syntax
    ((define-bitfield-manipulation-2
       (macro (keyword bits ea-mode)
	 `(define-instruction ,keyword
	    (((? ea ,ea-mode) (& (? offset)) (& (? width)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (4 #b0000)
			     (1 #b0)
			     (5 offset)
			     (1 #b0)
			     (5 width BFWIDTH)))

	    (((? ea ,ea-mode) (& (? offset)) (D (? r-width)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (4 #b0000)
			     (1 #b0)
			     (5 offset)
			     (3 #b100)
			     (3 r-width)))

	    (((? ea ,ea-mode) (D (? r-offset)) (& (? width)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (4 #b0000)
			     (3 #b100)
			     (3 r-offset)
			     (1 #b0)
			     (5 width BFWIDTH)))

	    (((? ea ,ea-mode) (D (? r-offset)) (D (? r-width)))
	     (WORD (4 #b1110)
		   (4 ,bits)
		   (2 #b11)
		   (6 ea DESTINATION-EA))
	     (EXTENSION-WORD (4 #b0000)
			     (3 #b100)
			     (3 r-offset)
			     (3 #b100)
			     (3 width)))))))

  (define-bitfield-manipulation-2 BFCHG  #b1010 ea-d/c&a)
  (define-bitfield-manipulation-2 BFCLR  #b1100 ea-d/c&a)
  (define-bitfield-manipulation-2 BFSET  #b1110 ea-d/c&a)
  (define-bitfield-manipulation-2 BFTST  #b1000 ea-d/c))

;;;; BCD instructions

(define-instruction PACK
  (((- A (? x)) (- A (? y)) (& (? adjustment)))
   (WORD (4 #b1000)
	 (3 y)
	 (6 #b101001)
	 (3 x))
   (immediate-word adjustment))

  (((D (? x)) (D (? y)) (& (? adjustment)))
   (WORD (4 #b1000)
	 (3 y)
	 (6 #b101000)
	 (3 x))
   (immediate-word adjustment)))

(define-instruction UNPK
  (((- A (? x)) (- A (? y)) (& (? adjustment)))
   (WORD (4 #b1000)
	 (3 y)
	 (6 #b110001)
	 (3 x))
   (immediate-word adjustment))

  (((D (? x)) (D (? y)) (& (? adjustment)))
   (WORD (4 #b1000)
	 (3 y)
	 (6 #b110000)
	 (3 x))
   (immediate-word adjustment)))

;;;; Control

;;; Call module instruction

(define-instruction CALLM
  (((& (? argument-count)) (? ea ea-c))
   (WORD (10 #b0000011011)
	 (6 ea DESTINATION-EA))
   (EXTENSION-WORD (8 #b00000000)
		   (8 argument-count))))

;;; Return from module instruction

(define-instruction RTM
  ((((? rtype da) (? n)))
   (WORD (12 #b000001101100)
	 (1 rtype)
	 (3 n))))

;;; Breakpoint instruction

(define-instruction BKPT
  (((& (? data)))
   (WORD (13 #b0100100001001)
	 (3 data))))

;;; Compare and swap operand instructions

(define-instruction CAS
  (((? size bwl+1) (D (? compare)) (D (? update)) (? ea ea-m&a))
   (WORD (5 #b00001)
	 (2 size)
	 (3 #b011)
	 (6 ea DESTINATION-EA))
   (EXTENSION-WORD (7 #b0000000)
		   (3 update)
		   (3 #b000)
		   (3 compare))))

(define-instruction CAS2
  (((? size nwl-n) (D (? c1)) (D (? c2)) (D (? u1)) (D (? u2))
		   ((? Rtype1 da) (? n1))
		   ((? Rtype2 da) (? n2)))
   (WORD (5 #b00001)
	 (2 size)
	 (9 #b011111100))
   (EXTENSION-WORD (1 Rtype1)
		   (3 n1)
		   (3 #b000)
		   (3 u1)
		   (3 #b000)
		   (3 c1)
		   (1 Rtype2)
		   (3 n2)
		   (3 #b000)
		   (3 u2)
		   (3 #b000)
		   (3 c2))))

;;;; Miscellaneous (continued)

;;; Extend byte to longword instruction

(define-instruction EXTB
  (((D (? n)))
   (WORD (7 #b0100100)
	 (3 #b111)
	 (3 #b000)
	 (3 n))))

;;; Range comparison instruction

(define-instruction CMP2
  (((? size bwl) (? ea ea-c) ((? rtype da) (? n)))
   (WORD (5 #b00000)
	 (2 size)
	 (3 #b011)
	 (6 ea SOURCE-EA size))
   (EXTENSION-WORD (1 rtype)
		   (3 n)
		   (12 #b000000000000))))

;;; Range check instruction

(define-instruction CHK2
  (((? size bwl) (? ea ea-c) ((? rtype da) (? n)))
   (WORD (5 #b00000)
	 (2 size)
	 (3 #b011)
	 (6 ea SOURCE-EA size))
   (EXTENSION-WORD (1 rtype)
		   (3 n)
		   (12 #b100000000000))))
