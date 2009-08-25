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

;;;; 68020 Instruction Set Description (in addition to 68000)
;;; Originally from arthur, patterned after GJS's.

(declare (usual-integrations))

;;;; Bit Field Instructions (1)

(let-syntax
    ((define-bitfield-manipulation-1
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     (((? ea ,(cadddr form)) (& (? offset)) (& (? width)) (D (? reg)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (1 #b0)
			      (3 reg)
			      (1 #b0)
			      (5 offset)
			      (1 #b0)
			      (5 width BFWIDTH)))

	     (((? ea ,(cadddr form)) (& (? offset))
				     (D (? r-width))
				     (D (? reg)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (1 #b0)
			      (3 reg)
			      (1 #b0)
			      (5 offset)
			      (3 #b100)
			      (3 r-width)))

	     (((? ea ,(cadddr form)) (D (? r-offset))
				     (& (? width))
				     (D (? reg)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (1 #b0)
			      (3 reg)
			      (3 #b100)
			      (3 r-offset)
			      (1 #b0)
			      (5 width BFWIDTH)))

	     (((? ea ,(cadddr form)) (D (? r-offset))
				     (D (? r-width))
				     (D (? reg)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (1 #b0)
			      (3 reg)
			      (3 #b100)
			      (3 r-offset)
			      (3 #b100)
			      (3 r-width))))))))

  (define-bitfield-manipulation-1 BFEXTS #b1011 ea-d/c)
  (define-bitfield-manipulation-1 BFEXTU #b1001 ea-d/c)
  (define-bitfield-manipulation-1 BFFFO  #b1101 ea-d/c)
  (define-bitfield-manipulation-1 BFINS  #b1111 ea-d/c&a))

;;;; Bit Field Instructions (2)

(let-syntax
    ((define-bitfield-manipulation-2
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     (((? ea ,(cadddr form)) (& (? offset)) (& (? width)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (4 #b0000)
			      (1 #b0)
			      (5 offset)
			      (1 #b0)
			      (5 width BFWIDTH)))

	     (((? ea ,(cadddr form)) (& (? offset)) (D (? r-width)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (4 #b0000)
			      (1 #b0)
			      (5 offset)
			      (3 #b100)
			      (3 r-width)))

	     (((? ea ,(cadddr form)) (D (? r-offset)) (& (? width)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (4 #b0000)
			      (3 #b100)
			      (3 r-offset)
			      (1 #b0)
			      (5 width BFWIDTH)))

	     (((? ea ,(cadddr form)) (D (? r-offset)) (D (? r-width)))
	      (WORD (4 #b1110)
		    (4 ,(caddr form))
		    (2 #b11)
		    (6 ea DESTINATION-EA))
	      (EXTENSION-WORD (4 #b0000)
			      (3 #b100)
			      (3 r-offset)
			      (3 #b100)
			      (3 r-width))))))))

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
  (((? size wl+2) (D (? c1)) (D (? c2)) (D (? u1)) (D (? u2))
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
