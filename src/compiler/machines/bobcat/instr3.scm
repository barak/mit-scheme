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

;;;; 68000 Instruction Set Description
;;; Originally from GJS (who did the hard part).

(declare (usual-integrations))

;;;; Control Transfer: Branch instructions

;; No size suffix means that the assembler should choose the right
;; size offset.

;; When the displacement is 0 (a branch to the immediately following
;; instruction), a NOP instruction is issued for non-subroutine
;; branches (BRA and Bcc).  The branch tensioner can't really handle
;; instructions that disappear.

;; For BSR instructions to the immediately following instruction,
;; there is nothing that can be done.  The branch tensioner assumes
;; that the output does not decrease with increasing discriminator
;; ranges, and the only two possibilities for this instruction would
;; be to put a NOP after the BSR, or to change the BSR into a
;; pc-relative PEA, but either of these options would make the code 32
;; bits long, longer than the 16 bits used for short displacements.
;; An error is generated if this situation arises.

(let-syntax
    ((define-branch-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     ((,@(caddr form) B (@PCO (? o)))
	      (WORD ,@(cadddr form)
		    (8 o SIGNED)))

	     ((,@(caddr form) B (@PCR (? l)))
	      (WORD ,@(cadddr form)
		    (8 l SHORT-LABEL)))

	     ((,@(caddr form) W (@PCO (? o)))
	      (WORD ,@(cadddr form)
		    (8 #b00000000))
	      (immediate-word o))

	     ((,@(caddr form) W (@PCR (? l)))
	      (WORD ,@(cadddr form)
		    (8 #b00000000))
	      (relative-word l))

	     ;; 68020 only

	     ((,@(caddr form) L (@PCO (? o)))
	      (WORD ,@(cadddr form)
		    (8 #b11111111))
	      (immediate-long o))

	     ((,@(caddr form) L (@PCR (? l)))
	      (WORD ,@(cadddr form)
		    (8 #b11111111))
	      (relative-long l))

	     ((,@(caddr form) (@PCO (? o)))
	      (GROWING-WORD (disp o)
			    ((0 0)
			     ,@(cddddr form))
			    ((-128 127)
			     (WORD ,@(cadddr form)
				   (8 disp SIGNED)))
			    ((-32768 32767)
			     (WORD ,@(cadddr form)
				   (8 #b00000000)
				   (16 disp SIGNED)))
			    ((() ())
			     (WORD ,@(cadddr form)
				   (8 #b11111111)
				   (32 disp SIGNED)))))

	     ((,@(caddr form) (@PCR (? l)))
	      (GROWING-WORD (disp `(- ,l (+ *PC* 2)))
			    ((0 0)
			     ,@(cddddr form))
			    ((-128 127)
			     (WORD ,@(cadddr form)
				   (8 disp SIGNED)))
			    ((-32768 32767)
			     (WORD ,@(cadddr form)
				   (8 #b00000000)
				   (16 disp SIGNED)))
			    ((() ())
			     (WORD ,@(cadddr form)
				   (8 #b11111111)
				   (32 disp SIGNED))))))))))

  (define-branch-instruction B ((? c cc)) ((4 #b0110) (4 c))
    (WORD (16 #b0100111001110001)))
  (define-branch-instruction BRA () ((8 #b01100000))
    (WORD (16 #b0100111001110001)))
  (define-branch-instruction BSR () ((8 #b01100001))
    (WORD (16 (error "BSR to following instruction")))))

(define-instruction DB
  (((? c cc) (D (? rx)) (@PCO (? o)))
   (WORD (4 #b0101)
	 (4 c)
	 (5 #b11001)
	 (3 rx))
   (immediate-word o))

  (((? c cc) (D (? rx)) (@PCR (? l)))
   (WORD (4 #b0101)
	 (4 c)
	 (5 #b11001)
	 (3 rx))
   (relative-word l)))

(define-instruction JMP
  (((? ea ea-c))
   (WORD (10 #b0100111011)
	 (6 ea DESTINATION-EA))))

(define-instruction JSR
  (((? ea ea-c))
   (WORD (10 #b0100111010)
	 (6 ea DESTINATION-EA))))

;; 68010 and 68020 only

(define-instruction RTD
  (((& (? offset)))
   (WORD (16 #b0100111001110100))
   (EXTENSION-WORD (16 offset))))

(define-instruction RTE
  (()
   (WORD (16 #b0100111001110011))))

(define-instruction RTR
  (()
   (WORD (16 #b0100111001110111))))

(define-instruction RTS
  (()
   (WORD (16 #b0100111001110101))))

(define-instruction TRAPV
  (()
   (WORD (16 #b0100111001110110))))

;;;; Family member dependent miscellaneous instructions.

#| 

;;  These are the 68000/68010 versions

(define-instruction TRAP
  (((& (? v)))
   (WORD (12 #b010011100100)
	 (4 v))))

(define-instruction CHK
  (((? ea ea-d) (D (? rx)))
   (WORD (4 #b0100)
	 (3 rx)
	 (3 #b110)
	 (6 ea SOURCE-EA 'W))))

(define-instruction LINK
  (((A (? rx)) (& (? d)))
   (WORD (13 #b0100111001010)
	 (3 rx))
   (immediate-word d)))

|#

;;;; Family member dependent miscellaneous instructions (continued).

;; These are the 68020 versions

(define-instruction TRAP
  (((& (? v)))
   (WORD (12 #b010011100100)
	 (4 v)))

  (((? c cc))
   (WORD (4 #b0101)
	 (4 cc)
	 (8 #b11111100)))

  (((? c cc) W (& (? data)))
   (WORD (4 #b0101)
	 (4 cc)
	 (8 #b11111010))
   (EXTENSION-WORD (16 data)))

  (((? c cc) L (& (? data)))
   (WORD (4 #b0101)
	 (4 cc)
	 (8 #b11111011))
   (EXTENSION-WORD (32 data))))

(define-instruction CHK
  ;; This is for compatibility with older (68000/68010) syntax.
  ;; There is no size suffix to the opcode.

  (((? ea ea-d) (D (? rx)))
   (WORD (4 #b0100)
	 (3 rx)
	 (3 #b110)
	 (6 ea SOURCE-EA 'W)))

  (((? size chkwl) (? ea ea-d) (D (? rx)))
   (WORD (4 #b0100)
	 (3 rx)
	 (3 size)
	 (6 ea SOURCE-EA 'W))))

(define-instruction LINK
  ((W (A (? rx)) (& (? d)))
   (WORD (13 #b0100111001010)
	 (3 rx))
   (immediate-word d))

  ((L (A (? rx)) (& (? d)))
   (WORD (13 #b0100100000001)
	 (3 rx))
   (immediate-long d)))

;;;; Randomness

(define-instruction ILLEGAL
  (()
   (WORD (16 #b0100101011111100))))

(define-instruction NOP
  (()
   (WORD (16 #b0100111001110001))))

(define-instruction RESET
  (()
   (WORD (16 #b0100111001110000))))

(define-instruction STOP
  (((& (? data)))
   (WORD (16 #b0100111001110010))
   (immediate-word data)))

(define-instruction SWAP
  (((D (? rx)))
   (WORD (13 #b0100100001000)
	 (3 rx))))

(define-instruction UNLK
  (((A (? rx)))
   (WORD (13 #b0100111001011)
	 (3 rx))))

;;;; Data Transfer

(define-instruction CLR
  (((? s bwl) (? ea ea-d&a))
   (WORD (8 #b01000010)
	 (2 s)
	 (6 ea DESTINATION-EA))))

(define-instruction EXG
  (((D (? rx)) (D (? ry)))
   (WORD (4 #b1100)
	 (3 rx)
	 (6 #b101000)
	 (3 ry)))

  (((A (? rx)) (A (? ry)))
   (WORD (4 #b1100)
	 (3 rx)
	 (6 #b101001)
	 (3 ry)))

  (((D (? rx)) (A (? ry)))
   (WORD (4 #b1100)
	 (3 rx)
	 (6 #b110001)
	 (3 ry)))

  (((A (? ry)) (D (? rx)))
   (WORD (4 #b1100)
	 (3 rx)
	 (6 #b110001)
	 (3 ry))))

(define-instruction LEA
  (((? ea ea-c) (A (? rx)))
   (WORD (4 #b0100)
	 (3 rx)
	 (3 #b111)
	 (6 ea DESTINATION-EA))))

(define-instruction PEA
  (((? cea ea-c))
   (WORD (10 #b0100100001)
	 (6 cea DESTINATION-EA))))

(define-instruction S
  (((? c cc) (? dea ea-d&a))
   (WORD (4 #b0101)
	 (4 c)
	 (2 #b11)
	 (6 dea DESTINATION-EA))))

(define-instruction TAS
  (((? dea ea-d&a))
   (WORD (10 #b0100101011)
	 (6 dea DESTINATION-EA))))

(define-instruction MOVE
  ((B (? sea ea-all-A) (? dea ea-d&a))
   (WORD (3 #b000)
	 (1 #b1)
	 (6 dea DESTINATION-EA-REVERSED)
	 (6 sea SOURCE-EA 'B)))

  ;; the following includes the MOVEA instruction

  (((? s lw ssym) (? sea ea-all) (? dea ea-all))
   (WORD (3 #b001)
	 (1 s)
	 (6 dea DESTINATION-EA-REVERSED)
	 (6 sea SOURCE-EA ssym)))

  ;; Special MOVE instructions

  ((W (? ea ea-d) (CCR))		;MOVE to CCR
   (WORD (10 #b0100010011)
	 (6 ea SOURCE-EA 'W)))

  ((W (CCR) (? ea ea-d))		;MOVE from CCR
   (WORD (10 #b0100001011)
	 (6 ea DESTINATION-EA 'W)))

  ((W (? ea ea-d) (SR))			;MOVE to SR
   (WORD (10 #b0100011011)
	 (6 ea SOURCE-EA 'W)))

  ((W (SR) (? ea ea-d&a))		;MOVE from SR
   (WORD (10 #b0100000011)
	 (6 ea DESTINATION-EA)))

  ((L (A (? rx)) (USP))			;MOVE to USP
   (WORD (13 #b0100111001100)
	 (3 rx)))

  ((L (USP) (A (? rx)))			;MOVE from USP
   (WORD (13 #b0100111001101)
	 (3 rx))))

;; MOV is a special case, separated for efficiency so there are less
;; rules to try.

(define-instruction MOV
  ((B (? sea ea-all-A) (? dea ea-d&a))
   (WORD (3 #b000)
	 (1 #b1)
	 (6 dea DESTINATION-EA-REVERSED)
	 (6 sea SOURCE-EA 'B)))

  ;; the following includes the MOVEA instruction

  (((? s lw ssym) (? sea ea-all) (? dea ea-all))
   (WORD (3 #b001)
	 (1 s)
	 (6 dea DESTINATION-EA-REVERSED)
	 (6 sea SOURCE-EA ssym))))

(define-instruction MOVEQ
  (((& (? data)) (D (? rx)))
   (WORD (4 #b0111)
	 (3 rx)
	 (1 #b0)
	 (8 data SIGNED))))

(define-instruction MOVEM
  (((? s wl) (? r @+reg-list) (? dea ea-c&a))
   (WORD (9 #b010010001)
	 (1 s)
	 (6 dea DESTINATION-EA))
   (output-bit-string r))

  (((? s wl) (? r @-reg-list) (@-a (? rx)))
   (WORD (9 #b010010001)
	 (1 s)
	 (3 #b100)
	 (3 rx))
   (output-bit-string r))

  (((? s wl) (? sea ea-c) (? r @+reg-list))
   (WORD (9 #b010011001)
	 (1 s)
	 (6 sea SOURCE-EA s))
   (output-bit-string r))

  (((? s wl) (@A+ (? rx)) (? r @+reg-list))
   (WORD (9 #b010011001)
	 (1 s)
	 (3 #b011)
	 (3 rx))
   (output-bit-string r)))

(define-instruction MOVEP
  (((? s wl) (D (? rx)) (@AO (? ry) (? o)))
   (WORD (4 #b0000)
	 (3 rx)
	 (2 #b11)
	 (1 s)
	 (3 #b001)
	 (3 ry))
   (offset-word o))

  (((? s wl) (D (? rx)) (@AR (? ry) (? l)))
   (WORD (4 #b0000)
	 (3 rx)
	 (2 #b11)
	 (1 s)
	 (3 #b001)
	 (3 ry))
   (relative-word l))

  (((? s wl) (@AO (? ry) (? o)) (D (? rx)))
   (WORD (4 #b0000)
	 (3 rx)
	 (2 #b10)
	 (1 s)
	 (3 #b001)
	 (3 ry))
   (offset-word o))

  (((? s wl) (@AR (? ry) (? l)) (D (? rx)))
   (WORD (4 #b0000)
	 (3 rx)
	 (2 #b10)
	 (1 s)
	 (3 #b001)
	 (3 ry))
   (relative-word l)))

;;;; 68010 and 68020 only privileged MOVE instructions.

;;; move from/to control register.

(define-instruction MOVEC
  ((((? creg cont-reg)) ((? rtype da) (? greg)))
   (WORD (15 #b010011100111101)
	 (1 #b0))
   (EXTENSION-WORD (1 rtype)
		   (3 greg)
		   (12 creg)))

  ((((? rtype da) (? greg)) ((? creg cont-reg)))
   (WORD (15 #b010011100111101)
	 (1 #b1))
   (EXTENSION-WORD (1 rtype)
		   (3 greg)
		   (12 creg))))

(define-instruction MOVES
  (((? size bwl) ((? rtype da) (? reg)) (? dest ea-m&a))
   (WORD (8 #b00001110)
	 (2 size)
	 (6 dest DESTINATION-EA))
   (EXTENSION-WORD (1 rtype)
		   (3 reg)
		   (1 #b1)
		   (11 #b00000000000)))
  (((? size bwl) (? dest ea-m&a) ((? rtype da) (? reg)))
   (WORD (8 #b00001110)
	 (2 size)
	 (6 dest DESTINATION-EA))
   (EXTENSION-WORD (1 rtype)
		   (3 reg)
		   (1 #b0)
		   (11 #b00000000000))))
