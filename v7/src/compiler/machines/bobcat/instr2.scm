#| -*-Scheme-*-

$Id: instr2.scm,v 1.20 2001/12/20 21:45:24 cph Exp $

Copyright (c) 1987-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; 68000 Instruction Set Description
;;; Originally from GJS (who did the hard part).

(declare (usual-integrations))

;;;; Pseudo ops

(define-instruction DC
  ((W (? expression))
   (WORD (16 expression SIGNED)))

  ((L (? expression))
   (WORD (32 expression SIGNED)))

  ((UW (? expression))
   (WORD (16 expression UNSIGNED)))

  ((UL (? expression))
   (WORD (32 expression UNSIGNED))))

;;;; BCD Arithmetic

(let-syntax ((define-BCD-addition
	      (lambda (keyword opcode)
		`(define-instruction ,keyword
		   (((D (? ry)) (D (? rx)))
		    (WORD (4 ,opcode)
			  (3 rx)
			  (6 #b100000)
			  (3 ry)))

		   (((@-A (? ry)) (@-A (? rx)))
		    (WORD (4 ,opcode)
			  (3 rx)
			  (6 #b100001)
			  (3 ry)))))))
  (define-BCD-addition ABCD #b1100)
  (define-BCD-addition SBCD #b1000))

(define-instruction NBCD
  ((? dea ea-d&a)
   (WORD (10 #b0100100000)
	 (6 dea DESTINATION-EA))))

;;;; Binary Arithmetic

(let-syntax ((define-binary-addition
	      (lambda (keyword Qkeyword Xkeyword opcode Qbit Iopcode)
		`(BEGIN
		  (define-instruction ,Qkeyword			;ADDQ/SUBQ
		    ((B (& (? data)) (? ea ea-all-A))
		     (WORD (4 #b0101)
			   (3 data QUICK)
			   (1 ,Qbit)
			   (2 #b00)
			   (6 ea DESTINATION-EA)))

		    (((? s bwl-b) (& (? data)) (? ea ea-all))
		     (WORD (4 #b0101)
			   (3 data QUICK)
			   (1 ,Qbit)
			   (2 s)
			   (6 ea DESTINATION-EA))))

		  (define-instruction ,keyword
		    (((? s bwl ssym) (& (? data)) (? ea ea-d&a)) ;ADDI/SUBI
		     (WORD (4 #b0000)
			   (4 ,Iopcode)
			   (2 s)
			   (6 ea DESTINATION-EA))
		     (immediate-words data ssym))

		    ((B (? ea ea-all-A) (D (? rx)))
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 #b0)
			   (2 #b00)
			   (6 ea SOURCE-EA 'B)))

		    (((? s bwl-b ssym) (? ea ea-all) (D (? rx)))
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 #b0)
			   (2 s)
			   (6 ea SOURCE-EA ssym)))

		    (((? s bwl) (D (? rx)) (? ea ea-m&a))
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 #b1)
			   (2 s)
			   (6 ea DESTINATION-EA)))

		    (((? s wl ssym) (? ea ea-all) (A (? rx)))   ;ADDA/SUBA
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 s)
			   (2 #b11)
			   (6 ea SOURCE-EA ssym))))

		  (define-instruction ,Xkeyword
		    (((? s bwl) (D (? ry)) (D (? rx)))
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 #b1)
			   (2 s)
			   (3 #b000)
			   (3 ry)))

		    (((? s bwl) (@-A (? ry)) (@-A (? rx)))
		     (WORD (4 ,opcode)
			   (3 rx)
			   (1 #b1)
			   (2 s)
			   (3 #b001)
			   (3 ry))))))))
  (define-binary-addition ADD ADDQ ADDX #b1101 #b0 #b0110)
  (define-binary-addition SUB SUBQ SUBX #b1001 #b1 #b0100))

(define-instruction EXT
  (((? s wl) (D (? rx)))
   (WORD (9 #b010010001)
	 (1 s)
	 (3 #b000)
	 (3 rx))))

(define-instruction NEG
  (((? s bwl) (? dea ea-d&a))
   (WORD (8 #b01000100)
	 (2 s)
	 (6 dea DESTINATION-EA))))

(define-instruction NEGX
  (((? s bwl) (? dea ea-d&a))
   (WORD (8 #b01000000)
	 (2 s)
	 (6 dea DESTINATION-EA))))

;;; Multiplication and division

#|

;; These are the 68000/68010 versions

(define-instruction DIV
  (((? sgn us) (D (? rx)) (? ea ea-d))
   (WORD (4 #b1000)
	 (3 rx)
	 (1 sgn)
	 (2 #b11)
	 (6 ea SOURCE-EA 'W))))

(define-instruction MUL
  (((? sgn us) (? ea ea-d) (D (? rx)))
   (WORD (4 #b1100)
	 (3 rx)
	 (1 sgn)
	 (2 #b11)
	 (6 ea SOURCE-EA 'W))))

|#

;; These are the 68020 versions

(let-syntax ((define-mul-and-div
	       (lambda (keyword word-form-bit long-form-bit)
		 `(define-instruction ,keyword
		    (((? sgn us) W (? ea ea-d) (D (? n)))
		     (WORD (1 #b1)
			   (1 ,word-form-bit)
			   (2 #b00)
			   (3 n)
			   (1 sgn)
			   (2 #b11)
			   (6 ea SOURCE-EA 'W)))

		    (((? sgn us) L (? ea ea-d) (D (? q)))
		     (WORD (9 #b010011000)
			   (1 ,long-form-bit)
			   (6 ea SOURCE-EA 'L))
		     (EXTENSION-WORD (1 #b0)
				     (3 q)
				     (1 sgn)
				     (8 #b00000000)
				     (3 q)))

		    (((? sgn us) L (? ea ea-d) (D (? r)) (D (? q)))
		     (WORD (9 #b010011000)
			   (1 ,long-form-bit)
			   (6 ea SOURCE-EA 'L))
		     (EXTENSION-WORD (1 #b0)
				     (3 q)
				     (1 sgn)
				     (8 #b10000000)
				     (3 r)))))))
  (define-mul-and-div MUL #b1 #b0)
  (define-mul-and-div DIV #b0 #b1))

(define-instruction DIVL
  (((? sgn us) L (? ea ea-d) (D (? r)) (D (? q)))
   (WORD (9 #b010011000)
	 (1 #b1)			; DIV long-form-bit
	 (6 ea SOURCE-EA 'L))
   (EXTENSION-WORD (1 #b0)
		   (3 q)
		   (1 sgn)
		   (8 #b00000000)
		   (3 r))))

;;;; Comparisons

(define-instruction CMP
  ((B (? ea ea-all-A) (D (? rx)))
   (WORD (4 #b1011)
	 (3 rx)
	 (1 #b0)
	 (2 #b00)
	 (6 ea SOURCE-EA 'B)))

  (((? s bwl-b ssym) (? ea ea-all) (D (? rx)))
   (WORD (4 #b1011)
	 (3 rx)
	 (1 #b0)
	 (2 s)
	 (6 ea SOURCE-EA ssym)))

  (((? s wl ssym) (? ea ea-all) (A (? rx)))	;CMPA
   (WORD (4 #b1011)
	 (3 rx)
	 (1 s)
	 (2 #b11)
	 (6 ea SOURCE-EA ssym)))

  (((? s bwl ssym) (& (? data)) (? ea ea-d&a))	;CMPI
   (WORD (8 #b00001100)
	 (2 s)
	 (6 ea DESTINATION-EA))
   (immediate-words data ssym))

  (((? s bwl) (@A+ (? ry)) (@A+ (? rx)))	;CMPM
   (WORD (4 #b1011)
	 (3 rx)
	 (1 #b1)
	 (2 s)
	 (3 #b001)
	 (3 ry))))

;; Also provided for efficiency.  Less rules to search.

(define-instruction CMPI
  (((? s bwl ssym) (& (? data)) (? ea ea-d&a))
   (WORD (8 #b00001100)
	 (2 s)
	 (6 ea DESTINATION-EA))
   (immediate-words data ssym)))

(define-instruction TST
  (((? s bwl) (? dea ea-d&a))
   (WORD (8 #b01001010)
	 (2 s)
	 (6 dea DESTINATION-EA))))

;;;; Bitwise Logical

(let-syntax ((define-bitwise-logical
	      (lambda (keyword opcode Iopcode)
		`(define-instruction ,keyword
		   (((? s bwl ssym) (? ea ea-d) (D (? rx)))
		    (WORD (4 ,opcode)
			  (3 rx)
			  (1 #b0)
			  (2 s)
			  (6 ea SOURCE-EA ssym)))

		   (((? s bwl) (D (? rx)) (? ea ea-m&a))
		    (WORD (4 ,opcode)
			  (3 rx)
			  (1 #b1)
			  (2 s)
			  (6 ea DESTINATION-EA)))

		   (((? s bwl ssym) (& (? data)) (? ea ea-d&a))	;fooI
		    (WORD (4 #b0000)
			  (4 ,Iopcode)
			  (2 s)
			  (6 ea DESTINATION-EA))
		    (immediate-unsigned-words data ssym))

		   (((? s bwl ssym) (& (? data)) (SR))		;fooI to CCR/SR
		    (WORD (4 #b0000)
			  (4 ,Iopcode)
			  (2 s)
			  (6 #b111100))
		    (immediate-unsigned-words data ssym))))))
  (define-bitwise-logical AND #b1100 #b0010) 	; and ANDI
  (define-bitwise-logical OR  #b1000 #b0000))	; and ORI

(define-instruction EOR
  (((? s bwl) (D (? rx)) (? ea ea-d&a))
   (WORD (4 #b1011)
	 (3 rx)
	 (1 #b1)
	 (2 s)
	 (6 ea DESTINATION-EA)))

  (((? s bwl ssym) (& (? data)) (? ea ea-d&a))	;EORI
   (WORD (8 #b00001010)
	 (2 s)
	 (6 ea DESTINATION-EA))
   (immediate-unsigned-words data ssym))

  (((? s bw ssym) (& (? data)) (SR))		;EORI to CCR/SR
   (WORD (8 #b00001010)
	 (2 s)
	 (6 #b111100))
   (immediate-unsigned-words data ssym)))

(define-instruction NOT
  (((? s bwl) (? dea ea-d&a))
   (WORD (8 #b01000110)
	 (2 s)
	 (6 dea DESTINATION-EA))))

;;;; Shift

(let-syntax ((define-shift-instruction
	      (lambda (keyword bits)
		`(define-instruction ,keyword
		   (((? d rl) (? s bwl) (D (? rx)) (D (? ry)))
		    (WORD (4 #b1110)
			  (3 rx)
			  (1 d)
			  (2 s)
			  (1 #b1)
			  (2 ,bits)
			  (3 ry)))

		   (((? d rl) (? s bwl) (& (? data)) (D (? ry)))
		    (WORD (4 #b1110)
			  (3 data SHIFT-NUMBER)
			  (1 d)
			  (2 s)
			  (1 #b0)
			  (2 ,bits)
			  (3 ry)))

		   (((? d rl) (? ea ea-m&a))
		    (WORD (5 #b11100)
			  (2 ,bits)
			  (1 d)
			  (2 #b11)
			  (6 ea DESTINATION-EA)))))))
  (define-shift-instruction AS  #b00)
  (define-shift-instruction LS  #b01)
  (define-shift-instruction ROX #b10)
  (define-shift-instruction RO  #b11))

;;;; Bit Manipulation

(let-syntax ((define-bit-manipulation
	      (lambda (keyword bits ea-register-target ea-immediate-target)
		`(define-instruction ,keyword
		   (((D (? rx)) (? ea ,ea-register-target))
		    (WORD (4 #b0000)
			  (3 rx)
			  (1 #b1)
			  (2 ,bits)
			  (6 ea DESTINATION-EA)))

		   (((& (? bitnum)) (? ea ,ea-immediate-target))
		    (WORD (8 #b00001000)
			  (2 ,bits)
			  (6 ea DESTINATION-EA))
		    (immediate-byte bitnum))))))
  (define-bit-manipulation BTST #b00 ea-d   ea-d&-&)
  (define-bit-manipulation BCHG #b01 ea-d&a ea-d&a)
  (define-bit-manipulation BCLR #b10 ea-d&a ea-d&a)
  (define-bit-manipulation BSET #b11 ea-d&a ea-d&a))
