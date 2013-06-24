#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; AMD x86-64 Instruction Set, part I
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Pseudo ops

(define-instruction BYTE
  ((S (? value signed-byte))
   (BITS (8 value SIGNED)))
  ((U (? value unsigned-byte))
   (BITS (8 value UNSIGNED))))

(define-instruction WORD
  ((S (? value signed-word))
   (BITS (16 value SIGNED)))
  ((U (? value unsigned-word))
   (BITS (16 value UNSIGNED))))

(define-instruction LONG
  ((S (? value signed-long))
   (BITS (32 value SIGNED)))
  ((U (? value unsigned-long))
   (BITS (32 value UNSIGNED))))

(define-instruction QUAD
  ((S (? value signed-quad))
   (BITS (64 value SIGNED)))
  ((U (? value unsigned-quad))
   (BITS (64 value UNSIGNED))))

;;;; Actual instructions

(let-syntax
    ((define-arithmetic-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form))
	       (digit (cadddr form)))
	   `(define-instruction ,mnemonic
	      ((B (? target r/m-ea) (R (? source)))
	       (PREFIX (ModR/M source target))
	       (BITS (8 ,opcode))
	       (ModR/M source target))

	      ((B (R (? target)) (? source r/m-ea))
	       (PREFIX (ModR/M source target))
	       (BITS (8 ,(+ opcode 2)))
	       (ModR/M target source))

	      ((B (R 0) (& (? value sign-extended-byte)))	;AL
	       (BITS (8 ,(+ opcode 4))
		     (8 value SIGNED)))

	      ((B (R 0) (&U (? value zero-extended-byte)))	;AL
	       (BITS (8 ,(+ opcode 4))
		     (8 value SIGNED)))

	      ((B (? target r/m-ea) (& (? value sign-extended-byte)))
	       (PREFIX (ModR/M target))
	       (BITS (8 #x80))
	       (ModR/M ,digit target)
	       (BITS (8 value SIGNED)))

	      ((B (? target r/m-ea) (&U (? value zero-extended-byte)))
	       (PREFIX (ModR/M target))
	       (BITS (8 #x80))
	       (ModR/M ,digit target)
	       (BITS (8 value SIGNED)))

	      ((W (R 0) (& (? value sign-extended-word)))
	       (PREFIX (OPERAND 'W))
	       (BITS (8 ,(+ opcode 5))
		     (16 value SIGNED)))

	      ((W (R 0) (&U (? value zero-extended-word)))
	       (PREFIX (OPERAND 'W))
	       (BITS (8 ,(+ opcode 5))
		     (16 value SIGNED)))

	      (((? size operand-size) (R 0) (& (? value sign-extended-long)))
	       (PREFIX (OPERAND size))
	       (BITS (8 ,(+ opcode 5))
		     (32 value SIGNED)))

	      (((? size operand-size) (R 0) (&U (? value zero-extended-long)))
	       (PREFIX (OPERAND size))
	       (BITS (8 ,(+ opcode 5))
		     (32 value SIGNED)))

	      (((? size operand-size) (? target r/m-ea) (R (? source)))
	       (PREFIX (OPERAND size) (ModR/M source target))
	       (BITS (8 ,(+ opcode 1)))
	       (ModR/M source target))

	      (((? size operand-size) (R (? target)) (? source r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M target source))
	       (BITS (8 ,(+ opcode 3)))
	       (ModR/M target source))

	      (((? size operand-size)
		(? target r/m-ea)
		(& (? value sign-extended-byte)))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #x83))
	       (ModR/M ,digit target)
	       (BITS (8 value SIGNED)))

	      (((? size operand-size)
		(? target r/m-ea)
		(&U (? value zero-extended-byte)))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #x83))
	       (ModR/M ,digit target)
	       (BITS (8 value SIGNED)))

	      ((W (? target r/m-ea) (& (? value sign-extended-word)))
	       (PREFIX (OPERAND 'W) (ModR/M target))
	       (BITS (8 #x81))
	       (ModR/M ,digit target)
	       (BITS (16 value SIGNED)))

	      ((W (? target r/m-ea) (&U (? value zero-extended-word)))
	       (PREFIX (OPERAND 'W) (ModR/M target))
	       (BITS (8 #x81))
	       (ModR/M ,digit target)
	       (BITS (16 value SIGNED)))

	      (((? size operand-size)
		(? target r/m-ea)
		(& (? value sign-extended-long)))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #x81))
	       (ModR/M ,digit target)
	       (BITS (32 value SIGNED)))

	      (((? size operand-size)
		(? target r/m-ea)
		(&U (? value zero-extended-long)))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #x81))
	       (ModR/M ,digit target)
	       (BITS (32 value SIGNED)))))))))

  (define-arithmetic-instruction ADC #x10 2)
  (define-arithmetic-instruction ADD #x00 0)
  (define-arithmetic-instruction AND #x20 4)
  (define-arithmetic-instruction CMP #x38 7)
  (define-arithmetic-instruction OR  #x08 1)
  (define-arithmetic-instruction SBB #x18 3)
  (define-arithmetic-instruction SUB #x28 5)
  (define-arithmetic-instruction XOR #x30 6))

(define-instruction BSF
  (((? size operand-size) (R (? target)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x0f)
	 (8 #xbc))
   (ModR/M target source)))

(define-instruction BSR
  (((? size operand-size) (R (? target)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x0f)
	 (8 #xbd))
   (ModR/M target source)))

(define-instruction BSWAP
  (((? size operand-size) (R (? reg)))
   (PREFIX (OPERAND size) (OPCODE-REGISTER reg))
   (BITS (8 #x0f)
	 (8 (opcode-register #xc8 reg)))))

(let-syntax
    ((define-bit-test-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form))
	       (digit (cadddr form)))
	   `(define-instruction ,mnemonic

	      (((? size operand-size) (? target r/m-ea)
				      (&U (? posn unsigned-byte)))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #x0f)
		     (8 #xba))
	       (ModR/M ,digit target)
	       (BITS (8 posn UNSIGNED)))

	      (((? target r/m-ea) (R (? posn)))
	       (PREFIX (ModR/M posn target))
	       (BITS (8 #x0f)
		     (8 ,opcode))
	       (ModR/M posn target))))))))

  (define-bit-test-instruction BT  #xa3 4)
  (define-bit-test-instruction BTC #xbb 7)
  (define-bit-test-instruction BTR #xb3 6)
  (define-bit-test-instruction BTS #xab 5))
  
(define-instruction CALL
  (((@PCR (? dest)))
   (BITS (8 #xe8)
	 (32 `(- ,dest (+ *PC* 4)))))

  (((@PCRO (? dest) (? offset)))
   (BITS (8 #xe8)
	 (32 `(- (+ ,dest ,offset) (+ *PC* 4)))))

  (((@PCO (? offset)))
   (BITS (8 #xe8)
	 (32 offset)))

  (((? dest r/m-ea))
   (PREFIX (ModR/M dest))
   (BITS (8 #xff))
   (ModR/M 2 dest)))

;;; Convert to Sign-Extended, in other assemblers identified by the
;;; six mnemonics CBW, CWDE, CDQE, CWD, CDQ, and CDO.  The operand
;;; size in this rendition means the size of the final result; thus,
;;; (CSE L (R 0)) sign-extends AX (word) to EAX (long), and (CSE Q (R
;;; 2) (R 0)) fills all sixty-four bits of RDX with the sign of RAX.

(define-instruction CSE
  (((? size operand-size) (R 0))
   (PREFIX (OPERAND (case size ((W) 'B) ((L) 'W) ((Q) 'L))))
   (BITS (8 #x98)))

  (((? size operand-size) (R 2) (R 0))
   (PREFIX (OPERAND size))
   (BITS (8 #x99))))

(define-trivial-instruction CLC #xf8)
(define-trivial-instruction CLD #xfc)
(define-trivial-instruction CLI #xfa)
(define-trivial-instruction CLTS #x0f #x06)
(define-trivial-instruction CMC #xf5)

(let-syntax
    ((define-string-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form)))
	   `(define-instruction ,mnemonic

	      ((B)
	       (BITS (8 ,opcode)))

	      (((? size operand-size))
	       (PREFIX (OPERAND size))
	       (BITS (8 ,(+ opcode 1))))))))))

  (define-string-instruction CMPS #xa6)
  (define-string-instruction LODS #xac)
  (define-string-instruction INS  #x6c)
  (define-string-instruction MOVS #xa4)
  (define-string-instruction OUTS #x6e)
  (define-string-instruction SCAS #xae)
  (define-string-instruction STOS #xaa))

(define-instruction CMPXCHG
  ((B (? target r/m-ea) (R (? reg)))
   (PREFIX (ModR/M reg target))
   (BITS (8 #x0f)
	 (8 #xb1))
   (ModR/M reg target))

  (((? size operand-size) (? target r/m-ea) (R (? reg)))
   (PREFIX (OPERAND size) (ModR/M reg target))
   (BITS (8 #x0f)
	 (8 #xb0))
   (ModR/M reg target)))

(define-trivial-instruction CPUID #x0F #xA2)

(define-trivial-instruction CWD #x99)
(define-trivial-instruction CDQ #x99)

(let-syntax
    ((define-inc/dec
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (digit (caddr form))
	       (opcode (cadddr form)))
	   `(define-instruction ,mnemonic
	      ;; There is no 64-bit analogue of these: the opcodes
	      ;; have been repurposed for REX prefixes!
	      ;;
	      ;; ((W (R (? reg)))
	      ;;  (PREFIX (OPERAND 'W))
	      ;;  (BITS (8 (+ ,opcode reg))))
	      ;;
	      ;; ((L (R (? reg)))
	      ;;  (BITS (8 (+ ,opcode reg))))

	      ((B (? target r/m-ea))
	       (PREFIX (ModR/M target))
	       (BITS (8 #xfe))
	       (ModR/M ,digit target))

	      (((? size operand-size) (? target r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M target))
	       (BITS (8 #xff))
	       (ModR/M ,digit target))))))))

  (define-inc/dec DEC 1 #x48)
  (define-inc/dec INC 0 #x40))

(let-syntax
    ((define-mul/div
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (digit (caddr form)))
	   `(define-instruction ,mnemonic
	      ((B ((R 2) : (R 0)) (? operand r/m-ea))
	       (PREFIX (ModR/M operand))
	       (BITS (8 #xf6))
	       (ModR/M ,digit operand))

	      (((? size operand-size) ((R 2) : (R 0)) (? operand r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M operand))
	       (BITS (8 #xf7))
	       (ModR/M ,digit operand))))))))

  (define-mul/div DIV 6)
  (define-mul/div IDIV 7)
  (define-mul/div MUL 4))

(define-instruction ENTER
  (((&U (? frame-size unsigned-word)) (&U (? lexical-level unsigned-byte)))
   (BITS (8 #xc8)
	 (16 frame-size UNSIGNED)
	 (8 lexical-level UNSIGNED))))

(define-trivial-instruction HLT #xf4)

(define-instruction IMUL
  ((B (R 0) (? source r/m-ea))
   (PREFIX (ModR/M source))
   (BITS (8 #xf6))
   (ModR/M 5 source))

  (((? size operand-size) ((R 2) : (R 0)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M source))
   (BITS (8 #xf7))
   (ModR/M 5 source))

  (((? size operand-size) (R (? target)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x0f)
	 (8 #xaf))
   (ModR/M target source))

  (((? size operand-size) (R (? target))
			  (? source r/m-ea)
			  (& (? multiplier sign-extended-byte)))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x6b))
   (ModR/M target source)
   (BITS (8 multiplier SIGNED)))

  (((? size operand-size) (R (? target))
			  (? source r/m-ea)
			  (&U (? multiplier zero-extended-byte)))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x6b))
   (ModR/M target source)
   (BITS (8 multiplier SIGNED)))

  ((W (R (? target))
      (? source r/m-ea)
      (& (? multiplier sign-extended-word)))
   (PREFIX (OPERAND 'W) (ModR/M target source))
   (BITS (8 #x69))
   (ModR/M target source)
   (BITS (16 multiplier SIGNED)))

  ((W (R (? target))
      (? source r/m-ea)
      (&U (? multiplier zero-extended-word)))
   (PREFIX (OPERAND 'W) (ModR/M target source))
   (BITS (8 #x69))
   (ModR/M target source)
   (BITS (16 multiplier SIGNED)))

  (((? size operand-size) (R (? target))
			  (? source r/m-ea)
			  (& (? multiplier sign-extended-long)))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x69))
   (ModR/M target source)
   (BITS (32 multiplier SIGNED)))

  (((? size operand-size) (R (? target))
			  (? source r/m-ea)
			  (&U (? multiplier zero-extended-long)))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x69))
   (ModR/M target source)
   (BITS (32 multiplier SIGNED))))

(define-instruction IN
  ((B (R 0) (&U (? port unsigned-byte)))
   (BITS (8 #xe4)
	 (8 port UNSIGNED)))

  ((B (R 0) (R 2))
   (BITS (8 #xec)))

  ((W (R 0) (&U (? port unsigned-byte)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xe5)
	 (8 port UNSIGNED)))

  ((W (R 0) (R 2))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xed)))

  ((L (R 0) (&U (? port unsigned-byte)))
   (BITS (8 #xe5)
	 (8 port UNSIGNED)))

  ((L (R 0) (R 2))
   (BITS (8 #xed))))

(define-instruction INT
  ((3)
   (BITS (8 #xcc)))

  (((&U (? vector unsigned-byte)))
   (BITS (8 #xcd)
	 (8 vector UNSIGNED))))

(define-trivial-instruction INVD #x0f #x08)	; 486 only
(define-trivial-instruction IRET #xcf)

(let-syntax
    ((define-jump-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (short-opcode (caddr form))
	       (near-opcode (cadddr form)))
	   `(define-instruction ,mnemonic
	      (((@PCR (? dest)))
	       (VARIABLE-WIDTH (disp `(- ,dest (+ *PC* 2)))
		 ((#x-80 #x7f)
		  (BITS (8 ,short-opcode)
			(8 disp SIGNED)))
		 ((() ())
		  (BITS (8 #x0f)
			(8 ,near-opcode)
			(32 (- disp 4) SIGNED)))))

	      ((B (@PCR (? dest)))
	       (BITS (8 ,short-opcode)
		     (8 `(- ,dest (+ *PC* 1)) SIGNED)))

	      ((L (@PCR (? dest)))
	       (BITS (8 #x0f)
		     (8 ,near-opcode)
		     (32 `(- ,dest (+ *PC* 4)) SIGNED)))

	      ((B (@PCO (? displ signed-byte)))
	       (BITS (8 ,short-opcode)
		     (8 displ SIGNED)))

	      ((L (@PCO (? displ signed-long)))
	       (BITS (8 #x0f)
		     (8 ,near-opcode)
		     (32 displ SIGNED)))))))))

  (define-jump-instruction JA   #x77 #x87)
  (define-jump-instruction JAE  #x73 #x83)
  (define-jump-instruction JB   #x72 #x82)
  (define-jump-instruction JBE  #x76 #x86)
  (define-jump-instruction JC   #x72 #x82)
  (define-jump-instruction JE   #x74 #x84)
  (define-jump-instruction JG   #x7f #x8f)
  (define-jump-instruction JGE  #x7d #x8d)
  (define-jump-instruction JL   #x7c #x8c)
  (define-jump-instruction JLE  #x7e #x8e)
  (define-jump-instruction JNA  #x76 #x86)
  (define-jump-instruction JNAE #x72 #x82)
  (define-jump-instruction JNB  #x73 #x83)
  (define-jump-instruction JNBE #x77 #x87)
  (define-jump-instruction JNC  #x73 #x83)
  (define-jump-instruction JNE  #x75 #x85)
  (define-jump-instruction JNG  #x7e #x8e)
  (define-jump-instruction JNGE #x7c #x8c)
  (define-jump-instruction JNL  #x7d #x8d)
  (define-jump-instruction JNLE #x7f #x8f)
  (define-jump-instruction JNO  #x71 #x81)
  (define-jump-instruction JNP  #x7b #x8b)
  (define-jump-instruction JNS  #x79 #x89)
  (define-jump-instruction JNZ  #x75 #x85)
  (define-jump-instruction JO   #x70 #x80)
  (define-jump-instruction JP   #x7a #x8a)
  (define-jump-instruction JPE  #x7a #x8a)
  (define-jump-instruction JPO  #x7b #x8b)
  (define-jump-instruction JS   #x78 #x88)
  (define-jump-instruction JZ   #x74 #x84))

(let-syntax
    ((define-loop-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form)))
	   `(define-instruction ,mnemonic
	      ((B (@PCR (? dest)))
	       (BITS (8 ,opcode)
		     (8 `(- ,dest (+ *PC* 1)) SIGNED)))

	      ((B (@PCO (? displ signed-byte)))
	       (BITS (8 ,opcode)
		     (8 displ SIGNED)))))))))

  (define-loop-instruction JCXZ   #xe3)
  (define-loop-instruction JECXZ  #xe3)
  (define-loop-instruction LOOP   #xe2)
  (define-loop-instruction LOOPE  #xe1)
  (define-loop-instruction LOOPZ  #xe1)
  (define-loop-instruction LOOPNE #xe0)
  (define-loop-instruction LOOPNZ #xe0))

(define-instruction JMP
  (((@PCR (? dest)))
   (VARIABLE-WIDTH (disp `(- ,dest (+ *PC* 2)))
     ((#x-80 #x7f)
      (BITS (8 #xeb)
	    (8 disp SIGNED)))
     ((() ())
      (BITS (8 #xe9)
	    (32 (- disp 3) SIGNED)))))

  (((@PCRO (? dest) (? offset)))
   (VARIABLE-WIDTH (disp `(- (+ ,dest ,offset) (+ *PC* 2)))
     ((#x-80 #x7f)
      (BITS (8 #xeb)
	    (8 disp SIGNED)))
     ((() ())
      (BITS (8 #xe9)
	    (32 (- disp 3) SIGNED)))))

  (((? dest r/m-ea))
   (PREFIX (ModR/M dest))
   (BITS (8 #xff))
   (ModR/M 4 dest))

  ((B (@PCR (? dest)))
   (BITS (8 #xeb)
	 (8 `(- ,dest (+ *PC* 1)) SIGNED)))

  ((L (@PCR (? dest)))
   (BITS (8 #xe9)
	 (32 `(- ,dest (+ *PC* 4)) SIGNED)))

  ((B (@PCO (? displ signed-byte)))
   (BITS (8 #xeb)
	 (8 displ SIGNED)))

  ((L (@PCO (? displ signed-long)))
   (BITS (8 #xe9)
	 (32 displ SIGNED))))

(define-trivial-instruction LAHF #x9f)

(define-instruction LEA
  (((? size operand-size) (R (? target)) (? source m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x8d))
   (ModR/M target source)))

(define-trivial-instruction LEAVE #xc9)

(let-syntax
    ((define-load/store-state
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form))
	       (digit (cadddr form)))
	   `(define-instruction ,mnemonic
	      (((? operand m-ea))
	       (BITS (8 #x0f)
		     (8 ,opcode))
	       (ModR/M ,digit operand))))))))

  (define-load/store-state INVLPG #x01 7)	; 486 only
  (define-load/store-state LGDT   #x01 2)
  (define-load/store-state LIDT   #x01 3)
  (define-load/store-state LLDT   #x00 2)
  (define-load/store-state LMSW   #x01 6)
  (define-load/store-state LTR    #x00 3)
  (define-load/store-state SGDT   #x01 0)
  (define-load/store-state SIDT   #x01 1)
  (define-load/store-state SLDT   #x00 0)
  (define-load/store-state SMSW   #x01 4)
  (define-load/store-state STR    #x00 1)
  (define-load/store-state VERR   #x00 4)
  (define-load/store-state VERW   #x00 5))