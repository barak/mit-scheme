#| -*-Scheme-*-

$Id: 2086a86d0eb76166f170d5c28980fbe18beeef76 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Intel i386 Instruction Set, part I
;;; package: (compiler lap-syntaxer)

;; Some of the instructions have their operands ill-specified in the
;; i486 book.  Check against the appendices or the i386 book.

(declare (usual-integrations))

;;;; Pseudo ops

(define-instruction BYTE
  ((S (? value))
   (BYTE (8 value SIGNED)))
  ((U (? value))
   (BYTE (8 value UNSIGNED))))

(define-instruction WORD
  ((S (? value))
   (BYTE (16 value SIGNED)))
  ((U (? value))
   (BYTE (16 value UNSIGNED))))

(define-instruction LONG
  ((S (? value))
   (BYTE (32 value SIGNED)))
  ((U (? value))
   (BYTE (32 value UNSIGNED))))

;;;; Actual instructions

(define-trivial-instruction AAA #x37)
(define-trivial-instruction AAD #xd5 #x0a)
(define-trivial-instruction AAM #xd4 #x0a)
(define-trivial-instruction AAS #x3f)

(let-syntax
    ((define-arithmetic-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form))
	       (digit (cadddr form)))
	   `(define-instruction ,mnemonic
	      ((W (? target r/mW) (R (? source)))
	       (BYTE (8 ,(+ opcode 1)))
	       (ModR/M source target))

	      ((W (R (? target)) (? source r/mW))
	       (BYTE (8 ,(+ opcode 3)))
	       (ModR/M target source))

	      ((W (? target r/mW) (& (? value sign-extended-byte)))
	       (BYTE (8 #x83))
	       (ModR/M ,digit target)
	       (BYTE (8 value SIGNED)))

	      ((W (R 0) (& (? value)))	; AX/EAX
	       (BYTE (8 ,(+ opcode 5)))
	       (IMMEDIATE value))

	      ((W (? target r/mW) (& (? value)))
	       (BYTE (8 #x81))
	       (ModR/M ,digit target)
	       (IMMEDIATE value))

	      ((W (? target r/mW) (&U (? value zero-extended-byte)))
	       (BYTE (8 #x83))
	       (ModR/M ,digit target)
	       (BYTE (8 value UNSIGNED)))

	      ((W (R 0) (&U (? value)))	; AX/EAX
	       (BYTE (8 ,(+ opcode 5)))
	       (IMMEDIATE value OPERAND UNSIGNED))

	      ((W (? target r/mW) (&U (? value)))
	       (BYTE (8 #x81))
	       (ModR/M ,digit target)
	       (IMMEDIATE value OPERAND UNSIGNED))

	      ((B (? target r/mB) (R (? source)))
	       (BYTE (8 ,opcode))
	       (ModR/M source target))

	      ((B (R (? target)) (? source r/mB))
	       (BYTE (8 ,(+ opcode 2)))
	       (ModR/M target source))

	      ((B (R 0) (& (? value)))	; AL
	       (BYTE (8 ,(+ opcode 4))
		     (8 value SIGNED)))

	      ((B (R 0) (&U (? value)))	; AL
	       (BYTE (8 ,(+ opcode 4))
		     (8 value UNSIGNED)))

	      ((B (? target r/mB) (& (? value)))
	       (BYTE (8 #x80))
	       (ModR/M ,digit target)
	       (BYTE (8 value SIGNED)))

	      ((B (? target r/mB) (&U (? value)))
	       (BYTE (8 #x80))
	       (ModR/M ,digit target)
	       (BYTE (8 value UNSIGNED)))))))))

  (define-arithmetic-instruction ADC #x10 2)
  (define-arithmetic-instruction ADD #x00 0)
  (define-arithmetic-instruction AND #x20 4)
  (define-arithmetic-instruction CMP #x38 7)
  (define-arithmetic-instruction OR  #x08 1)
  (define-arithmetic-instruction SBB #x18 3)
  (define-arithmetic-instruction SUB #x28 5)
  (define-arithmetic-instruction XOR #x30 6))

(define-instruction ARPL
  (((? target r/mW) (R (? source)))
   (BYTE (8 #x63))
   (ModR/M source target)))

(define-instruction BOUND
  (((R (? source)) (? bounds mW))
   (BYTE (8 #x62))
   (ModR/M source bounds)))

(define-instruction BSF
  (((R (? target)) (? source r/mW))
   (BYTE (8 #x0f)
	 (8 #xbc))
   (ModR/M target source)))

(define-instruction BSR
  (((R (? target)) (? source r/mW))
   (BYTE (8 #x0f)
	 (8 #xbd))
   (ModR/M target source)))

(define-instruction BSWAP			; 486 only
  (((R (? reg)))
   (BYTE (8 #x0f)
	 (8 (+ #xc8 reg)))))

(let-syntax
    ((define-bit-test-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form))
	       (digit (cadddr form)))
	   `(define-instruction ,mnemonic

	      (((? target r/mW) (& (? posn)))
	       (BYTE (8 #x0f)
		     (8 #xba))
	       (ModR/M ,digit target)
	       (BYTE (8 posn UNSIGNED)))

	      (((? target r/mW) (R (? posn)))
	       (BYTE (8 #x0f)
		     (8 ,opcode))
	       (ModR/M posn target))))))))

  (define-bit-test-instruction BT  #xa3 4)
  (define-bit-test-instruction BTC #xbb 7)
  (define-bit-test-instruction BTR #xb3 6)
  (define-bit-test-instruction BTS #xab 5))
  
(define-instruction CALL
  (((@PCR (? dest)))
   (BYTE (8 #xe8))
   (IMMEDIATE `(- ,dest (+ *PC* 4)) ADDRESS)) ; fcn(*ADDRESS-SIZE*)

  (((@PCRO (? dest) (? offset)))
   (BYTE (8 #xe8))
   (IMMEDIATE `(- (+ ,dest ,offset) (+ *PC* 4)) ADDRESS)); fcn(*ADDRESS-SIZE*)

  (((@PCO (? displ)))
   (BYTE (8 #xe8))
   (IMMEDIATE displ ADDRESS))

  (((? dest r/mW))
   (BYTE (8 #xff))
   (ModR/M 2 dest))

  ((F (? dest mW))
   (BYTE (8 #xff))
   (ModR/M 3 dest))

  ((F (SEGMENT (? seg)) (OFFSET (? off)))
   (BYTE (8 #x9a))
   (BYTE (16 seg))
   (IMMEDIATE off ADDRESS)))

(define-trivial-instruction CBW #x98)
(define-trivial-instruction CWDE #x98)
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

	      ((W)
	       (BYTE (8 ,(+ opcode 1))))

	      ((B)
	       (BYTE (8 ,opcode)))))))))

  (define-string-instruction CMPS #xa6)
  (define-string-instruction LODS #xac)
  (define-string-instruction INS  #x6c)
  (define-string-instruction MOVS #xa4)
  (define-string-instruction OUTS #x6e)
  (define-string-instruction SCAS #xae)
  (define-string-instruction STOS #xaa))

(define-instruction CMPXCHG			; 486 only
  ((W (? target r/mW) (R (? reg)))
   (BYTE (8 #x0f)
	 (8 #xa7))
   (ModR/M reg target))

  ((B (? target r/mB) (R (? reg)))
   (BYTE (8 #x0f)
	 (8 #xa6))
   (ModR/M reg target)))

(define-trivial-instruction CPUID #x0F #xA2)

(define-trivial-instruction CWD #x99)
(define-trivial-instruction CDQ #x99)
(define-trivial-instruction DAA #x27)
(define-trivial-instruction DAS #x2f)

(let-syntax
    ((define-inc/dec
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (digit (caddr form))
	       (opcode (cadddr form)))
	   `(define-instruction ,mnemonic
	      ((W (R (? reg)))
	       (BYTE (8 (+ ,opcode reg))))

	      ((W (? target r/mW))
	       (BYTE (8 #xff))
	       (ModR/M ,digit target))

	      ((B (? target r/mB))
	       (BYTE (8 #xfe))
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
	      ((W (R 0) (? operand r/mW))
	       (BYTE (8 #xf7))
	       (ModR/M ,digit operand))

	      ((B (R 0) (? operand r/mB))
	       (BYTE (8 #xf6))
	       (ModR/M ,digit operand))))))))

  (define-mul/div DIV 6)
  (define-mul/div IDIV 7)
  (define-mul/div MUL 4))

(define-instruction ENTER
  (((& (? frame-size)) (& (? lexical-level)))
   (BYTE (8 #xc8)
	 (16 frame-size)
	 (8 lexical-level))))

(define-trivial-instruction HLT #xf4)

(define-instruction IMUL
  ((W (R (? target)) (? source r/mW))
   (BYTE (8 #x0f)
	 (8 #xaf))
   (ModR/M target source))

  ((W (R (? target)) (? source r/mW) (& (? value sign-extended-byte)))
   (BYTE (8 #x6b))
   (ModR/M target source)
   (BYTE (8 value SIGNED)))

  ((W (R (? target)) (? source r/mW) (& (? value)))
   (BYTE (8 #x69))
   (ModR/M target source)
   (IMMEDIATE value))

  ((W (R (? target)) (? source r/mW) (&U (? value zero-extended-byte)))
   (BYTE (8 #x6b))
   (ModR/M target source)
   (BYTE (8 value UNSIGNED)))

  ((W (R (? target)) (? source r/mW) (&U (? value)))
   (BYTE (8 #x69))
   (ModR/M target source)
   (IMMEDIATE value OPERAND UNSIGNED))

  ((W ((R 2) : (R 0)) (? source r/mW))
   (BYTE (8 #xf7))
   (ModR/M 5 source))

  ((B (R 0) (? source r/mB))
   (BYTE (8 #xf6))
   (ModR/M 5 source)))

(define-instruction IN
  ((W (R 0) (& (? port)))
   (BYTE (8 #xe5)
	 (8 port)))

  ((W (R 0) (R 2))
   (BYTE (8 #xed)))

  ((B (R 0) (& (? port)))
   (BYTE (8 #xe4)
	 (8 port)))

  ((B (R 0) (R 2))
   (BYTE (8 #xec))))

(define-instruction INT
  ((3)
   (BYTE (8 #xcc)))

  (((& (? vector)))
   (BYTE (8 #xcd)
	 (8 vector))))

(define-trivial-instruction INTO #xce)
(define-trivial-instruction INVD #x0f #x08)	; 486 only
(define-trivial-instruction IRET #xcf)

(let-syntax
    ((define-jump-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode1 (caddr form))
	       (opcode2 (cadddr form)))
	   `(define-instruction ,mnemonic
	      ;; This assumes that *ADDRESS-SIZE* is 4 (32-bit mode)
	      (((@PCR (? dest)))
	       (VARIABLE-WIDTH
		(disp `(- ,dest (+ *PC* 2)))
		((-128 127)
		 (BYTE (8 ,opcode1)
		       (8 disp SIGNED)))
		((() ())
		 (BYTE (8 #x0f)
		       (8 ,opcode2)
		       (32 (- disp 4) SIGNED)))))

	      ((B (@PCR (? dest)))
	       (BYTE (8 ,opcode1)
		     (8 `(- ,dest (+ *PC* 1)) SIGNED)))

	      ((W (@PCR (? dest)))
	       (BYTE (8 #x0f)
		     (8 ,opcode2))
	       (IMMEDIATE `(- ,dest (+ *PC* 4)) ADDRESS)) ; fcn(*ADDRESS-SIZE*)

	      ((B (@PCO (? displ)))
	       (BYTE (8 ,opcode1)
		     (8 displ SIGNED)))

	      ((W (@PCO (? displ)))
	       (BYTE (8 #x0f)
		     (8 ,opcode2))
	       (IMMEDIATE displ ADDRESS))))))))

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
	       (BYTE (8 ,opcode)
		     (8 `(- ,dest (+ *PC* 1)) SIGNED)))

	      ((B (@PCO (? displ)))
	       (BYTE (8 ,opcode)
		     (8 displ SIGNED)))))))))

  (define-loop-instruction JCXZ   #xe3)
  (define-loop-instruction JECXZ  #xe3)
  (define-loop-instruction LOOP   #xe2)
  (define-loop-instruction LOOPE  #xe1)
  (define-loop-instruction LOOPZ  #xe1)
  (define-loop-instruction LOOPNE #xe0)
  (define-loop-instruction LOOPNZ #xe0))

(define-instruction JMP
  ;; This assumes that *ADDRESS-SIZE* is 4 (32-bit mode)
  (((@PCR (? dest)))
   (VARIABLE-WIDTH
    (disp `(- ,dest (+ *PC* 2)))
    ((-128 127)
     (BYTE (8 #xeb)
	   (8 disp SIGNED)))
    ((() ())
     (BYTE (8 #xe9)
	   (32 (- disp 3) SIGNED)))))

  (((@PCRO (? dest) (? offset)))
   (VARIABLE-WIDTH
    (disp `(- (+ ,dest ,offset) (+ *PC* 2)))
    ((-128 127)
     (BYTE (8 #xeb)
	   (8 disp SIGNED)))
    ((() ())
     (BYTE (8 #xe9)
	   (32 (- disp 3) SIGNED)))))

  (((? dest r/mW))
   (BYTE (8 #xff))
   (ModR/M 4 dest))

  ((B (@PCR (? dest)))
   (BYTE (8 #xeb)
	 (8 `(- ,dest (+ *PC* 1)) SIGNED)))

  ((W (@PCR (? dest)))
   (BYTE (8 #xe9))
   (IMMEDIATE `(- ,dest (+ *PC* 4)) ADDRESS)) ; fcn(*ADDRESS-SIZE*)

  ((B (@PCO (? displ)))
   (BYTE (8 #xeb)
	 (8 displ SIGNED)))

  ((W (@PCO (? displ)))
   (BYTE (8 #xe9))
   (IMMEDIATE displ ADDRESS))

  ((F (? dest mW))
   (BYTE (8 #xff))
   (ModR/M 5 dest))

  ((F (SEGMENT (? seg)) (OFFSET (? off)))
   (BYTE (8 #xea))
   (BYTE (16 seg))
   (IMMEDIATE off ADDRESS)))

(define-trivial-instruction LAHF #x9f)

(define-instruction LAR
  (((R (? target)) (? source r/mW))
   (BYTE (8 #x0f)
	 (8 #x02))
   (ModR/M target source)))

(define-instruction LEA
  (((R (? target)) (? source mW))
   (BYTE (8 #x8d))
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
	      (((? operand mW))
	       (BYTE (8 #x0f)
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