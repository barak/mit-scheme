#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/instr1.scm,v 1.3 1992/02/09 03:44:53 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Intel i386 Instruction Set, part I

;; Some of the instructions have their operands ill-specified in the
;; i486 book.  Check against the appendices or the i386 book.

(declare (usual-integrations))

;; Utility

(define-macro (define-trivial-instruction mnemonic opcode . extra)
  `(define-instruction ,mnemonic
     (()
      (BYTE (8 ,opcode))
      ,@(map (lambda (extra)
	       `(BYTE (8 ,extra)))
	     extra))))

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
       (macro (mnemonic opcode digit)
	 `(define-instruction ,mnemonic
	    ((W (? target r/mW) (R (? source)))
	     (BYTE (8 ,(1+ opcode)))
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

	    ((B (? target r/m8) (R (? source)))
	     (BYTE (8 ,opcode))
	     (ModR/M source target))

	    ((B (R (? target)) (? source r/m8))
	     (BYTE (8 ,(+ opcode 2)))
	     (ModR/M target source))

	    ((B (R 0) (& (? value)))	; AL
	     (BYTE (8 ,(+ opcode 4))
		   (8 value SIGNED)))

	    ((B (R 0) (&U (? value)))	; AL
	     (BYTE (8 ,(+ opcode 4))
		   (8 value UNSIGNED)))

	    ((B (? target r/m8) (& (? value)))
	     (BYTE (8 #x80))
	     (ModR/M ,digit target)
	     (BYTE (8 value SIGNED)))

	    ((B (? target r/m8) (&U (? value)))
	     (BYTE (8 #x80))
	     (ModR/M ,digit target)
	     (BYTE (8 value UNSIGNED)))))))

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
   (ModR/M source target 16)))

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
       (macro (mnemonic opcode digit)
	 `(define-instruction ,mnemonic
	    (((? target r/mW) (& (? posn)))
	     (BYTE (8 #x0f)
		   (8 #xba))
	     (ModR/M ,digit target)
	     (BYTE (8 posn UNSIGNED)))

	    (((? target r/mW) (R (? posn)))
	     (BYTE (8 #x0f)
		   (8 ,opcode))
	     (ModR/M posn target))))))

  (define-bit-test-instruction BT  #xa3 4)
  (define-bit-test-instruction BTC #xbb 7)
  (define-bit-test-instruction BTR #xb3 6)
  (define-bit-test-instruction BTS #xab 5))
  
(define-instruction CALL
  (((@PCR ,dest))
   (BYTE (8 #xe8))
   (IMMEDIATE `(- ,dest (+ *PC* ,*ADDRESS-SIZE*)) ADDRESS))

  (((@PCO ,displ))
   (BYTE (8 #xe8))
   (IMMEDIATE displ ADDRESS))

  (((? dest r/mW))
   (BYTE (8 #xff))
   (ModR/M 2 dest))

  ((I (? dest mW))
   (BYTE (8 #xff))
   (ModR/M 3 dest))

  ((I (SEGMENT (? seg)) (OFFSET (? off)))
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

(define-instruction CMPS
  ((W)
   (BYTE (8 #xa7)))

  ((B)
   (BYTE (8 #xa6))))

(define-instruction CMPXCHG
  ((W (? target r/mW) (R (? reg)))
   (BYTE (8 #x0f)
	 (8 #xa7))
   (ModR/M reg target))

  ((B (? target r/m8) (R (? reg)))
   (BYTE (8 #x0f)
	 (8 #xa6))
   (ModR/M reg target)))

(define-trivial-instruction CWD #x99)
(define-trivial-instruction CDQ #x99)
(define-trivial-instruction DAA #x27)
(define-trivial-instruction DAS #x2f)

(let-syntax
    ((define-inc/dec
       (macro (mnemonic digit opcode)
	 `(define-instruction ,mnemonic
	    ((W (R (? reg)))
	     (BYTE (8 (+ ,opcode reg))))

	    ((W (? target r/mW))
	     (BYTE (8 #xff))
	     (ModR/M ,digit target))

	    ((B (? target r/m8))
	     (BYTE (8 #xfe))
	     (ModR/M ,digit target))))))

  (define-inc/dec DEC 1 #x48)
  (define-inc/dec INC 0 #x40))

(let-syntax
    ((define-mul/div
       (macro (mnemonic digit)
	 `(define-instruction ,mnemonic
	    ((W (R 0) (? operand r/mW))
	     (BYTE (8 #xf7))
	     (ModR/M digit operand))

	    ((B (R 0) (? operand r/m8))
	     (BYTE (8 #xf6))
	     (ModR/M digit operand))))))

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

  ((B (R 0) (? source r/m8))
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

(define-instruction INS
  ((W)
   (BYTE (8 #x6d)))

  ((B)
   (BYTE (8 #x6c))))

(define-instruction INT
  ((3)
   (BYTE (8 #xcc)))

  (((& (? vector)))
   (BYTE (8 #xcd)
	 (8 vector))))

(define-trivial-instruction INTO #xce)
(define-trivial-instruction INVD #x0f #x08)	; 486 only?

(define-instruction INVLPG			; 486 only?
  (((? address mW))
   (BYTE (8 #x0f)
	 (8 #x01))
   (ModR/M 7 address)))

(define-trivial-instruction IRET #xcf)

(let-syntax
    ((define-jump-instruction
       (macro (mnemonic opcode1 opcode2)
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
	     (IMMEDIATE `(- ,dest (+ *PC* ,*ADDRESS-SIZE*)) ADDRESS))

	    ((B (@PCO (? displ)))
	     (BYTE (8 ,opcode1)
		   (8 displ SIGNED)))

	    ((W (@PCO (? displ)))
	     (BYTE (8 #x0f)
		   (8 ,opcode2))
	     (IMMEDIATE displ ADDRESS))))))

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
  (define-jump-instruction JS   #x74 #x84)
  (define-jump-instruction JZ   #x74 #x84))
  
(let-syntax
    ((define-loop-instruction
       (macro (mnemonic opcode)
	 `(define-instruction ,mnemonic
	    ((B (@PCR (? dest)))
	     (BYTE (8 ,opcode)
		   (8 `(- ,dest (+ *PC* 1)) SIGNED)))

	    ((B (@PCO (? displ)))
	     (BYTE (8 ,opcode)
		   (8 displ SIGNED)))))))

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

  (((? dest r/mW))
   (BYTE (8 #xff))
   (ModR/M 4 dest))

  ((B (@PCR (? dest)))
   (BYTE (8 #xeb)
	 (8 `(- ,dest (+ *PC* 1)) SIGNED)))

  ((W (@PCR (? dest)))
   (BYTE (8 #xe9))
   (IMMEDIATE `(- ,dest (+ *PC* ,*ADDRESS-SIZE*)) ADDRESS))

  ((B (@PCO (? displ)))
   (BYTE (8 #xeb)
	 (8 displ SIGNED)))

  ((W (@PCO (? displ)))
   (BYTE (8 #xe9))
   (IMMEDIATE displ ADDRESS))

  ((I (? dest mW))
   (BYTE (8 #xff))
   (ModR/M 5 dest))

  ((I (SEGMENT (? seg)) (OFFSET (? off)))
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

(define-instruction LGDT
  (((? source mW))
   (BYTE (8 #x0f)
	 (8 #x01))
   (ModR/M 2 source)))

(define-instruction LIDT
  (((? source mW))
   (BYTE (8 #x0f)
	 (8 #x01))
   (ModR/M 3 source)))

(let-syntax
    ((define-load-segment
       (macro (mnemonic . bytes)
	 `(define-instruction ,mnemonic
	    (((R (? reg)) (? pointer mW))
	     (BYTE ,@(map (lambda (byte)
			    `(8 ,byte))
			  bytes))
	     (ModR/M reg pointer))))))

  (define-load-segment LDS #xc5)
  (define-load-segment LSS #x0f #xb2)
  (define-load-segment LES #xc4)
  (define-load-segment LFS #x0f #xb4)
  (define-load-segment LGS #x0f #xb5))

(let-syntax
    ((define-load-state
       (macro (mnemonic opcode digit)
	 `(define-instruction ,mnemonic
	    (((? source r/mW))
	     (BYTE (8 #x0f)
		   (8 ,opcode))
	     (ModR/M ,digit source))))))

  (define-load-state LLDT #x00 2)
  (define-load-state LMSW #x01 6))

(define-trivial-instruction LOCK #xf0)

(define-instruction LODS
  ((B)
   (BYTE (8 #xac)))

  ((W)
   (BYTE (8 #xad))))

(define-instruction LSL
  (((R (? reg)) (? source r/mW))
   (BYTE (8 #x0f)
	 (8 #x03))
   (ModR/M reg source)))

(define-instruction LTR
  (((? source r/mW))
   (BYTE (8 #x0f)
	 (8 #x00))
   (ModR/M 3 source)))

(define-instruction MOV
  ;; **** Missing MOV to/from special registers ****
  ((W (R (? target)) (? source r/mW))
   (BYTE (8 #x8b))
   (ModR/M target source))

  ((W (? target r/mW) (R (? source)))
   (BYTE (8 #x89))
   (ModR/M source target))

  ((W (R (? reg)) (& (? value)))
   (BYTE (8 (+ #xb8 reg)))
   (IMMEDIATE value))

  ((W (? target r/mW) (& (? value)))
   (BYTE (8 #xc7))
   (ModR/M 0 target)
   (IMMEDIATE value))

  ((W (R (? reg)) (&U (? value)))
   (BYTE (8 (+ #xb8 reg)))
   (IMMEDIATE value OPERAND UNSIGNED))

  ((W (? target r/mW) (&U (? value)))
   (BYTE (8 #xc7))
   (ModR/M 0 target)
   (IMMEDIATE value OPERAND UNSIGNED))

  ((B (R (? target)) (? source r/m8))
   (BYTE (8 #x8a))
   (ModR/M target source))

  ((B (? target r/m8) (R (? source)))
   (BYTE (8 #x88))
   (ModR/M source target))

  ((B (R (? reg)) (& (? value)))
   (BYTE (8 (+ #xb0 reg))
	 (8 value SIGNED)))

  ((B (? target r/m8) (& (? value)))
   (BYTE (8 #xc6))
   (ModR/M 0 target)
   (BYTE (8 value SIGNED)))

  ((B (R (? reg)) (&U (? value)))
   (BYTE (8 (+ #xb0 reg))
	 (8 value UNSIGNED)))

  ((B (? target r/m8) (&U (? value)))
   (BYTE (8 #xc6))
   (ModR/M 0 target)
   (BYTE (8 value UNSIGNED)))

  ((W (R 0) (@ (? offset)))
   (BYTE (8 #xa1))
   (IMMEDIATE offset))

  ((W (@ (? offset)) (R 0))
   (BYTE (8 #xa3))
   (IMMEDIATE offset))

  ((B (R 0) (@ (? offset)))
   (BYTE (8 #xa0)
	 (8 offset SIGNED)))

  ((B (@ (? offset)) (R 0))
   (BYTE (8 #xa2)
	 (8 offset SIGNED)))

  (((? target r/mW) (S (? source)))
   (BYTE (8 #x8c))
   (ModR/M source target))

  (((S (? target)) (? source r/mW))
   (BYTE (8 #x8e))
   (ModR/M target source)))

(define-instruction MOVS
  ((B)
   (BYTE (8 #xa4)))

  ((W)
   (BYTE (8 #xa5))))

(let-syntax
    ((define-data-extension
       (macro (mnemonic opcode)
	 `(define-instruction ,mnemonic
	    ((B (R (? target)) (? source r/m8))
	     (BYTE (8 #x0f)
		   (8 ,opcode))
	     (ModR/M target source))

	    ((H (R (? target)) (? source r/mW))
	     (BYTE (8 #x0f)
		   (8 ,(1+ opcode)))
	     (ModR/M target source))))))

  (define-data-extension MOVSX #xbe)
  (define-data-extension MOVZX #xb6))

(let-syntax
    ((define-unary
       (macro (mnemonic digit)
	 `(define-instruction ,mnemonic
	    ((W (? operand r/mW))
	     (BYTE (8 #xf7))
	     (ModR/M ,digit operand))

	    ((B (? operand r/m8))
	     (BYTE (8 #xf6))
	     (ModR/M ,digit operand))))))

  (define-unary NEG 3)
  (define-unary NOT 2))

(define-trivial-instruction NOP #x90)

(define-instruction OUT
  ((W (& (? port)) (R 0))
   (BYTE (8 #xe7)
	 (8 port)))

  ((W (R 2) (R 0))
   (BYTE (8 #xef)))

  ((B (& (? port)) (R 0))
   (BYTE (8 #xe6)
	 (8 port)))

  ((B (R 2) (R 0))
   (BYTE (8 #xee))))

(define-instruction OUTS
  ((W)
   (BYTE (8 #x6f)))

  ((B)
   (BYTE (8 #x6e))))

(define-instruction POP
  (((R (? target)))
   (BYTE (8 (+ #x58 target))))

  (((? target mW))
   (BYTE (8 #x8f))
   (ModR/M 0 target))

  (((DS))
   (BYTE (8 #x1f)))

  (((ES))
   (BYTE (8 #x07)))

  (((SS))
   (BYTE (8 #x17)))

  (((FS))
   (BYTE (8 #x0f)
	 (8 #xa1)))

  (((GS))
   (BYTE (8 #x0f)
	 (8 #xa9))))

(define-trivial-instruction POPA #x61)
(define-trivial-instruction POPAD #x61)
(define-trivial-instruction POPF #x9d)
(define-trivial-instruction POPFD #x9d)

(define-instruction PUSH
  (((R (? source)))
   (BYTE (8 (+ #x50 source))))

  (((? source mW))
   (BYTE (8 #xff))
   (ModR/M 6 source))

  ((W (& (? value)))
   (BYTE (8 #x68))
   (IMMEDIATE value))

  ((W (&U (? value)))
   (BYTE (8 #x6a))
   (IMMEDIATE value OPERAND UNSIGNED))

  ((B (& (? value)))
   (BYTE (8 #x6a)
	 (8 value)))

  ((B (&U (? value)))
   (BYTE (8 #x6a)
	 (8 value UNSIGNED)))

  (((CS))
   (BYTE (8 #x0e)))

  (((SS))
   (BYTE (8 #x16)))

  (((DS))
   (BYTE (8 #x1e)))

  (((ES))
   (BYTE (8 #x06)))

  (((FS))
   (BYTE (8 #x0f)
	 (8 #xa0)))

  (((GS))
   (BYTE (8 #x0f)
	 (8 #xa8))))

(define-trivial-instruction PUSHA  #x60)
(define-trivial-instruction PUSHAD #x60)
(define-trivial-instruction PUSHF  #x9c)
(define-trivial-instruction PUSHFD #x9c)