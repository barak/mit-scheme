#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/instr1.scm,v 1.2 1992/02/09 00:26:28 jinx Exp $

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

	    ((W (? target r/mW) (& (? value full-word-value)))
	     (BYTE (8 #x81))
	     (ModR/M ,digit target)
	     (IMMEDIATE value))

	    ((W (? target r/mW) (& (? value sign-extensible-value)))
	     (BYTE (8 #x83))
	     (ModR/M ,digit target)
	     (BYTE (value SIGNED)))

	    ((W (R 0) (& (? value)))	; AX/EAX
	     (BYTE (8 ,(+ opcode 5)))
	     (IMMEDIATE value))

	    ((B (? target r/m8) (R (? source)))
	     (BYTE (8 ,opcode))
	     (ModR/M source target))

	    ((B (R (? target)) (? source r/m8))
	     (BYTE (8 ,(+ opcode 2)))
	     (ModR/M target source))

	    ((B (R 0) (& (? value)))	; AL
	     (BYTE (8 ,(+ opcode 4))
		   (8 value SIGNED)))

	    ((B (? target r/m8) (& (? value)))
	     (BYTE (8 #x80))
	     (ModR/M ,digit target)
	     (BYTE (8 value SIGNED)))))))

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

(define-instruction BSWAP
  ;; 486 only
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
   (IMMEDIATE `(- ,dest (+ *PC* 2)) ADDRESS))

  (((@PCO ,dest))
   (BYTE (8 #xe8))
   (IMMEDIATE dest ADDRESS))

  (((? dest r/mW))
   (BYTE (8 #xff))
   (ModR/M 2 dest))

  ((I (? dest mW))
   (BYTE (8 #xff))
   (ModR/M 3 dest))

  ((I (SEGMENT (? seg)) (OFFSET (? off)))
   (BYTE (8 #x9a))
   (BYTE (16 seg UNSIGNED))
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
