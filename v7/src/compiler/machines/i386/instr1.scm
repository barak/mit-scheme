#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/instr1.scm,v 1.1 1992/02/08 18:15:41 jinx Exp $

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

;;;; Intel 386 Instruction Set, part I

(declare (usual-integrations))

;; Utility

(define-macro (define-trivial-instruction mnemonic opcode)
  `(define-instruction ,mnemonic
     (()
      (BYTE (8 ,opcode)))))

(define-macro (define-trivial-instruction/2 mnemonic escape opcode)
  `(define-instruction ,mnemonic
     (()
      (BYTE (8 ,escape)
	    (8 ,opcode)))))

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

(define-trivial-instruction   AAA #x37)
(define-trivial-instruction/2 AAD #xd5 #x0a)
(define-trivial-instruction/2 AAM #xd4 #x0a)
(define-trivial-instruction   AAS #x3f)

(let-syntax
    ((define-arithmetic-instruction
       (macro (mnemonic opcode digit)
	 `(define-instruction ADC
	    ((B (R 0) (& (? value)))
	     (BYTE (8 ,(+ opcode 4))
		   (8 value SIGNED)))

	    ((W (R 0) (& (? value)))
	     (BYTE (8 ,(+ opcode 5)))
	     (IMMEDIATE value SIGNED))

	    ((B (? target r/m8) (& (? value)))
	     (BYTE (8 #x80))
	     (ModR/M ,digit target)
	     (BYTE (8 value SIGNED)))

	    ((W (? target r/mW) (& (? value full-word-value)))
	     (BYTE (8 #x81))
	     (ModR/M ,digit target)
	     (IMMEDIATE value SIGNED))

	    ((W (? target r/mW) (& (? value sign-extensible-value)))
	     (BYTE (8 #x83))
	     (ModR/M ,digit target)
	     (BYTE (value SIGNED)))

	    ((B (? target r/m8) (R (? source)))
	     (BYTE (8 ,opcode))
	     (ModR/M source target))

	    ((W (? target r/mW) (R (? source)))
	     (BYTE (8 ,(1+ opcode)))
	     (ModR/M source target))

	    ((B (R (? target)) (? source r/m8))
	     (BYTE (8 ,(+ opcode 2)))
	     (ModR/M target source))

	    ((W (R (? target)) (? source r/mW))
	     (BYTE (8 ,(+ opcode 3)))
	     (ModR/M target source))))))

  (define-arithmetic-instruction ADC #x10 2)
  (define-arithmetic-instruction ADD #x00 0)
  (define-arithmetic-instruction AND #x20 4)
  (define-arithmetic-instruction CMP #x38 7)
  (define-arithmetic-instruction OR  #x08 1)
  (define-arithmetic-instruction SBB #x18 3)
  (define-arithmetic-instruction SUB #x28 5)
  (define-arithmetic-instruction XOR #x30 6))