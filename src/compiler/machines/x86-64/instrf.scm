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

;;;; Intel i387/i486 Instruction Set
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

#|

(let-syntax
    ((define-binary-flonum
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (list-ref form 1))
		(pmnemonic (list-ref form 2))
		(imnemonic (list-ref form 3))
		(digit (list-ref form 4))
		(opcode1 (list-ref form 5))
		(opcode2 (list-ref form 6)))
	    `(begin
	       (define-instruction ,mnemonic
		 (((ST 0) (ST (? i)))
		  (BYTE (8 #xd8)
			(8 (+ ,opcode1 i))))

		 (((ST (? i)) (ST 0))
		  (BYTE (8 #xdc)
			(8 (+ ,opcode2 i))))

		 (()
		  (BYTE (8 #xde)
			(8 (+ ,opcode2 1))))

		 ((D (? source mW))
		  (BYTE (8 #xdc))
		  (ModR/M ,digit source))

		 ((S (? source mW))
		  (BYTE (8 #xd8))
		  (ModR/M ,digit source)))

	       (define-instruction ,pmnemonic
		 (((ST (? i)) (ST 0))
		  (BYTE (8 #xde)
			(8 (+ ,opcode2 i)))))

	       (define-instruction ,imnemonic
		 ((L (? source mW))
		  (BYTE (8 #xda))
		  (ModR/M ,digit source))

		 ((H (? source mW))
		  (BYTE (8 #xde))
		  (ModR/M ,digit source)))))))))

  ;; The i486 book (and 387, etc.) has inconsistent instruction
  ;; descriptions and opcode assignments for FSUB and siblings,
  ;; and FDIV and siblings.
  ;; FSUB ST(i),ST is described as replacing ST(i) with ST-ST(i)
  ;; while the opcode described replaces ST(i) with ST(i)-ST.

  ;; In the following, the F% forms follow the descriptions in the
  ;; book, namely, F%SUB computes ST-ST(i) and F%SUBR computes
  ;; ST(i)-ST, storing into their destination (first) argument.

  ;; The %-less forms follow the opcodes and usual convention,
  ;; namely FSUB computes destination (first) argument - source
  ;; argument FSUBR computes source - destination.

  (define-binary-flonum FADD   FADDP   FIADD   0 #xc0 #xc0)
  (define-binary-flonum F%DIV  F%DIVP  F%IDIV  6 #xf0 #xf0)
  (define-binary-flonum F%DIVR F%DIVPR F%IDIVR 7 #xf8 #xf8)
  (define-binary-flonum FDIV   FDIVP   FIDIV   6 #xf0 #xf8)
  (define-binary-flonum FDIVR  FDIVPR  FIDIVR  7 #xf8 #xf0)
  (define-binary-flonum FMUL   FMULP   FIMUL   1 #xc8 #xc8)
  (define-binary-flonum F%SUB  F%SUBP  F%ISUB  4 #xe0 #xe0)
  (define-binary-flonum F%SUBR F%SUBPR F%ISUBR 5 #xe8 #xe8)
  (define-binary-flonum FSUB   FSUBP   FISUB   4 #xe0 #xe8)
  (define-binary-flonum FSUBR  FSUBPR  FISUBR  5 #xe8 #xe0))

(define-trivial-instruction F2XM1 #xd9 #xf0)
(define-trivial-instruction FABS  #xd9 #xe1)

(define-instruction FBLD
  (((? source mW))
   (BYTE (8 #xd8))
   (ModR/M 4 source)))

(define-instruction FBSTP
  (((? target mW))
   (BYTE (8 #xdf))
   (ModR/M 6 target)))

(define-trivial-instruction FCHS   #xd9 #xe0)
(define-trivial-instruction FCLEX  #x9b #xdb #xe2) ; = (FWAIT) (FNCLEX)
(define-trivial-instruction FNCLEX #xdb #xe2)

(let-syntax
    ((define-flonum-comparison
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (cadr form))
		(digit (caddr form))
		(opcode (cadddr form)))
	    `(define-instruction ,mnemonic
	       (((ST 0) (ST (? i)))
		(BYTE (8 #xd8)
		      (8 (+ ,opcode i))))

	       (()
		(BYTE (8 #xd8)
		      (8 (+ ,opcode 1))))

	       ((D (? source mW))
		(BYTE (8 #xdc))
		(ModR/M ,digit source))

	       ((S (? source mW))
		(BYTE (8 #xd8))
		(ModR/M ,digit source))))))))

  (define-flonum-comparison FCOM  2 #xd0)
  (define-flonum-comparison FCOMP 3 #xd8))

(define-trivial-instruction FCOMPP  #xde #xd9)
(define-trivial-instruction FCOS    #xd9 #xff)
(define-trivial-instruction FDECSTP #xd9 #xf6)

(define-instruction FFREE
  (((ST (? i)))
   (BYTE (8 #xdd)
	 (8 (+ #xc0 i)))))

(let-syntax
    ((define-flonum-integer-comparison
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (cadr form))
		(digit (caddr form)))
	    `(define-instruction ,mnemonic
	       ((L (? source mW))
		(BYTE (8 #xda))
		(ModR/M ,digit source))

	       ((H (? source mW))
		(BYTE (8 #xde))
		(ModR/M ,digit source))))))))

  (define-flonum-integer-comparison FICOM  2)
  (define-flonum-integer-comparison FICOMP 3))

(let-syntax
    ((define-flonum-integer-memory
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (cadr form))
		(digit1 (caddr form))
		(digit2 (cadddr form)))
	    `(define-instruction ,mnemonic
	       ,@(if (not digit2)
		     `()
		     `(((Q (? source mW))
			(BYTE (8 #xdf))
			(ModR/M ,digit2 source))))

	       ((L (? source mW))
		(BYTE (8 #xdb))
		(ModR/M ,digit1 source))

	       ((H (? source mW))
		(BYTE (8 #xdf))
		(ModR/M ,digit1 source))))))))

  (define-flonum-integer-memory FILD  0 5)
  (define-flonum-integer-memory FIST  2 #f)
  (define-flonum-integer-memory FISTP 3 7))

(define-trivial-instruction FINCSTP #xd9 #xf7)
(define-trivial-instruction FINIT   #x9b #xdb #xe3) ; = (FWAIT) (FNINT)
(define-trivial-instruction FNINIT  #xdb #xe3)

(let-syntax
    ((define-flonum-memory
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (list-ref form 1))
		(digit1 (list-ref form 2))
		(digit2 (list-ref form 3))
		(opcode1 (list-ref form 4))
		(opcode2 (list-ref form 5)))
	    `(define-instruction ,mnemonic
	       (((ST (? i)))
		(BYTE (8 ,opcode1)
		      (8 (+ ,opcode2 i))))

	       ((D (? operand mW))
		(BYTE (8 #xdd))
		(ModR/M ,digit1 operand))

	       ((S (? operand mW))
		(BYTE (8 #xd9))
		(ModR/M ,digit1 operand))

	       ,@(if (not digit2)
		     `()
		     `(((X (? operand mW))
			(BYTE (8 #xdb))
			(ModR/M ,digit2 operand))))))))))

  (define-flonum-memory FLD  0 5  #xd9 #xc0)
  (define-flonum-memory FST  2 #f #xdd #xd0)
  (define-flonum-memory FSTP 3 7  #xdd #xd8))

(define-trivial-instruction FLD1   #xd9 #xe8)
(define-trivial-instruction FLDL2T #xd9 #xe9)
(define-trivial-instruction FLDL2E #xd9 #xea)
(define-trivial-instruction FLDPI  #xd9 #xeb)
(define-trivial-instruction FLDLG2 #xd9 #xec)
(define-trivial-instruction FLDLN2 #xd9 #xed)
(define-trivial-instruction FLDZ   #xd9 #xee)

(let-syntax
    ((define-flonum-state
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (list-ref form 1))
		(opcode (list-ref form 2))
		(digit (list-ref form 3))
		(mnemonic2 (list-ref form 4)))
	    `(begin
	       ,@(if (not mnemonic2)
		     `()
		     `((define-instruction ,mnemonic2
			 (((? source mW))
			  (BYTE (8 #x9b) ; (FWAIT)
				(8 ,opcode))
			  (ModR/M ,digit source)))))

	       (define-instruction ,mnemonic
		 (((? source mW))
		  (BYTE (8 ,opcode))
		  (ModR/M ,digit source)))))))))

  (define-flonum-state FNLDCW  #xd9 5 FLDCW)
  (define-flonum-state FLDENV  #xd9 4 #f)
  (define-flonum-state FNSTCW  #xd9 7 FSTCW)
  (define-flonum-state FNSTENV #xd9 6 FSTENV)
  (define-flonum-state FRSTOR  #xdb 4 #f)
  (define-flonum-state FNSAVE  #xdd 6 FSAVE))

(define-trivial-instruction FNOP    #xd9 #xd0)
(define-trivial-instruction FPATAN  #xd9 #xf3)
(define-trivial-instruction FPREM   #xd9 #xf8) ; truncating remainder
(define-trivial-instruction FPREM1  #xd9 #xf5) ; IEEE remainder
(define-trivial-instruction FPTAN   #xd9 #xf2)
(define-trivial-instruction FRNDINT #xd9 #xfc)
(define-trivial-instruction FSCALE  #xd9 #xfd)
(define-trivial-instruction FSIN    #xd9 #xfe)
(define-trivial-instruction FSINCOS #xd9 #xfb)
(define-trivial-instruction FSQRT   #xd9 #xfa)

(define-instruction FSTSW
  (((? target mW))
   (BYTE (8 #x9b)			; (FWAIT)
	 (8 #xdf))
   (ModR/M 7 target))

  (((R 0))
   (BYTE (8 #x9b)			; (FWAIT)
	 (8 #xdf)
	 (8 #xe0))))

(define-instruction FNSTSW
  (((? target mW))
   (BYTE (8 #xdf))
   (ModR/M 7 target))

  (((R 0))
   (BYTE (8 #xdf)
	 (8 #xe0))))

(define-trivial-instruction FTST #xd9 #xe4)

(let-syntax
    ((define-binary-flonum
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((mnemonic (cadr form))
		(opcode1 (caddr form))
		(opcode2 (cadddr form)))
	    `(define-instruction ,mnemonic
	       (((ST 0) (ST (? i)))
		(BYTE (8 ,opcode1)
		      (8 (+ ,opcode2 i))))

	       (()
		(BYTE (8 ,opcode1)
		      (8 (+ ,opcode2 1))))))))))

  (define-binary-flonum FUCOM  #xdd #xe0)
  (define-binary-flonum FUCOMP #xdd #xe8)
  (define-binary-flonum FXCH   #xd9 #xc8))

(define-trivial-instruction FUCOMPP #xda #xe9)
(define-trivial-instruction FWAIT   #x9b)
(define-trivial-instruction FXAM    #xd9 #xe5)
(define-trivial-instruction FXTRACT #xd9 #xf4)
(define-trivial-instruction FYL2X   #xd9 #xf1)
(define-trivial-instruction FYL2XP1 #xd9 #xf9)

|#