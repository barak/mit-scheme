#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/instrf.scm,v 1.3 1992/02/13 05:57:19 jinx Exp $

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

;;;; Intel i387/i486 Instruction Set

(declare (usual-integrations))

(define-macro (define-trivial-instruction mnemonic opcode . extra)
  `(define-instruction ,mnemonic
     (()
      (BYTE (8 ,opcode))
      ,@(map (lambda (extra)
	       `(BYTE (8 ,extra)))
	     extra))))

(define-trivial-instruction F2XM1 #xd9 #xf0)
(define-trivial-instruction FABS  #xd9 #xe1)

(let-syntax
    ((define-binary-flonum
       (macro (mnemonic pmnemonic imnemonic digit opcode1 opcode2)
	 `(begin
	    (define-instruction ,mnemonic
	      (((ST 0) (ST ,i))
	       (BYTE (8 #xd8)
		     (8 (+ ,opcode1 i))))

	      (((ST ,i) (ST 0))
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
	      (((ST ,i) (ST 0))
	       (BYTE (8 #xde)
		     (8 (+ #xc0 i)))))

	    (define-instruction ,imnemonic
	      ((L (? source mW))
	       (BYTE (8 #xda))
	       (ModR/M ,digit source))

	      ((H (? source mW))
	       (BYTE (8 #xde))
	       (ModR/M ,digit source)))))))

  (define-binary-flonum FADD  FADDP  FIADD  0 #xc0 #xc0)
  (define-binary-flonum FDIV  FDIVP  FIDIV  6 #xf0 #xf8)
  (define-binary-flonum FDIVR FDIVPR FIDIVR 7 #xf8 #xf0)
  (define-binary-flonum FMUL  FMULP  FIMUL  1 #xc8 #xc8)
  (define-binary-flonum FSUB  FSUBP  FISUB  4 #xe0 #xe8)
  (define-binary-flonum FSUBR FSUBPR FISUBR 5 #xe8 #xe0))

(define-instruction FBLD
  (((? source mW))
   (BYTE (#x d8))
   (ModR/M 4 source)))

(define-instruction FBSTP
  (((? target mW))
   (BYTE (#x df))
   (ModR/M 6 target)))

(define-trivial-instruction FCHS   #xd9 #xe0)
(define-trivial-instruction FCLEX  #x9b #xdb #xe2) ; = (FWAIT) (FNCLEX)
(define-trivial-instruction FNCLEX #xdb #xe2)

(let-syntax
    ((define-flonum-comparison
       (macro (mnemonic digit opcode)
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
	     (ModR/M ,digit source))))))

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
       (macro (mnemonic digit)
	 `(define-instruction ,mnemonic
	    ((L (? source mW))
	     (BYTE (8 #xda))
	     (ModR/M ,digit source))

	    ((H (? source mW))
	     (BYTE (8 #xde))
	     (ModR/M ,digit source))))))

  (define-flonum-integer-comparison FICOM  2)
  (define-flonum-integer-comparison FICOMP 3))

(let-syntax
    ((define-flonum-integer-memory
       (macro (mnemonic digit1 digit2)
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
	     (ModR/M ,digit1 source))))))

  (define-flonum-integer-memory FILD  0 5)
  (define-flonum-integer-memory FIST  2 #f)
  (define-flonum-integer-memory FISTP 3 7))

(define-trivial-instruction FINCSTP #xd9 #xf7)
(define-trivial-instruction FINIT   #x9b #xdb #xe3) = (FWAIT) (FNINT)
(define-trivial-instruction FNINIT  #xdb #xe3)

(let-syntax
    ((define-flonum-memory
       (macro (mnemonic digit1 digit2 opcode1 opcode2)
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
		     (ModR/M ,digit2 operand))))))))

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
       (macro (mnemonic opcode digit mnemonic2)
	 `(begin
	    ,@(if (not mnemonic2)
		  `()
		  `((define-instruction ,mnemonic2
		      (((? source mW))
		       (BYTE (8 #x9b)			; (FWAIT)
			     (8 ,opcode))
		       (ModR/M ,digit source)))))

	    (define-instruction ,mnemonic
	      (((? source mW))
	       (BYTE (8 ,opcode))
	       (ModR/M ,digit source)))))))

  (define-flonum-state FLDCW   #xd9 5 #f)
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
       (macro (mnemonic opcode1 opcode2)
	 `(define-instruction ,mnemonic
	    (((ST 0) (ST (? i)))
	     (BYTE (8 ,opcode1)
		   (8 (+ ,opcode2 i))))

	    (()
	     (BYTE (8 ,opcode1)
		   (8 (+ ,opcode2 1))))))))

  (define-binary-flonum FUCOM  #xdd #xe0)
  (define-binary-flonum FUCOMP #xdd #xe8)
  (define-binary-flonum FXCH   #xd9 #xc8))

(define-trivial-instruction FUCOMPP #xda #xe9)
(define-trivial-instruction FWAIT   #x9b)
(define-trivial-instruction FXAM    #xd9 #xe5)
(define-trivial-instruction FXTRACT #xd9 #xf4)
(define-trivial-instruction FYL2X   #xd9 #xf1)
(define-trivial-instruction FYL2XP1 #xd9 #xf9)