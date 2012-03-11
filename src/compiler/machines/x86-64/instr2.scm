#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; AMD x86-64 Instruction Set, part II
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Actual instructions

(let-syntax
    ((define-load-segment
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (byte1 (caddr form))
	       (byte2 (cadddr form)))
	   `(define-instruction ,mnemonic
	      (((? size operand-size) (R (? reg)) (? pointer m-ea))
	       (PREFIX (OPERAND size) (ModR/M reg pointer))
	       (BITS (8 ,byte1)
		     (8 ,byte2))
	       (ModR/M reg pointer))))))))

  (define-load-segment LSS #x0f #xb2)
  (define-load-segment LFS #x0f #xb4)
  (define-load-segment LGS #x0f #xb5))

(define-instruction LSL
  (((? size operand-size) (R (? reg)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M reg source))
   (BITS (8 #x0f)
	 (8 #x03))
   (ModR/M reg source)))

(let-syntax
    ((define-data-extension
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form)))
	   `(define-instruction ,mnemonic
	      (((? size operand-size) (R (? target)) B (? source r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M target source))
	       (BITS (8 #x0f)
		     (8 ,opcode))
	       (ModR/M target source))

	      (((? size operand-size) (R (? target)) H (? source r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M target source))
	       (BITS (8 #x0f)
		     (8 ,(+ opcode 1)))
	       (ModR/M target source))))))))

  (define-data-extension MOVSX #xbe)
  (define-data-extension MOVZX #xb6))

(let-syntax
    ((define-unary
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (digit (caddr form)))
	   `(define-instruction ,mnemonic
	      ((B (? operand r/m-ea))
	       (PREFIX (ModR/M operand))
	       (BITS (8 #xf6))
	       (ModR/M ,digit operand))

	      (((? size operand-size) (? operand r/m-ea))
	       (PREFIX (OPERAND size) (ModR/M operand))
	       (BITS (8 #xf7))
	       (ModR/M ,digit operand))))))))

  (define-unary NEG 3)
  (define-unary NOT 2))

(define-instruction MOV
  ((B (? target r/m-ea) (R (? source)))
   (PREFIX (ModR/M source target))
   (BITS (8 #x88))
   (ModR/M source target))

  (((? size operand-size) (? target r/m-ea) (R (? source)))
   (PREFIX (OPERAND size) (ModR/M source target))
   (BITS (8 #x89))
   (ModR/M source target))

  ((B (R (? target)) (? source r/m-ea))
   (PREFIX (ModR/M target source))
   (BITS (8 #x8a))
   (ModR/M target source))

  (((? size operand-size) (R (? target)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #x8b))
   (ModR/M target source))

  ((B (R 0) (@ (? moffset unsigned-byte)))
   (BITS (8 #xa0)
	 (8 moffset UNSIGNED)))

  ((W (R 0) (@ (? moffset unsigned-word)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xa1)
	 (16 moffset UNSIGNED)))

  ((L (R 0) (@ (? moffset unsigned-long)))
   (PREFIX (OPERAND 'L))
   (BITS (8 #xa1)
	 (32 moffset UNSIGNED)))

  ((Q (R 0) (@ (? moffset unsigned-quad)))
   (PREFIX (OPERAND 'Q))
   (BITS (8 #xa1)
	 (64 moffset UNSIGNED)))

  ((B (@ (? moffset unsigned-byte)) (R 0))
   (BITS (8 #xa2)
	 (8 moffset UNSIGNED)))

  ((W (@ (? moffset unsigned-word)) (R 0))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xa3)
	 (16 moffset UNSIGNED)))

  ((L (@ (? moffset unsigned-long)) (R 0))
   (PREFIX (OPERAND 'L))
   (BITS (8 #xa3)
	 (32 moffset UNSIGNED)))

  ((Q (@ (? moffset unsigned-quad)) (R 0))
   (PREFIX (OPERAND 'Q))
   (BITS (8 #xa3)
	 (64 moffset UNSIGNED)))

  ((B (R (? target)) (& (? value signed-byte)))
   (PREFIX (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb0 target))
	 (8 value SIGNED)))

  ((B (R (? target)) (&U (? value unsigned-byte)))
   (PREFIX (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb0 target))
	 (8 value UNSIGNED)))

  ((W (R (? target)) (& (? value signed-word)))
   (PREFIX (OPERAND 'W) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (16 value SIGNED)))

  ((W (R (? target)) (&U (? value unsigned-word)))
   (PREFIX (OPERAND 'W) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (16 value UNSIGNED)))

  ((L (R (? target)) (& (? value signed-long)))
   (PREFIX (OPERAND 'L) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (32 value SIGNED)))

  ((L (R (? target)) (&U (? value unsigned-long)))
   (PREFIX (OPERAND 'L) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (32 value UNSIGNED)))

  ((Q (R (? target)) (& (? value signed-quad)))
   (PREFIX (OPERAND 'Q) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (64 value SIGNED)))

  ((Q (R (? target)) (&U (? value unsigned-quad)))
   (PREFIX (OPERAND 'Q) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #xb8 target))
	 (64 value UNSIGNED)))

  ((B (? target r/m-ea) (& (? value signed-byte)))
   (PREFIX (ModR/M target))
   (BITS (8 #xc6))
   (ModR/M 0 target)
   (BITS (8 value SIGNED)))

  ((B (? target r/m-ea) (&U (? value unsigned-byte)))
   (PREFIX (ModR/M target))
   (BITS (8 #xc6))
   (ModR/M 0 target)
   (BITS (8 value UNSIGNED)))

  ((W (? target r/m-ea) (& (? value signed-word)))
   (PREFIX (OPERAND 'W) (ModR/M target))
   (BITS (8 #xc7))
   (ModR/M 0 target)
   (BITS (16 value SIGNED)))

  ((W (? target r/m-ea) (&U (? value unsigned-word)))
   (PREFIX (OPERAND 'W) (ModR/M target))
   (BITS (8 #xc7))
   (ModR/M 0 target)
   (BITS (16 value UNSIGNED)))

  (((? size operand-size) (? target r/m-ea) (& (? value signed-long)))
   (PREFIX (OPERAND size) (ModR/M target))
   (BITS (8 #xc7))
   (ModR/M 0 target)
   (BITS (32 value SIGNED)))

  (((? size operand-size) (? target r/m-ea) (&U (? value unsigned-long)))
   (PREFIX (OPERAND size) (ModR/M target))
   (BITS (8 #xc7))
   (ModR/M 0 target)
   (BITS (32 value UNSIGNED))))

(define-trivial-instruction NOP #x90)

(define-instruction OUT
  ((B (&U (? port unsigned-byte)) (R 0))
   (BITS (8 #xe6)
	 (8 port UNSIGNED)))

  ((B (R 2) (R 0))
   (BITS (8 #xee)))

  (((? size operand-size) (&U (? port unsigned-byte)) (R 0))
   (PREFIX (OPERAND size))
   (BITS (8 #xe7)
	 (8 port UNSIGNED)))

  (((PREFIX (OPERAND size)) (R 2) (R 0))
   (BITS (8 #xef))))

(define-instruction POPCNT
  (((? size operand-size) (R (? target)) (? source r/m-ea))
   (PREFIX (OPERAND size) (ModR/M target source))
   (BITS (8 #xf3)
	 (8 #x0f)
	 (8 #xb8))
   (ModR/M target source)))

(define-instruction POPF
  ;; No 8-bit or 32-bit operand sizes available.
  ((W)
   (PREFIX (OPERAND 'W))
   (BITS (8 #x9d)))
  ((Q)
   (BITS (8 #x9d))))

(define-instruction PUSHF
  ;; No 8-bit or 32-bit operand size available.
  ((W)
   (PREFIX (OPERAND 'W))
   (BITS (8 #x9c)))
  ((Q)
   (BITS (8 #x9c))))

(define-instruction POP
  ;; No 8-bit or 32-bit register operand size available.

  ((W (R (? target)))
   (PREFIX (OPERAND 'W) (OPCODE-REGISTER target))
   (BITS (8 (opcode-register #x58 target))))

  ((Q (R (? target)))
   (PREFIX (OPCODE-REGISTER target))	;No operand prefix.
   (BITS (8 (opcode-register #x58 target))))

  ;; No 8-bit or 32-bit memory operand size available.

  ((W (? target m-ea))
   (PREFIX (OPERAND 'W) (ModR/M target))
   (BITS (8 #x8f))
   (ModR/M 0 target))

  ((Q (? target m-ea))
   (PREFIX (ModR/M target))		;No operand prefix.
   (BITS (8 #x8f))
   (ModR/M 0 target))

  ((FS)
   (BITS (8 #x0f)
	 (8 #xa1)))

  ((GS)
   (BITS (8 #x0f)
	 (8 #xa9)))

  (((SR 4))
   (BITS (8 #x0f)
	 (8 #xa1)))

  (((SR 5))
   (BITS (8 #x0f)
	 (8 #xa9))))

;;; The PUSH instruction is capable of pushing 16-bit or 64-bit
;;; operands onto the stack.  For 16-bit pushes, an immediate operand
;;; can have eight or sixteen bits, and if it has eight bits then it
;;; is sign-extended to sixteen; for 64-bit pushes, an immediate
;;; operand can have eight or thirty-two bits, and in both cases it is
;;; sign-extended to sixty-four.  Unfortunately, this means that to
;;; push 64-bit constants we need to use a temporary register, which
;;; is pretty silly.

(define-instruction PUSH
  ((W (R (? source)))
   (PREFIX (OPERAND 'W) (OPCODE-REGISTER source))
   (BITS (8 (opcode-register #x50 source))))

  ((Q (R (? source)))
   (PREFIX (OPCODE-REGISTER source))	;No operand prefix.
   (BITS (8 (opcode-register #x50 source))))

  ((W (& (? value sign-extended-byte)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #x6a)
	 (8 value SIGNED)))

  ((W (&U (? value zero-extended-byte)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #x6a)
	 (8 value SIGNED)))

  ((W (& (? value signed-word)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #x68)
	 (16 value SIGNED)))

  ((W (&U (? value unsigned-word)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #x68)
	 (16 value UNSIGNED)))

  ((Q (& (? value sign-extended-byte)))	;No operand prefix.
   (BITS (8 #x6a)
	 (8 value SIGNED)))

  ((Q (&U (? value zero-extended-byte))) ;No operand prefix.
   (BITS (8 #x6a)
	 (8 value SIGNED)))

  ((Q (& (? value sign-extended-long)))	;No operand prefix.
   (BITS (8 #x68)
	 (32 value SIGNED)))

  ((Q (&U (? value zero-extended-long))) ;No operand prefix.
   (BITS (8 #x68)
	 (32 value SIGNED)))

  ((W (? source m-ea))
   (PREFIX (OPERAND 'W) (ModR/M source))
   (BITS (8 #xff))
   (ModR/M 6 source))

  ((Q (? source m-ea))
   (PREFIX (ModR/M source))		;No operand prefix.
   (BITS (8 #xff))
   (ModR/M 6 source))

  ((FS)
   (BITS (8 #x0f)
	 (8 #xa0)))

  ((GS)
   (BITS (8 #x0f)
	 (8 #xa8)))

  (((SR 4))
   (BITS (8 #x0f)
	 (8 #xa0)))

  (((SR 5))
   (BITS (8 #x0f)
	 (8 #xa8))))

(let-syntax
    ((define-rotate/shift
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (digit (caddr form)))
	   `(define-instruction ,mnemonic
	      ((B (? operand r/m-ea) (&U 1))
	       (PREFIX (ModR/M operand))
	       (BITS (8 #xd0))
	       (ModR/M ,digit operand))

	      ((B (? operand r/m-ea) (&U (? value unsigned-byte)))
	       (PREFIX (ModR/M operand))
	       (BITS (8 #xc0))
	       (ModR/M ,digit operand)
	       (BITS (8 value UNSIGNED)))

	      ((B (? operand r/m-ea) (R 1))
	       (PREFIX (ModR/M operand))
	       (BITS (8 #xd2))
	       (ModR/M ,digit operand))

	      (((? size operand-size) (? operand r/m-ea) (&U 1))
	       (PREFIX (OPERAND size) (ModR/M operand))
	       (BITS (8 #xd1))
	       (ModR/M ,digit operand))

	      (((? size operand-size) (? operand r/m-ea)
				      (&U (? value unsigned-byte)))
	       (PREFIX (OPERAND size) (ModR/M operand))
	       (BITS (8 #xc1))
	       (ModR/M ,digit operand)
	       (BITS (8 value UNSIGNED)))

	      (((? size operand-size) (? operand r/m-ea) (R 1))
	       (PREFIX (OPERAND size) (ModR/M operand))
	       (BITS (8 #xd3))
	       (ModR/M ,digit operand))))))))

  (define-rotate/shift RCL 2)
  (define-rotate/shift RCR 3)
  (define-rotate/shift ROL 0)
  (define-rotate/shift ROR 1)
  (define-rotate/shift SAL 4)
  (define-rotate/shift SAR 7)
  (define-rotate/shift SHL 4)
  (define-rotate/shift SHR 5))

(let-syntax
    ((define-double-shift
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form)))
	   `(define-instruction ,mnemonic
	      (((? size operand-size) (? target r/m-ea)
				      (R (? source))
				      (&U (? count unsigned-byte)))
	       (PREFIX (OPERAND size) (ModR/M target source))
	       (BITS (8 #x0f)
		     (8 ,opcode))
	       (ModR/M target source)
	       (BITS (8 count UNSIGNED)))

	      (((? size operand-size) (? target r/m-ea)
				      (R (? source))
				      (R 1))
	       (PREFIX (OPERAND size) (ModR/M source target))
	       (BITS (8 #x0f)
		     (8 ,(+ opcode 1)))
	       (ModR/M source target))))))))

  (define-double-shift SHLD #xa4)
  (define-double-shift SHRD #xac))

(define-instruction RET
  (()
   (BITS (8 #xc3)))

  ((F)
   (BITS (8 #xcb)))

  (((&U (? frame-size unsigned-word)))
   (BITS (8 #xc2)
	 (16 frame-size UNSIGNED)))

  ((F (&U (? frame-size unsigned-word)))
   (BITS (8 #xca)
	 (16 frame-size UNSIGNED))))

(define-trivial-instruction SAHF #x9e)

(let-syntax
    ((define-setcc-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (let ((mnemonic (cadr form))
	       (opcode (caddr form)))
	   `(define-instruction ,mnemonic
	      (((? target r/m-ea))
	       (PREFIX (ModR/M target))
	       (BITS (8 #x0f)
		     (8 ,opcode))
	       (ModR/M 0 target))))))))

  (define-setcc-instruction SETA   #x97)
  (define-setcc-instruction SETAE  #x93)
  (define-setcc-instruction SETB   #x92)
  (define-setcc-instruction SETBE  #x96)
  (define-setcc-instruction SETC   #x92)
  (define-setcc-instruction SETE   #x94)
  (define-setcc-instruction SETG   #x9f)
  (define-setcc-instruction SETGE  #x9d)
  (define-setcc-instruction SETL   #x9c)
  (define-setcc-instruction SETLE  #x9e)
  (define-setcc-instruction SETNA  #x96)
  (define-setcc-instruction SETNAE #x92)
  (define-setcc-instruction SETNB  #x93)
  (define-setcc-instruction SETNBE #x97)
  (define-setcc-instruction SETNC  #x93)
  (define-setcc-instruction SETNE  #x95)
  (define-setcc-instruction SETNG  #x9e)
  (define-setcc-instruction SETNGE #x9c)
  (define-setcc-instruction SETNL  #x9d)
  (define-setcc-instruction SETNLE #x9f)
  (define-setcc-instruction SETNO  #x91)
  (define-setcc-instruction SETNP  #x9b)
  (define-setcc-instruction SETNS  #x99)
  (define-setcc-instruction SETNZ  #x95)
  (define-setcc-instruction SETO   #x90)
  (define-setcc-instruction SETP   #x9a)
  (define-setcc-instruction SETPE  #x9a)
  (define-setcc-instruction SETPO  #x9b)
  (define-setcc-instruction SETS   #x98)
  (define-setcc-instruction SETZ   #x94))

(define-trivial-instruction STC #xf9)
(define-trivial-instruction STD #xfd)
(define-trivial-instruction STI #xfb)

(define-instruction TEST
  ((B (R 0) (& (? value signed-byte)))
   (BITS (8 #xa8)
	 (8 value SIGNED)))

  ((W (R 0) (& (? value signed-word)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xa9)
	 (16 value SIGNED)))

  ((L (R 0) (& (? value signed-long)))
   (PREFIX (OPERAND 'L))
   (BITS (8 #xa9)
	 (32 value SIGNED)))

  ((Q (R 0) (& (? value sign-extended-long)))
   (PREFIX (OPERAND 'Q))
   (BITS (8 #xa9)
	 (32 value SIGNED)))

  ((B (R 0) (&U (? value unsigned-byte)))
   (BITS (8 #xa8)
	 (8 value UNSIGNED)))

  ((W (R 0) (&U (? value unsigned-word)))
   (PREFIX (OPERAND 'W))
   (BITS (8 #xa9)
	 (16 value UNSIGNED)))

  ((L (R 0) (&U (? value unsigned-long)))
   (PREFIX (OPERAND 'L))
   (BITS (8 #xa9)
	 (32 value UNSIGNED)))

  ((Q (R 0) (&U (? value zero-extended-long)))
   (PREFIX (OPERAND 'Q))
   (BITS (8 #xa9)
	 (32 value SIGNED)))

  ((B (? operand r/m-ea) (& (? value signed-byte)))
   (PREFIX (ModR/M operand))
   (BITS (8 #xf6))
   (ModR/M 0 operand)
   (BITS (8 value SIGNED)))

  ((W (? operand r/m-ea) (& (? value signed-word)))
   (PREFIX (OPERAND 'W) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (16 value SIGNED)))

  ((L (? operand r/m-ea) (& (? value signed-long)))
   (PREFIX (OPERAND 'L) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (32 value SIGNED)))

  ((Q (? operand r/m-ea) (& (? value sign-extended-long)))
   (PREFIX (OPERAND 'Q) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (32 value SIGNED)))

  ((B (? operand r/m-ea) (&U (? value unsigned-byte)))
   (PREFIX (ModR/M operand))
   (BITS (8 #xf6))
   (ModR/M 0 operand)
   (BITS (8 value UNSIGNED)))

  ((W (? operand r/m-ea) (&U (? value unsigned-word)))
   (PREFIX (OPERAND 'W) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (16 value UNSIGNED)))

  ((L (? operand r/m-ea) (&U (? value unsigned-long)))
   (PREFIX (OPERAND 'L) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (32 value UNSIGNED)))

  ((Q (? operand r/m-ea) (&U (? value zero-extended-long)))
   (PREFIX (OPERAND 'Q) (ModR/M operand))
   (BITS (8 #xf7))
   (ModR/M 0 operand)
   (BITS (32 value SIGNED)))

  ((B (? r/m r/m-ea) (R (? reg)))
   (PREFIX (ModR/M reg r/m))
   (BITS (8 #x84))
   (ModR/M reg r/m))

  (((? size operand-size) (? r/m r/m-ea) (R (? reg)))
   (PREFIX (OPERAND size) (ModR/M reg r/m))
   (BITS (8 #x85))
   (ModR/M reg r/m)))

(define-trivial-instruction WAIT #x9b)		; = (FWAIT)
(define-trivial-instruction WBINVD #x0f #x09)	; 486 only

(define-instruction XADD			; 486 only
  ((B (? target r/m-ea) (R (? source)))
   (PREFIX (ModR/M source target))
   (BITS (8 #x0f)
	 (8 #xc0))
   (ModR/M source target))

  (((? size operand-size) (? target r/m-ea) (R (? source)))
   (PREFIX (OPERAND size) (ModR/M source target))
   (BITS (8 #x0f)
	 (8 #xc1))
   (ModR/M source target)))

(define-instruction XCHG
  ;; Register->register exchanges are symmetrical, but can be done
  ;; only if one if the registers is AX/EAX/RAX.

  (((? size operand-size) (R 0) (R (? reg)))
   (PREFIX (OPERAND size) (OPCODE-REGISTER reg))
   (BITS (8 (opcode-register #x90 reg))))

  (((? size operand-size) (R (? reg)) (R 0))
   (PREFIX (OPERAND size) (OPCODE-REGISTER reg))
   (BITS (8 (opcode-register #x90 reg))))

  ((B (? r/m r/m-ea) (R (? reg)))
   (PREFIX (ModR/M reg r/m))
   (BITS (8 #x86))
   (ModR/M reg r/m))

  ((B (R (? reg)) (? r/m r/m-ea))
   (PREFIX (ModR/M reg r/m))
   (BITS (8 #x86))
   (ModR/M reg r/m))

  (((? size operand-size) (? r/m r/m-ea) (R (? reg)))
   (PREFIX (OPERAND size) (ModR/M reg r/m))
   (BITS (8 #x87))
   (ModR/M reg r/m))

  (((? size operand-size) (R (? reg)) (? r/m r/m-ea))
   (PREFIX (OPERAND size) (ModR/M reg r/m))
   (BITS (8 #x87))
   (ModR/M reg r/m)))

(define-trivial-instruction XLAT #xd7)

;;;; Instruction prefixes.  Treated as separate instructions.

(define-trivial-instruction LOCK #xf0)

(define-trivial-instruction REP   #xf3)		; or #xf2 trust which appendix?
(define-trivial-instruction REPE  #xf3)
(define-trivial-instruction REPNE #xf2)
(define-trivial-instruction REPNZ #xf2)
(define-trivial-instruction REPZ  #xf3)

(define-trivial-instruction CSSEG #x2e)
(define-trivial-instruction SSSEG #x36)
(define-trivial-instruction DSSEG #x3e)
(define-trivial-instruction ESSEG #x26)
(define-trivial-instruction FSSEG #x64)
(define-trivial-instruction GSSEG #x65)

;; **** Missing MOV instruction to/from special registers. ****