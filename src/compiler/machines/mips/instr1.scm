#| -*-Scheme-*-

$Id: instr1.scm,v 1.11 2003/02/14 18:28:03 cph Exp $

Copyright (c) 1987-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; MIPS instruction set

;; Branch-tensioned instructions are in instr2.scm
;; Floating point instructions are in instr3.scm

(declare (usual-integrations))

(let-syntax
    ((arithmetic-immediate-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source) (? immediate))
	     (VARIABLE-WIDTH (evaluated-immediate immediate)
	       ((#x-8000 #x7fff)
		(LONG (6 ,(caddr form))
		      (5 source)
		      (5 destination)
		      (16 evaluated-immediate SIGNED)))
	       ((#x8000 #xffff)
		;; ORI     1, 0, immediate
		;; reg-op  destination, source, 1
		(LONG (6 13)		; ORI
		      (5 0)
		      (5 1)
		      (16 evaluated-immediate)
		      (6 0)		; reg-op
		      (5 source)
		      (5 1)
		      (5 destination)
		      (5 0)
		      (6 ,(cadddr form))))
	       ((() ())
		;; LUI     1, (top of immediate)
		;; ORI     1, 1, (bottom of immediate)
		;; reg-op  destination, source, 1
		(LONG (6 15)		; LUI
		      (5 0)
		      (5 1)
		      (16 (top-16-bits evaluated-immediate))
		      (6 13)		; ORI
		      (5 1)
		      (5 1)
		      (16 (bottom-16-bits evaluated-immediate))
		      (6 0)		; reg-op
		      (5 source)
		      (5 1)
		      (5 destination)
		      (5 0)
		      (6 ,(cadddr form)))))))))))
  (arithmetic-immediate-instruction addi 8 32)
  (arithmetic-immediate-instruction addiu 9 33)
  (arithmetic-immediate-instruction slti 10 42)
  (arithmetic-immediate-instruction sltiu 11 43))

(let-syntax
    ((unsigned-immediate-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source) (? immediate))
	     (VARIABLE-WIDTH (evaluated-immediate immediate)
	       ((0 #xffff)
		(LONG (6 ,(caddr form))
		      (5 source)
		      (5 destination)
		      (16 evaluated-immediate)))
	       ((() ())
		;; LUI     1, (top of immediate)
		;; ORI     1, 1, (bottom of immediate)
		;; reg-op  destination, source, 1
		(LONG (6 15)		; LUI
		      (5 0)
		      (5 1)
		      (16 (top-16-bits evaluated-immediate))
		      (6 13)		; ORI
		      (5 1)
		      (5 1)
		      (16 (bottom-16-bits evaluated-immediate))
		      (6 0)		; reg-op
		      (5 source)
		      (5 1)
		      (5 destination)
		      (5 0)
		      (6 ,(cadddr form)))))))))))
  (unsigned-immediate-instruction andi 12 36)
  (unsigned-immediate-instruction ori 13 37)
  (unsigned-immediate-instruction xori 14 38))

(define-instruction lui
  (((? destination) (? immediate))
   (LONG (6 15)
	 (5 0)
	 (5 destination)
	 (16 immediate))))

(define-instruction li
  (((? destination) (? immediate))
   (VARIABLE-WIDTH (evaluated-immediate immediate)
     ((#x-8000 #x7fff)
      ;; ADDI destination, 0, immediate
      (LONG (6 8)
	    (5 0)
	    (5 destination)
	    (16 evaluated-immediate SIGNED)))
     ((#x8000 #xffff)
      ;; ORI destination, 0, immediate
      (LONG (6 13)
	    (5 0)
	    (5 destination)
	    (16 evaluated-immediate)))
     ((() ())
      ;; LUI  destination, (top of immediate)
      ;; ORI  destination, destination, (bottom of immediate)
      (LONG (6 15)			; LUI
	    (5 0)
	    (5 destination)
	    (16 (top-16-bits evaluated-immediate))
	    (6 13)			; ORI
	    (5 destination)
	    (5 destination)
	    (16 (bottom-16-bits evaluated-immediate)))))))

(let-syntax
    ((3-operand-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source-1) (? source-2))
	     (LONG (6 0)
		   (5 source-1)
		   (5 source-2)
		   (5 destination)
		   (5 0)
		   (6 ,(caddr form)))))))))
  (3-operand-instruction add 32)
  (3-operand-instruction addu 33)
  (3-operand-instruction sub 34)
  (3-operand-instruction subu 35)
  (3-operand-instruction and 36)
  (3-operand-instruction or 37)
  (3-operand-instruction xor 38)
  (3-operand-instruction nor 39)
  (3-operand-instruction slt 42)
  (3-operand-instruction sltu 43))

(let-syntax
    ((shift-instruction
      (lambda (form environment)
	environment
	`(DEFINE-INSTRUCTION ,(cadr form)
	   (((? destination) (? source) (? amount))
	    (LONG (6 0)
		  (5 0)
		  (5 source)
		  (5 destination)
		  (5 amount)
		  (6 ,(caddr form))))))))
  (shift-instruction sll 0)
  (shift-instruction srl 2)
  (shift-instruction sra 3))

(let-syntax
    ((shift-variable-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source) (? amount))
	     (LONG (6 0)
		   (5 amount)
		   (5 source)
		   (5 destination)
		   (5 0)
		   (6 ,(caddr form)))))))))
  (shift-variable-instruction sllv 4)
  (shift-variable-instruction srlv 6)
  (shift-variable-instruction srav 7))

(let-syntax
    ((div/mul-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? source-1) (? source-2))
	     (LONG (6 0)
		   (5 source-1)
		   (5 source-2)
		   (5 0)
		   (5 0)
		   (6 ,(caddr form)))))))))
  (div/mul-instruction div 26)
  (div/mul-instruction divu 27)
  (div/mul-instruction mult 24)
  (div/mul-instruction multu 25))

(let-syntax
    ((from-hi/lo-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination))
	     (LONG (6 0)
		   (5 0)
		   (5 0)
		   (5 destination)
		   (5 0)
		   (6 ,(caddr form)))))))))
  (from-hi/lo-instruction mfhi 16)
  (from-hi/lo-instruction mflo 18))
#|
(let-syntax
    ((to-hi/lo-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? source))
	     (LONG (6 0)
		   (5 source)
		   (5 0)
		   (5 0)
		   (5 0)
		   (6 ,(caddr form)))))))))
  (to-hi/lo-instruction mthi 17)
  (to-hi/lo-instruction mtlo 19))

(let-syntax
    ((jump-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? address))
	     (LONG (6 ,(caddr form))
		   (26 (QUOTIENT address 2)))))))))
  (jump-instruction j 2)
  (jump-instruction jal 3))
|#
(define-instruction jalr
  (((? destination) (? source))
   (LONG (6 0)
	 (5 source)
	 (5 0)
	 (5 destination)
	 (5 0)
	 (6 9))))

(define-instruction jr
  (((? source))
   (LONG (6 0)
	 (5 source)
	 (5 0)
	 (5 0)
	 (5 0)
	 (6 8))))

(let-syntax
    ((move-coprocessor-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? rt-mci) (? rd-mci))
	     (LONG (6 ,(caddr form))
		   (5 ,(cadddr form))
		   (5 rt-mci)
		   (5 rd-mci)
		   (11 0))))))))
  ;; (move-coprocessor-instruction mfc0 16 #x000)
  (move-coprocessor-instruction mfc1 17 #x000)
  ;; (move-coprocessor-instruction mfc2 18 #x000)
  ;; (move-coprocessor-instruction mfc3 19 #x000)
  ;; (move-coprocessor-instruction cfc0 16 #x002)
  (move-coprocessor-instruction cfc1 17 #x002)
  ;; (move-coprocessor-instruction cfc2 18 #x002)
  ;; (move-coprocessor-instruction cfc3 19 #x002)
  ;; (move-coprocessor-instruction mtc0 16 #x004)
  (move-coprocessor-instruction mtc1 17 #x004)
  ;; (move-coprocessor-instruction mtc2 18 #x004)
  ;; (move-coprocessor-instruction mtc3 19 #x004)
  ;; (move-coprocessor-instruction ctc0 16 #x006)
  (move-coprocessor-instruction ctc1 17 #x006)
  ;; (move-coprocessor-instruction ctc2 18 #x006)
  ;; (move-coprocessor-instruction ctc3 19 #x006)
  )
#|
(let-syntax
    ((coprocessor-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? cofun))
	     (LONG (6 ,(caddr form))
		   (1 1)		; CO bit
		   (25 cofun))))))))
  (coprocessor-instruction cop0 16)
  (coprocessor-instruction cop1 17)
  (coprocessor-instruction cop2 18)
  (coprocessor-instruction cop3 19))

(let-syntax
    ((cop0-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (()
	     (LONG (6 16)
		   (1 1)		; CO
		   (20 0)
		   (5 ,(caddr form)))))))))
  (cop0-instruction rfe 16)
  (cop0-instruction tlbp 8)
  (cop0-instruction tlbr 1)
  (cop0-instruction tlbwi 2)
  (cop0-instruction tlbwr 6))

(define-instruction syscall
  (()
   (LONG (6 0) (20 0) (6 12))))

(define-instruction break
  (((? code))
   (LONG (6 0) (20 code) (6 13))))
|#

;;;; Assembler pseudo-ops

(define-instruction EXTERNAL-LABEL
  ;; External labels provide the garbage collector with header
  ;; information and the runtime system with type, arity, and
  ;; debugging information.
  (((? format-word) (@PCR (? label)))
   (if (eq? endianness 'LITTLE)
       (LONG (16 label BLOCK-OFFSET)
	     (16 format-word UNSIGNED))
       (LONG (16 format-word UNSIGNED)
	     (16 label BLOCK-OFFSET)))))

(define-instruction NOP
  ;; (SLL 0 0 0)
  (()
   (LONG (6 0) (5 0) (5 0) (5 0) (5 0) (6 0))))

(define-instruction LONG
  ((S (? value))
   (LONG (32 value SIGNED)))
  ((U (? value))
   (LONG (32 value UNSIGNED))))