#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/instr1.scm,v 1.2 1991/06/17 21:21:28 cph Exp $

Copyright (c) 1987-91 Massachusetts Institute of Technology

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

;;;; MIPS instruction set

;; Branch-tensioned instructions are in instr2.scm
;; Floating point instructions are in instr3.scm

(declare (usual-integrations))

(define-integrable (extract bit-string start end)
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

(define-integrable (extract-signed bit-string start end)
  (bit-string->signed-integer (bit-substring bit-string start end)))

(let-syntax
    ((immediate-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? dest-reg-ii) (? source-reg-ii) (? immediate))
	    (LONG (6 ,opcode)
		  (5 source-reg-ii)
		  (5 dest-reg-ii)
		  (16 immediate SIGNED))))))
     (unsigned-immediate-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? dest-reg-uii) (? source-reg-uii) (? uimmediate))
	    (LONG (6 ,opcode)
		  (5 source-reg-uii)
		  (5 dest-reg-uii)
		  (16 uimmediate))))))

     (special-instruction
      (macro (keyword special-op)
	`(define-instruction ,keyword
	   (((? dest-sp) (? reg-1-sp) (? reg-2-sp))
	    (LONG (6 0)
		  (5 reg-1-sp)
		  (5 reg-2-sp)
		  (5 dest-sp)
		  (5 0)
		  (6 ,special-op))))))
     (move-coprocessor-instruction
      (macro (keyword opcode move-op)
	`(define-instruction ,keyword
	   (((? rt-mci) (? rd-mci))
	    (LONG (6 ,opcode)
		  (5 ,move-op)
		  (5 rt-mci)
		  (5 rd-mci)
		  (11 0))))))
     (coprocessor-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? cofun))
	    (LONG (6 ,opcode)
		  (1 1)			; CO bit
		  (25 cofun))))))
     (div/mul-instruction
      (macro (keyword funct)
	`(define-instruction ,keyword
	   (((? rs-dm) (? rt-dm))
	    (LONG (6 0)
		  (5 rs-dm)
		  (5 rt-dm)
		  (10 0)
		  (6 ,funct))))))
     (jump-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? dest-j))
	    (LONG (6 ,opcode)
		  (26 dest-j))))))

     (from-hi/lo-instruction
      (macro (keyword funct)
	`(define-instruction ,keyword
	   (((? rd-fhl))
	    (LONG (6 0)
		  (10 0)
		  (5 rd-fhl)
		  (5 0)
		  (6 ,funct))))))
     (to-hi/lo-instruction
      (macro (keyword funct)
	`(define-instruction ,keyword
	   (((? rd-thl))
	    (LONG (6 0)
		  (5 rd-thl)
		  (15 0)
		  (6 ,funct))))))
     (cop0-instruction
      (macro (keyword cp0-op)
	`(define-instruction ,keyword
	   (()
	    (LONG (6 16)
		  (1 1)			; CO
		  (20 0)
		  (5 ,cp0-op))))))
     (shift-instruction
      (macro (keyword funct)
	`(define-instruction ,keyword
	   (((? dest-sh) (? source-sh) (? amount))
	    (LONG (6 0)
		  (5 0)
		  (5 source-sh)
		  (5 dest-sh)
		  (5 amount)
		  (6 ,funct))))))
     (shift-variable-instruction
      (macro (keyword funct)
	`(define-instruction ,keyword
	   (((? dest-sv) (? source-sv) (? amount-reg))
	    (LONG (6 0)
		  (5 amount-reg)
		  (5 source-sv)
		  (5 dest-sv)
		  (5 0)
		  (6 ,funct)))))))
  (special-instruction add 32)
  (immediate-instruction addi 8)
  (immediate-instruction addiu 9)
  (special-instruction addu 33)
  (special-instruction and 36)
  (unsigned-immediate-instruction andi 12)
  (define-instruction break
    (((? code))
     (LONG (6 0) (20 code) (6 13))))
  (move-coprocessor-instruction cfc0 16 #x002)
  (move-coprocessor-instruction cfc1 17 #x002)
  (move-coprocessor-instruction cfc2 18 #x002)
  (move-coprocessor-instruction cfc3 19 #x002)
  (coprocessor-instruction cop0 16)
  (coprocessor-instruction cop1 17)
  (coprocessor-instruction cop2 18)
  (coprocessor-instruction cop3 19)
  (move-coprocessor-instruction ctc0 16 #x006)
  (move-coprocessor-instruction ctc1 17 #x006)
  (move-coprocessor-instruction ctc2 18 #x006)
  (move-coprocessor-instruction ctc3 19 #x006)
  (div/mul-instruction div 26)
  (div/mul-instruction divu 27)
  (jump-instruction j 2)
  (jump-instruction jal 3)
  (define-instruction jalr
    (((? rd-jalr) (? rs-jalr))
     (LONG (6 0) (5 rs-jalr) (5 0) (5 rd-jalr) (5 0) (6 9))))
  (define-instruction jr
    (((? rs-jr))
     (LONG (6 0) (5 rs-jr) (15 0) (6 8))))
  (define-instruction lui
    (((? dest-lui) (? immediate-lui))
     (LONG (6 15) (5 0) (5 dest-lui) (16 immediate-lui))))
  (move-coprocessor-instruction mfc0 16 #x000)
  (move-coprocessor-instruction mfc1 17 #x000)
  (move-coprocessor-instruction mfc2 18 #x000)
  (move-coprocessor-instruction mfc3 19 #x000)
  (from-hi/lo-instruction mfhi 16)
  (from-hi/lo-instruction mflo 18)
  (move-coprocessor-instruction mtc0 16 #x004)
  (move-coprocessor-instruction mtc1 17 #x004)
  (move-coprocessor-instruction mtc2 18 #x004)
  (move-coprocessor-instruction mtc3 19 #x004)
  (to-hi/lo-instruction mthi 17)
  (to-hi/lo-instruction mtlo 19)
  (div/mul-instruction mult 24)
  (div/mul-instruction multu 25)
  (special-instruction nor 39)
  (special-instruction or 37)
  (unsigned-immediate-instruction ori 13)
  (cop0-instruction rfe 16)
  (shift-instruction sll 0)
  (shift-variable-instruction sllv 4)
  (special-instruction slt 42)
  (immediate-instruction slti 10)
  (immediate-instruction sltiu 11)
  (special-instruction sltu 43)
  (shift-instruction sra 3)
  (shift-variable-instruction srav 7)
  (shift-instruction srl 2)
  (shift-variable-instruction srlv 6)
  (special-instruction sub 34)
  (special-instruction subu 35)
  (define-instruction syscall
    (()
     (LONG (6 0) (20 0) (6 12))))
  (cop0-instruction tlbp 8)
  (cop0-instruction tlbr 1)
  (cop0-instruction tlbwi 2)
  (cop0-instruction tlbwr 6)
  (special-instruction xor 38)
  (unsigned-immediate-instruction xori 14))

;;;; Assembler pseudo-ops

(define-instruction WORD
  (((? expression))
   (LONG (32 expression SIGNED))))

(define-instruction UWORD
  (((? expression))
   (LONG (32 expression UNSIGNED))))

; External labels cause the output of GC header and format words
(define-instruction EXTERNAL-LABEL
  (((? format-word) (@PCR (? label)))
   (if (eq? endianness 'LITTLE)
       (LONG (16 label BLOCK-OFFSET)
	     (16 format-word UNSIGNED))
       (LONG (16 format-word UNSIGNED)
	     (16 label BLOCK-OFFSET))))

  (((? format-word) (@PCO (? offset)))
   (if (eq? endianness 'LITTLE)
       (LONG (16 offset UNSIGNED)
	     (16 format-word UNSIGNED))
       (LONG (16 format-word UNSIGNED)
	     (16 offset UNSIGNED)))))

(define-instruction PC-RELATIVE-OFFSET
  (((? target) (@PCR (? label)))
   (VARIABLE-WIDTH (offset `(- ,label (+ *PC* 8)))
     ((#x-8000 #x7FFF)
      ;     BGEZAL 0 X                *PC* is here
      ;     ADDI target, 31, offset
      ; X:  ...
      (LONG (6 1)			; BGEZAL
	    (5 0)
	    (5 17)
	    (16 1)
	    (6 8)			; ADDI
	    (5 31)
	    (5 target)
	    (16 offset SIGNED)))
     ((() ())
      ;     BGEZAL 0 X                *PC* is here
      ;     ADDIU target, 31, (right of offset)
      ; X:  LUI   1, (left_adjust of offset)
      ;     ADD   target, target, 1
      (LONG (6 1)			; BGEZAL
	    (5 0)
	    (5 17)
	    (16 1)
	    (6 9)			; ADDIU
	    (5 31)
	    (5 target)
	    (16 (adjusted:low offset) SIGNED)
	    (6 15)			; LUI
	    (5 0)
	    (5 1)
	    (16 (adjusted:high offset))
	    (6 0)			; ADD
	    (5 1)
	    (5 target)
	    (5 target)
	    (5 0)
	    (6 32)))))
  (((? target) (? offset) (? label))
   ; Load (into target) distance from here+offset to label
   (VARIABLE-WIDTH (offset `(- ,label (+ ,offset *PC*)))
     ((#x-8000 #x7FFF)
      ; ADDI target, 0, offset
      (LONG (6 8)			; ADDI
	    (5 0)
	    (5 target)
	    (16 offset SIGNED)))
     ((#x8000 #xFFFF)
      ; ORI target, 0, offset
      (LONG (6 13)			; ORI
	    (5 0)
	    (5 target)
	    (16 offset)))
     ((() ())
      ; LUI   target, (left_adjust of offset)
      ; ADDIU target, target, (right of offset)
      (LONG (6 15)			; LUI
	    (5 0)
	    (5 target)
	    (16 (adjusted:high (- offset 4)))
	    (6 9)			; ADDIU
	    (5 target)
	    (5 target)
	    (16 (adjusted:low (- offset 4)) SIGNED))))))

(define-instruction NOP
  (()					; ADDI 0, 0
   (LONG (6 8) (5 0) (5 0) (16 0))))