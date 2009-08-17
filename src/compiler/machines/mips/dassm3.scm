#| -*-Scheme-*-

$Id: c51df689d65186579a355c629efb4590b9d8ec59 $

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

;;; MIPS Disassembler: Internals

(declare (usual-integrations))

;;;; Utilities

(define (get-longword)
  (let ((word (read-bits *current-offset 32)))
    (set! *current-offset (+ *current-offset 4))
    word))

(declare (integrate-operator extract))
(declare (integrate-operator extract-signed))

(define (extract bit-string start end)
  (declare (integrate bit-string start end))
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

(define (extract-signed bit-string start end)
  (declare (integrate bit-string start end))
  (bit-string->signed-integer (bit-substring bit-string start end)))

;; Debugging assistance

(define (verify-instruction instruction)
  (let ((bits (car (lap:syntax-instruction instruction))))
    (if (bit-string? bits)
	(begin
	  (let ((disassembly (disassemble bits)))
	    (if (and (null? (cdr disassembly))
		     (equal? (car disassembly) instruction))
		   #T
		   disassembly)))
	(error "Assember oddity" bits))))

(define (v i) (verify-instruction i))

;;;; The disassembler proper

(define (handle-bad-instruction word)
  word
  (invalid-instruction))

(define (disassemble bit-string)
  (let ((stop (bit-string-length bit-string)))
    (let loop ((from 0)
	       (to 32)
	       (result '()))
      (if (> to stop)
	  result
	  (loop to (+ to 32) (cons (disassemble-word (bit-substring bit-string from to))
				   result))))))

(define disassemblers (make-vector (expt 2 6) handle-bad-instruction))

(define (disassemble-word word)
  (let ((op-code (extract word 26 32)))
    ((vector-ref disassemblers op-code) word)))

(vector-set! disassemblers special-op
  (lambda (word) (disassemble-special word)))
(vector-set! disassemblers bcond-op
  (lambda (word) (disassemble-branch-zero word)))
(vector-set! disassemblers j-op
  (lambda (word) (disassemble-jump word 'j)))
(vector-set! disassemblers jal-op
  (lambda (word) (disassemble-jump word 'jal)))
(vector-set! disassemblers beq-op
  (lambda (word) (disassemble-compare word 'beq)))
(vector-set! disassemblers bne-op
  (lambda (word) (disassemble-compare word 'bne)))
(vector-set! disassemblers blez-op
  (lambda (word) (disassemble-branch-zero-op word 'blez)))
(vector-set! disassemblers bgtz-op
  (lambda (word) (disassemble-branch-zero-op word 'bgtz)))
(vector-set! disassemblers addi-op
  (lambda (word) (disassemble-immediate word 'addi)))
(vector-set! disassemblers addiu-op
  (lambda (word) (disassemble-immediate word 'addiu)))
(vector-set! disassemblers slti-op
  (lambda (word) (disassemble-immediate word 'slti)))
(vector-set! disassemblers sltiu-op
  (lambda (word) (disassemble-immediate word 'sltiu)))
(vector-set! disassemblers andi-op
  (lambda (word) (disassemble-unsigned-immediate word 'andi)))
(vector-set! disassemblers ori-op
  (lambda (word) (disassemble-unsigned-immediate word 'ori)))
(vector-set! disassemblers xori-op
  (lambda (word) (disassemble-unsigned-immediate word 'xori)))
(vector-set! disassemblers lui-op
  (lambda (word) (disassemble-lui word)))
(vector-set! disassemblers cop0-op
  (lambda (word) (disassemble-coprocessor word 0)))
(vector-set! disassemblers cop1-op
  (lambda (word) (disassemble-coprocessor word 1)))
(vector-set! disassemblers cop2-op
  (lambda (word) (disassemble-coprocessor word 2)))
(vector-set! disassemblers cop3-op
  (lambda (word) (disassemble-coprocessor word 3)))
(vector-set! disassemblers lb-op
  (lambda (word) (disassemble-load/store word 'lb)))
(vector-set! disassemblers lh-op
  (lambda (word) (disassemble-load/store word 'lh)))
(vector-set! disassemblers lwl-op
  (lambda (word) (disassemble-load/store word 'lwl)))
(vector-set! disassemblers lw-op
  (lambda (word) (disassemble-load/store word 'lw)))
(vector-set! disassemblers lbu-op
  (lambda (word) (disassemble-load/store word 'lbu)))
(vector-set! disassemblers lhu-op
  (lambda (word) (disassemble-load/store word 'lhu)))
(vector-set! disassemblers lwr-op
  (lambda (word) (disassemble-load/store word 'lwr)))
(vector-set! disassemblers sb-op
  (lambda (word) (disassemble-load/store word 'sb)))
(vector-set! disassemblers sh-op
  (lambda (word) (disassemble-load/store word 'sh)))
(vector-set! disassemblers swl-op
  (lambda (word) (disassemble-load/store word 'swl)))
(vector-set! disassemblers sw-op
  (lambda (word) (disassemble-load/store word 'sw)))
(vector-set! disassemblers swr-op
  (lambda (word) (disassemble-load/store word 'swr)))
(vector-set! disassemblers lwc0-op
  (lambda (word) (disassemble-load/store word 'lwc0)))
(vector-set! disassemblers lwc1-op
  (lambda (word) (disassemble-load/store word 'lwc1)))
(vector-set! disassemblers lwc2-op
  (lambda (word) (disassemble-load/store word 'lwc2)))
(vector-set! disassemblers lwc3-op
  (lambda (word) (disassemble-load/store word 'lwc3)))
(vector-set! disassemblers swc0-op
  (lambda (word) (disassemble-load/store word 'swc0)))
(vector-set! disassemblers swc1-op
  (lambda (word) (disassemble-load/store word 'swc1)))
(vector-set! disassemblers swc2-op
  (lambda (word) (disassemble-load/store word 'swc2)))
(vector-set! disassemblers swc3-op
  (lambda (word) (disassemble-load/store word 'swc3)))

(define special-disassemblers (make-vector (expt 2 6) handle-bad-instruction))

(define (disassemble-special word)
  (let ((function-code (extract word 0 6)))
    ((vector-ref special-disassemblers function-code) word)))

(vector-set! special-disassemblers sll-funct (lambda (word) (shift word 'sll)))
(vector-set! special-disassemblers srl-funct (lambda (word) (shift word 'srl)))
(vector-set! special-disassemblers sra-funct (lambda (word) (shift word 'sra)))
(vector-set! special-disassemblers sllv-funct (lambda (word) (shift-variable word 'sllv)))
(vector-set! special-disassemblers srlv-funct (lambda (word) (shift-variable word 'srlv)))
(vector-set! special-disassemblers srav-funct (lambda (word) (shift-variable word 'srav)))
(vector-set! special-disassemblers jr-funct
  (lambda (word)
    (let ((MBZ (extract word 6 21))
	  (rs (extract word 21 26)))
      (if (zero? MBZ)
	  `(jr ,rs)
	  (invalid-instruction)))))
(vector-set! special-disassemblers jalr-funct
  (lambda (word)
    (let ((MBZ1 (extract word 16 21))
	  (MBZ2 (extract word 6 11))
	  (rs (extract word 21 26))
	  (rd (extract word 11 16)))
      (if (and (zero? MBZ1) (zero? MBZ2))
	  `(JALR ,rd ,rs)
	  (invalid-instruction)))))
(vector-set! special-disassemblers syscall-funct
  (lambda (word)
    (let ((MBZ (extract word 6 26)))
      (if (zero? MBZ)
	  '(SYSCALL)
	  (invalid-instruction)))))
(vector-set! special-disassemblers break-funct (lambda (word) `(BREAK ,(extract word 6 26))))
(vector-set! special-disassemblers mfhi-funct (lambda (word) (from-hi/lo word 'mfhi)))
(vector-set! special-disassemblers mthi-funct (lambda (word) (to-hi/lo word 'mthi)))
(vector-set! special-disassemblers mflo-funct (lambda (word) (from-hi/lo word 'mflo)))
(vector-set! special-disassemblers mtlo-funct (lambda (word) (to-hi/lo word 'mtlo)))
(vector-set! special-disassemblers mult-funct (lambda (word) (mul/div word 'mult)))
(vector-set! special-disassemblers multu-funct (lambda (word) (mul/div word 'multu)))
(vector-set! special-disassemblers div-funct (lambda (word) (mul/div word 'div)))
(vector-set! special-disassemblers divu-funct (lambda (word) (mul/div word 'divu)))
(vector-set! special-disassemblers add-funct (lambda (word) (arith word 'add)))
(vector-set! special-disassemblers addu-funct (lambda (word) (arith word 'addu)))
(vector-set! special-disassemblers sub-funct (lambda (word) (arith word 'sub)))
(vector-set! special-disassemblers subu-funct (lambda (word) (arith word 'subu)))
(vector-set! special-disassemblers and-funct (lambda (word) (arith word 'and)))
(vector-set! special-disassemblers or-funct (lambda (word) (arith word 'or)))
(vector-set! special-disassemblers xor-funct (lambda (word) (arith word 'xor)))
(vector-set! special-disassemblers nor-funct (lambda (word) (arith word 'nor)))
(vector-set! special-disassemblers slt-funct (lambda (word) (arith word 'slt)))
(vector-set! special-disassemblers sltu-funct (lambda (word) (arith word 'sltu)))

(define (shift word op)
  (let ((MBZ (extract word 21 26))
	(rt (extract word 16 21))
	(rd (extract word 11 16))
	(shamt (extract word 6 11)))
    (if (zero? MBZ)
	`(,op ,rd ,rt ,shamt)
	(invalid-instruction))))

(define (shift-variable word op)
  (let ((MBZ (extract word 6 11))
	(rs (extract word 21 26))
	(rt (extract word 16 21))
	(rd (extract word 11 16)))
    (if (zero? MBZ)
	`(,op ,rd ,rt ,rs)
	(invalid-instruction))))

(define (from-hi/lo word op)
  (let ((MBZ1 (extract word 16 26))
	(MBZ2 (extract word 6 11))
	(rd (extract word 11 16)))
    (if (and (zero? MBZ1) (zero? MBZ2))
	`(,op ,rd)
	(invalid-instruction))))

(define (to-hi/lo word op)
  (let ((MBZ (extract word 6 21))
	(rs (extract word 21 26)))
    (if (zero? MBZ)
	`(,op ,rs)
	(invalid-instruction))))

(define (mul/div word op)
  (let ((MBZ (extract word 6 16))
	(rs (extract word 21 26))
	(rt (extract word 16 21)))
    (if (zero? MBZ)
	`(,op ,rs ,rt)
	(invalid-instruction))))

(define (arith word op)
  (let ((MBZ (extract word 6 11))
	(rs (extract word 21 26))
	(rt (extract word 16 21))
	(rd (extract word 11 16)))
    (if (zero? MBZ)
	`(,op ,rd ,rs ,rt)
	(invalid-instruction))))

(define (disassemble-jump word op)
  `(,op ,(extract word 0 26)))

(define (relative-offset word)
  (offset->@pcr (+ *current-offset (* 4 (extract-signed word 0 16)))))

(define (offset->@pcr offset)
  `(@PCR ,(or (and disassembler/symbolize-output?
		   (disassembler/lookup-symbol *symbol-table offset))
	      offset)))

(define (disassemble-branch-zero word)
  (let ((conditions (extract word 16 21))
	(rs (extract word 21 26))
	(offset (relative-offset word)))
    (cond ((= conditions bltz-cond) `(BLTZ ,rs ,offset))
	  ((= conditions bltzal-cond) `(BLTZAL ,rs ,offset))
	  ((= conditions bgez-cond) `(BGEZ ,rs ,offset))
	  ((= conditions bgezal-cond) `(BGEZAL ,rs ,offset))
	  (else (invalid-instruction)))))

(define (disassemble-branch-zero-op word op)
  (let ((MBZ (extract word 16 21))
	(rs (extract word 21 26)))
    (if (zero? MBZ)
	`(,op ,rs ,(relative-offset word))
	(invalid-instruction))))

(define (disassemble-compare word op)
  `(,op ,(extract word 21 26)
	,(extract word 16 21)
	,(relative-offset word)))

(define (disassemble-immediate word op)
  `(,op ,(extract word 16 21)
	,(extract word 21 26)
	,(extract-signed word 0 16)))

(define (disassemble-unsigned-immediate word op)
  `(,op ,(extract word 16 21)
	,(extract word 21 26)
	,(extract word 0 16)))

(define (disassemble-lui word)
  (if (zero? (extract word 21 26))
      `(LUI ,(extract word 16 21)
	    ,(extract word 0 16))
      (invalid-instruction)))

(define (floating-point-cases code)
  (let ((format (extract code 21 25))
	(ft (extract code 16 21))
	(fs (extract code 11 16))
	(fd (extract code 6 11))
	(fp-code (extract code 0 6)))
    (let ((fmt (case format ((0) 'SINGLE) ((1) 'DOUBLE) (else '()))))
      (define (two-arg op-name)
	(if (zero? ft)
	    (list op-name fmt fd fs)
	    (invalid-instruction)))
      (define (compare op-name)
	(if (zero? fd)
	    (list op-name fmt fs ft)
	    (invalid-instruction)))
      (if fmt
	  (cond
	   ((= fp-code addf-op) `(FADD ,fmt ,fd ,fs ,ft))
	   ((= fp-code subf-op) `(FSUB ,fmt ,fd ,fs ,ft))
	   ((= fp-code mulf-op) `(FMUL ,fmt ,fd ,fs ,ft))
	   ((= fp-code divf-op) `(FDIV ,fmt ,fd ,fs ,ft))
	   ((= fp-code absf-op) (two-arg 'FABS))
	   ((= fp-code movf-op) (two-arg 'FMOV))
	   ((= fp-code negf-op) (two-arg 'FNEG))
	   ((= fp-code cvt.sf-op) (two-arg 'CVT.S))
	   ((= fp-code cvt.df-op) (two-arg 'CVT.D))
	   ((= fp-code cvt.wf-op) (two-arg 'CVT.W))
	   ((= fp-code c.ff-op) (compare 'C.F))
	   ((= fp-code c.unf-op) (compare 'C.UN))
	   ((= fp-code c.eqf-op) (compare 'C.EQ))
	   ((= fp-code c.ueqf-op) (compare 'C.UEQ))
	   ((= fp-code c.oltf-op) (compare 'C.OLT))
	   ((= fp-code c.ultf-op) (compare 'C.ULT))
	   ((= fp-code c.olef-op) (compare 'C.OLE))
	   ((= fp-code c.ulef-op) (compare 'C.ULE))
	   ((= fp-code c.sff-op) (compare 'C.SF))
	   ((= fp-code c.nglef-op) (compare 'C.NGLE))
	   ((= fp-code c.seqf-op) (compare 'C.SEQ))
	   ((= fp-code c.nglf-op) (compare 'C.NGL))
	   ((= fp-code c.ltf-op) (compare 'C.LT))
	   ((= fp-code c.ngef-op) (compare 'C.NGE))
	   ((= fp-code c.lef-op) (compare 'C.LE))
	   ((= fp-code c.ngtf-op) (compare 'C.NGT))
	   (else (invalid-instruction)))
	  (invalid-instruction)))))

(define (disassemble-coprocessor word op)
  (define (simple-cases op2)
    (if (zero? (extract word 0 11))
	`(,op2 ,(extract word 16 21) ,(extract word 11 16))))
  (define (branch-cases op2)
    `(,op2 ,(relative-offset word)))
  (define (cop0-cases code)
    (case code
      ((1) '(TLBR))
      ((2) '(TLBWI))
      ((6) '(TLBWR))
      ((8) '(TLBP))
      ((16) '(RFE))
      (else `(COP0 ,code))))
  (let ((code-high-bits (+ (* 4 (extract word 21 23))
			  (extract word 16 17)))
	(code-low-bits (extract word 23 26)))
    (let ((code (+ (* code-high-bits 8) code-low-bits)))
      (case code
	((0 8)				; MF
	 (case op
	   ((0) (simple-cases 'mfc0))
	   ((1) (simple-cases 'mfc1))
	   ((2) (simple-cases 'mfc2))
	   ((3) (simple-cases 'mfc3))))	 
	((1 9)				; MT
	 (case op
	   ((0) (simple-cases 'mtc0))
	   ((1) (simple-cases 'mtc1))
	   ((2) (simple-cases 'mtc2))
	   ((3) (simple-cases 'mtc3))))
	((2 3)				; BCF
	 (case op
	   ((0) (branch-cases 'bcf0))
	   ((1) (branch-cases 'bcf1))
	   ((2) (branch-cases 'bcf2))
	   ((3) (branch-cases 'bcf3))))
	((4  5  6  7  12 13 14 15 20 21 22 23 28 29 30 31
	  36 37 38 39 44 45 46 47 52 53 54 55 60 61 62 63) ; CO
	 (case op
	   ((0) (cop0-cases (extract word 0 25)))
	   ((1) (floating-point-cases (bit-substring word 0 25)))
	   ((2) `(cop2 ,(extract word 0 25)))
	   ((3) `(cop3 ,(extract word 0 25)))))
	((10 11)			; BCT
	 (case op
	   ((0) (branch-cases 'bct0))
	   ((1) (branch-cases 'bct1))
	   ((2) (branch-cases 'bct2))
	   ((3) (branch-cases 'bct3))))
	((32 40)			; CF
	 (case op
	   ((0) (simple-cases 'cfc0))
	   ((1) (simple-cases 'cfc1))
	   ((3) (simple-cases 'cfc2))
	   ((3) (simple-cases 'cfc3))))
	((33 41)			; CT
	 (case op
	   ((0) (simple-cases 'ctc0))
	   ((1) (simple-cases 'ctc1))
	   ((2) (simple-cases 'ctc2))
	   ((3) (simple-cases 'ctc3))))
	(else (invalid-instruction))))))

(define (disassemble-load/store word op)
  `(,op ,(extract word 16 21)
	(OFFSET ,(extract-signed word 0 16) ,(extract word 21 26))))
