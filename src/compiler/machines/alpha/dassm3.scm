#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;; Alpha Disassembler: Internals
;;; Package: (compiler disassembler)

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

;;;; instr1.scm

(define (disassemble-memory-format op-name word)
  `(,op-name ,(extract word 21 26)
	     (OFFSET ,(extract-signed word 0 16) ,(extract word 16 21))))

(vector-set! disassemblers #x08
	     (lambda (word)
	       (let ((base (extract word 16 21)))
		 (if (zero? base)
		     `(MOVEI ,(extract word 21 26)
			     (& ,(extract-signed word 0 16)))
		     `(LDA ,(extract word 21 26)
			   (OFFSET ,(extract-signed word 0 16)
				   ,(extract word 16 21)))))))
(vector-set! disassemblers #x09
	     (lambda (word) (disassemble-memory-format 'LDAH word)))
(vector-set! disassemblers #x20
	     (lambda (word) (disassemble-memory-format 'LDF word)))
(vector-set! disassemblers #x21
	     (lambda (word) (disassemble-memory-format 'LDG word)))
(vector-set! disassemblers #x28
	     (lambda (word) (disassemble-memory-format 'LDL word)))
(vector-set! disassemblers #x2A
	     (lambda (word) (disassemble-memory-format 'LDL_L word)))
(vector-set! disassemblers #x29
	     (lambda (word) (disassemble-memory-format 'LDQ word)))
(vector-set! disassemblers #x2B
	     (lambda (word) (disassemble-memory-format 'LDQ_L word)))
(vector-set! disassemblers #x0B
	     (lambda (word) (disassemble-memory-format 'LDQ_U word)))
(vector-set! disassemblers #x22
	     (lambda (word) (disassemble-memory-format 'LDS word)))
(vector-set! disassemblers #x23
	     (lambda (word) (disassemble-memory-format 'LDT word)))
(vector-set! disassemblers #x24
	     (lambda (word) (disassemble-memory-format 'STF word)))
(vector-set! disassemblers #x25
	     (lambda (word) (disassemble-memory-format 'STG word)))
(vector-set! disassemblers #x2C
	     (lambda (word) (disassemble-memory-format 'STL word)))
(vector-set! disassemblers #x2E
	     (lambda (word) (disassemble-memory-format 'STL_C word)))
(vector-set! disassemblers #x2D
	     (lambda (word) (disassemble-memory-format 'STQ word)))
(vector-set! disassemblers #x2F
	     (lambda (word) (disassemble-memory-format 'STQ_C word)))
(vector-set! disassemblers #x0F
	     (lambda (word) (disassemble-memory-format 'STQ_U word)))
(vector-set! disassemblers #x26
	     (lambda (word) (disassemble-memory-format 'STS word)))
(vector-set! disassemblers #x27
	     (lambda (word) (disassemble-memory-format 'STT word)))

(define operate-10-disassemblers (make-vector #x6D handle-bad-instruction))
(vector-set! disassemblers #x10
	     (lambda (word)
	       ((vector-ref operate-10-disassemblers (extract word 12 5))
		word)))
(define operate-11-disassemblers (make-vector #x66 handle-bad-instruction))
(vector-set! disassemblers #x11
	     (lambda (word)
	       ((vector-ref operate-11-disassemblers (extract word 12 5))
		word)))
(define operate-12-disassemblers (make-vector #x7A handle-bad-instruction))
(vector-set! disassemblers #x12
	     (lambda (word)
	       ((vector-ref operate-12-disassemblers (extract word 12 5))
		word)))
(define operate-13-disassemblers (make-vector #x60 handle-bad-instruction))
(vector-set! disassemblers #x13
	     (lambda (word)
	       ((vector-ref operate-13-disassemblers (extract word 5 12))
		word)))

(vector-set! operate-11-disassemblers #x20
	     (lambda (word)
	       (let ((Ra (extract word 21 26))
		     (Rc (extract word 0 5)))
		 (if (bit-string-ref word 12)
		     (invalid-instruction)
		     (let ((sbz (extract word 13 16))
			   (Rb (extract word 16 21)))
		       (if (not (zero? sbz))
			   (invalid-instruction))
		       (if (not (= Ra Rb))
			   (invalid-instruction))
		       `(COPY ,Ra ,Rc))))))

(vector-set! disassemblers #x18
	     (lambda (word)
	       (case (extract word 0 16)
		 ((#x0000) '(TRAPB))
		 ((#x4000) '(MB))
		 ((#x8000) `(FETCH ,(extract word 16 21)))
		 ((#xA000) `(FETCH_M ,(extract word 16 21)))
		 ((#xC000) `(RPCC ,(extract word 21 26)))
		 ((#xE000) `(RC ,(extract word 21 26)))
		 ((#xF000) `(RS ,(extract word 21 26))))))

(define ((disassemble-operate-format op-name) word)
  (let ((Ra (extract word 21 26))
	(Rc (extract word 0 5)))
    (if (bit-string-ref word 12)
	(let ((lit (extract word 13 21)))
	  `(,op-name ,Ra (& ,lit) ,Rc))
	(let ((sbz (extract word 13 16))
	      (Rb (extract word 16 21)))
	  (if (not (zero? sbz))
	      (invalid-instruction))
	  `(,op-name ,Ra ,Rb ,Rc)))))

(vector-set! operate-10-disassemblers #x00
	     (disassemble-operate-format 'ADDL))
(vector-set! operate-10-disassemblers #x40
	     (disassemble-operate-format 'ADDLV))
(vector-set! operate-10-disassemblers #x20
	     (disassemble-operate-format 'ADDQ))
(vector-set! operate-10-disassemblers #x60
	     (disassemble-operate-format 'ADDQV))
(vector-set! operate-11-disassemblers #x00
	     (disassemble-operate-format 'AND))
(vector-set! operate-11-disassemblers #x08
	     (disassemble-operate-format 'BIC))
(vector-set! operate-11-disassemblers #x20
	     (disassemble-operate-format 'BIS))
(vector-set! operate-11-disassemblers #x24
	     (disassemble-operate-format 'CMOVEQ))
(vector-set! operate-11-disassemblers #x46
	     (disassemble-operate-format 'CMOVGE))
(vector-set! operate-11-disassemblers #x66
	     (disassemble-operate-format 'CMOVGT))
(vector-set! operate-11-disassemblers #x16
	     (disassemble-operate-format 'CMOVLBC))
(vector-set! operate-11-disassemblers #x14
	     (disassemble-operate-format 'CMOVLBS))
(vector-set! operate-11-disassemblers #x64
	     (disassemble-operate-format 'CMOVLE))
(vector-set! operate-11-disassemblers #x44
	     (disassemble-operate-format 'CMOVLT))
(vector-set! operate-11-disassemblers #x26
	     (disassemble-operate-format 'CMOVNE))
(vector-set! operate-10-disassemblers #x2D
	     (disassemble-operate-format 'CMPEQ))
(vector-set! operate-10-disassemblers #x6D
	     (disassemble-operate-format 'CMPLE))
(vector-set! operate-10-disassemblers #x4D
	     (disassemble-operate-format 'CMPLT))
(vector-set! operate-10-disassemblers #x3D
	     (disassemble-operate-format 'CMPULE))
(vector-set! operate-10-disassemblers #x1D
	     (disassemble-operate-format 'CMPULT))
(vector-set! operate-11-disassemblers #x48
	     (disassemble-operate-format 'EQV))
(vector-set! operate-12-disassemblers #x06
	     (disassemble-operate-format 'EXTBL))
(vector-set! operate-12-disassemblers #x6A
	     (disassemble-operate-format 'EXTLH))
(vector-set! operate-12-disassemblers #x26
	     (disassemble-operate-format 'EXTLL))
(vector-set! operate-12-disassemblers #x7A
	     (disassemble-operate-format 'EXTQH))
(vector-set! operate-12-disassemblers #x36
	     (disassemble-operate-format 'EXTQL))
(vector-set! operate-12-disassemblers #x5A
	     (disassemble-operate-format 'EXTWH))
(vector-set! operate-12-disassemblers #x16
	     (disassemble-operate-format 'EXTWL))
(vector-set! operate-12-disassemblers #x0B
	     (disassemble-operate-format 'INSBL))
(vector-set! operate-12-disassemblers #x67
	     (disassemble-operate-format 'INSLH))
(vector-set! operate-12-disassemblers #x2B
	     (disassemble-operate-format 'INSLL))
(vector-set! operate-12-disassemblers #x77
	     (disassemble-operate-format 'INSQH))
(vector-set! operate-12-disassemblers #x3B
	     (disassemble-operate-format 'INSQL))
(vector-set! operate-12-disassemblers #x57
	     (disassemble-operate-format 'INSWH))
(vector-set! operate-12-disassemblers #x1B
	     (disassemble-operate-format 'INSWL))
(vector-set! operate-12-disassemblers #x02
	     (disassemble-operate-format 'MSKBL))
(vector-set! operate-12-disassemblers #x62
	     (disassemble-operate-format 'MSKLH))
(vector-set! operate-12-disassemblers #x22
	     (disassemble-operate-format 'MSKLL))
(vector-set! operate-12-disassemblers #x72
	     (disassemble-operate-format 'MSKQH))
(vector-set! operate-12-disassemblers #x32
	     (disassemble-operate-format 'MSKQL))
(vector-set! operate-12-disassemblers #x52
	     (disassemble-operate-format 'MSKWH))
(vector-set! operate-12-disassemblers #x12
	     (disassemble-operate-format 'MSKWL))
(vector-set! operate-13-disassemblers #x00
	     (disassemble-operate-format 'MULL))
(vector-set! operate-13-disassemblers #x40
	     (disassemble-operate-format 'MULLV))
(vector-set! operate-13-disassemblers #x20
	     (disassemble-operate-format 'MULQ))
(vector-set! operate-13-disassemblers #x60
	     (disassemble-operate-format 'MULQV))
(vector-set! operate-11-disassemblers #x28
	     (disassemble-operate-format 'ORNOT))
(vector-set! operate-10-disassemblers #x02
	     (disassemble-operate-format 'S4ADDL))
(vector-set! operate-10-disassemblers #x22
	     (disassemble-operate-format 'S4ADDQ))
(vector-set! operate-10-disassemblers #x0B
	     (disassemble-operate-format 'S4SUBL))
(vector-set! operate-10-disassemblers #x2B
	     (disassemble-operate-format 'S4SUBQ))
(vector-set! operate-10-disassemblers #x12
	     (disassemble-operate-format 'S8ADDL))
(vector-set! operate-10-disassemblers #x32
	     (disassemble-operate-format 'S8ADDQ))
(vector-set! operate-10-disassemblers #x1B
	     (disassemble-operate-format 'S8SUBL))
(vector-set! operate-10-disassemblers #x3B
	     (disassemble-operate-format 'S8SUBQ))
(vector-set! operate-12-disassemblers #x39
	     (disassemble-operate-format 'SLL))
(vector-set! operate-12-disassemblers #x3C
	     (disassemble-operate-foramt 'SRA))
(vector-set! operate-12-disassemblers #x34
	     (disassemble-operate-foramt 'SRL))
(vector-set! operate-10-disassemblers #x09
	     (disassemble-operate-format 'SUBL))
(vector-set! operate-10-disassemblers #x49
	     (disassemble-operate-format 'SUBLV))
(vector-set! operate-10-disassemblers #x29
	     (disassemble-operate-format 'SUBQ))
(vector-set! operate-10-disassemblers #x69
	     (disassemble-operate-format 'SUBQV))
(vector-set! operate-13-disassemblers #x30
	     (disassemble-operate-format 'UMULH))
(vector-set! operate-11-disassemblers #x40
	     (disassemble-operate-format 'XOR))
(vector-set! operate-12-disassemblers #x30
	     (disassemble-operate-format 'ZAP))
(vector-set! operate-12-disassemblers #x31
	     (disassemble-operate-format 'ZAPNOT))

;;; Punt PAL code for now!!!
(define pal-op-codes (make-vector #x1E handle-bad-instruction))

(vector-set! disassemblers #x00
	     (lambda (word)
	       (let ((function-code (extract word 0 26)))
		 (cond ((zero? function-code)
			'(HALT))
		       ((and (<= function-code #x9D)
			     (<= #x80 function-code))
			(vector-ref pal-op-codes (- function-code #x80)))
		       (else (invalid-instruction))))))

(vector-set! pal-op-codes #x00 '(BPT))
(vector-set! pal-op-codes #x01 '(BUGCHK))
(vector-set! pal-op-codes #x02 '(CHME))
(vector-set! pal-op-codes #x03 '(CHMK))
(vector-set! pal-op-codes #x04 '(CHMS))
(vector-set! pal-op-codes #x05 '(CHMU))
(vector-set! pal-op-codes #x06 '(IMB))
(vector-set! pal-op-codes #x07 '(INSQHIL))
(vector-set! pal-op-codes #x08 '(INSQTIL))
(vector-set! pal-op-codes #x09 '(INSQHIQ))
(vector-set! pal-op-codes #x0A '(INSQTIQ))
(vector-set! pal-op-codes #x0B '(INSQUEL))
(vector-set! pal-op-codes #x0C '(INSQUEQ))
(vector-set! pal-op-codes #x0D '(INSQUELD))
(vector-set! pal-op-codes #x0E '(INSQUEQD))
(vector-set! pal-op-codes #x0F '(PROBER))
(vector-set! pal-op-codes #x10 '(PROBEW))
(vector-set! pal-op-codes #x11 '(RD_PS))
(vector-set! pal-op-codes #x12 '(REI))
(vector-set! pal-op-codes #x13 '(REMQHIL))
(vector-set! pal-op-codes #x14 '(REMQTIL))
(vector-set! pal-op-codes #x15 '(REMQHIQ))
(vector-set! pal-op-codes #x16 '(REMQTIQ))
(vector-set! pal-op-codes #x17 '(REMQUEL))
(vector-set! pal-op-codes #x18 '(REMQUEQ))
(vector-set! pal-op-codes #x19 '(REMQUELD))
(vector-set! pal-op-codes #x1A '(REMQUEQD))
(vector-set! pal-op-codes #x1B '(SWASTEN))
(vector-set! pal-op-codes #x1C '(WR_PS_SW))
(vector-set! pal-op-codes #x1D '(RSCC))

;;;; instr2.scm

(vector-set! disassemblers #x1A
	     (lambda (word)
	       (let ((Ra (extract word 26 21))
		     (Rb (extract word 21 16))
		     (disp (extract-signed word 14 0))
		     (op-name (vector-ref #(JMP JSR RET COROUTINE)
					  (extract word 16 14))))
		 (if (zero? disp)
		     (if (= Ra regnum:came-from)
			 `(,op-name ,Rb)
			 `(,op-name ,Ra ,Rb))
		     `(,op-name ,Ra ,Rb ,(relative-offset
					  (extract-signed word 0 14)))))))

(define ((disassemble-branch op-name) word)
  `(,op-name ,(extract word 21 26) ,(relative-offset
				     (extract-signed word 0 21))))

(define (relative-offset offset)
  (offset->@pcr (+ *current-offset (* 4 offset))))

(define (offset->@pcr offset)
  `(@PCR ,(or (and disassembler/symbolize-output?
		   (disassembler/lookup-symbol *symbol-table offset))
	      offset)))

(vector-set! disassemblers #x39 (disassemble-branch 'BEQ))
(vector-set! disassemblers #x3E (disassemble-branch 'BGE))
(vector-set! disassemblers #x3F (disassemble-branch 'BGT))
(vector-set! disassemblers #x38 (disassemble-branch 'BLBC))
(vector-set! disassemblers #x3C (disassemble-branch 'BLBS))
(vector-set! disassemblers #x3B (disassemble-branch 'BLE))
(vector-set! disassemblers #x3A (disassemble-branch 'BLT))
(vector-set! disassemblers #x3D (disassemble-branch 'BNE))
(vector-set! disassemblers #x31 (disassemble-branch 'FBEQ))
(vector-set! disassemblers #x36 (disassemble-branch 'FBGE))
(vector-set! disassemblers #x37 (disassemble-branch 'FBGT))
(vector-set! disassemblers #x33 (disassemble-branch 'FBLE))
(vector-set! disassemblers #x32 (disassemble-branch 'FBLT))
(vector-set! disassemblers #x35 (disassemble-branch 'FBNE))

(vector-set! disassemblers #x30 (disassemble-branch 'BR))
(vector-set! disassemblers #x34 (disassemble-branch 'BSR))

;;;; instr3.scm

(define ((disassemble-float op-name) word)
  `(,op-name ,(extract word 21 26) ,(extract word 16 21) ,(extract word 0 5)))

(define float-disassemblers (make-vector #x31 handle-bad-instruction))

(vector-set! disassemblers #x17
	     (lambda (word)
	       (let ((function-code (extract word 5 16)))
		 (cond ((< function-code #x31)
			((vector-ref float-disassemblers function-code)
			 word))
		       ((= function-code #x530)
			((disassemble-float 'CVTQLSV) word))
		       ((= function-code #x130)
			((disassemble-float 'CVTQLV) word))
		       (else (invalid-instruction))))))

(vector-set! float-disassemblers #x20 (disassemble-float 'CPYS))
(vector-set! float-disassemblers #x22 (disassemble-float 'CPYSE))
(vector-set! float-disassemblers #x21 (disassemble-float 'CPYSN))
(vector-set! float-disassemblers #x10 (disassemble-float 'CVTLQ))
(vector-set! float-disassemblers #x30 (disassemble-float 'CVTQL))
(vector-set! float-disassemblers #x2A (disassemble-float 'FCMOVEQ))
(vector-set! float-disassemblers #x2D (disassemble-float 'FCMOVGE))
(vector-set! float-disassemblers #x2F (disassemble-float 'FCMOVGT))
(vector-set! float-disassemblers #x2E (disassemble-float 'FCMOVLE))
(vector-set! float-disassemblers #x2C (disassemble-float 'FCMOVLT))
(vector-set! float-disassemblers #x2B (disassemble-float 'FCMOVNE))
(vector-set! float-disassemblers #x25 (disassemble-float 'MF_FPCR))
(vector-set! float-disassemblers #x24 (disassemble-float 'MT_FPCR))

(define (setup-float-disassemblers-table vector options table)
  (let row-loop ((rows table))
    (if (pair? rows)
	(let ((row (car rows)))
	  (let ((op-name (car row)))
	    (let column-loop
		((cols (cdr row))
		 (options options))
	      (if (pair? cols)
		  (begin
		    (if (not (null? (car cols)))
			(vector-set! vector (car cols)
				     (if (null? (car options))
					 (lambda (word)
					   `(,op-name ,(extract word 21 26)
						      ,(extract word 16 21)
						      ,(extract word 0 5)))
					 (lambda (word)
					   `(,op-name (/ . ,(car options))
						      ,(extract word 21 26)
						      ,(extract word 16 21)
						      ,(extract word 0 5))))))
		    (column-loopf (cdr cols) (cdr options))))))
	  (row-loop (cdr rows))))))

(define ieee-float-disassemblers (make-vector #x7FF handle-bad-instruction))

(vector-set! disassemblers #x16
	     (lambda (word)
	       (let ((function-code (extract word 5 16)))
		 ((vector-ref ieee-float-disassemblers function-code) word))))

(setup-float-disassemblers-table
 ieee-float-disassemblers 
 '(    		  ()   (C)   (M)   (D)   (U)  (U C) (U M) (U D))
 '((ADDS	#x080 #x000 #x040 #x0C0 #x180 #x100 #x140 #x1C0)
   (ADDT	#x0A0 #x020 #x060 #x0E0 #x1A0 #x120 #x160 #x1E0)
   (CMPTEQ	#x0A5)
   (CMPTLT	#x0A6)
   (CMPTLE	#x0A7)
   (CMPTUN	#x0A4)
   (CVTQS	#x0BC #x03C #x07C #x0FC)
   (CVTQT	#x0BE #x03E #x07E #x0FE)
   (CVTTS	#x0AC #x02C #x06C #x0EC #x1AC #x12C #x16C #x1EC)
   (DIVS	#x083 #x003 #x043 #x0C3 #x183 #x103 #x143 #x1C3)
   (DIVT	#x0A3 #x023 #x063 #x0E3 #x1A3 #x123 #x163 #x1E3)
   (MULS	#x082 #x002 #x042 #x0C2 #x182 #x102 #x142 #x1C2)
   (MULT	#x0A2 #x022 #x062 #x0E2 #x1A2 #x122 #x162 #x1E2)
   (SUBS	#x081 #x001 #x041 #x0C1 #x181 #x101 #x141 #x1C1)
   (SUBT	#x0A1 #x021 #x061 #x0E1 #x1A1 #x121 #x161 #x1E1)))

(setup-float-disassemblers-table
 ieee-float-disassemblers
 '(		(S U)(S U C)(S U M)(S U D)(S U I)(S U I C)(S U I M)(S U I D))
 '((ADDS	#x580 #x500  #x540  #x5C0  #x780   #x700    #x740    #x7C0)
   (ADDT	#x5A0 #x520  #x560  #x5E0  #x7A0   #x720    #x760    #x7E0)
   (CMPTEQ	#x5A5)
   (CMPTLT	#x5A6)
   (CMPTLE	#x5A7)
   (CMPTUN	#x5A4)
   (CVTQS	  ()    ()     ()     ()   #x7BC   #x73C    #x77C    #x7FC)
   (CVTQT	  ()    ()     ()     ()   #x7BE   #x73E    #x77E    #x7FE)
   (CVTTS	#x5AC #x52C  #x56C  #x5EC  #x7AC   #x72C    #x76C    #x7EC)
   (DIVS	#x583 #x503  #x543  #x5C3  #x783   #x703    #x743    #x7C3)
   (DIVT	#x5A3 #x523  #x563  #x5E3  #x7A3   #x723    #x763    #x7E3)
   (MULS	#x582 #x502  #x542  #x5C2  #x782   #x702    #x742    #x7C2)
   (MULT	#x5A2 #x522  #x562  #x5E2  #x7A2   #x722    #x762    #x7E2)
   (SUBS	#x581 #x501  #x541  #x5C1  #x781   #x701    #x741    #x7C1)
   (SUBT	#x5A1 #x521  #x561  #x5E1  #x7A1   #x721    #x761    #x7E1)))

(setup-float-disassemblers-table
 ieee-float-disassemblers
 '(		  ()   (C)    (V)   (V C)  (S V)  (S V C)  (S V I) (S V I C))
 '((CVTTQ	#x0AF #x02F  #x1AF  #x12F  #x5AF   #x52F    #x7AF    #x72F)))

(setup-float-disasemblers-table
 ieee-float-disassemblers
 '(		 (D)  (V D) (S V D)(S V I D)(M)    (V M)   (S V M) (S V I M))
 '((CVTTQ	#x0EF #x1EF  #x5EF  #x7EF  #x06F   #x16F    #x56F    #x76F)))

(define vax-float-disassemblers (make-vector #x7FF handle-bad-instruction))

(vector-set! disassemblers #x15
	     (lambda (word)
	       (let ((function-code (extract word 5 16)))
		 ((vector-ref vax-float-disassemblers function-code) word))))


(setup-float-disassemblers-table
 vax-float-disassemblers
 '(		() (C) (U) (U C) (S) (S C) (S U) (S U C))
 '((ADDF	#x080 #x000 #x180 #x100 #x480 #x400 #x580 #x500)
   (CVTDG	#x09E #x01E #x19E #x11E #x49E #x41E #x59E #x51E)
   (ADDG	#x0A0 #x020 #x1A0 #x120 #x4A0 #x420 #x5A0 #x520)
   (CMPGEQ	#x0A5   ()    ()    ()  #x4A5)
   (CMPGLT	#x0A6   ()    ()    ()  #x4A6)
   (CMPGLE	#x0A7   ()    ()    ()  #x4A7)
   (CVTGF	#x0AC #x02C #x1AC #x12C #x4AC #x42C #x5AC #x52C)
   (CVTGD	#x0AD #x02D #x1AD #x12D #x4AD #x42D #x5AD #x52D)
   (CVTQF	#x0BC #x03C)
   (CVTQG	#x0BE #x03E)
   (DIVF	#x083 #x003 #x183 #x103 #x483 #x403 #x583 #x503)
   (DIVG	#x0A3 #x023 #x1A3 #x123 #x4A3 #x423 #x5A3 #x523)
   (MULF	#x082 #x002 #x182 #x102 #x482 #x402 #x582 #x502)
   (MULG	#x0A2 #x022 #x1A2 #x122 #x4A2 #x422 #x5A2 #x522)
   (SUBF	#x081 #x001 #x181 #x101 #x481 #x401 #x581 #x501)
   (SUBG	#x0A1 #x021 #x1A1 #x121 #x4A1 #x421 #x5A1 #x521)))

(setup-float-disassemblers-table
 vax-float-disassemblers
 '(		() (C) (V) (V C) (S) (S C) (S V) (S V C))
 '((CVTGQ	#x0AF #x02F #x1AF #x12F #x4AF #X42F #x5AF #x52F)))