#| -*-Scheme-*-

$Id: instr2.scm,v 1.3 2001/12/20 21:45:24 cph Exp $

Copyright (c) 1992-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

|#

;;;; Alpha instruction set, part 2
;;; Instructions that require branch tensioning
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

; Unconditional jump instructions
(let-syntax
    ((memory-branch
      (lambda (keyword hint)
	`(define-instruction ,keyword
	   (((? link-register) (? base))
	    (LONG (6 #x1a)
		  (5 link-register)
		  (5 base)
		  (2 ,hint)
		  (14 0 SIGNED)))
	   (((? base))
	    (LONG (6 #x1a)
		  (5 regnum:came-from)
		  (5 base)
		  (2 ,hint)
		  (14 0 SIGNED)))
	   (((? link-register) (? base) (@PCR (? probable-target)))
	    (LONG (6 #x1a)
		  (5 link-register)
		  (5 base)
		  (2 ,hint)
		  (14 `(/ (remainder (- ,probable-target (+ *PC* 4))
				     #x10000)
			  4)
		      SIGNED)))
	   (((? link-register) (? base) (@PCO (? probable-target-address)))
	    (LONG (6 #x1a)
		  (5 link-register)
		  (5 base)
		  (2 ,hint)
		  (14 `(/ (remainder ,probable-target-address
				     #x10000)
			  4)
		      SIGNED)))))))
  (memory-branch JMP #x0)
  (memory-branch JSR #x1)
  (memory-branch RET #x2)
  (memory-branch COROUTINE #x3))

; Conditional branch instructions

(let-syntax
    ((branch
      (lambda (keyword opcode reverse-op)
	`(define-instruction ,keyword
	   (((? reg) (@PCO (? offset)))
	    (LONG (6 ,opcode)
		  (5 reg)
		  (21 (quotient offset 4) SIGNED)))
	   (((? reg) (@PCR (? label)))
	    (VARIABLE-WIDTH (offset `(/ (- ,label (+ *PC* 4)) 4))
	      ((#x-100000 #xFFFFF)
	       (LONG (6 ,opcode)
		     (5 reg)
		     (21 offset SIGNED)))
	      ((#x-1FFFFFFE #x20000001)
	       ;; -1:      <reverse> xxx
	       ;;  0:      LDAH   temp, left[4*(offset-2)](R31)
	       ;; +1:      BR     link, yyy
	       ;;  2: yyy: ADDQ   temp, link, temp
	       ;;  3:      LDA    temp, right[4*(offset-2)](temp)
	       ;;  4:      JMP    came_from, temp, hint
	       ;;  5: xxx:
	       (LONG (6 ,reverse-op)	; reverse branch to (.+1)+4
		     (5 reg)            ;   register
		     (21 5 SIGNED)	;   offset = +5 instructions
	             (6 #x09)		; LDAH
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 31)		;   base = zero
		     (16 (adjusted:high (* (- offset 2) 4)) SIGNED)
		     (6 #x30)		; BR
		     (5 26)		;   return address to link
		     (21 0 SIGNED)	;   (.+4) + 0
		     (6 #x10)		; ADDQ
		     (5 regnum:assembler-temp) ; source = temp
		     (5 26)		;   source = link
		     (3 0)		;   should be 0
		     (1 0)		;   must be 0
		     (7 #x20)		;   function=ADDQ
		     (5 regnum:assembler-temp) ; destination = temp
		     (6 #x08)		; LDA
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 regnum:assembler-temp) ; base = temp
		     (16 (adjusted:low (* (- offset 2) 4)) SIGNED)
		     (6 #x1a)		; JMP
		     (5 regnum:assembler-temp) ; return address to "came from"
		     (5 regnum:assembler-temp) ; base = temp
		     (2 #x0)		;   jump hint
		     (14 (/ (adjusted:low (* (- offset 5) 4)) 4)
			 SIGNED)))))))))
  (branch beq #x39 #x3d)
  (branch bge #x3e #x3a)
  (branch bgt #x3f #x3b)
  (branch blbc #x38 #x3c)
  (branch blbs #x3c #x38)
  (branch ble #x3b #x3f)
  (branch blt #x3a #x3e)
  (branch bne #x3d #x39)
  (branch fbeq #x31 #x35)
  (branch fbge #x36 #x32)
  (branch fbgt #x37 #x33)
  (branch fble #x33 #x37)
  (branch fblt #x32 #x36)
  (branch fbne #x35 #x31))

; Unconditional branch instructions

(let-syntax
    ((unconditional-branch
      (lambda (keyword opcode hint)
	`(define-instruction ,keyword
	   (((? reg) (@PCO (? offset)))
	    (LONG (6 ,opcode)
		  (5 reg)
		  (21 (quotient offset 4) SIGNED)))
	   (((? reg) (@PCR (? label)))
	    (VARIABLE-WIDTH (offset `(/ (- ,label (+ *PC* 4)) 4))
	      ((#x-100000 #xFFFFF)
	       (LONG (6 ,opcode)
		     (5 reg)
		     (21 offset SIGNED)))
	      ((#x-1FFFFFFF #x20000000)
	       ;; -1:      LDAH   temp, left[4*(offset-1)](R31)
	       ;;  0:      BR     link, yyy
	       ;;  1: yyy: ADDQ   temp, link, temp
	       ;;  2:      LDA    temp, right[4*(offset-1)](temp)
	       ;;  3:      JMP    came_from, temp, hint
	       ;;  4: xxx:
	       (LONG (6 #x09)		; LDAH
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 31)		;   base = zero
		     (16 (adjusted:high (* (- offset 1) 4)) SIGNED)
		     (6 #x30)		; BR
		     (5 26)		;   return address to link
		     (21 0 SIGNED)	;   (.+4) + 0
		     (6 #x10)		; ADDQ
		     (5 regnum:assembler-temp) ; source = temp
		     (5 26)		;   source = link
		     (3 0)		;   should be 0
		     (1 0)		;   must be 0
		     (7 #x20)		;   function=ADDQ
		     (5 regnum:assembler-temp) ; destination = temp
		     (6 #x08)		; LDA
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 regnum:assembler-temp) ; base = temp
		     (16 (adjusted:low (* (- offset 1) 4)) SIGNED)
		     (6 #x1a)		; JMP
		     (5 reg)		;   return address register
		     (5 regnum:assembler-temp) ; base = temp
		     (2 ,hint)		;   jump hint
		     (14 (/ (adjusted:low (* (- offset 4) 4)) 4) SIGNED)))))
	   (((? reg) (OFFSET (? offset) (@PCR (? label))))
	    (VARIABLE-WIDTH (offset `(/ (- (+ ,offset ,label)
					   (+ *PC* 4))
					4))
	      ((#x-100000 #xFFFFF)
	       (LONG (6 ,opcode)
		     (5 reg)
		     (21 offset SIGNED)))
	      ((#x-1FFFFFFF #x20000000)
	       ;; -1:      LDAH   temp, left[4*(offset-1)](R31)
	       ;;  0:      BR     link, yyy
	       ;;  1: yyy: ADDQ   temp, link, temp
	       ;;  2:      LDQ    temp, right[4*(offset-1)]
	       ;;  2:      JMP    came_from, temp, hint
	       (LONG (6 #x09)		; LDAH
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 31)		;   base = zero
		     (16 (adjusted:high (* (- offset 1) 4)) SIGNED)
		     (6 #x30)		; BR
		     (5 26)		;   return address to link
		     (21 0 SIGNED)	;   (.+4) + 0
		     (6 #x10)		; ADDQ
		     (5 regnum:assembler-temp) ; source = temp
		     (5 26)		;   source = link
		     (3 0)		;   should be 0
		     (1 0)		;   must be 0
		     (7 #x20)		;   function=ADDQ
		     (5 regnum:assembler-temp) ; destination = temp
		     (6 #x08)		; LDA
		     (5 regnum:assembler-temp) ; destination = temp
		     (5 regnum:assembler-temp) ; base = temp
		     (16 (adjusted:low (* (- offset 1) 4)) SIGNED)
		     (6 #x1a)		; JMP
		     (5 reg)		;   return address register
		     (5 regnum:assembler-temp) ; base = temp
		     (2 ,hint)		;   jump hint
		     (14 (/ (adjusted:low (* (- offset 4) 4)) 4)
			 SIGNED)))))))))
  (unconditional-branch br #x30 #x0)
  (unconditional-branch bsr #x34 #x1))
