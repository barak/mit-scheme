#| -*-Scheme-*-

$Id: instr2a.scm,v 1.5 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; MIPS instruction set, part 2a

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: branch

(let-syntax
    ((branch
      (macro (keyword match-phrase forward reverse)
	`(define-instruction ,keyword
	   ((,@match-phrase (@PCO (? offset)))
	    (LONG ,@forward
		  (16 (quotient offset 4) SIGNED)))
	   ((,@match-phrase (@PCR (? label)))
	    (VARIABLE-WIDTH (offset `(/ (- ,label (+ *PC* 4)) 4))
	      ((#x-8000 #x7fff)
	       (LONG ,@forward (16 offset SIGNED)))
	      ((() ())
	       ;;         <reverse> xxx
	       ;;         LUI    $1, left_adj(offset*4 - 12)
	       ;;         BGEZAL $0, yyy
	       ;;         ADDIU  $1, $1, right(offset*4 - 12)
	       ;; yyy:    ADD    $1, $1, $31
	       ;;         JR     $1
	       ;; xxx:
	       (LONG ,@reverse		; reverse branch to (.+1)+5
		     (16 5)
	             (6 15)		; LUI
		     (5 0)
		     (5 1)
		     (16 (adjusted:high (* (- offset 3) 4)))
		     (6 1)		; BGEZAL
		     (5 0)
		     (5 17)
		     (16 1)
		     (6 9)		; ADDIU
		     (5 1)
		     (5 1)
		     (16 (adjusted:low (* (- offset 3) 4)) SIGNED)
		     (6 0)		; ADD
		     (5 1)
		     (5 31)
		     (5 1)
		     (5 0)
		     (6 32)
		     (6 0)		; JR
		     (5 1)
		     (15 0)
		     (6 8)))))))))
  (branch beq
	  ((? reg1) (? reg2))
	  ((6 4) (5 reg1) (5 reg2))
	  ((6 5) (5 reg1) (5 reg2)))
  (branch bne
	  ((? reg1) (? reg2))
	  ((6 5) (5 reg1) (5 reg2))
	  ((6 4) (5 reg1) (5 reg2)))
  (branch bgez
	  ((? reg))
	  ((6 1) (5 reg) (5 1))
	  ((6 1) (5 reg) (5 0)))
  (branch bgtz
	  ((? reg))
	  ((6 7) (5 reg) (5 0))
	  ((6 6) (5 reg) (5 0)))
  (branch blez
	  ((? reg))
	  ((6 6) (5 reg) (5 0))
	  ((6 7) (5 reg) (5 0)))
  (branch bltz
	  ((? reg))
	  ((6 1) (5 reg) (5 0))
	  ((6 1) (5 reg) (5 1)))
  (branch bgezal
	  ((? reg))
	  ((6 1) (5 reg) (5 17))
	  ((16 "can't branch tension a bgezal instruction")))
  (branch bltzal
	  ((? reg))
	  ((6 1) (5 reg) (5 16))
	  ((16 "can't branch tension a bltzal instruction")))
  ;; (branch bc0f () ((6 16) (10 #x100)) ((6 16) (10 #x101)))
  (branch bc1f () ((6 17) (10 #x100)) ((6 17) (10 #x101)))
  ;; (branch bc2f () ((6 18) (10 #x100)) ((6 18) (10 #x101)))
  ;; (branch bc3f () ((6 19) (10 #x100)) ((6 19) (10 #x101)))
  ;; (branch bc0t () ((6 16) (10 #x101)) ((6 16) (10 #x100)))
  (branch bc1t () ((6 17) (10 #x101)) ((6 17) (10 #x100)))
  ;; (branch bc2t () ((6 18) (10 #x101)) ((6 18) (10 #x100)))
  ;; (branch bc3t () ((6 19) (10 #x101)) ((6 19) (10 #x100)))
  )