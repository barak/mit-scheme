#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/instr2a.scm,v 1.2 1990/11/28 22:10:56 cph Rel $

Copyright (c) 1987, 1989, 1990 Massachusetts Institute of Technology

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

;;;; MIPS instruction set, part 2a

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: branch

(let-syntax
    ((branch
      (macro (keyword match-phrase forward reverse)
	`(define-instruction ,keyword
	   ((,@match-phrase (@PCO (? branch-dest-pco)))
	    (VARIABLE-WIDTH (offset (/ branch-dest-pco 4))
	      ((#x-8000 #x7fff) (LONG ,@forward (16 offset signed)))
	      ((() ()) (LONG (32 "Can't branch tension @PCO operands")))))
	   ((,@match-phrase (@PCR (? branch-dest-pcr)))
	    (VARIABLE-WIDTH (offset `(/ (- ,branch-dest-pcr (+ *PC* 4)) 4))
	      ((#x-8000 #x7fff) (LONG ,@forward (16 offset signed)))
	      ((() ())
	       ;;         <reverse> xxx
	       ;;         LUI    $1,left_adj(branch-dest - 16)
	       ;;         BGEZAL $0,yyy
	       ;;         ADDIU  $1,$1,right(branch-dest - 16)
	       ;; yyy:    ADD    $1,$1,$31
	       ;;         JR     $1
	       ;;         ADD    $0,$0,$0
	       ;; xxx:
	       (LONG ,@reverse (16 6)	; reverse branch to (.+1)+6
	             (6 15)		; LUI
		     (5 0)
		     (5 1)
		     (16 (adjusted:high offset))
		     (6 1)		; BGEZAL
		     (5 0)
		     (5 17)
		     (16 1)
		     (6 9)		; ADDIU
		     (5 1)
		     (5 1)
		     (16 (adjusted:low offset) SIGNED)
		     (6 0)		; ADD
		     (5 1)
		     (5 31)
		     (5 1)
		     (5 0)
		     (6 32)
		     (6 0)		; JR
		     (5 1)
		     (15 0)
		     (6 8)
		     (6 0)		; ADD
		     (5 0)
		     (5 0)
		     (5 0)
		     (5 0)
		     (6 32)))))))))
  (branch bc0f () ((6 16) (10 #x100)) ((6 16) (10 #x101)))
  (branch bc1f () ((6 17) (10 #x100)) ((6 17) (10 #x101)))
  (branch bc2f () ((6 18) (10 #x100)) ((6 18) (10 #x101)))
  (branch bc3f () ((6 19) (10 #x100)) ((6 19) (10 #x101)))
  (branch bc0t () ((6 16) (10 #x101)) ((6 16) (10 #x100)))
  (branch bc1t () ((6 17) (10 #x101)) ((6 17) (10 #x100)))
  (branch bc2t () ((6 18) (10 #x101)) ((6 18) (10 #x100)))
  (branch bc3t () ((6 19) (10 #x101)) ((6 19) (10 #x100)))
  (branch beq ((? reg1) (? reg2))
	      ((6 4) (5 reg1) (5 reg2))
	      ((6 5) (5 reg1) (5 reg2)))
  (branch bgez ((? reg))
	       ((6 1) (5 reg) (5 1))
	       ((6 1) (5 reg) (5 0)))
  (branch bgezal ((? reg))
	         ((6 1) (5 reg) (5 17))
		 ((16 "can't branch tension a bgezal instruction")))
  (branch bgtz ((? reg))
	       ((6 7) (5 reg) (5 0))
	       ((6 6) (5 reg) (5 0)))
  (branch blez ((? reg))
	       ((6 6) (5 reg) (5 0))
	       ((6 7) (5 reg) (5 0)))
  (branch bltz ((? reg))
	       ((6 1) (5 reg) (5 0))
	       ((6 1) (5 reg) (5 1)))
  (branch bltzal ((? reg))
	         ((6 1) (5 reg) (5 16))
		 ((16 "can't branch tension a bltzal instruction")))
  (branch bne ((? reg1) (? reg2))
	      ((6 5) (5 reg1) (5 reg2))
	      ((6 4) (5 reg1) (5 reg2))))