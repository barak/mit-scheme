#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/instr2b.scm,v 1.2 1991/07/21 07:41:51 cph Exp $

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

;;;; MIPS instruction set, part 2b

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: load/store

(let-syntax
    ((load/store-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? source/dest-reg) (OFFSET (? offset-ls) (? base-reg)))
	    (VARIABLE-WIDTH (delta offset-ls)
              ((#x-8000 #x7fff)
	       (LONG (6 ,opcode)
		     (5 base-reg)
		     (5 source/dest-reg)
		     (16 delta SIGNED)))
	      ((() ())
	       ;; LUI    1,adjusted-left<offset>
	       ;; ADDU    1,1,base-reg
	       ;; LW     source/dest-reg,right<offset>(1)
	       (LONG (6 15)	; LUI
		     (5 0)
		     (5 1)
		     (16 (adjusted:high delta))
		     (6 0)	; ADD
		     (5 1)
		     (5 base-reg)
		     (5 1)
		     (5 0)
		     (6 32)
		     (6 ,opcode); LW
		     (5 1)
		     (5 source/dest-reg)
		     (16 (adjusted:low delta) SIGNED)))))
	   (((? source/dest-reg) (@PCR (? label)))
	    (VARIABLE-WIDTH (delta `(- ,label (+ *PC* 8)))
	      ((#x-8000 #x7fff)
	       ; 	BGEZAL 0,X
	       ; 	LW source/dest-reg,delta(31)
	       ; X:
	       (LONG (6 1)		; BGEZAL
		     (5 0)
		     (5 17)
		     (16 1)
		     (6 ,opcode)	; LW
		     (5 31)
		     (5 source/dest-reg)
		     (16 delta)))
	      ((() ())
	      ;		BGEZAL	0,X
	      ;		LUI	1,upper-half-adjusted
	      ;	X:	ADDU	1,31,1
	      ;		LW	source/dest-reg,lower-half(1)
	       (LONG (6 1)		; BGEZAL
		     (5 0)
		     (5 17)
		     (16 1)
		     (6 15)		; LUI
		     (5 0)
		     (5 1)
		     (16 (adjusted:high delta))
		     (6 0)		; ADDU
		     (5 1)
		     (5 31)
		     (5 1)
		     (5 0)
		     (6 33)
		     (6 ,opcode)	; LW
		     (5 1)
		     (5 source/dest-reg)
		     (16 (adjusted:low delta) SIGNED)))))))))
  (load/store-instruction lb 32)
  (load/store-instruction lbu 36)
  (load/store-instruction lh 33)
  (load/store-instruction lhu 37)
  (load/store-instruction lw 35)
  (load/store-instruction lwc0 48)
  (load/store-instruction lwc1 49)
  (load/store-instruction lwc2 50)
  (load/store-instruction lwc3 51)
  (load/store-instruction lwl 34)
  (load/store-instruction lwr 38)
  (load/store-instruction sb 40)
  (load/store-instruction sh 41)
  (load/store-instruction sw 43)
  (load/store-instruction swc0 56)
  (load/store-instruction swc1 57)
  (load/store-instruction swc2 58)
  (load/store-instruction swc3 59)
  (load/store-instruction swl 42)
  (load/store-instruction swr 46))
