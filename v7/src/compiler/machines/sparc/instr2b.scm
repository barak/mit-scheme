#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/sparc/instr2b.scm,v 1.1 1993/06/08 06:11:02 gjr Exp $

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

;;;; SPARC instruction set, part 2b

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: load/store

(let-syntax
    ((load/store-instruction
      (macro (keyword opcode)
	`(define-instruction ,keyword
	   (((? source/dest-reg) (OFFSET (? offset-ls) (? base-reg)))
	    (VARIABLE-WIDTH (delta offset-ls)
              ((#x-fff #xfff)
	       (LONG (2 3)
		     (5 source/dest-reg)
		     (6 ,opcode)
		     (5 base-reg)
		     (1 1)
		     (13 delta SIGNED)))
	      ((() ())
	       ;; SETHI  1, %hi(offset)
	       ;; OR     1, 1, %lo(offset)
	       ;; LD     source/dest-reg,1,base-reg
	       (LONG (2 0)		; SETHI
		     (5 1)
		     (3 4)
		     (22 (high-bits delta))
		     
		     (2 2)		; OR
		     (5 1)
		     (6 2)
		     (5 1)
		     (1 1)
		     (13 (low-bits delta))

		     (2 3)		; LD
		     (5 source/dest-reg)
		     (6 ,opcode)
		     (5 1)
		     (1 0)
		     (8 0)
		     (5 base-reg)))))))))
  (load/store-instruction ldsb 9)
  (load/store-instruction ldsh 10)
  (load/store-instruction ldub 1)
  (load/store-instruction lduh 2)
  (load/store-instruction ld 0)
  (load/store-instruction ldd 3)
  (load/store-instruction stb 5)
  (load/store-instruction sth 6)
  (load/store-instruction st 4)
  (load/store-instruction std 7)
  (load/store-instruction ldf 32)
  (load/store-instruction lddf 35)
  (load/store-instruction ldfsr 33)
  (load/store-instruction stf 36)
  (load/store-instruction ltdf 39)
  (load/store-instruction stfsr 37)
  )
