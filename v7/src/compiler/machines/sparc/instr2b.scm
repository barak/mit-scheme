#| -*-Scheme-*-

$Id: instr2b.scm,v 1.3 2001/12/20 21:45:25 cph Exp $

Copyright (c) 1987-1999, 2001 Massachusetts Institute of Technology

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

;;;; SPARC instruction set, part 2b

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: load/store

(let-syntax
    ((load/store-instruction
      (lambda (keyword opcode)
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
