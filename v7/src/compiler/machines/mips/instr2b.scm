#| -*-Scheme-*-

$Id: instr2b.scm,v 1.6 2002/02/22 03:52:45 cph Exp $

Copyright (c) 1987-1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; MIPS instruction set, part 2b

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: load/store

(let-syntax
    ((load/store-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? source/dest-reg) (OFFSET (? offset-ls) (? base-reg)))
	     (VARIABLE-WIDTH (delta offset-ls)
	       ((#x-8000 #x7fff)
		(LONG (6 ,(caddr form))
		      (5 base-reg)
		      (5 source/dest-reg)
		      (16 delta SIGNED)))
	       ((() ())
		;; LUI    1,adjusted-left<offset>
		;; ADDU   1,1,base-reg
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
		      (6 ,(caddr form)); LW
		      (5 1)
		      (5 source/dest-reg)
		      (16 (adjusted:low delta) SIGNED))))))))))
  (load/store-instruction lb 32)
  (load/store-instruction lbu 36)
  (load/store-instruction lh 33)
  (load/store-instruction lhu 37)
  (load/store-instruction lw 35)
  ;; (load/store-instruction lwc0 48)
  (load/store-instruction lwc1 49)
  ;; (load/store-instruction lwc2 50)
  ;; (load/store-instruction lwc3 51)
  ;; (load/store-instruction lwl 34)
  ;; (load/store-instruction lwr 38)
  (load/store-instruction sb 40)
  (load/store-instruction sh 41)
  (load/store-instruction sw 43)
  ;; (load/store-instruction swc0 56)
  (load/store-instruction swc1 57)
  ;; (load/store-instruction swc2 58)
  ;; (load/store-instruction swc3 59)
  ;; (load/store-instruction swl 42)
  ;; (load/store-instruction swr 46)
  )