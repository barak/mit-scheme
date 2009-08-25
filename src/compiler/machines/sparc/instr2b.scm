#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; SPARC instruction set, part 2b

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
	       ((#x-fff #xfff)
		(LONG (2 3)
		      (5 source/dest-reg)
		      (6 ,(caddr form))
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
		      (6 ,(caddr form))
		      (5 1)
		      (1 0)
		      (8 0)
		      (5 base-reg))))))))))
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