#| -*-Scheme-*-

$Id: instr3.scm,v 1.4 2001/12/20 21:45:25 cph Exp $

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

;;;; MIPS instruction set, part 3
;;; Floating point co-processor (R2010)

(declare (usual-integrations))

(let-syntax
    ((three-reg
      (lambda (keyword function-code)
	`(BEGIN
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.S)
	     (((? fd) (? fs) (? ft))
	      (LONG (6 17)
		    (1 1)
		    (4 0)		; single precision
		    (5 ft)
		    (5 fs)
		    (5 fd)
		    (6 ,function-code))))
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.D)
	     (((? fd) (? fs) (? ft))
	      (LONG (6 17)
		    (1 1)
		    (4 1)		; double precision
		    (5 ft)
		    (5 fs)
		    (5 fd)
		    (6 ,function-code))))))))

  (three-reg add 0)
  (three-reg sub 1)
  (three-reg mul 2)
  (three-reg div 3))

(let-syntax
    ((two-reg
      (lambda (keyword function-code)
	`(BEGIN
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.S)
	     (((? fd) (? fs))
	      (LONG (6 17)
		    (1 1)
		    (4 0)		; single precision
		    (5 0)
		    (5 fs)
		    (5 fd)
		    (6 ,function-code))))
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.D)
	     (((? fd) (? fs))
	      (LONG (6 17)
		    (1 1)
		    (4 1)		; double precision
		    (5 0)
		    (5 fs)
		    (5 fd)
		    (6 ,function-code))))))))
  (two-reg abs 5)
  (two-reg mov 6)
  (two-reg neg 7))

(define-instruction cvt.d.s
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 0)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 33))))

(define-instruction cvt.d.w
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 4)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 33))))

(define-instruction cvt.s.d
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 1)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 32))))

(define-instruction cvt.s.w
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 4)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 32))))

(define-instruction cvt.w.d
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 1)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 36))))

(define-instruction cvt.w.s
  (((? fd) (? fs))
   (LONG (6 17)
	 (1 1)
	 (4 0)
	 (5 0)
	 (5 fs)
	 (5 fd)
	 (6 36))))

(let-syntax
    ((compare
      (lambda (keyword conditions)
	`(BEGIN
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.S)
	     (((? fs) (? ft))
	      (LONG (6 17)
		    (1 1)
		    (4 0)
		    (5 ft)
		    (5 fs)
		    (5 0)
		    (6 ,conditions))))
	   (DEFINE-INSTRUCTION ,(symbol-append keyword '.D)
	     (((? fs) (? ft))
	      (LONG (6 17)
		    (1 1)
		    (4 1)
		    (5 ft)
		    (5 fs)
		    (5 0)
		    (6 ,conditions))))))))
  (compare c.f 48)
  (compare c.un 49)
  (compare c.eq 50)
  (compare c.ueq 51)
  (compare c.olt 52)
  (compare c.ult 53)
  (compare c.ole 54)
  (compare c.ule 55)
  (compare c.sf 56)
  (compare c.ngle 57)
  (compare c.seq 58)
  (compare c.ngl 59)
  (compare c.lt 60)
  (compare c.nge 61)
  (compare c.le 62)
  (compare c.ngt 63))