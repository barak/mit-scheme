#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/instr3.scm,v 1.1 1990/05/07 04:14:47 jinx Rel $

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

;;;; MIPS instruction set, part 3

(declare (usual-integrations))
;;;; Floating point co-processor (R2010)

(let-syntax
    ((three-reg
      (macro (keyword function-code)
	`(define-instruction ,keyword
	   ((SINGLE (? fd) (? fs) (? ft))
	    (LONG (6 17)
		  (1 1)
		  (4 0)	       ; single precision
		  (5 ft)
		  (5 fs)
		  (5 fd)
		  (6 ,function-code)))
	   ((DOUBLE (? fd) (? fs) (? ft))
	    (LONG (6 17)
		  (1 1)
		  (4 1)	       ; double precision
		  (5 ft)
		  (5 fs)
		  (5 fd)
		  (6 ,function-code))))))
     (two-reg
      (macro (keyword function-code)
	`(define-instruction ,keyword
	   ((SINGLE (? fd) (? fs))
	    (LONG (6 17)
		  (1 1)
		  (4 0)	       ; single precision
		  (5 0)
		  (5 fs)
		  (5 fd)
		  (6 ,function-code)))
	   ((DOUBLE (? fd) (? fs))
	    (LONG (6 17)
		  (1 1)
		  (4 1)	       ; double precision
		  (5 0)
		  (5 fs)
		  (5 fd)
		  (6 ,function-code))))))
     (compare
      (macro (keyword conditions)
	`(define-instruction ,keyword
	   ((SINGLE (? fs) (? ft))
	    (LONG (6 17)
		  (1 1)
		  (4 0)	       ; single precision
		  (5 ft)
		  (5 fs)
		  (5 0)
		  (6 ,conditions)))
	   ((DOUBLE (? fs) (? ft))
	    (LONG (6 17)
		  (1 1)
		  (4 1)	       ; double precision
		  (5 ft)
		  (5 fs)
		  (5 0)
		  (6 ,conditions)))))))

  (three-reg fadd 0)
  (three-reg fsub 1)
  (three-reg fmul 2)
  (three-reg fdiv 3)
  (two-reg fabs 5)
  (two-reg fmov 6)
  (two-reg fneg 7)
  (two-reg cvt.s 32)
  (two-reg cvt.d 33)
  (two-reg cvt.w 36)
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

