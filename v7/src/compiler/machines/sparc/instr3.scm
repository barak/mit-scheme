#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/sparc/instr3.scm,v 1.1 1993/06/08 06:11:02 gjr Exp $

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

;;;; SPARC instruction set, part 3

(declare (usual-integrations))

(let-syntax
    ((float-instruction-3
      (macro (keyword major minor)
	`(define-instruction ,keyword
	   (((? destination) (? source1) (? source2))
	    (LONG (2 2)
		  (5 destination)
		  (6 ,major)
		  (5 source1)
		  (9 ,minor)
		  (5 source2)))))))
  (float-instruction-3 fadds 52 65)
  (float-instruction-3 faddd 52 66)
  (float-instruction-3 faddq 52 67)
  (float-instruction-3 fsubs 52 69)
  (float-instruction-3 fsubd 52 70)
  (float-instruction-3 fsubq 52 71)
  (float-instruction-3 fmuls 52 73)
  (float-instruction-3 fmuld 52 74)
  (float-instruction-3 fmulq 52 75)
  (float-instruction-3 fsmuld 52 #x69)
  (float-instruction-3 fdmulq 52 #x6e)
  (float-instruction-3 fdivs 52 #x4d)
  (float-instruction-3 fdivd 52 #x4e)
  (float-instruction-3 fdivq 52 #x4f))

(let-syntax
    ((float-instruction-cmp
      (macro (keyword major minor)
	`(define-instruction ,keyword
	   (((? source1) (? source2))
	    (LONG (2 2)
		  (5 0)
		  (6 ,major)
		  (5 source1)
		  (9 ,minor)
		  (5 source2)))))))
  (float-instruction-cmp fcmps 53 #x51)
  (float-instruction-cmp fcmpd 53 #x52)
  (float-instruction-cmp fcmpq 53 #x53)
  (float-instruction-cmp fcmpes 53 #x55)
  (float-instruction-cmp fcmped 53 #x56)
  (float-instruction-cmp fcmpeq 53 #x57))
  
(let-syntax
    ((float-instruction-2
      (macro (keyword major minor)
	`(define-instruction ,keyword
	   (((? destination) (? source))
	    (LONG (2 2)
		  (5 destination)
		  (6 ,major)
		  (5 0)
		  (9 ,minor)
		  (5 source)))))))
  (float-instruction-2 fsqrts #x34 #x29)
  (float-instruction-2 fsqrtd #x34 #x2a)
  (float-instruction-2 fsqrtq #x34 #x2b)

  (float-instruction-2 fmovs #x34 #x01)
  (float-instruction-2 fnegs #x34 #x05)
  (float-instruction-2 fabss #x34 #x09)
  
  (float-instruction-2 fstoi #x34 #xd1)
  (float-instruction-2 fdtoi #x34 #xd2)
  (float-instruction-2 fqtoi #x34 #xd3)

  (float-instruction-2 fitos #x34 #xc4)
  (float-instruction-2 fitod #x34 #xc8)
  (float-instruction-2 fitoq #x34 #xcc)
    
  (float-instruction-2 fstod #x34 #xc9)
  (float-instruction-2 fstoq #x34 #xcd)
  
  (float-instruction-2 fdtos #x34 #xc6)
  (float-instruction-2 fstod #x34 #xce)
  
  (float-instruction-2 fstod #x34 #xc7)
  (float-instruction-2 fstod #x34 #xcb))
  
  
  