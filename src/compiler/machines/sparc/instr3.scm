#| -*-Scheme-*-

$Id: ab022a55deff5b5a422d90c6534a3e4b69c38319 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; SPARC instruction set, part 3

(declare (usual-integrations))

(let-syntax
    ((float-instruction-3
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source1) (? source2))
	     (LONG (2 2)
		   (5 destination)
		   (6 ,(caddr form))
		   (5 source1)
		   (9 ,(cadddr form))
		   (5 source2))))))))
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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? source1) (? source2))
	     (LONG (2 2)
		   (5 0)
		   (6 ,(caddr form))
		   (5 source1)
		   (9 ,(cadddr form))
		   (5 source2))))))))
  (float-instruction-cmp fcmps 53 #x51)
  (float-instruction-cmp fcmpd 53 #x52)
  (float-instruction-cmp fcmpq 53 #x53)
  (float-instruction-cmp fcmpes 53 #x55)
  (float-instruction-cmp fcmped 53 #x56)
  (float-instruction-cmp fcmpeq 53 #x57))
  
(let-syntax
    ((float-instruction-2
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source))
	     (LONG (2 2)
		   (5 destination)
		   (6 ,(caddr form))
		   (5 0)
		   (9 ,(cadddr form))
		   (5 source))))))))
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