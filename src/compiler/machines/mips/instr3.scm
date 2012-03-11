#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; MIPS instruction set, part 3
;;; Floating point co-processor (R2010)

(declare (usual-integrations))

(let-syntax
    ((three-reg
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(BEGIN
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.S)
	      (((? fd) (? fs) (? ft))
	       (LONG (6 17)
		     (1 1)
		     (4 0)		; single precision
		     (5 ft)
		     (5 fs)
		     (5 fd)
		     (6 ,(caddr form)))))
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.D)
	      (((? fd) (? fs) (? ft))
	       (LONG (6 17)
		     (1 1)
		     (4 1)		; double precision
		     (5 ft)
		     (5 fs)
		     (5 fd)
		     (6 ,(caddr form))))))))))

  (three-reg add 0)
  (three-reg sub 1)
  (three-reg mul 2)
  (three-reg div 3))

(let-syntax
    ((two-reg
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(BEGIN
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.S)
	      (((? fd) (? fs))
	       (LONG (6 17)
		     (1 1)
		     (4 0)		; single precision
		     (5 0)
		     (5 fs)
		     (5 fd)
		     (6 ,(caddr form)))))
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.D)
	      (((? fd) (? fs))
	       (LONG (6 17)
		     (1 1)
		     (4 1)		; double precision
		     (5 0)
		     (5 fs)
		     (5 fd)
		     (6 ,(caddr form))))))))))
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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(BEGIN
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.S)
	      (((? fs) (? ft))
	       (LONG (6 17)
		     (1 1)
		     (4 0)
		     (5 ft)
		     (5 fs)
		     (5 0)
		     (6 ,(caddr form)))))
	    (DEFINE-INSTRUCTION ,(symbol-append (cadr form) '.D)
	      (((? fs) (? ft))
	       (LONG (6 17)
		     (1 1)
		     (4 1)
		     (5 ft)
		     (5 fs)
		     (5 0)
		     (6 ,(caddr form))))))))))
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