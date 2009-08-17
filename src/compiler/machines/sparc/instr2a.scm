#| -*-Scheme-*-

$Id: c9931c4ae6faa80b98c2bebd82eb97ba64867b9d $

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

;;;; SPARC instruction set, part 2a

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: branch

(let-syntax
    ((branch
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((@PCO (? offset)))
	     (LONG (2 0)
		   ,(caddr form)
		   ,(cadddr form)
		   (3 2)
		   (22 (quotient offset 4) SIGNED)))
	    (((@PCR (? label)))
	     (VARIABLE-WIDTH (offset `(/ (- ,label (+ *PC* 0)) 4))
	       ((#x-400000 #x3fffff)
		(LONG (2 0)
		      ,(caddr form)
		      ,(cadddr form)
		      (3 2)
		      (22 offset SIGNED)))
	       ((() ())
		;; B??a condition, yyy
		;; JMPL xxx, $0
		;; yyy: SETHI $1, high(offset)
		;; OR $1, $1, low(offset)
		;; JMPL $1,$0
		;; xxx: fall through
		(LONG (2 0)
		      (1 1)		; set anull bit, the JMPL is cancelled
					 ; on a taken branch
		      ,(cadddr form)
		      (3 2)
		      (22 2 SIGNED)	; B??condition, yyy
		      (2 2)
		      (5 0)
		      (6 #x38)
		      (5 0)
		      (1 1)
		      (13 16 SIGNED)	; JMPL xxx, $0
		      (2 0)
		      (5 1)
		      (3 4)
		      (22 (high-bits (* offset 4)) SIGNED)
					 ; SETHI $1, high22(offset)
		      (2 2)
		      (5 1)
		      (6 2)
		      (5 1)
		      (1 1)
		      (13 (low-bits (* offset 4)) SIGNED)
					 ; OR $1, $1, low10(offset)
		      (2 2)
		      (5 0)
		      (6 #x38)
		      (5 1)
		      (1 0)
		      (8 0)
		      (5 0)		; JMPL $1,$0
		      )))))))))
  (branch ba  (1 0) (4 8))
  (branch bn  (1 0) (4 0))
  (branch bne (1 0) (4 9))
  (branch be  (1 0) (4 1))
  (branch bg  (1 0) (4 10))
  (branch ble (1 0) (4 2))
  (branch bge (1 0) (4 11))
  (branch bl  (1 0) (4 3))
  (branch bgu (1 0) (4 12))
  (branch bleu (1 0) (4 4))
  (branch bcc (1 0) (4 13))
  (branch bcs (1 0) (4 5))
  (branch bpos (1 0) (4 14))
  (branch bneg (1 0) (4 6))
  (branch bvc  (1 0) (4 15))
  (branch bvs  (1 0) (4 7))
  )