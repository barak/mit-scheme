#| -*-Scheme-*-

$Id: instr2a.scm,v 1.4 2002/02/22 04:09:27 cph Exp $

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