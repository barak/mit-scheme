#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/sparc/instr2a.scm,v 1.1 1993/06/08 06:11:02 gjr Exp $

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

;;;; SPARC instruction set, part 2a

(declare (usual-integrations))

;;;; Instructions that require branch tensioning: branch

(let-syntax
    ((branch
      (macro (keyword annul condition)
	`(define-instruction ,keyword
	   (((@PCO (? offset)))
	    (LONG (2 0)
		  ,annul
		  ,condition
		  (3 2)
		  (22 (quotient offset 4) SIGNED)))
	   (((@PCR (? label)))
	    (VARIABLE-WIDTH (offset `(/ (- ,label (+ *PC* 0)) 4))
	      ((#x-400000 #x3fffff)
	       (LONG (2 0)
		     ,annul
		     ,condition
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
		     ,condition
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
		     ))))))))
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