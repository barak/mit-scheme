#| -*-Scheme-*-

$Id: instr3.scm,v 1.1 1992/08/29 13:51:25 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; Alpha instruction set, part 3
;;; Floating point instructions
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define (encode-fp-qualifier qualifier)
  (define (translate symbol)
    (case symbol
      ((C) #x-080)	; Chopped (round toward 0)
      ((M) #x-040)	; Round to minus infinity
      ((D)  #x040)	; Round from state bits (dynamic)
      ((U)  #x100)	; Underflow enabled
      ((V)  #x100)	; Integer overflow enabled (CVTTQ only)
      ((I)  #x200)	; Inexact enabled
      ((S)  #x400)	; Software
      (else (error "ENCODE-FP-QUALIFIER: unknown qualifier" symbol))))
  (if (symbol? qualifier)
      (translate qualifier)
      (apply + (map translate qualifier))))

(let-syntax
    ((floating-operate
      (macro (keyword function-code)
	`(define-instruction ,keyword
	   (((? src-1) (? src-2) (? dest))
	    (LONG (6 #x17)		; Opcode
		  (5 src-1)
		  (5 src-2)
		  (11 ,function-code)
		  (5 dest)))))))
  (floating-operate CPYS #x20)
  (floating-operate CPYSE #x22)
  (floating-operate CPYSN #x21)
  (floating-operate CVTLQ #x10)
  (floating-operate CVTQL #x30)
  (floating-operate CVTQLSV #x330)
  (floating-operate CVTQLV #x130)
  (floating-operate FCMOVEQ #x2a)
  (floating-operate FCMOVGE #x2d)
  (floating-operate FCMOVGT #x2f)
  (floating-operate FCMOVLE #x2e)
  (floating-operate FCMOVLT #x2c)
  (floating-operate FCMOVNE #x2b)
  (floating-operate MF_FPCR #x25)
  (floating-operate MT_FPCR #x24))

(let-syntax
    ((ieee
      (macro (keyword function-code)
	`(define-instruction ,keyword
	   (((? src-1) (? src-2) (? dest))
	    (LONG (6 #x16)		; Opcode
		  (5 src-1)
		  (5 src-2)
		  (11 ,function-code)
		  (5 dest)))
	   ((/ (? qualifier) (? src-1) (? src-2) (? dest))
	    (LONG (6 #x16)		; Opcode
		  (5 src-1)
		  (5 src-2)
		  (11 (+ ,function-code (encode-fp-qualifier qualifier)))
		  (5 dest)))))))
  (ieee ADDS #x80)
  (ieee ADDT #xA0)
  (ieee CMPTEQ #xA5)
  (ieee CMPTLE #xA7)
  (ieee CMPTLT #xA6)
  (ieee CMPTUN #xA4)
  (ieee CVTQS #xBC)
  (ieee CVTQT #xBE)
  (ieee CVTTQ #xAF)
  (ieee CVTTS #xAC)
  (ieee DIVS #x83)
  (ieee DIVT #xA3)
  (ieee MULS #x82)
  (ieee MULT #xA2)
  (ieee SUBS #x81)
  (ieee SUBT #xA1))

(let-syntax
    ((vax
      (macro (keyword function-code)
	`(define-instruction ,keyword
	   (((? src-1) (? src-2) (? dest))
	    (LONG (6 #x15)		; Opcode
		  (5 src-1)
		  (5 src-2)
		  (11 ,function-code)
		  (5 dest)))
	   ((/ (? qualifier) (? src-1) (? src-2) (? dest))
	    (LONG (6 #x15)		; Opcode
		  (5 src-1)
		  (5 src-2)
		  (11 (+ ,function-code (encode-fp-qualifier qualifier)))
		  (5 dest)))))))
  (vax ADDF #x80)
  (vax ADDG #xa0)
  (vax CMPGEQ #xa5)
  (vax CMPGLE #xa7)
  (vax CMPGLT #xa6)
  (vax CVTDG #x9e)
  (vax CVTGD #xad)
  (vax CVTGF #xac)
  (vax CVTGQ #xaf)
  (vax CVTQF #xbc)
  (vax CVTQG #xbe)
  (vax DIVF #x83)
  (vax DIVG #xa3)
  (vax MULF #xb2)
  (vax MULG #x81)
  (vax SUBF #x81)
  (vax SUBG #xa1))
