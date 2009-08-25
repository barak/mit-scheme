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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? src-1) (? src-2) (? dest))
	     (LONG (6 #x17)		; Opcode
		   (5 src-1)
		   (5 src-2)
		   (11 ,(caddr form))
		   (5 dest))))))))
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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? src-1) (? src-2) (? dest))
	     (LONG (6 #x16)		; Opcode
		   (5 src-1)
		   (5 src-2)
		   (11 ,(caddr form))
		   (5 dest)))
	    ((/ (? qualifier) (? src-1) (? src-2) (? dest))
	     (LONG (6 #x16)		; Opcode
		   (5 src-1)
		   (5 src-2)
		   (11 (+ ,(caddr form) (encode-fp-qualifier qualifier)))
		   (5 dest))))))))
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
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? src-1) (? src-2) (? dest))
	     (LONG (6 #x15)		; Opcode
		   (5 src-1)
		   (5 src-2)
		   (11 ,(caddr form))
		   (5 dest)))
	    ((/ (? qualifier) (? src-1) (? src-2) (? dest))
	     (LONG (6 #x15)		; Opcode
		   (5 src-1)
		   (5 src-2)
		   (11 (+ ,(caddr form) (encode-fp-qualifier qualifier)))
		   (5 dest))))))))
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