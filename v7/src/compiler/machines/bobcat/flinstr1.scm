#| -*-Scheme-*-

$Id: flinstr1.scm,v 1.5 2002/11/20 19:45:51 cph Exp $

Copyright (c) 1988-1989, 1999, 2001-2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Instruction set description for 68881 floating point processor
;;; Originally provided courtesy of BBN ACI.

;;; These instructions are not handled: FDBcc FMOVECR FMOVEM FNOP FRESTORE FSAVE
;;; FScc FSINCOS FTRAPcc

(declare (usual-integrations))

(define FPC #b001)			; Floating point chip identifier for
					;   coprocessor instructions.
(define-symbol-transformer
  float-source-format
  (L . 0)				; long word integer
  (S . 1)				; single precision real
  (X . 2)				; extended precision real
  (P . 3)				; packed decimal real
  (W . 4)				; word integer
  (D . 5)				; double precision real
  (B . 6))				; byte integer

(define-symbol-transformer
  float-destination-format
  (L . 0)				; long word integer
  (S . 1)				; single precision real
  (X . 2)				; extended precision real
  (W . 4)				; word integer
  (D . 5)				; double precision real
  (B . 6))				; byte integer

(define-symbol-transformer float-reg
  (FP0 . 0) (FP1 . 1) (FP2 . 2) (FP3 . 3)
  (FP4 . 4) (FP5 . 5) (FP6 . 6) (FP7 . 7))

(define-symbol-transformer float-ctl-reg
  (FPCR . 4) (FPSR . 2) (FPIAR 1))

(define-symbol-transformer float-cc
  (EQ . 1) (NE . 14) (GT . 2) (NGT . 13)
  (GE . 3) (NGE . 12) (LT . 4) (NLT . 11)
  (LE . 5) (NLE . 10) (GL . 6) (NGL . 9)
  (MI . 4) (PL . 3)
  (GLE . 7) (NGLE . 8) (F . 0) (T . 15))

(define-instruction FMOVE

  (((? type float-source-format) (? source ea-d) (? destination float-reg))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 source SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b010)
		   (3 type)
		   (3 destination)
		   (7 #b0000000)))
  
  (((? source float-reg) (? destination float-reg))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 #b000000))
   (EXTENSION-WORD (3 #b000)
		   (3 source)
		   (3 destination)
		   (7 #b0000000)))

  (((? type float-destination-format)
    (? source float-reg)
    (? destination ea-d&a))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 destination DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 type)
		   (3 source)
		   (7 #b0000000)))

  (((P (? k-factor)) (? source float-reg) (? destination ea-d&a))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 destination DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 #b011)
		   (3 source)
		   (7 k-factor)))

  (((PD (? k-reg)) (? source float-reg) (? destination ea-d&a))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 destination DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b011)
		   (3 #b111)
		   (3 source)
		   (3 k-reg)
		   (4 #b0000)))

  ((L (? source ea-d) (? destination float-ctl-reg))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 source SOURCE-EA 'L))
   (EXTENSION-WORD (3 #b100)
		   (3 destination)
		   (10 #b0000000000)))

  ((L (? source float-ctl-reg) (? destination ea-d))
   (WORD (4 #b1111)
	 (3 FPC)
	 (3 #b000)
	 (6 destination DESTINATION-EA 'L))
   (EXTENSION-WORD (3 #b101)
		   (3 source)
		   (10 #b0000000000))))

(let-syntax
    ((define-unary-flop
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)

	     (((? type float-source-format)
	       (? source ea-d)
	       (? destination float-reg))
	      (WORD (4 #b1111)
		    (3 FPC)
		    (3 #b000)
		    (6 source SOURCE-EA 'L))
	      (EXTENSION-WORD (3 #b010)
			      (3 type)
			      (3 destination)
			      (7 ,(caddr form))))

	     (((? source float-reg) (? destination float-reg))
	      (WORD (4 #b1111)
		    (3 FPC)
		    (3 #b000)
		    (6 #b000000))
	      (EXTENSION-WORD (3 #b000)
			      (3 source)
			      (3 destination)
			      (7 ,(caddr form))))

	     (((? reg float-reg))
	      (WORD (4 #b1111)
		    (3 FPC)
		    (3 #b000)
		    (6 #b000000))
	      (EXTENSION-WORD (3 #b000)
			      (3 reg)
			      (3 reg)
			      (7 ,(caddr form)))))))))
  (define-unary-flop FABS	#b0011000)
  (define-unary-flop FACOS	#b0011100)
  (define-unary-flop FASIN	#b0001100)
  (define-unary-flop FATAN	#b0001010)
  (define-unary-flop FATANH	#b0001101)
  (define-unary-flop FCOS	#b0011101)
  (define-unary-flop FCOSH	#b0011001)
  (define-unary-flop FETOX	#b0010000)
  (define-unary-flop FETOXM1	#b0001000)
  (define-unary-flop FGETEXP	#b0011110)
  (define-unary-flop FGETMAN	#b0011111)
  (define-unary-flop FINT	#b0000001)
  (define-unary-flop FINTRZ	#b0000011)
  (define-unary-flop FLOG10	#b0010101)
  (define-unary-flop FLOG2	#b0010110)
  (define-unary-flop FLOGN	#b0010100)
  (define-unary-flop FLOGNP1	#b0000110)
  (define-unary-flop FNEG	#b0011010)
  (define-unary-flop FSIN	#b0001110)
  (define-unary-flop FSINH	#b0000010)
  (define-unary-flop FSQRT	#b0000100)
  (define-unary-flop FTAN	#b0001111)
  (define-unary-flop FTANH	#b0001001)
  (define-unary-flop FTENTOX	#b0010010)
  (define-unary-flop FTWOTOX	#b0010001))