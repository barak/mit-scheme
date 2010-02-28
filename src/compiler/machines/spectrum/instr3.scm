#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; HP Spectrum Instruction Set Description
;;; Originally from Walt Hill, who did the hard part.

(declare (usual-integrations))

;;;; Computation instructions

(let-syntax ((arith-logical
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl complal) (? source-reg1) (? source-reg2)
					(? target-reg))
		     (LONG (6 #x02)
			   (5 source-reg2)
			   (5 source-reg1)
			   (3 (car compl))
			   (1 (cadr compl))
			   (7 ,(caddr form))
			   (5 target-reg))))))))

  (arith-logical ANDCM    #x00)
  (arith-logical AND      #x10)
  (arith-logical OR       #x12)
  (arith-logical XOR      #x14)
  (arith-logical UXOR     #x1c)
  (arith-logical SUB      #x20)
  (arith-logical DS       #x22)
  (arith-logical SUBT     #x26)
  (arith-logical SUBB     #x28)
  (arith-logical ADD      #x30)
  (arith-logical SH1ADD   #x32)
  (arith-logical SH2ADD   #x34)
  (arith-logical SH3ADD   #x36)
  (arith-logical ADDC     #x38)
  (arith-logical COMCLR   #x44)
  (arith-logical UADDCM   #x4c)
  (arith-logical UADDCMT  #x4e)
  (arith-logical ADDL     #x50)
  (arith-logical SH1ADDL  #x52)
  (arith-logical SH2ADDL  #x54)
  (arith-logical SH3ADDL  #x56)
  (arith-logical SUBO     #x60)
  (arith-logical SUBTO    #x66)
  (arith-logical SUBBO    #x68)
  (arith-logical ADDO     #x70)
  (arith-logical SH1ADDO  #x72)
  (arith-logical SH2ADDO  #x74)
  (arith-logical SH3ADDO  #x76)
  (arith-logical ADDCO    #x78))

;; WH Maybe someday. (Spec-DefOpcode DCOR    2048 DecimalCorrect)        % 02
;;                   (Spec-DefOpcode IDCOR   2048 DecimalCorrect)        % 02

;;;; Assembler pseudo-ops

(define-instruction NOP			; pseudo-op: (OR complt 0 0 0)
  (((? compl complal))
   (LONG (6 #x02)
	 (10 #b0000000000)
	 (3 (car compl))
	 (1 (cadr compl))
	 (7 #x12)
	 (5 #b00000))))

(define-instruction COPY		; pseudo-op (OR complt 0 s t)
  (((? compl complal) (? source-reg) (? target-reg))
   (LONG (6 #x02)
	 (5 #b00000)
	 (5 source-reg)
	 (3 (car compl))
	 (1 (cadr compl))
	 (7 #x12)
	 (5 target-reg))))

(define-instruction SKIP		; pseudo-op (ADD complt 0 0 0)
  (((? compl complal))
   (LONG (6 #x02)
	 (10 #b0000000000)
	 (3 (car compl))
	 (1 (cadr compl))
	 (7 #x30)
	 (5 #b00000))))

(let-syntax ((immed-arith
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl complal) (? immed-11) (? source-reg)
					(? target-reg))
		     (LONG (6 ,(caddr form))
			   (5 source-reg)
			   (5 target-reg)
			   (3 (car compl))
			   (1 (cadr compl))
			   (1 ,(cadddr form))
			   (11 immed-11 RIGHT-SIGNED))))))))
  (immed-arith ADDI    #x2d 0)
  (immed-arith ADDIO   #x2d 1)
  (immed-arith ADDIT   #x2c 0)
  (immed-arith ADDITO  #x2c 1)
  (immed-arith SUBI    #x25 0)
  (immed-arith SUBIO   #x25 1)
  (immed-arith COMICLR #x24 0))

(define-instruction VSHD
  (((? compl compled) (? source-reg1) (? source-reg2)
		      (? target-reg))
   (LONG (6 #x34)
	 (5 source-reg2)
	 (5 source-reg1)
	 (3 compl)
	 (3 0)
	 (5 #b00000)
	 (5 target-reg))))

(define-instruction SHD
  (((? compl compled) (? source-reg1) (? source-reg2) (? pos)
		      (? target-reg))
   (LONG (6 #x34)
	 (5 source-reg2)
	 (5 source-reg1)
	 (3 compl)
	 (3 2)
	 (5 (- 31 pos))
	 (5 target-reg))))

(let-syntax ((extr
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? source-reg) (? pos) (? len)
					(? target-reg))
		     (LONG (6 #x34)
			   (5 source-reg)
			   (5 target-reg)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 pos)
			   (5 (- 32 len))))))))
	     (vextr
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? source-reg) (? len)
					(? target-reg))
		     (LONG (6 #x34)
			   (5 source-reg)
			   (5 target-reg)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 #b00000)
			   (5 (- 32 len)))))))))
  (extr  EXTRU  6)
  (extr  EXTRS  7)
  (vextr VEXTRU 4)
  (vextr VEXTRS 5))

(let-syntax ((depos
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? source-reg) (? pos) (? len)
					(? target-reg))
		     (LONG (6 #x35)
			   (5 target-reg)
			   (5 source-reg)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 (- 31 pos))
			   (5 (- 32 len))))))))
	     (vdepos
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? source-reg) (? len)
					(? target-reg))
		     (LONG (6 #x35)
			   (5 target-reg)
			   (5 source-reg)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 #b00000)
			   (5 (- 32 len))))))))
	     (idepos
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? immed) (? pos) (? len)
					(? target-reg))
		     (LONG (6 #x35)
			   (5 target-reg)
			   (5 immed RIGHT-SIGNED)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 (- 31 pos))
			   (5 (- 32 len))))))))
	     (videpos
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    (((? compl compled) (? immed) (? len)
					(? target-reg))
		     (LONG (6 #x35)
			   (5 target-reg)
			   (5 immed RIGHT-SIGNED)
			   (3 compl)
			   (3 ,(caddr form))
			   (5 #b00000)
			   (5 (- 32 len)))))))))

  (idepos  DEPI   7)
  (idepos  ZDEPI  6)
  (videpos VDEPI  5)
  (videpos ZVDEPI 4)
  (depos   DEP    3)
  (depos   ZDEP   2)
  (vdepos  VDEP   1)
  (vdepos  ZVDEP  0))

(let-syntax ((Probe-Read-Write
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((() (OFFSET 0 (? space) (? base)) (? priv-reg)
			 (? target-reg))
		     (LONG (6 1)
			   (5 base)
			   (5 priv-reg)
			   (2 space)
			   (8 ,(caddr form))
			   (1 #b0)
			   (5 target-reg))))))))
  (Probe-Read-Write PROBER  #x46)
  (Probe-Read-Write PROBEW  #x47)
  (Probe-Read-Write PROBERI #xc6)
  (Probe-Read-Write PROBEWI #xc7))

(define-instruction BREAK
  ((() (? immed-5) (? immed-13))
   (LONG (6 #b000000)
	 (13 immed-13)
	 (8 #b00000000)
	 (5 immed-5))))

(define-instruction LDSID
  ((() (OFFSET 0 (? space) (? base)) (? target-reg))
   (LONG (6 #b000000)
	 (5 base)
	 (5 #b00000)
	 (2 space)
	 (1 #b0)
	 (8 #x85)
	 (5 target-reg))))

(define-instruction MTSP
  ((() (? source-reg) (? space-reg sr3))
   (LONG (6 #b000000)
	 (5 #b00000)
	 (5 source-reg)
	 (3 space-reg)
	 (8 #xc1)
	 (5 #b00000))))

(define-instruction MTCTL
  ((() (? source-reg) (? control-reg))
   (LONG (6 #b000000)
	 (5 control-reg)
	 (5 source-reg)
	 (3 #b000)
	 (8 #xc2)
	 (5 #b00000))))

(define-instruction MTSAR		; pseudo-oop (MTCLT () source 11)
  ((() (? source-reg))
   (LONG (6 #b000000)
	 (5 #x0b)
	 (5 source-reg)
	 (3 #b000)
	 (8 #xc2)
	 (5 #b00000))))

(define-instruction MFSP
  ((() (? space-reg sr3) (? target-reg))
   (LONG (16 #b0000000000000000)
	 (3 space-reg)
	 (8 #x25)
	 (5 target-reg))))

(define-instruction MFCTL
  ((() (? control-reg) (? target-reg))
   (LONG (6 #b000000)
	 (5 control-reg)
	 (5 #b00000)
	 (3 #b000)
	 (8 #x45)
	 (5 target-reg))))

(define-instruction SYNC
  ((())
   (LONG (16 #b0000000000000000)
	 (3 #b000)
	 (8 #x20)
	 (5 #b00000))))

#|
Missing:

LPA
LHA
PDTLB
PITLB
PDTLBE
PITLBE
IDTLBA
IITLBA
IDTLBP
IITLBP
DIAG

|#

(let-syntax ((floatarith-1
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((((? fmt fpformat)) (? source-reg) (? target-reg))
		     (LONG (6 #x0c)
			   (5 source-reg)
			   (5 #b00000)
			   (3 ,(caddr form))
			   (2 fmt)
			   (2 ,(cadddr form))
			   (4 #b0000)
			   (5 target-reg)))))))
	     (floatarith-2
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((((? fmt fpformat)) (? source-reg1) (? source-reg2)
					 (? target-reg))
		     (LONG (6 #x0c)
			   (5 source-reg1)
			   (5 source-reg2)
			   (3 ,(caddr form))
			   (2 fmt)
			   (2 ,(cadddr form))
			   (4 #b0000)
			   (5 target-reg))))))))

  (floatarith-2 FADD   0 3)
  (floatarith-2 FSUB   1 3)
  (floatarith-2 FMPY   2 3)
  (floatarith-2 FDIV   3 3)
  (floatarith-1 FSQRT  4 0)
  (floatarith-1 FABS   3 0)
  (floatarith-2 FREM   4 3)
  (floatarith-1 FRND   5 0)
  (floatarith-1 FCPY   2 0))

(define-instruction FCMP
  ((((? condition fpcond) (? fmt fpformat)) (? reg1) (? reg2))
   (LONG (6 #x0c)
	 (5 reg1)
	 (5 reg2)
	 (3 #b000)
	 (2 fmt)
	 (6 #b100000)
	 (5 condition))))

(let-syntax ((fpconvert
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((((? sf fpformat) (? df fpformat))
		      (? source-reg1)
		      (? reg-t))
		     (LONG (6 #x0c)
			   (5 source-reg1)
			   (4 #b0000)
			   (2 ,(caddr form))
			   (2 df)
			   (2 sf)
			   (6 #b010000)
			   (5 reg-t))))))))
  (fpconvert FCNVFF  0)
  (fpconvert FCNVFX  1)
  (fpconvert FCNVXF  2)
  (fpconvert FCNVFXT 3))

(define-instruction FTEST
  ((())
   (LONG (6 #x0c)
	 (10 #b0000000000)
	 (16 #b0010010000100000))))

#|
;; What SFU is this? -- Jinx

;;  WARNING  The SFU instruction code below should be
;;	     tested before use.    WLH  11/18/86

(let-syntax ((multdiv
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((() (? reg-1) (? reg-2))
		     (LONG (6 #x04)
			   (5 reg-2)
			   (5 reg-1)
			   (5 ,(caddr form))
			   (11 #b11000000000))))))))
  (multdiv MPYS    #x08)
  (multdiv MPYU    #x0a)
  (multdiv MPYSCV  #x0c)
  (multdiv MPYUCV  #x0e)
  (multdiv MPYACCS #x0d)
  (multdiv MPYACCU #x0f)
  (multdiv DIVSIR  #x00)
  (multdiv DIVSFR  #x04)
  (multdiv DIVUIR  #x03)
  (multdiv DIVUFR  #x07)
  (multdiv DIVSIM  #x01)
  (multdiv DIVSFM  #x05)
  (multdiv MDRR    #x06))

(define-instruction MDRO
  ((() (? reg))
   (LONG (6 #x04)
	 (5 reg)
	 (5 #b00000)
	 (16 #b1000000000000000))))

(let-syntax ((multdivresult
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 `(DEFINE-INSTRUCTION ,(cadr form)
		    ((() (? reg-t))
		     (LONG (6 #x04)
			   (10 #b0000000000)
			   (5 ,(caddr form))
			   (5 #b01000)
			   (1 ,(cadddr form))
			   (5 reg-t))))))))
  (multdivresult MDLO    4 0)
  (multdivresult MDLNV   4 1)
  (multdivresult MDLV    5 1)
  (multdivresult MDL     5 0)
  (multdivresult MDHO    6 0)
  (multdivresult MDHNV   6 1)
  (multdivresult MDHV    7 1)
  (multdivresult MDH     7 0)
  (multdivresult MDSFUID 0 0))
|#