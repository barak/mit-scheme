#| -*-Scheme-*-

$Id: instr2.scm,v 1.8 2001/12/20 20:51:15 cph Exp $

Copyright (c) 1987-1999, 2001 Massachusetts Institute of Technology

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

;;;; HP Spectrum Instruction Set Description
;;; Originally from Walt Hill, who did the hard part.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Memory and offset operations

;;; The long forms of many of the following instructions use register
;;; 1 -- this may be inappropriate for assembly-language programs, but
;;; is OK for the output of the compiler.
(let-syntax ((long-load
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (OFFSET (? offset) (? space) (? base)) (? reg))
		    (VARIABLE-WIDTH (disp offset)
		      ((#x-2000 #x1FFF)
		       (LONG (6 ,opcode)
			     (5 base)
			     (5 reg)
			     (2 space)
			     (14 disp RIGHT-SIGNED)))
		      ((() ())
		       (LONG
			;; (ADDIL () L$,offset ,base)
			(6 #x0A)
			(5 base)
			(21 (quotient disp #x800) ASSEMBLE21:X)
			;; (LDW () (OFFSET R$,offset ,space 1) ,reg)
			(6 ,opcode)
			(5 1)
			(5 reg)
			(2 space)
			(14 (remainder disp #x800) RIGHT-SIGNED))))))))

	     (long-store
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (? reg) (OFFSET (? offset) (? space) (? base)))
		    (VARIABLE-WIDTH (disp offset)
		      ((#x-2000 #x1FFF)
		       (LONG (6 ,opcode)
			     (5 base)
			     (5 reg)
			     (2 space)
			     (14 disp RIGHT-SIGNED)))
		      ((() ())
		       (LONG
			;; (ADDIL () L$,offset ,base)
			(6 #x0A)
			(5 base)
			(21 (quotient disp #x800) ASSEMBLE21:X)
			;; (STW () ,reg (OFFSET R$,offset ,space 1))
			(6 ,opcode)
			(5 1)
			(5 reg)
			(2 space)
			(14 (remainder disp #x800) RIGHT-SIGNED))))))))

	     (load-offset
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (OFFSET (? offset) 0 (? base)) (? reg))
		    (VARIABLE-WIDTH (disp offset)
		      ((#x-2000 #x1FFF)
		       (LONG (6 ,opcode)
			     (5 base)
			     (5 reg)
			     (2 #b00)
			     (14 disp RIGHT-SIGNED)))
		      ((() ())
		       (LONG
			;; (ADDIL () L$,offset ,base)
			(6 #x0A)
			(5 base)
			(21 (quotient disp #x800) ASSEMBLE21:X)
			;; (LDO () (OFFSET R$,offset 0 1) ,reg)
			(6 ,opcode)
			(5 1)
			(5 reg)
			(2 #b00)
			(14 (remainder disp #x800) RIGHT-SIGNED))))))))

	     (load-immediate
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (? offset) (? reg))
		    (VARIABLE-WIDTH (disp offset)
		      ((#x-2000 #x1FFF)
		       (LONG (6 ,opcode)
			     (5 0)
			     (5 reg)
			     (2 #b00)
			     (14 disp RIGHT-SIGNED)))
		      ((() ())
		       (LONG
			;; (LDIL () L$,offset ,base)
			(6 #x08)
			(5 reg)
			(21 (quotient disp #x800) ASSEMBLE21:X)
			;; (LDO () (OFFSET R$,offset 0 ,reg) ,reg)
			(6 ,opcode)
			(5 reg)
			(5 reg)
			(2 #b00)
			(14 (remainder disp #x800) RIGHT-SIGNED))))))))

	     (left-immediate
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (? immed-21) (? reg))
		    (LONG (6 ,opcode)
			  (5 reg)
			  (21 immed-21 ASSEMBLE21:X)))))))

  (long-load      LDW   #x12)
  (long-load      LDWM  #x13)
  (long-load      LDH   #x11)
  (long-load      LDB   #x10)

  (long-store     STW   #x1a)
  (long-store     STWM  #x1b)
  (long-store     STH   #x19)
  (long-store     STB   #x18)

  (load-offset    LDO   #x0d)
  (load-immediate LDI   #x0d)	; pseudo-op (LDO complt (OFFSET displ 0) reg)

  (left-immediate LDIL  #x08)
  (left-immediate ADDIL #x0a))

;; In the following, the middle completer field (2 bits) appears to be zero,
;; according to the hardware.  Also, the u-bit seems not to exist in the
;; cache instructions.

(let-syntax ((indexed-load
	      (macro (keyword opcode extn)
		`(define-instruction ,keyword
		   (((? compl complx) (INDEX (? index-reg) (? space) (? base))
				      (? reg))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 index-reg)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b0)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 reg))))))

	     (indexed-store
	      (macro (keyword opcode extn)
		`(define-instruction ,keyword
		   (((? compl complx) (? reg)
				      (INDEX (? index-reg) (? space) (? base)))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 index-reg)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b0)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 reg))))))

	     (indexed-d-cache
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   (((? compl m-val) (INDEX (? index-reg) (? space) (? base)))
		    (LONG (6 #x01)
			  (5 base)
			  (5 index-reg)
			  (2 space)
			  (8 ,extn)
			  (1 compl)
			  (5 #x0))))))

	     (indexed-i-cache
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   (((? compl m-val)
		     (INDEX (? index-reg) (? space sr3) (? base)))
		    (LONG (6 #x01)
			  (5 base)
			  (5 index-reg)
			  (3 space)
			  (7 ,extn)
			  (1 compl)
			  (5 #x0)))))))
  
  (indexed-load  LDWX  #x03 #x2)
  (indexed-load  LDHX  #x03 #x1)
  (indexed-load  LDBX  #x03 #x0)
  (indexed-load  LDCWX #x03 #x7)
  (indexed-load  FLDWX #x09 #x0)
  (indexed-load  FLDDX #x0B #x0)

  (indexed-store FSTWX #x09 #x8)
  (indexed-store FSTDX #x0b #x8)

  (indexed-d-cache PDC  #x4e)
  (indexed-d-cache FDC  #x4a)
  (indexed-i-cache FIC  #x0a)
  (indexed-d-cache FDCE #x4b)
  (indexed-i-cache FICE #x0b))

(let-syntax ((scalr-short-load
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   (((? compl compls) (OFFSET (? offset) (? space) (? base))
				      (? reg))
		    (LONG (6 #x03)
			  (5 base)
			  (5 offset RIGHT-SIGNED)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b1)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 reg))))))

	     (scalr-short-store
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   (((? compl compls) (? reg)
				      (OFFSET (? offset) (? space) (? base)))
		    (LONG (6 #x03)
			  (5 base)
			  (5 reg)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b1)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 offset RIGHT-SIGNED))))))

	     (float-short-load
	      (macro (keyword opcode extn)
		`(define-instruction ,keyword
		   (((? compl compls) (OFFSET (? offset) (? space) (? base))
				      (? reg))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 offset RIGHT-SIGNED)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b1)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 reg))))))

	     (float-short-store
	      (macro (keyword opcode extn)
		`(define-instruction ,keyword
		   (((? compl compls) (? reg)
				      (OFFSET (? offset) (? space) (? base)))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 offset RIGHT-SIGNED)
			  (2 space)
			  (1 (vector-ref compl 0))
			  (1 #b1)
			  (2 (vector-ref compl 1))
			  (4 ,extn)
			  (1 (vector-ref compl 2))
			  (5 reg)))))))

  (scalr-short-load  LDWS   #x02)
  (scalr-short-load  LDHS   #x01)
  (scalr-short-load  LDBS   #x00)
  (scalr-short-load  LDCWS  #x07)

  (scalr-short-store STWS   #x0a)
  (scalr-short-store STHS   #x09)
  (scalr-short-store STBS   #x08)
  (scalr-short-store STBYS  #x0c)

  (float-short-load  FLDWS  #x09 #x00)
  (float-short-load  FLDDS  #x0b #x00)

  (float-short-store FSTWS  #x09 #x08)
  (float-short-store FSTDS  #x0b #x08))

;;;; Control transfer instructions

;;; Note: For the time being the unconditionaly branch instructions are not
;;; branch tensioned since their range is pretty large (1/2 Mbyte).
;;; They should be eventually (by using an LDIL,LDI,BLR sequence, for example).

(let-syntax ((branch&link
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   ((() (? reg) (@PCR (? label)))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 label PC-REL ASSEMBLE17:X)
			  (3 ,extn)
			  (11 label PC-REL ASSEMBLE17:Y)
			  (1 0)
			  (1 label PC-REL ASSEMBLE17:Z)))

		   (((N) (? reg) (@PCR (? label)))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 label PC-REL ASSEMBLE17:X)
			  (3 ,extn)
			  (11 label PC-REL ASSEMBLE17:Y)
			  (1 1)
			  (1 label PC-REL ASSEMBLE17:Z)))

		   ((() (? reg) (@PCO (? offset)))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 offset ASSEMBLE17:X)
			  (3 ,extn)
			  (11 offset ASSEMBLE17:Y)
			  (1 0)
			  (1 offset ASSEMBLE17:Z)))

		   (((N) (? reg) (@PCO (? offset)))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 offset ASSEMBLE17:X)
			  (3 ,extn)
			  (11 offset ASSEMBLE17:Y)
			  (1 1)
			  (1 offset ASSEMBLE17:Z))))))
	      
	     (branch
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   ((() (@PCR (? l)))
		    (LONG (6 #x3a)
			  (5 #b00000)
			  (5 l PC-REL ASSEMBLE17:X)
			  (3 #b000)
			  (11 l PC-REL ASSEMBLE17:Y)
			  (1 0)
			  (1 l PC-REL ASSEMBLE17:Z)))

		   (((N) (@PCR (? l)))
		    (LONG (6 #x3a)
			  (5 #b00000)
			  (5 l PC-REL ASSEMBLE17:X)
			  (3 #b000)
			  (11 l PC-REL ASSEMBLE17:Y)
			  (1 1)
			  (1 l PC-REL ASSEMBLE17:Z)))

		   ((() (@PCO (? offset)))
		    (LONG (6 #x3a)
			  (5 #b00000)
			  (5 offset ASSEMBLE17:X)
			  (3 #b000)
			  (11 offset ASSEMBLE17:Y)
			  (1 0)
			  (1 offset ASSEMBLE17:Z)))

		   (((N) (@PCO (? offset)))
		    (LONG (6 #x3a)
			  (5 #b00000)
			  (5 offset ASSEMBLE17:X)
			  (3 #b000)
			  (11 offset ASSEMBLE17:Y)
			  (1 1)
			  (1 offset ASSEMBLE17:Z)))))))

  (branch      B    0)		; pseudo-op (BL complt 0 displ)
  (branch&link BL   0)
  (branch&link GATE 1))

(let-syntax ((BV&BLR
	      (macro (keyword extn)
		`(define-instruction ,keyword
		   ((() (? offset-reg) (? reg))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 offset-reg)
			  (3 ,extn)
			  (11 #b00000000000)
			  (1 0)
			  (1 #b0)))

		   (((N) (? offset-reg) (? reg))
		    (LONG (6 #x3a)
			  (5 reg)
			  (5 offset-reg)
			  (3 ,extn)
			  (11 #b00000000000)
			  (1 1)
			  (1 #b0))))))

	     (BE&BLE
	      (macro (keyword opcode)
		`(define-instruction ,keyword
		   ((() (OFFSET (? offset) (? space sr3) (? base)))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 offset ASSEMBLE17:X)
			  (3 space)
			  (11 offset ASSEMBLE17:Y)
			  (1 0)
			  (1 offset ASSEMBLE17:Z)))

		   (((N) (OFFSET (? offset) (? space sr3) (? base)))
		    (LONG (6 ,opcode)
			  (5 base)
			  (5 offset ASSEMBLE17:X)
			  (3 space)
			  (11 offset ASSEMBLE17:Y)
			  (1 1)
			  (1 offset ASSEMBLE17:Z)))))))
  (BV&BLR BLR 2)
  (BV&BLR BV  6)
  (BE&BLE BE  #x38)
  (BE&BLE BLE #x39))

;;;; Conditional branch instructions

#|

Branch tensioning notes for the conditional branch instructions:

The sequence

	combt,cc	r1,r2,label
	instr1
	instr2

becomes

	combf,cc,n	r1,r2,tlabel		; pco = 0
	b		label			; no nullification
tlabel	instr1
	instr2

The sequence

	combt,cc,n	r1,r2,label
	instr1
	instr2

becomes either

	combf,cc,n	r1,r2,tlabel		; pco = 0
	b,n		label			; nullification
tlabel	instr1
	instr2

when label is downstream (a forwards branch)

or

	combf,cc,n	r1,r2,tlabel		; pco = 4
	b		label			; no nullification
	instr1
tlabel	instr2

when label is upstream (a backwards branch).

This adjusting of the nullify bits, the pc offset, etc. for tlabel are
performed by the utilities branch-extend-pco, branch-extend-disp, and
branch-extend-nullify in instr1.
|#

;;;; Compare/compute and branch.

(let-syntax
    ((defccbranch
       (macro (keyword completer opcode1 opcode2 opr1)
	 `(define-instruction ,keyword
	    (((? compl ,completer) (? ,(car opr1)) (? reg-2) (@PCO (? offset)))
	     (LONG (6  ,opcode1)
		   (5  reg-2)
		   (5  ,@opr1)
		   (3  (cadr compl))
		   (11 offset ASSEMBLE12:X)
		   (1  (car compl))
		   (1  offset ASSEMBLE12:Y)))

	    (((? compl ,completer) (? ,(car opr1)) (? reg-2) (@PCR (? l)))
	     (VARIABLE-WIDTH
	      (disp `(- ,l (+ *PC* 8)))
	      ((#x-2000 #x1FFF)
	       (LONG (6  ,opcode1)
		     (5  reg-2)
		     (5  ,@opr1)
		     (3  (cadr compl))
		     (11 disp ASSEMBLE12:X)
		     (1  (car compl))
		     (1  disp ASSEMBLE12:Y)))

	      ((() ())
	       ;; See page comment above.
	       (LONG (6  ,opcode2)		; COMBF
		     (5  reg-2)
		     (5  ,@opr1)
		     (3  (cadr compl))
		     (11 (branch-extend-pco disp (car compl)) ASSEMBLE12:X)
		     (1  1)
		     (1  (branch-extend-pco disp (car compl)) ASSEMBLE12:Y)

		     (6  #x3a)			; B
		     (5  0)
		     (5  (branch-extend-disp disp) ASSEMBLE17:X)
		     (3  0)
		     (11 (branch-extend-disp disp) ASSEMBLE17:Y)
		     (1  (branch-extend-nullify disp (car compl)))
		     (1  (branch-extend-disp disp) ASSEMBLE17:Z)))))))))

  (define-syntax defcond
    (lambda (name opcode1 opcode2 opr1)
      `(defccbranch ,name complaltfb ,opcode1 ,opcode2 ,opr1)))

  (define-syntax defpseudo
    (lambda (name opcode opr1)
      `(defccbranch ,name complalb
	 (TF-adjust ,opcode (cdr compl))
	 (TF-adjust-inverted ,opcode (cdr compl))
	 ,opr1)))

  (defcond COMBT #x20 #x22 (reg-1))
  (defcond COMBF #x22 #x20 (reg-1))
  (defcond ADDBT #x28 #x2a (reg-1))
  (defcond ADDBF #x2a #x28 (reg-1))

  (defcond COMIBT #X21 #x23 (immed-5 right-signed))
  (defcond COMIBF #X23 #x21 (immed-5 right-signed))
  (defcond ADDIBT #X29 #x2b (immed-5 right-signed))
  (defcond ADDIBF #X2b #x29 (immed-5 right-signed))

  (defpseudo COMB  #X20 (reg-1))
  (defpseudo ADDB  #X28 (reg-1))
  (defpseudo COMIB #X21 (immed-5 right-signed))
  (defpseudo ADDIB #x29 (immed-5 right-signed)))

;;;; Pseudo branch instructions.

#|

These nullify the following instruction when the branch is taken.
irrelevant of the sign of the displacement (unlike the real instructions).
If the displacement is positive, they use the nullify bit.
If the displacement is negative, they use a NOP.

	combn,cc	r1,r2,label
	
becomes either
	
	comb,cc,n	r1,r2,label

if label is downstream (forward branch)

or

	comb,cc		r1,r2,label
	nop

if label is upstream (backward branch)

If the displacement is too large, it becomes

	comb,!cc,n	r1,r2,tlabel	; pco = 0
	b,n		label
tlabel

Note: Only those currently used by the code generator are implemented.
|#

(let-syntax
    ((defccbranch
       (macro (keyword completer opcode1 opcode2 opr1)
	 `(define-instruction ,keyword
	    ;; No @PCO form.
	    ;; This is a pseudo-instruction used by the code-generator
	    (((? compl ,completer) (? ,(car opr1)) (? reg-2) (@PCR (? l)))
	     (VARIABLE-WIDTH
	      (disp `(- ,l (+ *PC* 8)))
	      ((0 #x1FFF)
	       ;; Forward branch.  Nullify.
	       (LONG (6  ,opcode1)		 ; COMB,cc,n
		     (5  reg-2)
		     (5  ,@opr1)
		     (3  (car compl))
		     (11 disp ASSEMBLE12:X)
		     (1  1)
		     (1  disp ASSEMBLE12:Y)))

	      ((#x-2000 -1)
	       ;; Backward branch.  No nullification, insert NOP.
	       (LONG (6  ,opcode1)		; COMB,cc
		     (5  reg-2)
		     (5  ,@opr1)
		     (3  (car compl))
		     (11 disp ASSEMBLE12:X)
		     (1  0)
		     (1  disp ASSEMBLE12:Y)

		     (6 #x02)			 ; NOP (OR 0 0 0)
		     (10 #b0000000000)
		     (3 0)
		     (1 0)
		     (7 #x12)
		     (5 #b00000)))

	      ((() ())
	       (LONG (6  ,opcode2)		; COMB!,n
		     (5  reg-2)
		     (5  ,@opr1)
		     (3  (car compl))
		     (11 0 ASSEMBLE12:X)
		     (1  1)
		     (1  0 ASSEMBLE12:Y)

		     (6  #x3a)			; B,n
		     (5  0)
		     (5  (branch-extend-disp disp) ASSEMBLE17:X)
		     (3  0)
		     (11 (branch-extend-disp disp) ASSEMBLE17:Y)
		     (1  1)
		     (1  (branch-extend-disp disp) ASSEMBLE17:Z)))))))))

  (define-syntax defcond
    (lambda (name opcode1 opcode2 opr1)
      `(defccbranch ,name complaltf ,opcode1 ,opcode2 ,opr1)))

  (define-syntax defpseudo
    (lambda (name opcode opr1)
      `(defccbranch ,name complal
	 (TF-adjust ,opcode compl)
	 (TF-adjust-inverted ,opcode compl)
	 ,opr1)))

  (defcond COMIBTN #X21 #x23 (immed-5 right-signed))
  (defcond COMIBFN #X23 #x21 (immed-5 right-signed))

  (defpseudo COMBN #X20 (reg-1)))

;;;; Miscellaneous control

(let-syntax
    ((defmovb&bb
       (macro (name opcode opr1 opr2 field2)
	 `(define-instruction ,name
	    (((? compl compledb) (? ,(car opr1)) ,@opr2 (@PCO (? offset)))
	     (LONG (6  ,opcode)
		   (5  ,field2)
		   (5  ,@opr1)
		   (3  (cdr compl))
		   (11 offset ASSEMBLE12:X)
		   (1  (car compl))
		   (1  offset ASSEMBLE12:Y)))

	    (((? compl compledb) (? ,(car opr1)) ,@opr2 (@PCR (? l)))
	     (VARIABLE-WIDTH
	      (disp `(- ,l (+ *PC* 8)))
	      ((#x-2000 #x1FFF)
	       (LONG (6  ,opcode)
		     (5  ,field2)
		     (5  ,@opr1)
		     (3  (cdr compl))
		     (11 l PC-REL ASSEMBLE12:X)
		     (1  (car compl))
		     (1  l PC-REL ASSEMBLE12:Y)))

	      ((() ())
	       ;; See page comment above.
	       (LONG (6  ,opcode)		; MOVB
		     (5  ,field2)
		     (5  ,@opr1)
		     (3  (branch-extend-edcc (cdr compl)))
		     (11 (branch-extend-pco disp (car compl)) ASSEMBLE12:X)
		     (1  1)
		     (1  (branch-extend-pco disp (car compl)) ASSEMBLE12:Y)
		     
		     (6  #x3a)			; B
		     (5  0)
		     (5  (branch-extend-disp disp) ASSEMBLE17:X)
		     (3  0)
		     (11 (branch-extend-disp disp) ASSEMBLE17:Y)
		     (1  (branch-extend-nullify disp (car compl)))
		     (1  (branch-extend-disp disp) ASSEMBLE17:Z)))))))))


  (defmovb&bb BVB	#x30 (reg)		    () 		#b00000)
  (defmovb&bb BB	#x31 (reg)		    ((? pos))	pos)
  (defmovb&bb MOVB	#x32 (reg-1)		    ((? reg-2))	reg-2)
  (defmovb&bb MOVIB	#x33 (immed-5 right-signed) ((? reg-2))	reg-2))

;;;; Assembler pseudo-ops

(define-instruction USHORT
  ((() (? high) (? low))
   (LONG (16 high UNSIGNED)
	 (16 low UNSIGNED))))

(define-instruction WORD
  ((() (? expression))
   (LONG (32 expression SIGNED))))

(define-instruction UWORD
  ((() (? expression))
   (LONG (32 expression UNSIGNED))))

(define-instruction EXTERNAL-LABEL
  ((() (? format-word) (@PCR (? label)))
   (LONG (16 format-word UNSIGNED)
	 (16 label BLOCK-OFFSET)))

  ((() (? format-word) (@PCO (? offset)))
   (LONG (16 format-word UNSIGNED)
	 (16 offset UNSIGNED))))

(define-instruction PCR-HOOK
  ((() (? target)
       (OFFSET (? offset) (? space sr3) (? base))
       (@PCR (? label)))
   (VARIABLE-WIDTH
    (disp `(- ,label (+ *PC* 8)))
    ((#x-2000 #x1FFF)
     (LONG
      ;; (BLE () (OFFSET ,offset ,space ,base))
      (6 #x39)
      (5 base)
      (5 offset ASSEMBLE17:X)
      (3 space)
      (11 offset ASSEMBLE17:Y)
      (1 0)
      (1 offset ASSEMBLE17:Z)
      ;; (LDO () (OFFSET ,disp 0 31) ,target)
      (6 #x0D)
      (5 31)
      (5 target)
      (2 #b00)
      (14 disp RIGHT-SIGNED)))
    ((() ())
     (LONG
      ;; (LDIL () L$disp-8 target)
      (6 #x08)
      (5 1)
      (21 (quotient (- disp 8) #x800) ASSEMBLE21:X)
      ;; (LDO () (OFFSET R$disp-4 0 1) target)
      (6 #x0D)
      (5 1)
      (5 1)
      (2 #b00)
      (14 (remainder (- disp 8) #x800) RIGHT-SIGNED)
      ;; (BLE () (OFFSET ,offset ,space ,base))
      (6 #x39)
      (5 base)
      (5 offset ASSEMBLE17:X)
      (3 space)
      (11 offset ASSEMBLE17:Y)
      (1 0)
      (1 offset ASSEMBLE17:Z)
      ;; (ADD () 31 1 target)
      (6 #x02)
      (5 31)
      (5 1)
      (3 0)
      (1 0)
      (7 #x30)
      (5 target))))))