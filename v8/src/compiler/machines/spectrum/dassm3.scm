#| -*-Scheme-*-

$Id$

Copyright (c) 1987, 1989, 1990, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;; Spectrum Disassembler: Internals
;;; package: (compiler disassembler)

(declare (usual-integrations))

;;;; Utilities

(define (get-longword)
  (let ((word (read-bits *current-offset 32)))
    (set! *current-offset (+ *current-offset 4))
    word))

(declare (integrate-operator extract))

(define (extract bit-string start end)
  (declare (integrate bit-string start end))
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

#|
(define disassembly '())

(define (verify-instruction instruction)
  (let ((bits (car (syntax-instruction instruction))))
    (if (and (bit-string? bits)
	     (= (bit-string-length bits) 32))
	(begin (set! disassembly (disassemble-word bits))
	       (newline)
	       (newline)
	       (if (equal? instruction disassembly)
		   (write "EQUAL")
		   (write "************************* NOT EQUAL"))
	       (newline)
	       (newline)
	       (write instruction)
	       (newline)
	       (newline)
	       (write "Disassembly:   ")
	       (write disassembly)))))

(define v verify-instruction)
|#

(define-integrable Mask-2-9   #b0011111111000000)
(define-integrable Mask-2-16  #b0011111111111111)
(define-integrable Mask-3-14  #b0001111111111100)
(define-integrable Mask-3-10  #b0001111111100000)
(define-integrable Mask-3-5   #b0001110000000000)
(define-integrable Mask-4-10  #b0000111111100000)
(define-integrable Mask-4-5   #b0000110000000000)
(define-integrable Mask-6-9   #b0000001111000000)
(define-integrable Mask-6-10  #b0000001111100000)
(define-integrable Mask-11-15 #b0000000000011111)
(define-integrable mask-copr  #b0000000111000000)

;;;; The disassembler proper

(define (disassemble-word word)
  (let ((hi-halfword (extract word 16 32))
	(lo-halfword (extract word 0 16)))
    (let ((opcode (fix:quotient hi-halfword #x400)))
      ((case opcode
	 ((#x00) sysctl-1)
	 ((#x01) sysctl-2)
	 ((#x02) arith&log)
	 ((#x03) indexed-mem)
	 ((#x04) #| SFUop |# unknown-major-opcode)
	 ((#x05)
	  (lambda (opcode hi lo)
	    opcode hi lo		;ignore
	    `(DIAG () ,(extract word 0 26))))
	 ((#x08 #x0a) ldil&addil)
	 ((#x09 #x0b) #| COPR-w and COPR-dw |# float-mem)
	 ((#x0c) #| COPRop |# float-op)
	 ((#x0d #x10 #x11 #x12 #x13) scalar-load)
	 ((#x18 #x19 #x1a #x1b) scalar-store)
	 ((#x20 #x21 #x22 #x23 #x28 #x29 #x2a #x2b #x30 #x31 #x32 #x33)
	  cond-branch)
	 ((#x24 #x25 #x2c #x2d) addi&subi)
	 ((#x34 #x35) extr&dep)
	 ((#x38 #x39) be&ble)
	 ((#x3a) branch)
	 (else unknown-major-opcode))
       opcode hi-halfword lo-halfword))))

(define (unknown-major-opcode opcode hi lo)
  opcode hi lo				;ignore
  (invalid-instruction))
      
(define (sysctl-1 opcode hi-halfword lo-halfword)
  ;; BREAK SYNC MFSP MFCTL MTSP MTCTL LDSID
  ;; Missing other system control:
  ;; MTSM, RSM, SSM, RFI.
  opcode				;ignore
  (let ((opcode-extn (fix:quotient (fix:and lo-halfword Mask-3-10) #x20)))
    (case opcode-extn
      ((#x00)
       (let ((immed-13-hi (fix:and hi-halfword 1023))
	     (immed-13-lo (fix:quotient lo-halfword #x2000))
	     (immed-5 (fix:and lo-halfword #x1f)))
	 `(BREAK () ,immed-5 ,(+ (* immed-13-hi #x100) immed-13-lo))))
      ((#x20)
       `(SYNC ()))
      ((#x25)
       (let ((target-reg (fix:and hi-halfword #x1f))
	     (space-reg (fix:quotient lo-halfword #x2000)))
	 `(MFSP () ,space-reg ,target-reg)))
      ((#x45)
       (let ((ctl-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				    #x20))
	     (target-reg (fix:and lo-halfword #x1f)))
	 `(MFCTL () ,ctl-reg ,target-reg)))
      ((#xc1)
       (let ((source-reg hi-halfword)
	     (space-reg (fix:quotient lo-halfword #x2000)))
	 `(MTSP () ,source-reg ,space-reg)))
      ((#xc2)
       (let ((ctl-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				    #x20))
	     (source-reg (fix:and hi-halfword #x1f)))
	 `(MTCTL () ,source-reg ,ctl-reg)))
      ((#x85)
       (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				     #x20))
	     (space-spec (fix:quotient lo-halfword #x4000))
	     (target-reg (fix:and lo-halfword #x1f)))
	 `(LDSID () (OFFSET ,space-spec ,base-reg)
		 ,target-reg)))
      (else
       (invalid-instruction)))))

(define (sysctl-2 opcode hi-halfword lo-halfword)
  ;; PROBER PROBERI PROBEW PROBEWI
  ;; Missing other system control:
  ;; LPA, LHA, PDTLB, PITLB, PDTLBE, PITLBE, IDTLBA, IITLBA,
  ;; IDTLBP, IITLBP, PDC, FDC, FIC, FDCE, FICE.
  opcode				;ignore
  (let ((opcode-extn (fix:quotient (fix:and lo-halfword Mask-2-9) #x40)))
    (let ((mnemonic (case opcode-extn
		      ((#x46) 'PROBER)
		      ((#xc6) 'PROBERI)
		      ((#x47) 'PROBEW)
		      ((#xc7) 'PROBEWI)
		      (else (invalid-instruction))))
	  (base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				  #x20))
	  (priv-reg (fix:and hi-halfword #x1f))
	  (space-spec (fix:quotient lo-halfword #x4000))
	  (target-reg (fix:and lo-halfword #x1f)))
      `(,mnemonic () (OFFSET ,space-spec ,base-reg)
		  ,priv-reg ,target-reg))))

(define (arith&log opcode hi-halfword lo-halfword)
  opcode				;ignore
  (let ((opcode-extn (fix:quotient (fix:and Mask-4-10 lo-halfword) #x20)))
    (let ((source-reg-2 (fix:quotient (fix:and Mask-6-10 hi-halfword)
				      #x20))
	  (source-reg-1 (fix:and hi-halfword #x1f))
	  (target-reg (fix:and lo-halfword #x1f))
	  (completer (x-arith-log-completer lo-halfword opcode-extn))
	  (mnemonic
	   (case opcode-extn
	     ((#x00) 'ANDCM)
	     ((#x10) 'AND)
	     ((#x12) 'OR)
	     ((#x14) 'XOR)
	     ((#x1c) 'UXOR)
	     ((#x20) 'SUB)
	     ((#x22) 'DS)
	     ((#x26) 'SUBT)
	     ((#x28) 'SUBB)
	     ((#x30) 'ADD)
	     ((#x32) 'SH1ADD)
	     ((#x34) 'SH2ADD)
	     ((#x36) 'SH3ADD)
	     ((#x38) 'ADDC)
	     ((#x44) 'COMCLR)
	     ((#x4c) 'UADDCM)
	     ((#x4e) 'UADDCMT)
	     ((#x50) 'ADDL)
	     ((#x52) 'SH1ADDL)
	     ((#x54) 'SH2ADDL)
	     ((#x56) 'SH3ADDL)
	     ((#x5c) 'DCOR)
	     ((#x5e) 'IDCOR)
	     ((#x60) 'SUBO)
	     ((#x66) 'SUBTO)
	     ((#x68) 'SUBBO)
	     ((#x70) 'ADDO)
	     ((#x72) 'SH1ADDO)
	     ((#x74) 'SH2ADDO)
	     ((#x76) 'SH3ADDO)
	     ((#x78) 'ADDCO)
	     (else (invalid-instruction)))))
      (cond ((or (eq? mnemonic 'DCOR) (eq? mnemonic 'IDCOR))
	     `(,mnemonic ,completer ,source-reg-2 ,target-reg))
	    ((and (eq? mnemonic 'OR) (zero? source-reg-2))
	     (if (and (zero? source-reg-1) (zero? target-reg))
		 `(NOP ,completer)
		 `(COPY ,completer ,source-reg-1 ,target-reg)))
	    (else
	     `(,mnemonic ,completer ,source-reg-1 ,source-reg-2
			 ,target-reg))))))

(define (indexed-mem opcode hi-halfword lo-halfword)
  ;; LDBX/S LDHX/S LDWX/S LDCWX/S STWS STHS STBS STBYS
  opcode				;ignore
  (let ((short-flag (fix:and lo-halfword #x1000)))
    (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				  #x20))
	  (index-or-source (fix:and hi-halfword #x1f))
	  (space-spec (fix:quotient lo-halfword #x4000))
	  (opcode-extn (fix:quotient (fix:and lo-halfword Mask-6-9) #x40))
	  (target-or-index (fix:and lo-halfword #x1f))
	  (cc-print-completer (cc-completer lo-halfword))
	  (um-print-completer (um-completer short-flag lo-halfword)))
      (let ((mnemonic
	     (if (zero? short-flag)
		 (case opcode-extn
		   ((#x0) 'LDBX)
		   ((#x1) 'LDHX)
		   ((#x2) 'LDWX)
		   ((#x7) 'LDCWX)
		   (else (invalid-instruction)))
		 (case opcode-extn
		   ((#x0) 'LDBS)
		   ((#x1) 'LDHS)
		   ((#x2) 'LDWS)
		   ((#x7) 'LDCWS)
		   ((#x8) 'STBS)
		   ((#x9) 'STHS)
		   ((#xa) 'STWS)
		   ((#xc) 'STBYS)
		   (else (invalid-instruction))))))
	(if (< opcode-extn 8)
	    `(,mnemonic (,@um-print-completer ,@cc-print-completer)
			(,(if (zero? short-flag) 'INDEX 'OFFSET)
			 ,(if (zero? short-flag)
			      index-or-source
			      (X-Signed-5-Bit index-or-source))
			 ,space-spec ,base-reg)
			,target-or-index)
	    `(,mnemonic (,@um-print-completer ,@cc-print-completer)
			,index-or-source
			(,(if (zero? short-flag) 'INDEX 'OFFSET)
			 ,(if (zero? short-flag)
			      target-or-index
			      (X-Signed-5-Bit target-or-index))
			 ,space-spec ,base-reg)))))))

(define (ldil&addil opcode hi-halfword lo-halfword)
  ;; LDIL ADDIL
  (let* ((reg (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	 (hi-immed (fix:and hi-halfword #x1f))
	 (immed (assemble-21 (+ (* hi-immed #x10000) lo-halfword))))
    `(,(if (= opcode #x08) 'LDIL 'ADDIL) () ,immed ,reg)))

(define (float-mem opcode hi-halfword lo-halfword)
  ;; FLDWX/S FLDDX/S FSTWX/S FSTDX/S 
  (let ((short-flag (fix:and lo-halfword #x1000))
	(index (fix:and hi-halfword #x1f)))
    (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	  (index (if (zero? short-flag)
		     index
		     (X-Signed-5-Bit index)))
	  (space-spec (fix:quotient lo-halfword #x4000))
	  (opcode-extn (fix:quotient (fix:and lo-halfword Mask-6-9) #x40))
	  (source-or-target (fix:and lo-halfword #x1f))
	  (cc-print-completer (cc-completer lo-halfword))
	  (um-print-completer (um-completer short-flag lo-halfword)))
      (let ((mnemonic
	     (if (zero? short-flag)
		 (if (= opcode #x09)
		     (if (= opcode-extn 0) 'FLDWX 'FSTWX)
		     (if (= opcode-extn 0) 'FLDDX 'FSTDX))
		 (if (= opcode #x09)
		     (if (= opcode-extn 0) 'FLDWS 'FSTWS)
		     (if (= opcode-extn 0) 'FLDDS 'FSTDS)))))
	(if (< opcode-extn 8)
	    `(,mnemonic (,@um-print-completer ,@cc-print-completer)
			(,(if (zero? short-flag) 'INDEX 'OFFSET)
			 ,index ,space-spec ,base-reg)
			,source-or-target)
	    `(,mnemonic (,@um-print-completer ,@cc-print-completer)
			,source-or-target
			(,(if (zero? short-flag) 'INDEX 'OFFSET)
			 ,index ,space-spec ,base-reg)))))))

(define (scalar-load opcode hi-halfword lo-halfword)
  ;; LDO LDB LDH LDW LDWM
  (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	(space-spec (fix:quotient lo-halfword #x4000))
	(target-reg (fix:and hi-halfword #x1f))
	(displacement (XRight2s (fix:and lo-halfword Mask-2-16)))
	(mnemonic
	 (case opcode
	   ((#x0d) 'LDO)
	   ((#x10) 'LDB)
	   ((#x11) 'LDH)
	   ((#x12) 'LDW)
	   ((#x13) 'LDWM)
	   (else (invalid-instruction)))))
    (cond ((not (eq? mnemonic 'LDO))
	   `(,mnemonic ()
		       (OFFSET ,displacement ,space-spec ,base-reg)
		       ,target-reg))
	  ((zero? base-reg)
	   `(LDI () ,displacement ,target-reg))
	  (else
	   `(,mnemonic ()
		       (OFFSET ,displacement 0 ,base-reg)
		       ,target-reg)))))

(define (scalar-store opcode hi-halfword lo-halfword)
  ;; STB STH STW STWM
  (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				#x20))
	(space-spec (fix:quotient lo-halfword #x4000))
	(source-reg (fix:and hi-halfword #x1f))
	(displacement (XRight2s (fix:and lo-halfword Mask-2-16)))
	(mnemonic
	 (case opcode
	   ((#x18) 'STB)
	   ((#x19) 'STH)
	   ((#x1a) 'STW)
	   ((#x1b) 'STWM)
	   (else (invalid-instruction)))))
    `(,mnemonic () ,source-reg
		(OFFSET ,displacement ,space-spec ,base-reg))))

(define (cond-branch opcode hi-halfword lo-halfword)
  ;; MOVB MOVIB COMB COMIB ADDB ADDIB BVB BB
  (let*  ((reg-2 (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	  (reg-1 (if (and (not (= opcode #x31))
			  (odd? opcode))
		     ;; For odd opcodes, this is immed-5 data, not reg-1
		     (X-Signed-5-Bit (fix:and hi-halfword #x1f))
		     (fix:and hi-halfword #x1f)))
	  (c (fix:quotient lo-halfword #x2000))
	  (word-displacement (collect-14 lo-halfword))
	  (null-completer (nullify-bit lo-halfword))
	  (mnemonic (case opcode
		      ((#x20) 'COMBT)
		      ((#x21) 'COMIBT)
		      ((#x22) 'COMBF)
		      ((#x23) 'COMIBF)
		      ((#x28) 'ADDBT)
		      ((#x29) 'ADDIBT)
		      ((#x2a) 'ADDBF)
		      ((#x2b) 'ADDIBF)
		      ((#x30) 'BVB)
		      ((#x31) 'BB)
		      ((#x32) 'MOVB)
		      ((#x33) 'MOVIB)
		      (else (invalid-instruction))))
	  (completer-symbol 
	   (X-Extract-Deposit-Completers c)))
    (if (eq? mnemonic 'BVB)
	`(,mnemonic (,@completer-symbol ,@null-completer) ,reg-1
		    ,word-displacement)
	`(,mnemonic (,@completer-symbol ,@null-completer) ,reg-1 ,reg-2
		    ,word-displacement))))

(define (addi&subi opcode hi-halfword lo-halfword)
  ;; ADDI-T-O SUBI-O COMICLR
  (let ((opcode-extn (fix:quotient (fix:and 2048 lo-halfword) #x800)))
    (let ((source-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				    #x20))
	  (target-reg (fix:and hi-halfword #x1f))
	  (immed-value (X-Signed-11-Bit (fix:and lo-halfword 2047)))
	  (completer-symbol (x-arith-log-completer lo-halfword opcode))
	  (mnemonic
	   (if (= opcode-extn 0)
	       (case opcode
		 ((#x24) 'COMICLR)
		 ((#x25) 'SUBI)
		 ((#x2c) 'ADDIT)
		 ((#x2d) 'ADDI)
		 (else (invalid-instruction)))
	       (case opcode
		 ((#x25) 'SUBIO)
		 ((#x2c) 'ADDITO)
		 ((#x2d) 'ADDIO)
		 (else (invalid-instruction))))))
      `(,mnemonic ,completer-symbol ,immed-value
		  ,source-reg ,target-reg))))

(define (extr&dep opcode hi-halfword lo-halfword)
  ;; VEXTRU VEXTRS VDEP ZVDEP
  (let*  ((reg-2 (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	  (reg-1 (fix:and hi-halfword #x1f))
	  (c (fix:quotient lo-halfword #x2000))
	  (opcode-extn (fix:quotient (fix:and lo-halfword Mask-3-5) #x400))
	  (cp (fix:quotient (fix:and lo-halfword Mask-6-10) #x20))
	  (clen (fix:and lo-halfword #x1f))
	  (completer-symbol (X-Extract-Deposit-Completers c))
	  (mnemonic
	   (vector-ref (if (= opcode #x34)
			   '#(VSHD *INVALID* SHD *INVALID*
				   VEXTRU VEXTRS EXTRU EXTRS)
			   '#(ZVDEP VDEP ZDEP DEP
				    ZVDEPI VDEPI ZDEPI DEPI))
		       opcode-extn)))

    (define (process reg-1 reg-2)
      (cond ((or (<= 4 opcode-extn 5)
		 (and (= opcode #x35)
		      (< opcode-extn 2)))
	     ;; Variable dep/ext
	     `(,mnemonic ,completer-symbol ,reg-1 ,(- 32 clen) ,reg-2))
	    ((eq? mnemonic 'VSHD)
	     `(,mnemonic ,completer-symbol ,reg-1 ,reg-2 ,clen))
	    ((eq? mnemonic 'SHD)
	     `(,mnemonic ,completer-symbol ,reg-1 ,reg-2 ,(- 31 cp) ,clen))
	    (else
	     `(,mnemonic ,completer-symbol
			 ,reg-1
			 ,(if (= opcode #x34) cp (- 31 cp))
			 ,(- 32 clen) ,
			 reg-2))))

    (cond ((eq? mnemonic '*INVALID*)
	   (invalid-instruction))
	  ((<= opcode-extn 3)
	   (process reg-1 reg-2))
	  ((= opcode #x34)
	   (process reg-2 reg-1))
	  (else
	   (process (X-Signed-5-Bit reg-1) reg-2)))))

(define (be&ble opcode hi-halfword lo-halfword)
  ;; BE BLE
  (let ((base-reg (fix:quotient (fix:and Mask-6-10 hi-halfword) #x20))
	(space-reg (Assemble-3 (fix:quotient lo-halfword #x2000)))
	(null-completer (nullify-bit lo-halfword))
	(word-displacement (collect-19 lo-halfword hi-halfword false))
	(mnemonic (if (= opcode #x38) 'BE 'BLE)))
    `(,mnemonic ,null-completer
		(OFFSET ,word-displacement ,space-reg ,base-reg))))

(define (branch opcode hi-halfword lo-halfword)
  ;; B, BL, BLR, BV, GATE
  opcode				;ignore
  (let ((opcode-extension (fix:quotient lo-halfword #x2000)))
    (case opcode-extension
      ((0 1)
       ;; B BL GATE
       (let ((return-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				       #x20))
	     (word-displacement (collect-19 lo-halfword hi-halfword true))
	     (null-completer (nullify-bit lo-halfword)))
	 (let ((mnemonic (cond ((= opcode-extension 1) 'GATE)
			       ((= return-reg 0) 'B)
			       (else 'BL))))
	   (if (eq? mnemonic 'B)
	       `(,mnemonic ,null-completer ,word-displacement)
	       `(,mnemonic ,null-completer ,return-reg ,word-displacement)))))
      ((2 6)
       ;; BLR BV
       (let ((return-reg (fix:quotient (fix:and Mask-6-10 hi-halfword)
				       #x20))
	     (offset-reg (fix:and hi-halfword #x1f))
	     (null-completer (nullify-bit lo-halfword))
	     (mnemonic (if (= opcode-extension 2)
			   'BLR
			   'BV)))
	 `(,mnemonic ,null-completer ,offset-reg ,return-reg)))
      (else (invalid-instruction)))))

;;;; FLoating point operations

(define (float-op opcode hi-halfword lo-halfword)
  ;; Copr 0 is the floating point copr.
  opcode				;ignore
  (if (not (zero? (fix:and (fix:quotient lo-halfword #x40) 7)))
      (invalid-instruction)
      ((case (fix:and (fix:quotient lo-halfword #x200) 3)
	 ((0) float-op0)
	 ((1) float-op1)
	 ((2) float-op2)
	 (else float-op3))
       hi-halfword lo-halfword)))

(define (float-op0 hi-halfword lo-halfword)
  (let ((mnemonic
	 (vector-ref '#(COPR *INVALID* FCPY FABS FSQRT FRND
			     *INVALID* *INVALID*)
		     (fix:quotient lo-halfword #x2000)))
	(fmt (floating-format (fix:and (fix:quotient lo-halfword #x800) 3)))
	(r (fix:and (fix:quotient hi-halfword #x20) #x1f))
	(t (fix:and lo-halfword #x1f)))
    (if (eq? mnemonic '*INVALID*)
	(invalid-instruction)
	`(,mnemonic (,fmt) ,r ,t))))

(define (float-op1 hi-halfword lo-halfword)
  (let ((mnemonic
	 (vector-ref '#(FCNVFF FCNVXF FCNVFX FCNVFXT)
		     (+ (* 2 (fix:and hi-halfword 1))
			(fix:quotient lo-halfword #x8000))))
	(sf (floating-format (fix:and (fix:quotient lo-halfword #x800) 3)))
	(df (floating-format (fix:and (fix:quotient lo-halfword #x2000) 3)))
	(r (fix:and (fix:quotient hi-halfword #x20) #x1f))
	(t (fix:and lo-halfword #x1f)))
    `(,mnemonic (,sf ,df) ,r ,t)))

(define (float-op2 hi-halfword lo-halfword)
  (case (fix:quotient lo-halfword #x2000)
    ((0)
     (let ((fmt (floating-format (fix:and (fix:quotient lo-halfword #x800) 3)))
	   (r1 (fix:and (fix:quotient hi-halfword #x20) #x1f))
	   (r2 (fix:and hi-halfword #x1f))
	   (c (float-completer (fix:and lo-halfword #x1f))))
       `(FCMP (,c ,fmt) ,r1 ,r2)))
    ((1)
     `(FTEST))
    (else
     (invalid-instruction))))    

(define (float-op3 hi-halfword lo-halfword)
  (let ((mnemonic
	 (vector-ref '#(FADD FSUB FMPY FDIV FREM *INVALID* *INVALID* *INVALID*)
		     (fix:quotient lo-halfword #x2000)))
	(fmt (floating-format (fix:and (fix:quotient lo-halfword #x800) 3)))
	(r1 (fix:and (fix:quotient hi-halfword #x20) #x1f))
	(r2 (fix:and hi-halfword #x1f))
	(t (fix:and lo-halfword #x1f)))
    (if (eq? mnemonic '*INVALID*)
	(invalid-instruction)
	`(,mnemonic (,fmt) ,r1 ,r2 ,t))))

;;;; Field extraction

(define (assemble-3 x)
  (let ((split (integer-divide x 2)))
    (+ (* (integer-divide-remainder split) 4)
       (integer-divide-quotient split))))

(define (assemble-12 x y)
  (let ((split (integer-divide x 2)))
    (+ (* y #x800)
       (* (integer-divide-remainder split) #x400)
       (integer-divide-quotient split))))

(define (assemble-17 x y z)
  (let ((split (integer-divide y 2)))
    (+ (* z #x10000)
       (* x #x800)
       (* (integer-divide-remainder split) #x400)
       (integer-divide-quotient split))))

#|
(define (assemble-21 x)				   ; Source        Dest
  (+ (* (* (fix:and x 1) #x10000) #x10)		   ; bit 20        bit 0
     (* (fix:and x #xffe) #x100)		   ; bits 9-19     bits 1-11
     (fix:quotient (fix:and x #xc000) #x80)	   ; bits 5-6      bits 12-13
     (fix:quotient (fix:and x #x1f0000) #x4000)	   ; bits 0-4      bits 14-18
     (fix:quotient (fix:and x #x3000) #x1000)))	   ; bits 7-8      bits 19-20
|#

(define (assemble-21 x)
  (let ((b (unsigned-integer->bit-string 21 x)))
    (+ (* (extract b 0 1) #x100000)
       (* (extract b 1 12) #x200)
       (* (extract b 14 16) #x80)
       (* (extract b 16 21) #x4)
       (extract b 12 14))))

(define (x-signed-5-bit x)		; Sign bit is lo.
  (let ((sign-bit (fix:and x 1))
	(hi-bits (fix:quotient x 2)))
    (if (= sign-bit 0)
	hi-bits
	(- hi-bits 16))))

(define (x-signed-11-bit x)		; Sign bit is lo.
  (let ((sign-bit (fix:and x 1))
	(hi-bits (fix:quotient x 2)))
    (if (= sign-bit 0)
	hi-bits
	(- hi-bits #x400))))

(define (xright2s d)
  (let ((sign-bit (fix:and d 1)))
    (- (fix:quotient d 2)
       (if (= sign-bit 0)
	   0
	   #x2000))))

(define-integrable (make-pc-relative value)
  (offset->pc-relative value *current-offset))

(define (collect-14 lo-halfword)
  (let* ((sign (fix:and lo-halfword 1))
	 (w (* 4 (assemble-12 (fix:quotient (fix:and lo-halfword #x1ffc) 4)
			      sign))))
    (make-pc-relative (if (= sign 1)
			  (- w #x4000)	; (expt 2 14)
			  w))))

(define (collect-19 lo-halfword hi-halfword pc-rel?)
  (let* ((sign (fix:and 1 lo-halfword))
	 (w (* 4 (assemble-17 (fix:and Mask-11-15 hi-halfword)
			      (fix:quotient (fix:and Mask-3-14 lo-halfword)
					4)
			      sign)))
	 (disp (if (= sign 1)
		   (- w #x80000)	; (expt 2 19)
		   w)))
    (if pc-rel?
	(make-pc-relative disp)
	disp)))

;;;; Completers (modifier suffixes)

(define (x-arith-log-completer lo-halfword xtra)
  ;; c is 3-bit, f 1-bit
  (let ((c (fix:quotient lo-halfword #x2000))
	(f (fix:quotient (fix:and lo-halfword 4096) #x1000)))
    (let ((index (+ (* f 8) c)))
      (case xtra
	((#x2c #x2d #x30 #x32 #x34 #x36 #x38 #x4c #x4e
	       #x50 #x52 #x54 #x56 #x70 #x72 #x74 #x76 #x78)
	 ;; adds: #x2c #x2d are ADDI
	 (vector-ref
	  '#(() (=) (<) (<=) (NUV) (ZNV) (SV) (OD)
		(TR) (<>) (>=) (>) (UV) (VNZ) (NSV) (EV))
	  #|
	  '#(() (Eq) (Lt) (LtEq) (NUV) (ZNV) (SV) (OD)
		(TR) (LtGt) (GtEq) (Gt) (UV) (VNZ) (NSV) (EV))
	  |#
	  index))
	((#x20 #x22 #x24 #x25 #x26 #x28 #x44 #x60 #x66 #x68)
	 ;; subtract/compare: #x24 #x25 are SUBI
	 (vector-ref
	  '#(() (=) (<) (<=) (<<) (<<=) (SV) (OD)
		(TR) (<>) (>=) (>) (>>=) (>>) (NSV) (EV))
	  #|
	  '#(() (Eq) (Lt) (LtEq) (LtLt) (LtLtEq) (SV) (OD)
		(TR) (LtGt) (GtEq) (Gt) (GtGtEq) (GtGt) (NSV) (EV))
	  |#
	  index))
	((0 #x10 #x12 #x14 #x1c)
	 ;; logical
	 (vector-ref
	  '#(() (=) (<) (<=) () () () (OD)
		(TR) (<>) (>=) (>) () () () (EV))
	  #|
	  '#(() (Eq) (Lt) (LtEq) () () () (OD)
		(TR) (LtGt) (GtEq) (Gt) () () () (EV))
	  |#
	  index))
	((#x5c #x5e)
	 ;; unit
	 (vector-ref '#(() () (SBZ) (SHZ) (SDC) () (SBC) (SHC)
			   (TR) () (NBZ) (NHZ) (NDC) () (NBC) (NHC))
		     index))))))

(define (X-Extract-Deposit-Completers c)
  (vector-ref '#(() (=) (<) (OD) (TR) (<>) (>=) (EV))
	      #| '#(() (Eq) (Lt) (OD) (TR) (LtGt) (GtEq) (EV)) |#
	      c))

(define (cc-completer lo-halfword)
  (vector-ref '#(() (C) (Q) (P))
	      (fix:quotient (fix:and lo-halfword Mask-4-5) #x400)))

(define (um-completer short-flag lo-halfword)
  (let ((u-completer (fix:and lo-halfword #x2000))
	(m-completer (fix:and lo-halfword #x20)))
    (if (zero? short-flag)
	(if (zero? u-completer)
	    (if (zero? m-completer) '() '(M))
	    (if (zero? m-completer) '(S) '(SM)))
	(if (zero? m-completer)
	    '()
	    (if (zero? u-completer) '(MA) '(MB))))))

(define-integrable (nullify-bit lo-halfword)
  (if (= (fix:and lo-halfword 2) 2) '(N) '()))

(define-integrable (floating-format value)
  (vector-ref '#(SGL DBL FMT=2 QUAD) value))

(define-integrable (float-completer value)
  (vector-ref '#(false? false ? !<=> = =T ?= !<> !?>= < ?< !>= !?> <= ?<= !>
		 !?<= > ?> !<= !?< >= ?>= !< !?= <> != !=T !? <=> true? true)
	      value))