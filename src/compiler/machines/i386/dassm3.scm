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

;;;; Intel i386 Disassembler: Internals
;;; package: (compiler disassembler)

(declare (usual-integrations))

;; IMPORTANT: This disassembler currently does not handle
;; operand size and address size modifiers.
;; Thus it is "stuck" in 32-bit mode, just like the assembler.

;; These really depend on the current operand size

(define next-word next-32-bit-word)
(define next-unsigned-word next-unsigned-32-bit-word)

;; This really depends on the current address size

(define next-offset next-word)


(define-integrable (high-nibble byte)
  (fix:lsh byte -4))

(define-integrable (low-nibble byte)
  (fix:and byte #xf))

(define-integrable (low-three-bits byte)
  (fix:and byte #x7))

(define-integrable (modr/m-mod modr/m-byte)
  (fix:and (fix:lsh modr/m-byte -6) #x3))

(define-integrable (modr/m-reg modr/m-byte)
  (fix:and (fix:lsh modr/m-byte -3) #x7))

(define-integrable (modr/m-base modr/m-byte)
  (fix:and modr/m-byte #x7))

(define-integrable (sib-base sib-byte)
  (fix:and sib-byte #x7))

(define-integrable (sib-index sib-byte)
  (fix:and (fix:lsh sib-byte -3) #x7))

(define (sib-scale sib-byte)
  (vector-ref '#(1 2 4 8) (fix:and (fix:lsh sib-byte -6) #x3)))

(define (pc-relative prefix offset)
  (cond ((find-label (+ *current-offset offset))
	 =>
	 (lambda (label)
	   `(,@prefix (@PCR ,label))))
	(else
	 `(,@prefix (@PCO ,offset)))))

(define (@R reg)
  (let ((operand `(@R ,reg)))
    (or (and disassembler/symbolize-output?
	     (interpreter-register? operand))
	operand)))

(define (@RO size reg offset)
  (let ((operand `(@RO ,size ,reg ,offset)))
    (or (and disassembler/symbolize-output?
	     (interpreter-register? operand))
	operand)))

(define (immediate-byte)
  `(& ,(next-byte)))

(define (immediate-word)
  `(& ,(next-word)))

(define (decode-r/m-32 byte)
  (let ((base (modr/m-base byte)))
    (define (ea size next-offset)
      (cond ((fix:= base 4)			; esp
	     (let ((sib (next-unsigned-byte)))
	       (let ((base (sib-base sib))
		     (index (sib-index sib))
		     (scale (sib-scale sib)))
		 (if (fix:= index 4)		; esp
		     (cond ((and (fix:= base 5)
				 (fix:= scale 1))
			    (if (not size)
				`(@ 0)		; ???
				`(@ ,(next-offset))))
			   ((not size)
			    (@R base))
			   (else
			    (@RO size base (next-offset))))
		     (cond ((and (fix:= base 5)
				 (fix:= scale 1))
			    (if (not size)
				(@R index)
				(@RO size index (next-offset))))
			   ((not size)
			    `(@RI ,base ,index ,scale))
			   (else
			    `(@ROI ,size ,base ,(next-offset)
				   ,index ,scale)))))))
	    ((not size)
	     (@R base))
	    (else
	     (@RO size base (next-offset)))))

    (case (modr/m-mod byte)
      ((0)
       (if (fix:= base 5)				; ebp
	   `(@ ,(next-32-bit-word))
	   (ea #f (lambda () 0))))
      ((1)
       (ea 'B next-byte))
      ((2)
       (ea 'W next-32-bit-word))
      ((3)
       `(R ,base))
      (else
       (error "decode-r/m: bad mode" byte)))))

(define (decode-r/m-16 byte)
  (let ((base (modr/m-base byte)))
    (define (ea size offset)
      (if (fix:< base 4)
	  (let ((base (if (fix:> base 1) 5 3))
		(index (fix:+ 6 (fix:and base 1))))
	    (if size
		`(@RI ,base ,index 1)
		`(@ROI ,size ,base ,offset ,index 1)))
	  (let ((reg (vector-ref '#(6 7 5 3) (fix:- base 4))))
	    (if size
		(@RO size reg offset)
		(@R reg)))))

    (case (modr/m-mod byte)
      ((0)
       (if (fix:= base 6)
	   `(@ ,(next-16-bit-word))
	   (ea #f 0)))
	   
      ((1)
       (ea 'B (next-byte)))
      ((2)
       (ea 'W (next-16-bit-word)))
      ((3)
       `(R ,base))
      (else
       (error "decode-r/m: bad mode" byte)))))

(define decode-r/m decode-r/m-32)

(define (make-modr/m-decoder receiver)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (let* ((modr/m-byte (next-unsigned-byte))
	   (ea (decode-r/m modr/m-byte)))
      (receiver (modr/m-reg modr/m-byte) ea))))

(define (decode-E prefix reg-value)
  (lambda (opcode-byte)
    (let ((modr/m-byte (next-unsigned-byte)))
      (if (not (= (modr/m-reg modr/m-byte) reg-value))
	  (unknown-inst opcode-byte modr/m-byte)
	  `(,@prefix ,(decode-r/m modr/m-byte))))))

(define (decode-E/G prefix)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix ,ea (R ,reg)))))

(define (decode-G/E prefix)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix (R ,reg) ,ea))))

(define (decode-E/I prefix next)
  (make-modr/m-decoder
   (lambda (reg ea)
     reg					; ignored, should be checked
     `(,@prefix ,ea (& ,(next))))))

(define (decode-G/E/I prefix next)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix (R ,reg) ,ea ,(next)))))

(define (decode-E/G/I prefix next)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix ,ea (R ,reg) ,(next)))))

(define (decode-G/M prefix)
  ;; This should check that we are dealing with a memory EA!
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix (R ,reg) ,ea))))

(define (decode-E/X prefix reg-kind)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix ,ea (,reg-kind ,reg)))))

(define (decode-X/E prefix reg-kind)
  (make-modr/m-decoder
   (lambda (reg ea)
     `(,@prefix (,reg-kind ,reg) ,ea))))

(define (decode-@ prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (let ((offset (next-offset)))
      `(,@prefix (@ ,offset)))))

(define (decode-Ap prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (let ((offset (next-offset)))
      `(,@prefix (SEGMENT ,(next-unsigned-16-bit-word))
		 (OFFSET ,offset)))))

(define (decode-Ib prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    `(,@prefix (& ,(next-byte)))))
  
(define (decode-I16 prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    `(,@prefix (& ,(next-16-bit-word)))))

(define (decode-Iw prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    `(,@prefix (& ,(next-word)))))

(define (decode-ENTER opcode-byte)
  opcode-byte					; ignored
  (let ((first (next-unsigned-16-bit-word)))
    `(ENTER (& ,first) (& ,(next-unsigned-byte)))))

(define (decode-pcrb prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (pc-relative prefix (next-byte))))

(define (decode-pcrw prefix)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (pc-relative prefix (next-offset))))

(define (unknown-inst opcode-byte . more-bytes)
  (set! *valid? false)				; re-synch.
  `(BYTE U ,opcode-byte ,@more-bytes))

(define-integrable (simple-inst inst)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    inst))

(define (backwards handler)
  (lambda (opcode-byte)
    (let ((result (handler opcode-byte)))
      (let ((back (reverse result)))
	(reverse (cons* (cadr back)
			(cons (car back)
			      (cddr back))))))))

(define-integrable (register-op prefix)
  (lambda (opcode-byte)
    `(,@prefix (R ,(fix:and opcode-byte #x7)))))	 

(define jcc-opcodes
  '#(
     JO JNO JB  JNB
     JZ JNZ JBE JNBE
     JS JNS JP  JNP
     JL JNL JLE JNLE))

(define setcc-opcodes
  '#(
     SETO SETNO SETB  SETNB
     SETZ SETNZ SETBE SETNBE
     SETS SETNS SETP  SETNP
     SETL SETNL SETLE SETNLE))

(define (group-1&2 opcodes size get-operand)
  (lambda (opcode-byte)
    opcode-byte				; ignored
    (let ((modr/m-byte (next-unsigned-byte)))
      (let ((operand (decode-r/m modr/m-byte))
	    (opcode (vector-ref opcodes (modr/m-reg modr/m-byte))))
	`(,opcode ,size ,operand ,(get-operand))))))

(define (group-3 size read-operand)
  (lambda (opcode-byte)
    opcode-byte					; ignored
    (let* ((modr/m-byte (next-unsigned-byte))
	   (operand (decode-r/m modr/m-byte)))
      (let ((dispatch (modr/m-reg modr/m-byte)))
	(cond ((< dispatch 2)
	       `(TEST ,size ,operand (& ,(read-operand))))
	      ((< dispatch 4)
	       `(,(if (= dispatch 2) 'NOT 'NEG) ,size ,operand))
	      (else
	       `(,(vector-ref '#(MUL IMUL DIV IDIV) (- dispatch 4))
		 ,size
		 (R 0)
		 ,operand)))))))	 

(define (group-4 size)
  (lambda (opcode-byte)
    (let* ((modr/m-byte (next-unsigned-byte))
	   (operand (lambda () (decode-r/m modr/m-byte))))
      (case (modr/m-reg modr/m-byte)
	((0)
	 `(INC ,size ,(operand)))
	((1)
	 `(DEC ,size ,(operand)))
	(else
	 (unknown-inst opcode-byte modr/m-byte))))))

(define (group-5 size)
  (lambda (opcode-byte)
    (let* ((modr/m-byte (next-unsigned-byte))
	   (operand (lambda () (decode-r/m modr/m-byte))))
      (case (modr/m-reg modr/m-byte)
	((0)
	 `(INC ,size ,(operand)))
	((1)
	 `(DEC ,size ,(operand)))
	((2)
	 `(CALL ,(operand)))
	((3)
	 `(CALL F ,(operand)))
	((4)
	 `(JMP ,(operand)))
	((5)
	 `(JMP F ,(operand)))
	((6)
	 `(PUSH ,(operand)))
	(else
	 (unknown-inst opcode-byte modr/m-byte))))))

(define (group-6&7 opcodes)
  (lambda (second-byte)
    (let* ((modr/m-byte (next-unsigned-byte))
	   (op (vector-ref opcodes (modr/m-reg modr/m-byte))))
      (if (not op)
	  (unknown-inst #x0f second-byte modr/m-byte)
	  `(,op ,(decode-r/m modr/m-byte))))))

(define group-8
  (let ((opcodes '#(#f #f #f #f BT BTS BTR BTC)))
    (lambda (second-byte)
      (let* ((modr/m-byte (next-unsigned-byte))
	     (op (vector-ref opcodes (modr/m-reg modr/m-byte))))
	(if (not op)
	    (unknown-inst #x0f second-byte modr/m-byte)
	    `(,op ,(decode-r/m modr/m-byte) (& ,(next-byte))))))))

;;; Utilities for the main dispatchers

(define (dispatch-on-bit low high)
  (lambda (opcode-byte)
    ((if (fix:= (fix:and opcode-byte #x8) 0) low high)
     opcode-byte)))

(define (dispatch-on-low-bits mask opcodes)
  (lambda (opcode-byte)
    ((vector-ref opcodes (fix:and opcode-byte mask))
     opcode-byte)))

(define (dispatch-on-low-nibble . cases)
  (if (not (= (length cases) 16))
      (error "dispatch-on-low-nibble: Wrong number of cases"
	     cases))
  (dispatch-on-low-bits #xf (list->vector cases)))

(define (dispatch-on-low-three-bits . cases)
  (if (not (= (length cases) 8))
      (error "dispatch-on-low-three-bits: Wrong number of cases"
	     cases))
  (dispatch-on-low-bits #x7 (list->vector cases)))

;;; Floating-point instructions

(define (fp-table-maker fields->index)
  (lambda (cases)
    (let ((table (make-vector 64 #f)))
      (for-each
       (lambda (a-case)
	 (let ((opcode (car a-case))
	       (next (cadr a-case)))
	   (let ((index (fields->index opcode next)))
	     (cond ((not index)
		    (error "make-table-1-3: Bad fields" a-case))
		   ((vector-ref table index)
		    (error "make-table-1-3: Duplicate case"
			   (vector-ref table index) a-case)))
	     (vector-set! table index (cddr a-case)))))
       cases)
      table)))

(define make-table-1-3
  (fp-table-maker
   (lambda (opcode next)
     (and (fix:< opcode 8)
	  (fix:< next 8)
	  (fix:or (fix:lsh next 3) opcode)))))

(define make-table-4&5
  (fp-table-maker
   (lambda (opcode next)
     (and (or (fix:= opcode 1) (fix:= opcode 3))
	  (fix:< next #x20)
	  (fix:or (fix:lsh (fix:- opcode 1) 4)
		  next)))))

(define decode-fp
  (let-syntax
      ((IN
	(rsc-macro-transformer
	 (lambda (form environment)
	   `(,(close-syntax 'LET environment)
	     ,(cddr form)
	     ,(cadr form))))))
    (IN
     (lambda (opcode-byte)
       (let* ((next (next-unsigned-byte))
	      (disc (fix:and opcode-byte #x7))
	      (index (fix:or (fix:and next #x38) disc)))
	 
	 (cond ((not (fix:= (modr/m-mod next) 3)) ; register op
		(let ((prefix (vector-ref table-1&2 index)))
		  (if (not prefix)
		      (maybe-special opcode-byte next)
		      `(,@prefix ,(decode-r/m next)))))
	       ((or (fix:= disc 3)
		    (and (fix:= disc 1)
			 (fix:= (fix:and next #x20) #x20)))
		(let ((inst (vector-ref
			     table-4&5
			     (fix:or (fix:lsh (fix:- disc 1) 4)
				     (fix:and next #x1f)))))
		  (if (not inst)
		      (maybe-special opcode-byte next)
		      inst)))
	       (else
		(let ((spec (vector-ref table-3 index))
		      (loc (fix:and next #x7)))
		  (cond ((not spec)
			 (maybe-special opcode-byte next))
			((null? (cdr spec))
			 `(,(car spec) (ST ,loc)))
			((cadr spec)		; reverse ops
			 `(,(car spec) (ST ,loc) (ST 0)))
			(else
			 `(,(car spec) (ST 0) (ST ,loc)))))))))

     (maybe-special
      (let ((special '(
		       (#xe0df FNSTSW (R 0))
		       (#xd0d9 FNOP)
		       )))
	(lambda (opcode-byte next)
	  (let* ((word (fix:or (fix:lsh next 8) opcode-byte))
		 (place (assq word special)))
	    (if place
		(cdr place)
		(unknown-inst opcode-byte next))))))
	

     (table-4&5
      (make-table-4&5
       '(
	 (1	4	FTST)
	 (1	5	FXAM)
	 (1	#xe	FLDZ)
	 (1	8	FLD1)
	 (1	#xb	FLDPI)
	 (1	9	FLD2T)
	 (1	#xa	FLD2E)
	 (1	#xc	FLDG2)
	 (1	#xd	FLDLN2)
	 (1	#x1a	FSQRT)
	 (1	#x1d	FSCALE)
	 (1	#x14	FXTRACT)
	 (1	#x18	FPREM)
	 (1	#x15	FPREM1)
	 (1	#x1c	FRNDINT)
	 (1	1	FABS)
	 (1	0	FCHS)
	 (1	#x1f	FCOS)
	 (1	#x12	FPTAN)
	 (1	#x13	FPATAN)
	 (1	#x1e	FSIN)
	 (1	#x1b	FSINCOS)
	 (1	#x10	F2XM1)
	 (1	#x11	FYL2X)
	 (1	#x19	FYL2XP1)
	 (3	3	FNINIT)
	 (3	2	FCLEX)
	 (1	#x17	FINCSTP)
	 (1	#x16	FDECSTP))))


     (table-3
      (make-table-1-3
       '(
	 (1 0 FLD)
	 (5 2 FST)
	 (5 3 FSTP)				; i486 book has 5 1
	 (1 1 FXCH #f)
	 (0 2 FCOM #f)
	 (0 3 FCOMP #f)
	 (6 3 FCOMPP #f)			; really only with (ST 1)
	 (5 4 FUCOM #f)
	 (5 5 FUCOMP #f)
	 (2 5 FUCOMPP #f)			; really only with (ST 1)
	 (0 0 FADD #f)
	 (4 0 FADD #t)
	 (6 0 FADDP #t)
	 (0 5 FSUB #f)
	 (4 5 FSUB #t)
	 (6 5 FSUBP #t)
	 (0 4 FSUBR #f)
	 (4 4 FSUBR #t)
	 (6 4 FSUBRP #t)
	 (0 1 FMUL #f)
	 (4 1 FMUL #t)
	 (6 1 FMULP #t)
	 (0 7 FDIV #f)
	 (4 7 FDIV #t)
	 (6 7 FDIVP #t)
	 (0 6 FDIVR #f)
	 (4 6 FDIVR #t)
	 (6 6 FDIVRP #t)
	 (5 0 FFREE))))

     (table-1&2
      (make-table-1-3
       '(
	 (1 0 FLD S)
	 (5 0 FLD D)
	 (3 5 FLD X)
	 (7 0 FILD H)
	 (3 0 FILD L)
	 (7 5 FILD Q)
	 (7 4 FBLD)
	 (1 2 FST S)
	 (5 2 FST D)
	 (1 3 FSTP S)				; i486 book has 3 3 like FISTP
	 (5 3 FSTP D)
	 (3 7 FSTP X)
	 (7 2 FIST H)
	 (3 2 FIST L)
	 (7 3 FISTP H)
	 (3 3 FISTP L)
	 (7 7 FISTP Q)
	 (7 6 FBSTP)
	 (0 2 FCOM S (ST 0))
	 (4 2 FCOM D (ST 0))
	 (0 3 FCOMP S (ST 0))
	 (4 3 FCOMP D (ST 0))
	 (6 2 FICOM H)
	 (2 2 FICOM L)
	 (6 3 FICOMP H)
	 (2 3 FICOMP L)
	 (0 0 FADD S)
	 (4 0 FADD D)
	 (0 4 FSUB S)
	 (4 4 FSUB D)
	 (0 5 FSUBR S)
	 (4 5 FSUBR D)
	 (0 1 FMUL S)
	 (4 1 FMUL D)
	 (0 6 FDIV S)
	 (4 6 FDIV D)				; i486 manual has 4 4 like FSUB
	 (0 7 FDIVR S)
	 (4 7 FDIVR D)
	 (6 0 FIADD H)
	 (2 0 FIADD L)
	 (6 4 FISUB H)
	 (2 4 FISUB L)
	 (6 5 FISUBR H)
	 (2 5 FISUBR L)
	 (6 1 FIMUL H)
	 (2 1 FIMUL L)
	 (6 6 FIDIV H)
	 (2 6 FIDIV L)
	 (6 7 FIDIVR H)
	 (2 7 FIDIVR L)
	 (5 7 FNSTSW)
	 (1 5 FLDCW)
	 (1 7 FNSTCW)
	 (1 6 FNSTENV)
	 (1 4 FLDENV)
	 (5 6 FNSAVE)
	 (5 4 FRSTOR)))))))

(define dispatch/0f
  (let* ((unknown-inst
	  (lambda (second-byte)
	    (unknown-inst #x0f second-byte)))
	 (table
	  (vector
	   (dispatch-on-low-nibble		; 0
	    (group-6&7 '#(SLDT STR LLDT LTR VERR VERW #f #f))
	    (group-6&7 '#(SGDT SIDT LGDT LIDT SMSW #f LMSW #f))
	    (decode-G/E '(LAR))
	    (decode-G/E '(LSL))
	    unknown-inst
	    unknown-inst
	    (simple-inst '(CLTS))
	    unknown-inst

	    (simple-inst '(INVD))
	    (simple-inst '(WBINVD))
	    unknown-inst
	    unknown-inst
	    unknown-inst
	    unknown-inst
	    unknown-inst
	    unknown-inst)

	   unknown-inst				; 1

	   (dispatch-on-bit			; 2
	    (dispatch-on-low-three-bits
	     (decode-X/E '(MOV) 'CR)
	     (decode-X/E '(MOV) 'DR)
	     (decode-E/X '(MOV) 'CR)
	     (decode-E/X '(MOV) 'DR)
	     (decode-X/E '(MOV) 'TR)
	     unknown-inst
	     (decode-E/X '(MOV) 'TR)
	     unknown-inst)
	    unknown-inst)

	   unknown-inst				; 3

	   unknown-inst				; 4

	   unknown-inst				; 5

	   unknown-inst				; 6

	   unknown-inst				; 7

	   (lambda (opcode-byte)		; 8
	     ((decode-pcrw
	       `(,(vector-ref jcc-opcodes (low-nibble opcode-byte))
		 W))
	      opcode-byte))

	   (lambda (opcode-byte)		; 9
	     ((decode-E
	       `(,(vector-ref setcc-opcodes (low-nibble opcode-byte))))
	      opcode-byte))

	   (dispatch-on-low-nibble		; A
	    (simple-inst '(PUSH FS))
	    (simple-inst '(POP FS))
	    (simple-inst '(CPUID))
	    (decode-E/G '(BT))
	    (decode-E/G/I '(SHLD) immediate-byte)
	    (decode-E/G/I '(SHLD) (lambda () '(R 1)))
	    (decode-E/G '(CMPXCHG B))
	    (decode-E/G '(CMPXCHG W))

	    (simple-inst '(PUSH GS))
	    (simple-inst '(POP GS))
	    unknown-inst
	    (decode-E/G '(BTS))
	    (decode-E/G/I '(SHRD) immediate-byte)
	    (decode-E/G/I '(SHRD) (lambda () '(R 1)))
	    unknown-inst
	    (decode-G/E '(IMUL W)))

	   (dispatch-on-low-nibble		; B
	    unknown-inst
	    unknown-inst
	    (decode-G/M '(LSS))
	    (decode-E/G '(BTR))
	    (decode-G/M '(LFS))
	    (decode-G/M '(LGS))
	    (decode-G/E '(MOVZX B))
	    (decode-G/E '(MOVZX W))

	    unknown-inst
	    unknown-inst
	    group-8
	    (decode-E/G '(BTC))
	    (decode-G/E '(BSF))
	    (decode-G/E '(BSR))
	    (decode-G/E '(MOVSX B))
	    (decode-G/E '(MOVSX W)))

	   (dispatch-on-bit			; C
	    (dispatch-on-low-three-bits
	     (decode-E/G '(XADD B))
	     (decode-E/G '(XADD W))
	     unknown-inst
	     unknown-inst
	     unknown-inst
	     unknown-inst
	     unknown-inst
	     unknown-inst)
	    (register-op '(BSWAP)))

	   unknown-inst				; D

	   unknown-inst				; E

	   unknown-inst)))			; F

    (lambda (opcode-byte)
      opcode-byte			; ignored
      (let ((next (next-unsigned-byte)))
	((vector-ref table (high-nibble next))
	 next)))))

(define disassemble-next-instruction
  (let* ((arith-opcodes
	  '#(ADD OR ADC SBB AND SUB XOR CMP))
	 (shift-opcodes
	  '#(ROL ROR RCL RCR SHL SHR SAL SAR))
	 (table
	  (vector
	   (dispatch-on-low-nibble		; 0
	    (decode-E/G '(ADD B))
	    (decode-E/G '(ADD W))
	    (decode-G/E '(ADD B))
	    (decode-G/E '(ADD W))
	    (decode-Ib '(ADD B (R 0)))
	    (decode-Iw '(ADD W (R 0)))
	    (simple-inst '(PUSH ES))
	    (simple-inst '(POP ES))

	    (decode-E/G '(OR B))
	    (decode-E/G '(OR W))
	    (decode-G/E '(OR B))
	    (decode-G/E '(OR W))
	    (decode-Ib '(OR B (R 0)))
	    (decode-Iw '(OR W (R 0)))
	    (simple-inst '(PUSH CS))
	    dispatch/0f)

	   (dispatch-on-low-nibble		; 1
	    (decode-E/G '(ADC B))
	    (decode-E/G '(ADC W))
	    (decode-G/E '(ADC B))
	    (decode-G/E '(ADC W))
	    (decode-Ib '(ADC B (R 0)))
	    (decode-Iw '(ADC W (R 0)))
	    (simple-inst '(PUSH SS))
	    (simple-inst '(POP SS))

	    (decode-E/G '(SBB B))
	    (decode-E/G '(SBB W))
	    (decode-G/E '(SBB B))
	    (decode-G/E '(SBB W))
	    (decode-Ib '(SBB B (R 0)))
	    (decode-Iw '(SBB W (R 0)))
	    (simple-inst '(PUSH DS))
	    (simple-inst '(POP DS)))

	   (dispatch-on-low-nibble		; 2
	    (decode-E/G '(AND B))
	    (decode-E/G '(AND W))
	    (decode-G/E '(AND B))
	    (decode-G/E '(AND W))
	    (decode-Ib '(AND B (R 0)))
	    (decode-Iw '(AND W (R 0)))
	    (simple-inst '(ESSEG))
	    (simple-inst '(DAA))

	    (decode-E/G '(SUB B))
	    (decode-E/G '(SUB W))
	    (decode-G/E '(SUB B))
	    (decode-G/E '(SUB W))
	    (decode-Ib '(SUB B (R 0)))
	    (decode-Iw '(AND W (R 0)))
	    (simple-inst '(CSSEG))
	    (simple-inst '(DAS)))

	   (dispatch-on-low-nibble		; 3
	    (decode-E/G '(XOR B))
	    (decode-E/G '(XOR W))
	    (decode-G/E '(XOR B))
	    (decode-G/E '(XOR W))
	    (decode-Ib '(XOR B (R 0)))
	    (decode-Iw '(XOR W (R 0)))
	    (simple-inst '(SSSEG))
	    (simple-inst '(AAA))

	    (decode-E/G '(CMP B))
	    (decode-E/G '(CMP W))
	    (decode-G/E '(CMP B))
	    (decode-G/E '(CMP W))
	    (decode-Ib '(CMP B (R 0)))
	    (decode-Iw '(CMP W (R 0)))
	    (simple-inst '(DSSEG))
	    (simple-inst '(AAS)))

	   (dispatch-on-bit			; 4
	     (register-op '(INC))
	     (register-op '(DEC)))

	   (dispatch-on-bit			; 5
	     (register-op '(PUSH))
	     (register-op '(POP)))

	   (dispatch-on-low-nibble		; 6
	    (simple-inst '(PUSHA))
	    (simple-inst '(POPA))
	    (decode-G/M '(BOUND))
	    (decode-E/G '(ARPL))
	    (simple-inst '(FSSEG))
	    (simple-inst '(GSSEG))
	    (simple-inst '(OPSIZE))
	    (simple-inst '(ADSIZE))

	    (decode-Iw '(PUSH W))
	    (decode-G/E/I '(IMUL W) immediate-word)
	    (decode-Ib '(PUSH B))
	    (decode-G/E/I '(IMUL B) immediate-byte)
	    (simple-inst '(INS B))
	    (simple-inst '(INS W))
	    (simple-inst '(OUTS B))
	    (simple-inst '(OUTS W)))

	   (lambda (opcode-byte)		; 7
	     ((decode-pcrb
	       `(,(vector-ref jcc-opcodes (low-nibble opcode-byte))
		 B))
	      opcode-byte))

	   (dispatch-on-low-nibble		; 8
	    (group-1&2 arith-opcodes 'B immediate-byte)
	    (group-1&2 arith-opcodes 'W immediate-word)
	    (decode-Ib '(MOV B (R 0)))
	    (group-1&2 arith-opcodes 'W immediate-byte)
	    (decode-E/G '(TEST B))
	    (decode-E/G '(TEST W))
	    (decode-E/G '(XCHG B))
	    (decode-E/G '(XCHG W))

	    (decode-E/G '(MOV B))
	    (decode-E/G '(MOV W))
	    (decode-G/E '(MOV B))
	    (decode-G/E '(MOV W))
	    (decode-E/X '(MOV) 'SR)
	    (decode-G/M '(LEA))
	    (decode-X/E '(MOV) 'SR)
	    (decode-E '(POP) 0))

	   (dispatch-on-bit			; 9
	    (register-op '(XCHG W (R 0)))
	    (dispatch-on-low-three-bits
	     (simple-inst '(CBW))
	     (simple-inst '(CWDE))
	     (decode-Ap '(CALL F))
	     (simple-inst '(WAIT))
	     (simple-inst '(PUSHF))
	     (simple-inst '(POPF))
	     (simple-inst '(SAHF))
	     (simple-inst '(LAHF))))

	   (dispatch-on-low-nibble		; A
	    (decode-@ '(MOV B (R 0)))
	    (decode-@ '(MOV W (R 0)))
	    (backwards
	     (decode-@ '(MOV B (R 0))))
	    (backwards
	     (decode-@ '(MOV W (R 0))))
	    (simple-inst '(MOVSB))
	    (simple-inst '(MOVSW))
	    (simple-inst '(CMPSB))
	    (simple-inst '(CMPSW))

	    (decode-Ib '(TEST B (R 0)))
	    (decode-Iw '(TEST W (R 0)))
	    (simple-inst '(STOS B))
	    (simple-inst '(STOS W))
	    (simple-inst '(LODS B))
	    (simple-inst '(LODS W))
	    (simple-inst '(SCAS B))
	    (simple-inst '(SCAS W)))

	   (dispatch-on-bit			; B
	     (lambda (opcode)
	       ((decode-Ib
		 `(MOV B (R ,(fix:and opcode #x7))))
		opcode))
	     (lambda (opcode)
	       ((decode-Iw
		 `(MOV W (R ,(fix:and opcode #x7))))
		opcode)))

	   (dispatch-on-low-nibble		; C
	    (group-1&2 shift-opcodes 'B immediate-byte)
	    (group-1&2 shift-opcodes 'W immediate-byte)
	    (decode-I16 '(RET))
	    (simple-inst '(RET))
	    (decode-G/M '(LES))
	    (decode-G/M '(LDS))
	    (decode-E/I '(MOV B) next-byte)
	    (decode-E/I '(MOV W) next-word)

	    decode-ENTER
	    (simple-inst '(LEAVE))
	    (decode-I16 '(RET F))
	    (simple-inst '(RET F))
	    (simple-inst '(INT 3))
	    (decode-Ib '(INT))
	    (simple-inst '(INTO))
	    (simple-inst '(IRET)))

	   (dispatch-on-bit			; D
	    (dispatch-on-low-three-bits
	     (group-1&2 shift-opcodes 'B (lambda () '(& 1)))
	     (group-1&2 shift-opcodes 'W (lambda () '(& 1)))
	     (group-1&2 shift-opcodes 'B (lambda () '(R 1)))
	     (group-1&2 shift-opcodes 'W (lambda () '(R 1)))
	     (simple-inst '(AAM))
	     (simple-inst '(AAD))
	     unknown-inst
	     (simple-inst '(XLAT)))
	    decode-fp)

	   (dispatch-on-low-nibble		; E
	    (decode-pcrb '(LOOPNE))
	    (decode-pcrb '(LOOPE))
	    (decode-pcrb '(LOOP))
	    (decode-pcrb '(JCXZ))
	    (decode-Ib '(IN B (R 0)))
	    (decode-Iw '(IN W (R 0)))
	    (backwards (decode-Ib '(OUT B (R 0))))
	    (backwards (decode-IW '(OUT W (R 0))))

	    (decode-pcrw '(CALL))
	    (decode-pcrw '(JMP W))
	    (decode-ap '(JMP F))
	    (decode-pcrb '(JMP B))
	    (simple-inst '(IN B (R 0) (R 2)))
	    (simple-inst '(IN W (R 0) (R 2)))
	    (simple-inst '(OUT B (R 2) (R 0)))
	    (simple-inst '(OUT W (R 2) (R 0))))

	   (dispatch-on-low-nibble		; F
	    (simple-inst '(LOCK))
	    unknown-inst
	    (simple-inst '(REPNE))
	    (simple-inst '(REPE))
	    (simple-inst '(HLT))
	    (simple-inst '(CMC))
	    (group-3 'B next-byte)
	    (group-3 'W next-word)

	    (simple-inst '(CLC))
	    (simple-inst '(STC))
	    (simple-inst '(CLI))
	    (simple-inst '(STI))
	    (simple-inst '(CLD))
	    (simple-inst '(STD))
	    (group-4 'B)
	    (group-5 'W)))))

    (lambda ()
      (let ((opcode-byte (next-unsigned-byte)))
	((vector-ref table (high-nibble opcode-byte))
	 opcode-byte)))))