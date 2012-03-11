#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; 68000 Disassembler: Internals

(declare (usual-integrations))

(define opcode-dispatch
  (vector (lambda ()
	    ((vector-ref bit-manipulation/MOVEP/immediate-dispatch
			 (extract *ir 8 12))))
	  (lambda () %MOVE-byte)
	  (lambda () %MOVE-long)
	  (lambda () %MOVE-word)
	  (lambda ()
	    ((vector-ref miscellaneous-dispatch (extract *ir 8 12))))
	  (lambda ()
	    (if (= (extract *ir 6 8) #b11)
		(if (= (extract *ir 3 6) #b001)
		    %DBcc
		    %Scc)
		(if (= (extract *ir 8 9) #b0)
		    %ADDQ
		    %SUBQ)))
	  (lambda () %Bcc/%BSR)
	  (lambda ()
	    (if (= (extract *ir 8 9) #b0)
		%MOVEQ
		undefined-instruction))
	  (lambda ()
	    (let ((size (extract *ir 6 8)))
	      (cond ((= size #b00)
		     (if (= (extract *ir 4 6) #b00)
			 %SBCD
			 %OR))
		    ((= size #b11) %DIV)
		    (else %OR))))
	  (lambda ()
	    (if (= (extract *ir 6 8) #b11)
		%SUBA
		(if (and (= (extract *ir 8 9) #b1)
			 (= (extract *ir 4 6) #b00))
		    %SUBX
		    %SUB)))
	  undefined
	  (lambda ()
	    (if (= (extract *ir 6 8) #b11)
		%CMPA
		(if (= (extract *ir 8 9) 0)
		    %CMP
		    (if (= (extract *ir 3 6) #b001)
			%CMPM
			%EOR))))
	  (lambda ()
	    (let ((size (extract *ir 6 8)))
	      (cond ((= size #b00)
		     (if (= (extract *ir 4 6) #b00)
			 %ABCD
			 %AND))
		    ((= size #b01)
		     (if (= (extract *ir 4 6) #b00)
			 %EXG
			 %AND))
		    ((= size #b10)
		     (if (= (extract *ir 3 6) #b001)
			 %EXGM
			 %AND))
		    (else %MUL))))
	  (lambda ()
	    (if (= (extract *ir 6 8) #b11)
		%ADDA
		(if (and (= (extract *ir 8 9) #b1)
			 (= (extract *ir 4 6) #b00))
		    %ADDX
		    %ADD)))
	  (lambda () shift/rotate/bitop)
	  (lambda () coprocessor)))

;;;; Operations

(define bit-manipulation/MOVEP/immediate-dispatch
  (let ((ORI (lambda () %ORI))
	(ANDI (lambda () %ANDI))
	(SUBI (lambda () %SUBI))
	(ADDI (lambda () %ADDI))
	(EORI (lambda () %EORI))
	(CMPI (lambda () %CMPI))
	(dynamic-bit/MOVEP
	 (lambda ()
	   (if (= (extract *ir 3 6) 1)
	       %MOVEP
	       dynamic-bit)))
	(static-bit (lambda () static-bit)))
    (vector ORI
	    dynamic-bit/MOVEP
	    ANDI
	    dynamic-bit/MOVEP
	    SUBI
	    dynamic-bit/MOVEP
	    ADDI
	    dynamic-bit/MOVEP
	    static-bit
	    dynamic-bit/MOVEP
	    EORI
	    dynamic-bit/MOVEP
	    CMPI
	    dynamic-bit/MOVEP
	    undefined
	    dynamic-bit/MOVEP)))

(define (dynamic-bit)
  `(,(decode-bit (extract *ir 6 8))
    ,(make-data-register 'D (extract *ir 9 12))
    ,(decode-ea-d&a)))

(define (static-bit)
  (let ((ea (decode-ea-d&a)))
    `(,(decode-bit (extract *ir 6 8))
      (& ,(fetch-immediate 'B))
      ,ea)))

(define (%MOVEP)
  `(MOVEP ,(decode-wl (extract *ir 6 7))
	  ,@(let ((data-register (extract *ir 9 12))
		  (address-register (extract *ir 0 3))
		  (offset (bit-string->signed-integer (get-word))))
	      (if (zero? (extract *ir 7 8))
		  `(,(make-address-offset address-register offset)
		    ,(make-data-register 'D data-register))
		  `(,(make-data-register 'D data-register)
		    ,(make-address-offset address-register offset))))))


(define ((logical-immediate keyword))
  (let ((size (decode-bwl (extract *ir 6 8))))
    (cond ((null? size)
	   (undefined-instruction))
	  ((= (extract *ir 0 6) #b111100)
	   (if (eq? size 'L)
	       (undefined-instruction)
	       `(,keyword ,size (& ,(fetch-immediate size)) (SR))))
	  (else
	   (let ((immediate (fetch-immediate size)))
	     `(,keyword ,size (& ,immediate) ,(decode-ea-d&a)))))))

(define %ORI (logical-immediate 'ORI))
(define %ANDI (logical-immediate 'ANDI))
(define %EORI (logical-immediate 'EORI))

(define ((arithmetic-immediate keyword))
  (let ((size (decode-bwl (extract *ir 6 8))))
    (if (null? size)
	(undefined-instruction)
	(let ((immediate (fetch-immediate size)))
	  `(,keyword ,size (& ,immediate) ,(decode-ea-d&a))))))

(define %SUBI (arithmetic-immediate 'SUBI))
(define %ADDI (arithmetic-immediate 'ADDI))
(define %CMPI (arithmetic-immediate 'CMPI))

(define ((%MOVE size))
  (let ((sea (decode-ea-b=>-A size)))
    (let ((dea (decode-ea-MOVE-destination size)))
      `(MOVE ,size ,sea ,dea))))

(define %MOVE-byte (%MOVE 'B))
(define %MOVE-word (%MOVE 'W))
(define %MOVE-long (%MOVE 'L))

(define miscellaneous-dispatch
  (let ((NEGX/MOVE<-SR
	 (lambda ()
	   (if (= (extract *ir 6 8) #b11) %MOVE<-SR %NEGX)))
	(CLR (lambda () %CLR))
	(NEG/MOVE->CCR
	 (lambda ()
	   (if (= (extract *ir 6 8) #b11) %MOVE->CCR %NEG)))
	(NOT/MOVE->SR
	 (lambda ()
	   (if (= (extract *ir 6 8) #b11) %MOVE->SR %NOT)))
	(NBCD/PEA/SWAP/MOVEM-registers->ea/EXT
	 (lambda ()
	   (if (= (extract *ir 7 8) 0)
	       (if (= (extract *ir 6 7) 0)
		   %NBCD
		   (if (= (extract *ir 3 6) 0)
		       %SWAP
		       %PEA))
	       (if (= (extract *ir 3 6) 0)
		   %EXT
		   %MOVEM-registers->ea))))
	(TST/TAS/illegal
	 (lambda ()
	   (if (not (= (extract *ir 6 8) #b11))
	       %TST
	       (if (not (= (extract *ir 0 6) #b111100))
		   %TAS
		   %ILLEGAL))))
	(MULL/DIVL/MOVEM-ea->registers
	 (lambda ()
	   (case (extract *ir 6 8)
	     ((#b00) %MULL)
	     ((#b01) %DIVL)
	     ((#b11) %MOVEM-ea->registers)
	     (else undefined-instruction))))
	(all-the-rest
	 (lambda ()
	   ((vector-ref all-the-rest-dispatch (extract *ir 6 8)))))
	(CHK/LEA
	 (lambda ()
	   ((vector-ref CHK/LEA-dispatch (extract *ir 6 8))))))
    (vector NEGX/MOVE<-SR
	    CHK/LEA
	    CLR
	    CHK/LEA
	    NEG/MOVE->CCR
	    CHK/LEA
	    NOT/MOVE->SR
	    CHK/LEA
	    NBCD/PEA/SWAP/MOVEM-registers->ea/EXT
	    CHK/LEA
	    TST/TAS/illegal
	    CHK/LEA
	    MULL/DIVL/MOVEM-ea->registers
	    CHK/LEA
	    all-the-rest
	    CHK/LEA)))

(define all-the-rest-dispatch
  (vector undefined
	  (lambda () ((vector-ref all-the-rest-1-dispatch (extract *ir 3 6))))
	  (lambda () %JSR)
	  (lambda () %JMP)))

(define all-the-rest-1-dispatch
  (vector (lambda () %TRAP)
	  (lambda () %TRAP)
	  (lambda () %LINK)
	  (lambda () %UNLK)
	  (lambda () %MOVE->USP)
	  (lambda () %MOVE<-USP)
	  (lambda ()
	    (let ((register (extract *ir 0 3)))
	      (if (= register #b100)
		  undefined-instruction
		  (lambda ()
		    `(,(vector-ref #(RESET NOP STOP RTE () RTS TRAPV RTR)
				   register))))))
	  undefined))

(define ((single-ea-d&a keyword))
  `(,keyword ,(decode-bwl (extract *ir 6 8))
	     ,(decode-ea-d&a)))

(define %NEGX (single-ea-d&a 'NEGX))
(define %CLR (single-ea-d&a 'CLR))
(define %NEG (single-ea-d&a 'NEG))
(define %NOT (single-ea-d&a 'NOT))
(define %TST (single-ea-d&a 'TST))


(define (%MOVE<-SR)
  `(MOVE W (SR) ,(decode-ea-d&a)))

(define (%MOVE->CCR)
  `(MOVE W ,(decode-ea-d 'W) (CCR)))

(define (%MOVE->SR)
  `(MOVE W ,(decode-ea-d 'W) (SR)))

(define (%NBCD)
  `(NBCD ,(decode-ea-d&a)))

(define (%SWAP)
  `(SWAP ,(make-data-register 'D (extract *ir 0 3))))

(define (%PEA)
  `(PEA ,(decode-ea-c)))

(define (%EXT)
  `(EXT ,(decode-wl (extract *ir 6 7))
	,(make-data-register 'D (extract *ir 0 3))))

(define (%TAS)
  `(TAS B ,(decode-ea-d&a)))

(define (%ILLEGAL)
  '(ILLEGAL))

(define (%TRAP)
  `(TRAP (& ,(extract *ir 0 4))))

(define (%LINK)
  `(LINK ,(make-address-register 'A (extract *ir 0 3))))

(define (%UNLK)
  `(UNLK ,(make-address-register 'A (extract *ir 0 3))))

(define (%MOVE->USP)
  `(MOVE L ,(make-address-register 'A (extract *ir 0 3)) (USP)))

(define (%MOVE<-USP)
  `(MOVE L (USP) ,(make-address-register 'A (extract *ir 0 3))))

(define (%JSR)
  `(JSR ,(decode-ea-c)))

(define (%JMP)
  `(JMP ,(decode-ea-c)))

(define (%MOVEM-registers->ea)
  (let ((mode (extract *ir 3 6))
	(size (decode-wl (extract *ir 6 7))))
    (if (= mode 4)
	`(MOVEM ,size
		,(decode-@-aregister-list (get-word))
		(make-address-register '@-A (extract *ir 0 3)))
	(let ((ea (decode-ea-c)))
	  `(MOVEM ,size
		  ,(decode-c@a+register-list (get-word))
		  ,ea)))))

(define (%MOVEM-ea->registers)
  (let ((mode (extract *ir 3 6))
	(size (decode-wl (extract *ir 6 7))))
    (let ((ea (if (= mode #b011)
		  (make-address-register '@A+ (extract *ir 0 3))
		  (decode-ea-c&a size))))
      `(MOVEM ,size ,ea ,(decode-c@a+register-list (get-word))))))

(define (decode-@-aregister-list word)
  (define (loop n registers)
    (if (null? registers)
	'()
	(if (zero? (bit-string-ref word n))
	    (loop (1+ n) (cdr registers))
	    (cons (car registers)
		  (loop (1+ n) (cdr registers))))))
  (loop 0 '(A7 A6 A5 A4 A3 A2 A1 A0 D7 D6 D5 D4 D3 D2 D1 D0)))

(define (decode-c@a+register-list word)
  (define (loop n registers)
    (if (null? registers)
	'()
	(if (zero? (bit-string-ref word n))
	    (loop (1+ n) (cdr registers))
	    (cons (car registers)
		  (loop (1+ n) (cdr registers))))))
  (loop 0 '(D0 D1 D2 D3 D4 D5 D6 D7 A0 A1 A2 A3 A4 A5 A6 A7)))

(define CHK/LEA-dispatch
  (vector undefined
	  undefined
	  (lambda () %CHK)
	  (lambda () %LEA)))

(define (%CHK)
  `(CHK ,(decode-ea-d 'W)
	,(make-data-register 'D (extract *ir 9 12))))

(define (%LEA)
  `(LEA ,(decode-ea-c)
	,(make-address-register 'A (extract *ir 9 12))))

(define (%Scc)
  `(S ,(decode-cc (extract *ir 8 12))
      ,(decode-ea-d&a)))

(define (%DBcc)
  `(DB ,(decode-cc (extract *ir 8 12))
       ,(make-data-register 'D (extract *ir 0 3))
       ,(make-pc-relative (lambda () (fetch-immediate 'W)))))

(define (%Bcc/%BSR)
  (let ((cc (decode-cc (extract *ir 8 12)))
	(displacement (extract+ *ir 0 8)))
    ((access append system-global-environment)
     (cond ((eq? cc 'T) '(BRA))
	   ((eq? cc 'F) '(BSR))
	   (else `(B , cc)))
     (cond ((= displacement 0)
	    `(W ,(make-pc-relative (lambda () (fetch-immediate 'W)))))
	   ((= displacement -1)
	    `(L ,(make-pc-relative (lambda () (fetch-immediate 'L)))))
	   (else
	    `(B ,(make-pc-relative (lambda () displacement))))))))

(define (%MOVEQ)
  `(MOVEQ (& ,(extract+ *ir 0 8))
	  ,(make-data-register 'D (extract *ir 9 12))))

(define ((logical keyword))
  (let ((size (decode-bwl (extract *ir 6 8)))
	(register (extract *ir 9 12)))
    (if (= (extract *ir 8 9) #b0)
	`(,keyword ,size
		   ,(decode-ea-d size)
		   ,(make-data-register 'D register))
	`(,keyword ,size
		   ,(make-data-register 'D register)
		   ,(decode-ea-m&a)))))

(define %OR (logical 'OR))
(define %AND (logical 'AND))

(define (%EOR)
  `(EOR ,(decode-bwl (extract *ir 6 8))
	,(make-data-register 'D (extract *ir 9 12))
	,(decode-ea-d&a)))

(define ((binary keyword))
  (let ((size (decode-bwl (extract *ir 6 8)))
	(register (extract *ir 9 12)))
    (if (= (extract *ir 8 9) #b0)
	`(,keyword ,size
		   ,(decode-ea-b=>-A size)
		   ,(make-data-register 'D register))
	`(,keyword ,size
		   ,(make-data-register 'D register)
		   ,(decode-ea-m&a)))))

(define %ADD (binary 'ADD))
(define %SUB (binary 'SUB))

(define (%CMP)
  (let ((size (decode-bwl (extract *ir 6 8))))
    `(CMP ,size
	  ,(decode-ea-b=>-A size)
	  ,(make-data-register 'D (extract *ir 9 12)))))

(define ((binary-address keyword))
  (let ((size (decode-wl (extract *ir 8 9))))
    `(,keyword ,size
	       ,(decode-ea-all size)
	       ,(make-address-register 'A (extract *ir 9 12)))))

(define %ADDA (binary-address 'ADD))
(define %SUBA (binary-address 'SUB))
(define %CMPA (binary-address 'CMP))

(define ((binary-extended keyword))
  (define (receiver mode maker)
    `(,keyword ,(decode-bwl (extract *ir 6 8))
	       ,(maker mode (extract *ir 0 3))
	       ,(maker mode (extract *ir 9 12))))
  (if (= (extract *ir 3 4) #b0) 
      (receiver 'D make-data-register)
      (receiver '@-A make-address-register)))

(define %ADDX (binary-extended 'ADDX))
(define %SUBX (binary-extended 'SUBX))

(define (%CMPM)
  `(CMPM ,(decode-bwl (extract *ir 6 8))
	 ,(make-address-register '@A+ (extract *ir 0 3))
	 ,(make-address-register '@A+ (extract *ir 9 12))))

(define ((binary-quick keyword))
  (let ((size (decode-bwl (extract *ir 6 8))))
    `(,keyword ,size
	       (& ,(let ((n (extract *ir 9 12)))
		     (if (zero? n) 8 n)))
	       ,(decode-ea-a&<b=>-A> size))))

(define %ADDQ (binary-quick 'ADDQ))
(define %SUBQ (binary-quick 'SUBQ))

(define ((decimal keyword))
  (define (receiver mode maker)
    `(,keyword ,(maker mode (extract *ir 0 3))
	       ,(maker mode (extract *ir 9 12))))
  (if (= (extract *ir 3 4) #b0)
      (receiver 'D make-data-register)
      (receiver '@A- make-address-register)))

(define %ABCD (decimal 'ABCD))
(define %SBCD (decimal 'SBCD))

(define ((%MUL/%DIV keyword))
  `(,keyword ,(decode-us (extract *ir 8 9))
	     W
	     ,(decode-ea-d 'W)
	     ,(make-data-register 'D (extract *ir 9 12))))

(define %MUL (%MUL/%DIV 'MUL))
(define %DIV (%MUL/%DIV 'DIV))

(define ((%MULL/%DIVL force-short? keyword1 keyword2))
  (let ((next (get-word)))
    (let ((dr (extract next 0 3))
	  (dq (extract next 12 15)))
      (cond ((= (extract next 10 11) #b1)
	     `(,keyword1 ,(decode-us (extract next 11 12))
			 L
			 ,(decode-ea-d 'L)
			 ,(make-data-register 'D dr)
			 ,(make-data-register 'D dq)))
	    ((or force-short? (= dr dq))
	     `(,keyword1 ,(decode-us (extract next 11 12))
			 L
			 ,(decode-ea-d 'L)
			 ,(make-data-register 'D dq)))
	    (else
	     `(,keyword2 ,(decode-us (extract next 11 12))
			 L
			 ,(decode-ea-d 'L)
			 ,(make-data-register 'D dr)
			 ,(make-data-register 'D dq)))))))

(define %MULL (%MULL/%DIVL true 'MUL 'MULL))
(define %DIVL (%MULL/%DIVL false 'DIV 'DIVL))

(define (%EXG)
  (let ((mode (if (= (extract *ir 3 4) #b0) 'D 'A)))
    `(EXG (,mode ,(extract *ir 0 3))
	  (,mode ,(extract *ir 9 12)))))

(define (%EXGM)
  `(EXG ,(make-address-register 'A (extract *ir 0 3))
	,(make-data-register 'D (extract *ir 9 12))))

(define (shift/rotate/bitop)
  (if (= #b11 (extract *ir 6 8))
      (bit-extract)
      (shift-rotate)))

(define (shift-rotate)
  (let ((size (decode-bwl (extract *ir 6 8)))
	(direction (decode-rl (extract *ir 8 9))))
    (if (null? size)
	`(,(decode-shift-type (extract *ir 9 11))
	  ,direction
	  ,(decode-ea-m&a))
	`(,(decode-shift-type (extract *ir 3 5))
	  ,direction
	  ,size
	  ,(if (= (extract *ir 5 6) #b0)
	       `(& ,(let ((n (extract *ir 9 12)))
		      (if (zero? n) 8 n)))
	       `,(make-data-register 'D (extract *ir 9 12)))
	  ,(make-data-register 'D (extract *ir 0 3))))))

(define (bit-extract)
  (let* ((opcode (decode-bf (extract *ir 8 11)))
	 (extension (get-word))
	 (source (decode-ea-m&d)))
    (let ((target (if (memq opcode '(BFEXTS BFEXTU BFFFO BFINS))
		      `(,(make-data-register 'D
					     (extract extension 12 15)))
		      '()))
	  (offset (if (= #b0 (extract extension 11 12))
		      `(& ,(extract extension 6 11))
		      (make-data-register 'D (extract extension 6 9))))
	  (width (if (= #b0 (extract extension 5 6))
		     `(& ,(extract extension 0 5))
		     (make-data-register 'D (extract extension 0 3)))))
      `(,opcode ,source ,offset ,width ,@target))))

;;;
;;; COPROCESSOR
;;;

(define (coprocessor)
  (if (= (coprocessor-id) floating-point-coprocessor-id)
      (floating-point-coprocessor)
      (undefined-instruction)))

;;;
;;; FLOATING POINT INSTRUCTIONS
;;;

(define floating-point-coprocessor-id #b001)

(define (coprocessor-id)
  (extract *ir 9 12))

(define (floating-point-coprocessor)
  (let* ((op-class-indicator (extract *ir 6 9))
	 (opcode (extract (peek-word) 0 7)))
    (cond ((and (= op-class-indicator #b000)
		(= opcode #b0000000))
	   (let ((ext (get-word)))
	     (let ((keyword (get-fmove-keyword *ir ext)))
	       (if (null? keyword)
		   (undefined-instruction)
		   (case keyword
		     (FMOVE-TO-FP
		      (decode-ordinary-floating-instruction 'FMOVE ext))
		     (FMOVE-FROM-FP
		      (let ((dst-fmt (floating-specifier->mnemonic
				      (extract ext 10 13)))
			    (src-reg (extract ext 7 10)))
			(if (eq? dst-fmt 'P)
			    '(FMOVE packed decimal)
			    `(FMOVE ,dst-fmt
				    (FP ,src-reg)
				    ,(decode-ea-d 'L)))))
		     (FMOVE-FPcr
		      (let ((reg
			     (cdr (assoc (extract ext 10 13) 
					 '((#b001 . FPIAR)
					   (#b010 . FPSR)
					   (#b100 . FPCR))))))
			(if (= (extract ext 13 14) 1)
			    `(FMOVE ,reg ,(decode-ea-d 'L))
			    `(FMOVE ,(decode-ea-d 'L) ,reg))))
		     (FMOVECR
		      `(FMOVECR X (& ,(extract ext 0 7))
				(FP ,(extract ext 7 10))))
		     (FMOVEM-FPn
		      '(FMOVEM to FP-s))
		     (FMOVEM-FPcr
		      '(FMOVEM to CR-s)))))))
	  ((= op-class-indicator #b000)
	   (let ((ext (get-word))
		 (opcode-name (floating-opcode->mnemonic opcode)))
	     (decode-ordinary-floating-instruction opcode-name ext)))
	  ((= (extract *ir 7 9) #b01)
	   (let ((float-cc (decode-float-cc (extract *ir 0 6)))
		 (size (extract *ir 6 7)))
	     ((access append system-global-environment)
	      `(FB ,float-cc)
	      (if (= size 0)
		  `(W ,(make-pc-relative (lambda () (fetch-immediate 'W))))
		  `(L ,(make-pc-relative (lambda () (fetch-immediate 'L))))))))
	  (else
	   (undefined-instruction)))))

(define (decode-ordinary-floating-instruction opcode-name ext)
  (let ((src-spec (extract ext 10 13))
	(rm (extract ext 14 15))
	(dst-reg (extract ext 7 10)))
    (if (= rm 1)
	`(,opcode-name
	  ,(floating-specifier->mnemonic src-spec)
	  ,(decode-ea-d 'L)
	  (FP ,dst-reg))
	`(,opcode-name (FP ,src-spec) (FP ,dst-reg)))))

(define (floating-opcode->mnemonic n)
  (let ((entry (assoc n 
		      '((#b0011000 . FABS)
			(#b0011100 . FACOS)
			(#b0100010 . FADD)
			(#b0001100 . FASIN)
			(#b0001010 . FATAN)
			(#b0001101 . FATANH)
			(#b0111000 . FCMP)
			(#b0011101 . FCOS)
			(#b0011001 . FCOSH)
			(#b0100000 . FDIV)
			(#b0010000 . FETOX)
			(#b0001000 . FETOXM1)
			(#b0011110 . FGETEXP)
			(#b0011111 . FGETMAN)
			(#b0000001 . FINT)
			(#b0000011 . FINTRZ)
			(#b0010101 . FLOG10)
			(#b0010110 . FLOG2)
			(#b0010100 . FLOGN)
			(#b0000110 . FLOGNP1)
			(#b0100001 . FMOD)
			(#b0100011 . FMUL)
			(#b0011010 . FNEG)
			(#b0100101 . FREM)
			(#b0100110 . FSCALE)
			(#b0100100 . FSGLDIV)
			(#b0100111 . FSGLMUL)
			(#b0001110 . FSIN)
			(#b0000010 . FSINH)
			(#b0000100 . FSQRT)
			(#b0101000 . FSUB)
			(#b0001111 . FTAN)
			(#b0001001 . FTANH)
			(#b0010010 . FTENTOX)
			(#b0111010 . FTST)
			(#b0010001 . FTWOTOX)))))
    (and entry
	 (cdr entry))))

(define (floating-specifier->mnemonic n)
  (let ((entry (assoc n 
		      '((0 . L)
			(1 . S)
			(2 . X)
			(3 . P)
			(4 . W)
			(5 . D)
			(6 . B)))))
    (and entry
	 (cdr entry))))

(define (decode-float-cc bits)
  (cdr (or (assv bits
		 '((1 . EQ) (14 . NE)
		   (2 . GT) (13 . NGT)
		   (3 . GE) (12 . NGE)
		   (4 . LT) (11 . NLT)
		   (5 . LE) (10 . NLE)
		   (6 . GL) (9 . NGL)
		   (4 . MI) (3 . PL)
		   (7 . GLE) (8 . NGLE)
		   (0 . F) (15 . T)))
      (error "DECODE-FLOAT-CC: Unrecognized floating point condition code"
	     bits))))

(define (match-bits? high low pattern-list)
  (let high-loop ((i 15) (l pattern-list))
    (cond ((< i 0)
	   (let low-loop ((i 15) (l l))
	     (cond ((< i 0) #t)
		   ((or (eq? (car l) '?)
			(eq? (if (bit-string-ref low i) 1 0)
			     (car l)))
		    (low-loop (-1+ i) (cdr l)))
		   (else
		    #f))))
	  ((or (eq? (car l) '?)
	       (eq? (if (bit-string-ref high i) 1 0)
		    (car l)))
	   (high-loop (-1+ i) (cdr l)))
	  (else #f))))

(define (get-fmove-keyword high low)
  (let loop ((l fmove-patterns))
    (cond ((null? l) '())
	  ((match-bits? high low (caar l))
	   (cdar l))
	  (else
	   (loop (cdr l))))))

(define fmove-patterns
  '(((1 1 1 1 0 0 1 0 0 0 ? ? ? ? ? ?
      0 ? 0 ? ? ? ? ? ? 0 0 0 0 0 0 0) . FMOVE-TO-FP)
    ((1 1 1 1 0 0 1 0 0 0 ? ? ? ? ? ?
      0 1 1 ? ? ? ? ? ? ? ? ? ? ? ? ?) . FMOVE-FROM-FP)
    ((1 1 1 1 0 0 1 0 0 0 ? ? ? ? ? ?
      1 0 ? ? ? ? 0 0 0 0 0 0 0 0 0 0) . FMOVE-FPcr)
    ((1 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0
      0 1 0 1 1 1 ? ? ? ? ? ? ? ? ? ?) . FMOVECR)
    ((1 1 1 1 0 0 1 0 0 0 ? ? ? ? ? ?
      1 1 ? ? ? ? 0 0 0 ? ? ? ? ? ? ?) . FMOVEM-FPn)
    ((1 1 1 1 0 0 1 0 0 0 ? ? ? ? ? ?
      1 0 ? ? ? ? 0 0 0 0 0 0 0 0 0 0) . FMOVEM-FPcr)))

;;;; Bit String Manipulation

(define (fetch-immediate size)
  (cond ((eq? size 'B) (extract+ (get-word) 0 8))
	((eq? size 'W) (bit-string->signed-integer (get-word)))
	((eq? size 'L) (bit-string->signed-integer (get-longword)))
	(else (error "Unknown size" 'FETCH-IMMEDIATE size))))

(define (make-fetcher size-in-bits)
  (let ((size-in-bytes (quotient size-in-bits 8)))
    (lambda ()
      (let ((word (read-bits *current-offset size-in-bits)))
	(set! *current-offset (+ *current-offset size-in-bytes))
	word))))

(define get-word (make-fetcher 16))
(define get-longword (make-fetcher 32))

(define (make-peeker size-in-bits)
  (lambda ()
    (read-bits *current-offset size-in-bits)))

(define peek-word (make-peeker 16))
(define peek-longword (make-peeker 32))

(declare (integrate-operator extract extract+))

(define (extract bit-string start end)
  (declare (integrate bit-string start end))
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

(define (extract+ bit-string start end)
  (declare (integrate bit-string start end))
  (bit-string->signed-integer (bit-substring bit-string start end)))

;;; Symbolic representation of bit strings

(define ((symbol-decoder symbols) index)
  (vector-ref symbols index))

(define decode-bwl (symbol-decoder #(B W L ())))
(define decode-wl  (symbol-decoder #(W L)))
(define decode-rl  (symbol-decoder #(R L)))
(define decode-us  (symbol-decoder #(U S)))
(define decode-da  (symbol-decoder #(D A)))
(define decode-cc
  (symbol-decoder #(T F HI LS CC CS NE EQ VC VS PL MI GE LT GT LE)))
(define decode-bit (symbol-decoder #(BTST BCHG BCLR BSET)))
(define decode-shift-type (symbol-decoder #(AS LS ROX RO)))
(define decode-ze  (symbol-decoder #(E Z)))

(define decode-bf
  (symbol-decoder #(BFTST BFEXTU BFCHG BFEXTS BFCLR BFFFO BFSET BFINS)))

(define (decode-scale scale)
  (vector-ref '#(1 2 4 8) scale))

;;;; Effective Addressing

(define (decode-ea-<D> register size)
  size					; ignored
  (make-data-register 'D register))

(define (decode-ea-<A> register size)
  size					; ignored
  (make-address-register 'A register))

(define (decode-ea-<b=>-A> register size)
  size					; ignored
  (if (memq size '(W L))
      (make-address-register 'A register)
      (undefined-instruction)))

(define (decode-ea-<@A> register size)
  size					; ignored
  (make-address-register '@A register))

(define (decode-ea-<@A+> register size)
  size					; ignored
  (make-address-register '@A+ register))

(define (decode-ea-<@-A> register size)
  size					; ignored
  (make-address-register '@-A register))

(define (decode-ea-<@AO> register size)
  size					; ignored
  (make-address-offset register
		       (bit-string->signed-integer (get-word))))

(define (decode-ea-<W> size)
  size					; ignored
  `(W ,(bit-string->signed-integer (get-word))))

(define (decode-ea-<L> size)
  size					; ignored
  `(L ,(bit-string->signed-integer (get-longword))))

(define (decode-ea-<@PCO> size)
  size					; ignored
  (make-pc-relative (lambda () (bit-string->signed-integer (get-word)))))

(define (decode-ea-<&> size)
  (cond ((eq? size 'B) `(& ,(extract+ (get-word) 0 8)))
	((eq? size 'W) `(& ,(bit-string->signed-integer (get-word))))
	((eq? size 'L) `(& ,(bit-string->signed-integer (get-longword))))
	(else (error "Unknown size" 'DECODE-EA-<&> size))))

;;;; Extended 68020 effective addresses

(define (decode-ea-<@AOX> register size)
  size					; ignored
  (decode-ea-extension
   (lambda (d/a xr w/l scale brs irs bd od operation)
     (cond ((eq? (cadr bd) 'B)
	    (if (= scale 1)
		;; This is the only possibility on a 68000/68010
		`(,@(make-address-register '@AOX register) ,(car bd)
							   ,d/a
							   ,xr
							   ,w/l)
		`(,@(make-address-register '@AOXS register) ,(car bd)
							    (,d/a ,xr)
							    ,w/l
							    ,scale)))
	   ((and (eq? d/a 'D) (eq? w/l 'L) (= scale 1)
		 (eq? brs 'Z) (eq? irs 'E)
		 (eq? (cadr od) 'N) (false? operation)
		 (memq (cadr bd) '(N W)))
	    (if (eq? (cadr bd) 'N)
		(make-data-register '@D xr)
		`(,@(make-data-register '@DO xr) ,(car bd))))
	   (else
	    `(,@(make-address-register '@AOF register) ,brs ,bd ,operation
						       ((,d/a ,xr) ,w/l ,scale)
						       ,irs ,od))))))

(define (decode-ea-<@PCOX> size)
  size					; ignored
  (let ((base-offset *current-offset))
    (decode-ea-extension
     (lambda (d/a xr w/l scale brs irs bd od operation)
       (cond ((eq? (cadr bd) 'B)
	      (if (= scale 1)
		  ;; This is the only possibility on a 68000/68010
		  `(@PCOX ,(car bd) ,d/a ,xr ,w/l)
		  `(@PCOXS ,(car bd) (,d/a ,xr) ,w/l ,scale)))
	     ((and (eq? brs 'E) (eq? irs 'Z) (false? operation)
		   (not (eq? (cadr bd) 'N)) (eq? (cadr od) 'N))
	      (offset->pc-relative (car bd) base-offset))
	     (else
	      `(@PCOF ,brs ,bd ,operation
		      ((,d/a ,xr) ,w/l ,scale)
		      ,irs ,od)))))))

(define (decode-ea-extension receiver)
  (let ((extension (get-word)))
    (let ((d/a (decode-da (extract extension 15 16)))
	  (xr (extract extension 12 15))
	  (w/l (decode-wl (extract extension 11 12)))
	  (scale (decode-scale (extract extension 9 11))))
      (if (not (bit-string-ref extension 8))
	  (receiver d/a xr w/l scale 'E 'E
		    `(,(extract+ extension 0 8) B)
		    '(0 N)
		    #F)
	  (let ((brs (decode-ze (extract extension 7 8)))
		(irs (decode-ze (extract extension 6 7)))
		(i/is (extract extension 0 3))
		(bd (case (extract extension 4 6)
		      ((1) '(0 N))
		      ((2) `(,(fetch-immediate 'W) W))
		      ((3) `(,(fetch-immediate 'L) L))
		      (else
		       #| (error "decode-ea-extension: bad bd-size"
		                 (extract extension 4 6)) |#
		       (undefined-instruction)))))
	    (receiver d/a xr w/l scale brs irs bd
		      (case (if (> i/is 3) (- i/is 4) i/is)
			((0 1) '(0 N))
			((2) `(,(fetch-immediate 'W) W))
			((3) `(,(fetch-immediate 'L) L))
			(else
			 #| (error "decode-ea-extension: bad i/is" i/is) |#
			 (undefined-instruction)))
		      (cond ((zero? i/is) #F)
			    ((> i/is 3) 'POST)
			    (else 'PRE))))))))

(define make-ea-dispatch
  (let ()
    (define (kernel dispatch mode-7)
      (vector-set! dispatch 7
		   (lambda (register size)
		     ((vector-ref mode-7 register) size)))
      (lambda (mode register size)
	((vector-ref dispatch mode) register size)))

    (lambda (d a @a @a+ @-a @ao @aox w l @pco @pcox &)
      (kernel (vector d a @a @a+ @-a @ao @aox '())
	      (vector w l @pco @pcox &
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined)))))

(define (decode-ea-with-size d a @a @a+ @-a @ao @aox w l @pco @pcox &)
  (let ((kernel (make-ea-dispatch d a @a @a+ @-a @ao @aox w l @pco @pcox &)))
    (lambda (size)
      (kernel (extract *ir 3 6)
	      (extract *ir 0 3)
	      size))))

(define (decode-ea-w/o-size d a @a @a+ @-a @ao @aox w l @pco @pcox &)
  (let ((kernel (make-ea-dispatch d a @a @a+ @-a @ao @aox w l @pco @pcox &)))
    (lambda ()
      (kernel (extract *ir 3 6)
	      (extract *ir 0 3)
	      '()))))

(define (decode-ea-undefined register size)
  register size				; ignored
  (undefined-instruction))

(define (decode-ea-mode-7-undefined size)
  size					; ignored
  (undefined-instruction))

(define decode-ea-d
  (decode-ea-with-size decode-ea-<D>
		       decode-ea-undefined
		       decode-ea-<@A>
		       decode-ea-<@A+>
		       decode-ea-<@-A>
		       decode-ea-<@AO>
		       decode-ea-<@AOX>
		       decode-ea-<W>
		       decode-ea-<L>
		       decode-ea-<@PCO>
		       decode-ea-<@PCOX>
		       decode-ea-<&>))

(define decode-ea-m&d
  (decode-ea-w/o-size decode-ea-<D>
		      decode-ea-undefined
		      decode-ea-<@A>
		      decode-ea-undefined
		      decode-ea-undefined
		      decode-ea-<@AO>
		      decode-ea-<@AOX>
		      decode-ea-<W>
		      decode-ea-<L>
		      decode-ea-<@PCO>
		      decode-ea-<@PCOX>
		      decode-ea-undefined))

(define decode-ea-c
  (decode-ea-w/o-size decode-ea-undefined
		      decode-ea-undefined
		      decode-ea-<@A>
		      decode-ea-undefined
		      decode-ea-undefined
		      decode-ea-<@AO>
		      decode-ea-<@AOX>
		      decode-ea-<W>
		      decode-ea-<L>
		      decode-ea-<@PCO>
		      decode-ea-<@PCOX>
		      decode-ea-mode-7-undefined))

(define decode-ea-d&a
  (decode-ea-w/o-size decode-ea-<D>
		      decode-ea-undefined
		      decode-ea-<@A>
		      decode-ea-<@A+>
		      decode-ea-<@-A>
		      decode-ea-<@AO>
		      decode-ea-<@AOX>
		      decode-ea-<W>
		      decode-ea-<L>
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined))

(define decode-ea-c&a
  (decode-ea-with-size decode-ea-undefined
		       decode-ea-undefined
		       decode-ea-<@A>
		       decode-ea-undefined
		       decode-ea-undefined
		       decode-ea-<@AO>
		       decode-ea-<@AOX>
		       decode-ea-<W>
		       decode-ea-<L>
		       decode-ea-mode-7-undefined
		       decode-ea-mode-7-undefined
		       decode-ea-mode-7-undefined))

(define decode-ea-m&a
  (decode-ea-w/o-size decode-ea-undefined
		      decode-ea-undefined
		      decode-ea-<@A>
		      decode-ea-<@A+>
		      decode-ea-<@-A>
		      decode-ea-<@AO>
		      decode-ea-<@AOX>
		      decode-ea-<W>
		      decode-ea-<L>
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined
		      decode-ea-mode-7-undefined))

(define decode-ea-all
  (decode-ea-with-size decode-ea-<D>
		       decode-ea-<A>
		       decode-ea-<@A>
		       decode-ea-<@A+>
		       decode-ea-<@-A>
		       decode-ea-<@AO>
		       decode-ea-<@AOX>
		       decode-ea-<W>
		       decode-ea-<L>
		       decode-ea-<@PCO>
		       decode-ea-<@PCOX>
		       decode-ea-<&>))

(define decode-ea-b=>-A
  (decode-ea-with-size decode-ea-<D>
		       decode-ea-<b=>-A>
		       decode-ea-<@A>
		       decode-ea-<@A+>
		       decode-ea-<@-A>
		       decode-ea-<@AO>
		       decode-ea-<@AOX>
		       decode-ea-<W>
		       decode-ea-<L>
		       decode-ea-<@PCO>
		       decode-ea-<@PCOX>
		       decode-ea-<&>))

(define decode-ea-a&<b=>-A>
  (decode-ea-with-size decode-ea-<D>
		       decode-ea-<b=>-A>
		       decode-ea-<@A>
		       decode-ea-<@A+>
		       decode-ea-<@-A>
		       decode-ea-<@AO>
		       decode-ea-<@AOX>
		       decode-ea-<W>
		       decode-ea-<L>
		       decode-ea-mode-7-undefined
		       decode-ea-mode-7-undefined
		       decode-ea-mode-7-undefined))

(define decode-ea-MOVE-destination
  (let ((kernel (make-ea-dispatch decode-ea-<D>
				  decode-ea-<A>
				  decode-ea-<@A>
				  decode-ea-<@A+>
				  decode-ea-<@-A>
				  decode-ea-<@AO>
				  decode-ea-<@AOX>
				  decode-ea-<W>
				  decode-ea-<L>
				  decode-ea-mode-7-undefined
				  decode-ea-mode-7-undefined
				  decode-ea-mode-7-undefined)))
    (lambda (size)
      (kernel (extract *ir 6 9)
	      (extract *ir 9 12)
	      size))))