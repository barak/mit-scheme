#| -*-Scheme-*-

$Id: instr1.scm,v 1.8 2003/02/14 18:28:02 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Alpha instruction set
;;; Package: (compiler lap-syntaxer)

;; Branch-tensioned instructions are in instr2.scm
;; Floating point instructions are in instr3.scm

(declare (usual-integrations))

(let-syntax
    ((memory-format-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (OFFSET (? offset) (? base)))
	     (VARIABLE-WIDTH (offset offset)
	       ((#x-8000 #x7FFF)
		(LONG (6 ,(caddr form))
		      (5 destination)
		      (5 base)
		      (16 offset SIGNED)))
	       ((#x-80000000 #x7FFFFFFF)
		;; LDAH    temp, left[offset](base)
		;; LDx/STx destination, right[offset](temp)
		(LONG (6 #x09)		; LDAH
		      (5 regnum:volatile-scratch) ; destination = temp
		      (5 base)		;   base
		      (16 (adjusted:high offset) SIGNED)
		      (6 ,(caddr form))	; LDx/STx
		      (5 destination)	;   destination
		      (5 regnum:volatile-scratch) ; base = temp
		      (16 (adjusted:low offset) SIGNED))))))))))
  (memory-format-instruction LDA #x08)	 ; Load Address
  (memory-format-instruction LDAH #x09)	 ; Load Address High
  (memory-format-instruction LDF #x20)	 ; Load F floating from memory
  (memory-format-instruction LDG #x21)	 ; Load G floating from memory
  (memory-format-instruction LDL #x28)	 ; Load sext long
  (memory-format-instruction LDL_L #x2A) ; Load sext long, locked
  (memory-format-instruction LDQ #x29)	 ; Load quadword
  (memory-format-instruction LDQ_L #x2B) ; Load quadword, locked
  (memory-format-instruction LDQ_U #x0B) ; Load quadword unaligned
  (memory-format-instruction LDS #x22)	 ; Load S floating from memory
  (memory-format-instruction LDT #x23)	 ; Load IEEE T floating from memory
  (memory-format-instruction STF #x24)	 ; Store F floating to memory
  (memory-format-instruction STG #x25)	 ; Store G floating to memory
  (memory-format-instruction STL #x2C)	 ; Store long
  (memory-format-instruction STL_C #x2E) ; Store long, conditional
  (memory-format-instruction STQ #x2D)	 ; Store quadword
  (memory-format-instruction STQ_C #x2F) ; Store quadword, conditional
  (memory-format-instruction STQ_U #x0F) ; Store quadword unaligned
  (memory-format-instruction STS #x26)	 ; Store S floating to memory
  (memory-format-instruction STT #x27)	 ; Store IEEE T floating to memory
  )

(define-instruction MOVEI
  (((? destination) (& (? constant)))
   (LONG (6 #x08)			; LDA
	 (5 destination)
	 (5 regnum:zero)
	 (16 constant SIGNED))))

(define-instruction COPY
  (((? source) (? destination))
   (LONG (6 #x11)			; Arithmetic/Logical
	 (5 source)
	 (5 source)
	 (3 0)				; Should be zero
	 (1 0)				; Must be zero
	 (7 #x20)			; BIS
	 (5 destination))))
  
(let-syntax
    ((special-memory-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (()
	     (LONG (6 #x18)
		   (5 #x0)
		   (5 #x0)
		   (16 ,(caddr form))))))))
     (special-memory-instruction-Ra
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? Ra))
	     (LONG (6 #x18)
		   (5 Ra)
		   (5 #x0)
		   (16 ,(caddr form))))))))
     (special-memory-instruction-Rb
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? Rb))
	     (LONG (6 #x18)
		   (5 #x0)
		   (5 Rb)
		   (16 ,(caddr form)))))))))
  (special-memory-instruction DRAINT #x0000)	; Drain instruction pipe
  (special-memory-instruction-Rb FETCH #x8000)	; Prefetch data
  (special-memory-instruction-Rb FETCH_M #xA000); Prefetch data, modify intent
  (special-memory-instruction MB #x4000)	; Memory barrier
  (special-memory-instruction-Ra RC #xE000)	; Read and clear (VAX converter)
  (special-memory-instruction-Ra RPCC #xC000)	; Read process cycle counter
  (special-memory-instruction-Ra RS #xF000)	; Read and set (VAX converter)
  (special-memory-instruction TRAPB #x0000)	; Trap barrier
  )

(let-syntax
    ((operate-format
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? source-1) (& (? constant)) (? destination))
	     (LONG (6 ,(caddr form))
		   (5 source-1)
		   (8 constant UNSIGNED)
		   (1 1)		; Must be one
		   (7 ,(cadddr form))
		   (5 destination)))
	    (((? source-1) (? source-2) (? destination))
	     (LONG (6 ,(caddr form))
		   (5 source-1)
		   (5 source-2)
		   (3 0)		; Should be zero
		   (1 0)		; Must be zero
		   (7 ,(cadddr form))
		   (5 destination))))))))
  (operate-format ADDL #x10 #x00)	 ; Add longword
  (operate-format ADDLV #x10 #x40)	 ; Add longword, enable oflow trap
  (operate-format ADDQ #x10 #x20)	 ; Add quadword
  (operate-format ADDQV #x10 #x60)	 ; Add quadword, enable oflow trap
  (operate-format AND #x11 #x00)	 ; Logical product
  (operate-format BIC #x11 #x08)	 ; Bit clear
  (operate-format BIS #x11 #x20)	 ; Bit set (logical sum, OR)
  (operate-format CMOVEQ #x11 #x24)	 ; Rc <- Rb if Ra = 0
  (operate-format CMOVGE #x11 #x46)	 ; Rc <- Rb if Ra >= 0
  (operate-format CMOVGT #x11 #x66)	 ; Rc <- Rb if Ra > 0
  (operate-format CMOVLBC #x11 #x16)	 ; Rc <- Rb if Ra low bit clear
  (operate-format CMOVLBS #x11 #x14)	 ; Rc <- Rb if Ra low bit set
  (operate-format CMOVLE #x11 #x64)	 ; Rc <- Rb if Ra <= 0
  (operate-format CMOVLT #x11 #x44)	 ; Rc <- Rb if Ra < 0
  (operate-format CMOVNE #x11 #x26)	 ; Rc <- Rb if Ra != 0
  (operate-format CMPBGE #x10 #x0f)	 ; Compare 8 bytes in parallel
  (operate-format CMPEQ #x10 #x2d)	 ; Compare quadwords for equal
  (operate-format CMPLE #x10 #x6d)	 ; Compare quadwords for <=
  (operate-format CMPLT #x10 #x4d)	 ; Compare quadwords for <
  (operate-format CMPULE #x10 #x3d)	 ; Unsigned compare quadwords for <=
  (operate-format CMPULT #x10 #x1d)	 ; Unsigned compare quadwords for <
  (operate-format EQV #x11 #x48)	 ; Bitwise logical equivalence
  (operate-format EXTBL #x12 #x06)	 ; Extract byte low
  (operate-format EXTLH #x12 #x6a)	 ; Extract longword high
  (operate-format EXTLL #x12 #x26)	 ; Extract longword low
  (operate-format EXTQH #x12 #x7a)	 ; Extract quadword high
  (operate-format EXTQL #x12 #x36)	 ; Extract quadword low
  (operate-format EXTWH #x12 #x5a)	 ; Extract word high
  (operate-format EXTWL #x12 #x16)	 ; Extract word low
  (operate-format INSBL #x12 #x0b)	 ; Insert byte low
  (operate-format INSLH #x12 #x67)	 ; Insert longword high
  (operate-format INSLL #x12 #x2b)	 ; Insert longword low
  (operate-format INSQH #x12 #x77)	 ; Insert quadword high
  (operate-format INSQL #x12 #x3b)	 ; Insert quadword low
  (operate-format INSWH #x12 #x57)	 ; Insert word high
  (operate-format INSWL #x12 #x1b)	 ; Insert word low
  (operate-format MSKBL #x12 #x02)	 ; Mask byte low
  (operate-format MSKLH #x12 #x62)	 ; Mask longword high
  (operate-format MSKLL #x12 #x22)	 ; Mask longword low
  (operate-format MSKQH #x12 #x72)	 ; Mask quadword high
  (operate-format MSKQL #x12 #x32)	 ; Mask quadword low
  (operate-format MSKWH #x12 #x52)	 ; Mask word high
  (operate-format MSKWL #x12 #x12)	 ; Mask word low
  (operate-format MULL #x13 #x00)	 ; Multiply longword
  (operate-format MULLV #x13 #x40)	 ; Multiply longword, enable oflow trap
  (operate-format MULQ #x13 #x20)	 ; Multiply quadword
  (operate-format MULQV #x13 #x60)	 ; Multiply quadword, enable oflow trap
  (operate-format ORNOT #x11 #x28)	 ; Ra v ~Rb
  (operate-format S4ADDL #x10 #x02)	 ; Shift Ra by 4 and longword add to Rb
  (operate-format S4ADDQ #x10 #x22)	 ; Shift Ra by 4 and quadword add to Rb
  (operate-format S4SUBL #x10 #x0b)	 ; Shift Ra and longword subtract Rb
  (operate-format S4SUBQ #x10 #x2b)	 ; Shift Ra and quadword subtract Rb
  (operate-format S8ADDL #x10 #x12)	 ; Shift Ra by 8 and longword add to Rb
  (operate-format S8ADDQ #x10 #x32)	 ; Shift Ra by 8 and quadword add to Rb
  (operate-format S8SUBL #x10 #x1b)	 ; Shift Ra and longword subtract Rb
  (operate-format S8SUBQ #x10 #x3b)	 ; Shift Ra and quadword subtract Rb
  (operate-format SLL #x12 #x39)	 ; Shift left logical
  (operate-format SRA #x12 #x3c)	 ; Shift right arithmetic
  (operate-format SRL #x12 #x34)	 ; Shift right logical
  (operate-format SUBL #x10 #x09)	 ; Subtract longword
  (operate-format SUBLV #x10 #x49)	 ; Subtract longword, enable oflow trap
  (operate-format SUBQ #x10 #x29)	 ; Subtract quadword
  (operate-format SUBQV #x10 #x69)	 ; Subtract quadword, enable oflow trap
  (operate-format UMULH #x13 #x30)	 ; Unsigned multiply quadword high
  (operate-format XOR #x11 #x40)	 ; Logical difference (xor)
  (operate-format ZAP #x12 #x30)	 ; Zero bytes
  (operate-format ZAPNOT #x12 #x31)	 ; Zero bytes not
  )

(let-syntax
    ((pal-format
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (()
	     (LONG (6 0)
		   (26 ,(caddr form)))))))))

  (pal-format BPT #x0080)		 ; Initiate program debugging
  (pal-format BUGCHK #x0081)		 ; Initiate program exception
  (pal-format CHME #x0082)		 ; Change mode to emulator
  (pal-format CHMK #x0083)		 ; Change mode to kernel
  (pal-format CHMS #x0084)		 ; Change mode to supervisor
  (pal-format CHMU #x0085)		 ; Change mode to user
  (pal-format IMB #x0086)		 ; Instruction memory barrier
  (pal-format INSQHIL #x0087)		 ; Insert into longword queue at head, interlocked
  (pal-format INSQHIQ #x0089)		 ; ... quadword ... head
  (pal-format INSQTIL #x0088)		 ; ... longword ... tail
  (pal-format INSQTIQ #x008a)		 ; ... quadword ... tail
  (pal-format INSQUEL #x008b)		 ; Insert into longword queue
  (pal-format INSQUELD #x008d)		 ; 
  (pal-format INSQUEQ #x008c)		 ; Insert into quadword queue
  (pal-format INSQUEQD #x008e)		 ;
  (pal-format PROBER #x008f)		 ; Probe for read access
  (pal-format PROBEW #x0090)		 ; Probe for write access
  (pal-format RD_PS #x0091)		 ; Move processor status
  (pal-format REI #x0092)		 ; Return from exception or interrupt
  (pal-format REMQHIL #x0093)		 ; Remove from longword queue at head, interlocked
  (pal-format REMQHIQ #x0095)		 ; ... quadword ... head
  (pal-format REMQTIL #x0094)		 ; ... longword ... tail
  (pal-format REMQTIQ #x0096)		 ; ... quadword ... tail
  (pal-format REMQUEL #x0097)		 ; Remove from longword queue
  (pal-format REMQUELD #x0099)		 ;
  (pal-format REMQUEQ #x0098)		 ; Remove from quadword queue
  (pal-format REMQUEQD #x009a)		 ;
  (pal-format RSCC #x009d)		 ;
  (pal-format SWASTEN #x009b)		 ; Swap AST enable
  (pal-format WR_PS_SW #x009c)		 ; Write processor status s'ware field

  ;; Privileged PALcode instructions.
  (pal-format HALT #x0000)
  )

;;;; Assembler pseudo-ops

(define-instruction EXTERNAL-LABEL
  ;; External labels provide the garbage collector with header
  ;; information and the runtime system with type, arity, and
  ;; debugging information.
  (((? format-word) (@PCR (? label)))
   (LONG (16 label BLOCK-OFFSET)
	 (16 format-word UNSIGNED))))

(define-instruction NOP
  ;; BIS R31 R31 R31
  (()
   (LONG (6 #x11) (5 31) (5 31) (3 0) (1 0) (7 #x20) (5 31))))

(define-instruction UWORD
  ;; Directly insert 32 bit word into output stream
  (((? expression))
   (LONG (32 expression UNSIGNED))))