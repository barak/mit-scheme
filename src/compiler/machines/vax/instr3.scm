#| -*-Scheme-*-

$Id: instr3.scm,v 1.16 2003/02/14 18:28:07 cph Exp $

Copyright (c) 1987, 1989, 1991, 1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; VAX Instruction Set Description, Part 3

;;; The ordering is essentially that in "Vax Architecture Handbook" 1981.

(declare (usual-integrations))

(define-instruction ASH
  ((L (? cnt ea-r-b) (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #x78))
   (OPERAND B  cnt)
   (OPERAND L src)
   (OPERAND L dst))

  ((Q (? cnt ea-r-b) (? src ea-r-q) (? dst ea-w-q))
   (BYTE (8 #x79))
   (OPERAND B cnt)
   (OPERAND Q src)
   (OPERAND Q dst)))

(define-instruction ROTL
  (((? cnt ea-r-b) (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #x9C))
   (OPERAND B cnt)
   (OPERAND L src)
   (OPERAND L dst)))

(define-instruction POLY
  ((F (? arg ea-r-f) (? degree ea-r-w) (? tbladdr ea-a-b))
   (BYTE (8 #x55))
   (OPERAND F arg)
   (OPERAND W degree)
   (OPERAND B tbladdr))

  ((D (? arg ea-r-d) (? degree ea-r-w) (? tbladdr ea-a-b))
   (BYTE (8 #x75))
   (OPERAND D arg)
   (OPERAND W degree)
   (OPERAND B tbladdr))

  ((G (? arg ea-r-g) (? degree ea-r-w) (? tbladdr ea-a-b))
   (BYTE (16 #x55FD))
   (OPERAND G arg)
   (OPERAND W degree)
   (OPERAND B tbladdr))

  ((H (? arg ea-r-h) (? degree ea-r-w) (? tbladdr ea-a-b))
   (BYTE (16 #x75FD))
   (OPERAND H arg)
   (OPERAND W degree)
   (OPERAND B tbladdr)))

;;;; Special instructions (Chap. 12)

(define-instruction PUSHR
  (((? mask ea-r-w))
   (BYTE (8 #xBB))
   (OPERAND W mask)))

(define-instruction POPR
  (((? mask ea-r-w))
   (BYTE (8 #xBA))
   (OPERAND W mask)))

(define-instruction MOVPSL
  (((? dst ea-w-l))
   (BYTE (8 #xDC))
   (OPERAND L dst)))

(define-instruction BISPSW
  (((? mask ea-r-w))
   (BYTE (8 #xB8))
   (OPERAND W mask)))

(define-instruction BICPSW
  (((? mask ea-r-w))
   (BYTE (8 #xB9))
   (OPERAND W mask)))

(define-instruction MOVA
  ((B (? src ea-a-b) (? dst ea-w-l))
   (BYTE (8 #x9E))
   (OPERAND B src)
   (OPERAND L dst))

  ((W (? src ea-a-w) (? dst ea-w-l))
   (BYTE (8 #x3E))
   (OPERAND W src)
   (OPERAND L dst))

  ((L (? src ea-a-l) (? dst ea-w-l))
   (BYTE (8 #xDE))
   (OPERAND L src)
   (OPERAND L dst))

  ((F (? src ea-a-f) (? dst ea-w-l))
   (BYTE (8 #xDE))
   (OPERAND F src)
   (OPERAND L dst))

  ((Q (? src ea-a-q) (? dst ea-w-l))
   (BYTE (8 #x7E))
   (OPERAND Q src)
   (OPERAND L dst))

  ((D (? src ea-a-d) (? dst ea-w-l))
   (BYTE (8 #x7E))
   (OPERAND D src)
   (OPERAND L dst))

  ((G (? src ea-a-g) (? dst ea-w-l))
   (BYTE (8 #x7E))
   (OPERAND G src)
   (OPERAND L dst))

  ((H (? src ea-a-h) (? dst ea-w-l))
   (BYTE (16 #x7EFD))
   (OPERAND H src)
   (OPERAND L dst))

  ((O (? src ea-a-o) (? dst ea-w-l))
   (BYTE (16 #x7EFD))
   (OPERAND O src)
   (OPERAND L dst)))

(define-instruction PUSHA
  ((B (? src ea-a-b))
   (BYTE (8 #x9F))
   (OPERAND B src))

  ((W (? src ea-a-w))
   (BYTE (8 #x3F))
   (OPERAND W src))

  ((L (? src ea-a-l))
   (BYTE (8 #xDF))
   (OPERAND L src))

  ((F (? src ea-a-f))
   (BYTE (8 #xDF))
   (OPERAND F src))

  ((Q (? src ea-a-q))
   (BYTE (8 #x7F))
   (OPERAND Q src))

  ((D (? src ea-a-d))
   (BYTE (8 #x7F))
   (OPERAND D src))

  ((G (? src ea-a-g))
   (BYTE (8 #x7F))
   (OPERAND G src))

  ((H (? src ea-a-h))
   (BYTE (16 #x7FFD))
   (OPERAND H src))

  ((O (? src ea-a-o))
   (BYTE (16 #x7FFD))
   (OPERAND O src)))

;;; Array indeces and queues

(define-instruction INDEX
  (((? subscript ea-r-l) (? low ea-r-l) (? high ea-r-l)
    (? size ea-r-l) (? indexin ea-r-l) (? indexout ea-w-l))
   (BYTE (8 #x0A))
   (OPERAND L subscript)
   (OPERAND L low)
   (OPERAND L high)
   (OPERAND L size)
   (OPERAND L indexin)
   (OPERAND L indexout)))

(define-instruction INSQUE
  (((? entry ea-a-b) (? pred ea-a-b))
   (BYTE (8 #x0E))
   (OPERAND B entry)
   (OPERAND B pred)))

(define-instruction REMQUE
  (((? entry ea-a-b) (? addr ea-w-l))
   (BYTE (8 #x0F))
   (OPERAND B entry)
   (OPERAND L addr)))

(define-instruction INSQHI
  (((? entry ea-a-b) (? header ea-a-q))
   (BYTE (8 #x5C))
   (OPERAND B entry)
   (OPERAND Q header)))

(define-instruction INSQTI
  (((? entry ea-a-b) (? header ea-a-q))
   (BYTE (8 #x5D))
   (OPERAND B entry)
   (OPERAND Q header)))

(define-instruction REMQHI
  (((? header ea-a-q) (? addr ea-w-l))
   (BYTE (8 #x5E))
   (OPERAND Q header)
   (OPERAND L addr)))

(define-instruction REMQTI
  (((? header ea-a-q) (? addr ea-w-l))
   (BYTE (8 #x5F))
   (OPERAND Q header)
   (OPERAND L addr)))

;;; Bit field instructions

(let-syntax
    ((define-field-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((name (list-ref form 1))
		(suffix1 (list-ref form 2))
		(suffix2 (list-ref form 3))
		(opcode (list-ref form 4))
		(mode (list-ref form 5)))
	    `(DEFINE-INSTRUCTION ,name
	       ((,suffix1 (? pos ea-r-l) (? size ea-r-b) (? base ea-v-b)
			  (? dst ,mode))
		(BYTE (8 ,opcode))
		(OPERAND L pos)
		(OPERAND B size)
		(OPERAND B base)
		(OPERAND L dst))

	       ((,suffix2 (? pos ea-r-l) (? size ea-r-b) (? base ea-v-b)
			  (? dst ,mode))
		(BYTE (8 ,(1+ opcode)))
		(OPERAND L pos)
		(OPERAND B size)
		(OPERAND B base)
		(OPERAND L dst))))))))

  (define-field-instruction FF S C #xEA ea-w-l)
  (define-field-instruction EXTV S Z #xEE ea-w-l)
  (define-field-instruction CMPV S Z #xEC ea-r-l))

(define-instruction INSV
  (((? src ea-r-l) (? pos ea-r-l) (? size ea-r-b) (? base ea-v-b))
   (BYTE (8 #xF0))
   (OPERAND L src)
   (OPERAND L pos)
   (OPERAND B size)
   (OPERAND B base)))

;;;; Control instructions (Chap. 13)

;; The VAX only has byte offset conditional branch instructions.
;; Longer displacements are obtained by negating the condition and
;; branching over an unconditional instruction.

(define-instruction B
  ((B (? c cc) (@PCO (? dest)))
   (BYTE (4 c) (4 #x1))
   (DISPLACEMENT (8 dest)))

  ((B (? c cc) (@PCR (? dest)))
   (BYTE (4 c) (4 #x1))
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((W (? c inverse-cc) (@PCO (? dest)))
   (BYTE (4 c) (4 #x1))			; (B B (~ cc) (+ *PC* 3))
   (BYTE (8 #x03 SIGNED))
   (BYTE (8 #x31))			; (BR W dest)
   (DISPLACEMENT (16 dest)))

  ((W (? c inverse-cc) (@PCR (? dest)))
   (BYTE (4 c) (4 #x1))			; (B B (~ cc) (+ *PC* 3))
   (BYTE (8 #x03 SIGNED))
   (BYTE (8 #x31))			; (BR W dest)
   (DISPLACEMENT (16 `(- ,dest (+ *PC* 2)))))

  ;; Self adjusting version. It does not handle @PCO
  (((? c cc cs) (@PCR (? label)))
   (VARIABLE-WIDTH
    (disp `(- ,label (+ *PC* 2)))
    ((-128 127)
     (BYTE (4 c) (4 #x1))
     (BYTE (8 disp SIGNED)))
    ((-32765 32770)
     (BYTE (4 (inverse-cc cs)) (4 #x1))	; (B B (~ cc) (+ *PC* 3))
     (BYTE (8 #x03))
     (BYTE (8 #x31))			; (BR W label)
     (BYTE (16 (- disp 3) SIGNED)))
    ((() ())
     (BYTE (4 (inverse-cc cs)) (4 #x1))	; (B B (~ cc) (+ *PC* 6))
     (BYTE (8 #x06))
     (BYTE (8 #x17))			; (JMP (@PCO L label))
     (BYTE (4 15) (4 14))
     (BYTE (32 (- disp 6) SIGNED)))))

  (((? c cc cs) (@PCRO (? label) (? offset))) ; Kludge!
   (VARIABLE-WIDTH
    (disp `(+ ,offset (- ,label (+ *PC* 2))))
    ((-128 127)
     (BYTE (4 c) (4 #x1))
     (BYTE (8 disp SIGNED)))
    ((-32765 32770)
     (BYTE (4 (inverse-cc cs)) (4 #x1))	; (B B (~ cc) (+ *PC* 3))
     (BYTE (8 #x03))
     (BYTE (8 #x31))			; (BR W label)
     (BYTE (16 (- disp 3) SIGNED)))
    ((() ())
     (BYTE (4 (inverse-cc cs)) (4 #x1))	; (B B (~ cc) (+ *PC* 6))
     (BYTE (8 #x06))
     (BYTE (8 #x17))			; (JMP (@PCO L label))
     (BYTE (4 15) (4 14))
     (BYTE (32 (- disp 6) SIGNED))))))

(let-syntax
    ((define-unconditional-transfer
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((nameb (cadr form))
		(namej (caddr form))
		(bit (cadddr form)))
	    `(BEGIN
	       (DEFINE-INSTRUCTION ,nameb
		 ((B (@PCO (? dest)))
		  (BYTE (8 ,(+ #x10 bit)))
		  (DISPLACEMENT (8 dest)))

		 ((B (@PCR (? dest)))
		  (BYTE (8 ,(+ #x10 bit)))
		  (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

		 ((W (@PCO (? dest)))
		  (BYTE (8 ,(+ #x30 bit)))
		  (DISPLACEMENT (16 dest)))

		 ((W (@PCR (? dest)))
		  (BYTE (8 ,(+ #x30 bit)))
		  (DISPLACEMENT (16  `(- ,dest (+ *PC* 2)))))

		 ;; Self tensioned version. @PCO not handled.
		 (((@PCR (? label)))
		  (VARIABLE-WIDTH
		   (disp `(- ,label (+ *PC* 2)))
		   ((-128 127)		; (BR/BSB B label)
		    (BYTE (8 ,(+ #x10 bit)))
		    (BYTE (8 disp SIGNED)))
		   ((-32767 32768)	; (BR/BSB W label)
		    (BYTE (8 ,(+ #x30 bit)))
		    (BYTE (16 (- disp 1) SIGNED)))
		   ((() ())		; (JMP/JSB (@PCO L label))
		    (BYTE (8 ,(+ #x16 bit)))
		    (BYTE (4 15)
			  (4 14))
		    (BYTE (32 (- disp 4) SIGNED)))))

		 (((@PCRO (? label) (? offset))) ; Kludge!
		  (VARIABLE-WIDTH
		   (disp `(+ ,offset (- ,label (+ *PC* 2))))
		   ((-128 127)		; (BR/BSB B label)
		    (BYTE (8 ,(+ #x10 bit)))
		    (BYTE (8 disp SIGNED)))
		   ((-32767 32768)	; (BR/BSB W label)
		    (BYTE (8 ,(+ #x30 bit)))
		    (BYTE (16 (- disp 1) SIGNED)))
		   ((() ())		; (JMP/JSB (@PCO L label))
		    (BYTE (8 ,(+ #x16 bit)))
		    (BYTE (4 15)
			  (4 14))
		    (BYTE (32 (- disp 4) SIGNED))))))

	       (DEFINE-INSTRUCTION ,namej
		 (((? dst ea-a-b))
		  (BYTE (8 ,(+ #x16 bit)))
		  (OPERAND B dst)))))))))

  (define-unconditional-transfer BR JMP #x1)
  (define-unconditional-transfer BSB JSB #x0))

(define-trivial-instruction RSB #x05)

(define-instruction CALLG
  (((? arglist ea-a-b) (? dst ea-a-b))
   (BYTE (8 #xFA))
   (OPERAND B arglist)
   (OPERAND B dst)))

(define-instruction CALLS
  (((? narg ea-r-l) (? dst ea-a-b))
   (BYTE (8 #xFB))
   (OPERAND L narg)
   (OPERAND B dst)))

(define-trivial-instruction RET #x04)

(define-instruction BLB
  ((S (? src ea-r-l) (@PCO (? dest)))
   (BYTE (8 #xE8))
   (OPERAND L src)
   (DISPLACEMENT (8 dest)))

  ((S (? src ea-r-l) (@PCR (? dest)))
   (BYTE (8 #xE8))
   (OPERAND L src)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((C (? src ea-r-l) (@PCO (? dest)))
   (BYTE (8 #xE9))
   (OPERAND L src)
   (DISPLACEMENT (8 dest)))

  ((C (? src ea-r-l) (@PCR (? dest)))
   (BYTE (8 #xE9))
   (OPERAND L src)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1))))))

(define-instruction BB
  ((S (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE0))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((S (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE0))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((C (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE1))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((C (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE1))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((S S (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE2))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((S S (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE2))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((C S (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE3))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((C S (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE3))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((S C (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE4))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((S C (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE4))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((C C (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE5))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((C C (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE5))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((S S I (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE6))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((S S I (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE6))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((C C I (? pos ea-r-l) (? base ea-v-b) (@PCO (? dest)))
   (BYTE (8 #xE7))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 dest)))

  ((C C I (? pos ea-r-l) (? base ea-v-b) (@PCR (? dest)))
   (BYTE (8 #xE7))
   (OPERAND L pos)
   (OPERAND B base)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1))))))

(define-instruction ACB
  ((B (? limit ea-r-b) (? add ea-r-b) (? index ea-m-b) (@PCO (? dest)))
   (BYTE (8 #x9D))
   (OPERAND B limit)
   (OPERAND B add)
   (OPERAND B index)
   (DISPLACEMENT (8 dest)))

  ((B (? limit ea-r-b) (? add ea-r-b) (? index ea-m-b) (@PCR (? dest)))
   (BYTE (8 #x9D))
   (OPERAND B limit)
   (OPERAND B add)
   (OPERAND B index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((W (? limit ea-r-w) (? add ea-r-w) (? index ea-m-w) (@PCO (? dest)))
   (BYTE (8 #x3D))
   (OPERAND W limit)
   (OPERAND W add)
   (OPERAND W index)
   (DISPLACEMENT (8 dest)))

  ((W (? limit ea-r-w) (? add ea-r-w) (? index ea-m-w) (@PCR (? dest)))
   (BYTE (8 #x3D))
   (OPERAND W limit)
   (OPERAND W add)
   (OPERAND W index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((L (? limit ea-r-l) (? add ea-r-l) (? index ea-m-l) (@PCO (? dest)))
   (BYTE (8 #xF1))
   (OPERAND L limit)
   (OPERAND L add)
   (OPERAND L index)
   (DISPLACEMENT (8 dest)))

  ((L (? limit ea-r-l) (? add ea-r-l) (? index ea-m-l) (@PCR (? dest)))
   (BYTE (8 #xF1))
   (OPERAND L limit)
   (OPERAND L add)
   (OPERAND L index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((F (? limit ea-r-f) (? add ea-r-f) (? index ea-m-f) (@PCO (? dest)))
   (BYTE (8 #x4F))
   (OPERAND F limit)
   (OPERAND F add)
   (OPERAND F index)
   (DISPLACEMENT (8 dest)))

  ((F (? limit ea-r-f) (? add ea-r-f) (? index ea-m-f) (@PCR (? dest)))
   (BYTE (8 #x4F))
   (OPERAND F limit)
   (OPERAND F add)
   (OPERAND F index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((D (? limit ea-r-d) (? add ea-r-d) (? index ea-m-d) (@PCO (? dest)))
   (BYTE (8 #x6F))
   (OPERAND D limit)
   (OPERAND D add)
   (OPERAND D index)
   (DISPLACEMENT (8 dest)))

  ((D (? limit ea-r-d) (? add ea-r-d) (? index ea-m-d) (@PCR (? dest)))
   (BYTE (8 #x6F))
   (OPERAND D limit)
   (OPERAND D add)
   (OPERAND D index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((G (? limit ea-r-g) (? add ea-r-g) (? index ea-m-g) (@PCO (? dest)))
   (BYTE (16 #x4FFD))
   (OPERAND G limit)
   (OPERAND G add)
   (OPERAND G index)
   (DISPLACEMENT (8 dest)))

  ((G (? limit ea-r-g) (? add ea-r-g) (? index ea-m-g) (@PCR (? dest)))
   (BYTE (16 #x4FFD))
   (OPERAND G limit)
   (OPERAND G add)
   (OPERAND G index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))
  
  ((H (? limit ea-r-h) (? add ea-r-h) (? index ea-m-h) (@PCO (? dest)))
   (BYTE (16 #x6FFD))
   (OPERAND H limit)
   (OPERAND H add)
   (OPERAND H index)
   (DISPLACEMENT (8 dest)))

  ((H (? limit ea-r-h) (? add ea-r-h) (? index ea-m-h) (@PCR (? dest)))
   (BYTE (16 #x6FFD))
   (OPERAND H limit)
   (OPERAND H add)
   (OPERAND H index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1))))))

(define-instruction AOB
  ((LSS (? limit ea-r-l) (? index ea-m-l) (@PCO (? dest)))
   (BYTE (8 #xF2))
   (OPERAND L limit)
   (OPERAND L index)
   (DISPLACEMENT (8 dest)))

  ((LSS (? limit ea-r-l) (? index ea-m-l) (@PCR (? dest)))
   (BYTE (8 #xF2))
   (OPERAND L limit)
   (OPERAND L index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((LEQ (? limit ea-r-l) (? index ea-m-l) (@PCO (? dest)))
   (BYTE (8 #xF3))
   (OPERAND L limit)
   (OPERAND L index)
   (DISPLACEMENT (8 dest)))

  ((LEQ (? limit ea-r-l) (? index ea-m-l) (@PCR (? dest)))
   (BYTE (8 #xF3))
   (OPERAND L limit)
   (OPERAND L index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1))))))

(define-instruction SOB
  ((GEQ (? index ea-m-l) (@PCO (? dest)))
   (BYTE (8 #xF4))
   (OPERAND L index)
   (DISPLACEMENT (8 dest)))

  ((GEQ (? index ea-m-l) (@PCR (? dest)))
   (BYTE (8 #xF4))
   (OPERAND L index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1)))))

  ((GTR (? index ea-m-l) (@PCO (? dest)))
   (BYTE (8 #xF5))
   (OPERAND L index)
   (DISPLACEMENT (8 dest)))

  ((GTR (? index ea-m-l) (@PCR (? dest)))
   (BYTE (8 #xF5))
   (OPERAND L index)
   (DISPLACEMENT (8 `(- ,dest (+ *PC* 1))))))

;; NOTE: The displacements must be placed separately on the
;; instruction stream after the instruction.
;;
;; For example:
;;
;; (CASE B (R 0) (& 5) (& 2))
;; (LABEL case-begin)
;; (WORD `(- case-5 case-begin))
;; (WORD `(- case-6 case-begin))
;; (WORD `(- case-7 case-begin))
;; <fall through if out of range>

(define-instruction CASE
  ((B (? selector ea-r-b) (? base ea-r-b) (? limit ea-r-b))
   (BYTE (8 #x8F))
   (OPERAND B selector)
   (OPERAND B base)
   (OPERAND B limit))

  ((W (? selector ea-r-w) (? base ea-r-w) (? limit ea-r-w))
   (BYTE (8 #xAF))
   (OPERAND W selector)
   (OPERAND W base)
   (OPERAND W limit))

  ((L (? selector ea-r-l) (? base ea-r-l) (? limit ea-r-l))
   (BYTE (8 #xCF))
   (OPERAND L selector)
   (OPERAND L base)
   (OPERAND L limit)))

;;;; BCD instructions (Chap 15.)

(let-syntax
    ((define-add/sub-bcd-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  (let ((opcode4 (caddr form)))
	    `(DEFINE-INSTRUCTION ,(cadr form)
	       (((? oplen ea-r-w) (? op ea-a-b)
				  (? reslen ea-r-w) (? res ea-a-b))
		(BYTE (8 ,opcode4))
		(OPERAND W oplen)
		(OPERAND B op)
		(OPERAND W reslen)
		(OPERAND B res))

	       (((? op1len ea-r-w) (? op1 ea-a-b)
				   (? op2len ea-r-w) (? op2 ea-a-b)
				   (? reslen ea-r-w) (? res ea-a-b))
		(BYTE (8 ,(1+ opcode4)))
		(OPERAND W op1len)
		(OPERAND B op1)
		(OPERAND W op2len)
		(OPERAND B op2)
		(OPERAND W reslen)
		(OPERAND B res))))))))

  (define-add/sub-bcd-instruction ADDP #x20)
  (define-add/sub-bcd-instruction SUBP #x22))

(let-syntax
    ((define-add/sub-bcd-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     (((? op1len ea-r-w) (? op1 ea-a-b)
	       (? op2len ea-r-w) (? op2 ea-a-b)
	       (? reslen ea-r-w) (? res ea-a-b))
	      (BYTE (8 ,(caddr form)))
	      (OPERAND W op1len)
	      (OPERAND B op1)
	      (OPERAND W op2len)
	      (OPERAND B op2)
	      (OPERAND W reslen)
	      (OPERAND B res)))))))

  (define-add/sub-bcd-instruction MULP #x25)
  (define-add/sub-bcd-instruction DIVP #x27))

(define-instruction CMPP
  (((? len ea-r-w) (? src1 ea-a-b) (? src2 ea-a-b))
   (BYTE (8 #x35))
   (OPERAND W len)
   (OPERAND B src1)
   (OPERAND B src2))

  (((? len1 ea-r-w) (? src1 ea-a-b) (? len2 ea-r-w) (? src2 ea-a-b))
   (BYTE (8 #x37))
   (OPERAND W len1)
   (OPERAND B src1)
   (OPERAND W len2)
   (OPERAND B src2)))

(define-instruction ASHP
  (((? srclen ea-r-w) (? src ea-a-b)
    (? round ea-r-b)
    (? dstlen ea-r-w) (? dst ea-a-b))
   (BYTE (8 #xF8))
   (OPERAND W srclen)
   (OPERAND B src)
   (OPERAND B round)
   (OPERAND W dstlen)
   (OPERAND B dst)))

(define-instruction MOVP
  (((? len ea-r-w) (? src ea-a-b) (? dst ea-a-b))
   (BYTE (8 #x34))
   (OPERAND W len)
   (OPERAND B src)
   (OPERAND B dst)))   

(define-instruction CVTLP
  (((? src ea-r-l) (? len ea-r-w) (? dst ea-a-b))
   (BYTE (8 #xF9))
   (OPERAND L src)
   (OPERAND W len)
   (OPERAND B dst)))

(define-instruction CVTPL
  (((? len ea-r-w) (? src ea-a-b) (? dst ea-w-l))
   (BYTE (8 #x36))
   (OPERAND W len)
   (OPERAND B src)
   (OPERAND L dst)))

(let-syntax
    ((define-cvt-trailing-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     (((? srclen ea-r-w) (? src ea-a-b) 
	       (? tbl ea-a-b)
	       (? dstlen ea-r-w) (? dst ea-a-b))
	      (BYTE (8 ,(caddr form)))
	      (OPERAND W srclen)
	      (OPERAND B src)
	      (OPERAND B tbl)
	      (OPERAND W dstlen)
	      (OPERAND B dst)))))))

  (define-cvt-trailing-instruction CVTPT #x24)
  (define-cvt-trailing-instruction CVTTT #x26))

(let-syntax
    ((define-cvt-separate-instruction
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-INSTRUCTION ,(cadr form)
	     (((? srclen ea-r-w) (? src ea-a-b)
	       (? dstlen ea-r-w) (? dst ea-a-b))
	      (BYTE (8 ,(caddr form)))
	      (OPERAND W srclen)
	      (OPERAND B src)
	      (OPERAND W dstlen)
	      (OPERAND B dst)))))))

  (define-cvt-separate-instruction CVTPS #x08)
  (define-cvt-separate-instruction CVTSP #x09))