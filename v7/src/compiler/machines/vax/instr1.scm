#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/instr1.scm,v 1.1 1987/08/14 05:04:50 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; VAX Instruction Set Description, Part 1

;;; The ordering is essentially that in "Vax Architecture Handbook" 1981.

(declare (usual-integrations))

;;;; REMARKS

#|

A) There are three types of operand specifiers:

    - General addressing mode operand specifier, with matching pattern syntax

      (? foo ea-<access-type>-<operand-type>)

      Access types and operand types are described on the "Vax Architecture
      Handbook", on Appendix E. 
      They are implemented in insutl.scm

    - Displacement for branch instructions.  The matching pattern syntax is

      (? value displacement)

      This matches either (@PCO offset) or (@PCR label).

    - Immediate operand.  Only the BUG instruction uses this.  The
      matching syntax is (? value).      

B) The instruction set is currently incomplete.  In particular, none
of the instructions in chapters 14 or 16 are below.  The missing
opcodes are

- Chap. 14: MOVC, MOVTC, MOVTUC, CMPC, SCANC, SPANC, LOCC, SKPC,
            MATCHC, CRC.

- Chap. 16: EDITPC.

|#

;; Utility

(define-macro (define-trivial-instruction mnemonic opcode)
  `(define-instruction ,mnemonic
     (()
      (BYTE (8 ,opcode)))))

;; Pseudo-op

(define-instruction DC
  ((B (? value))
   (BYTE (8 value SIGNED)))

  ((W (? value))
   (BYTE (16 value SIGNED)))

  ((L (? value))
   (BYTE (32 value SIGNED))))

;;; Privilleged and miscellaneous (Chap. 10)

(define-instruction CHM
  ((K (? code ea-r-w))		; kernel
   (BYTE (8 #xBC))
   (OPERAND code))

  ((E (? code ea-r-w))		; executive
   (BYTE (8 #xBD))
   (OPERAND code))

  ((S (? code ea-r-w))		; supervisor
   (BYTE (8 #xBE))
   (OPERAND code))

  ((U (? code ea-r-w))		; user
   (BYTE (8 #xBF))
   (OPERAND code)))

(define-instruction PROBE
  ((R (? mode ea-r-b) (? len ea-r-w) (? base ea-a-b))
   (BYTE (8 #xOC))
   (OPERAND mode)
   (OPERAND len)
   (OPERAND base))

  ((W (? mode ea-r-b) (? len ea-r-w) (? base ea-a-b))
   (BYTE (8 #xOD))
   (OPERAND mode)
   (OPERAND len)
   (OPERAND base)))

(define-trivial-instruction REI #x02)
(define-trivial-instruction LDPCTX #x06)
(define-trivial-instruction SVPCTX #x07)

(define-instruction MTPR
  (((? src ea-r-l) (? procreg ea-r-l))
   (BYTE (8 #xDA))
   (OPERAND src)
   (OPERAND procreg)))

(define-instruction MFPR
  (((? procreg ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xDB))
   (OPERAND procreg)
   (OPERAND dst)))

(define-trivial-instruction XFC #xFC)

(define-trivial-instruction BPT #x03)

(define-instruction BUG
  ((W (? message))
   (BYTE (16 #xFEFF)
	 (16 message)))

  ((L (? message))
   (BYTE (16 #xFDFF)
	 (32 message))))

(define-trivial-instruction HALT #x00)

;;;; Integer and floating point instructions (Chap. 11)

(define-instruction MOV
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x90))
   (OPERAND src)
   (OPERAND dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xB0))
   (OPERAND src)
   (OPERAND dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xD0))
   (OPERAND src)
   (OPERAND dst))

  ((Q (? src ea-r-q) (? dst ea-w-q))
   (BYTE (8 #x7D))
   (OPERAND src)
   (OPERAND dst))

  ((O (? src ea-r-o) (? dst ea-w-o))
   (BYTE (16 #x7DFD))
   (OPERAND src)
   (OPERAND dst))

  ((F (? src ea-r-f) (? dst ea-w-f))
   (BYTE (8 #x50))
   (OPERAND src)
   (OPERAND dst))

  ((D (? src ea-r-d) (? dst ea-w-d))
   (BYTE (8 #x70))
   (OPERAND src)
   (OPERAND dst))

  ((G (? src ea-r-g) (? dst ea-w-g))
   (BYTE (16 #x50FD))
   (OPERAND src)
   (OPERAND dst))

  ((H (? src ea-r-h) (? dst ea-w-h))
   (BYTE (16 #x70FD))
   (OPERAND src)
   (OPERAND dst)))

(define-instruction PUSHL
  (((? src ea-r-l))
   (BYTE (8 #XDD))
   (OPERAND src)))

(define-instruction CLR
  ((B (? dst ea-w-b))
   (BYTE (8 #x94))
   (OPERAND dst))

  ((W (? dst ea-w-w))
   (BYTE (8 #xB4))
   (OPERAND dst))

  ((L (? dst ea-w-l))
   (BYTE (8 #xD4))
   (OPERAND dst))

  ((F (? dst ea-w-f))
   (BYTE (8 #xD4))
   (OPERAND dst))

  ((Q (? dst ea-w-q))
   (BYTE (8 #x7C))
   (OPERAND dst))

  ((D (? dst ea-w-d))
   (BYTE (8 #x7C))
   (OPERAND dst))
  
  ((G (? dst ea-w-g))
   (BYTE (8 #x7C))
   (OPERAND dst))

  ((O (? dst ea-w-o))
   (BYTE (16 #x7CFD))
   (OPERAND dst))

  ((H (? dst ea-w-h))
   (BYTE (16 #x7CFD))
   (OPERAND dst)))

(define-instruction MNEG
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x8E))
   (OPERAND src)
   (OPERAND dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xAE))
   (OPERAND src)
   (OPERAND dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xCE))
   (OPERAND src)
   (OPERAND dst))

  ((F (? src ea-r-f) (? dst ea-w-f))
   (BYTE (8 #x52))
   (OPERAND src)
   (OPERAND dst))

  ((D (? src ea-r-d) (? dst ea-w-d))
   (BYTE (8 #x72))
   (OPERAND src)
   (OPERAND dst))

  ((G (? src ea-r-g) (? dst ea-w-g))
   (BYTE (16 #x52FD))
   (OPERAND src)
   (OPERAND dst))

  ((H (? src ea-r-h) (? dst ea-w-h))
   (BYTE (16 #x72FD))
   (OPERAND src)
   (OPERAND dst)))

(define-instruction MCOM
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x92))
   (OPERAND src)
   (OPERAND dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xB2))
   (OPERAND src)
   (OPERAND dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xD2))
   (OPERAND src)
   (OPERAND dst)))
