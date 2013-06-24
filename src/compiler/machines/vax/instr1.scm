#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; VAX Instruction Set Description, Part 1

;;; The ordering is essentially that in "Vax Architecture Handbook" 1981.

(declare (usual-integrations))

;;;; REMARKS

#|

A) There are two types of operand specifiers:

    - General addressing mode operand specifier, with matching pattern syntax

      (? foo ea-<access-type>-<operand-type>)

      Access types and operand types are described on the "Vax Architecture
      Handbook", on Appendix E. 
      They are implemented in insutl.scm

    - Immediate operands.  The matching syntax is (? value).  The operand
      is processed appropriately by the body of the instruction definition.
      It is used for instruction displacements (ie. the SOB instruction), or
      immediate operands (ie. the BUG instruction).

B) The instruction set is currently incomplete.  In particular, none
of the instructions in chapters 14 or 16 are below.  The missing
opcodes are

- Chap. 14: MOVC, MOVTC, MOVTUC, CMPC, SCANC, SPANC, LOCC, SKPC,
            MATCHC, CRC.

- Chap. 16: EDITPC.

|#

;; Pseudo ops

(define-instruction BYTE
  ((S (? value))
   (BYTE (8 value SIGNED)))
  ((U (? value))
   (BYTE (8 value UNSIGNED))))

(define-instruction WORD
  ((S (? value))
   (BYTE (16 value SIGNED)))
  ((U (? value))
   (BYTE (16 value UNSIGNED))))

(define-instruction LONG
  ((S (? value))
   (BYTE (32 value SIGNED)))
  ((U (? value))
   (BYTE (32 value UNSIGNED))))

;;; Privilleged and miscellaneous (Chap. 10)

(define-instruction CHM
  ((K (? code ea-r-w))		; kernel
   (BYTE (8 #xBC))
   (OPERAND W code))

  ((E (? code ea-r-w))		; executive
   (BYTE (8 #xBD))
   (OPERAND W code))

  ((S (? code ea-r-w))		; supervisor
   (BYTE (8 #xBE))
   (OPERAND W code))

  ((U (? code ea-r-w))		; user
   (BYTE (8 #xBF))
   (OPERAND W code)))

(define-instruction PROBE
  ((R (? mode ea-r-b) (? len ea-r-w) (? base ea-a-b))
   (BYTE (8 #x0C))
   (OPERAND B mode)
   (OPERAND W len)
   (OPERAND B base))

  ((W (? mode ea-r-b) (? len ea-r-w) (? base ea-a-b))
   (BYTE (8 #x0D))
   (OPERAND B mode)
   (OPERAND W len)
   (OPERAND B base)))

(define-trivial-instruction REI #x02)
(define-trivial-instruction LDPCTX #x06)
(define-trivial-instruction SVPCTX #x07)

(define-instruction MTPR
  (((? src ea-r-l) (? procreg ea-r-l))
   (BYTE (8 #xDA))
   (OPERAND L src)
   (OPERAND L procreg)))

(define-instruction MFPR
  (((? procreg ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xDB))
   (OPERAND L procreg)
   (OPERAND L dst)))

(define-trivial-instruction XFC #xFC)

(define-trivial-instruction BPT #x03)

(define-instruction BUG
  ((W (? message))
   (BYTE (16 #xFEFF))
   (BYTE (16 message)))

  ((L (? message))
   (BYTE (16 #xFDFF))
   (BYTE (32 message))))

(define-trivial-instruction HALT #x00)

;;;; Integer and floating point instructions (Chap. 11)

(define-instruction MOV
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x90))
   (OPERAND B src)
   (OPERAND B dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xB0))
   (OPERAND W src)
   (OPERAND W dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xD0))
   (OPERAND L src)
   (OPERAND L dst))

  ((Q (? src ea-r-q) (? dst ea-w-q))
   (BYTE (8 #x7D))
   (OPERAND Q src)
   (OPERAND Q dst))

  ((O (? src ea-r-o) (? dst ea-w-o))
   (BYTE (16 #x7DFD))
   (OPERAND O src)
   (OPERAND O dst))

  ((F (? src ea-r-f) (? dst ea-w-f))
   (BYTE (8 #x50))
   (OPERAND F src)
   (OPERAND F dst))

  ((D (? src ea-r-d) (? dst ea-w-d))
   (BYTE (8 #x70))
   (OPERAND D src)
   (OPERAND D dst))

  ((G (? src ea-r-g) (? dst ea-w-g))
   (BYTE (16 #x50FD))
   (OPERAND G src)
   (OPERAND G dst))

  ((H (? src ea-r-h) (? dst ea-w-h))
   (BYTE (16 #x70FD))
   (OPERAND H src)
   (OPERAND H dst)))

(define-instruction PUSHL
  (((? src ea-r-l))
   (BYTE (8 #xDD))
   (OPERAND L src)))

(define-instruction CLR
  ((B (? dst ea-w-b))
   (BYTE (8 #x94))
   (OPERAND B dst))

  ((W (? dst ea-w-w))
   (BYTE (8 #xB4))
   (OPERAND W dst))

  ((L (? dst ea-w-l))
   (BYTE (8 #xD4))
   (OPERAND L dst))

  ((F (? dst ea-w-f))
   (BYTE (8 #xD4))
   (OPERAND F dst))

  ((Q (? dst ea-w-q))
   (BYTE (8 #x7C))
   (OPERAND Q dst))

  ((D (? dst ea-w-d))
   (BYTE (8 #x7C))
   (OPERAND D dst))
  
  ((G (? dst ea-w-g))
   (BYTE (8 #x7C))
   (OPERAND G dst))

  ((O (? dst ea-w-o))
   (BYTE (16 #x7CFD))
   (OPERAND O dst))

  ((H (? dst ea-w-h))
   (BYTE (16 #x7CFD))
   (OPERAND H dst)))

(define-instruction MNEG
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x8E))
   (OPERAND B src)
   (OPERAND B dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xAE))
   (OPERAND W src)
   (OPERAND W dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xCE))
   (OPERAND L src)
   (OPERAND L dst))

  ((F (? src ea-r-f) (? dst ea-w-f))
   (BYTE (8 #x52))
   (OPERAND F src)
   (OPERAND F dst))

  ((D (? src ea-r-d) (? dst ea-w-d))
   (BYTE (8 #x72))
   (OPERAND F src)
   (OPERAND F dst))

  ((G (? src ea-r-g) (? dst ea-w-g))
   (BYTE (16 #x52FD))
   (OPERAND G src)
   (OPERAND G dst))

  ((H (? src ea-r-h) (? dst ea-w-h))
   (BYTE (16 #x72FD))
   (OPERAND H src)
   (OPERAND H dst)))

(define-instruction MCOM
  ((B (? src ea-r-b) (? dst ea-w-b))
   (BYTE (8 #x92))
   (OPERAND B src)
   (OPERAND B dst))

  ((W (? src ea-r-w) (? dst ea-w-w))
   (BYTE (8 #xB2))
   (OPERAND W src)
   (OPERAND W dst))

  ((L (? src ea-r-l) (? dst ea-w-l))
   (BYTE (8 #xD2))
   (OPERAND L src)
   (OPERAND L dst)))