#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/insutl.scm,v 1.3 1992/02/11 14:47:21 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Intel 386 Instruction Set, utilities

(declare (usual-integrations))

;;;; Addressing modes

;; r/m part of ModR/M byte and SIB byte.
;; These are valid only for 32-bit addressing.

(define-ea-database
  ((R (? r))
   (REGISTER)
   #b11 r)

  ((@R (? r indirect-reg))
   (MEMORY)
   #b00 r)

  ((@R 5)				; EBP
   (MEMORY)
   #b01 5
   (BYTE (8 0)))

  ((@R 4)				; ESP
   (MEMORY)
   #b00 4
   (BYTE (2 0)
	 (3 4)
	 (3 4)))

  ((@RO B (? r index-reg) (? offset))
   (MEMORY)
   #b01 r
   (BYTE (8 offset SIGNED)))

  ((@RO UB (? r index-reg) (? offset))
   (MEMORY)
   #b01 r
   (BYTE (8 offset UNSIGNED)))

  ((@RO B 4 (? offset))
   (MEMORY)
   #b01 4
   (BYTE (2 0)
	 (3 4)
	 (3 4)
	 (8 offset SIGNED)))

  ((@RO UB 4 (? offset))
   (MEMORY)
   #b01 4
   (BYTE (2 0)
	 (3 4)
	 (3 4)
	 (8 offset UNSIGNED)))

  ((@RO W (? r index-reg) (? offset))
   (MEMORY)
   #b10 r
   (IMMEDIATE offset ADDRESS SIGNED))

  ((@RO UW (? r index-reg) (? offset))
   (MEMORY)
   #b10 r
   (IMMEDIATE offset ADDRESS UNSIGNED))

  ((@RO W 4 (? offset))			; ESP
   (MEMORY)
   #b10 #b100
   (BYTE (2 0)
	 (3 4)
	 (3 4))
   (IMMEDIATE offset ADDRESS SIGNED))

  ((@RO UW 4 (? offset))		; ESP
   (MEMORY)
   #b10 #b100
   (BYTE (2 0)
	 (3 4)
	 (3 4))
   (IMMEDIATE offset ADDRESS UNSIGNED))
   
  ((@RI (? b base-reg) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b00 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 b)))

  ((@RI 5 (? i index-reg) (? s index-scale)) ; EBP
   (MEMORY)
   #b01 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 5)
	 (8 0)))

  ((@ROI B (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b01 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 b)
	 (8 offset SIGNED)))

  ((@ROI UB (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b01 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 b)
	 (8 offset UNSIGNED)))

  ((@ROI W (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b10 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 b))
   (IMMEDIATE offset ADDRESS SIGNED))

  ((@ROI UW (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b10 #b100
   (BYTE (2 s)
	 (3 i)
	 (3 b))
   (IMMEDIATE offset ADDRESS UNSIGNED))

  ((@ (? value))
   (MEMORY)
   #b00 #b101
   (IMMEDIATE value ADDRESS)))

(define-integrable (fits-in-signed-byte? value)
  (and (>= value -128) (< value 128)))

(define (sign-extended-byte value)
  (and (fits-in-signed-byte? value)
       value))

(define-integrable (fits-in-unsigned-byte? value)
  (and (>= value 0) (< value 128)))

(define (zero-extended-byte value)
  (and (fits-in-unsigned-byte? value)
       value))

(define-integrable (indirect-reg r)
  (and (not (= r esp))
       (not (= r ebp))
       r))

(define-integrable (base-reg r)
  (and (not (= r ebp))
       r))

(define-integrable (index-reg r)
  (and (not (= r esp))
       r))

(define (index-scale scale-value)
  (case scale-value
    ((1) #b00)
    ((2) #b01)
    ((4) #b10)
    ((8) #b11)
    (else false)))