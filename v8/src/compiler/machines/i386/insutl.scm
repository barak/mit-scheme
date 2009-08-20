#| -*-Scheme-*-

$Id$

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

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
   (BYTE (3 4)
	 (3 4)
	 (2 0)))

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
   (BYTE (3 4)
	 (3 4)
	 (2 0)
	 (8 offset SIGNED)))

  ((@RO UB 4 (? offset))
   (MEMORY)
   #b01 4
   (BYTE (3 4)
	 (3 4)
	 (2 0)
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
   (BYTE (3 4)
	 (3 4)
	 (2 0))
   (IMMEDIATE offset ADDRESS SIGNED))

  ((@RO UW 4 (? offset))		; ESP
   (MEMORY)
   #b10 #b100
   (BYTE (3 4)
	 (3 4)
	 (2 0))
   (IMMEDIATE offset ADDRESS UNSIGNED))
   
  ((@RI (? b base-reg) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b00 #b100
   (BYTE (3 b)
	 (3 i)
	 (2 s)))

  ((@RI 5 (? i index-reg) (? s index-scale)) ; EBP
   (MEMORY)
   #b01 #b100
   (BYTE (3 5)
	 (3 i)
	 (2 s)
	 (8 0)))

  ((@ROI B (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b01 #b100
   (BYTE (3 b)
	 (3 i)
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI UB (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b01 #b100
   (BYTE (3 b)
	 (3 i)
	 (2 s)
	 (8 offset UNSIGNED)))

  ((@ROI W (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b10 #b100
   (BYTE (3 b)
	 (3 i)
	 (2 s))
   (IMMEDIATE offset ADDRESS SIGNED))

  ((@ROI UW (? b) (? offset) (? i index-reg) (? s index-scale))
   (MEMORY)
   #b10 #b100
   (BYTE (3 b)
	 (3 i)
	 (2 s))
   (IMMEDIATE offset ADDRESS UNSIGNED))

  ((@ (? value))
   (MEMORY)
   #b00 #b101
   (IMMEDIATE value ADDRESS)))

(define-ea-transformer r/mW)
(define-ea-transformer mW MEMORY)
(define-ea-transformer r/mB)
(define-ea-transformer mB MEMORY)

(define-structure (effective-address
		   (conc-name ea/)
		   (constructor make-effective-address))
  (keyword false read-only true)
  (categories false read-only true)
  (mode false read-only true)
  (register false read-only true)
  (extra '() read-only true))

(define (sign-extended-byte value)
  (and (fits-in-signed-byte? value)
       value))

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