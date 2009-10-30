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

;;;; Intel 386 Instruction Set, utilities

(declare (usual-integrations))

;;;; Addressing modes

;; r/m part of ModR/M byte and SIB byte.
;; These are valid only for 64-bit addressing.

(define-ea-database

;;;; Register

  ((R (? r extended-reg))
   (REGISTER)
   #x41 #b11 r)

  ((R (? r))
   (REGISTER)
   0 #b11 r)

;;;; Register-indirect

  ((@R (? r extended-indirect-reg))
   (MEMORY)
   #x41 #b00 r)

  ((@R (? r indirect-reg))
   (MEMORY)
   0 #b00 r)

  ;; Mode of 0 with R/M of 4 means that what follows is a SIB format,
  ;; and R/M of 5 means that what follows is a PC-relative immediate
  ;; offset (in 64-bit mode), so we must have special cases for rsp,
  ;; rbp, r12, and r13.

  ;; SIB format, with no scale.
  ((@R 4)				; rsp
   (MEMORY)
   0 #b00 4
   (BITS (3 4)
	 (3 4)
	 (2 0)))

  ;; rbp plus offset, with zero offset.
  ((@R 5)				; rbp
   (MEMORY)
   0 #b01 5
   (BITS (8 0)))

  ;; SIB format, with no scale.
  ((@R 12)
   (MEMORY)
   #x41 #b00 4
   (BITS (3 4)
	 (3 4)
	 (2 0)))

  ;; r13 plus offset, with zero offset.
  ((@R 13)
   (MEMORY)
   #x41 #b01 5
   (BITS (8 0)))

;;;; Register-indirect with 8-bit Offset

  ;; Mode of #b01 with R/M of 13 means SIB plus offset, so we must
  ;; have special cases for rsp and r12.

  ((@RO B (? r extended-index-reg) (? offset))
   (MEMORY)
   #x41 #b01 r
   (BITS (8 offset SIGNED)))

  ((@RO B (? r index-reg) (? offset))
   (MEMORY)
   0 #b01 r
   (BITS (8 offset SIGNED)))

  ((@RO UB (? r extended-index-reg) (? offset))
   (MEMORY)
   #x41 #b01 r
   (BITS (8 offset UNSIGNED)))

  ((@RO UB (? r index-reg) (? offset))
   (MEMORY)
   0 #b01 r
   (BITS (8 offset UNSIGNED)))

  ((@RO B 4 (? offset))			; rsp
   (MEMORY)
   0 #b01 4
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (8 offset SIGNED)))

  ((@RO B 12 (? offset))
   (MEMORY)
   #x41 #b01 4
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (8 offset SIGNED)))

  ((@RO UB 4 (? offset))
   (MEMORY)
   0 #b01 4
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (8 offset UNSIGNED)))

  ((@RO UB 12 (? offset))
   (MEMORY)
   #x41 #b01 4
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (8 offset UNSIGNED)))

;;;; Register-indirect with 32-bit Offset

  ((@RO L (? r extended-index-reg) (? offset signed-long))
   (MEMORY)
   #x41 #b10 r
   (BITS (32 offset SIGNED)))

  ((@RO L (? r index-reg) (? offset signed-long))
   (MEMORY)
   0 #b10 r
   (BITS (32 offset SIGNED)))

  ((@RO L 4 (? offset signed-long))	; rsp
   (MEMORY)
   0 #b10 #b100
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (32 offset SIGNED)))

  ((@RO L 12 (? offset signed-long))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (32 offset SIGNED)))

  ((@RO UL (? r extended-index-reg) (? offset unsigned-long))
   (MEMORY)
   #x41 #b10 r
   (BITS (32 offset UNSIGNED)))

  ((@RO UL (? r index-reg) (? offset unsigned-long))
   (MEMORY)
   0 #b10 r
   (BITS (32 offset UNSIGNED)))

  ((@RO UL 4 (? offset unsigned-long))	; rsp
   (MEMORY)
   0 #b10 #b100
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (32 offset UNSIGNED)))

  ((@RO UL 12 (? offset unsigned-long))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (32 offset UNSIGNED)))
   
;;;; Register-indirect Indexed

  ((@RI (? b extended-base-reg) (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x43 #b00 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)))
   
  ((@RI (? b extended-base-reg) (? i index-reg) (? s index-scale))
   (MEMORY)
   #x41 #b00 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)))
   
  ((@RI (? b base-reg) (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x42 #b00 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)))
   
  ((@RI (? b base-reg) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b00 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)))

  ((@RI 5 (? i extended-index-reg) (? s index-scale)) ; rbp
   (MEMORY)
   #x42 #b01 #b100
   (BITS (3 5)
	 (3 i)
	 (2 s)
	 (8 0)))

  ((@RI 5 (? i index-reg) (? s index-scale)) ; rbp
   (MEMORY)
   0 #b01 #b100
   (BITS (3 5)
	 (3 i)
	 (2 s)
	 (8 0)))

  ((@RI 13 (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x43 #b01 #b100
   (BITS (3 5)
	 (3 i)
	 (2 s)
	 (8 0)))

  ((@RI 13 (? i index-reg) (? s index-scale))
   (MEMORY)
   #x41 #b01 #b100
   (BITS (3 5)
	 (3 i)
	 (2 s)
	 (8 0)))

;;;; Register-indirect with Offset, Indexed

  ((@ROI B (? b extended-reg) (? offset signed-byte) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x43 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI B (? b extended-reg) (? offset signed-byte) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI B (? b) (? offset signed-byte) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI B (? b) (? offset signed-byte) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI UB (? b extended-reg) (? offset unsigned-byte)
	 (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x43 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset UNSIGNED)))

  ((@ROI UB (? b extended-reg) (? offset unsigned-byte) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset UNSIGNED)))

  ((@ROI UB (? b) (? offset unsigned-byte) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset UNSIGNED)))

  ((@ROI UB (? b) (? offset unsigned-byte) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b01 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (8 offset UNSIGNED)))

  ((@ROI W (? b extended-reg) (? offset signed-word) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x43 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset SIGNED)))

  ((@ROI W (? b extended-reg) (? offset signed-word) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset SIGNED)))

  ((@ROI W (? b) (? offset signed-word) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset SIGNED)))

  ((@ROI W (? b) (? offset signed-word) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset SIGNED)))

  ((@ROI UW (? b extended-reg) (? offset unsigned-word)
	 (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x43 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset UNSIGNED)))

  ((@ROI UW (? b extended-reg) (? offset unsigned-word) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset UNSIGNED)))

  ((@ROI UW (? b) (? offset unsigned-word) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset UNSIGNED)))

  ((@ROI UW (? b) (? offset unsigned-word) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (16 offset UNSIGNED)))

  ((@ROI L (? b extended-reg) (? offset signed-long) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x43 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset SIGNED)))

  ((@ROI L (? b extended-reg) (? offset signed-long) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset SIGNED)))

  ((@ROI L (? b) (? offset signed-long) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset SIGNED)))

  ((@ROI L (? b) (? offset signed-long) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset SIGNED)))

  ((@ROI UL (? b extended-reg) (? offset unsigned-long)
	 (? i extended-index-reg) (? s index-scale))
   (MEMORY)
   #x43 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset UNSIGNED)))

  ((@ROI UL (? b extended-reg) (? offset unsigned-long) (? i index-reg)
	 (? s index-scale))
   (MEMORY)
   #x41 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset UNSIGNED)))

  ((@ROI UL (? b) (? offset unsigned-long) (? i extended-index-reg)
	 (? s index-scale))
   (MEMORY)
   #x42 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset UNSIGNED)))

  ((@ROI UL (? b) (? offset unsigned-long) (? i index-reg) (? s index-scale))
   (MEMORY)
   0 #b10 #b100
   (BITS (3 b)
	 (3 i)
	 (2 s)
	 (32 offset UNSIGNED)))

  ((@PCR (? label))
   (MEMORY)
   0 #b00 #b101
   (BITS (32 `(- ,label (+ *PC* 4)) SIGNED)))

  ((@PCO (? offset))
   (MEMORY)
   0 #b00 #b101
   (BITS (32 offset SIGNED))))

(define-ea-transformer r/m-ea)
(define-ea-transformer m-ea MEMORY)

(define-structure (effective-address
		   (conc-name ea/)
		   (constructor make-effective-address))
  (keyword #f read-only #t)
  (categories #f read-only #t)
  (rex-prefix #f read-only #t)
  (mode #f read-only #t)
  (register #f read-only #t)
  (extra '() read-only #t))

(define (cons-prefix operand-size register r/m tail)
  (let ((tail
	 (if (eq? operand-size 'W)
	     (cons-syntax (syntax-evaluation #x66 coerce-8-bit-unsigned)
			  tail)
	     tail)))
    ((lambda (rex-prefix)
       (if (zero? rex-prefix)
	   tail
	   (cons-syntax (syntax-evaluation rex-prefix coerce-8-bit-unsigned)
			tail)))
     (fix:or
      (case operand-size
	;; B must be handled separately; there is no prefix for it.
	;; W is handled with a #x66 prefix.
	;; L is the default.
	((#F W L) 0)
	((Q) #x48)
	(else (error "Invalid operand size:" operand-size)))
      (let ((extended-register?
	     (or (eqv? register #t)
		 (and register (>= register r8)))))
	(if r/m
	    (fix:or (if extended-register? #x44 0) (ea/rex-prefix r/m))
	    (if extended-register? #x41 0)))))))

(define (cons-modr/m digit ea tail)
  (cons-syntax (ea/register ea)
    (cons-syntax digit
      (cons-syntax (ea/mode ea)
	(append-syntax! (ea/extra ea) tail)))))

(declare (integrate-operator opcode-register))
(define (opcode-register opcode register)
  (declare (integrate opcode))
  (+ opcode (if (>= register r8) (- register r8) register)))

(define (operand-size s)
  ;; B must be handled separately in general.
  (case s
    ((W L Q) s)
    (else #f)))

(define (extended-reg r)
  (and (>= r r8)
       (- r r8)))

(define (indirect-reg r)
  (and (< r r8)
       (not (= r rsp))
       (not (= r rbp))
       r))

(define (extended-indirect-reg r)
  (and (not (= r r12))
       (not (= r r13))
       (extended-reg r)))

(define (base-reg r)
  (and (< r r8)
       (not (= r rbp))
       r))

(define (extended-base-reg r)
  (and (not (= r r13))
       (extended-reg r)))

(define (index-reg r)
  (and (< r r8)
       (not (= r rsp))
       r))

(define (extended-index-reg r)
  (and (not (= r r12))
       (extended-reg r)))

(define (index-scale scale-value)
  (case scale-value
    ((1) #b00)
    ((2) #b01)
    ((4) #b10)
    ((8) #b11)
    (else false)))

(define (signed-byte value)
  (and (fits-in-signed-byte? value)
       value))

(define (unsigned-byte value)
  (and (fits-in-unsigned-byte? value)
       value))

(define (signed-word value)
  (and (fits-in-signed-word? value)
       value))

(define (unsigned-word value)
  (and (fits-in-unsigned-word? value)
       value))

(define (signed-long value)
  (and (fits-in-signed-long? value)
       value))

(define (unsigned-long value)
  (and (fits-in-unsigned-long? value)
       value))

(define (signed-quad value)
  (and (fits-in-signed-quad? value)
       value))

(define (unsigned-quad value)
  (and (fits-in-unsigned-quad? value)
       value))

(define (sign-extended-byte value)
  (and (fits-in-signed-byte? value)
       value))

(define (zero-extended-byte value)
  (and (not (negative? value))
       (fits-in-signed-byte? value)
       value))

(define (sign-extended-word value)
  (and (fits-in-signed-word? value)
       value))

(define (zero-extended-word value)
  (and (not (negative? value))
       (fits-in-signed-word? value)
       value))

(define (sign-extended-long value)
  (and (fits-in-signed-long? value)
       value))

(define (zero-extended-long value)
  (and (not (negative? value))
       (fits-in-signed-long? value)
       value))

(define (sign-extended-quad value)
  (and (fits-in-signed-quad? value)
       value))

(define (zero-extended-quad value)
  (and (not (negative? value))
       (fits-in-signed-quad? value)
       value))