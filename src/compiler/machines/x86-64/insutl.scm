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

  ((R (? r))
   (CATEGORIES REGISTER)
   (REX (B r))
   (MODE #b11)
   (R/M (register-bits r)))

  ((XMM (? r))
   (CATEGORIES XMM)
   (REX (B r))
   (MODE #b11)
   (R/M (register-bits r)))

;;;; Register-indirect

  ((@R (? r indirect-reg))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b00)
   (R/M (register-bits r)))

  ;; Mode #b00, r/m 4 means SIB, so put the register in a SIB base and
  ;; use no index (i.e. index of 4).

  ((@R (? r indirect-reg=4mod8))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b00)
   (R/M 4)
   (BITS (3 4)
	 (3 4)
	 (2 0)))

  ;; Mode #b00, r/m 5 means RIP-relative 32-bit offset, so use mode
  ;; #b01, r/m 5, which means the register plus 8-bit offset, and
  ;; specify a zero offset.

  ((@R (? r indirect-reg=5mod8))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b01)
   (R/M 5)
   (BITS (8 0)))

;;;; Register-indirect with 8-bit offset

  ((@RO (? r offset-indirect-reg) (? offset sign-extended-byte))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b01)
   (R/M (register-bits r))
   (BITS (8 offset SIGNED)))

  ;; Mode #b01, r/m 4 means SIB plus 8-bit offset, so use the SIB base
  ;; for the register with no index (i.e. index of 4).

  ((@RO (? r offset-indirect-reg=4mod8) (? offset sign-extended-byte))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b01)
   (R/M 4)
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (8 offset SIGNED)))

;;;; Register-indirect with 32-bit offset

  ((@RO (? r offset-indirect-reg) (? offset sign-extended-long))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b10)
   (R/M (register-bits r))
   (BITS (32 offset SIGNED)))

  ;; Same special case as above, but with 32-bit offsets.

  ((@RO (? r offset-indirect-reg=4mod8) (? offset sign-extended-long))
   (CATEGORIES MEMORY)
   (REX (B r))
   (MODE #b10)
   (R/M 4)
   (BITS (3 4)
	 (3 4)
	 (2 0)
	 (32 offset SIGNED)))

;;;; Register-indirect with index

  ((@RI (? b base-reg) (? i index-reg) (? s index-scale))
   (CATEGORIES MEMORY)
   (REX (B b) (X i))
   (MODE #b00)
   (R/M 4)
   (BITS (3 (register-bits b))
	 (3 (register-bits i))
	 (2 s)))

  ;; Mode 0, r/m 4, SIB base 5 mean the register plus 32-bit offset,
  ;; so specify a zero offset.

  ((@RI (? b base-reg=5mod8) (? i index-reg) (? s index-scale))
   (CATEGORIES MEMORY)
   (REX (B b) (X i))
   (MODE #b01)
   (R/M 4)
   (BITS (3 5)
	 (3 (register-bits i))
	 (2 s)
	 (8 0)))

;;;; Register-indirect with offset and scaled index

  ;; No more special cases -- except that rsp can't be the index
  ;; register at all here.

  ((@ROI (? b) (? offset sign-extended-byte) (? i index-reg) (? s index-scale))
   (CATEGORIES MEMORY)
   (REX (B b) (X i))
   (MODE #b01)
   (R/M 4)
   (BITS (3 (register-bits b))
	 (3 (register-bits i))
	 (2 s)
	 (8 offset SIGNED)))

  ((@ROI (? b) (? offset sign-extended-long) (? i index-reg) (? s index-scale))
   (CATEGORIES MEMORY)
   (REX (B b) (X i))
   (MODE #b10)
   (R/M 4)
   (BITS (3 (register-bits b))
	 (3 (register-bits i))
	 (2 s)
	 (32 offset SIGNED)))

;;;; RIP-relative (PC-relative)

  ((@PCR (? label))
   (CATEGORIES MEMORY)
   (REX)
   (MODE #b00)
   (R/M 5)
   (BITS (32 `(- ,label (+ *PC* 4)) SIGNED)))

  ((@PCO (? offset signed-long))
   (CATEGORIES MEMORY)
   (REX)
   (MODE #b00)
   (R/M 5)
   (BITS (32 offset SIGNED))))

(define-ea-transformer r-ea REGISTER)
(define-ea-transformer xmm-ea XMM)
(define-ea-transformer m-ea MEMORY)
(define-ea-transformer r/m-ea REGISTER MEMORY)
(define-ea-transformer xmm/m-ea XMM MEMORY)

(define-structure (effective-address
		   (conc-name ea/)
		   (constructor make-effective-address))
  (keyword #f read-only #t)
  (categories #f read-only #t)
  (rex-prefix #f read-only #t)
  (mode #f read-only #t)
  (register #f read-only #t)
  (extra '() read-only #t))

(declare (integrate-operator register-rex))
(define-integrable (register-rex register rex)
  (declare (integrate register))
  (if (>= register 8)
      rex
      0))

(define (cons-ModR/M digit ea tail)
  (cons-syntax (ea/register ea)
    (cons-syntax digit
      (cons-syntax (ea/mode ea)
	(append-syntax! (ea/extra ea) tail)))))

(declare (integrate-operator opcode-register))
(define (opcode-register opcode register)
  (declare (integrate opcode))
  (+ opcode (if (>= register 8) (- register 8) register)))

(declare (integrate-operator float-comparator))
(define (float-comparator comparator)
  (case comparator
    ((=) 0)
    ((<) 1)
    ((<=) 2)
    ((UNORDERED) 3)
    ((/=) 4)
    ((>=) 5)
    ((>) 6)
    ((ORDERED) 7)
    (else (error "Bad float comparator:" comparator))))

(declare (integrate-operator operand-size))
(define (operand-size s)
  ;; B must be handled separately in general.
  (case s
    ((W L Q) s)
    (else #f)))

(declare (integrate-operator float-packed/scalar))
(define (float-packed/scalar s)
  (case s
    ((S P) s)
    (else #f)))

(declare (integrate-operator float-precision))
(define (float-precision s)
  (case s
    ((D S) s)
    (else #f)))

;;; The REX prefix must come last, just before the `actual' opcode.

(define (cons-prefix operand-size register ea tail)
  ((lambda (tail)
     (if (eq? operand-size 'W)
	 (cons-syntax (syntax-evaluation #x66 coerce-8-bit-unsigned)
		      tail)
	 tail))
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
		(and register (>= register 8)))))
       (if ea
	   (fix:or (if extended-register? #x44 0) (ea/rex-prefix ea))
	   (if extended-register? #x41 0)))))))

(define (cons-float-prefix register ea packed/scalar precision tail)
  ((lambda (tail)
     (let ((float (list packed/scalar precision)))
       (if (equal? float '(P S))
	   tail
	   (cons-syntax (syntax-evaluation
			 (cond ((equal? float '(P D)) #x66)
			       ((equal? float '(S D)) #xF2)
			       ((equal? float '(S S)) #xF3)
			       (else (error "Bad float type:" float)))
			 coerce-8-bit-unsigned)
			tail))))
   (let ((rex-prefix
	  (let ((extended-register?
		 (or (eqv? register #t)
		     (and register (>= register 8)))))
	    (if ea
		(fix:or (if extended-register? #x44 0) (ea/rex-prefix ea))
		(if extended-register? #x41 0)))))
     (if (zero? rex-prefix)
	 tail
	 (cons-syntax (syntax-evaluation rex-prefix coerce-8-bit-unsigned)
		      tail)))))

(define-integrable (register-bits r)
  (fix:and r #b111))

(declare (integrate-operator indirect-reg))
(define (indirect-reg r)
  (and (not (let ((bits (register-bits r)))
	      (or (= bits 4)
		  (= bits 5))))
       r))

(declare (integrate-operator indirect-reg=4mod8))
(define (indirect-reg=4mod8 r)
  (and (= (register-bits r) 4)
       r))

(declare (integrate-operator indirect-reg=5mod8))
(define (indirect-reg=5mod8 r)
  (and (= (register-bits r) 5)
       r))

(declare (integrate-operator offset-indirect-reg))
(define (offset-indirect-reg r)
  (and (not (= (register-bits r) 4))
       r))

(declare (integrate-operator offset-indirect-reg=4mod8))
(define (offset-indirect-reg=4mod8 r)
  (and (= (register-bits r) 4)
       r))

(declare (integrate-operator base-reg))
(define (base-reg r)
  (and (not (= (register-bits r) 5))
       r))

(declare (integrate-operator base-reg=5mod8))
(define (base-reg=5mod8 r)
  (and (= (register-bits r) 5)
       r))

(declare (integrate-operator index-reg))
(define (index-reg r)
  (and (not (= r 4))
       r))

(define-integrable (index-scale scale-value)
  (case scale-value
    ((1) #b00)
    ((2) #b01)
    ((4) #b10)
    ((8) #b11)
    (else false)))

(declare (integrate-operator unsigned-2bit))
(define (unsigned-2bit value)
  (and (<= 0 value #b11) value))

(declare (integrate-operator unsigned-3bit))
(define (unsigned-3bit value)
  (and (<= 0 value #b111) value))

(declare (integrate-operator unsigned-5bit))
(define (unsigned-5bit value)
  (and (<= 0 value #b11111) value))

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