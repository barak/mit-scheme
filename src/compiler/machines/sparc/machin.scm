#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Machine Model for SPARC
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? false)
(define endianness 'BIG)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-type-width 6)	;or 8
(define-integrable type-scale-factor (expt 2 (- 8 scheme-type-width)))

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable float-width 64)
(define-integrable float-alignment 64)

(define-integrable address-units-per-float
  (quotient float-width addressing-granularity))

;;; It is currently required that both packed characters and objects
;;; be integrable numbers of address units.  Furthermore, the number
;;; of address units per object must be an integral multiple of the
;;; number of address units per character.  This will cause problems
;;; on a machine that is word addressed, in which case we will have to
;;; rethink the character addressing strategy.

(define-integrable address-units-per-object
  (quotient scheme-object-width addressing-granularity))

(define-integrable address-units-per-packed-char 1)

(define-integrable signed-fixnum/upper-limit (expt 2 (-1+ scheme-datum-width)))
(define-integrable signed-fixnum/lower-limit (- signed-fixnum/upper-limit))
(define-integrable unsigned-fixnum/upper-limit (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)
(define-integrable execute-cache-size 3) ; Long words per UUO link slot
(define-integrable closure-entry-size
  ;; Long words in a single closure entry:
  ;;   Format + GC offset word
  ;;   SETHI
  ;;   JALR/JAL
  ;;   ADDI
  4)

;; Given: the number of entry points in a closure, and a particular
;; entry point number. Return: the distance from that entry point to
;; the first variable slot in the closure (in words).

(define (closure-first-offset nentries entry)
  (if (zero? nentries)
      1					; Strange boundary case
      (- (* closure-entry-size (- nentries entry)) 1)))

;; Like the above, but from the start of the complete closure object,
;; viewed as a vector, and including the header word.

(define (closure-object-first-offset nentries)
  (case nentries
    ((0)
     ;; Vector header only
     1)
    ((1)
     ;; Manifest closure header followed by single entry point
     (+ 1 closure-entry-size))
    (else
     ;; Manifest closure header, number of entries, then entries.
     (+ 1 1 (* closure-entry-size nentries)))))

;; Bump from one entry point to another -- distance in BYTES

(define (closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* (* closure-entry-size 4) (- entry* entry)))

;; Bump to the canonical entry point.  On a RISC (which forces
;; longword alignment for entry points anyway) there is no need to
;; canonicalize.

(define (closure-environment-adjustment nentries entry)
  nentries entry			; ignored
  0)

;;;; Machine Registers

(define-integrable g0 0)
(define-integrable g1 1)
(define-integrable g2 2)
(define-integrable g3 3)
(define-integrable g4 4)
(define-integrable g5 5)
(define-integrable g6 6)
(define-integrable g7 7)
(define-integrable g8 8)
(define-integrable g9 9)
(define-integrable g10 10)
(define-integrable g11 11)
(define-integrable g12 12)
(define-integrable g13 13)
(define-integrable g14 14)
(define-integrable g15 15)
(define-integrable g16 16)
(define-integrable g17 17)
(define-integrable g18 18)
(define-integrable g19 19)
(define-integrable g20 20)
(define-integrable g21 21)
(define-integrable g22 22)
(define-integrable g23 23)
(define-integrable g24 24)
(define-integrable g25 25)
(define-integrable g26 26)
(define-integrable g27 27)
(define-integrable g28 28)
(define-integrable g29 29)
(define-integrable g30 30)
(define-integrable g31 31)

;; Floating point general registers --  the odd numbered ones are
;; only used when transferring to/from the CPU
(define-integrable fp0 32)
(define-integrable fp1 33)
(define-integrable fp2 34)
(define-integrable fp3 35)
(define-integrable fp4 36)
(define-integrable fp5 37)
(define-integrable fp6 38)
(define-integrable fp7 39)
(define-integrable fp8 40)
(define-integrable fp9 41)
(define-integrable fp10 42)
(define-integrable fp11 43)
(define-integrable fp12 44)
(define-integrable fp13 45)
(define-integrable fp14 46)
(define-integrable fp15 47)
(define-integrable fp16 48)
(define-integrable fp17 49)
(define-integrable fp18 50)
(define-integrable fp19 51)
(define-integrable fp20 52)
(define-integrable fp21 53)
(define-integrable fp22 54)
(define-integrable fp23 55)
(define-integrable fp24 56)
(define-integrable fp25 57)
(define-integrable fp26 58)
(define-integrable fp27 59)
(define-integrable fp28 60)
(define-integrable fp29 61)
(define-integrable fp30 62)
(define-integrable fp31 63)

(define-integrable number-of-machine-registers 64)
(define-integrable number-of-temporary-registers 256)

;;; Fixed-use registers for Scheme compiled code.
(define-integrable regnum:return-value g16)
(define-integrable regnum:stack-pointer g17)
(define-integrable regnum:memtop g18)
(define-integrable regnum:free g19)
(define-integrable regnum:scheme-to-interface g20)
(define-integrable regnum:dynamic-link g21)
(define-integrable regnum:closure-free g22)
(define-integrable regnum:address-mask g25)
(define-integrable regnum:regs-pointer g26)
(define-integrable regnum:quad-bits g27)
(define-integrable regnum:closure-hook g28)
(define-integrable regnum:interface-index g13)

;;; Fixed-use registers due to architecture or OS calling conventions.
(define-integrable regnum:zero g0)
(define-integrable regnum:assembler-temp g1)
(define-integrable regnum:C-return-receive-value g8)
(define-integrable regnum:C-return-send-value g24)
(define-integrable regnum:C-stack-pointer g14)
(define-integrable regnum:first-arg g8)
(define-integrable regnum:second-arg g9)
(define-integrable regnum:third-arg g10)
(define-integrable regnum:fourth-arg g11)
(define-integrable regnum:fifth-arg g12)
(define-integrable regnum:sixth-arg g13)
(define-integrable regnum:reserved-global-1 g2)
(define-integrable regnum:reserved-global-2 g3)
(define-integrable regnum:reserved-global-3 g4)
(define-integrable regnum:reserved-global-4 g5)
(define-integrable regnum:reserved-global-5 g6)
(define-integrable regnum:reserved-global-6 g7)
(define-integrable regnum:linkage g31)
(define-integrable regnum:call-result g15)

(define address-regs
  (list regnum:stack-pointer regnum:memtop regnum:free regnum:dynamic-link
	regnum:linkage))

(define object-regs
  (list regnum:return-value regnum:C-return-send-value))

(define immediate-regs
  (list regnum:address-mask regnum:quad-bits))

(define unboxed-regs
  (list regnum:scheme-to-interface
	regnum:regs-pointer regnum:assembler-temp
	regnum:reserved-global-4
	regnum:reserved-global-5
	regnum:reserved-global-6
	regnum:C-stack-pointer
	))
	
(define machine-register-value-class
  (lambda (register)
    (cond ((member register address-regs) value-class=address)
	  ((member register object-regs) value-class=object)
	  ((member register immediate-regs) value-class=immediate)
	  ((member register unboxed-regs) value-class=unboxed)
	  ((<= g0 register g31) value-class=word)
	  ((<= fp0 register fp31) value-class=float)
	  (else (error "illegal machine register" register)))))

(define-integrable (machine-register-known-value register)
  register				;ignore
  false)

;;;; Interpreter Registers

(define-integrable (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free))

(define (interpreter-free-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:free)))

(define-integrable (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:regs-pointer))

(define (interpreter-regs-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:regs-pointer)))

(define-integrable (interpreter-value-register)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-value-register? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:return-value)))

(define-integrable (interpreter-stack-pointer)
  (rtl:make-machine-register regnum:stack-pointer))

(define (interpreter-stack-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:stack-pointer)))

(define-integrable (interpreter-dynamic-link)
  (rtl:make-machine-register regnum:dynamic-link))

(define (interpreter-dynamic-link? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:dynamic-link)))

(define-integrable (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer) 3))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (= 3 (rtl:offset-number expression))))

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register regnum:C-return-send-value))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register regnum:C-return-send-value))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register regnum:C-return-send-value))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register regnum:C-return-send-value))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register regnum:C-return-send-value))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register regnum:C-return-send-value))

;;;; RTL Registers, Constants, and Primitives

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
    ((DYNAMIC-LINK)
     (interpreter-dynamic-link))
    ((VALUE)
     (interpreter-value-register))
    ((INTERPRETER-CALL-RESULT:ACCESS)
     (interpreter-register:access))
    ((INTERPRETER-CALL-RESULT:CACHE-REFERENCE)
     (interpreter-register:cache-reference))
    ((INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?)
     (interpreter-register:cache-unassigned?))
    ((INTERPRETER-CALL-RESULT:LOOKUP)
     (interpreter-register:lookup))
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?)
     (interpreter-register:unassigned?))
    ((INTERPRETER-CALL-RESULT:UNBOUND?)
     (interpreter-register:unbound?))
    (else false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP) 0)
    ((STACK-GUARD) 1)
    ((ENVIRONMENT) 3)
    ((TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  ;; Magic numbers.
  (let ((if-integer
	 (lambda (value)
	   (cond ((zero? value) 1)
		 ((or (fits-in-16-bits-signed? value)
		      (fits-in-16-bits-unsigned? value)
		      (top-16-bits-only? value))
		  2)
		 (else 3)))))
    (let ((if-synthesized-constant
	   (lambda (type datum)
	     (if-integer (make-non-pointer-literal type datum)))))
      (case (rtl:expression-type expression)
	((CONSTANT)
	 (let ((value (rtl:constant-value expression)))
	   (if (non-pointer-object? value)
	       (if-synthesized-constant (object-type value)
					(object-datum value))
	       3)))
	((MACHINE-CONSTANT)
	 (if-integer (rtl:machine-constant-value expression)))
	((ENTRY:PROCEDURE
	  ENTRY:CONTINUATION
	  ASSIGNMENT-CACHE
	  VARIABLE-CACHE
	  OFFSET-ADDRESS)
	 3)
	((CONS-NON-POINTER)
	 (and (rtl:machine-constant? (rtl:cons-non-pointer-type expression))
	      (rtl:machine-constant? (rtl:cons-non-pointer-datum expression))
	      (if-synthesized-constant
	       (rtl:machine-constant-value
		(rtl:cons-non-pointer-type expression))
	       (rtl:machine-constant-value
		(rtl:cons-non-pointer-datum expression)))))
	(else false)))))

(define compiler:open-code-floating-point-arithmetic?
  true)

(set! compiler:open-code-primitives? #f)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM FIXNUM-QUOTIENT FIXNUM-REMAINDER
    FIXNUM-NOT FIXNUM-AND FIXNUM-ANDC FIXNUM-OR FIXNUM-XOR FIXNUM-LSH
    INTEGER-QUOTIENT INTEGER-REMAINDER &/ QUOTIENT REMAINDER
    FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN FLONUM-ACOS
    FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-TRUNCATE FLONUM-ROUND
    FLONUM-REMAINDER FLONUM-SQRT FLONUM-EXPM1 FLONUM-LOG1P
    VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS
    FLOATING-VECTOR-REF FLOATING-VECTOR-SET!))