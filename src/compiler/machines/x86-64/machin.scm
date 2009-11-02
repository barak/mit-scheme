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

;;;; Machine Model for the AMD x86-64
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? false)
(define-integrable endianness 'LITTLE)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 64)
(define-integrable scheme-type-width 6)	;or 8

;; NOTE: expt is not being constant-folded now.
;; For the time being, some of the parameters below are
;; pre-computed and marked with ***
;; There are similar parameters in lapgen.scm
;; Change them if any of the parameters above change.

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
;;; on a machine that is word addressed: we will have to rethink the
;;; character addressing strategy.

(define-integrable address-units-per-object
  (quotient scheme-object-width addressing-granularity))

(define-integrable address-units-per-packed-char 1)

(define-integrable signed-fixnum/upper-limit
  ;; (expt 2 (-1+ scheme-datum-width)) ***
  #x0200000000000000)

(define-integrable signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable unsigned-fixnum/upper-limit
  (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)

;;;; Closure format

;;; See microcode/cmpintmd/x86-64.h for a description of the layout.

(define-integrable closure-entry-size 2)

(define-integrable address-units-per-closure-manifest address-units-per-object)
(define-integrable address-units-per-entry-format-code 4)
(define-integrable address-units-per-closure-entry-count 4)
(define-integrable address-units-per-closure-padding 4)

;;; (MOV Q (R ,rax) (&U <entry>))	48 B8 <eight-byte immediate>
;;; (CALL (R ,rax))			FF D0
(define-integrable address-units-per-closure-entry-instructions 12)

(define-integrable address-units-per-closure-entry
  (+ address-units-per-entry-format-code
     address-units-per-closure-entry-instructions))

;;; Note:
;;;
;;; (= address-units-per-closure-entry #| 16 |#
;;;    (* closure-entry-size #| 2 |# address-units-per-object #| 8 |#))

;;; Given the number of entries in a closure, and the index of an
;;; entry, return the number of words from that entry's closure
;;; pointer to the location of the storage for the closure's first
;;; free variable.  In this case, the closure pointer is the same as
;;; the compiled entry pointer into the entry instructions.  This is
;;; different from the i386, where the entry instructions are not all
;;; object-aligned, and thus the closure pointer is adjusted to point
;;; to the first entry in the closure block, which is always aligned.
;;;
;;; When there are zero entries, the `closure' is just a vector, and
;;; represented by a tagged pointer to a manifest, following which are
;;; the free variables.  In this case, the first offset is one object
;;; past the manifest's address.

(define (closure-first-offset nentries entry)
  (if (zero? nentries)
      1
      (* (- nentries entry) closure-entry-size)))

;;; Given the number of entry points in a closure, return the distance
;;; in objects from the address of the manifest closure to the address
;;; of the first free variable.

(define (closure-object-first-offset nentries)
  (if (zero? nentries)
      1					;One vector manifest.
      ;; One object for the closure manifest, and one object for the
      ;; leading entry count and the trailing padding.
      (+ 2 (* nentries closure-entry-size))))

;;; Given the number of entries in a closure, and the indices of two
;;; entries, return the number of bytes separating the two entries.

(define (closure-entry-distance nentries entry entry*)
  nentries				;ignore
  (* (- entry* entry) address-units-per-closure-entry))

;;; Given the number of entries in a closure, and the index of an
;;; entry, return the number of bytes to add to a possibly misaligned
;;; closure pointer to obtain a `canonical' entry point, which is
;;; aligned on an object boundary.  Since all closure entry points are
;;; aligned thus on this machine, we need adjust nothing.

(define (closure-environment-adjustment nentries entry)
  nentries entry			;ignore
  0)

;;;; Machine registers

(define rax 0)				; accumulator
(define rcx 1)				; counter register
(define rdx 2)				; multiplication high-half target
(define rbx 3)				; distinguished useful register
(define rsp 4)				; stack pointer
(define rbp 5)				; frame pointer
(define rsi 6)				; string source pointer
(define rdi 7)				; string destination pointer

;;; More general-purpose registers.

(define r8 8)
(define r9 9)
(define r10 10)
(define r11 11)
(define r12 12)
(define r13 13)
(define r14 14)
(define r15 15)

;;; x87 floating-point stack locations, allocated as if registers.

(define fr0 16)
(define fr1 17)
(define fr2 18)
(define fr3 19)
(define fr4 20)
(define fr5 21)
(define fr6 22)
(define fr7 23)

;;; 64-bit media registers (deprecated).

(define mmx0 24)
(define mmx1 25)
(define mmx2 26)
(define mmx3 27)
(define mmx4 28)
(define mmx5 29)
(define mmx6 30)
(define mmx7 31)

;;; 128-bit media registers.

(define xmm0 32)
(define xmm1 33)
(define xmm2 34)
(define xmm3 35)
(define xmm4 36)
(define xmm5 37)
(define xmm6 38)
(define xmm7 39)
(define xmm8 40)
(define xmm9 41)
(define xmm10 42)
(define xmm11 43)
(define xmm12 44)
(define xmm13 45)
(define xmm14 46)
(define xmm15 47)

(define number-of-machine-registers 16)
(define number-of-temporary-registers 256)

(define-integrable regnum:stack-pointer rsp)
(define-integrable regnum:datum-mask rbp)
(define-integrable regnum:regs-pointer rsi)
(define-integrable regnum:free-pointer rdi)

(define-integrable (machine-register-known-value register)
  register				; ignored
  false)

(define (machine-register-value-class register)
  (cond ((<= rax register rbx)
	 value-class=object)
	((= register regnum:datum-mask)
	 value-class=immediate)
	((or (= register regnum:stack-pointer)
	     (= register regnum:free-pointer)
	     (= register regnum:regs-pointer))
	 value-class=address)
	((<= r8 register r15)
	 value-class=object)
	((<= fr0 register fr7)
	 value-class=float)
	((<= mmx0 register mmx7)
	 (error "MMX media registers not allocated:" register))
	((<= xmm0 register xmm15)
	 (error "XMM media registers not allocated:" register))
	(else
	 (error "Invalid machine register:" register))))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
(define-integrable register-block/utility-arg4-offset 9) ; closure free
(define-integrable register-block/stack-guard-offset 11)

(define-integrable (fits-in-signed-byte? value)
  (<= #x-80 value #x7f))

(define-integrable (fits-in-unsigned-byte? value)
  (<= 0 value #xff))

(define-integrable (fits-in-signed-word? value)
  (<= #x-8000 value #x7fff))

(define-integrable (fits-in-unsigned-word? value)
  (<= 0 value #xffff))

(define-integrable (fits-in-signed-long? value)
  (<= #x-80000000 value #x7fffffff))

(define-integrable (fits-in-unsigned-long? value)
  (<= 0 value #xffffffff))

(define-integrable (fits-in-signed-quad? value)
  (<= #x-8000000000000000 value #x7fffffffffffffff))

(define-integrable (fits-in-unsigned-quad? value)
  (<= 0 value #xffffffffffffffff))

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register rax))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register rax))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register rax))

(define (interpreter-register:lookup)
  (rtl:make-machine-register rax))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register rax))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register rax))

(define-integrable (interpreter-block-register offset-value)
  (rtl:make-offset (interpreter-regs-pointer)
		   (rtl:make-machine-constant offset-value)))

(define-integrable (interpreter-block-register? expression offset-value)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= (rtl:machine-constant-value offset)
		 offset-value)))))
  
(define-integrable (interpreter-value-register)
  (interpreter-block-register register-block/value-offset))

(define (interpreter-value-register? expression)
  (interpreter-block-register? expression register-block/value-offset))

(define (interpreter-environment-register)
  (interpreter-block-register register-block/environment-offset))

(define (interpreter-environment-register? expression)
  (interpreter-block-register? expression register-block/environment-offset))

(define (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free-pointer))

(define (interpreter-free-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:free-pointer)))

(define (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:regs-pointer))

(define (interpreter-regs-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:regs-pointer)))

(define (interpreter-stack-pointer)
  (rtl:make-machine-register regnum:stack-pointer))

(define (interpreter-stack-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:stack-pointer)))

(define (interpreter-dynamic-link)
  (interpreter-block-register register-block/dynamic-link-offset))

(define (interpreter-dynamic-link? expression)
  (interpreter-block-register? expression register-block/dynamic-link-offset))

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
    #|
    ((VALUE)
     (interpreter-value-register))
    |#
    ((FREE)
     (interpreter-free-pointer))
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
    (else
     false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP)
     register-block/memtop-offset)
    ((INT-MASK)
     register-block/int-mask-offset)
    ((STACK-GUARD)
     register-block/stack-guard-offset)
    ((VALUE)
     register-block/value-offset)
    ((ENVIRONMENT)
     register-block/environment-offset)
    ((DYNAMIC-LINK TEMPORARY)
     register-block/dynamic-link-offset)
    (else
     false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  ;; i486 clock count for instruction to construct/fetch into register.
  (let ((if-integer
	 (lambda (value)
	   value			; ignored
	   ;; Can this be done in fewer bytes for suitably small values?
	   1))				; MOV immediate
	(get-pc-cost
	 (+ 3				; CALL
	    4))				; POP
	(based-reference-cost
	 1)				; MOV r/m
	(address-offset-cost
	 1))				; LEA instruction

    (define (if-synthesized-constant type datum)
      (if-integer (make-non-pointer-literal type datum)))

    (case (rtl:expression-type expression)
      ((CONSTANT)
       (let ((value (rtl:constant-value expression)))
	 (if (non-pointer-object? value)
	     (if-synthesized-constant (object-type value)
				      (careful-object-datum value))
	     (+ get-pc-cost based-reference-cost))))
      ((MACHINE-CONSTANT)
       (if-integer (rtl:machine-constant-value expression)))
      ((ENTRY:PROCEDURE
	ENTRY:CONTINUATION)
       (+ get-pc-cost address-offset-cost))
      ((ASSIGNMENT-CACHE
	VARIABLE-CACHE)
       (+ get-pc-cost based-reference-cost))
      ((OFFSET-ADDRESS
	BYTE-OFFSET-ADDRESS
	FLOAT-OFFSET-ADDRESS)
       address-offset-cost)
      ((CONS-POINTER)
       (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	    (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	    (if-synthesized-constant
	     (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	     (rtl:machine-constant-value
	      (rtl:cons-pointer-datum expression)))))
      (else
       false))))

(define compiler:open-code-floating-point-arithmetic?
  false)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM
    &/
    FLOATING-VECTOR-CONS FLOATING-VECTOR-LENGTH
    FLOATING-VECTOR-REF FLOATING-VECTOR-SET! FLONUM-ABS
    FLONUM-ACOS FLONUM-ADD FLONUM-ASIN FLONUM-ATAN FLONUM-ATAN2
    FLONUM-CEILING FLONUM-COS FLONUM-DIVIDE FLONUM-EQUAL?
    FLONUM-EXP FLONUM-FLOOR FLONUM-GREATER? FLONUM-LESS?
    FLONUM-LOG FLONUM-MULTIPLY FLONUM-NEGATE FLONUM-NEGATIVE?
    FLONUM-POSITIVE? FLONUM-ROUND FLONUM-SIN FLONUM-SQRT
    FLONUM-SUBTRACT FLONUM-TAN FLONUM-TRUNCATE FLONUM-ZERO?
    FLONUM? GCD-FIXNUM STRING-ALLOCATE VECTOR-CONS))