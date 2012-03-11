#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;; 128-bit media registers.

(define xmm0 16)
(define xmm1 17)
(define xmm2 18)
(define xmm3 19)
(define xmm4 20)
(define xmm5 21)
(define xmm6 22)
(define xmm7 23)
(define xmm8 24)
(define xmm9 25)
(define xmm10 26)
(define xmm11 27)
(define xmm12 28)
(define xmm13 29)
(define xmm14 30)
(define xmm15 31)

(define number-of-machine-registers 32)
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
	((<= xmm0 register xmm15)
	 value-class=float)
	(else
	 (error "Invalid machine register:" register))))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
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
  ;; Counts derived from the AMD64 Software Optimization Guide, Rev
  ;; 3.06, from September 2005.  Scaled by two because LEA costs 1/2!
  ;; This is pretty silly, but probably better than using i486 clock
  ;; counts.
  (let ((cost:lea 1)
	(cost:mov-mem 6)
	(cost:mov-imm 2)
	(cost:or 2))
    (case (rtl:expression-type expression)
      ((CONSTANT)
       (let ((value (rtl:constant-value expression)))
	 (if (non-pointer-object? value)
	     cost:mov-imm
	     cost:mov-mem)))
      ((MACHINE-CONSTANT)
       cost:mov-imm)
      ((ENTRY:PROCEDURE ENTRY:CONTINUATION)
       (+ cost:mov-imm cost:lea cost:or))
      ((OFFSET-ADDRESS BYTE-OFFSET-ADDRESS FLOAT-OFFSET-ADDRESS)
       (receive (offset-selector scale)
	   (case (rtl:expression-type expression)
	     ((OFFSET-ADDRESS)
	      (values rtl:offset-address-offset address-units-per-object))
	     ((BYTE-OFFSET-ADDRESS)
	      (values rtl:byte-offset-address-offset 1))
	     ((FLOAT-OFFSET-ADDRESS)
	      (values rtl:float-offset-address-offset
		      address-units-per-float)))
	 (let ((offset (offset-selector expression)))
	   (if (and (rtl:machine-constant? offset)
		    (not
		     (fits-in-signed-long?
		      (* scale (rtl:machine-constant-value offset)))))
	       (+ cost:mov-imm cost:lea)
	       cost:lea))))
      ((CONS-POINTER)
       (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	    (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	    cost:mov-imm))
      (else #f))))

(define compiler:open-code-floating-point-arithmetic?
  #t)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM
    &/
    FLOATING-VECTOR-CONS FLONUM-ACOS FLONUM-ASIN FLONUM-ATAN
    FLONUM-ATAN2 FLONUM-CEILING FLONUM-COS FLONUM-EXP FLONUM-EXPM1
    FLONUM-FLOOR FLONUM-LOG FLONUM-LOG1P FLONUM-ROUND FLONUM-SIN
    FLONUM-TAN FLONUM-TRUNCATE GCD-FIXNUM STRING-ALLOCATE VECTOR-CONS))