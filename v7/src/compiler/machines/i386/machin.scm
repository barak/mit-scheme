#| -*-Scheme-*-

$Id: machin.scm,v 1.20 2002/11/20 19:45:52 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Machine Model for the Intel 386, i486, and successors
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? false)
(define-integrable endianness 'LITTLE)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-type-width 6)	;or 8

;; NOTE: expt is not being constant-folded now.
;; For the time being, some of the parameters below are
;; pre-computed and marked with ***
;; There are similar parameters in lapgen.scm
;; Change them if any of the parameters above change.

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable float-width 64)
(define-integrable float-alignment 32)

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
  33554432)

(define-integrable signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable unsigned-fixnum/upper-limit
  (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)

;;;; Closure format

;; See microcode/cmpint-i386.h for a description of the layout.
;; This must return a word based offset.
;; On the i386, to save space, entries can be at 2 mod 4 addresses,
;; which makes it impossible if the closure object used for
;; referencing points to arbitrary entries.  Instead, all closure
;; entry points bump to the canonical entry point, which is always
;; longword aligned.

(define (closure-first-offset nentries entry)
  entry					; ignored
  (if (zero? nentries)
      1
      (quotient (+ (+ 3 1) (* 5 (- nentries 1))) 2)))

;; This is from the start of the complete closure object,
;; viewed as a vector, and including the header word.

(define (closure-object-first-offset nentries)
  (case nentries
    ((0) 1)
    ((1) 4)
    (else
     (quotient (+ 5 (* 5 nentries)) 2))))

;; Bump from one entry point to another.

(define (closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* 10 (- entry* entry)))

;; Bump to the canonical entry point.

(define (closure-environment-adjustment nentries entry)
  (declare (integrate-operator closure-entry-distance))
  (closure-entry-distance nentries entry 0))

;;;; Machine registers

(define eax 0)				; acumulator
(define ecx 1)				; counter register
(define edx 2)				; multiplication high-half target
(define ebx 3)				; distinguished useful register
(define esp 4)				; stack pointer
(define ebp 5)				; frame pointer
(define esi 6)				; string source pointer
(define edi 7)				; string destination pointer

;; Virtual floating point registers:
;; Floating point stack locations, allocated as if registers.
;; One left free to allow room to push and operate.

(define fr0 8)
(define fr1 9)
(define fr2 10)
(define fr3 11)
(define fr4 12)
(define fr5 13)
(define fr6 14)
(define fr7 15)

(define number-of-machine-registers 16)
(define number-of-temporary-registers 256)

(define-integrable regnum:stack-pointer esp)
(define-integrable regnum:datum-mask ebp)
(define-integrable regnum:regs-pointer esi)
(define-integrable regnum:free-pointer edi)

(define-integrable (machine-register-known-value register)
  register				; ignored
  false)

(define (machine-register-value-class register)
  (cond ((<= eax register ebx)
	 value-class=object)
	((= register regnum:datum-mask)
	 value-class=immediate)
	((or (= register regnum:stack-pointer)
	     (= register regnum:free-pointer)
	     (= register regnum:regs-pointer))
	 value-class=address)
	((<= fr0 register fr7)
	 value-class=float)
	(else
	 (error "illegal machine register" register))))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
(define-integrable register-block/utility-arg4-offset 9) ; closure free
(define-integrable register-block/stack-guard-offset 11)

(define-integrable (fits-in-signed-byte? value)
  (and (>= value -128) (< value 128)))

(define-integrable (fits-in-unsigned-byte? value)
  (and (>= value 0) (< value 128)))

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register eax))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register eax))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register eax))

(define (interpreter-register:lookup)
  (rtl:make-machine-register eax))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register eax))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register eax))

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
  true)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/
		  ;; The rewriting rules in rulrew.scm don't work.
		  ;; Treat as not available.
		  FLONUM-ASIN FLONUM-ACOS
		  ;; Disabled for now.  The F2XM1 instruction is
		  ;; broken on the 387 (or at least some of them).
		  FLONUM-EXP
		  VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS))