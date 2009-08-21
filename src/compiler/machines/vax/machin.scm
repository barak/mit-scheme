#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Machine Model for DEC Vax
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? true)
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

;; This must return a word based offset.
;; On the VAX, to save space, entries can be at 2 mod 4 addresses,
;; which makes it impossible if the closure object used for
;; referencing points to arbitrary entries.  Instead, all closure
;; entry points bump to the canonical entry point, which is always
;; longword aligned.
;; On other machines (word aligned), it may be easier to bump back
;; to each entry point, and the entry number `entry' would be part
;; of the computation.

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

(define-integrable r0 0)		; return value
(define-integrable r1 1)
(define-integrable r2 2)
(define-integrable r3 3)
(define-integrable r4 4)
(define-integrable r5 5)
(define-integrable r6 6)
(define-integrable r7 7)
(define-integrable r8 8)
(define-integrable r9 9)
(define-integrable r10 10)
(define-integrable r11 11)
(define-integrable r12 12)		; AP
(define-integrable r13 13)		; FP
(define-integrable r14 14)		; SP
(define-integrable r15 15) 		; PC, not really useable.

(define number-of-machine-registers 16)
(define number-of-temporary-registers 256)

(define-integrable regnum:return-value r9)
(define-integrable regnum:regs-pointer r10)
(define-integrable regnum:pointer-mask r11)
(define-integrable regnum:free-pointer r12)
(define-integrable regnum:dynamic-link r13)
(define-integrable regnum:stack-pointer r14)
(define-integrable (machine-register-known-value register) register false)

(define (machine-register-value-class register)
  (cond ((<= 0 register 9) value-class=object)
	((= 11 register) value-class=immediate)
	((<= 10 register 15) value-class=address)
	(else (error "illegal machine register" register))))

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register r0))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register r0))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register r0))

(define (interpreter-register:lookup)
  (rtl:make-machine-register r0))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register r0))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-value-register)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-value-register? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:return-value)))

(define (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer) 3))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (= 3 (rtl:offset-number expression))))

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
  (rtl:make-machine-register regnum:dynamic-link))

(define (interpreter-dynamic-link? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:dynamic-link)))

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
    ((DYNAMIC-LINK)
     (interpreter-dynamic-link))
    ((VALUE)
     (interpreter-value-register))
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
    ((MEMORY-TOP) 0)
    ((INT-MASK) 1)
    #| ((VALUE) 2) |#
    ((ENVIRONMENT) 3)
    ((TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  ;; Magic numbers
  ;; number of bytes for the instruction to construct/fetch into register.
  (let ((if-integer
	 (lambda (value)
	   (cond ((zero? value) 2)
		 ((<= -63 value 63)
		  3)
		 (else
		  7)))))
    (let ((if-synthesized-constant
	   (lambda (type datum)
	     (if-integer (make-non-pointer-literal type datum)))))
      (case (rtl:expression-type expression)
	((CONSTANT)
	 (let ((value (rtl:constant-value expression)))
	   (if (non-pointer-object? value)
	       (if-synthesized-constant (object-type value)
					(careful-object-datum value))
	       3)))
	((MACHINE-CONSTANT)
	 (if-integer (rtl:machine-constant-value expression)))
	((ENTRY:PROCEDURE
	  ENTRY:CONTINUATION
	  ASSIGNMENT-CACHE
	  VARIABLE-CACHE
	  OFFSET-ADDRESS
	  BYTE-OFFSET-ADDRESS)
	 4)				; assuming word offset
	((CONS-POINTER)
	 (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	      (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	      (if-synthesized-constant
	       (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	       (rtl:machine-constant-value
		(rtl:cons-pointer-datum expression)))))
	(else false)))))

;;; Floating-point open-coding not implemented for VAXen.

(define compiler:open-code-floating-point-arithmetic?
  false)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/
    VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS
    FLOATING-VECTOR-REF FLOATING-VECTOR-SET!))