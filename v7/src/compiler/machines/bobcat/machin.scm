#| -*-Scheme-*-

$Id: machin.scm,v 4.34 2002/02/22 03:36:54 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Machine Model for the Motorola MC68K family
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? true)
(define-integrable endianness 'BIG)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-type-width 6)	;or 8

;; NOTE: expt is not being constant-folded now.
;; For the time being, some of the parameters below are
;; pre-computed and marked with ***
;; There are similar parameters in lapgen.scm
;; Change them if any of the parameters above do.

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable type-scale-factor
  ;; (expt 2 (- 8 scheme-type-width)) ***
  4)

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

;; There are two versions of the closure format.
;; The MC68040 format can be used by all processors in the family,
;; irrelevant of cache operation, but is slower.
;; The MC68020 format can be used by all processors except the MC68040
;; unless its data cache is operating in write-through mode (instead
;; of store-in or copyback).
;; MC68020-format closure entry points are not long-word aligned, thus
;; they are canonicalized to the first entry point at call time.
;; MC68040-format closure entry points are long-word aligned, and
;; there is no canonicalization.

;; When using the MC68020 format, to save space, entries can be at 2
;; mod 4 addresses, thus if we used the entry points for environments,
;; the requirement that all environment pointers be long-word aligned
;; would be violated.  Instead, all closure entry points are bumped to
;; the canonical entry point, which is always long-word aligned.

#|
   An MC68020-format closure entry:
   	DC.W	<format word>, <GC offset word>
	JSR	#target

   Entries are not padded to long-word length.  The JSR-absolute
   instruction is 6 bytes long, so the total size per entry is
   10 bytes.
|#

(define (MC68020/closure-first-offset nentries entry)
  entry					; ignored
  (if (zero? nentries)
      1
      (quotient (+ (+ 3 1) (* 5 (- nentries 1))) 2)))

(define (MC68020/closure-object-first-offset nentries)
  (case nentries
    ((0) 1)
    ((1) 4)
    (else
     (quotient (+ 5 (* 5 nentries)) 2))))

(define (MC68020/closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* 10 (- entry* entry)))

;; When using the MC68020 format, bump to the canonical entry point.

(define (MC68020/closure-environment-adjustment nentries entry)
  (declare (integrate-operator MC68020/closure-entry-distance))
  (MC68020/closure-entry-distance nentries entry 0))

(define-integrable MC68040/closure-entry-size
  #|
     Long-words in a single closure entry:
       DC.W	<format word>, <GC offset word>
       JSR	closure_hook(a6)
       DC.L	target
  |#
   3)

(define (MC68040/closure-first-offset nentries entry)
  entry					; ignored
  (if (zero? nentries)
      1
      (- (* MC68040/closure-entry-size (- nentries entry)) 1)))

(define (MC68040/closure-object-first-offset nentries)
  (case nentries
    ((0)
     ;; Vector header only
     1)
    ((1)
     ;; Manifest closure header followed by single entry point.
     (1+ MC68040/closure-entry-size))
    (else
     ;; Manifest closure header, number of entries, then entries.
     (+ 1 1 (* MC68040/closure-entry-size nentries)))))

(define (MC68040/closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* (* MC68040/closure-entry-size 4) (- entry* entry)))

;; With the 68040 layout, this is the entry point itself, no bumping.

(define (MC68040/closure-environment-adjustment nentries entry)
  nentries entry			; ignored
  0)

;;;; Closure choices

(define-integrable MC68K/closure-format 'MC68040) ; or MC68020

(let-syntax
    ((define/format-dependent
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (cadr form)))
	    `(DEFINE ,name
	       (CASE MC68K/CLOSURE-FORMAT
		 ((MC68020)
		  ,(close-syntax (symbol-append 'MC68020/ name) environment))
		 ((MC68040)
		  ,(close-syntax (symbol-append 'MC68040/ name) environment))
		 (ELSE
		  (ERROR "Unknown closure format" CLOSURE-FORMAT)))))))))

;; Given: the number of entry points in a closure, and a particular
;; entry point number, compute the distance from that entry point to
;; the first variable slot in the closure object (in long words).

(define/format-dependent closure-first-offset)

;; Like the above, but from the start of the complete closure object,
;; viewed as a vector, and including the header word.

(define/format-dependent closure-object-first-offset)

;; Bump distance in bytes from one entry point to another.
;; Used for invocation purposes.

(define/format-dependent closure-entry-distance)

;; Bump distance in bytes from one entry point to the entry point used
;; for variable-reference purposes.

(define/format-dependent closure-environment-adjustment)
)

(define-integrable d0 0)
(define-integrable d1 1)
(define-integrable d2 2)
(define-integrable d3 3)
(define-integrable d4 4)
(define-integrable d5 5)
(define-integrable d6 6)
(define-integrable d7 7)
(define-integrable a0 8)
(define-integrable a1 9)
(define-integrable a2 10)
(define-integrable a3 11)
(define-integrable a4 12)
(define-integrable a5 13)
(define-integrable a6 14)
(define-integrable a7 15)
(define-integrable fp0 16)
(define-integrable fp1 17)
(define-integrable fp2 18)
(define-integrable fp3 19)
(define-integrable fp4 20)
(define-integrable fp5 21)
(define-integrable fp6 22)
(define-integrable fp7 23)

(define-integrable number-of-machine-registers 24)
(define-integrable number-of-temporary-registers 256)

(define-integrable regnum:return-value d6)
(define-integrable regnum:pointer-mask d7)
(define-integrable regnum:dynamic-link a4)
(define-integrable regnum:free-pointer a5)
(define-integrable regnum:regs-pointer a6)
(define-integrable regnum:stack-pointer a7)
(define-integrable (machine-register-known-value register) register false)

(define (machine-register-value-class register)
  (cond ((or (<= 0 register 6) (<= 8 register 11)) value-class=object)
	((= 7 register) value-class=immediate)
	((<= 12 register 15) value-class=address)
	((<= 16 register 23) value-class=float)
	(else (error "illegal machine register" register))))

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register d0))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register d0))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register d0))

(define (interpreter-register:lookup)
  (rtl:make-machine-register d0))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register d0))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register d0))

(define (interpreter-value-register)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-value-register? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:return-value)))

(define (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer)
		   (rtl:make-machine-constant 3)))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= 3 (rtl:machine-constant-value offset))))))

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
    (else false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP) 0)
    ((INT-MASK) 1)
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
	   (if (and (not (negative? value)) (< value #x3F)) 2 3))))
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
	  BYTE-OFFSET-ADDRESS
	  FLOAT-OFFSET-ADDRESS)
	 3)
	((CONS-POINTER)
	 (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	      (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	      (if-synthesized-constant
	       (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	       (rtl:machine-constant-value
		(rtl:cons-pointer-datum expression)))))
	(else false)))))

(define compiler:open-code-floating-point-arithmetic?
  true)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/
    VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS
    FLONUM-CEILING FLONUM-FLOOR FLONUM-ATAN2))