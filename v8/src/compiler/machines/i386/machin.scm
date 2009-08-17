#| -*-Scheme-*-

$Id: 32edca92fe25ef312d8374519c744d89fa29f821 $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

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

;;;; Machine Model for the Intel 386, i486, and successors
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define stack-use-pre/post-increment? true)
(define heap-use-pre/post-increment? false)
(define continuation-in-stack? true)
(define closure-in-stack? true)

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

; Pulled from Spectrum's 
(define-integrable max-type-code
  ;; (-1+ (expt 2 scheme-type-width))  ***
  63)

; Pulled from Spectrum's 
(define (machine/cont-adjustment)
  ;; Distance in bytes between a raw continuation
  ;; (as left behind by JSR) and the real continuation
  ;; (after descriptor)
  4)

(define #|-integrable|# untagged-fixnums?
  ;; true when fixnums have tags 000000... and 111111...
  (and (= 0 (ucode-type positive-fixnum))
       (= max-type-code (ucode-type negative-fixnum))))

(if (and (not untagged-fixnums?)
	 (not (= (ucode-type positive-fixnum) (ucode-type negative-fixnum))))
    (error "machin.scm: split fixnum type-codes must be 000... and 111..."))

(define #|-integrable|# signed-fixnum/upper-limit
  (if untagged-fixnums?
      ;; (expt 2 scheme-datum-width) ***
      67108864
      ;; (expt 2 (-1+ scheme-datum-width)) ***
      33554432))

(define-integrable signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define #|-integrable|# unsigned-fixnum/upper-limit
  (if untagged-fixnums?
      signed-fixnum/upper-limit
      (* 2 signed-fixnum/upper-limit)))

(define #|-integrable|# untagged-entries?
  ;; This is true if the value we have chosen for the compiled-entry
  ;; type-code is equal to the bits in the type-code field of an
  ;; address.
  false)

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
  (- (closure-entry-distance nentries entry 0) 5))

;;;; Machine registers

;; This gives us an extra scratch register
(define use-ebp-as-mask? #t)


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

    

(define-integrable regnum:regs-pointer esi)
(define-integrable regnum:free-pointer edi)
(define-integrable regnum:hook eax)
(define-integrable regnum:first-arg ecx)
(define-integrable regnum:second-arg edx)
(define-integrable regnum:third-arg ebx)


(define datum-mask-value)
(define regnum:datum-mask)

(if use-ebp-as-mask?
    (begin
      (set! regnum:datum-mask ebp)
      (set! datum-mask-value `(R ,ebp)))
    (set! datum-mask-value `(& ,(- (expt 2 scheme-datum-width) 1))))

(define-integrable (machine-register-known-value register)
  register				; ignored
  false)

(define machine-register-value-class)

(if use-ebp-as-mask?
    (set! machine-register-value-class
	  (lambda (register)
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
		   (error "illegal machine register" register)))))
    (set! machine-register-value-class
	  (lambda (register)
	    (cond ((or (<= eax register ebx)
		       (= ebp register))
		   value-class=object)
		  ((or (= register regnum:stack-pointer)
		       (= register regnum:free-pointer)
		       (= register regnum:regs-pointer))
		   value-class=address)
		  ((<= fr0 register fr7)
		   value-class=float)
		  (else
		   (error "illegal machine register" register))))))

(define *rtlgen/argument-registers*
  (vector edx ecx))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
(define-integrable register-block/utility-arg4-offset 9) ; closure free
(define-integrable register-block/stack-guard-offset 11)
(define-integrable register-block/empty-list 14)

(define (get-regblock-ea offs)
  `(@RO B ,regnum:regs-pointer ,(* 4 offs)))

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
				      (386-object-datum value))
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
		  FLONUM-ROUND->EXACT FLONUM-CEILING->EXACT
		  FLONUM-TRUNCATE->EXACT FLONUM-FLOOR->EXACT
		  FLONUM-NORMALIZE FLONUM-DENORMALIZE
		  FLONUM-EXPT
		  VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS))

;; This fits the normal calling convention, even though the real expectation
;; is that arg 2 will go to ebx, but code in i386.m4 fixes that.

(define (rtlgen/interpreter-call/argument-home index)
  (case index
    ((1) `(REGISTER ,edx))
    ((2) `(REGISTER ,ecx))
    (else
     (internal-error "Unexpected interpreter-call argument index" index))))

(define #|-integrable|# quad-mask-value
  (cond ((= scheme-type-width 5)  #b01000)
	((= scheme-type-width 6)  #b010000)
	((= scheme-type-width 8)  #b01000000)
	(else (error "machin.scm: weird type width:" scheme-type-width))))

(define (machine/indexed-loads? type)
  type					; for all types
  #T)

(define (machine/indexed-stores? type)
  type					; for all types
  #T)

(define (386-object-type d)
  (object-type d))

(define (386-object-datum d)
  (if (false? d)
      (- (careful-object-datum d) 16)
      (careful-object-datum d)))
