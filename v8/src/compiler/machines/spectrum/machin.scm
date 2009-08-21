#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;; Machine Model for Spectrum
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define stack-use-pre/post-increment? true)
(define heap-use-pre/post-increment? true)
(define continuation-in-stack? false)
(define closure-in-stack? false)

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

(define-integrable max-type-code
  ;; (-1+ (expt 2 scheme-type-width))  ***
  63)

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

(define #|-integrable|# quad-mask-value
  (cond ((= scheme-type-width 5)  #b01000)
	((= scheme-type-width 6)  #b010000)
	((= scheme-type-width 8)  #b01000000)
	(else (error "machin.scm: weird type width:" scheme-type-width))))

(define #|-integrable|# untagged-entries?
  ;; This is true if the value we have chosen for the compiled-entry
  ;; type-code is equal to the bits in the type-code field of an
  ;; address.
  (= quad-mask-value (ucode-type compiled-entry)))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)
(define-integrable execute-cache-size 3) ; Long words per UUO link slot

;;;; Closures and multi-closures

;; On the 68k, to save space, entries can be at 2 mod 4 addresses,
;; which makes it impossible to use an arbitrary closure entry-point
;; to reference closed-over variables since the compiler only uses
;; long-word offsets.  Instead, all closure entry points are bumped
;; back to the first entry point, which is always long-word aligned.

;; On the HP-PA, and all other RISCs, all the entry points are
;; long-word aligned, so there is no need to bump back to the first
;; entry point.

(define-integrable closure-entry-size
  #|
     Long words in a single closure entry:
       GC offset word
       LDIL	L'target,26
       BLE	R'target(5,26)
       ADDI	-12,31,31
   |#
  4)

;; Given: the number of entry points in a closure, and a particular
;; entry point number, compute the distance from that entry point to
;; the first variable slot in the closure object (in long words).

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

;; Bump distance in bytes from one entry point to another.
;; Used for invocation purposes.

(define (closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* (* closure-entry-size 4) (- entry* entry)))

;; Bump distance in bytes from one entry point to the entry point used
;; for variable-reference purposes.
;; On a RISC, this is the entry point itself.

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

;; fp0 - fp3 are status registers.  The rest are real registers
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

;; The following registers are available only on the newer processors
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
(define-integrable regnum:return-value g2)
(define-integrable regnum:scheme-to-interface-ble g3)
(define-integrable regnum:regs-pointer g4)
(define-integrable regnum:quad-bitmask g5)
(define-integrable regnum:false-value g5) ; Yes: same as quad-bitmask
(define-integrable regnum:empty-list g18)
(define-integrable regnum:continuation g19)
(define-integrable regnum:memtop-pointer g20)
(define-integrable regnum:free-pointer g21)
(define-integrable regnum:stack-pointer g22)
(define-integrable regnum:closure g25)

;;; Fixed-use registers due to architecture or OS calling conventions.
(define-integrable regnum:zero g0)
(define-integrable regnum:addil-result g1)
(define-integrable regnum:C-global-pointer g27)
(define-integrable regnum:C-return-value g28)
(define-integrable regnum:C-stack-pointer g30)
(define-integrable regnum:ble-return g31)
(define-integrable regnum:fourth-arg g23)
(define-integrable regnum:third-arg g24)
(define-integrable regnum:second-arg g25)
(define-integrable regnum:first-arg g26)

(define (machine-register-value-class register)
  (cond ((or (= register 0)
	     (= register 26)
	     (= register 29)
	     (= register regnum:ble-return))
	 value-class=word)
	((or (= register regnum:addil-result)
	     (= register regnum:scheme-to-interface-ble))
	 value-class=unboxed)
	((or (= register regnum:continuation)
	     (= register regnum:closure))
	 (if untagged-entries?
	     value-class=object		; because it is untagged
	     value-class=address))
	(;; argument registers
	 (or (= register 2)
	     (<= 6 register 17)
	     (<= 23 register 24))
	 value-class=object)
	((or (= register regnum:false-value)
	     (= register regnum:empty-list))
	 value-class=object)
	((or (= register regnum:regs-pointer)
	     (= register regnum:memtop-pointer)
	     (= register regnum:free-pointer)
	     (= register regnum:stack-pointer)
	     (= register 27)
	     (= register 30))
	 value-class=address)
	((= register 28)
	 value-class=object)
	((<= 32 register 63)
	 value-class=float)
	(else
	 (error "illegal machine register" register))))

;;(define *rtlgen/argument-registers*
;;  ;; arbitrary small number for debugging stack arguments
;;  '#(2 6 7))

(define *rtlgen/argument-registers*
  ;; Leave 28, 29, and 31 as temporaries
  ;; For now, 25 and 26 cannot be used because closure patterns
  ;; use them to jump.
  '#(#| 0 1 |#
     2                                 #| 3 4 5 |#
     6 7 8 9 10 11 12 13 14 15 16 17   #| 18 19 20 21 22 |#
     23 24                             #| 25 26 27 28 29 30 31 |#
     ))

(define-integrable (machine-register-known-value register)
  register				;ignore
  false)

(define (machine-register-known-type register)
  (cond ((and (machine-register? register)
	      (value-class=address? (machine-register-value-class register)))
	 quad-mask-value)
	(else
	 #F)))

;;;; Interpreter Registers

(define-integrable (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free-pointer))

(define (interpreter-free-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:free-pointer)))

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

(define-integrable (interpreter-int-mask-register)
  (rtl:make-offset (interpreter-regs-pointer)
		   (rtl:make-machine-constant 1)))

(define-integrable (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer)
		   (rtl:make-machine-constant 3)))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= 3 (rtl:machine-constant-value offset))))))

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register g28))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register g28))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register g28))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register g28))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register g28))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register g28))


(define-integrable (interpreter-continuation-register)
  ;; defined only if not continuation-in-stack?
  ;; Needs to be a param in machin.scm
  ;; ***IMPORTANT: cannot be 31 because BLE clobbers
  ;; it when going to the interface***
  ;; It should be 2, like for C, but we can't do this
  ;; until the calling interface is changed.
  (rtl:make-machine-register regnum:continuation))

(define-integrable (interpreter-closure-register)
  ;; defined only if not closure-in-stack?
  (rtl:make-machine-register regnum:closure))

(define-integrable (interpreter-memtop-register)
  (rtl:make-machine-register regnum:memtop-pointer))

;;;; Parameters moved from RTLGEN

(define (rtlgen/interpreter-call/argument-home index)
  (case index
    ((1) `(REGISTER 25))
    ((2) `(REGISTER 24))
    (else
     (internal-error "Unexpected interpreter-call argument index" index))))

(define (machine/indexed-loads? type)
  type					; for all types
  #T)

(define (machine/indexed-stores? type)
  (eq? type 'FLOAT))

(define (machine/cont-adjustment)
  ;; Distance in bytes between a raw continuation
  ;; (as left behind by JSR) and the real continuation
  ;; (after descriptor)
  0)


;;;; RTL Registers, Constants, and Primitives

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
    ;((DYNAMIC-LINK)
    ; (interpreter-dynamic-link))
    ((VALUE)
     (interpreter-value-register))
    ((FREE)
     (interpreter-free-pointer))
    ((MEMORY-TOP)
     (rtl:make-machine-register regnum:memtop-pointer))
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
    ((INT-MASK) 1)
    ((ENVIRONMENT) 3)
    ((TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  ;; Magic numbers.
  ;; 0, #F and '() all live in registers.
  ;; Is there any reason that all these costs were originally >0 ?
  ;; Making 0 #F and '() all 0 cost prevents any spurious rtl cse.
  ;; *** THIS IS A BAD IDEA - it makes substitutions even though there might
  ;;     not be rules to handle it! (the way to fix this is better rules).

  ;; A real problem with the current cse algorithm is that the expression
  ;; costs are independent of context, and this is hard to fix.  The
  ;; constant `3' in a fixnum-plus is free becaus it fits in the 14
  ;; bit literal field.  However, the same `3' is more expensive in a
  ;; multiply instruction.

  (define (if-integer value)
    (cond ((zero? value) 1)
	  (else 2)))
  (define (if-synthesized-constant type datum)
    (if-integer (make-non-pointer-literal type datum)))
  (case (rtl:expression-type expression)
    ((CONSTANT)
     (let ((value (rtl:constant-value expression)))
       (cond ((eq? value #F)  1)
	     ((eq? value '()) 1)
	     ((non-pointer-object? value)
	      (if-synthesized-constant (object-type value)
				       (object-datum value)))
	     (else 3))))
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
    (else false)))

(define compiler:open-code-floating-point-arithmetic?
  true)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/ FLONUM-ROUND->EXACT
		  FLONUM-TRUNCATE->EXACT FLONUM-FLOOR->EXACT
		  FLONUM-CEILING->EXACT FLONUM-NORMALIZE
		  FLONUM-DENORMALIZE FLONUM-EXPT
		  ;; SET-INTERRUPT-ENABLES!
		  ))


;(define (target-object-type object)
;  ;; This should be fixed for cross-compilation
;  (if (and (fix:fixnum? object)
;	   (negative? object))
;      #x3F
;      (object-type object)))

(define (target-object-type object)
  (object-type object))
