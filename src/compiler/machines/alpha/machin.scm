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
;;; Machine Model for Alpha
;;; Package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? false)
(define-integrable endianness 'LITTLE)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 64)
(define-integrable scheme-type-width 8) ; or 6

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable type-scale-factor
  (expt 2 (- 8 scheme-type-width)))

(define-integrable float-width 64)
(define-integrable float-alignment 64)
;; Number of address units (bytes) in a floating point value
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

(define-integrable address-units-per-gc&format-word
  (quotient 32 addressing-granularity))

(define-integrable address-units-per-packed-char 1)

(define-integrable signed-fixnum/upper-limit (expt 2 (-1+ scheme-datum-width)))
(define-integrable signed-fixnum/lower-limit (- signed-fixnum/upper-limit))
(define-integrable unsigned-fixnum/upper-limit (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)
(define-integrable execute-cache-size 2) ; Long words per UUO link slot
(define-integrable closure-entry-size
  ;; Long words in a single closure entry:
  ;;   Padding / Format and GC offset word 
  ;;   SUBQ    / BR or JMP
  ;;   absolute target address
  3)

;; Given: the number of entry points in a closure, return: the
;; distance in objects from the gc header word of the closure
;; block to the location of the first free variable.

(define (closure-object-first-offset nentries)
  (case nentries
    ((0)
     ;; Vector header only
     1)
    (else
     ;; Manifest closure header, then entries.
     (+ 1 (* closure-entry-size nentries)))))

;; Given: the number of entry points in a closure, and a particular
;; entry point number. Return: the distance from that entry point to
;; the first variable slot in the closure (in words).

(define (closure-first-offset nentries entry)
  (if (zero? nentries)
      1					; Strange boundary case
      (- (* closure-entry-size (- nentries entry)) 1)))

;; Bump from one entry point to another -- distance in BYTES

(define (closure-entry-distance nentries entry entry*)
  nentries				; ignored
  (* (* closure-entry-size address-units-per-object)
     (- entry* entry)))

;; Bump to the canonical entry point.  Since every closure entry point
;; on the Alpha is aligned on an object boundary, there is no need to
;; canonicalize.

(define (closure-environment-adjustment nentries entry)
  nentries entry			; ignored
  0)

;;;; Machine Registers

(define-integrable r0 0)
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
(define-integrable r12 12)
(define-integrable r13 13)
(define-integrable r14 14)
(define-integrable r15 15)
(define-integrable r16 16)
(define-integrable r17 17)
(define-integrable r18 18)
(define-integrable r19 19)
(define-integrable r20 20)
(define-integrable r21 21)
(define-integrable r22 22)
(define-integrable r23 23)
(define-integrable r24 24)
(define-integrable r25 25)
(define-integrable r26 26)
(define-integrable r27 27)
(define-integrable r28 28)
(define-integrable r29 29)
(define-integrable r30 30)
(define-integrable r31 31)

;; Floating point general registers --  the odd numbered ones are
;; only used when transferring to/from the CPU
(define-integrable f0 32)
(define-integrable f1 33)
(define-integrable f2 34)
(define-integrable f3 35)
(define-integrable f4 36)
(define-integrable f5 37)
(define-integrable f6 38)
(define-integrable f7 39)
(define-integrable f8 40)
(define-integrable f9 41)
(define-integrable f10 42)
(define-integrable f11 43)
(define-integrable f12 44)
(define-integrable f13 45)
(define-integrable f14 46)
(define-integrable f15 47)
(define-integrable f16 48)
(define-integrable f17 49)
(define-integrable f18 50)
(define-integrable f19 51)
(define-integrable f20 52)
(define-integrable f21 53)
(define-integrable f22 54)
(define-integrable f23 55)
(define-integrable f24 56)
(define-integrable f25 57)
(define-integrable f26 58)
(define-integrable f27 59)
(define-integrable f28 60)
(define-integrable f29 61)
(define-integrable f30 62)
(define-integrable f31 63)

(define-integrable number-of-machine-registers 64)
(define-integrable number-of-temporary-registers 256)

; Number  .dis   C                 Scheme
; ======  ====   =======           ======
; 0       v0     Return Value      Return Value
; 1       t0     caller saves      <free, but utility index (not shifted)>
; 2       t1     caller saves      Stack-Pointer
; 3       t2     caller saves      MemTop
; 4       t3     caller saves      Free
; 5       t4     caller saves      Dynamic Link
; 6       t5     caller saves      <free>
; 7       t6     caller saves      <free>
; 8       t7     caller saves      <free>
; 9       s0     callee saves      Regs-Pointer                           
; 10      s1     callee saves      Scheme-To-Interface                    
; 11      s2     callee saves      Closure Hook (jump ind. for full addresse)
; 12      s3     callee saves      Scheme-To-Interface-JSR                
; 13      s4     callee saves      Compiled-Entry-Type-Bits               
; 14      s5     callee saves      Closure-Free                           
; 15      fp?    frame base        <free>
; 16      a0     argument 1        <free, but for utilities>
; 17      a1     argument 2        <free, but for utilities>
; 18      a2     argument 3        <free, but for utilities>
; 19      a3     argument 4        <free, but for utilities>
; 20      a4     argument 5        <free, but for utilities>
; 21      a5     argument 6        <free>
; 22      t8     caller saves      <free>
; 23      t9     caller saves      <free>
; 24      t10    caller saves      <free>
; 25      t11    caller saves      <free>
; 26      ra     return address    <free, but used for closure linkage>   
; 27      t12    proc. descript.   <free>                                 
; 28      at?    volatile scratch  Assembler Temporary (tensioning)       
; 29      gp     global pointer    <free>
; 30      sp     stack pointer     C Stack Pointer (do not use!)
; 31      zero   Z E R O           Z E R O

;;; Fixed-use registers due to architecture or OS calling conventions.
;; Callee saves: r9-r15, r30 (stack pointer), f2-9 all others are caller saves
(define-integrable regnum:C-return-value r0)
(define-integrable regnum:C-frame-pointer r15)
(define-integrable regnum:first-C-arg r16)
(define-integrable regnum:second-C-arg r17)
(define-integrable regnum:third-C-arg r18)
(define-integrable regnum:fourth-C-arg r19)
(define-integrable regnum:fifth-C-arg r20)
(define-integrable regnum:sixth-C-arg r21)
(define-integrable regnum:linkage r26)
(define-integrable regnum:C-procedure-descriptor r27)
(define-integrable regnum:volatile-scratch r28)
(define-integrable regnum:C-global-pointer r29)
(define-integrable regnum:C-stack-pointer r30)
(define-integrable regnum:zero r31)

(define-integrable regnum:fp-return-1 f0)
(define-integrable regnum:fp-return-2 f1)
(define-integrable regnum:first-fp-arg f16)
(define-integrable regnum:second-fp-arg f17)
(define-integrable regnum:third-fp-arg f18)
(define-integrable regnum:fourth-fp-arg f19)
(define-integrable regnum:fifth-fp-arg f20)
(define-integrable regnum:sixth-fp-arg f21)
(define-integrable regnum:fp-zero f31)

;;; Fixed-use registers for Scheme compiled code.
(define-integrable regnum:return-value regnum:C-return-value)	  ; 0
(define-integrable regnum:interface-index r1)			  ; 1
(define-integrable regnum:stack-pointer r2)                       ; 2
(define-integrable regnum:memtop r3)				  ; 3
(define-integrable regnum:free r4)				  ; 4
(define-integrable regnum:dynamic-link r5)			  ; 5
                                                                  ; (6, 7, 8)
(define-integrable regnum:regs-pointer r9)			  ; 9
(define-integrable regnum:scheme-to-interface r10)		  ; 10
(define-integrable regnum:closure-hook r11)			  ; 11
(define-integrable regnum:scheme-to-interface-jsr r12)		  ; 12
(define-integrable regnum:compiled-entry-type-bits r13)           ; 13
(define-integrable regnum:closure-free r14) 			  ; 14
								  ; (15, 16)
;;;;;;; Note: regnum:first-C-arg is where the address for structure
;;;;;;; return values is passed.  Since all of the Scheme utilities
;;;;;;; return structure values, we "hide" this register to correspond
;;;;;;; to the C view of the argument number rather than the assembly
;;;;;;; language view.
(define-integrable regnum:first-arg regnum:second-C-arg)	  ; 17
(define-integrable regnum:second-arg regnum:third-C-arg)	  ; 18
(define-integrable regnum:third-arg regnum:fourth-C-arg)	  ; 19
(define-integrable regnum:fourth-arg regnum:fifth-C-arg)	  ; 20
								  ; (21, 22, 23, 24, 25)
(define-integrable regnum:closure-linkage regnum:linkage)	  ; 26
								  ; (27)
(define-integrable regnum:assembler-temp regnum:volatile-scratch) ; 28
(define-integrable regnum:came-from regnum:volatile-scratch)      ; 28
								  ; (29)

(define machine-register-value-class
  (let ((special-registers
	 `((,regnum:return-value            . ,value-class=object)
	   (,regnum:regs-pointer            . ,value-class=unboxed)
	   (,regnum:scheme-to-interface     . ,value-class=unboxed)
	   (,regnum:closure-hook            . ,value-class=unboxed)
	   (,regnum:scheme-to-interface-jsr . ,value-class=unboxed)
	   (,regnum:dynamic-link            . ,value-class=address)
	   (,regnum:free                    . ,value-class=address)
	   (,regnum:memtop                  . ,value-class=address)
	   (,regnum:assembler-temp          . ,value-class=unboxed)
	   (,regnum:stack-pointer           . ,value-class=address)
	   (,regnum:c-stack-pointer         . ,value-class=unboxed))))
    (lambda (register)
      (let ((lookup (assv register special-registers)))
	(cond
	 ((not (null? lookup)) (cdr lookup))
	 ((<= r0 register r31) value-class=word)
	 ((<= f0 register f31) value-class=float)
	 (else (error "illegal machine register" register)))))))

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
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= 3 (rtl:machine-constant-value offset))))))

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register regnum:C-return-value))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register regnum:C-return-value))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register regnum:C-return-value))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register regnum:C-return-value))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register regnum:C-return-value))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register regnum:C-return-value))

;;;; RTL Registers, Constants, and Primitives

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
    ((MEMORY-TOP)
     (rtl:make-machine-register regnum:memtop))
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
  ;; Magic numbers.  Cycles needed to generate value in specified
  ;; register.
  ;; Note: the 6 here is really two instructions (one to calculate the
  ;; PC-relative address, the other to load from memory) that require
  ;; 6 cycles worst case without cache miss.
  (let ((if-integer
	 (lambda (value)
	   (if (or (zero? value)
		   (fits-in-16-bits-signed? value)
		   (top-16-of-32-bits-only? value))
	       1
	       6))))
    (let ((if-synthesized-constant
	   (lambda (type datum)
	     (if-integer (make-non-pointer-literal type datum)))))
      (case (rtl:expression-type expression)
	((CONSTANT)
	 (let ((value (rtl:constant-value expression)))
	   (if (non-pointer-object? value)
	       (if-synthesized-constant (object-type value)
					(object-datum value))
	       6)))
	((MACHINE-CONSTANT)
	 (if-integer (rtl:machine-constant-value expression)))
	((ENTRY:PROCEDURE ENTRY:CONTINUATION
	  ASSIGNMENT-CACHE VARIABLE-CACHE
	  OFFSET-ADDRESS BYTE-OFFSET-ADDRESS FLOAT-OFFSET-ADDRESS)
	 6)
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

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM
    ; FIXNUM-QUOTIENT FIXNUM-REMAINDER
    INTEGER-QUOTIENT INTEGER-REMAINDER &/
    FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN FLONUM-ACOS FLONUM-ATAN2
    FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-REMAINDER FLONUM-SQRT
    FLONUM-TRUNCATE FLONUM-ROUND FLONUM-CEILING FLONUM-FLOOR
    VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS
    FLOATING-VECTOR-REF FLOATING-VECTOR-SET!))
