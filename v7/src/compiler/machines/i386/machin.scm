#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/machin.scm,v 1.1 1992/01/21 00:08:40 jinx Exp $
$MC68020-Header: /scheme/src/compiler/machines/bobcat/RCS/machin.scm,v 4.26 1991/10/25 06:49:34 cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Machine Model for the Intel 386, i486, and successors
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

;; **** Does this apply to the stack as well? ****
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

(define-integrable flonum-size 2)
(define-integrable float-alignment 32)

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

(define eax 0)				; acumulator
(define ecx 1)				; counter register
(define edx 2)				; multiplication high-half target
(define ebx 3)				; distinguished useful register
(define esp 4)				; stack pointer
(define ebp 5)				; frame pointer
(define esi 6)				; string source pointer
(define edi 7)				; string destination pointer

(define number-of-machine-registers 8)
(define number-of-temporary-registers 256)

(define-integrable regnum:return-value ebx)
(define-integrable regnum:regs-pointer esi)
(define-integrable regnum:pointer-mask ebp)
(define-integrable regnum:free-pointer edi)
(define-integrable regnum:stack-pointer esp)

(define-integrable (machine-register-known-value register)
  register				; ignored
  false)

;; **** Need to do something about float "registers" ****

(define (machine-register-value-class register)
  (cond ((<= 0 register 2) value-class=object)
	((= register ebp) value-class=immediate)
	((<= 0 register 7) value-class=address)
	(false value-class=float)
	(else
	 (error "illegal machine register" register))))

(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
;; ^ Could also use the closure registers, not needed for this port.
;; Need to check whether they are spuriously initialized or reset.

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-register:lookup)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register regnum:return-value))

(define-integrable (interpreter-value-register)
  (rtl:make-machine-register regnum:return-value))

(define (interpreter-value-register? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:return-value)))

(define (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer)
		   register-block/environment-offset))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (=
	(rtl:offset-number expression) register-block/environment-offset)))

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
  (rtl:make-offset (interpreter-regs-pointer)
		   register-block/dynamic-link-offset))

(define (interpreter-dynamic-link? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (= (rtl:offset-number expression)
	  register-block/dynamic-link-offset)))

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
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
    (else
     false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP) 0)
    ((STACK-GUARD) 1)
    #| ((VALUE) 2) |#
    ((ENVIRONMENT) 3)
    ((DYNAMIC-LINK TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  ;; i486 clock count for instruction to construct/fetch into register.
  (let ((if-integer
	 (lambda (value)
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
	BYTE-OFFSET-ADDRESS)
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

;;; Floating-point open-coding not implemented for i386, for now.

;;; **** Fix this ****

(define compiler:open-code-floating-point-arithmetic?
  false)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/))