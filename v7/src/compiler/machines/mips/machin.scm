#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/machin.scm,v 1.1 1990/05/07 04:15:24 jinx Exp $
$MC68020-Header: machin.scm,v 4.20 90/01/18 22:43:44 GMT cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;; Machine Model for MIPS

(declare (usual-integrations))

;;;; Architecture Parameters

(define-integrable endianness 'LITTLE)
(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-type-width 6)	;or 8

(define-integrable scheme-datum-width
  (- scheme-object-width scheme-type-width))

(define-integrable type-scale-factor
  (expt 2 (- 8 scheme-type-width)))

(define-integrable flonum-size 2)
(define-integrable float-alignment 64)

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
(define-integrable closure-block-first-offset 2)
(define-integrable execute-cache-size 2) ; Long words per UUO link slot

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

(define-integrable number-of-machine-registers 63)
(define-integrable number-of-temporary-registers 256)

;;; Fixed-use registers for Scheme compiled code.
(define-integrable regnum:return-value g2)
(define-integrable regnum:stack-pointer g3)
(define-integrable regnum:memtop g8)
(define-integrable regnum:free g9)
(define-integrable regnum:scheme-to-interface g10)
(define-integrable regnum:dynamic-link g11)
(define-integrable regnum:address-mask g20)
(define-integrable regnum:regs-pointer g21)
(define-integrable regnum:quad-bits g22)
(define-integrable regnum:interface-index g25)

;;; Fixed-use registers due to architecture or OS calling conventions.
(define-integrable regnum:zero g0)
(define-integrable regnum:assembler-temp g1)
(define-integrable regnum:C-return-value g2)
(define-integrable regnum:first-arg g4)
(define-integrable regnum:second-arg g5)
(define-integrable regnum:third-arg g6)
(define-integrable regnum:fourth-arg g7)
(define-integrable regnum:kernel-reserved-1 g26)
(define-integrable regnum:kernel-reserved-2 g27)
(define-integrable regnum:C-global-pointer g28)
(define-integrable regnum:C-stack-pointer g29)
(define-integrable regnum:linkage g31)

(define machine-register-value-class
  (let ((special-registers
	 `((,regnum:return-value        . ,value-class=object)
	   (,regnum:stack-pointer       . ,value-class=address)
	   (,regnum:memtop              . ,value-class=address)
	   (,regnum:free                . ,value-class=address)
	   (,regnum:scheme-to-interface . ,value-class=unboxed)
	   (,regnum:dynamic-link        . ,value-class=address)
	   (,regnum:address-mask        . ,value-class=immediate)
	   (,regnum:regs-pointer        . ,value-class=unboxed)
	   (,regnum:quad-bits           . ,value-class=immediate)
	   (,regnum:assembler-temp      . ,value-class=unboxed)
	   (,regnum:kernel-reserved-1   . ,value-class=unboxed)
	   (,regnum:kernel-reserved-2   . ,value-class=unboxed)
	   (,regnum:C-global-pointer    . ,value-class=unboxed)
	   (,regnum:C-stack-pointer     . ,value-class=unboxed)
	   (,regnum:linkage             . ,value-class=address))))
    (lambda (register)
      (let ((lookup (assv register special-registers)))
	(cond
	 ((not (null? lookup)) (cdr lookup))
	 ((<= g0 register g31) value-class=word)
	 ((<= fp0 register fp31) value-class=float)
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
       (= 3 (rtl:offset-number expression))))

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
  '(DIVIDE-FIXNUM GCD-FIXNUM FIXNUM-QUOTIENT FIXNUM-REMAINDER
    INTEGER-QUOTIENT INTEGER-REMAINDER &/
    FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN FLONUM-ACOS
    FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-TRUNCATE FLONUM-ROUND
    FLONUM-REMAINDER FLONUM-SQRT))