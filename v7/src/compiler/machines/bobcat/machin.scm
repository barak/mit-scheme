#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/machin.scm,v 4.15 1989/07/25 12:39:50 arthur Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Machine Model for 68020

(declare (usual-integrations))
;;; Size of words.  Some of the stuff in "assmd.scm" might want to
;;; come here.

(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-datum-width 24)
(define-integrable scheme-type-width 8)
(define-integrable flonum-size 2)
(define-integrable float-alignment 32)

;; It is currently required that both packed characters and objects be
;; integrable numbers of address units.  Furthermore, the number of
;; address units per object must be an integral multiple of the number
;; of address units per character.  This will cause problems on a
;; machine that is word addressed, in which case we will have to
;; rethink the character addressing strategy.
(define-integrable address-units-per-object 4)
(define-integrable address-units-per-packed-char 1)

(let-syntax ((fold
	      (macro (expression)
		(eval expression system-global-environment))))
  (define-integrable unsigned-fixnum/upper-limit (fold (expt 2 24)))
  (define-integrable signed-fixnum/upper-limit (fold (expt 2 23)))
  (define-integrable signed-fixnum/lower-limit (fold (- (expt 2 23)))))

(define-integrable (stack->memory-offset offset)
  offset)

(define ic-block-first-parameter-offset
  2)

(define closure-block-first-offset
  2)

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((DYNAMIC-LINK) (interpreter-dynamic-link))
    ((INTERPRETER-CALL-RESULT:ACCESS) (interpreter-register:access))
    ((INTERPRETER-CALL-RESULT:CACHE-REFERENCE)
     (interpreter-register:cache-reference))
    ((INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?)
     (interpreter-register:cache-unassigned?))
    ((INTERPRETER-CALL-RESULT:LOOKUP) (interpreter-register:lookup))
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?) (interpreter-register:unassigned?))
    ((INTERPRETER-CALL-RESULT:UNBOUND?) (interpreter-register:unbound?))
    (else false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP) 0)
    ((STACK-GUARD) 1)
    ((VALUE) 2)
    ((ENVIRONMENT) 3)
    ((TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost constant)
  ;; Magic numbers.  Ask RMS where they came from.
  (if (and (object-type? 0 constant)
	   (zero? (object-datum constant)))
      0
      3))

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
(define number-of-machine-registers 24)
(define number-of-temporary-registers 50)

(define-integrable regnum:dynamic-link a4)
(define-integrable regnum:free-pointer a5)
(define-integrable regnum:regs-pointer a6)
(define-integrable regnum:stack-pointer a7)

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list d0 d1 d2 d3 d4 d5 d6
	a0 a1 a2 a3
	fp0 fp1 fp2 fp3 fp4 fp5 fp6 fp7))

(define initial-non-object-registers
  (list d7 a4 a5 a6 a7))

(define (float-register? register)
  (if (machine-register? register)
      (eq? (register-type register) 'FLOAT)
      (error "FLOAT-REGISTER? valid only for machine registers" register)))

(define (word-register? register)
  (if (machine-register? register)
      (memq (register-type register)
	    '(DATA ADDRESS))))

(define (register-types-compatible? type1 type2)  (eq? (eq? type1 'FLOAT) (eq? type2 'FLOAT)))

(define register-type
  (let ((types (make-vector number-of-machine-registers)))
    (let loop ((i 0) (j 8) (k 16))
      (if (< i 8)
	  (begin (vector-set! types i 'DATA)
		 (vector-set! types j 'ADDRESS)
		 (vector-set! types k 'FLOAT)
		 (loop (1+ i) (1+ j) (1+ k)))))
    (lambda (register)
      (vector-ref types register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (let loop ((i 0) (j 8))
      (if (< i 8)
	  (begin (vector-set! references i (INST-EA (D ,i)))
		 (vector-set! references j (INST-EA (A ,i)))
		 (loop (1+ i) (1+ j)))))
    (let loop ((i 16) (names '(FP0 FP1 FP2 FP3 FP4 FP5 FP6 FP7)))
      (if (not (null? names))
	  (begin (vector-set! references i (car names))
		 (loop (1+ i) (cdr names)))))
    (lambda (register)
      (vector-ref references register))))

(define mask-reference (INST-EA (D 7)))

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-value-register)
  (rtl:make-offset (interpreter-regs-pointer) 2))

(define (interpreter-value-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-register expression))
       (= 2 (rtl:offset-number expression))))

(define-integrable (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer) 3))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-register expression))
       (= 3 (rtl:offset-number expression))))

(define-integrable (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free-pointer))

(define-integrable (interpreter-free-pointer? register)
  (= (rtl:register-number register) regnum:free-pointer))

(define-integrable (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:regs-pointer))

(define-integrable (interpreter-regs-pointer? register)
  (= (rtl:register-number register) regnum:regs-pointer))

(define-integrable (interpreter-stack-pointer)
  (rtl:make-machine-register regnum:stack-pointer))

(define-integrable (interpreter-stack-pointer? register)
  (= (rtl:register-number register) regnum:stack-pointer))

(define-integrable (interpreter-dynamic-link)
  (rtl:make-machine-register regnum:dynamic-link))

(define-integrable (interpreter-dynamic-link? register)
  (= (rtl:register-number register) regnum:dynamic-link))