#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/machin.scm,v 4.6 1989/09/05 22:34:32 arthur Rel $
$MC68020-Header: machin.scm,v 4.14 89/01/18 09:58:56 GMT cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; Machine Model for DEC Vax

(declare (usual-integrations))

;;; Floating-point open-coding not implemented for VAXen.
(define compiler:open-code-floating-point-arithmetic? false)

;;; Size of words.  Some of the stuff in "assmd.scm" might want to
;;; come here.

(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable scheme-datum-width 24)
(define-integrable scheme-type-width 8)

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
(define number-of-machine-registers 16)
;; Each is a quadword long
(define number-of-temporary-registers 256)

(define-integrable regnum:dynamic-link r10)
(define-integrable regnum:free-pointer r12)
(define-integrable regnum:regs-pointer r13)
(define-integrable regnum:stack-pointer r14)

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list r0 r1 r2 r3 r4 r5 r6 r7 r8 r9))

(define initial-non-object-registers
  (list r10 r11 r12 r13 r14 r15))

(define-integrable (register-type register)
  ;; This may have to be changed when floating support is added.
  'GENERAL)

(define register-reference
  (let ((references (make-vector 16)))
    (let loop ((i 0))
      (if (< i 16)
	  (begin
	    (vector-set! references i (INST-EA (R ,i)))
	    (loop (1+ i)))))
    (lambda (register)
      (vector-ref references register))))

(define mask-reference (INST-EA (R 11)))

;; These must agree with cmpvax.m4

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register r0))

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