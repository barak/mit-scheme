;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Machine Model for Spectrum

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/machin.scm,v 1.40 1987/02/13 09:41:41 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define (rtl:message-receiver-size:closure) 1)
(define (rtl:message-receiver-size:stack) 1)
(define (rtl:message-receiver-size:subproblem) 1)

(define-integrable (stack->memory-offset offset)
  offset)

(define (rtl:expression-cost expression)
  ;; Returns an estimate of the cost of evaluating the expression.
  ;; For time being, disable this feature.
  1)

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((INTERPRETER-CALL-RESULT:ACCESS) (interpreter-register:access))
    ((INTERPRETER-CALL-RESULT:ENCLOSE) (interpreter-register:enclose))
    ((INTERPRETER-CALL-RESULT:LOOKUP) (interpreter-register:lookup))
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?) (interpreter-register:unassigned?))
    ((INTERPRETER-CALL-RESULT:UNBOUND?) (interpreter-register:unbound?))
    (else false)))

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY_TOP) 0)
    ((STACK_GUARD) 1)
    ((VALUE) 2)
    ((ENVIRONMENT) 3)
    ((TEMPORARY) 4)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

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

(define number-of-machine-registers 32)

(define-integrable (sort-machine-registers registers)
  registers)

(define (pseudo-register=? x y)
  (= (register-renumber x) (register-renumber y)))

(define available-machine-registers
  (list r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18
	r19 r20 r21 r22))

(define-integrable (register-contains-address? register)
  (memv register '(23 24 25 30)))

(define-integrable (register-type register)
  false)

(define-integrable (register-reference register)
  register)

(define-integrable regnum:frame-size r3)
(define-integrable regnum:call-argument-0 r4)
(define-integrable regnum:call-argument-1 r5)
(define-integrable regnum:call-argument-2 r6)
(define-integrable regnum:call-value r28)

(define-integrable regnum:memtop-pointer r23)
(define-integrable regnum:regs-pointer r24)
(define-integrable regnum:free-pointer r25)
(define-integrable regnum:code-object-base r26)
(define-integrable regnum:address-offset r27)
(define-integrable regnum:stack-pointer r30)

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register regnum:call-value))

(define-integrable (interpreter-register:enclose)
  (rtl:make-machine-register regnum:call-value))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register regnum:call-value))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register regnum:call-value))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register regnum:call-value))

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

(define (lap:make-label-statement label)
  `(LABEL ,label))

(define (lap:make-unconditional-branch label)
  `((BL (N) (- (- ,label *PC*) 8) 0)))

(define (lap:make-entry-point label block-start-label)
  `((ENTRY-POINT ,label)
    (WORD (- ,label ,block-start-label))
    (LABEL ,label)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
    (LABEL ,label)))