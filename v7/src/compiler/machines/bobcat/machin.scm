;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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

;;;; Machine Model for 68020

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/machin.scm,v 1.43 1987/02/10 22:14:37 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)
(define (rtl:message-receiver-size:closure) 1)
(define (rtl:message-receiver-size:stack) 1)
(define (rtl:message-receiver-size:subproblem) 2)

(define-integrable (stack->memory-offset offset)
  offset)

(define (rtl:expression-cost expression)
  ;; Returns an estimate of the cost of evaluating the expression.
  ;; For simplicity, we try to estimate the actual number of cycles
  ;; that a typical code sequence would produce.
  (case (rtl:expression-type expression)
    ((CONSTANT)
     (let ((value (cadr expression)))
       (cond ((false? value) 4)		;clr.l reg
	     ((or (eq? value true)
		  (char? value)
		  (and (integer? value)
		       (<= -#x80000000 value #x7FFFFFFF)))
	      12)			;move.l #...,reg
	     (else 16))))		;move.l d(pc),reg
    ((CONS-POINTER)
     ;; Best case = 12 cycles, worst =  44
     ;; move.l reg,d(reg) = 16
     ;; move.b reg,d(reg) = 12
     ;; move.l d(reg),reg = 16
     (+ 30
	(rtl:expression-cost (rtl:cons-pointer-type expression))
	(rtl:expression-cost (rtl:cons-pointer-datum expression))))
    ((OBJECT->ADDRESS OBJECT->DATUM) 6)	;and.l d7,reg
    ;; move.l reg,d(reg) = 16
    ;; move.b d(reg),reg = 12
    ((OBJECT->TYPE) 28)
    ((OFFSET) 16)			;move.l d(reg),reg
    ((OFFSET-ADDRESS) 8)		;lea d(an),reg
    ((POST-INCREMENT) 12)		;move.l (reg)+,reg
    ((PRE-INCREMENT) 14)		;move.l -(reg),reg
    ((REGISTER) 4)			;move.l reg,reg
    ((UNASSIGNED) 12)			;move.l #data,reg
    ;; lea d(pc),reg       =  8
    ;; move.l reg,d(reg)   = 16
    ;; move.b #type,d(reg) = 16
    ;; move.l d(reg),reg   = 16
    ((ENTRY:CONTINUATION ENTRY:PROCEDURE) 56)
    (else (error "Unknown expression type" expression))))

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((INTERPRETER-CALL-RESULT:ACCESS) (interpreter-register:access))
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
    ((INTERPRETER-CALL-RESULT:ENCLOSE) 5)
    (else false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

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

(define number-of-machine-registers 16)

(define-integrable (sort-machine-registers registers)
  registers)

(define (pseudo-register=? x y)
  (= (register-renumber x) (register-renumber y)))

(define available-machine-registers
  (list d0 d1 d2 d3 d4 d5 d6 a0 a1 a2 a3 a4))

(define-integrable (register-contains-address? register)
  (memv register '(13 14 15)))

(define register-type
  (let ((types (make-vector 16)))
    (let loop ((i 0) (j 8))
      (if (< i 8)
	  (begin (vector-set! types i 'DATA)
		 (vector-set! types j 'ADDRESS)
		 (loop (1+ i) (1+ j)))))
    (lambda (register)
      (vector-ref types register))))

(define register-reference
  (let ((references (make-vector 16)))
    (let loop ((i 0) (j 8))
      (if (< i 8)
	  (begin (vector-set! references i `(D ,i))
		 (vector-set! references j `(A ,i))
		 (loop (1+ i) (1+ j)))))    (lambda (register)
      (vector-ref references register))))

(define mask-reference
  '(D 7))

(define regnum:free-pointer a5)
(define regnum:regs-pointer a6)
(define regnum:stack-pointer a7)

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:enclose)
  (rtl:make-machine-register a0))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register d0))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register d0))

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
  `(BRA L (@PCR ,label)))

(define (lap:make-entry-point label block-start-label)
  `((ENTRY-POINT ,label)
    (DC W (- ,label ,block-start-label))
    (LABEL ,label)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
    (LABEL ,label)))