#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/machin.scm,v 4.1 1988/01/07 21:14:55 bal Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(define-integrable (stack->memory-offset offset)
  offset)

(define ic-block-first-parameter-offset
  2)

(define (rtl:expression-cost expression)
  ;; Returns an estimate of the cost of evaluating the expression.
  ;; The number of cycles is processor dependent, and not published.
  ;; Thus the number of bytes is used as the cost.
  ;; In the following, temp, and temp+3 are assumed to qualify as byte
  ;; offsets.
  (case (rtl:expression-type expression)
    ((CONS-POINTER)
     ;; movl  free,temp(regs)	 = 4
     ;; movb  &type,3+temp(regs) = 4 (literal, rather than byte immediate)
     ;; movl  temp(regs),reg     = 4
     (+ 12
	(rtl:expression-cost (rtl:cons-pointer-type expression))
	(rtl:expression-cost (rtl:cons-pointer-datum expression))))
    ((CONSTANT)
     (let ((value (cadr expression)))
       (cond ((false? value) 2)		;clrl  reg
	     ((or (eq? value true)
		  (char? value)
		  (and (integer? value)
		       (<= -#x80000000 value #x7FFFFFFF)))
	      7)			;movl  #...,reg
	     (else 5))))		;movl  d(pc),reg (word offset)
    ;; mova  d(pc),reg          =  5 (word offset)
    ;; movl  reg,temp(regs)     =  4
    ;; movb  &type,3+temp(regs) =  4 (literal, rather than byte immediate)
    ;; movl  temp(regs),reg     =  4
    ((ENTRY:CONTINUATION ENTRY:PROCEDURE) 17)
    ((OBJECT->ADDRESS OBJECT->DATUM) 3)	;bicl2 rmask,reg
    ;; movl  reg,temp(regs)     =  4
    ;; movb  temp+3(regs),reg   =  4
    ((OBJECT->TYPE) 8)
    ((OFFSET) 4)			;movl  d(reg),reg (byte offset)
    ((OFFSET-ADDRESS) 4)		;mova  d(reg),reg (byte offset)
    ((POST-INCREMENT) 3)		;movl  (reg)+,reg
    ((PRE-INCREMENT) 3)			;movl  -(reg),reg
    ((REGISTER) 3)			;movl  reg,reg
    ((UNASSIGNED) 7)			;movl  #data,reg
    ((VARIABLE-CACHE) 5)		;movl  d(pc),reg (word offset)
    (else (error "Unknown expression type" expression))))

;;; Machine registers

(define-integrable interregnum:memory-top	0)
(define-integrable interregnum:stack-guard	1)
(define-integrable interregnum:value		2)
(define-integrable interregnum:environment	3)
(define-integrable interregnum:temporary	4)
(define-integrable interregnum:enclose		5)

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((FRAME-POINTER) (interpreter-frame-pointer))
    ((STACK-POINTER) (interpreter-stack-pointer))
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
    ((MEMORY-TOP) interregnum:memory-top)
    ((STACK-GUARD) interregnum:stack-guard)
    ((VALUE) interregnum:value)
    ((ENVIRONMENT) interregnum:environment)
    ((TEMPORARY) interregnum:temporary)
    ((INTERPRETER-CALL-RESULT:ENCLOSE) interregnum:enclose)
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
(define number-of-machine-registers 16)

(define-integrable (register-contains-address? register)
  (memv register '(10 12 13 14 15)))

(define initial-address-registers
  (list r10 r12 r13 r14 r15))

(define-integrable regnum:frame-pointer r10)
(define-integrable regnum:free-pointer r12)
(define-integrable regnum:regs-pointer r13)
(define-integrable regnum:stack-pointer r14)

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10))

(define-integrable (pseudo-register=? x y)
  (= (register-renumber x) (register-renumber y)))

;;; Interpreter registers



(define (register-type register)
  'GENERAL)

(define register-reference
  (let ((references (make-vector 16)))
    (let loop ((i 0))
      (if (< i 16)
	  (begin (vector-set! references i (INST-EA (R ,i)))
		 (loop (1+ i)))))
    (lambda (register)
      (vector-ref references register))))

(define mask-reference (INST-EA (R 11)))

(define-integrable (interpreter-register:access)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:cache-reference)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:enclose)
  (rtl:make-offset (interpreter-regs-pointer) interregnum:enclose))

(define-integrable (interpreter-register:lookup)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:unassigned?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-register:unbound?)
  (rtl:make-machine-register r0))

(define-integrable (interpreter-frame-pointer)
  (rtl:make-machine-register regnum:frame-pointer))

(define-integrable (interpreter-frame-pointer? register)
  (= (rtl:register-number register) regnum:frame-pointer))

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

;;;; Exports from machines/lapgen

(define lap:make-label-statement)
(define lap:make-unconditional-branch)
(define lap:make-entry-point)
