#| -*-Scheme-*-

$Id: rulflo.scm,v 1.1 1992/08/29 13:51:34 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; LAP Generation Rules: Flonum rules
;; Package: (compiler lap-syntaxer)
;; Syntax: lap-generator-syntax-table

(declare (usual-integrations))

(define fpr:zero (float-register->fpr regnum:fp-zero))

(define (flonum-source! register)
  (float-register->fpr (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (float-register->fpr (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (float-register->fpr (allocate-temporary-register! 'FLOAT)))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let* ((source (flonum-source! source))
	 (target (standard-target! target)))
    (LAP
     ,@(with-values
	 (lambda ()
	   (immediate->register
	    (make-non-pointer-literal (ucode-type manifest-nm-vector)
				      flonum-size)))
	 (lambda (prefix alias)
	   (LAP ,@prefix
		(STQ ,alias (OFFSET 0 ,regnum:free)))))
     ,@(deposit-type-address (ucode-type flonum) regnum:free target)
     (STT ,source (OFFSET ,address-units-per-object ,regnum:free))
     (ADDQ ,regnum:free (& ,(* address-units-per-object (+ 1 flonum-size)))
	   ,regnum:free))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (flonum-target! target)))
    (LAP ,@(object->address source temp)
	 (LDT ,target (OFFSET ,address-units-per-object ,temp)))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  (let ((source (flonum-source! source)))
    ((flonum-1-arg/operator operation) (flonum-target! target) source)))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define-arithmetic-method 'FLONUM-ABS flonum-methods/1-arg
  (lambda (target source)
    (LAP (CPYS ,fpr:zero ,source ,target))))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    ; The following line is suggested by the Alpha instruction manual
    ; but it looks like it might generate a negative 0.0
    ; (LAP (CPYSN ,source ,source ,target))
    (LAP (SUBT ,fpr:zero ,source ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  (let ((source1 (flonum-source! source1))
	(source2 (flonum-source! source2)))
    ((flonum-2-args/operator operation) (flonum-target! target)
					source1
					source2)))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name opcode)
	 `(define-arithmetic-method ',primitive-name flonum-methods/2-args
	    (lambda (target source1 source2)
	      (LAP (,opcode ,',source1 ,',source2 ,',target)))))))
  (define-flonum-operation flonum-add ADDT)
  (define-flonum-operation flonum-subtract SUBT)
  (define-flonum-operation flonum-multiply MULT)
  (define-flonum-operation flonum-divide DIVT))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((source (flonum-source! source)))
    (flonum-compare source
     (case predicate
       ((FLONUM-ZERO?) '(FBEQ FBNE))
       ((FLONUM-NEGATIVE?) '(FBLT FBGE))
       ((FLONUM-POSITIVE?) '(FBGT FBLE))
       (else (error "unknown flonum predicate" predicate))))
    (LAP)))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (let* ((source1 (flonum-source! source1))
	 (source2 (flonum-source! source2))
	 (temp (flonum-temporary!)))
    (flonum-compare temp '(FBNE FBEQ))
    (case predicate
      ((FLONUM-EQUAL?) (LAP (CMPTEQ ,source1 ,source2 ,temp)))
      ((FLONUM-LESS?) (LAP (CMPTLT ,source1 ,source2 ,temp)))
      ((FLONUM-GREATER?) (LAP (CMPTLT ,source2 ,source1 ,temp)))
      (else (error "unknown flonum predicate" predicate)))))

(define (flonum-compare source opcodes)
  (set-current-branches!
   (lambda (label)
     (LAP (,(car opcodes) ,source (@PCR ,label))))
   (lambda (label)
     (LAP (,(cadr opcodes) ,source (@PCR ,label))))))
