#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/opncod.scm,v 4.1 1987/12/04 20:30:30 cph Exp $

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

;;;; RTL Generation: Inline Combinations

(declare (usual-integrations))

(package (open-coding-analysis combination/inline)

;;;; Analysis

(define-export (open-coding-analysis applications)
  (for-each (if compiler:open-code-primitives?
		(lambda (application)
		  (if (eq? (application-type application) 'COMBINATION)
		      (set-combination/inliner!
		       application
		       (analyze-combination application))))
		(lambda (application)
		  (if (eq? (application-type application) 'COMBINATION)
		      (set-combination/inliner! application false))))
	    applications))

(define (analyze-combination combination)
  (let ((callee (rvalue-known-value (combination/operator combination))))
    (and callee
	 (rvalue/constant? callee)
	 (let ((value (constant-value callee)))
	   (and (scode/primitive-procedure? value)
		(let ((entry
		       (assq (primitive-procedure-name value)
			     name->open-coders)))
		  (and entry
		       (try-handler combination value (cdr entry)))))))))

(define (try-handler combination primitive entry)
  (let ((operands (combination/operands combination)))
    (and (primitive-arity-correct? primitive (length operands))
	 (let ((result ((vector-ref entry 0) operands)))
	   (and result
		(transmit-values result
		  (lambda (generator indices)
		    (make-inliner entry generator indices))))))))

;;;; Code Generator

(define-export (combination/inline combination offset)
  (generate/return* (combination/block combination)
		    (combination/continuation combination)
		    (let ((inliner (combination/inliner combination)))
		      (let ((handler (inliner/handler inliner))
			    (generator (inliner/generator inliner))
			    (expressions
			     (map (lambda (continuation)
				    (rtl:make-fetch
				     (continuation*/register continuation)))
				  (inliner/operands inliner))))
			(make-return-operand
			 (lambda (offset)
			   ((vector-ref handler 1) generator expressions))
			 (lambda (offset finish)
			   ((vector-ref handler 2) generator
						   expressions
						   finish))
			 (lambda (offset finish)
			   ((vector-ref handler 3) generator
						   expressions
						   finish))
			 false)))
		    offset))

(define (invoke/effect->effect generator expressions)
  (generator expressions false))

(define (invoke/predicate->value generator expressions finish)
  (generator expressions
    (lambda (pcfg)
      (let ((temporary (rtl:make-pseudo-register)))
	(scfg*scfg->scfg!
	 (pcfg*scfg->scfg!
	  pcfg
	  (rtl:make-assignment temporary (rtl:make-constant true))
	  (rtl:make-assignment temporary (rtl:make-constant false)))
	 (finish (rtl:make-fetch temporary)))))))

(define (invoke/value->effect generator expressions)
  (make-null-cfg))

(define (invoke/value->predicate generator expressions finish)
  (generator expressions
    (lambda (expression)
      (finish (rtl:make-true-test expression)))))

(define (invoke/value->value generator expressions finish)
  (generator expressions finish))

;;;; Definers

(define (open-coder-definer ->effect ->predicate ->value)
  (let ((per-name
	 (lambda (name handler)
	   (let ((entry (assq name name->open-coders))
		 (item (vector handler ->effect ->predicate ->value)))
	     (if entry
		 (set-cdr! entry item)
		 (set! name->open-coders
		       (cons (cons name item) name->open-coders)))))))
    (lambda (name handler)
      (if (pair? name)
	  (for-each (lambda (name)
		      (per-name name handler))
		    name)
	  (per-name name handler))
      name)))

(define name->open-coders
  '())

(define define-open-coder/effect
  (open-coder-definer invoke/effect->effect
		      invoke/value->predicate
		      invoke/value->value))

(define define-open-coder/predicate
  (open-coder-definer invoke/value->effect
		      invoke/value->value
		      invoke/predicate->value))

(define define-open-coder/value
  (open-coder-definer invoke/value->effect
		      invoke/value->predicate
		      invoke/value->value))

;;;; Operand Filters

(define (filter/constant rvalue predicate generator)
  (let ((operand (rvalue-known-value rvalue)))
    (and operand
	 (rvalue/constant? operand)
	 (let ((value (constant-value operand)))
	   (and (predicate value)
		(generator value))))))

(define (filter/nonnegative-integer operand generator)
  (filter/constant operand
		   (lambda (value)
		     (and (integer? value)
			  (not (negative? value))))
		   generator))

(define (filter/positive-integer operand generator)
  (filter/constant operand
		   (lambda (value)
		     (and (integer? value)
			  (positive? value)))
		   generator))

;;;; Open Coders

(let ((open-code/type-test
       (lambda (type)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-type-test (rtl:make-object->type (car expressions))
				type))))))

  (let ((define/type-test
	  (lambda (name type)
	    (define-open-coder/predicate name
	      (lambda (operands)
		(return-2 (open-code/type-test type) '(0)))))))
    (define/type-test 'PAIR? (ucode-type pair))
    (define/type-test 'STRING? (ucode-type string))
    (define/type-test 'BIT-STRING? (ucode-type vector-1b)))

  (define-open-coder/predicate 'PRIMITIVE-TYPE?
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/type-test type) '(1)))))))

(let ((open-code/eq-test
       (lambda (expressions finish)
	 (finish (rtl:make-eq-test (car expressions) (cadr expressions))))))
  (define-open-coder/predicate 'EQ?
    (lambda (operands)
      (return-2 open-code/eq-test '(0 1)))))

(let ((open-code/pair-cons
       (lambda (type)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-typed-cons:pair (rtl:make-constant type)
				      (car expressions)
				      (cadr expressions)))))))

  (define-open-coder/value 'CONS
    (lambda (operands)
      (return-2 (open-code/pair-cons (ucode-type pair)) '(0 1))))

  (define-open-coder/value 'SYSTEM-PAIR-CONS
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/pair-cons type) '(1 2)))))))

(let ((open-code/memory-length
       (lambda (index)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-cons-pointer
	     (rtl:make-constant (ucode-type fixnum))
	     (rtl:make-fetch
	      (rtl:locative-offset (car expressions) index))))))))
  (let ((define/length
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		(return-2 (open-code/memory-length index) '(0)))))))
    (define/length '(VECTOR-LENGTH SYSTEM-VECTOR-SIZE) 0)
    (define/length '(STRING-LENGTH BIT-STRING-LENGTH) 1)))

(let ((open-code/memory-ref
       (lambda (index)
	 (lambda (expressions finish)
	   (finish
	    (rtl:make-fetch (rtl:locative-offset (car expressions) index)))))))

  (let ((define/ref
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		(return-2 (open-code/memory-ref index) '(0)))))))
    (define/ref '(CAR SYSTEM-PAIR-CAR CELL-CONTENTS SYSTEM-HUNK3-CXR0) 0)
    (define/ref '(CDR SYSTEM-PAIR-CDR SYSTEM-HUNK3-CXR1) 1)
    (define/ref 'SYSTEM-HUNK3-CXR2 2))

  (define-open-coder/value '(VECTOR-REF SYSTEM-VECTOR-REF)
    (lambda (operands)
      (filter/nonnegative-integer (cadr operands)
	(lambda (index)
	  (return-2 (open-code/memory-ref index) '(0)))))))

(let ((open-code/general-car-cdr
       (lambda (pattern)
	 (lambda (expressions finish)
	   (finish
	    (let loop ((pattern pattern) (expression (car expressions)))
	      (if (= pattern 1)
		  expression
		  (let ((qr (integer-divide pattern 2)))
		    (loop (integer-divide-quotient qr)
			  (rtl:make-fetch
			   (rtl:locative-offset
			    expression
			    (- 1 (integer-divide-remainder qr)))))))))))))
  (define-open-coder/value 'GENERAL-CAR-CDR
    (lambda (operands)
      (filter/positive-integer (cadr operands)
	(lambda (pattern)
	  (return-2 (open-code/general-car-cdr pattern) '(0)))))))

(let ((open-code/memory-assignment
       (lambda (index)
	 (lambda (expressions finish)
	   (let ((locative (rtl:locative-offset (car expressions) index)))
	     (let ((assignment
		    (rtl:make-assignment locative (cadr expressions))))
	       (if finish
		   (let ((temporary (rtl:make-pseudo-register)))
		     (scfg-append!
		      (rtl:make-assignment temporary (rtl:make-fetch locative))
		      assignment
		      (finish (rtl:make-fetch temporary))))
		   assignment)))))))

  (let ((define/set!
	  (lambda (name index)
	    (define-open-coder/effect name
	      (lambda (operands)
		(return-2 (open-code/memory-assignment index) '(0 1)))))))
    (define/set! '(SET-CAR! SYSTEM-PAIR-SET-CAR!
			    SET-CELL-CONTENTS!
			    SYSTEM-HUNK3-SET-CXR0!)
      0)
    (define/set! '(SET-CDR! SYSTEM-PAIR-SET-CDR! SYSTEM-HUNK3-SET-CXR1!) 1)
    (define/set! 'SYSTEM-HUNK3-SET-CXR2! 2))

  (define-open-coder/effect '(VECTOR-SET! SYSTEM-VECTOR-SET!)
    (lambda (operands)
      (filter/nonnegative-integer (cadr operands)
	(lambda (index)
	  (return-2 (open-code/memory-assignment index) '(0 2)))))))

;;; end COMBINATION/INLINE
)