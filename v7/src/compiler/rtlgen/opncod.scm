#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/opncod.scm,v 4.28 1989/03/29 04:14:08 jinx Exp $

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

;;;; RTL Generation: Inline Combinations

(declare (usual-integrations))

;;;; Analysis

(define (open-coding-analysis applications)
  (for-each (if compiler:open-code-primitives?
		(lambda (application)
		  (if (eq? (application-type application) 'COMBINATION)
		      (let ((inliner (analyze-combination application)))
			(set-combination/inliner! application inliner)
			;; Don't push a return address on the stack
			;; if: (1) the combination is inline coded,
			;; (2) the continuation is known, and (3) the
			;; push is unique for this combination.
			(let ((push
			       (combination/continuation-push application)))
			  (if (and inliner
				   push
				   (rvalue-known-value
				    (combination/continuation application)))
			      (set-virtual-continuation/type!
			       (virtual-return-operator push)
			       continuation-type/effect))))))
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

(define (combination/inline combination)
  (let ((context (combination/context combination))
	(inliner (combination/inliner combination)))
    (generate/return* context
		      (combination/continuation combination)
		      (combination/continuation-push combination)
		      (let ((handler (inliner/handler inliner))
			    (generator (inliner/generator inliner))
			    (expressions
			     (map subproblem->expression
				  (inliner/operands inliner))))
			(make-return-operand
			 (lambda ()
			   ((vector-ref handler 1) generator
						   context
						   expressions))
			 (lambda (finish)
			   ((vector-ref handler 2) generator
						   context
						   expressions
						   finish))
			 (lambda (finish)
			   ((vector-ref handler 3) generator
						   context
						   expressions
						   finish))
			 false)))))

(define (combination/inline/simple? combination)
  (not (memq (primitive-procedure-name
	      (constant-value
	       (rvalue-known-value (combination/operator combination))))
	     non-simple-primitive-names)))

(define (subproblem->expression subproblem)
  (let ((rvalue (subproblem-rvalue subproblem)))
    (let ((value (rvalue-known-value rvalue)))
      (cond ((and value (rvalue/constant? value))
	     (rtl:make-constant (constant-value value)))
	    ((and value
		  (rvalue/procedure? value)
		  (procedure/trivial-or-virtual? value))
	     (make-trivial-closure-cons value))
	    ((and (rvalue/reference? rvalue)
		  (not (variable/value-variable? (reference-lvalue rvalue)))
		  (reference-to-known-location? rvalue))
	     (rtl:make-fetch
	      (find-known-variable (reference-context rvalue)
				   (reference-lvalue rvalue))))
	    (else
	     (rtl:make-fetch
	      (continuation*/register
	       (subproblem-continuation subproblem))))))))

(define (invoke/effect->effect generator context expressions)
  (generator context expressions false))

(define (invoke/predicate->value generator context expressions finish)
  (generator context expressions
    (lambda (pcfg)
      (let ((temporary (rtl:make-pseudo-register)))
	;; Force assignments to be made first.
	(let ((consequent
	       (rtl:make-assignment temporary (rtl:make-constant true)))
	      (alternative
	       (rtl:make-assignment temporary (rtl:make-constant false))))
	  (scfg*scfg->scfg!
	   (pcfg*scfg->scfg! pcfg consequent alternative)
	   (finish (rtl:make-fetch temporary))))))))

(define (invoke/value->effect generator context expressions)
  generator context expressions
  (make-null-cfg))

(define (invoke/value->predicate generator context expressions finish)
  (generator context expressions
    (lambda (expression)
      (finish (rtl:make-true-test expression)))))

(define (invoke/value->value generator context expressions finish)
  (generator context expressions finish))

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
      (if (list? name)
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

(define (define-non-simple-primitive! name)
  (if (not (memq name non-simple-primitive-names))
      (set! non-simple-primitive-names (cons name non-simple-primitive-names)))
  unspecific)

(define non-simple-primitive-names
  '())

;;;; Operand Filters

(define (simple-open-coder generator operand-indices)
  (lambda (operands)
    operands
    (return-2 generator operand-indices)))

(define (constant-filter predicate)
  (lambda (generator constant-index operand-indices)
    (lambda (operands)
      (let ((operand (rvalue-known-value (list-ref operands constant-index))))
	(and operand
	     (rvalue/constant? operand)
	     (let ((value (constant-value operand)))
	       (and (predicate value)
		    (return-2 (generator value) operand-indices))))))))

(define filter/nonnegative-integer
  (constant-filter
   (lambda (value) (and (integer? value) (not (negative? value))))))

(define filter/positive-integer
  (constant-filter
   (lambda (value) (and (integer? value) (positive? value)))))

;;;; Constraint Checkers

(define (open-code:with-checks context checks non-error-cfg error-finish
			       primitive-name expressions)
  (let ((checks (list-transform-negative checks cfg-null?)))
    (if (null? checks)
	non-error-cfg
	;; Don't generate `error-cfg' unless it is needed.  Otherwise
	;; it creates some unreachable code which we can't easily
	;; remove from the output afterwards.
	(let ((error-cfg
	       (with-values (lambda () (generate-continuation-entry context))
		 (lambda (label setup cleanup)
		   (scfg-append!
		    (generate-primitive primitive-name expressions setup label)
		    cleanup
		    (if error-finish
			(error-finish (rtl:make-fetch register:value))
			(make-null-cfg)))))))
	  (let loop ((checks checks))
	    (if (null? checks)
		non-error-cfg
		(pcfg*scfg->scfg! (car checks)
				  (loop (cdr checks)) error-cfg)))))))

(define (generate-primitive name argument-expressions
			    continuation-setup continuation-label)
  (scfg*scfg->scfg!
   (let loop ((args argument-expressions))
     (if (null? args)
	 (scfg*scfg->scfg! continuation-setup
			   (rtl:make-push-return continuation-label))
	 (load-temporary-register scfg*scfg->scfg! (car args)
	   (lambda (temporary)
	     (scfg*scfg->scfg! (loop (cdr args))
			       (rtl:make-push temporary))))))
   (let ((primitive (make-primitive-procedure name true)))
     ((or (special-primitive-handler primitive)
	  rtl:make-invocation:primitive)
      (1+ (length argument-expressions))
      continuation-label
      primitive))))

(define (open-code:type-check expression type)
  (if compiler:generate-type-checks?
      (generate-type-test type
			  expression
			  make-false-pcfg
			  make-true-pcfg
			  identity-procedure)
      (make-null-cfg)))

(define (generate-type-test type expression if-false if-true if-test)
  (if (rtl:constant? expression)
      (if (object-type? type (rtl:constant-value expression))
	  (if-true)
	  (if-false))
      (if-test
       (pcfg/prefer-consequent!
	(rtl:make-type-test (rtl:make-object->type expression) type)))))

(define (open-code:range-check index-expression limit-locative)
  (if compiler:generate-range-checks?
      (pcfg*pcfg->pcfg!
       (generate-nonnegative-check index-expression)
       (pcfg/prefer-consequent!
	(rtl:make-fixnum-pred-2-args
	 'LESS-THAN-FIXNUM?
	 (rtl:make-object->fixnum index-expression)
	 (rtl:make-object->fixnum limit-locative)))
       (make-null-cfg))
      (make-null-cfg)))

(define (open-code:nonnegative-check expression)
  (if compiler:generate-range-checks?
      (generate-nonnegative-check expression)
      (make-null-cfg)))

(define (generate-nonnegative-check expression)
  (if (and (rtl:constant? expression)
	   (let ((value (rtl:constant-value expression)))
	     (and (object-type? (ucode-type fixnum) value)
		  (not (negative? value)))))
      (make-true-pcfg)
      (pcfg-invert
       (pcfg/prefer-alternative!
	(rtl:make-fixnum-pred-1-arg
	 'NEGATIVE-FIXNUM?
	 (rtl:make-object->fixnum expression))))))

;;;; Indexed Memory References

(define (indexed-memory-reference type length-expression index-locative)
  (lambda (name value-type generator)
    (lambda (context expressions finish)
      (let ((object (car expressions))
	    (index (cadr expressions)))
	(open-code:with-checks
	 context
	 (cons*
	  (open-code:type-check object type)
	  (open-code:type-check index (ucode-type fixnum))
	  (open-code:range-check index (length-expression object))
	  (if value-type
	      (list (open-code:type-check (caddr expressions) value-type))
	      '()))
	 (index-locative object index
	   (lambda (locative)
	     (generator locative expressions finish)))
	 finish
	 name
	 expressions)))))

(define (index-locative-generator make-locative
				  header-length-in-objects
				  address-units-per-index)
  (let ((header-length-in-indexes
	 (* header-length-in-objects
	    (quotient address-units-per-object address-units-per-index))))
    (lambda (base index finish)
      (let ((unknown-index
	     (lambda ()
	       (load-temporary-register
		scfg*scfg->scfg!
		(rtl:make-fixnum->address
		 (rtl:make-fixnum-2-args
		  'PLUS-FIXNUM
		  (rtl:make-address->fixnum (rtl:make-object->address base))
		  (let ((index (rtl:make-object->fixnum index)))
		    (if (= address-units-per-index 1)
			index
			(rtl:make-fixnum-2-args
			 'MULTIPLY-FIXNUM
			 (rtl:make-object->fixnum
			  (rtl:make-constant address-units-per-index))
			 index)))))
		(lambda (expression)
		  (finish
		   (make-locative expression header-length-in-indexes)))))))
	(if (rtl:constant? index)
	    (let ((value (rtl:constant-value index)))
	      (if (and (object-type? (ucode-type fixnum) value)
		       (not (negative? value)))
		  (finish
		   (make-locative base (+ header-length-in-indexes value)))
		  (unknown-index)))
	    (unknown-index))))))

(define vector-memory-reference
  (indexed-memory-reference
   (ucode-type vector)
   (lambda (expression)
     (rtl:make-fetch (rtl:locative-offset expression 0)))
   (index-locative-generator rtl:locative-offset 1 address-units-per-object)))

(define string-memory-reference
  (indexed-memory-reference
   (ucode-type string)
   (lambda (expression)
     (rtl:make-fetch (rtl:locative-offset expression 1)))
   (index-locative-generator rtl:locative-byte-offset
			     2
			     address-units-per-packed-char)))

(define (rtl:length-fetch locative)
  (rtl:make-cons-pointer (rtl:make-constant (ucode-type fixnum))
			 (rtl:make-fetch locative)))

(define (rtl:vector-length-fetch locative)
  (rtl:make-cons-pointer (rtl:make-constant (ucode-type fixnum))
			 (rtl:make-object->datum (rtl:make-fetch locative))))

(define (rtl:string-fetch locative)
  (rtl:make-cons-pointer (rtl:make-constant (ucode-type character))
			 (rtl:make-fetch locative)))

(define (rtl:string-assignment locative value)
  (rtl:make-assignment locative (rtl:make-char->ascii value)))

(define (assignment-finisher make-assignment make-fetch)
  (lambda (locative value finish)
    (let ((assignment (make-assignment locative value)))
      (if finish
	  (load-temporary-register scfg*scfg->scfg! (make-fetch locative)
	    (lambda (temporary)
	      (scfg*scfg->scfg! assignment (finish temporary))))
	  assignment))))

(define finish-vector-assignment
  (assignment-finisher rtl:make-assignment rtl:make-fetch))

(define finish-string-assignment
  (assignment-finisher rtl:string-assignment rtl:string-fetch))

;;;; Open Coders

(define-open-coder/predicate 'NULL?
  (simple-open-coder
   (lambda (context expressions finish)
     context
     (finish (pcfg-invert (rtl:make-true-test (car expressions)))))
   '(0)))

(let ((open-code/type-test
       (lambda (type)
	 (lambda (context expressions finish)
	   context
	   (finish
	    (rtl:make-type-test (rtl:make-object->type (car expressions))
				type))))))

  (let ((simple-type-test
	 (lambda (name type)
	   (define-open-coder/predicate name
	     (simple-open-coder (open-code/type-test type) '(0))))))
    (simple-type-test 'PAIR? (ucode-type pair))
    (simple-type-test 'STRING? (ucode-type string))
    (simple-type-test 'BIT-STRING? (ucode-type vector-1b)))

  (define-open-coder/predicate 'OBJECT-TYPE?
    (filter/nonnegative-integer open-code/type-test 0 '(1))))

(define-open-coder/predicate 'EQ?
  (simple-open-coder
   (lambda (context expressions finish)
     context
     (finish (rtl:make-eq-test (car expressions) (cadr expressions))))
   '(0 1)))

(let ((open-code/pair-cons
       (lambda (type)
	 (lambda (context expressions finish)
	   context
	   (finish
	    (rtl:make-typed-cons:pair (rtl:make-constant type)
				      (car expressions)
				      (cadr expressions)))))))

  (define-open-coder/value 'CONS
    (simple-open-coder (open-code/pair-cons (ucode-type pair)) '(0 1)))

  (define-open-coder/value 'SYSTEM-PAIR-CONS
    (filter/nonnegative-integer open-code/pair-cons 0 '(1 2))))

(define-open-coder/value 'VECTOR
  (lambda (operands)
    (and (< (length operands) 32)
	 (return-2 (lambda (context expressions finish)
		     context
		     (finish
		      (rtl:make-typed-cons:vector
		       (rtl:make-constant (ucode-type vector))
		       expressions)))
		   (all-operand-indices operands)))))

(define (all-operand-indices operands)
  (let loop ((operands operands) (index 0))
    (if (null? operands)
	'()
	(cons index (loop (cdr operands) (1+ index))))))

#|
;; This is somewhat painful to implement.  The problem is that most of
;; the open coding takes place in "rtlcon.scm", and the mechanism for
;; doing such things is here.  We should probably try to remodularize
;; the code that transforms "expression-style" RTL into
;; "statement-style" RTL, so we can call it from here and then work in
;; the "statement-style" domain.

(define-open-coder/value 'STRING-ALLOCATE
  (simple-open-coder
   (lambda (context expressions finish)
     (let ((length (car expressions)))
       (open-code:with-checks
	context
	(list (open-code:nonnegative-check length))
	(finish
	 (rtl:make-typed-cons:string
	  (rtl:make-constant (ucode-type string))
	  length))
	finish
	'STRING-ALLOCATE
	expressions)))
   '(0)))
|#

(let ((make-fixed-ref
       (lambda (name make-fetch type index)
	 (lambda (context expressions finish)
	   (let ((expression (car expressions)))
	     (open-code:with-checks
	      context
	      (if type (list (open-code:type-check expression type)) '())
	      (finish (make-fetch (rtl:locative-offset expression index)))
	      finish
	      name
	      expressions)))))
      (standard-def
       (lambda (name fixed-ref)
	 (define-open-coder/value name
	   (simple-open-coder fixed-ref '(0))))))
  (let ((user-ref
	 (lambda (name make-fetch type index)
	   (standard-def name (make-fixed-ref name make-fetch type index)))))
    (user-ref 'CELL-CONTENTS rtl:make-fetch (ucode-type cell) 0)
    (user-ref 'VECTOR-LENGTH rtl:vector-length-fetch (ucode-type vector) 0)
    (user-ref 'STRING-LENGTH rtl:length-fetch (ucode-type string) 1)
    (user-ref 'BIT-STRING-LENGTH rtl:length-fetch (ucode-type vector-1b) 1)
    (user-ref 'SYSTEM-PAIR-CAR rtl:make-fetch false 0)
    (user-ref 'SYSTEM-PAIR-CDR rtl:make-fetch false 1)
    (user-ref 'SYSTEM-HUNK3-CXR0 rtl:make-fetch false 0)
    (user-ref 'SYSTEM-HUNK3-CXR1 rtl:make-fetch false 1)
    (user-ref 'SYSTEM-HUNK3-CXR2 rtl:make-fetch false 2)
    (user-ref 'SYSTEM-VECTOR-SIZE rtl:vector-length-fetch false 0))
  (let ((car-ref (make-fixed-ref 'CAR rtl:make-fetch (ucode-type pair) 0))
	(cdr-ref (make-fixed-ref 'CDR rtl:make-fetch (ucode-type pair) 1)))
    (standard-def 'CAR car-ref)
    (standard-def 'CDR cdr-ref)
    (define-open-coder/value 'GENERAL-CAR-CDR
      (filter/positive-integer
       (lambda (pattern)
	 (lambda (context expressions finish)
	   context
	   (finish
	    (let loop ((pattern pattern) (expression (car expressions)))
	      (if (= pattern 1)
		  expression
		  ((if (odd? pattern) car-ref cdr-ref)
		   context
		   (list expression)
		   (lambda (expression)
		     (loop (quotient pattern 2) expression))))))))
       1
       '(0)))))

(for-each (lambda (name)
	    (define-open-coder/value name
	      (simple-open-coder
	       (vector-memory-reference name false
		 (lambda (locative expressions finish)
		   expressions
		   (finish (rtl:make-fetch locative))))
	       '(0 1))))
	  '(VECTOR-REF SYSTEM-VECTOR-REF))

;; For now SYSTEM-XXXX side effect procedures are considered
;; dangerous to the garbage collector's health.  Some day we will
;; again be able to enable them.

(let ((fixed-assignment
       (lambda (name type index)
	 (define-open-coder/effect name
	   (simple-open-coder
	    (lambda (context expressions finish)
	      (let ((object (car expressions)))
		(open-code:with-checks
		 context
		 (if type (list (open-code:type-check object type)) '())
		 (finish-vector-assignment (rtl:locative-offset object index)
					   (cadr expressions)
					   finish)
		 finish
		 name
		 expressions)))
	    '(0 1))))))
  (fixed-assignment 'SET-CAR! (ucode-type pair) 0)
  (fixed-assignment 'SET-CDR! (ucode-type pair) 1)
  (fixed-assignment 'SET-CELL-CONTENTS! (ucode-type cell) 0)
  #|
  (fixed-assignment 'SYSTEM-PAIR-SET-CAR! false 0)
  (fixed-assignment 'SYSTEM-PAIR-SET-CDR! false 1)
  (fixed-assignment 'SYSTEM-HUNK3-SET-CXR0! false 0)
  (fixed-assignment 'SYSTEM-HUNK3-SET-CXR1! false 1)
  (fixed-assignment 'SYSTEM-HUNK3-SET-CXR2! false 2)
  |#)

(for-each (lambda (name)
	    (define-open-coder/effect name
	      (simple-open-coder
	       (vector-memory-reference name false
		 (lambda (locative expressions finish)
		   (finish-vector-assignment locative
					     (caddr expressions)
					     finish)))
	       '(0 1 2))))
	  '(VECTOR-SET! #| SYSTEM-VECTOR-SET! |#))

;;;; Character/String Primitives

(define-open-coder/value 'CHAR->INTEGER
  (simple-open-coder
   (lambda (context expressions finish)
     (let ((char (car expressions)))
       (open-code:with-checks
	context
	(list (open-code:type-check char (ucode-type character)))
	(finish
	 (rtl:make-cons-pointer
	  (rtl:make-constant (ucode-type fixnum))
	  (rtl:make-object->datum char)))
	finish
	'CHAR->INTEGER
	expressions)))
   '(0)))

(define-open-coder/value 'STRING-REF
  (simple-open-coder
   (string-memory-reference 'STRING-REF false
     (lambda (locative expressions finish)
       expressions
       (finish (rtl:string-fetch locative))))
   '(0 1)))

(define-open-coder/effect 'STRING-SET!
  (simple-open-coder
   (string-memory-reference 'STRING-SET! (ucode-type character)
     (lambda (locative expressions finish)
       (finish-string-assignment locative (caddr expressions) finish)))
   '(0 1 2)))

;;;; Fixnum Arithmetic

(for-each (lambda (fixnum-operator)
	    (define-open-coder/value fixnum-operator
	      (simple-open-coder
	       (lambda (context expressions finish)
		 context
		 (finish
		  (rtl:make-fixnum->object
		   (rtl:make-fixnum-2-args
		    fixnum-operator
		    (rtl:make-object->fixnum (car expressions))
		    (rtl:make-object->fixnum (cadr expressions))))))
	       '(0 1))))
	  '(PLUS-FIXNUM
	    MINUS-FIXNUM
	    MULTIPLY-FIXNUM
	    #| DIVIDE-FIXNUM |#
	    #| GCD-FIXNUM |#))

(for-each (lambda (fixnum-operator)
	    (define-open-coder/value fixnum-operator
	      (simple-open-coder
	       (lambda (context expressions finish)
		 context
		 (finish
		  (rtl:make-fixnum->object
		   (rtl:make-fixnum-1-arg
		    fixnum-operator
		    (rtl:make-object->fixnum (car expressions))))))
	       '(0))))
	  '(ONE-PLUS-FIXNUM MINUS-ONE-PLUS-FIXNUM))

(for-each (lambda (fixnum-pred)
	    (define-open-coder/predicate fixnum-pred
	      (simple-open-coder
	       (lambda (context expressions finish)
		 context
		 (finish
		  (rtl:make-fixnum-pred-2-args
		   fixnum-pred
		   (rtl:make-object->fixnum (car expressions))
		   (rtl:make-object->fixnum (cadr expressions)))))
	       '(0 1))))
	  '(EQUAL-FIXNUM? LESS-THAN-FIXNUM? GREATER-THAN-FIXNUM?))

(for-each (lambda (fixnum-pred)
	    (define-open-coder/predicate fixnum-pred
	      (simple-open-coder
	       (lambda (context expressions finish)
		 context
		 (finish
		  (rtl:make-fixnum-pred-1-arg
		   fixnum-pred
		   (rtl:make-object->fixnum (car expressions)))))
	       '(0))))
	  '(ZERO-FIXNUM? POSITIVE-FIXNUM? NEGATIVE-FIXNUM?))
;;; Generic arithmetic

(define (generic-binary-generator generic-op is-pred?)
  (define-non-simple-primitive! generic-op)
  ((if is-pred? define-open-coder/predicate define-open-coder/value)
   generic-op
   (simple-open-coder
    (let ((fix-op (generic->fixnum-op generic-op)))
      (lambda (context expressions finish)
	(let ((op1 (car expressions))
	      (op2 (cadr expressions))
	      (give-it-up
	       (generic-default generic-op is-pred?
				context expressions finish)))
	  (if is-pred?
	      (generate-binary-type-test (ucode-type fixnum) op1 op2
		give-it-up
		(lambda ()
		  (finish
		   (if (eq? fix-op 'EQUAL-FIXNUM?)
		       ;; This produces better code.
		       (rtl:make-eq-test op1 op2)
		       (rtl:make-fixnum-pred-2-args
			fix-op
			(rtl:make-object->fixnum op1)
			(rtl:make-object->fixnum op2))))))
	      (let ((give-it-up (give-it-up)))
		(generate-binary-type-test (ucode-type fixnum) op1 op2
		  (lambda ()
		    give-it-up)
		  (lambda ()
		    (load-temporary-register scfg*scfg->scfg!
					     (rtl:make-fixnum-2-args
					      fix-op
					      (rtl:make-object->fixnum op1)
					      (rtl:make-object->fixnum op2))
		      (lambda (fix-temp)
			(pcfg*scfg->scfg!
			 (pcfg/prefer-alternative! (rtl:make-overflow-test))
			 give-it-up
			 (finish (rtl:make-fixnum->object fix-temp))))))))))))
    '(0 1))))

(define (generate-binary-type-test type op1 op2 give-it-up do-it)
  (generate-type-test type op1
    give-it-up
    (lambda ()
      (generate-type-test type op2
	give-it-up
	do-it
	(lambda (test)
	  (pcfg*scfg->scfg! test (do-it) (give-it-up)))))
    (lambda (test)
      (generate-type-test type op2
	give-it-up
	(lambda ()
	  (pcfg*scfg->scfg! test (do-it) (give-it-up)))
	(lambda (test*)
	  (let ((give-it-up (give-it-up)))
	    (pcfg*scfg->scfg! test
			      (pcfg*scfg->scfg! test* (do-it) give-it-up)
			      give-it-up)))))))

(define (generic-unary-generator generic-op is-pred?)
  (define-non-simple-primitive! generic-op)
  ((if is-pred? define-open-coder/predicate define-open-coder/value)
   generic-op
   (simple-open-coder
    (let ((fix-op (generic->fixnum-op generic-op)))
      (lambda (context expressions finish)
	(let ((op (car expressions))
	      (give-it-up
	       (generic-default generic-op is-pred?
				context expressions finish)))
	  (if is-pred?
	      (generate-unary-type-test (ucode-type fixnum) op
		give-it-up
		(lambda ()
		  (finish
		   (rtl:make-fixnum-pred-1-arg
		    fix-op
		    (rtl:make-object->fixnum op)))))
	      (let ((give-it-up (give-it-up)))
		(generate-unary-type-test (ucode-type fixnum) op
		  (lambda ()
		    give-it-up)
		  (lambda ()
		    (load-temporary-register scfg*scfg->scfg!
					     (rtl:make-fixnum-1-arg
					      fix-op
					      (rtl:make-object->fixnum op))
		      (lambda (fix-temp)
			(pcfg*scfg->scfg!
			 (pcfg/prefer-alternative! (rtl:make-overflow-test))
			 give-it-up
			 (finish (rtl:make-fixnum->object fix-temp))))))))))))
    '(0))))

(define (generate-unary-type-test type op give-it-up do-it)
  (generate-type-test type op
    give-it-up
    do-it
    (lambda (test)
      (pcfg*scfg->scfg! test (do-it) (give-it-up)))))

(define (generic-default generic-op is-pred? context expressions finish)
  (lambda ()
    (with-values (lambda () (generate-continuation-entry context))
      (lambda (label setup cleanup)
	(scfg-append!
	 (generate-primitive generic-op expressions setup label)
	 cleanup
	 (if is-pred?
	     (finish (rtl:make-true-test (rtl:make-fetch register:value)))
	     (expression-simplify-for-statement (rtl:make-fetch register:value)
						finish)))))))

(define (generic->fixnum-op generic-op)
  (case generic-op
    ((&+) 'PLUS-FIXNUM)
    ((&-) 'MINUS-FIXNUM)
    ((&*) 'MULTIPLY-FIXNUM)
    ((1+) 'ONE-PLUS-FIXNUM)
    ((-1+) 'MINUS-ONE-PLUS-FIXNUM)
    ((&<) 'LESS-THAN-FIXNUM?)
    ((&>) 'GREATER-THAN-FIXNUM?)
    ((&=) 'EQUAL-FIXNUM?)
    ((zero?) 'ZERO-FIXNUM?)
    ((positive?) 'POSITIVE-FIXNUM?)
    ((negative?) 'NEGATIVE-FIXNUM?)
    (else (error "Can't find corresponding fixnum op:" generic-op))))

(define (generic->floatnum-op generic-op)
  (case generic-op
    ((&+) 'PLUS-FLOATNUM)
    ((&-) 'MINUS-FLOATNUM)
    ((&*) 'MULTIPLY-FLOATNUM)
    ((1+) 'ONE-PLUS-FLOATNUM)
    ((-1+) 'MINUS-ONE-PLUS-FLOATNUM)
    ((&<) 'LESS-THAN-FLOATNUM?)
    ((&>) 'GREATER-THAN-FLOATNUM?)
    ((&=) 'EQUAL-FLOATNUM?)
    ((zero?) 'ZERO-FLOATNUM?)
    ((positive?) 'POSITIVE-FLOATNUM?)
    ((negative?) 'NEGATIVE-FLOATNUM?)
    (else (error "Can't find corresponding floatnum op:" generic-op))))

(for-each (lambda (generic-op)
	    (generic-binary-generator generic-op false))
	  '(&+ &- &*))

(for-each (lambda (generic-op)
	    (generic-binary-generator generic-op true))
	  '(&= &< &>))

(for-each (lambda (generic-op)
	    (generic-unary-generator generic-op false))
	  '(1+ -1+))

(for-each (lambda (generic-op)
	    (generic-unary-generator generic-op true))
	  '(zero? positive? negative?))