#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/opncod.scm,v 4.20 1988/11/04 22:37:44 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

(define-export (combination/inline combination)
  (let ((offset (node/offset combination)))
    (generate/return* (combination/block combination)
		      (combination/continuation combination)
		      (combination/continuation-push combination)
		      (let ((inliner (combination/inliner combination)))
			(let ((handler (inliner/handler inliner))
			      (generator (inliner/generator inliner))
			      (expressions
			       (map (subproblem->expression offset)
				    (inliner/operands inliner))))
			  (make-return-operand
			   (lambda (offset)
			     offset
			     ((vector-ref handler 1) generator expressions))
			   (lambda (offset finish)
			     offset
			     ((vector-ref handler 2) generator
						     expressions
						     finish))
			   (lambda (offset finish)
			     offset
			     ((vector-ref handler 3) generator
						     expressions
						     finish))
			   false)))
		      offset)))

(define (subproblem->expression offset)
  (lambda (subproblem)
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
		(find-known-variable (reference-block rvalue)
				     (reference-lvalue rvalue)
				     offset)))
	      (else
	       (rtl:make-fetch
		(continuation*/register
		 (subproblem-continuation subproblem)))))))))

(define (invoke/effect->effect generator expressions)
  (generator expressions false))

(define (invoke/predicate->value generator expressions finish)
  (generator expressions
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

(define (invoke/value->effect generator expressions)
  generator expressions
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

;;;; Constraint Checkers

(define-integrable (make-invocation operator operands)
  `(,operator ,@operands))

(define (multiply-guarded-statement guards statement alternate)
  (let guard-loop ((guards guards))
    (cond ((null? guards) statement)
	  ((cfg-null? (car guards)) (guard-loop (cdr guards)))
	  (else
	   (pcfg*scfg->scfg!
	    (car guards)
	    (guard-loop (cdr guards))
	    alternate)))))

(define (open-code:with-checks checks non-error-cfg error-finish prim-invocation)
  (let* ((continuation-entry (generate-continuation-entry))
	 (error-continuation
	  (scfg*scfg->scfg!
	   continuation-entry
	   (if error-finish
	       (error-finish (rtl:make-fetch register:value))
	       (make-null-cfg))))
	 (error-cfg
	  (scfg*scfg->scfg!
	   (generate-primitive
	    (car prim-invocation)
	    (cdr prim-invocation)
	    (rtl:continuation-entry-continuation
	     (rinst-rtl
	      (bblock-instructions
	       (cfg-entry-node continuation-entry)))))
	   error-continuation)))
    (multiply-guarded-statement checks non-error-cfg error-cfg)))

(define (open-code:limit-check checkee-locative limit-locative)
  (if compiler:generate-range-checks?
      (rtl:make-fixnum-pred-2-args 'LESS-THAN-FIXNUM?
	(rtl:make-object->fixnum checkee-locative)
	(rtl:make-object->fixnum limit-locative))
      (make-null-cfg)))

(define (open-code:range-check checkee-locative limit-locative)
  (if compiler:generate-range-checks?
      (pcfg*pcfg->pcfg!
	 (open-code:limit-check checkee-locative limit-locative)
	 (pcfg-invert
	  (rtl:make-fixnum-pred-1-arg 'NEGATIVE-FIXNUM?
	   (rtl:make-object->fixnum checkee-locative)))
	 (make-null-cfg))
      (make-null-cfg)))

(define (open-code:type-check checkee-locative type)
  (if compiler:generate-type-checks?
      (generate-type-test type checkee-locative)
      (make-null-cfg)))


(define (generate-continuation-entry)
  (let* ((label (generate-label))
	 (rtl (rtl:make-continuation-entry label))
	 (rtl-continuation
	  (make-rtl-continuation *current-rgraph* label (cfg-entry-edge rtl))))
    (set! *extra-continuations* (cons rtl-continuation *extra-continuations*))
    rtl))

(define (generate-primitive name arg-list continuation-label)
  (scfg*scfg->scfg!
   (let loop ((args arg-list))
     (if (null? args)
	 (rtl:make-push-return continuation-label)
	 (load-temporary-register scfg*scfg->scfg! (car args)
	   (lambda (temporary)
	     (scfg*scfg->scfg! (loop (cdr args))
			       (rtl:make-push temporary))))))
   (let ((primitive (make-primitive-procedure name true)))
     ((or (special-primitive-handler primitive)
	  rtl:make-invocation:primitive)
      (1+ (length arg-list))
      continuation-label
      primitive))))

(define (generate-type-test type expression)
  (let ((mu-type (microcode-type type)))
    (if (rtl:constant? expression)
	(if (eq? mu-type (object-type (rtl:constant-value expression)))
	    (make-true-pcfg)
	    (make-false-pcfg))
	(rtl:make-type-test (rtl:make-object->type expression) mu-type))))

;;;; Open Coders

(define-open-coder/predicate 'NULL?
  (lambda (operands)
    operands
    (return-2 (lambda (expressions finish)
		(finish (pcfg-invert (rtl:make-true-test (car expressions)))))
	      '(0))))

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
		operands
		(return-2 (open-code/type-test type) '(0)))))))
    (define/type-test 'PAIR? (ucode-type pair))
    (define/type-test 'STRING? (ucode-type string))
    (define/type-test 'BIT-STRING? (ucode-type vector-1b)))

  (define-open-coder/predicate 'OBJECT-TYPE?
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/type-test type) '(1)))))))

(let ((open-code/eq-test
       (lambda (expressions finish)
	 (finish (rtl:make-eq-test (car expressions) (cadr expressions))))))
  (define-open-coder/predicate 'EQ?
    (lambda (operands)
      operands
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
      operands
      (return-2 (open-code/pair-cons (ucode-type pair)) '(0 1))))

  (define-open-coder/value 'SYSTEM-PAIR-CONS
    (lambda (operands)
      (filter/nonnegative-integer (car operands)
	(lambda (type)
	  (return-2 (open-code/pair-cons type) '(1 2)))))))

(define-open-coder/value 'VECTOR
  (lambda (operands)
    (and (< (length operands) 32)
	 (return-2 (lambda (expressions finish)
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
		operands
		(return-2 (open-code/memory-length index) '(0)))))))
    (define/length '(VECTOR-LENGTH SYSTEM-VECTOR-SIZE) 0)
    (define/length '(STRING-LENGTH BIT-STRING-LENGTH) 1)))

(define (generate-index-locative vector index finish)
  (load-temporary-register
   scfg*scfg->scfg!
   (rtl:make-fixnum->address
    (rtl:make-fixnum-2-args
     'PLUS-FIXNUM
     (rtl:make-address->fixnum (rtl:make-object->address vector))
     (rtl:make-fixnum-2-args
      'MULTIPLY-FIXNUM
      (rtl:make-object->fixnum
       (rtl:make-constant
	(quotient scheme-object-width
		  addressing-granularity)))
      (rtl:make-object->fixnum index))))
   finish))

(let* ((open-code/memory-ref
	(lambda (index)
	  (lambda (expressions finish)
	    (finish
	     (rtl:make-fetch
	      (rtl:locative-offset (car expressions) index))))))
       (open-code/vector-ref
	(lambda (name)
	  (lambda (expressions finish)
	    (let ((vector (car expressions))
		  (index (cadr expressions)))
	      (open-code:with-checks
	       (list
		(open-code:type-check vector 'VECTOR)
		(open-code:type-check index 'FIXNUM)
		(open-code:range-check
		 index
		 (rtl:make-fetch
		  (rtl:locative-offset vector 0))))
	       (generate-index-locative
		vector
		index
		(lambda (memory-locative)
		  ((open-code/memory-ref 1)
		   (list memory-locative)
		   finish)))
	       finish
	       (make-invocation name expressions))))))
       (open-code/constant-vector-ref
	(lambda (name index)
	  (lambda (expressions finish)
	    (let ((vector (car expressions)))
	      (open-code:with-checks
	       (list
		(open-code:type-check vector 'VECTOR)
		(open-code:limit-check
		 (rtl:make-constant index)
		 (rtl:make-fetch
		  (rtl:locative-offset vector 0))))
	       ((open-code/memory-ref index) expressions finish)
	       finish
	       (make-invocation name expressions)))))))

  (let ((define/ref
	  (lambda (name index)
	    (define-open-coder/value name
	      (lambda (operands)
		operands
		(return-2 (open-code/memory-ref index) '(0)))))))
    (define/ref
      '(CAR SYSTEM-PAIR-CAR CELL-CONTENTS SYSTEM-HUNK3-CXR0) 0)
    (define/ref
      '(CDR SYSTEM-PAIR-CDR SYSTEM-HUNK3-CXR1) 1)
    (define/ref 'SYSTEM-HUNK3-CXR2 2))

  (for-each
   (lambda (name)
     (define-open-coder/value name
       (lambda (operands)
	 (or (filter/nonnegative-integer (cadr operands)
		  (lambda (index)
		    (return-2
		     (open-code/constant-vector-ref name (1+ index))
		     '(0 1))))
	     (return-2 (open-code/vector-ref name)
		       '(0 1))))))
   '(VECTOR-REF SYSTEM-VECTOR-REF)))


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

(let* ((open-code/memory-assignment
	(lambda (index)
	  (lambda (expressions finish)
	    (let* ((locative (rtl:locative-offset (car expressions) index))
		   (assignment
		    (rtl:make-assignment locative
					 (car (last-pair expressions)))))
	      (if finish
		  (load-temporary-register scfg*scfg->scfg!
					   (rtl:make-fetch locative)
		    (lambda (temporary)
		      (scfg*scfg->scfg! assignment (finish temporary))))
		  assignment)))))
       (open-code/vector-set
	(lambda (name)
	  (lambda (expressions finish)
	    (let ((vector (car expressions))
		  (index (cadr expressions))
		  (newval-list (cddr expressions)))
	      (open-code:with-checks
	       (list
		(open-code:type-check vector 'VECTOR)
		(open-code:type-check index 'FIXNUM)
		(open-code:range-check
		 index
		 (rtl:make-fetch
		  (rtl:locative-offset vector 0))))
	       (generate-index-locative
		vector
		index
		(lambda (memory-locative)
		  ((open-code/memory-assignment 1)
		   (cons memory-locative newval-list)
		   finish)))
	       finish
	       (make-invocation name expressions))))))
       (open-code/constant-vector-set
	(lambda (name index)
	  (lambda (expressions finish)
	    (let ((vector (car expressions)))
	      (open-code:with-checks
	       (list
		(open-code:type-check vector 'VECTOR)
		(open-code:limit-check
		 (rtl:make-constant index)
		 (rtl:make-fetch
		  (rtl:locative-offset vector 0))))
	       ((open-code/memory-assignment index) expressions finish)
	       finish
	       (make-invocation name expressions)))))))

  ;; For now SYSTEM-XXXX side effect procedures are considered
  ;; dangerous to the garbage collector's health.  Some day we will
  ;; again be able to enable them.

  (let ((define/set!
	  (lambda (name index)
	    (define-open-coder/effect name
	      (lambda (operands)
		operands
		(return-2 (open-code/memory-assignment index) '(0 1)))))))
    (define/set! '(SET-CAR!
		   SET-CELL-CONTENTS!
		   #| SYSTEM-PAIR-SET-CAR! |#
		   #| SYSTEM-HUNK3-SET-CXR0! |#)
      0)
    (define/set! '(SET-CDR!
		   #| SYSTEM-PAIR-SET-CDR! |#
		   #| SYSTEM-HUNK3-SET-CXR1! |#)
      1)
    (define/set! '(#| SYSTEM-HUNK3-SET-CXR2! |#)
      2))

  (for-each
   (lambda (name)
     (define-open-coder/effect name
       (lambda (operands)
	 (or (filter/nonnegative-integer
	      (cadr operands)
	      (lambda (index)
		(return-2 (open-code/constant-vector-set name (1+ index))
			  '(0 1 2))))
	     (return-2 (open-code/vector-set name)
		       '(0 1 2))))))
   '(VECTOR-SET! #| SYSTEM-VECTOR-SET! |#)))

(let ((define-fixnum-2-args
	(lambda (fixnum-operator)
	  (define-open-coder/value fixnum-operator
	    (lambda (operands)
	      operands
	      (return-2
	       (lambda (expressions finish)
		 (finish (rtl:make-fixnum->object
			  (rtl:make-fixnum-2-args
			   fixnum-operator
			   (rtl:make-object->fixnum (car expressions))
			   (rtl:make-object->fixnum (cadr expressions))))))
	       '(0 1)))))))
  (for-each define-fixnum-2-args
	    '(PLUS-FIXNUM
	      MINUS-FIXNUM
	      MULTIPLY-FIXNUM
	      #| DIVIDE-FIXNUM |#
	      #| GCD-FIXNUM |#)))

(let ((define-fixnum-1-arg
	(lambda (fixnum-operator)
	  (define-open-coder/value fixnum-operator
	    (lambda (operand)
	      operand
	      (return-2
	       (lambda (expressions finish)
		 (finish (rtl:make-fixnum->object
			  (rtl:make-fixnum-1-arg
			   fixnum-operator
			   (rtl:make-object->fixnum (car expressions))))))
	       '(0)))))))
  (for-each
   define-fixnum-1-arg
    '(ONE-PLUS-FIXNUM MINUS-ONE-PLUS-FIXNUM)))

(let ((define-fixnum-pred-2-args
	(lambda (fixnum-pred)
	  (define-open-coder/predicate fixnum-pred
	    (lambda (operands)
	      operands
	      (return-2
	       (lambda (expressions finish)
		 (finish (rtl:make-fixnum-pred-2-args
			  fixnum-pred
			  (rtl:make-object->fixnum (car expressions))
			  (rtl:make-object->fixnum (cadr expressions)))))
	       '(0 1)))))))
  (for-each
   define-fixnum-pred-2-args
   '(EQUAL-FIXNUM? LESS-THAN-FIXNUM? GREATER-THAN-FIXNUM?)))

(let ((define-fixnum-pred-1-arg
	(lambda (fixnum-pred)
	  (define-open-coder/predicate fixnum-pred
	    (lambda (operand)
	      operand
	      (return-2
	       (lambda (expressions finish)
		 (finish (rtl:make-fixnum-pred-1-arg
			  fixnum-pred
			  (rtl:make-object->fixnum (car expressions)))))
	       '(0)))))))
  (for-each
   define-fixnum-pred-1-arg
   '(ZERO-FIXNUM? POSITIVE-FIXNUM? NEGATIVE-FIXNUM?)))


;;; Generic arithmetic

(define (generate-generic-binary expression finish is-pred?)
  (let ((continuation-entry (generate-continuation-entry))
	(generic-op (rtl:generic-binary-operator expression))
	(fix-op
	 (generic->fixnum-op (rtl:generic-binary-operator expression)))
	(flo-op
	 (generic->floatnum-op (rtl:generic-binary-operator expression)))
	(op1 (rtl:generic-binary-operand-1 expression))
	(op2 (rtl:generic-binary-operand-2 expression)))
    (let* ((give-it-up
	    (scfg-append!
	     (generate-primitive
	      generic-op
	      (cddr expression)
	      (rtl:continuation-entry-continuation
	       (rinst-rtl
		(bblock-instructions
		 (cfg-entry-node continuation-entry)))))
	     continuation-entry
	     (if is-pred?
		 (finish
		  (rtl:make-true-test (rtl:make-fetch register:value)))
		 (expression-simplify-for-statement
		  (rtl:make-fetch register:value)
		  finish))))
	   (generic-flonum
	    ;; For now we will just call the generic op.
	    ;; When we have open coded flonums, we will
	    ;; stick that stuff here.
	    give-it-up)
	   (generic-3
	    ;; op1 is a flonum, op2 is not
	    (pcfg*scfg->scfg!
	     (generate-type-test 'FIXNUM op2)
	     ;; Whem we have open coded flonums we
	     ;; will convert op2 to a float and do a
	     ;; floating op.
	     generic-flonum
	     give-it-up))
	   (generic-2
	    ;; op1 is a fixnum, op2 is not
	    (if compiler:open-code-flonum-checks?
		(pcfg*scfg->scfg!
		 (generate-type-test 'FLONUM op2)
		 ;; Whem we have open coded flonums we
		 ;; will convert op1 to a float and do a
		 ;; floating op.
		 generic-flonum
		 give-it-up)
		give-it-up))
	   (generic-1
	    ;; op1 is not a fixnum, op2 unknown
	    (if compiler:open-code-flonum-checks?
		(pcfg*scfg->scfg!
		 (generate-type-test 'FLONUM op1)
		 (pcfg*scfg->scfg!
		  (generate-type-test 'FLONUM op2)
		  ;; For now we will just call the generic op.
		  ;; When we have open coded flonums, we will
		  ;; stick that stuff here.
		  generic-flonum
		  generic-3)
		 give-it-up)
		give-it-up)))
      (if is-pred?
	  (if (eq? fix-op 'EQUAL-FIXNUM?)
	      ;; This produces significantly better code.
	      (pcfg*scfg->scfg!
	       (rtl:make-eq-test op1 op2)
	       (finish (make-true-pcfg))
	       generic-1)
	      (pcfg*scfg->scfg!
	       (generate-type-test 'FIXNUM op1)
	       (pcfg*scfg->scfg!
		(generate-type-test 'FIXNUM op2)
		(finish
		 (rtl:make-fixnum-pred-2-args
		  fix-op
		  (rtl:make-object->fixnum op1)
		  (rtl:make-object->fixnum op2)))
		generic-2)
	       generic-1))
	  (pcfg*scfg->scfg!
	   (generate-type-test 'FIXNUM op1)
	   (pcfg*scfg->scfg!
	    (generate-type-test 'FIXNUM op2)
	    (load-temporary-register scfg*scfg->scfg!
				     (rtl:make-fixnum-2-args
				      fix-op
				      (rtl:make-object->fixnum op1)
				      (rtl:make-object->fixnum op2))
	      (lambda (fix-temp)
		(pcfg*scfg->scfg!
		 (rtl:make-overflow-test)
		 give-it-up
		 (finish (rtl:make-fixnum->object fix-temp)))))
	    generic-2)
	   generic-1)))))

(define (generate-generic-unary expression finish is-pred?)
  (let ((continuation-entry (generate-continuation-entry))
	(generic-op (rtl:generic-unary-operator expression))
	(fix-op (generic->fixnum-op
		 (rtl:generic-unary-operator expression)))
	(flo-op (generic->floatnum-op
		 (rtl:generic-unary-operator expression)))
	(op (rtl:generic-unary-operand expression)))
    (let* ((give-it-up
	    (scfg-append!
	     (generate-primitive
	      generic-op
	      (cddr expression)
	      (rtl:continuation-entry-continuation
	       (rinst-rtl
		(bblock-instructions
		 (cfg-entry-node continuation-entry)))))
	     continuation-entry
	     (if is-pred?
		 (finish
		  (rtl:make-true-test (rtl:make-fetch register:value)))
		 (expression-simplify-for-statement
		  (rtl:make-fetch register:value)
		  finish))))
	   (generic-flonum
	    ;; For now we will just call the generic op.
	    ;; When we have open coded flonums, we will
	    ;; stick that stuff here.
	    give-it-up))
      (if is-pred?
	  (pcfg*scfg->scfg!
	   (generate-type-test 'FIXNUM op)
	   (finish
	    (rtl:make-fixnum-pred-1-arg
	    fix-op
	    (rtl:make-object->fixnum op)))
	   (if compiler:open-code-flonum-checks?
	       (pcfg*scfg->scfg!
		(generate-type-test 'FLONUM op)
		generic-flonum
		give-it-up)
	       give-it-up))
	  (pcfg*scfg->scfg!
	   (generate-type-test 'FIXNUM op)
	   (load-temporary-register scfg*scfg->scfg!
				    (rtl:make-fixnum-1-arg
				     fix-op
				     (rtl:make-object->fixnum op))
	     (lambda (fix-temp)
	       (pcfg*scfg->scfg!
		(rtl:make-overflow-test)
		give-it-up
		(finish (rtl:make-fixnum->object fix-temp)))))
	   (if compiler:open-code-flonum-checks?
	       (pcfg*scfg->scfg!
		(generate-type-test 'FLONUM op)
		generic-flonum
		give-it-up)
	       give-it-up))))))

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
    (else (error "Can't find corresponding fixnum op:"
		 generic-op))))

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
    (else (error "Can't find corresponding floatnum op:"
		 generic-op))))


(for-each (lambda (generic-op)
	    (define-open-coder/value generic-op
	      (lambda (operands)
		operands
		(return-2
		  (lambda (expressions finish)
		    (generate-generic-binary
		     (rtl:make-generic-binary generic-op
					      (car expressions)
					      (cadr expressions))
		     finish
		     false))
		  '(0 1)))))
	  '(&+ &- &*))

(for-each (lambda (generic-op)
	    (define-open-coder/value generic-op
	      (lambda (operand)
		operand
		(return-2
		  (lambda (expression finish)
		    (generate-generic-unary
		     (rtl:make-generic-unary generic-op (car expression))
		     finish
		     false))
		  '(0)))))
	  '(1+ -1+))

(for-each (lambda (generic-op)
	    (define-open-coder/predicate generic-op
	      (lambda (operands)
		operands
		(return-2
		  (lambda (expressions finish)
		    (generate-generic-binary
		     (rtl:make-generic-binary generic-op
					      (car expressions)
					      (cadr expressions))
		     finish
		     true))
		  '(0 1)))))
	  '(&= &< &>))

(for-each (lambda (generic-op)
	    (define-open-coder/predicate generic-op
	      (lambda (operand)
		operand
		(return-2
		  (lambda (expression finish)
		    (generate-generic-unary
		     (rtl:make-generic-unary generic-op (car expression))
		     finish
		     true))
		  '(0)))))
	  '(zero? positive? negative?))

;;; Character open-coding

(let ((define-character->fixnum
	(lambda (character->fixnum rtl:coercion)
	  (define-open-coder/value character->fixnum
	    (lambda (operand)
	      operand
	      (return-2 (lambda (expressions finish)
			  (finish (rtl:make-cons-pointer
				   (rtl:make-constant (ucode-type fixnum))
				   (rtl:coercion (car expressions)))))
			'(0)))))))
  (define-character->fixnum 'CHAR->INTEGER rtl:make-object->datum)
  (define-character->fixnum 'CHAR->ASCII rtl:make-char->ascii))

;;; String

(let ((string-header-size (quotient (* 2 scheme-object-width) 8)))

(define-open-coder/value 'STRING-REF
  (lambda (operands)
    (filter/nonnegative-integer (cadr operands)
      (lambda (index)
	(return-2
	 (lambda (expressions finish)
	   (finish (rtl:make-cons-pointer
		    (rtl:make-constant (ucode-type character))
		    (rtl:make-fetch
		     (rtl:locative-byte-offset
		      (car expressions)
		      (+ string-header-size index))))))
	 '(0))))))

(define-open-coder/effect 'STRING-SET!
  (lambda (operands)
    (filter/nonnegative-integer (cadr operands)
      (lambda (index)
	(return-2
	 (lambda (expressions finish)
	   (let* ((locative
		   (rtl:locative-byte-offset (car expressions)
					     (+ string-header-size index)))
		  (assignment
		   (rtl:make-assignment
		    locative
		    (rtl:make-char->ascii (cadr expressions)))))
	     (if finish
		 (load-temporary-register
		  scfg*scfg->scfg!
		  (rtl:make-cons-pointer
		   (rtl:make-constant (ucode-type character))
		   (rtl:make-fetch locative))
		  (lambda (temporary)
		    (scfg*scfg->scfg! assignment (finish temporary))))
		 assignment)))
	 '(0 2))))))

;;; End STRING operations, LET
)

;;; end COMBINATION/INLINE
)