#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/opncod.scm,v 4.42 1991/06/12 20:47:39 cph Exp $

Copyright (c) 1988-1991 Massachusetts Institute of Technology

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
;;; package: (compiler rtl-generator combination/inline)

(declare (usual-integrations))

;;;; Analysis

;; These allows each port to open code a subset of everything below.

(define-integrable (available-primitive? prim)
  (not (memq prim compiler:primitives-with-no-open-coding)))

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
	 (with-values (lambda () ((vector-ref entry 0) operands))
	   (lambda (generator indices internal-close-coding?)
	     (and generator
		  (make-inliner entry
				generator
				indices
				(if (boolean? internal-close-coding?)
				    internal-close-coding?
				    (internal-close-coding?)))))))))

;;;; Code Generator

(define (combination/inline combination)
  (let ((inliner (combination/inliner combination)))
    (let ((finish
	   (lambda (context operand->expression)
	     (generate/return*
	      context
	      (combination/continuation combination)
	      (combination/continuation-push combination)
	      (let ((handler (inliner/handler inliner))
		    (generator (inliner/generator inliner))
		    (expressions
		     (map operand->expression (inliner/operands inliner))))
		(make-return-operand (lambda ()
				       ((vector-ref handler 1) generator
							       combination
							       expressions))
				     (lambda (finish)
				       ((vector-ref handler 2) generator
							       combination
							       expressions
							       finish))
				     (lambda (finish)
				       ((vector-ref handler 3) generator
							       combination
							       expressions
							       finish))
				     false))))))
      (if (and (inliner/internal-close-coding? inliner)
	       (combination/reduction? combination))
	  (let ((prefix (generate/invocation-prefix combination))
		(invocation
		 (finish
		  ;; This value of context is a special kludge.  See
		  ;; `generate/return*' for the details.
		  (length (inliner/operands inliner))
		  (lambda (index)
		    (index->reduction-expression index combination)))))
	    (if prefix
		(scfg*scfg->scfg!
		 (prefix (combination/frame-size combination) 0)
		 invocation)
		invocation))
	  (finish (combination/context combination) subproblem->expression)))))

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
	     (find-known-variable (reference-context rvalue)
				  (reference-lvalue rvalue)))
	    (else
	     (rtl:make-fetch
	      (continuation*/register
	       (subproblem-continuation subproblem))))))))

(define (index->reduction-expression index combination)
  (let ((operand (list-ref (combination/operands combination) index)))
    (if (rvalue-known-constant? operand)
	(rtl:make-constant (rvalue-constant-value operand))
	(rtl:make-fetch
	 (stack-locative-offset (rtl:make-fetch register:stack-pointer)
				index)))))

(define-integrable (combination/reduction? combination)
  (return-operator/reduction? (combination/continuation combination)))

(define (invoke/effect->effect generator combination expressions)
  (generator combination expressions false))

(define (invoke/effect->predicate generator combination expressions finish)
  (generator combination expressions
    (lambda (expression)
      (finish (rtl:make-true-test expression)))))

(define (invoke/effect->value generator combination expressions finish)
  (generator combination expressions finish))

(define (invoke/predicate->effect generator combination expressions)
  generator combination expressions
  (make-null-cfg))

(define (invoke/predicate->predicate generator combination expressions finish)
  (generator combination expressions finish))

(define (invoke/predicate->value generator combination expressions finish)
  (generator combination expressions (finish/predicate->value finish)))

(define ((finish/predicate->value finish) pcfg)
  (pcfg*scfg->scfg! pcfg
		    (finish (rtl:make-constant true))
		    (finish (rtl:make-constant false))))

(define (invoke/value->effect generator combination expressions)
  generator combination expressions
  (make-null-cfg))

(define (invoke/value->predicate generator combination expressions finish)
  (generator combination expressions
    (lambda (expression)
      (finish (rtl:make-true-test expression)))))

(define (invoke/value->value generator combination expressions finish)
  (generator combination expressions finish))

;;;; Definers

(define (open-coder-definer ->effect ->predicate ->value)
  (let ((per-name
	 (lambda (name handler)
	   (if (available-primitive? name)
	       (let ((entry (assq name name->open-coders))
		     (item (vector handler ->effect ->predicate ->value)))
		 (if entry
		     (set-cdr! entry item)
		     (set! name->open-coders
			   (cons (cons name item) name->open-coders))))))))
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
		      invoke/effect->predicate
		      invoke/effect->value))

(define define-open-coder/predicate
  (open-coder-definer invoke/predicate->effect
		      invoke/predicate->predicate
		      invoke/predicate->value))

(define define-open-coder/generic-predicate
  (open-coder-definer
   invoke/predicate->effect
   (lambda (generator combination expressions finish)
     (generator combination expressions true finish))
   (lambda (generator combination expressions finish)
     (generator combination expressions false finish))))

(define define-open-coder/value
  (open-coder-definer invoke/value->effect
		      invoke/value->predicate
		      invoke/value->value))

;;;; Operand Filters

(define (simple-open-coder generator operand-indices internal-close-coding?)
  (lambda (operands)
    operands
    (values generator operand-indices internal-close-coding?)))

(define (constant-filter predicate)
  (lambda (generator constant-index operand-indices internal-close-coding?)
    (lambda (operands)
      (let ((operand (rvalue-known-value (list-ref operands constant-index))))
	(if (and operand
		 (rvalue/constant? operand)
		 (predicate (constant-value operand)))
	    (values (generator (constant-value operand))
		    operand-indices
		    internal-close-coding?)
	    (values false false false))))))

(define filter/nonnegative-integer
  (constant-filter exact-nonnegative-integer?))

(define filter/positive-integer
  (constant-filter
   (lambda (value) (and (exact-integer? value) (positive? value)))))

(define (internal-close-coding-for-type-checks)
  compiler:generate-type-checks?)

(define (internal-close-coding-for-range-checks)
  compiler:generate-range-checks?)

(define (internal-close-coding-for-type-or-range-checks)
  (or compiler:generate-type-checks?
      compiler:generate-range-checks?))

;;;; Constraint Checkers

(define (open-code:with-checks combination checks non-error-cfg error-finish
			       primitive-name expressions)
  (let ((checks
	 (list-transform-negative checks
	   (lambda (cfg)
	     (or (cfg-null? cfg)
		 (pcfg-true? cfg))))))
    (if (null? checks)
	non-error-cfg
	;; Don't generate `error-cfg' unless it is needed.  Otherwise
	;; it creates some unreachable code which we can't easily
	;; remove from the output afterwards.
	(let ((error-cfg
	       (if (combination/reduction? combination)
		   (let ((scfg
			  (generate-primitive primitive-name '() false false)))
		     (make-scfg (cfg-entry-node scfg) '()))
		   (with-values
		       (lambda ()
			 (generate-continuation-entry
			  (combination/context combination)))
		     (lambda (label setup cleanup)
		       (scfg-append!
			(generate-primitive primitive-name
					    expressions
					    setup
					    label)
			cleanup
			(if error-finish
			    (error-finish (rtl:make-fetch register:value))
			    (make-null-cfg))))))))
	  (let loop ((checks checks))
	    (if (null? checks)
		non-error-cfg
		(pcfg*scfg->scfg! (car checks)
				  (loop (cdr checks)) error-cfg)))))))

(define (generate-primitive name argument-expressions
			    continuation-setup continuation-label)
  (scfg*scfg->scfg!
   (if continuation-label
       (let loop ((args argument-expressions))
	 (if (null? args)
	     (scfg*scfg->scfg! continuation-setup
			       (rtl:make-push-return continuation-label))
	     (load-temporary-register scfg*scfg->scfg! (car args)
	       (lambda (temporary)
		 (scfg*scfg->scfg! (loop (cdr args))
				   (rtl:make-push temporary))))))
       (make-null-cfg))
   (let ((primitive (make-primitive-procedure name true)))
     ((or (special-primitive-handler primitive)
	  rtl:make-invocation:primitive)
      (1+ (length argument-expressions))
      continuation-label
      primitive))))

(define (open-code:type-check expression type)
  (if (and type compiler:generate-type-checks?)
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

;; A bunch of these directly use the open coding for fixnum arithmetic.
;; This is not reasonable since the port may not include such open codings.

(define (open-code:range-check index-expression limit-locative)
  (if (and limit-locative compiler:generate-range-checks?)
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

(define (indexed-memory-reference length-expression index-locative)
  (lambda (name base-type value-type generator)
    (lambda (combination expressions finish)
      (let ((object (car expressions))
	    (index (cadr expressions)))
	(open-code:with-checks
	 combination
	 (cons*
	  (open-code:type-check object base-type)
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
			 index
			 false)))
		  false))
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

(define object-memory-reference
  (indexed-memory-reference
   (lambda (expression) expression false)
   (index-locative-generator rtl:locative-offset 0 address-units-per-object)))

(define vector-memory-reference
  (indexed-memory-reference
   (lambda (expression) (rtl:make-fetch (rtl:locative-offset expression 0)))
   (index-locative-generator rtl:locative-offset 1 address-units-per-object)))

(define string-memory-reference
  (indexed-memory-reference
   (lambda (expression) (rtl:make-fetch (rtl:locative-offset expression 1)))
   (index-locative-generator rtl:locative-byte-offset
			     2
			     address-units-per-packed-char)))

(define (rtl:length-fetch locative)
  (rtl:make-cons-pointer (rtl:make-machine-constant (ucode-type fixnum))
			 (rtl:make-fetch locative)))

(define (rtl:vector-length-fetch locative)
  (rtl:make-cons-pointer (rtl:make-machine-constant (ucode-type fixnum))
			 (rtl:make-object->datum (rtl:make-fetch locative))))

(define (rtl:string-fetch locative)
  (rtl:make-cons-pointer (rtl:make-machine-constant (ucode-type character))
			 (rtl:make-fetch locative)))

(define (rtl:vector-8b-fetch locative)
  (rtl:make-cons-pointer (rtl:make-machine-constant (ucode-type fixnum))
			 (rtl:make-fetch locative)))

(define (rtl:string-assignment locative value)
  (rtl:make-assignment locative (rtl:make-char->ascii value)))

(define (assignment-finisher make-assignment make-fetch)
  make-fetch				;ignore
  (lambda (locative value finish)
    (let ((assignment (make-assignment locative value)))
      (if finish
#|	  
	  (load-temporary-register scfg*scfg->scfg! (make-fetch locative)
	    (lambda (temporary)
	      (scfg*scfg->scfg! assignment (finish temporary))))
|#
	  (scfg*scfg->scfg! assignment (finish (rtl:make-constant unspecific)))
	  assignment))))

(define finish-vector-assignment
  (assignment-finisher rtl:make-assignment rtl:make-fetch))

(define finish-string-assignment
  (assignment-finisher rtl:string-assignment rtl:string-fetch))

(define finish-vector-8b-assignment
  (assignment-finisher rtl:make-assignment rtl:vector-8b-fetch))

;;;; Open Coders

(define-open-coder/predicate 'NULL?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-false-test (car expressions))))
   '(0)
   false))

(let ((open-code/type-test
       (lambda (type)
	 (lambda (combination expressions finish)
	   combination
	   (finish
	    (rtl:make-type-test (rtl:make-object->type (car expressions))
				type))))))

  (let ((simple-type-test
	 (lambda (name type)
	   (define-open-coder/predicate name
	     (simple-open-coder (open-code/type-test type) '(0) false)))))
    (simple-type-test 'PAIR? (ucode-type pair))
    (simple-type-test 'STRING? (ucode-type string))
    (simple-type-test 'BIT-STRING? (ucode-type vector-1b)))

  (define-open-coder/predicate 'OBJECT-TYPE?
    (filter/nonnegative-integer open-code/type-test 0 '(1) false)))

(define-open-coder/predicate 'EQ?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-eq-test (car expressions) (cadr expressions))))
   '(0 1)
   false))

(let ((open-code/pair-cons
       (lambda (type)
	 (lambda (combination expressions finish)
	   combination
	   (finish
	    (rtl:make-typed-cons:pair (rtl:make-machine-constant type)
				      (car expressions)
				      (cadr expressions)))))))

  (define-open-coder/value 'CONS
    (simple-open-coder (open-code/pair-cons (ucode-type pair)) '(0 1) false))

  (define-open-coder/value 'SYSTEM-PAIR-CONS
    (filter/nonnegative-integer open-code/pair-cons 0 '(1 2) false)))

(define-open-coder/value 'VECTOR
  (lambda (operands)
    (if (< (length operands) 32)
	(values (lambda (combination expressions finish)
		  combination
		  (finish
		   (rtl:make-typed-cons:vector
		    (rtl:make-machine-constant (ucode-type vector))
		    expressions)))
		(all-operand-indices operands)
		false)
	(values false false false))))

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
   (lambda (combination expressions finish)
     (let ((length (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:nonnegative-check length))
	(finish
	 (rtl:make-typed-cons:string
	  (rtl:make-machine-constant (ucode-type string))
	  length))
	finish
	'STRING-ALLOCATE
	expressions)))
   '(0)
   internal-close-coding-for-range-checks))
|#

(let ((user-ref
       (lambda (name make-fetch type index)
	 (define-open-coder/value name
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((expression (car expressions)))
		(open-code:with-checks
		 combination
		 (if type
		     (list (open-code:type-check expression type))
		     '())
		 (finish (make-fetch (rtl:locative-offset expression index)))
		 finish
		 name
		 expressions)))
	    '(0)
	    internal-close-coding-for-type-checks)))))
  (user-ref 'CELL-CONTENTS rtl:make-fetch (ucode-type cell) 0)
  (user-ref 'VECTOR-LENGTH rtl:length-fetch (ucode-type vector) 0)
  (user-ref 'SYSTEM-VECTOR-SIZE rtl:length-fetch false 0)
  (user-ref 'STRING-LENGTH rtl:length-fetch (ucode-type string) 1)
  (user-ref 'BIT-STRING-LENGTH rtl:length-fetch (ucode-type vector-1b) 1)
  (user-ref 'CAR rtl:make-fetch (ucode-type pair) 0)
  (user-ref 'CDR rtl:make-fetch (ucode-type pair) 1))

(let ((system-ref
       (lambda (name make-fetch index)
	 (define-open-coder/value name
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      combination
	      (finish
	       (make-fetch (rtl:locative-offset (car expressions) index))))
	    '(0)
	    false)))))
  (system-ref 'SYSTEM-PAIR-CAR rtl:make-fetch 0)
  (system-ref 'SYSTEM-PAIR-CDR rtl:make-fetch 1)
  (system-ref 'SYSTEM-HUNK3-CXR0 rtl:make-fetch 0)
  (system-ref 'SYSTEM-HUNK3-CXR1 rtl:make-fetch 1)
  (system-ref 'SYSTEM-HUNK3-CXR2 rtl:make-fetch 2))

(let ((make-fixed-ref
       (lambda (name index)
	 (lambda (combination expressions finish)
	   (let ((expression (car expressions)))
	     (open-code:with-checks
	      combination
	      (list (open-code:type-check expression (ucode-type pair)))
	      (finish (rtl:make-fetch (rtl:locative-offset expression index)))
	      finish
	      name
	      expressions))))))
  (let ((car-ref (make-fixed-ref 'CAR 0))
	(cdr-ref (make-fixed-ref 'CDR 1)))
    (define-open-coder/value 'GENERAL-CAR-CDR
      (filter/positive-integer
       (lambda (pattern)
	 (if (= pattern 1)
	     (lambda (combination expressions finish)
	       combination
	       (finish (car expressions)))
	     (lambda (combination expressions finish)
	       (let loop ((pattern pattern)
			  (expression (car expressions)))
		 (let ((new-pattern (quotient pattern 2)))
		   ((if (odd? pattern) car-ref cdr-ref)
		    combination
		    (list expression)
		    (if (= new-pattern 1)
			finish
			(lambda (expression)
			  (loop new-pattern expression)))))))))
       1
       '(0)
       internal-close-coding-for-type-checks))))

(let ((make-ref
       (lambda (name type)
	 (define-open-coder/value name
	   (simple-open-coder
	    (vector-memory-reference name type false
	      (lambda (locative expressions finish)
		expressions
		(finish (rtl:make-fetch locative))))
	    '(0 1)
	    internal-close-coding-for-type-or-range-checks)))))
  (make-ref 'VECTOR-REF (ucode-type vector))
  (make-ref 'SYSTEM-VECTOR-REF false))

(define-open-coder/value 'PRIMITIVE-OBJECT-REF
  (simple-open-coder
   (object-memory-reference 'PRIMITIVE-OBJECT-REF false false
    (lambda (locative expressions finish)
      expressions
      (finish (rtl:make-fetch locative))))
   '(0 1)
   false))

;; For now SYSTEM-XXXX side effect procedures are considered
;; dangerous to the garbage collector's health.  Some day we will
;; again be able to enable them.

(let ((fixed-assignment
       (lambda (name type index)
	 (define-open-coder/effect name
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((object (car expressions)))
		(open-code:with-checks
		 combination
		 (if type (list (open-code:type-check object type)) '())
		 (finish-vector-assignment (rtl:locative-offset object index)
					   (cadr expressions)
					   finish)
		 finish
		 name
		 expressions)))
	    '(0 1)
	    internal-close-coding-for-type-checks)))))
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

(let ((make-assignment
       (lambda (name type)
	 (define-open-coder/effect name
	   (simple-open-coder
	    (vector-memory-reference name type false
	      (lambda (locative expressions finish)
		(finish-vector-assignment locative
					  (caddr expressions)
					  finish)))
	    '(0 1 2)
	    internal-close-coding-for-type-or-range-checks)))))
  (make-assignment 'VECTOR-SET! (ucode-type vector))
  #|
  (make-assignment 'SYSTEM-VECTOR-SET! false)
  |#)

(define-open-coder/effect 'PRIMITIVE-OBJECT-SET!
  (simple-open-coder
   (object-memory-reference 'PRIMITIVE-OBJECT-SET! false false
    (lambda (locative expressions finish)
      (finish-vector-assignment locative
				(caddr expressions)
				finish)))
   '(0 1 2)
   false))

;;;; Character/String Primitives

(define-open-coder/value 'CHAR->INTEGER
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((char (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:type-check char (ucode-type character)))
	(finish
	 (rtl:make-cons-pointer
	  (rtl:make-machine-constant (ucode-type fixnum))
	  (rtl:make-object->datum char)))
	finish
	'CHAR->INTEGER
	expressions)))
   '(0)
   internal-close-coding-for-type-checks))

(define-open-coder/value 'STRING-REF
  (simple-open-coder
   (string-memory-reference 'STRING-REF (ucode-type string) false
     (lambda (locative expressions finish)
       expressions
       (finish (rtl:string-fetch locative))))
   '(0 1)
   internal-close-coding-for-type-or-range-checks))

(define-open-coder/value 'VECTOR-8B-REF
  (simple-open-coder
   (string-memory-reference 'VECTOR-8B-REF (ucode-type string) false
     (lambda (locative expressions finish)
       expressions
       (finish (rtl:vector-8b-fetch locative))))
   '(0 1)
   internal-close-coding-for-type-or-range-checks))

(define-open-coder/effect 'STRING-SET!
  (simple-open-coder
   (string-memory-reference 'STRING-SET!
			    (ucode-type string)
			    (ucode-type character)
     (lambda (locative expressions finish)
       (finish-string-assignment locative (caddr expressions) finish)))
   '(0 1 2)
   internal-close-coding-for-type-or-range-checks))

(define-open-coder/effect 'VECTOR-8B-SET!
  (simple-open-coder
   (string-memory-reference 'VECTOR-8B-SET!
			    (ucode-type string)
			    (ucode-type fixnum)
     (lambda (locative expressions finish)
       (finish-vector-8b-assignment locative (caddr expressions) finish)))
   '(0 1 2)
   internal-close-coding-for-type-or-range-checks))

;;;; Fixnum Arithmetic

(for-each (lambda (fixnum-operator)
	    (define-open-coder/value fixnum-operator
	      (simple-open-coder
	       (lambda (combination expressions finish)
		 combination
		 (finish
		  (rtl:make-fixnum->object
		   (rtl:make-fixnum-2-args
		    fixnum-operator
		    (rtl:make-object->fixnum (car expressions))
		    (rtl:make-object->fixnum (cadr expressions))
		    false))))
	       '(0 1)
	       false)))
	  '(PLUS-FIXNUM
	    MINUS-FIXNUM
	    MULTIPLY-FIXNUM
	    ;; DIVIDE-FIXNUM
	    GCD-FIXNUM
	    FIXNUM-QUOTIENT
	    FIXNUM-REMAINDER
	    FIXNUM-ANDC
	    FIXNUM-AND
	    FIXNUM-OR
	    FIXNUM-XOR
	    FIXNUM-LSH))

(for-each (lambda (fixnum-operator)
	    (define-open-coder/value fixnum-operator
	      (simple-open-coder
	       (lambda (combination expressions finish)
		 combination
		 (finish
		  (rtl:make-fixnum->object
		   (rtl:make-fixnum-1-arg
		    fixnum-operator
		    (rtl:make-object->fixnum (car expressions))
		    false))))
	       '(0)
	       false)))
	  '(ONE-PLUS-FIXNUM
	    MINUS-ONE-PLUS-FIXNUM
	    FIXNUM-NOT))

(for-each (lambda (fixnum-pred)
	    (define-open-coder/predicate fixnum-pred
	      (simple-open-coder
	       (lambda (combination expressions finish)
		 combination
		 (finish
		  (rtl:make-fixnum-pred-2-args
		   fixnum-pred
		   (rtl:make-object->fixnum (car expressions))
		   (rtl:make-object->fixnum (cadr expressions)))))
	       '(0 1)
	       false)))
	  '(EQUAL-FIXNUM? LESS-THAN-FIXNUM? GREATER-THAN-FIXNUM?))

(for-each (lambda (fixnum-pred)
	    (define-open-coder/predicate fixnum-pred
	      (simple-open-coder
	       (lambda (combination expressions finish)
		 combination
		 (finish
		  (rtl:make-fixnum-pred-1-arg
		   fixnum-pred
		   (rtl:make-object->fixnum (car expressions)))))
	       '(0)
	       false)))
	  '(ZERO-FIXNUM? POSITIVE-FIXNUM? NEGATIVE-FIXNUM?))

;;; Floating Point Arithmetic

(if compiler:open-code-floating-point-arithmetic?
    (begin
      (for-each
       (lambda (flonum-operator)
	 (define-open-coder/value flonum-operator
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((argument (car expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check argument (ucode-type flonum)))
		 (finish (rtl:make-float->object
			  (rtl:make-flonum-1-arg
			   flonum-operator
			   (rtl:make-@address->float
				     (rtl:make-object->address argument))
			   false)))
		 finish
		 flonum-operator
		 expressions)))
	    '(0)
	    internal-close-coding-for-type-checks)))
       '(FLONUM-NEGATE FLONUM-ABS FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN
	 FLONUM-ACOS FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-SQRT FLONUM-ROUND
	 FLONUM-TRUNCATE))

      (for-each
       (lambda (flonum-operator)
	 (define-open-coder/value flonum-operator
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((arg1 (car expressions))
		    (arg2 (cadr expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check arg1 (ucode-type flonum))
		       (open-code:type-check arg2 (ucode-type flonum)))
		 (finish
		  (rtl:make-float->object
		   (rtl:make-flonum-2-args
		    flonum-operator
		    (rtl:make-@address->float
			      (rtl:make-object->address arg1))
		    (rtl:make-@address->float
			      (rtl:make-object->address arg2))
		    false)))
		 finish
		 flonum-operator
		 expressions)))
	    '(0 1)
	    internal-close-coding-for-type-checks)))
       '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE))

      (for-each
       (lambda (flonum-pred)
	 (define-open-coder/predicate flonum-pred
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((argument (car expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check argument (ucode-type flonum)))
		 (finish
		  (rtl:make-flonum-pred-1-arg
		   flonum-pred
		   (rtl:make-@address->float
			     (rtl:make-object->address argument))))
		 (lambda (expression)
		   (finish (rtl:make-true-test expression)))
		 flonum-pred
		 expressions)))
	    '(0)
	    internal-close-coding-for-type-checks)))
       '(FLONUM-ZERO? FLONUM-POSITIVE? FLONUM-NEGATIVE?))

      (for-each
       (lambda (flonum-pred)
	 (define-open-coder/predicate flonum-pred
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((arg1 (car expressions))
		    (arg2 (cadr expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check arg1 (ucode-type flonum))
		       (open-code:type-check arg2 (ucode-type flonum)))
		 (finish (rtl:make-flonum-pred-2-args
			  flonum-pred
			  (rtl:make-@address->float
				    (rtl:make-object->address arg1))
			  (rtl:make-@address->float
				    (rtl:make-object->address arg2))))
		 (lambda (expression)
		   (finish (rtl:make-true-test expression)))
		 flonum-pred
		 expressions)))
	    '(0 1)
	    internal-close-coding-for-type-checks)))
       '(FLONUM-EQUAL? FLONUM-LESS? FLONUM-GREATER?))

      ;; end COMPILER:OPEN-CODE-FLOATING-POINT-ARITHMETIC?
      ))

;;; Generic arithmetic

(define (generic-binary-operator generic-op)
  (define-open-coder/value generic-op
    (simple-open-coder
     (let ((fix-op (generic->fixnum-op generic-op)))
       (lambda (combination expressions finish)
	 (let ((op1 (car expressions))
	       (op2 (cadr expressions))
	       (give-it-up
		(generic-default generic-op combination expressions
				 false finish)))
	   (let ((give-it-up (give-it-up)))
	     (generate-binary-type-test (ucode-type fixnum) op1 op2
	       (lambda ()
		 give-it-up)
	       (lambda ()
		 (load-temporary-register scfg*scfg->scfg!
					  (rtl:make-fixnum-2-args
					   fix-op
					   (rtl:make-object->fixnum op1)
					   (rtl:make-object->fixnum op2)
					   true)
		   (lambda (fix-temp)
		     (pcfg*scfg->scfg!
		      (pcfg/prefer-alternative! (rtl:make-overflow-test))
		      give-it-up
		      (finish (rtl:make-fixnum->object fix-temp)))))))))))
     '(0 1)
     true)))

(define (generic-binary-predicate generic-op)
  (define-open-coder/generic-predicate generic-op
    (simple-open-coder
     (let ((fix-op (generic->fixnum-op generic-op)))
       (lambda (combination expressions predicate? finish)
	 (let ((op1 (car expressions))
	       (op2 (cadr expressions)))
	   (generate-binary-type-test (ucode-type fixnum) op1 op2
	     (generic-default generic-op combination expressions predicate?
			      finish)
	     (lambda ()
	       ((if predicate? finish (finish/predicate->value finish))
		(if (eq? fix-op 'EQUAL-FIXNUM?)
		    ;; This produces better code.
		    (rtl:make-eq-test op1 op2)
		    (rtl:make-fixnum-pred-2-args
		     fix-op
		     (rtl:make-object->fixnum op1)
		     (rtl:make-object->fixnum op2)))))))))
     '(0 1)
     true)))

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

(define (generic-unary-operator generic-op)
  (define-open-coder/value generic-op
    (simple-open-coder
     (let ((fix-op (generic->fixnum-op generic-op)))
       (lambda (combination expressions finish)
	 (let ((op (car expressions)))
	   (let ((give-it-up
		  ((generic-default generic-op combination expressions
				    false finish))))
	     (generate-unary-type-test (ucode-type fixnum) op
	       (lambda ()
		 give-it-up)
	       (lambda ()
		 (load-temporary-register scfg*scfg->scfg!
					  (rtl:make-fixnum-1-arg
					   fix-op
					   (rtl:make-object->fixnum op)
					   true)
		   (lambda (fix-temp)
		     (pcfg*scfg->scfg!
		      (pcfg/prefer-alternative! (rtl:make-overflow-test))
		      give-it-up
		      (finish (rtl:make-fixnum->object fix-temp)))))))))))
     '(0)
     true)))

(define (generic-unary-predicate generic-op)
  (define-open-coder/generic-predicate generic-op
    (simple-open-coder
     (let ((fix-op (generic->fixnum-op generic-op)))
       (lambda (combination expressions predicate? finish)
	 (let ((op (car expressions)))
	   (generate-unary-type-test (ucode-type fixnum) op
	     (generic-default generic-op combination expressions predicate?
			      finish)
	     (lambda ()
	       ((if predicate? finish (finish/predicate->value finish))
		(rtl:make-fixnum-pred-1-arg
		 fix-op
		 (rtl:make-object->fixnum op))))))))
     '(0)
     true)))

(define (generate-unary-type-test type op give-it-up do-it)
  (generate-type-test type op
    give-it-up
    do-it
    (lambda (test)
      (pcfg*scfg->scfg! test (do-it) (give-it-up)))))

(define (generic-default generic-op combination expressions predicate? finish)
  (lambda ()
    (if (combination/reduction? combination)
	(let ((scfg (generate-primitive generic-op '() false false)))
	  (make-scfg (cfg-entry-node scfg) '()))
	(with-values
	    (lambda ()
	      (generate-continuation-entry (combination/context combination)))
	  (lambda (label setup cleanup)
	    (scfg-append!
	     (generate-primitive generic-op expressions setup label)
	     cleanup
	     (if predicate?
		 (finish (rtl:make-true-test (rtl:make-fetch register:value)))
		 (expression-simplify-for-statement
		  (rtl:make-fetch register:value)
		  finish))))))))

(define (generic->fixnum-op generic-op)
  (case generic-op
    ((integer-add &+) 'plus-fixnum)
    ((integer-subtract &-) 'minus-fixnum)
    ((integer-multiply &*) 'multiply-fixnum)
    ((integer-quotient quotient) 'fixnum-quotient)
    ((integer-remainder remainder) 'fixnum-remainder)
    ((integer-add-1 1+) 'one-plus-fixnum)
    ((integer-subtract-1 -1+) 'minus-one-plus-fixnum)
    ((integer-negate) 'fixnum-negate)
    ((integer-less? &<) 'less-than-fixnum?)
    ((integer-greater? &>) 'greater-than-fixnum?)
    ((integer-equal? &=) 'equal-fixnum?)
    ((integer-zero? zero?) 'zero-fixnum?)
    ((integer-positive? positive?) 'positive-fixnum?)
    ((integer-negative? negative?) 'negative-fixnum?)
    (else (error "Can't find corresponding fixnum op:" generic-op))))

(for-each (lambda (generic-op)
	    (generic-binary-operator generic-op))
	  '(&+ &- &* #| &/ |# quotient remainder
	       integer-add integer-subtract integer-multiply
	       integer-quotient integer-remainder))

(for-each (lambda (generic-op)
	    (generic-binary-predicate generic-op))
	  '(&= &< &> integer-equal? integer-less? integer-greater?))

(for-each (lambda (generic-op)
	    (generic-unary-operator generic-op))
	  '(1+ -1+ integer-add-1 integer-subtract-1))

(for-each (lambda (generic-op)
	    (generic-unary-predicate generic-op))
	  '(zero? positive? negative?
	    integer-zero? integer-positive? integer-negative?))