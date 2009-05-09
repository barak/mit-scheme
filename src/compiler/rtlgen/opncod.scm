#| -*-Scheme-*-

$Id: opncod.scm,v 4.82 2008/01/30 20:01:56 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
	 (receive (generator indices internal-close-coding?)
	     ((vector-ref entry 0) operands
				   primitive
				   (combination/block combination))
	   (and generator
		(make-inliner entry
			      generator
			      indices
			      (if (boolean? internal-close-coding?)
				  internal-close-coding?
				  (internal-close-coding?
				   primitive
				   (combination/block combination)))))))))

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
  (lambda (operands primitive block)
    operands primitive block
    (values generator operand-indices internal-close-coding?)))

(define (conditional-open-coder predicate open-coder)
  (lambda (operands primitive block)
    (if (predicate operands primitive block)
	(open-coder operands primitive block)
	(values false '() false))))

(define (constant-filter predicate)
  (lambda (generator constant-index operand-indices internal-close-coding?)
    (lambda (operands primitive block)
      primitive block			;ignore
      (let ((operand (rvalue-known-value (list-ref operands constant-index))))
	(if (and operand
		 (rvalue/constant? operand)
		 (predicate (constant-value operand)))
	    (values (generator (constant-value operand))
		    operand-indices
		    internal-close-coding?)
	    (values false false false))))))

(define-integrable scheme-type-limit
  (back-end:expt 2 scheme-type-width))

(define filter/type-code
  (constant-filter
   (lambda (operand)
     (and (exact-nonnegative-integer? operand)
	  (back-end:< operand scheme-type-limit)))))

(define (internal-close-coding-for-type-checks primitive block)
  (block/generate-type-checks? block primitive))

(define (internal-close-coding-for-range-checks primitive block)
  (block/generate-range-checks? block primitive))

(define (internal-close-coding-for-type-or-range-checks primitive block)
  (or (block/generate-type-checks? block primitive)
      (block/generate-range-checks? block primitive)))

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
			  (generate-primitive primitive-name
					      (length expressions)
					      '() false false)))
		     (make-scfg (cfg-entry-node scfg) '()))
		   (with-values
		       (lambda ()
			 (generate-continuation-entry
			  (combination/context combination)))
		     (lambda (label setup cleanup)
		       (scfg-append!
			(generate-primitive primitive-name
					    (length expressions)
					    expressions setup label)
			cleanup
			(if error-finish
			    (error-finish (rtl:make-fetch register:value))
			    (make-null-cfg)))
		       #|
		       ;; This code is preferable to the above
		       ;; expression in some circumstances.  It
		       ;; creates a continuation, but the continuation
		       ;; is left dangling instead of being hooked
		       ;; back into the subsequent code.  This avoids
		       ;; a merge in the RTL and allows the CSE to do
		       ;; a better job -- but the cost is that it
		       ;; creates a continuation that, if invoked, has
		       ;; unpredictable behavior.
		       (let ((scfg
			      (scfg*scfg->scfg!
			       (generate-primitive primitive-name
						   (length expressions)
						   expressions setup label)
			       cleanup)))
			 (make-scfg (cfg-entry-node scfg) '()))
		       |#
		       )))))
	  (let loop ((checks checks))
	    (if (null? checks)
		non-error-cfg
		(pcfg*scfg->scfg! (car checks)
				  (loop (cdr checks))
				  error-cfg)))))))

(define (generate-primitive name nargs argument-expressions
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
      (1+ nargs)
      continuation-label
      primitive))))

(define (open-code:type-check expression type primitive block)
  (if (and type
	   (block/generate-type-checks? block primitive))
      (generate-type-test type
			  expression
			  make-false-pcfg
			  make-true-pcfg
			  identity-procedure)
      (make-true-pcfg)))

(define (generate-type-test type expression if-false if-true if-test)
  (if (rtl:constant? expression)
      (if (object-type? type (rtl:constant-value expression))
	  (if-true)
	  (if-false))
      (if-test
       (pcfg/prefer-consequent!
	(rtl:make-type-test (rtl:make-object->type expression) type)))))

;; The C back end can't use generate-type-test for this because
;; fixnums in the running system (e.g. 64 bits) may be too wide for
;; the portable C output (which assumes no more than 32 bits)
;; Important: This is only used by the open coded generic arithmetic.

(define (generate-fixnum-test expression if-false if-true if-test)
  (if (rtl:constant? expression)
      (if (let ((value (rtl:constant-value expression)))
	    (and (fix:fixnum? value)
		 (>= value signed-fixnum/lower-limit)
		 (< value signed-fixnum/upper-limit)))
	  (if-true)
	  (if-false))
      (if-test
       (pcfg/prefer-consequent!
	(rtl:make-type-test (rtl:make-object->type expression)
			    (ucode-type fixnum))))))

;; A bunch of these directly use the open coding for fixnum arithmetic.
;; This is not reasonable since the port may not include such open codings.

#|
(define (open-code:range-check index-expression limit-locative
			       primitive block)
  (cond ((and limit-locative (block/generate-range-checks? block primitive))
	 (pcfg/prefer-consequent!
	   (rtl:make-fixnum-pred-2-args
	    'UNSIGNED-LESS-THAN-FIXNUM?
	    (rtl:make-object->fixnum index-expression)
	    (rtl:make-object->fixnum limit-locative))))
	(else
	 (make-true-pcfg))))
|#

(define (open-code:index-check index-expression limit-locative
			       primitive block)
  (cond ((not limit-locative)
	 (open-code:index-fixnum-check index-expression primitive block))
	((block/generate-range-checks? block primitive)
	 (pcfg*pcfg->pcfg!
	  (open-code:type-check index-expression (ucode-type fixnum)
				primitive block)
	  (pcfg/prefer-consequent!
	   (rtl:make-fixnum-pred-2-args
	    'UNSIGNED-LESS-THAN-FIXNUM?
	    (rtl:make-object->fixnum index-expression)
	    (rtl:make-object->fixnum limit-locative)))
	  (make-false-pcfg)))
	((block/generate-type-checks? block primitive)
	 (open-code:type-check index-expression (ucode-type fixnum)
			       primitive block))
	(else
	 (make-true-pcfg))))

(define (open-code:nonnegative-check expression primitive block)
  (if (block/generate-range-checks? block primitive)
      (generate-nonnegative-check expression)
      (make-true-pcfg)))

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

(define (open-code:index-fixnum-check expression primitive block)
  (if (or (block/generate-range-checks? block primitive)
	  (block/generate-type-checks? block primitive))
      (generate-index-fixnum-check expression)
      (make-true-pcfg)))

(define (generate-index-fixnum-check expression)
  (if (rtl:constant? expression)
      (let ((value (rtl:constant-value expression)))
	(if (and (object-type? (ucode-type fixnum) value)
		 (not (negative? value)))
	    (make-true-pcfg)
	    (make-false-pcfg)))
      (pcfg/prefer-consequent!
       (rtl:make-pred-1-arg 'INDEX-FIXNUM? expression))))

;;;; Indexed Memory References

(define (indexed-memory-reference length-expression index-locative)
  (lambda (name base-type value-type generator)
    (lambda (combination expressions finish)
      (let ((object (car expressions))
	    (index (cadr expressions)))
	(open-code:with-checks
	 combination
	 (let ((block (combination/block combination)))
	   (cons*
	    (open-code:type-check object base-type name block)
	    (open-code:index-check index (length-expression object) name block)
	    (if value-type
		(list (open-code:type-check (caddr expressions)
					    value-type
					    name
					    block))
		'())))
	 (index-locative object index
	   (lambda (locative)
	     (generator locative expressions finish)))
	 finish
	 name
	 expressions)))))

(define (index-locative-generator make-constant-locative
				  make-variable-locative
				  header-length-in-units
				  scfg*scfg->scfg!)
  scfg*scfg->scfg!			; ignored
  (lambda (base index finish)
    (let ((unknown-index
	   (lambda ()
	     (finish
	      (make-constant-locative
	       (make-variable-locative base
				       (rtl:make-object->datum index))
	       header-length-in-units)))))	       
      (if (rtl:constant? index)
	  (let ((value (rtl:constant-value index)))
	    (if (and (object-type? (ucode-type fixnum) value)
		     (not (negative? value)))
		(finish
		 (make-constant-locative
		  base
		  (back-end:+ value header-length-in-units)))
		(unknown-index)))
	  (unknown-index)))))

(define object-memory-reference
  (indexed-memory-reference
   (lambda (expression) expression false)
   (index-locative-generator rtl:locative-object-offset
			     rtl:locative-object-index
			     0
			     scfg*scfg->scfg!)))

(define vector-memory-reference
  (indexed-memory-reference
   (lambda (expression) (rtl:make-fetch (rtl:locative-offset expression 0)))
   (index-locative-generator rtl:locative-object-offset
			     rtl:locative-object-index
			     1
			     scfg*scfg->scfg!)))

(define string-memory-reference
  (indexed-memory-reference
   (lambda (expression) (rtl:make-fetch (rtl:locative-offset expression 1)))
   (index-locative-generator rtl:locative-byte-offset
			     rtl:locative-byte-index
			     (back-end:* address-units-per-object 2)
			     scfg*scfg->scfg!)))

(define float-memory-reference
  (indexed-memory-reference
   (lambda (expression) (rtl:make-fetch (rtl:locative-offset expression 0)))
   (if (back-end:= address-units-per-float address-units-per-object)
       (index-locative-generator rtl:locative-float-offset
				 rtl:locative-float-index
				 1
				 scfg*scfg->scfg!)
       (lambda (base index finish)
	 (let* ((data-base (rtl:locative-offset base 1))
		(unknown-index
		 (lambda ()
		   (finish
		    (rtl:locative-float-index
		     data-base
		     (rtl:make-object->datum index))))))
	   (if (rtl:constant? index)
	       (let ((value (rtl:constant-value index)))
		 (if (and (object-type? (ucode-type fixnum) value)
			  (not (negative? value)))
		     (finish (rtl:locative-float-offset data-base value))
		     (unknown-index)))
	       (unknown-index)))))))

(define (rtl:length-fetch locative)
  (rtl:make-cons-non-pointer (rtl:make-machine-constant (ucode-type fixnum))
			     (rtl:make-fetch locative)))

(define (rtl:vector-length-fetch locative)
  (rtl:make-cons-non-pointer
   (rtl:make-machine-constant (ucode-type fixnum))
   (rtl:make-object->datum (rtl:make-fetch locative))))

(define (rtl:string-fetch locative)
  (rtl:make-cons-non-pointer (rtl:make-machine-constant (ucode-type character))
			     (rtl:make-fetch locative)))

(define (rtl:vector-8b-fetch locative)
  (rtl:make-cons-non-pointer (rtl:make-machine-constant (ucode-type fixnum))
			     (rtl:make-fetch locative)))

(define (rtl:float-fetch locative)
  (rtl:make-float->object (rtl:make-fetch locative)))

(define (rtl:string-assignment locative value)
  (rtl:make-assignment locative (rtl:make-char->ascii value)))

(define (rtl:float-assignment locative value)
  (rtl:make-assignment locative
		       (rtl:make-object->float value)))

(define rtl:floating-vector-length-fetch
  (if (back-end:= address-units-per-float address-units-per-object)
      rtl:vector-length-fetch
      (let ((quantum
	     (back-end:quotient
	      (back-end:+ address-units-per-float
			  (back-end:- address-units-per-object 1))
	      address-units-per-object)))
	(if (and (number? quantum) (= quantum 2))
	    (lambda (locative)
	      (rtl:make-fixnum->object
	       (rtl:make-fixnum-2-args
		'FIXNUM-LSH
		(rtl:make-object->fixnum (rtl:make-fetch locative))
		(rtl:make-object->fixnum (rtl:make-constant -1))
		false)))
	    (lambda (locative)
	      (rtl:make-fixnum->object
	       (rtl:make-fixnum-2-args
		'FIXNUM-QUOTIENT
		(rtl:make-object->fixnum (rtl:make-fetch locative))
		(rtl:make-object->fixnum (rtl:make-constant quantum))
		false)))))))

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

(define finish-float-assignment
  (assignment-finisher rtl:float-assignment rtl:float-fetch))

;;;; Open Coders

(define-open-coder/predicate 'NULL?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-eq-test (car expressions) (rtl:make-constant '()))))
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
    (simple-type-test 'CHAR?    (ucode-type character))
    (simple-type-test 'PAIR?    (ucode-type pair))
    (simple-type-test 'STRING?  (ucode-type string))
    (simple-type-test 'VECTOR?  (ucode-type vector))
    (simple-type-test '%RECORD? (ucode-type record))
    (simple-type-test 'FIXNUM?  (ucode-type fixnum))
    (simple-type-test 'FLONUM?  (ucode-type flonum))
    (simple-type-test 'BIT-STRING? (ucode-type vector-1b))))

(define-open-coder/predicate 'EQ?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-eq-test (car expressions) (cadr expressions))))
   '(0 1)
   false))

(define-open-coder/predicate 'EQUAL-FIXNUM?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-eq-test (car expressions) (cadr expressions))))
   '(0 1)
   false))

(define-open-coder/predicate 'ZERO-FIXNUM?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-eq-test (car expressions) (rtl:make-constant 0))))
   '(0)
   false))

(define-open-coder/predicate 'INDEX-FIXNUM?
  (simple-open-coder
   (lambda (combination expressions finish)
     combination
     (finish (rtl:make-pred-1-arg 'INDEX-FIXNUM? (car expressions))))
   '(0)
   false))

(define-open-coder/predicate 'OBJECT-TYPE?
  (lambda (operands primitive block)
    primitive block			;ignore
    (let ((operand (rvalue-known-value (car operands))))
      (if (and operand
	       (rvalue/constant? operand)
	       (let ((value (constant-value operand)))
		 (and (exact-nonnegative-integer? value)
		      (back-end:< value scheme-type-limit))))
	  (values (lambda (combination expressions finish)
		    combination
		    (let ((type (car expressions))
			  (object (cadr expressions)))
		      (finish
		       (rtl:make-type-test (rtl:make-object->type object)
					   (rtl:constant-value type)))))
		  '(0 1)
		  false)
	  (values (lambda (combination expressions finish)
		    (let ((type (car expressions))
			  (object (cadr expressions)))
		      (open-code:with-checks
		       combination
		       (list (open-code:index-check
			      type
			      (rtl:make-constant scheme-type-limit)
			      'OBJECT-TYPE?
			      (combination/block combination)))
		       (finish
			(rtl:make-eq-test (rtl:make-object->datum type)
					  (rtl:make-object->type object)))
		       (lambda (expression)
			 (finish (rtl:make-true-test expression)))
		       'OBJECT-TYPE?
		       expressions)))
		  '(0 1)
		  internal-close-coding-for-type-or-range-checks)))))

(let ((open-coder
       (simple-open-coder
	(lambda (combination expressions finish)
	  combination
	  (finish
	   (rtl:make-cons-non-pointer
	    (rtl:make-machine-constant (ucode-type fixnum))
	    (rtl:make-object->type (car expressions)))))
	'(0)
	false)))
  (define-open-coder/value 'OBJECT-TYPE open-coder)
  (define-open-coder/value 'PRIMITIVE-OBJECT-TYPE open-coder))

(define-open-coder/value 'PRIMITIVE-OBJECT-SET-TYPE
  (filter/type-code
   (lambda (type)
     (lambda (combination expressions finish)
       combination
       (finish
	(rtl:make-cons-non-pointer
	 (rtl:make-machine-constant type)
	 (rtl:make-object->datum (car expressions))))))
   0
   '(1)
   false))

(define-open-coder/value 'GET-INTERRUPT-ENABLES
  (simple-open-coder
   (lambda (combination expressions finish)
     combination expressions
     (finish (rtl:length-fetch register:int-mask)))
   '()
   false))

#|
;; This can't work correctly because it needs to do complicated setup
;; of memtop and stack_guard registers, which is a fairly lengthy code
;; sequence on most machines.  Instead it should be implemented by an
;; assembly language hook.

(define-open-coder/effect 'SET-INTERRUPT-ENABLES!
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((mask (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:type-check mask
				    (ucode-type fixnum)
				    'SET-INTERRUPT-ENABLES!
				    (combination/block combination)))
	(let ((assignment
	       (rtl:make-assignment register:int-mask
				    (rtl:make-object->datum mask))))
	  (if finish
	      (load-temporary-register scfg*scfg->scfg!
				       (rtl:length-fetch register:int-mask)
		(lambda (temporary)
		  (scfg*scfg->scfg! assignment (finish temporary))))
	      assignment))
	finish
	'SET-INTERRUPT-ENABLES!
	expressions)
       ))
   '(0)
   internal-close-coding-for-type-checks))
|#

(define-open-coder/value 'PRIMITIVE-GET-FREE
  (filter/type-code
   (lambda (type)
     (lambda (combination expressions finish)
       combination expressions
       (finish
	(rtl:make-cons-pointer (rtl:make-machine-constant type)
			       (rtl:make-fetch register:free)))))
   0
   '()
   false))

(define-open-coder/effect 'PRIMITIVE-INCREMENT-FREE
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((length (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:index-fixnum-check length
					    'PRIMITIVE-INCREMENT-FREE
					    (combination/block combination)))
	(let ((assignment
	       ((index-locative-generator rtl:locative-object-offset
					  rtl:locative-object-index
					  0
					  scfg*scfg->scfg!)
		(rtl:make-fetch register:free)
		length
		(lambda (locative)
		  (rtl:make-assignment register:free
				       (rtl:make-address locative))))))
	  (if finish
	      (scfg*scfg->scfg! assignment
				(finish (rtl:make-constant unspecific)))
	      assignment))
	finish
	'PRIMITIVE-INCREMENT-FREE
	expressions)))
   '(0)
   internal-close-coding-for-type-or-range-checks))

(define-open-coder/predicate 'HEAP-AVAILABLE?
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((length (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:index-fixnum-check length
					    'HEAP-AVAILABLE?
					    (combination/block combination)))
	((index-locative-generator rtl:locative-object-offset
				   rtl:locative-object-index
				   0
				   scfg*pcfg->pcfg!)
	 (rtl:make-fetch register:free)
	 length
	 (lambda (locative)
	   (finish
	    (rtl:make-fixnum-pred-2-args
	     'UNSIGNED-LESS-THAN-FIXNUM?
	     (rtl:make-address->fixnum (rtl:make-address locative))
	     (rtl:make-address->fixnum
	      (rtl:make-fetch register:memory-top))))))
	(lambda (expression)
	  (finish (rtl:make-true-test expression)))
	'HEAP-AVAILABLE?
	expressions)))
   '(0)
   internal-close-coding-for-type-or-range-checks))

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
    (filter/type-code open-code/pair-cons 0 '(1 2) false)))

(define-open-coder/value 'VECTOR
  (lambda (operands primitive block)
    primitive block			;ignore
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

(define-open-coder/value '%RECORD
  (lambda (operands primitive block)
    primitive block			;ignore
    (if (< 1 (length operands) 32)
	(values (lambda (combination expressions finish)
		  combination
		  (finish
		   (rtl:make-typed-cons:vector
		    (rtl:make-machine-constant (ucode-type record))
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
	(list (open-code:nonnegative-check length
					   'STRING-ALLOCATE
					   (combination/block combination)))
	(scfg*scfg->scfg!
	 (finish
	  (rtl:make-typed-cons:string
	   (rtl:make-machine-constant (ucode-type string))
	   length)))
	finish
	'STRING-ALLOCATE
	expressions)))
   '(0)
   internal-close-coding-for-range-checks))
|#

;; The following are discretionally open-coded by the back-end.
;; This allows the type and range checking to take place if
;; the switch is set appropriately.  The back-end does not check.

(define (define-allocator-open-coder name args)
  (define-open-coder/value name
    (simple-open-coder
     (lambda (combination expressions finish)
       (let ((length (car expressions)))
	 (open-code:with-checks
	  combination
	  (list (open-code:index-fixnum-check length
					      name
					      (combination/block combination))
		(make-false-pcfg))
	  (make-null-cfg)
	  finish
	  name
	  expressions)))
     args
     true)))

(define-allocator-open-coder 'STRING-ALLOCATE '(0))
(define-allocator-open-coder 'FLOATING-VECTOR-CONS '(0))
(define-allocator-open-coder 'VECTOR-CONS '(0 1))

(let ((user-ref
       (lambda (name make-fetch type index)
	 (define-open-coder/value name
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((expression (car expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check expression
					     type
					     name
					     (combination/block combination)))
		 (finish (make-fetch (rtl:locative-offset expression index)))
		 finish
		 name
		 expressions)))
	    '(0)
	    internal-close-coding-for-type-checks)))))
  (user-ref 'VECTOR-LENGTH rtl:length-fetch (ucode-type vector) 0)
  (user-ref '%RECORD-LENGTH rtl:vector-length-fetch (ucode-type record) 0)
  (user-ref 'STRING-LENGTH rtl:length-fetch (ucode-type string) 1)
  (user-ref 'BIT-STRING-LENGTH rtl:length-fetch (ucode-type vector-1b) 1)
  (user-ref 'FLOATING-VECTOR-LENGTH
	    rtl:floating-vector-length-fetch
	    (ucode-type flonum)
	    0)
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
  (system-ref 'SYSTEM-HUNK3-CXR2 rtl:make-fetch 2)
  (system-ref 'SYSTEM-VECTOR-SIZE rtl:vector-length-fetch 0))

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
  (make-ref '%RECORD-REF (ucode-type record))
  (make-ref 'SYSTEM-VECTOR-REF false))

(define-open-coder/value 'PRIMITIVE-OBJECT-REF
  (simple-open-coder
   (object-memory-reference 'PRIMITIVE-OBJECT-REF false false
    (lambda (locative expressions finish)
      expressions
      (finish (rtl:make-fetch locative))))
   '(0 1)
   false))

(let ((fixed-assignment
       (lambda (name type index)
	 (define-open-coder/effect name
	   (simple-open-coder
	    (lambda (combination expressions finish)
	      (let ((object (car expressions)))
		(open-code:with-checks
		 combination
		 (list (open-code:type-check object
					     type
					     name
					     (combination/block combination)))
		 (finish-vector-assignment (rtl:locative-offset object index)
					   (cadr expressions)
					   finish)
		 finish
		 name
		 expressions)))
	    '(0 1)
	    internal-close-coding-for-type-checks)))))
  (fixed-assignment 'SET-CAR! (ucode-type pair) 0)
  (fixed-assignment 'SET-CDR! (ucode-type pair) 1))

(define-open-coder/effect 'SET-STRING-LENGTH!
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((object (car expressions))
	   (length (cadr expressions)))
       (open-code:with-checks
	combination
	(let ((name 'SET-STRING-LENGTH!)
	      (block (combination/block combination)))
	  (list (open-code:type-check object (ucode-type string) name block)
		(open-code:index-fixnum-check length name block)))
	(finish-vector-assignment (rtl:locative-offset object 1)
				  (rtl:make-object->datum length)
				  finish)
	finish
	'SET-STRING-LENGTH!
	expressions)))
   '(0 1)
   internal-close-coding-for-type-or-range-checks))

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
  (make-assignment '%RECORD-SET! (ucode-type record)))

(define-open-coder/effect 'PRIMITIVE-OBJECT-SET!
  (simple-open-coder
   (object-memory-reference 'PRIMITIVE-OBJECT-SET! false false
    (lambda (locative expressions finish)
      (finish-vector-assignment locative
				(caddr expressions)
				finish)))
   '(0 1 2)
   false))

;;;; Characters

(define-open-coder/value 'INTEGER->CHAR
  (conditional-open-coder
   (lambda (operands primitive block)
     operands
     (not (block/generate-range-checks? block primitive)))
   (simple-open-coder
    (lambda (combination expressions finish)
      (let ((arg (car expressions)))
	(open-code:with-checks
	 combination
	 (list (open-code:type-check arg
				     (ucode-type fixnum)
				     'INTEGER->CHAR
				     (combination/block combination)))
	 (finish
	  (rtl:make-cons-non-pointer
	   (rtl:make-machine-constant (ucode-type character))
	   (rtl:make-object->datum arg)))
	 finish
	 'INTEGER->CHAR
	 expressions)))
    '(0)
    internal-close-coding-for-type-checks)))

(define-open-coder/value 'CHAR->INTEGER
  (simple-open-coder
   (lambda (combination expressions finish)
     (let ((char (car expressions)))
       (open-code:with-checks
	combination
	(list (open-code:type-check char
				    (ucode-type character)
				    'CHAR->INTEGER
				    (combination/block combination)))
	(finish
	 (rtl:make-cons-non-pointer
	  (rtl:make-machine-constant (ucode-type fixnum))
	  (rtl:make-object->datum char)))
	finish
	'CHAR->INTEGER
	expressions)))
   '(0)
   internal-close-coding-for-type-checks))

;;;; Unboxed vectors

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

(define-open-coder/value 'FLOATING-VECTOR-REF
  (simple-open-coder
   (float-memory-reference 'FLOATING-VECTOR-REF (ucode-type flonum) false
     (lambda (locative expressions finish)
       expressions
       (finish (rtl:float-fetch locative))))
   '(0 1)
   internal-close-coding-for-type-or-range-checks))

(define-open-coder/effect 'FLOATING-VECTOR-SET!
  (simple-open-coder
   (float-memory-reference 'FLOATING-VECTOR-SET!
			    (ucode-type flonum)
			    (ucode-type flonum)
     (lambda (locative expressions finish)
       (finish-float-assignment locative (caddr expressions) finish)))
   '(0 1 2)
   internal-close-coding-for-type-or-range-checks))

;;;; Fixnum Arithmetic

(let* ((one-operand
	(lambda (operator operand)
	  (rtl:make-fixnum->object
	   (rtl:make-fixnum-1-arg
	    operator
	    (rtl:make-object->fixnum operand)
	    false))))

       (two-operand
	(lambda (operator comm? pos neg)
	  (define-open-coder/value operator
	    (simple-open-coder
	     (lambda (combination expressions finish)
	       (define (default)
		 (rtl:make-fixnum->object
		  (rtl:make-fixnum-2-args
		   operator
		   (rtl:make-object->fixnum (car expressions))
		   (rtl:make-object->fixnum (cadr expressions))
		   false)))

	       ;; Guarantee that (fix:-1+ x) and (fix:- x 1)
	       ;; generate identical code, etc.
	       combination
	       (finish
		(cond ((and comm? (rtl:constant? (car expressions)))
		       (case (rtl:constant-value (car expressions))
			 ((0) (cadr expressions))
			 ((1) (one-operand pos (cadr expressions)))
			 ((-1) (one-operand neg (cadr expressions)))
			 (else (default))))
		      ((rtl:constant? (cadr expressions))
		       (case (rtl:constant-value (cadr expressions))
			 ((0) (car expressions))
			 ((1) (one-operand pos (car expressions)))
			 ((-1) (one-operand neg (car expressions)))
			 (else (default))))
		      (else
		       (default)))))
	     '(0 1)
	     false)))))

  (two-operand 'PLUS-FIXNUM true 'ONE-PLUS-FIXNUM 'MINUS-ONE-PLUS-FIXNUM)
  (two-operand 'MINUS-FIXNUM false 'MINUS-ONE-PLUS-FIXNUM 'ONE-PLUS-FIXNUM))

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
	  '(MULTIPLY-FIXNUM
	    ;; DIVIDE-FIXNUM
	    GCD-FIXNUM
	    FIXNUM-QUOTIENT
	    FIXNUM-REMAINDER
	    FIXNUM-ANDC
	    FIXNUM-AND
	    FIXNUM-OR
	    FIXNUM-XOR
	    FIXNUM-LSH))

(for-each (lambda (fixnum-pred first-zero second-zero)
	    (define-open-coder/predicate fixnum-pred
	      (simple-open-coder
	       (lambda (combination expressions finish)
		 combination
		 (finish
		  (cond ((rtl:fixnum-zero? (car expressions))
			 (rtl:make-fixnum-pred-1-arg
			  first-zero
			  (rtl:make-object->fixnum (cadr expressions))))
			((rtl:fixnum-zero? (cadr expressions))
			 (rtl:make-fixnum-pred-1-arg
			  second-zero
			  (rtl:make-object->fixnum (car expressions))))
			(else
			 (rtl:make-fixnum-pred-2-args
			  fixnum-pred
			  (rtl:make-object->fixnum (car expressions))
			  (rtl:make-object->fixnum (cadr expressions)))))))
	       '(0 1)
	       false)))
	  '(EQUAL-FIXNUM? LESS-THAN-FIXNUM? GREATER-THAN-FIXNUM?)
	  '(ZERO-FIXNUM? POSITIVE-FIXNUM? NEGATIVE-FIXNUM?)
	  '(ZERO-FIXNUM? NEGATIVE-FIXNUM? POSITIVE-FIXNUM?))

(define (rtl:fixnum-zero? expression)
  (and (rtl:constant? expression)
       (eqv? 0 (rtl:constant-value expression))))

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

;;;; Floating Point Arithmetic

;; On some machines, there are optional floating-point co-processors,
;; The decision of whether to open-code floating-point arithmetic or
;; not should be made at the last moment, not when the compiler is
;; built.

(define (floating-point-open-coder generator indices internal-close-coding?)
  (conditional-open-coder
   (lambda (operands primitive block)
     operands primitive block		; ignored
     compiler:open-code-floating-point-arithmetic?)
   (simple-open-coder generator indices internal-close-coding?)))

(for-each
 (lambda (flonum-operator)
   (define-open-coder/value flonum-operator
     (floating-point-open-coder
      (lambda (combination expressions finish)
	(let ((argument (car expressions)))
	  (open-code:with-checks
	   combination
	   (list (open-code:type-check argument
				       (ucode-type flonum)
				       flonum-operator
				       (combination/block combination)))
	   (finish (rtl:make-float->object
		    (rtl:make-flonum-1-arg
		     flonum-operator
		     (rtl:make-object->float argument)
		     false)))
	   finish
	   flonum-operator
	   expressions)))
      '(0)
      internal-close-coding-for-type-checks)))
 '(FLONUM-NEGATE FLONUM-ABS FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN
   FLONUM-ACOS FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-SQRT FLONUM-ROUND
   FLONUM-TRUNCATE FLONUM-CEILING FLONUM-FLOOR))

(for-each
 (lambda (flonum-operator)
   (define-open-coder/value flonum-operator
     (floating-point-open-coder
      (lambda (combination expressions finish)
	(let ((arg1 (car expressions))
	      (arg2 (cadr expressions)))
	  (open-code:with-checks
	   combination
	   (let ((name flonum-operator)
		 (block (combination/block combination)))
	     (list (open-code:type-check arg1 (ucode-type flonum) name block)
		   (open-code:type-check arg2 (ucode-type flonum) name block)))
	   (finish
	    (rtl:make-float->object
	     (rtl:make-flonum-2-args
	      flonum-operator
	      (rtl:make-object->float arg1)
	      (rtl:make-object->float arg2)
	      false)))
	   finish
	   flonum-operator
	   expressions)))
      '(0 1)
      internal-close-coding-for-type-checks)))
 '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE FLONUM-ATAN2))

(for-each
 (lambda (flonum-pred)
   (define-open-coder/predicate flonum-pred
     (floating-point-open-coder
      (lambda (combination expressions finish)
	(let ((argument (car expressions)))
	  (open-code:with-checks
	   combination
	   (list (open-code:type-check argument
				       (ucode-type flonum)
				       flonum-pred
				       (combination/block combination)))
	   (finish
	    (rtl:make-flonum-pred-1-arg
	     flonum-pred
	     (rtl:make-object->float argument)))
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
     (floating-point-open-coder
      (lambda (combination expressions finish)
	(let ((arg1 (car expressions))
	      (arg2 (cadr expressions)))
	  (open-code:with-checks
	   combination
	   (let ((name flonum-pred)
		 (block (combination/block combination)))
	     (list (open-code:type-check arg1 (ucode-type flonum) name block)
		   (open-code:type-check arg2 (ucode-type flonum) name block)))
	   (finish (rtl:make-flonum-pred-2-args
		    flonum-pred
		    (rtl:make-object->float arg1)
		    (rtl:make-object->float arg2)))
	   (lambda (expression)
	     (finish (rtl:make-true-test expression)))
	   flonum-pred
	   expressions)))
      '(0 1)
      internal-close-coding-for-type-checks)))
 '(FLONUM-EQUAL? FLONUM-LESS? FLONUM-GREATER?))

;;;; Generic arithmetic

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
	     (generate-binary-fixnum-test op1 op2
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
	   (generate-binary-fixnum-test op1 op2
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

(define (generate-binary-fixnum-test op1 op2 give-it-up do-it)
  (generate-fixnum-test op1
    give-it-up
    (lambda ()
      (generate-fixnum-test op2
	give-it-up
	do-it
	(lambda (test)
	  (pcfg*scfg->scfg! test (do-it) (give-it-up)))))
    (lambda (test)
      (generate-fixnum-test op2
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
	     (generate-unary-fixnum-test op
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
	   (generate-unary-fixnum-test op
	     (generic-default generic-op combination expressions predicate?
			      finish)
	     (lambda ()
	       ((if predicate? finish (finish/predicate->value finish))
		(rtl:make-fixnum-pred-1-arg
		 fix-op
		 (rtl:make-object->fixnum op))))))))
     '(0)
     true)))

(define (generate-unary-fixnum-test op give-it-up do-it)
  (generate-fixnum-test op
    give-it-up
    do-it
    (lambda (test)
      (pcfg*scfg->scfg! test (do-it) (give-it-up)))))

(define (generic-default generic-op combination expressions predicate? finish)
  (lambda ()
    (if (combination/reduction? combination)
	(let ((scfg (generate-primitive generic-op (length expressions) '()
					false false)))
	  (make-scfg (cfg-entry-node scfg) '()))
	(with-values
	    (lambda ()
	      (generate-continuation-entry (combination/context combination)))
	  (lambda (label setup cleanup)
	    (scfg-append!
	     (generate-primitive generic-op (length expressions)
				 expressions setup label)
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
	  '(&+ &- &* #| &/ |# QUOTIENT REMAINDER
	       INTEGER-ADD INTEGER-SUBTRACT INTEGER-MULTIPLY
	       INTEGER-QUOTIENT INTEGER-REMAINDER))

(for-each (lambda (generic-op)
	    (generic-binary-predicate generic-op))
	  '(&= &< &> INTEGER-EQUAL? INTEGER-LESS? INTEGER-GREATER?))

(for-each (lambda (generic-op)
	    (generic-unary-operator generic-op))
	  '(1+ -1+ INTEGER-ADD-1 INTEGER-SUBTRACT-1))

(for-each (lambda (generic-op)
	    (generic-unary-predicate generic-op))
	  '(ZERO? POSITIVE? NEGATIVE?
	    INTEGER-ZERO? INTEGER-POSITIVE? INTEGER-NEGATIVE?))