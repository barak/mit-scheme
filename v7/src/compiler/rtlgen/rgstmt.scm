#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgstmt.scm,v 4.12 1990/02/02 18:40:04 cph Exp $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; RTL Generation: Statements

(declare (usual-integrations))

;;;; Assignments

(define (generate/assignment assignment)
  (let ((context (assignment-context assignment))
	(lvalue (assignment-lvalue assignment))
	(rvalue (assignment-rvalue assignment)))
    (if (lvalue-integrated? lvalue)
	(make-null-cfg)
	(generate/rvalue rvalue scfg*scfg->scfg!
	  (lambda (expression)
	    (find-variable context lvalue
	      (lambda (locative)
		(rtl:make-assignment locative expression))
	      (lambda (environment name)
		(load-temporary-register scfg*scfg->scfg! environment
		  (lambda (environment)
		    (load-temporary-register scfg*scfg->scfg! expression
		      (lambda (expression)
			(wrap-with-continuation-entry
			 context
			 (rtl:make-interpreter-call:set!
			  environment
			  (intern-scode-variable!
			   (reference-context/block context)
			   name)
			  expression)))))))
	      (lambda (name)
		(if (memq 'IGNORE-ASSIGNMENT-TRAPS
			  (variable-declarations lvalue))
		    (load-temporary-register scfg*scfg->scfg!
					     (rtl:make-assignment-cache name)
		      (lambda (cell)
			(rtl:make-assignment cell expression)))
		    (generate/cached-assignment context
						name
						expression)))))))))

(define (generate/cached-assignment context name value)
  (load-temporary-register scfg*scfg->scfg!
			   (rtl:make-assignment-cache name)
    (lambda (cell)
      (load-temporary-register scfg*scfg->scfg! value
	(lambda (value)
	  (let ((contents (rtl:make-fetch cell)))
	    (let ((n2 (rtl:make-type-test (rtl:make-object->type contents)
					  (ucode-type reference-trap)))
		  (n3 (rtl:make-unassigned-test contents))
		  (n4 (rtl:make-assignment cell value))
		  (n5
		   (wrap-with-continuation-entry
		    context
		    (rtl:make-interpreter-call:cache-assignment cell value)))
		  ;; Copy prevents premature control merge which confuses CSE
		  (n6 (rtl:make-assignment cell value)))
	      (pcfg-consequent-connect! n2 n3)
	      (pcfg-alternative-connect! n2 n4)
	      (pcfg-consequent-connect! n3 n6)
	      (pcfg-alternative-connect! n3 n5)
	      (make-scfg (cfg-entry-node n2)
			 (hooks-union
			  (scfg-next-hooks n4)
			  (hooks-union (scfg-next-hooks n5)
				       (scfg-next-hooks n6)))))))))))

(define (generate/definition definition)
  (let ((context (definition-context definition))
	(lvalue (definition-lvalue definition))
	(rvalue (definition-rvalue definition)))
    (generate/rvalue rvalue scfg*scfg->scfg!
      (lambda (expression)
	(with-values (lambda () (find-definition-variable context lvalue))
	  (lambda (environment name)
	    (load-temporary-register scfg*scfg->scfg! environment
	      (lambda (environment)
		(load-temporary-register scfg*scfg->scfg! expression
		  (lambda (expression)
		    (wrap-with-continuation-entry
		     context
		     (rtl:make-interpreter-call:define environment
						       name
						       expression))))))))))))

;;;; Virtual Returns

(define (generate/virtual-return return)
  (let ((operator (virtual-return-operator return))
	(operand (virtual-return-operand return)))
    (if (virtual-continuation/reified? operator)
	(generate/trivial-return (virtual-return-context return)
				 (virtual-continuation/reification operator)
				 operand)
	;; Special case for static links.  These should be handled
	;; using the general mechanism in rgrval, except that there
	;; must be a block reference object, distinct from the block
	;; itself, that contains the context of the reference.  It was
	;; a mistake to make blocks be rvalues in the first place.
	(let ((static-link-reference
	       (lambda ()
		 (let ((locative
			(block-ancestor-or-self->locative
			 (virtual-continuation/context operator)
			 operand
			 0
			 0)))
		   (if (stack-block? operand)
		       (rtl:make-environment locative)
		       locative)))))
	  (enumeration-case continuation-type
	      (virtual-continuation/type operator)
	    ((EFFECT)
	     (make-null-cfg))
	    ((REGISTER VALUE)
	     (let ((register (virtual-continuation/register operator)))
	       (if (rvalue/block? operand)
		   (rtl:make-assignment register (static-link-reference))
		   (operand->register operand register))))
	    ((PUSH)
	     (cond ((rvalue/block? operand)
		    (rtl:make-push (static-link-reference)))
		   ((rvalue/continuation? operand)
		    ;; This is a pun set up by the FG generator.
		    (generate/continuation-cons operand))
		   ((let ((variable (virtual-return/target-lvalue return)))
		      (and variable
			   (variable-in-cell? variable)
			   (procedure-inline-code?
			    (block-procedure (variable-block variable)))))
		    (generate/rvalue operand scfg*scfg->scfg!
		      (lambda (expression)
			(rtl:make-push (rtl:make-cell-cons expression)))))
		   (else
		    (operand->push operand))))
	    (else
	     (error "Unknown continuation type" return)))))))

(define (operand->push operand)
  (generate/rvalue operand scfg*scfg->scfg! rtl:make-push))

(define (operand->register operand register)
  (generate/rvalue operand scfg*scfg->scfg!
    (lambda (expression)
      (rtl:make-assignment register expression))))

(define (load-temporary-register receiver expression generator)
  (let ((temporary (rtl:make-pseudo-register)))
    ;; Force assignment to be made before `generator' is called.  This
    ;; must be done because `rtl:make-assignment' examines
    ;; `expression' and marks `temporary' with attributes that are
    ;; required for proper code generation (for example, if the result
    ;; of `expression' is not an object, this is recorded).  Failure
    ;; to obey this constraint can result in incorrect code.
    (let ((setup (rtl:make-assignment temporary expression)))
      (receiver setup (generator (rtl:make-fetch temporary))))))

(define (generate/continuation-cons continuation)
  (let ((extra
	 (push-continuation-extra (continuation/closing-block continuation))))
    (if (continuation/always-known-operator? continuation)
	extra
	(begin
	  (enqueue-continuation! continuation)
	  (scfg*scfg->scfg!
	   extra
	   (rtl:make-push-return (continuation/label continuation)))))))

(define (generate/pop pop)
  (rtl:make-pop (continuation*/register (pop-continuation pop))))

(define (generate/stack-overwrite stack-overwrite)
  (let ((locative
	 (stack-overwrite-locative (stack-overwrite-context stack-overwrite)
				   (stack-overwrite-target stack-overwrite)))
	(continuation (stack-overwrite-continuation stack-overwrite)))
    (enumeration-case continuation-type (continuation*/type continuation)
      ((REGISTER)
       (let ((simple
	      (lambda ()
		(rtl:make-assignment
		 locative
		 (rtl:make-fetch (continuation*/register continuation))))))
	 (if (procedure? continuation)
	     (let ((lvalue (continuation/parameter continuation)))
	       (if (lvalue-integrated? lvalue)
		   (generate/rvalue (lvalue-known-value lvalue)
				    scfg*scfg->scfg!
		     (lambda (expression)
		       (rtl:make-assignment locative expression)))
		   (simple)))
	     (simple))))
      ((PUSH)
       (rtl:make-pop locative))
      (else
       (error "Unknown continuation type" continuation)))))

(define (stack-overwrite-locative context target)
  (cond ((variable? target)
	 (find-closure-variable context target))
	((block? target)
	 (block-ancestor-or-self->locative
	  context
	  target
	  0
	  (let ((procedure (block-procedure target)))
	    (if (procedure/closure? procedure)
		(procedure-closure-offset procedure)
		(-1+ (block-frame-size target))))))
	(else
	 (error "Unknown target type" target))))

;;;; Predicates

(define (generate/true-test true-test)
  (generate/predicate (true-test-rvalue true-test)
		      (pnode-consequent true-test)
		      (pnode-alternative true-test)))

(define (generate/predicate rvalue consequent alternative)
  (if (rvalue/unassigned-test? rvalue)
      (generate/unassigned-test rvalue consequent alternative)
      (let ((value (rvalue-known-value rvalue)))
	(if value
	    (generate/known-predicate value consequent alternative)
	    (pcfg*scfg->scfg!
	     (generate/rvalue rvalue scfg*pcfg->pcfg!
	       rtl:make-true-test)
	     (generate/node consequent)
	     (generate/node alternative))))))

(define (generate/known-predicate value consequent alternative)
  (generate/node (if (and (constant? value) (false? (constant-value value)))
		     alternative
		     consequent)))

(define (generate/unassigned-test rvalue consequent alternative)
  (let ((context (unassigned-test-context rvalue))
	(lvalue (unassigned-test-lvalue rvalue)))
    (let ((value (lvalue-known-value lvalue)))
      (cond ((not value)
	     (pcfg*scfg->scfg!
	      (find-variable context lvalue
		(lambda (locative)
		  (rtl:make-unassigned-test (rtl:make-fetch locative)))
		(lambda (environment name)
		  (scfg*pcfg->pcfg!
		   (load-temporary-register scfg*scfg->scfg! environment
		     (lambda (environment)
		       (wrap-with-continuation-entry
			context
			(rtl:make-interpreter-call:unassigned? environment
							       name))))
		   (rtl:make-true-test
		    (rtl:interpreter-call-result:unassigned?))))
		(lambda (name)
		  (generate/cached-unassigned? context name)))
	      (generate/node consequent)
	      (generate/node alternative)))
	    ((and (rvalue/constant? value)
		  (unassigned-reference-trap? (constant-value value)))
	     (generate/node consequent))
	    (else
	     (generate/node alternative))))))

(define (generate/cached-unassigned? context name)
  (load-temporary-register scfg*pcfg->pcfg!
			   (rtl:make-variable-cache name)
    (lambda (cell)
      (let ((reference (rtl:make-fetch cell)))
	(let ((n2 (rtl:make-type-test (rtl:make-object->type reference)
				      (ucode-type reference-trap)))
	      (n3 (rtl:make-unassigned-test reference))
	      (n4
	       (wrap-with-continuation-entry
		context
		(rtl:make-interpreter-call:cache-unassigned? cell)))
	      (n5
	       (rtl:make-true-test
		(rtl:interpreter-call-result:cache-unassigned?))))
	  (pcfg-consequent-connect! n2 n3)
	  (pcfg-alternative-connect! n3 n4)
	  (scfg-next-connect! n4 n5)
	  (make-pcfg (cfg-entry-node n2)
		     (hooks-union (pcfg-consequent-hooks n3)
				  (pcfg-consequent-hooks n5))
		     (hooks-union (pcfg-alternative-hooks n2)
				  (pcfg-alternative-hooks n5))))))))