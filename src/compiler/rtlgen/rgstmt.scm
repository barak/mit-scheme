#| -*-Scheme-*-

$Id: rgstmt.scm,v 4.22 2008/01/30 20:01:56 cph Exp $

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

;;;; RTL Generation: Statements
;;; package: (compiler rtl-generator)

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
	    (find-variable/locative context lvalue
	      (lambda (locative)
		(rtl:make-assignment locative expression))
	      (lambda (environment name)
		(load-temporary-register scfg*scfg->scfg! environment
		  (lambda (environment)
		    (load-temporary-register scfg*scfg->scfg! expression
		      (lambda (expression)
			(wrap-with-continuation-entry
			 context
			 (lambda (cont-label)
			   (rtl:make-interpreter-call:set!
			    cont-label
			    environment
			    (intern-scode-variable!
			     (reference-context/block context)
			     name)
			    expression))))))))
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
		    (lambda (cont-label)
		      (rtl:make-interpreter-call:cache-assignment
		       cont-label cell value))))
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
		     (lambda (cont-label)
		       (rtl:make-interpreter-call:define
			cont-label
			environment
			name
			expression)))))))))))))

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
			   (let ((procedure
				  (block-procedure (variable-block variable))))
			     (and (rvalue/procedure? procedure)
				  (procedure-inline-code? procedure)))))
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
	 (find-stack-overwrite-variable context target))
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
	      (find-variable/value context lvalue
	       rtl:make-unassigned-test
		(lambda (environment name)
		  (scfg*pcfg->pcfg!
		   (load-temporary-register scfg*scfg->scfg! environment
		     (lambda (environment)
		       (wrap-with-continuation-entry
			context
			(lambda (cont-label)
			  (rtl:make-interpreter-call:unassigned?
			   cont-label
			   environment
			   name)))))
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
		(lambda (cont-label)
		  (rtl:make-interpreter-call:cache-unassigned?
		   cont-label
		   cell))))
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