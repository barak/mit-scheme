#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgstmt.scm,v 4.1 1987/12/04 20:31:53 cph Exp $

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

;;;; RTL Generation: Statements

(declare (usual-integrations))

;;;; Assignments

(define (generate/assignment assignment offset)
  (let ((block (assignment-block assignment))
	(lvalue (assignment-lvalue assignment))
	(rvalue (assignment-rvalue assignment)))
    (if (lvalue-integrated? lvalue)
	(make-null-cfg)
	(generate/rvalue rvalue offset scfg*scfg->scfg!
	  (lambda (expression)
	    (find-variable block lvalue offset
	      (lambda (locative)
		(rtl:make-assignment locative expression))
	      (lambda (environment name)
		(rtl:make-interpreter-call:set!
		 environment
		 (intern-scode-variable! block name)
		 expression))
	      (lambda (name)
		(generate/cached-assignment name expression))))))))

(define (generate/cached-assignment name value)
  (let ((temp (rtl:make-pseudo-register)))
    (let ((cell (rtl:make-fetch temp)))
      (let ((contents (rtl:make-fetch cell)))
	(let ((n1 (rtl:make-assignment temp (rtl:make-assignment-cache name)))
	      (n2 (rtl:make-type-test (rtl:make-object->type contents)
				      (ucode-type reference-trap)))
	      (n3 (rtl:make-unassigned-test contents))
	      (n4 (rtl:make-assignment cell value))
	      (n5 (rtl:make-interpreter-call:cache-assignment cell value))
	      (n6 (rtl:make-assignment cell value)))
	  (scfg-next-connect! n1 n2)
	  (pcfg-consequent-connect! n2 n3)
	  (pcfg-alternative-connect! n2 n4)
	  (pcfg-consequent-connect! n3 n6)
	  (pcfg-alternative-connect! n3 n5)
	  (make-scfg (cfg-entry-node n1)
		     (hooks-union (scfg-next-hooks n4)
				  (hooks-union (scfg-next-hooks n5)
					       (scfg-next-hooks n6)))))))))

(define (generate/definition definition offset)
  (let ((block (definition-block definition))
	(lvalue (definition-lvalue definition))
	(rvalue (definition-rvalue definition)))
    (generate/rvalue rvalue offset scfg*scfg->scfg!
      (lambda (expression)
	(transmit-values (find-definition-variable block lvalue offset)
	  (lambda (environment name)
	    (rtl:make-interpreter-call:define environment
					      name
					      expression)))))))

;;;; Virtual Returns

(define (generate/virtual-return return offset)
  (let ((operator (virtual-return-operator return))
	(operand (virtual-return-operand return)))
    (enumeration-case continuation-type (virtual-continuation/type operator)
      ((EFFECT)
       (return-2 (make-null-cfg) offset))
      ((REGISTER VALUE)
       (return-2 (operand->register operand
				    offset
				    (virtual-continuation/register operator))
		 offset))
      ((PUSH)
       (let ((block (virtual-continuation/block operator)))
	 (cond ((rvalue/block? operand)
		(return-2
		 (rtl:make-push
		  (rtl:make-environment
		   (block-ancestor-or-self->locative block
						     operand
						     offset)))
		 (1+ offset)))
	       ((rvalue/continuation? operand)
		;; This is a pun set up by the FG generator.
		(generate/continuation-cons block operand offset))
	       (else
		(return-2 (operand->push operand offset) (1+ offset))))))
      (else
       (error "Unknown continuation type" return)))))

(define (operand->push operand offset)
  (generate/rvalue operand offset scfg*scfg->scfg! rtl:make-push))

(define (operand->register operand offset register)
  (generate/rvalue operand offset scfg*scfg->scfg!
    (lambda (expression)
      (rtl:make-assignment register expression))))

(package (generate/continuation-cons)

(define-export (generate/continuation-cons block continuation offset)
  (set-continuation/offset! continuation offset)
  (let ((values
	 (let ((values
		(if (continuation/dynamic-link? continuation)
		    (return-2 (rtl:make-push-link) (1+ offset))
		    (return-2 (make-null-cfg) offset))))
	   (if (continuation/always-known-operator? continuation)
	       values
	       (begin
		 (enqueue-continuation! continuation)
		 (push-prefix values
			      (rtl:make-push-return
			       (continuation/label continuation))))))))
    (if (ic-block? (continuation/closing-block continuation))
	(push-prefix values
		     (rtl:make-push (rtl:make-fetch register:environment)))
	values)))

(define (push-prefix values prefix)
  (transmit-values values
    (lambda (scfg offset)
      (return-2 (scfg*scfg->scfg! prefix scfg) (1+ offset)))))

)

(define (generate/pop pop offset)
  (rtl:make-pop (continuation*/register (pop-continuation pop))))

;;;; Predicates

(define (generate/true-test true-test offset)
  (generate/predicate (true-test-rvalue true-test)
		      (pnode-consequent true-test)
		      (pnode-alternative true-test)
		      offset))

(define (generate/predicate rvalue consequent alternative offset)
  (if (rvalue/unassigned-test? rvalue)
      (generate/unassigned-test rvalue consequent alternative offset)
      (let ((value (rvalue-known-value rvalue)))
	(if value
	    (generate/known-predicate value consequent alternative offset)
	    (pcfg*scfg->scfg!
	     (generate/rvalue rvalue offset scfg*pcfg->pcfg!
	       rtl:make-true-test)
	     (generate/node consequent offset)
	     (generate/node alternative offset))))))

(define (generate/known-predicate value consequent alternative offset)
  (generate/node (if (and (constant? value) (false? (constant-value value)))
		     alternative
		     consequent)
		 offset))

(define (generate/unassigned-test rvalue consequent alternative offset)
  (let ((block (unassigned-test-block rvalue))
	(lvalue (unassigned-test-lvalue rvalue)))
    (let ((value (lvalue-known-value lvalue)))
      (cond ((not value)
	     (pcfg*scfg->scfg!
	      (find-variable block lvalue offset
		(lambda (locative)
		  (rtl:make-unassigned-test (rtl:make-fetch locative)))
		(lambda (environment name)
		  (scfg*pcfg->pcfg!
		   (rtl:make-interpreter-call:unassigned? environment name)
		   (rtl:make-true-test
		    (rtl:interpreter-call-result:unassigned?))))
		generate/cached-unassigned?)
	      (generate/node consequent offset)
	      (generate/node alternative offset)))
	    ((and (rvalue/constant? value)
		  (scode/unassigned-object? (constant-value value)))
	     (generate/node consequent offset))
	    (else
	     (generate/node alternative offset))))))

(define (generate/cached-unassigned? name)
  (let ((temp (rtl:make-pseudo-register)))
    (let ((cell (rtl:make-fetch temp)))
      (let ((reference (rtl:make-fetch cell)))
	(let ((n1 (rtl:make-assignment temp (rtl:make-variable-cache name)))
	      (n2 (rtl:make-type-test (rtl:make-object->type reference)
				      (ucode-type reference-trap)))
	      (n3 (rtl:make-unassigned-test reference))
	      (n4 (rtl:make-interpreter-call:cache-unassigned? cell))
	      (n5
	       (rtl:make-true-test
		(rtl:interpreter-call-result:cache-unassigned?))))
	  (scfg-next-connect! n1 n2)
	  (pcfg-consequent-connect! n2 n3)
	  (pcfg-alternative-connect! n3 n4)
	  (scfg-next-connect! n4 n5)
	  (make-pcfg (cfg-entry-node n1)
		     (hooks-union (pcfg-consequent-hooks n3)
				  (pcfg-consequent-hooks n5))
		     (hooks-union (pcfg-alternative-hooks n2)
				  (pcfg-alternative-hooks n5))))))))