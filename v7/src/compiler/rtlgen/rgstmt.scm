#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgstmt.scm,v 1.2 1987/05/15 19:46:15 cph Exp $

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

;;;; Statements

(define-statement-generator definition-tag
  (lambda (node subproblem?)
    (transmit-values (generate/rvalue (definition-rvalue node))
      (lambda (prefix expression)
	(scfg*scfg->scfg!
	 prefix
	 (find-variable (definition-block node) (definition-lvalue node)
	   (lambda (locative)
	     (error "Definition of compiled variable"))
	   (lambda (environment name)
	     (rtl:make-interpreter-call:define environment name
					       expression))))))))

(define-statement-generator assignment-tag
  (lambda (node subproblem?)
    (let ((lvalue (assignment-lvalue node)))
      (if (and (integrated-vnode? lvalue)
	       (not (value-temporary? lvalue)))
	  (make-null-cfg)
	  (transmit-values (generate/rvalue (definition-rvalue node))
	    (lambda (prefix expression)
	      (scfg*scfg->scfg!
	       prefix
	       (generate/assignment (assignment-block node)
				    lvalue
				    expression
				    subproblem?))))))))

(define (generate/assignment block lvalue expression subproblem?)
  ((vector-method lvalue generate/assignment)
   block lvalue expression subproblem?))

(define (define-assignment tag generator)
  (define-vector-method tag generate/assignment generator))

(define-assignment variable-tag
  (lambda (block lvalue expression subproblem?)
    (find-variable block lvalue
      (lambda (locative)
	(rtl:make-assignment locative expression))
      (lambda (environment name)
	(rtl:make-interpreter-call:set! environment
					(intern-scode-variable! block name)
					expression)))))

(define-assignment temporary-tag
  (lambda (block lvalue expression subproblem?)
    (case (temporary-type lvalue)
      ((#F)
       (rtl:make-assignment lvalue expression))
      ((VALUE)
       (assignment/value-register block expression subproblem?))
      (else
       (error "Unknown temporary type" lvalue)))))

(define (assignment/value-register block expression subproblem?)
;  (if subproblem? (error "Return node has next"))
  (let ((assignment (rtl:make-assignment register:value expression)))
    (if subproblem?
	assignment
	(scfg*scfg->scfg!
	 assignment
	 (if (stack-block? block)
	     (if (stack-parent? block)
		 (rtl:make-message-sender:value (block-frame-size block))
		 (scfg*scfg->scfg!
		  (rtl:make-pop-frame (block-frame-size block))
		  (rtl:make-return)))
	     (rtl:make-return))))))

(define-assignment value-ignore-tag
  (lambda (block lvalue rvalue subproblem? wrap-expression)
    (if subproblem? (error "Return node has next"))
					       (scfg-next-hooks n6)))))))))