#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlcon.scm,v 1.1 1987/04/21 23:49:53 cph Exp $

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

;;;; Register Transfer Language: Complex Constructors

(declare (usual-integrations))

;;;; Statements

(define (rtl:make-assignment locative expression)
  (expression-simplify-for-statement expression
    (lambda (expression)
      (locative-dereference-for-statement locative
	(lambda (address)
	  (%make-assign address expression))))))

(define (rtl:make-eq-test expression-1 expression-2)
  (expression-simplify-for-predicate expression-1
    (lambda (expression-1)
      (expression-simplify-for-predicate expression-2
	(lambda (expression-2)
	  (%make-eq-test expression-1 expression-2))))))

(define (rtl:make-true-test expression)
  (expression-simplify-for-predicate expression
    (lambda (expression)
      (%make-true-test expression))))

(define (rtl:make-type-test expression type)
  (expression-simplify-for-predicate expression
    (lambda (expression)
      (%make-type-test expression type))))

(define (rtl:make-unassigned-test expression)
  (expression-simplify-for-predicate expression
    (lambda (expression)
      (%make-unassigned-test expression))))

;;; Some statements vanish, converted into lower-level patterns.

(define (rtl:make-pop locative)
  (locative-dereference-for-statement locative
    (lambda (address)
      (%make-assign address (stack-pop-address)))))

(define (rtl:make-pop-frame n)
  (rtl:make-assignment
   register:stack-pointer
   (rtl:make-address
    (stack-locative-offset (rtl:make-fetch register:stack-pointer) n))))

(define (rtl:make-push expression)
  (expression-simplify-for-statement expression
    (lambda (expression)
      (%make-assign (stack-push-address) expression))))

(define (rtl:make-push-return continuation)
  (%make-assign (stack-push-address)
		    (rtl:make-entry:continuation continuation)))

;;; Interpreter Calls

(define ((interpreter-lookup-maker %make) environment name)
  (expression-simplify-for-statement environment
    (lambda (environment)
      (%make environment name))))

(define ((interpreter-assignment-maker %make) environment name value)
  (expression-simplify-for-statement value
    (lambda (value)
      (expression-simplify-for-statement environment
	(lambda (environment)
	  (%make environment name value))))))

(define rtl:make-interpreter-call:access
  (interpreter-lookup-maker %make-interpreter-call:access))

(define rtl:make-interpreter-call:define
  (interpreter-assignment-maker %make-interpreter-call:define))

(define rtl:make-interpreter-call:lookup
  (interpreter-lookup-maker %make-interpreter-call:lookup))

(define rtl:make-interpreter-call:set!
  (interpreter-assignment-maker %make-interpreter-call:set!))

(define rtl:make-interpreter-call:unassigned?
  (interpreter-lookup-maker %make-interpreter-call:unassigned?))

(define rtl:make-interpreter-call:unbound?
  (interpreter-lookup-maker %make-interpreter-call:unbound?))

;;; Invocations

(define *jump-invocations*)

(define (rtl:make-invocation:jump number-pushed prefix continuation procedure)
  (let ((scfg
	 (%make-invocation:jump number-pushed prefix continuation procedure)))
    (set! *jump-invocations* (cons (cfg-entry-node scfg) *jump-invocations*))
    scfg))

(define (rtl:make-invocation:lookup number-pushed prefix continuation
				    environment name)
  (expression-simplify-for-statement environment
    (lambda (environment)
      (%make-invocation:lookup number-pushed prefix continuation
			       environment name))))

;;;; Expression Simplification

(package (locative-dereference-for-statement
	  expression-simplify-for-statement
	  expression-simplify-for-predicate)

(define-export (locative-dereference-for-statement locative receiver)
  (locative-dereference locative scfg*scfg->scfg!
    receiver
    (lambda (register offset)
      (receiver (rtl:make-offset register offset)))))

(define (locative-dereference locative scfg-append! if-register if-memory)
  (locative-dereference-1 locative scfg-append! locative-fetch
			  if-register if-memory))

(define (locative-dereference-1 locative scfg-append! locative-fetch
				if-register if-memory)
  (cond ((symbol? locative)
	 (let ((register (rtl:machine-register? locative)))
	   (if register
	       (if-register register)
	       (if-memory (interpreter-regs-pointer)
			  (rtl:interpreter-register->offset locative)))))
	((temporary? locative)
	 (if-register (temporary->register locative)))
	((pair? locative)
	 (case (car locative)
	   ((FETCH)
	    (locative-fetch (cadr locative) scfg-append!
	      (lambda (register)
		(if-memory register 0))))
	   ((OFFSET)
	    (let ((fetch (cadr locative)))
	      (if (and (pair? fetch) (eq? (car fetch) 'FETCH))
		  (locative-fetch (cadr fetch) scfg-append!
		    (lambda (register)
		      (if-memory register (caddr locative))))
		  (error "LOCATIVE-DEREFERENCE: Bad OFFSET" locative))))
	   (else
	    (error "LOCATIVE-DEREFERENCE: Unknown keyword" (car locative)))))
	(else
	 (error "LOCATIVE-DEREFERENCE: Illegal locative" locative))))

(define (locative-fetch locative scfg-append! receiver)
  (locative-fetch-1 locative scfg-append!
    (lambda (register)
      (if (register-contains-address? (rtl:register-number register))
	  (receiver register)
	  (assign-to-temporary (rtl:make-object->address register)
			       scfg-append!
			       receiver)))))

(define (locative-fetch-1 locative scfg-append! receiver)
  (locative-dereference locative scfg-append!
    receiver
    (lambda (register offset)
      (assign-to-temporary (rtl:make-offset register offset)
			   scfg-append!
			   receiver))))

(define-export (expression-simplify-for-statement expression receiver)
  (expression-simplify expression scfg*scfg->scfg! receiver))

(define-export (expression-simplify-for-predicate expression receiver)
  (expression-simplify expression scfg*pcfg->pcfg! receiver))

(define (expression-simplify expression scfg-append! receiver)
  (let ((entry (assq (car expression) expression-methods))
	(receiver (expression-receiver scfg-append! receiver)))
    (if entry
	(apply (cdr entry) receiver scfg-append! (cdr expression))
	(receiver expression))))

(define ((expression-receiver scfg-append! receiver) expression)
  (if (memq (car expression)
	    '(REGISTER CONSTANT ENTRY:CONTINUATION ENTRY:PROCEDURE UNASSIGNED))
      (receiver expression)
      (assign-to-temporary expression scfg-append! receiver)))

(define (assign-to-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (scfg-append! (%make-assign pseudo expression) (receiver pseudo))))

(define (define-expression-method name method)
  (let ((entry (assq name expression-methods)))
    (if entry
	(set-cdr! entry method)
	(set! expression-methods
	      (cons (cons name method) expression-methods)))))

(define expression-methods
  '())

(define-expression-method 'ADDRESS
  (lambda (receiver scfg-append! locative)
    (locative-dereference-1 locative scfg-append! locative-fetch-1
      (lambda (register)
	(error "Can't take ADDRESS of a register" locative))
      (lambda (register offset)
	(receiver (if (zero? offset)
		      register
		      (rtl:make-offset-address register offset)))))))

(define-expression-method 'CELL-CONS
  (lambda (receiver scfg-append! expression)
    (let ((free (interpreter-free-pointer)))
      (assign-to-temporary
       (rtl:make-cons-pointer (rtl:make-constant type-code:cell) free)
       scfg-append!
       (lambda (temporary)
	 (expression-simplify expression scfg-append!
	   (lambda (expression)
	     (scfg-append!
	      (%make-assign (rtl:make-post-increment free 1) expression)
	      (receiver temporary)))))))))

(define-expression-method 'FETCH
  (lambda (receiver scfg-append! locative)
    (locative-dereference locative scfg-append!
      receiver
      (lambda (register offset)
	(receiver (rtl:make-offset register offset))))))

(define-expression-method 'TYPED-CONS:PAIR
  (lambda (receiver scfg-append! type car cdr)
    (let ((free (interpreter-free-pointer)))
      (let ((target (rtl:make-post-increment free 1)))
	(expression-simplify type scfg-append!
	  (lambda (type)
	    (assign-to-temporary (rtl:make-cons-pointer type free) scfg-append!
	      (lambda (temporary)
		(expression-simplify car scfg-append!
		  (lambda (car)
		    (scfg-append!
		     (%make-assign target car)
		     (expression-simplify cdr scfg-append!
		       (lambda (cdr)
			 (scfg-append! (%make-assign target cdr)
				       (receiver temporary)))))))))))))))

(define-expression-method 'OBJECT->TYPE
  (lambda (receiver scfg-append! expression)
    (expression-simplify expression scfg-append!
      (lambda (expression)
	(receiver (rtl:make-object->type expression))))))

(define-expression-method 'CONS-POINTER
  (lambda (receiver scfg-append! type datum)
    (expression-simplify type scfg-append!
      (lambda (type)
	(expression-simplify datum scfg-append!
	  (lambda (datum)
	    (receiver (rtl:make-cons-pointer type datum))))))))

;;; end EXPRESSION-SIMPLIFY package
)