#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlcon.scm,v 4.5 1988/03/14 21:04:25 jinx Exp $

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
	  (if (and (rtl:pseudo-register-expression? address)
		   (rtl:address-valued-expression? expression))
	      ;; We don't know for sure that this register is assigned
	      ;; only once.  However, if it is assigned multiple
	      ;; times, then all of those assignments should be
	      ;; address valued expressions.  This constraint is not
	      ;; enforced.
	      (add-rgraph-address-register! *current-rgraph*
					    (rtl:register-number address)))
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

(define (rtl:make-pop locative)
  (locative-dereference-for-statement locative
    (lambda (locative)
      (%make-assign locative (stack-pop-address)))))

(define (rtl:make-push expression)
  (expression-simplify-for-statement expression
    (lambda (expression)
      (%make-assign (stack-push-address) expression))))

(define-integrable (rtl:make-address->environment address)
  (rtl:make-cons-pointer (rtl:make-constant (ucode-type stack-environment))
			 address))

(define-integrable (rtl:make-push-return continuation)
  (rtl:make-push (rtl:make-entry:continuation continuation)))

(define (rtl:make-push-link)
  (rtl:make-push
   (rtl:make-cons-pointer (rtl:make-constant (ucode-type stack-environment))
			  (rtl:make-fetch register:dynamic-link))))

(define (rtl:make-pop-link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-object->address (stack-pop-address))))

(define (rtl:make-stack-pointer->link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-fetch register:stack-pointer)))

(define (rtl:make-link->stack-pointer)
  (rtl:make-assignment register:stack-pointer
		       (rtl:make-fetch register:dynamic-link)))

;;; Interpreter Calls

(define rtl:make-interpreter-call:access)
(define rtl:make-interpreter-call:unassigned?)
(define rtl:make-interpreter-call:unbound?)
(let ((interpreter-lookup-maker
       (lambda (%make)
	 (lambda (environment name)
	   (expression-simplify-for-statement environment
	     (lambda (environment)
	       (%make environment name)))))))
  (set! rtl:make-interpreter-call:access
	(interpreter-lookup-maker %make-interpreter-call:access))
  (set! rtl:make-interpreter-call:unassigned?
	(interpreter-lookup-maker %make-interpreter-call:unassigned?))
  (set! rtl:make-interpreter-call:unbound?
	(interpreter-lookup-maker %make-interpreter-call:unbound?)))

(define rtl:make-interpreter-call:define)
(define rtl:make-interpreter-call:set!)
(let ((interpreter-assignment-maker
       (lambda (%make)
	 (lambda (environment name value)
	   (expression-simplify-for-statement value
	     (lambda (value)
	       (expression-simplify-for-statement environment
		 (lambda (environment)
		   (%make environment name value)))))))))
  (set! rtl:make-interpreter-call:define
	(interpreter-assignment-maker %make-interpreter-call:define))
  (set! rtl:make-interpreter-call:set!
	(interpreter-assignment-maker %make-interpreter-call:set!)))

(define (rtl:make-interpreter-call:lookup environment name safe?)
  (expression-simplify-for-statement environment
    (lambda (environment)
      (%make-interpreter-call:lookup environment name safe?))))

(define (rtl:make-interpreter-call:cache-assignment name value)
  (expression-simplify-for-statement name
    (lambda (name)
      (expression-simplify-for-statement value
	(lambda (value)
	  (%make-interpreter-call:cache-assignment name value))))))

(define (rtl:make-interpreter-call:cache-reference name safe?)
  (expression-simplify-for-statement name
    (lambda (name)
      (%make-interpreter-call:cache-reference name safe?))))

(define (rtl:make-interpreter-call:cache-unassigned? name)
  (expression-simplify-for-statement name
    (lambda (name)
      (%make-interpreter-call:cache-unassigned? name))))

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
	((pair? locative)
	 (case (car locative)
	   ((REGISTER)
	    (if-register locative))
	   ((FETCH)
	    (locative-fetch (cadr locative) 0 scfg-append! if-memory))
	   ((OFFSET)
	    (let ((fetch (cadr locative)))
	      (if (and (pair? fetch) (eq? (car fetch) 'FETCH))
		  (locative-fetch (cadr fetch)
				  (caddr locative)
				  scfg-append!
				  if-memory)
		  (error "LOCATIVE-DEREFERENCE: Bad OFFSET" locative))))
	   ((CONSTANT)
	    (assign-to-temporary locative scfg-append!
	      (lambda (register)
		(assign-to-address-temporary register scfg-append!
		  (lambda (register)
		    (if-memory register 0))))))
	   (else
	    (error "LOCATIVE-DEREFERENCE: Unknown keyword" (car locative)))))
	(else
	 (error "LOCATIVE-DEREFERENCE: Illegal locative" locative))))

(define (locative-fetch locative offset scfg-append! receiver)
  (let ((receiver
	 (lambda (register)
	   (guarantee-address register scfg-append!
	     (lambda (address)
	       (receiver address offset))))))
    (locative-dereference locative scfg-append!
      receiver
      (lambda (register offset)
	(assign-to-temporary (rtl:make-offset register offset)
			     scfg-append!
			     receiver)))))

(define (locative-fetch-1 locative offset scfg-append! receiver)
  (locative-dereference locative scfg-append!
    (lambda (register)
      (receiver register offset))
    (lambda (register offset*)
      (receiver (rtl:make-offset register offset*) offset))))

(define (guarantee-address expression scfg-append! receiver)
  (if (rtl:address-valued-expression? expression)
      (receiver expression)
      (guarantee-register expression scfg-append!
	(lambda (register)
	  (assign-to-address-temporary register scfg-append! receiver)))))

(define (guarantee-register expression scfg-append! receiver)
  (if (rtl:register? expression)
      (receiver expression)
      (assign-to-temporary expression scfg-append! receiver)))

(define (generate-offset-address expression offset scfg-append! receiver)
  (guarantee-address expression scfg-append!
    (lambda (address)
      (guarantee-register address scfg-append!
	(lambda (register)
	  (receiver (rtl:make-offset-address register offset)))))))

(define-export (expression-simplify-for-statement expression receiver)
  (expression-simplify expression scfg*scfg->scfg! receiver))

(define-export (expression-simplify-for-predicate expression receiver)
  (expression-simplify expression scfg*pcfg->pcfg! receiver))

(define (expression-simplify* expression scfg-append! receiver)
  (expression-simplify expression
		       scfg-append!
		       (expression-receiver scfg-append! receiver)))

(define ((expression-receiver scfg-append! receiver) expression)
  (if (rtl:trivial-expression? expression)
      (receiver expression)
      (assign-to-temporary expression scfg-append! receiver)))

(define (expression-simplify expression scfg-append! receiver)
  (let ((entry (assq (car expression) expression-methods)))
    (if entry
	(apply (cdr entry) receiver scfg-append! (cdr expression))
	(receiver expression))))

(define (assign-to-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (if (rtl:address-valued-expression? expression)
	(add-rgraph-address-register! *current-rgraph*
				      (rtl:register-number pseudo)))
    (scfg-append! (%make-assign pseudo expression) (receiver pseudo))))

(define (assign-to-address-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (add-rgraph-address-register! *current-rgraph*
				  (rtl:register-number pseudo))
    (scfg-append! (%make-assign pseudo (rtl:make-object->address expression))
		  (receiver pseudo))))

(define (define-expression-method name method)
  (let ((entry (assq name expression-methods)))
    (if entry
	(set-cdr! entry method)
	(set! expression-methods
	      (cons (cons name method) expression-methods)))))

(define expression-methods
  '())

(define (address-method generator)
  (lambda (receiver scfg-append! locative)
    (locative-dereference-1 locative scfg-append! locative-fetch-1
      (lambda (register)
	(error "Can't take ADDRESS of a register" locative))
      (generator receiver scfg-append!))))

(define-expression-method 'ADDRESS
  (address-method
   (lambda (receiver scfg-append!)
     (lambda (expression offset)
       (if (zero? offset)
	   (guarantee-address expression scfg-append! receiver)
	   (generate-offset-address expression
				    offset
				    scfg-append!
				    receiver))))))

(define-expression-method 'CELL-CONS
  (lambda (receiver scfg-append! expression)
    (expression-simplify* expression scfg-append!
      (lambda (expression)
	(let ((free (interpreter-free-pointer)))
	  (assign-to-temporary
	   (rtl:make-cons-pointer (rtl:make-constant type-code:cell) free)
	   scfg-append!
	   (lambda (temporary)
	     (scfg-append!
	      (%make-assign (rtl:make-post-increment free 1) expression)
	      (receiver temporary)))))))))

(define-expression-method 'ENVIRONMENT
  (address-method
   (lambda (receiver scfg-append!)
     (lambda (expression offset)
       (if (zero? offset)
	   (receiver
	    (if (rtl:address-valued-expression? expression)
		(rtl:make-address->environment expression)
		expression))
	   (generate-offset-address expression offset scfg-append!
	     (lambda (expression)
	       (assign-to-temporary expression scfg-append!
		 (lambda (register)
		  (receiver (rtl:make-address->environment register)))))))))))

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
	(expression-simplify* type scfg-append!
	  (lambda (type)
	    (expression-simplify* car scfg-append!
	      (lambda (car)
		 (expression-simplify* cdr scfg-append!
		   (lambda (cdr)
		     (assign-to-temporary (rtl:make-cons-pointer type free)
					  scfg-append!
		       (lambda (temporary)
			 (scfg-append!
			  (%make-assign target car)
			  (scfg-append! (%make-assign target cdr)
					(receiver temporary)))))))))))))))

(define-expression-method 'TYPED-CONS:VECTOR
  (lambda (receiver scfg-append! type . elements)
    (let ((free (interpreter-free-pointer))
	  (header
	   (rtl:make-cons-pointer
	    (rtl:make-constant (ucode-type manifest-vector))
	    (rtl:make-constant (length elements)))))
      (let ((target (rtl:make-post-increment free 1)))
	(expression-simplify* type scfg-append!
	  (lambda (type)
	    (let loop ((elements elements) (simplified-elements '()))
	      (if (null? elements)
		  (assign-to-temporary (rtl:make-cons-pointer type free)
				       scfg-append!
		    (lambda (temporary)
		      (scfg-append!
		       (%make-assign target header)
		       (let loop ((elements (reverse! simplified-elements)))
			 (if (null? elements)
			     (receiver temporary)
			     (scfg-append! (%make-assign target (car elements))
					   (loop (cdr elements))))))))
		  (expression-simplify* (car elements) scfg-append!
		    (lambda (element)
		      (loop (cdr elements)
			    (cons element simplified-elements))))))))))))

;; A NOP for simplification

(define-expression-method 'TYPED-CONS:PROCEDURE
  (lambda (receiver scfg-append! type entry min max size)
    (receiver (rtl:make-typed-cons:procedure type entry min max size))))

(define (object-selector make-object-selector)
  (lambda (receiver scfg-append! expression)
    (expression-simplify* expression scfg-append!
      (lambda (expression)
	(receiver (make-object-selector expression))))))

(define-expression-method 'OBJECT->TYPE
  (object-selector rtl:make-object->type))

(define-expression-method 'OBJECT->DATUM
  (object-selector rtl:make-object->datum))

(define-expression-method 'OBJECT->ADDRESS
  (object-selector rtl:make-object->address))

(define-expression-method 'CONS-POINTER
  (lambda (receiver scfg-append! type datum)
    (expression-simplify* type scfg-append!
      (lambda (type)
	(expression-simplify* datum scfg-append!
	  (lambda (datum)
	    (receiver (rtl:make-cons-pointer type datum))))))))

;;; end EXPRESSION-SIMPLIFY package
)