#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlcon.scm,v 4.21 1990/05/03 15:10:19 jinx Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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
;;; package: (compiler)

(declare (usual-integrations))

;;;; Statements

(define (rtl:make-assignment locative expression)
  (locative-dereference-for-statement locative
    (lambda (locative)
      (let ((receiver
	     (lambda (expression)
	       (rtl:make-assignment-internal locative expression))))
	(if (rtl:pseudo-register-expression? locative)
	    (expression-simplify-for-pseudo-assignment expression receiver)
	    (expression-simplify-for-statement expression receiver))))))

(define (rtl:make-assignment-internal locative expression)
  (cond ((and (or (rtl:register? locative) (rtl:offset? locative))
	      (equal? locative expression))
	 (make-null-cfg))
	((or (rtl:register? locative) (rtl:register? expression))
	 (%make-assign locative expression))
	(else
	 (let ((register (rtl:make-pseudo-register)))
	   (scfg*scfg->scfg! (%make-assign register expression)
			     (%make-assign locative register))))))

(define (rtl:make-pop locative)
  (locative-dereference-for-statement locative
    (lambda (locative)
      (rtl:make-assignment-internal locative (stack-pop-address)))))

(define (rtl:make-push expression)
  (expression-simplify-for-statement expression
    (lambda (expression)
      (rtl:make-assignment-internal (stack-push-address) expression))))

(define (rtl:make-eq-test expression-1 expression-2)
  (expression-simplify-for-predicate expression-1
    (lambda (expression-1)
      (expression-simplify-for-predicate expression-2
	(lambda (expression-2)
	  (%make-eq-test expression-1 expression-2))))))

(define (rtl:make-false-test expression)
  (rtl:make-eq-test expression (rtl:make-constant false)))

(define (rtl:make-true-test expression)
  (pcfg-invert (rtl:make-false-test expression)))

(define (rtl:make-type-test expression type)
  (expression-simplify-for-predicate expression
    (lambda (expression)
      (%make-type-test expression type))))

(define (rtl:make-unassigned-test expression)
  (rtl:make-eq-test
   expression
   (rtl:make-cons-pointer (rtl:make-machine-constant (ucode-type unassigned))
			  (rtl:make-machine-constant 0))))

(define (rtl:make-fixnum-pred-1-arg predicate operand)
  (expression-simplify-for-predicate operand
    (lambda (operand)
      (%make-fixnum-pred-1-arg predicate operand))))

(define (rtl:make-fixnum-pred-2-args predicate operand1 operand2)
  (expression-simplify-for-predicate operand1
    (lambda (operand1)
      (expression-simplify-for-predicate operand2
	(lambda (operand2)
	  (%make-fixnum-pred-2-args predicate operand1 operand2))))))

(define (rtl:make-flonum-pred-1-arg predicate operand)
  (expression-simplify-for-predicate operand
    (lambda (operand)
      (%make-flonum-pred-1-arg predicate operand))))

(define (rtl:make-flonum-pred-2-args predicate operand1 operand2)
  (expression-simplify-for-predicate operand1
    (lambda (operand1)
      (expression-simplify-for-predicate operand2
	(lambda (operand2)
	  (%make-flonum-pred-2-args predicate operand1 operand2))))))

(define (rtl:make-push-return continuation)
  (rtl:make-push
   (rtl:make-cons-pointer (rtl:make-machine-constant type-code:compiled-entry)
			  (rtl:make-entry:continuation continuation))))

(define (rtl:make-push-link)
  (rtl:make-push
   (rtl:make-environment (rtl:make-fetch register:dynamic-link))))

(define (rtl:make-pop-link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-object->address (stack-pop-address))))

(define (rtl:make-stack-pointer->link)
  (rtl:make-assignment register:dynamic-link
		       (rtl:make-fetch register:stack-pointer)))

(define (rtl:make-link->stack-pointer)
  (rtl:make-assignment register:stack-pointer
		       (rtl:make-fetch register:dynamic-link)))

(define (rtl:make-constant value)
  (if (unassigned-reference-trap? value)
      (rtl:make-cons-pointer
       (rtl:make-machine-constant type-code:unassigned)
       (rtl:make-machine-constant 0))
      (%make-constant value)))
 
(define make-non-pointer-literal
  (let ((type-maximum (expt 2 scheme-type-width))
	(type-scale-factor (expt 2 scheme-datum-width)))
    (lambda (type datum)
      (if (not (and (exact-nonnegative-integer? type)
		    (< type type-maximum)))
	  (error "non-pointer type out of range" type))
      (if (not (and (exact-nonnegative-integer? datum)
		    (< datum type-scale-factor)))
	  (error "non-pointer datum out of range" datum))
      (+ (* type type-scale-factor) datum))))

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
	  expression-simplify-for-predicate
	  expression-simplify-for-pseudo-assignment)

(define-export (locative-dereference-for-statement locative receiver)
  (locative-dereference locative scfg*scfg->scfg!
    receiver
    (lambda (register offset granularity)
      (receiver (make-offset register offset granularity)))))

(define-export (expression-simplify-for-statement expression receiver)
  (expression-simplify expression scfg*scfg->scfg! receiver))

(define-export (expression-simplify-for-predicate expression receiver)
  (expression-simplify expression scfg*pcfg->pcfg! receiver))

(define-export (expression-simplify-for-pseudo-assignment expression receiver)
  (let ((entry (assq (car expression) expression-methods)))
    (if entry
	(apply (cdr entry) receiver scfg*scfg->scfg! (cdr expression))
	(receiver expression))))

(define (expression-simplify expression scfg-append! receiver)
  (if (rtl:register? expression)
      (receiver expression)
      (let ((entry (assq (car expression) expression-methods)))
	(if entry
	    (apply (cdr entry)
		   (lambda (expression)
		     (if (rtl:register? expression)
			 (receiver expression)
			 (assign-to-temporary expression
					      scfg-append!
					      receiver)))
		   scfg-append!
		   (cdr expression))
	    (assign-to-temporary expression scfg-append! receiver)))))

(define (assign-to-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (scfg-append! (rtl:make-assignment-internal pseudo expression)
		  (receiver pseudo))))

(define (make-offset register offset granularity)
  (case granularity
    ((OBJECT) (rtl:make-offset register offset))
    ((BYTE) (rtl:make-byte-offset register offset))
    (else (error "unknown offset granularity" granularity))))

(define (locative-dereference locative scfg-append! if-register if-memory)
  (let ((dereference-fetch
	 (lambda (locative offset granularity)
	   (let ((if-address
		  (lambda (address)
		    (if-memory address offset granularity))))
	     (let ((if-not-address
		    (lambda (register)
		      (assign-to-address-temporary register
						   scfg-append!
						   if-address))))
	       (locative-dereference (cadr locative) scfg-append!
		 (lambda (expression)
		   (let ((register (rtl:register-number expression)))
		     (if (and (machine-register? register)
			      (register-value-class=address? register))
			 (if-address expression)
			 (if-not-address expression))))
		 (lambda (register offset granularity)
		   (assign-to-temporary
		    (make-offset register offset granularity)
		    scfg-append!
		    if-not-address)))))))
	(dereference-constant
	 (lambda (locative offset granularity)
	   (assign-to-temporary locative scfg-append!
	     (lambda (register)
	       (assign-to-address-temporary register scfg-append!
		 (lambda (register)
		   (if-memory register offset granularity))))))))
    (cond ((symbol? locative)
	   (let ((register (rtl:machine-register? locative)))
	     (if register
		 (if-register register)
		 (if-memory (interpreter-regs-pointer)
			    (rtl:interpreter-register->offset locative)
			    'OBJECT))))
	  ((pair? locative)
	   (case (car locative)
	     ((REGISTER)
	      (if-register locative))
	     ((FETCH)
	      (dereference-fetch locative 0 'OBJECT))
	     ((OFFSET)
	      (let ((base (rtl:locative-offset-base locative))
		    (offset (rtl:locative-offset-offset locative))
		    (granularity (rtl:locative-offset-granularity locative)))
		(if (not (pair? base))
		    (error "offset base not pair" locative))
		(case (car base)
		  ((FETCH)
		   (dereference-fetch base offset granularity))
		  ((CONSTANT)
		   (dereference-constant base offset granularity))
		  (else
		   (error "illegal offset base" locative)))))
	     ((CONSTANT)
	      (dereference-constant locative 0 'OBJECT))
	     (else
	      (error "unknown keyword" locative))))
	  (else
	   (error "illegal locative" locative)))))

(define (assign-to-address-temporary expression scfg-append! receiver)
  (let ((pseudo (rtl:make-pseudo-register)))
    (scfg-append!
     (rtl:make-assignment-internal pseudo
				   (rtl:make-object->address expression))
     (receiver pseudo))))

(define (define-expression-method name method)
  (let ((entry (assq name expression-methods)))
    (if entry
	(set-cdr! entry method)
	(set! expression-methods
	      (cons (cons name method) expression-methods))))
  name)

(define expression-methods
  '())

(define-expression-method 'FETCH
  (lambda (receiver scfg-append! locative)
    (locative-dereference locative scfg-append!
      receiver
      (lambda (register offset granularity)
	(receiver (make-offset register offset granularity))))))

(define (address-method generator)
  (lambda (receiver scfg-append! locative)
    (locative-dereference locative scfg-append!
      (lambda (register)
	register
	(error "Can't take ADDRESS of a register" locative))
      (generator receiver scfg-append!))))

(define-expression-method 'ADDRESS
  (address-method
   (lambda (receiver scfg-append!)
     scfg-append!			;ignore
     (lambda (address offset granularity)
       (if (not (eq? granularity 'OBJECT))
	   (error "can't take address of non-object offset" granularity))
       (if (zero? offset)
	   (receiver address)
	   (receiver (rtl:make-offset-address address offset)))))))

(define-expression-method 'ENVIRONMENT
  (address-method
   (lambda (receiver scfg-append!)
     (lambda (address offset granularity)
       (if (not (eq? granularity 'OBJECT))
	   (error "can't take address of non-object offset" granularity))
       (let ((receiver
	      (lambda (address)
		(expression-simplify
		 (rtl:make-cons-pointer
		  (rtl:make-machine-constant (ucode-type stack-environment))
		  address)
		 scfg-append!
		 receiver))))
	 (if (zero? offset)
	     (receiver address)
	     (assign-to-temporary (rtl:make-offset-address address offset)
				  scfg-append!
				  receiver)))))))

(define-expression-method 'CONS-POINTER
  (lambda (receiver scfg-append! type datum)
    (expression-simplify type scfg-append!
      (lambda (type)
	(expression-simplify datum scfg-append!
	  (lambda (datum)
	    (receiver (rtl:make-cons-pointer type datum))))))))

(define-expression-method 'CELL-CONS
  (lambda (receiver scfg-append! expression)
    (expression-simplify expression scfg-append!
      (lambda (expression)
	(let ((free (interpreter-free-pointer)))
	  (expression-simplify
	   (rtl:make-cons-pointer (rtl:make-machine-constant type-code:cell)
				  free)
	   scfg-append!
	   (lambda (temporary)
	     (scfg-append!
	      (rtl:make-assignment-internal (rtl:make-post-increment free 1)
					    expression)
	      (receiver temporary)))))))))

(define-expression-method 'TYPED-CONS:PAIR
  (lambda (receiver scfg-append! type car cdr)
    (let ((free (interpreter-free-pointer)))
      (let ((target (rtl:make-post-increment free 1)))
	(expression-simplify type scfg-append!
	  (lambda (type)
	    (expression-simplify car scfg-append!
	      (lambda (car)
		 (expression-simplify cdr scfg-append!
		   (lambda (cdr)
		     (assign-to-temporary (rtl:make-cons-pointer type free)
					  scfg-append!
		       (lambda (temporary)
			 (scfg-append!
			  (rtl:make-assignment-internal target car)
			  (scfg-append!
			   (rtl:make-assignment-internal target cdr)
			   (receiver temporary)))))))))))))))

(define-expression-method 'TYPED-CONS:VECTOR
  (lambda (receiver scfg-append! type . elements)
    (let* ((free (interpreter-free-pointer))
	   (target (rtl:make-post-increment free 1)))
      (expression-simplify type scfg-append!
	(lambda (type)
	  (let loop ((elements* elements) (simplified-elements '()))
	    (if (null? elements*)
		(assign-to-temporary (rtl:make-cons-pointer type free)
				     scfg-append!
		  (lambda (temporary)
		    (expression-simplify
		     (rtl:make-cons-pointer
		      (rtl:make-machine-constant (ucode-type manifest-vector))
		      (rtl:make-machine-constant (length elements)))
		     scfg-append!
		     (lambda (header)
		       (scfg-append!
			(rtl:make-assignment-internal target header)
			(let loop ((elements (reverse! simplified-elements)))
			  (if (null? elements)
			      (receiver temporary)
			      (scfg-append!
			       (rtl:make-assignment-internal target
							     (car elements))
			       (loop (cdr elements))))))))))
		(expression-simplify (car elements*) scfg-append!
		  (lambda (element)
		    (loop (cdr elements*)
			  (cons element simplified-elements)))))))))))

(define-expression-method 'TYPED-CONS:PROCEDURE
  (lambda (receiver scfg-append! entry)
    (expression-simplify
     entry scfg-append!
     (lambda (entry)
       (receiver (rtl:make-cons-pointer
		  (rtl:make-machine-constant type-code:compiled-entry)
		  entry))))))

(define-expression-method 'BYTE-OFFSET-ADDRESS
  (lambda (receiver scfg-append! base number)
    (expression-simplify
     base scfg-append!
     (lambda (base)
       (receiver (rtl:make-byte-offset-address base number))))))

;; NOPs for simplification

(define-expression-method 'ENTRY:CONTINUATION
  (lambda (receiver scfg-append! label)
    scfg-append!			; unused
    (receiver (rtl:make-entry:continuation label))))

(define-expression-method 'ENTRY:PROCEDURE
  (lambda (receiver scfg-append! label)
    scfg-append!			; unused
    (receiver (rtl:make-entry:procedure label))))

(define-expression-method 'CONS-CLOSURE
  (lambda (receiver scfg-append! entry min max size)
    scfg-append!			; unused
    (receiver (rtl:make-cons-closure entry min max size))))

(define-expression-method 'CONS-MULTICLOSURE
  (lambda (receiver scfg-append! nentries size entries)
    scfg-append!			; unused
    (receiver (rtl:make-cons-multiclosure nentries size entries))))

(define (object-selector make-object-selector)
  (lambda (receiver scfg-append! expression)
    (expression-simplify expression scfg-append!
      (lambda (expression)
	(receiver (make-object-selector expression))))))

(define-expression-method 'OBJECT->TYPE
  (object-selector rtl:make-object->type))

(define-expression-method 'CHAR->ASCII
  (object-selector rtl:make-char->ascii))

(define-expression-method 'OBJECT->DATUM
  (object-selector rtl:make-object->datum))

(define-expression-method 'OBJECT->ADDRESS
  (object-selector rtl:make-object->address))

(define-expression-method 'FIXNUM->OBJECT
  (object-selector rtl:make-fixnum->object))

(define-expression-method 'FIXNUM->ADDRESS
  (object-selector rtl:make-fixnum->address))

(define-expression-method 'ADDRESS->FIXNUM
  (object-selector rtl:make-address->fixnum))

(define-expression-method 'OBJECT->FIXNUM
  (object-selector rtl:make-object->fixnum))

(define-expression-method 'FLOAT->OBJECT
  (object-selector rtl:make-float->object))

(define-expression-method '@ADDRESS->FLOAT
  (object-selector rtl:make-@address->float))

(define-expression-method 'FIXNUM-2-ARGS
  (lambda (receiver scfg-append! operator operand1 operand2 overflow?)
    (expression-simplify operand1 scfg-append!
      (lambda (operand1)
	(expression-simplify operand2 scfg-append!
	  (lambda (operand2)
	    (receiver
	     (rtl:make-fixnum-2-args operator
				     operand1
				     operand2
				     overflow?))))))))

(define-expression-method 'FIXNUM-1-ARG
  (lambda (receiver scfg-append! operator operand overflow?)
    (expression-simplify operand scfg-append!
      (lambda (operand)
	(receiver (rtl:make-fixnum-1-arg operator operand overflow?))))))

(define-expression-method 'FLONUM-1-ARG
  (lambda (receiver scfg-append! operator operand overflow?)
    (expression-simplify operand scfg-append!
      (lambda (s-operand)
	(receiver (rtl:make-flonum-1-arg
		   operator
		   s-operand
		   overflow?))))))

(define-expression-method 'FLONUM-2-ARGS
  (lambda (receiver scfg-append! operator operand1 operand2 overflow?)
    (expression-simplify operand1 scfg-append!
      (lambda (s-operand1)
	(expression-simplify operand2 scfg-append!
	  (lambda (s-operand2)
	    (receiver (rtl:make-flonum-2-args
		       operator
		       s-operand1
		       s-operand2
		       overflow?))))))))

;;; end EXPRESSION-SIMPLIFY package
)