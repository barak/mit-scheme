#| -*-Scheme-*-

$Id: triveval.scm,v 1.1 1994/11/19 02:04:29 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; "Trivial" KMP Scheme evaluator
;;; package: (compiler midend)

(declare (usual-integrations))

;;;; Trivial evaluator's runtime library

;; New special forms handled as procedures

(define (lookup value)
  value)

(define (call operator cont . operands)
  (if (eq? operator %invoke-continuation)
      (apply cont operands)
      (let ((rator (operator->procedure operator)))
	(cond ((cps-proc? rator)
	       (cps-proc/apply rator cont operands))
	      ((not cont)
	       (apply rator operands))
	      ((continuation? cont)
	       (within-continuation cont
				    (lambda ()
				      (apply rator operands))))
	      (else
	       (cont (apply rator operands)))))))

(define-structure (cps-proc
		   (conc-name cps-proc/)
		   (constructor %cps-proc/make%))
  (handler false read-only true))

(define (cps-proc/apply proc cont operands)
  ;; if cont is false, proc should not need it
  #|
  (if (not cont)
      (apply proc operands)
      (apply (cps-proc/handler proc) cont operands))
  |#
  (apply (cps-proc/handler proc) cont operands))

(define (funcall nargs operator . operands)
  nargs					; ignored
  (apply operator operands))

(define *last-env*)
(define *this-env* (the-environment))

(define (fetch-environment)
  (let ((env *last-env*))
    (set! *last-env*)
    env))

(define (execute expr env)
  (set! *last-env* env)
  (eval (cond ((cps-program1? expr)
	       (cps-rewrite (caddr expr)))
              ((cps-program2? expr)
               (cps-rewrite expr))
	      ((compatible-program? expr)
	       (compatible-rewrite expr))
	      (else
	       (pre-cps-rewrite expr)))
	*this-env*))

(define (pre-cps-rewrite expr)
  `(let-syntax ((NON-CPS-LAMBDA
		 (macro (param-list body)
		   (list 'LAMBDA (cdr param-list) body))))
     ,(form/replace expr '((LAMBDA NON-CPS-LAMBDA)))))

(define triveval/?cont-variable (->pattern-variable 'CONT-VARIABLE))
(define triveval/?body (->pattern-variable 'BODY))
(define triveval/?ignore (->pattern-variable 'IGNORE))
(define triveval/?frame (->pattern-variable 'FRAME))
(define triveval/?frame-vector (->pattern-variable 'FRAME-VECTOR))

(define triveval/compatible-expr-pattern
  `(LAMBDA (,triveval/?ignore)
     (LET ((,triveval/?frame
	    (CALL (QUOTE ,%fetch-stack-closure)
		  (QUOTE #F)
		  (QUOTE ,triveval/?frame-vector))))
       ,triveval/?body)))

(define (compatible-program? expr)
  (form/match triveval/compatible-expr-pattern expr))

(define (compatible-rewrite expr)
  (let ((expr* (%cps-rewrite (caddr expr)))
	(name (generate-uninterned-symbol 'CONT)))
    `(call-with-current-continuation
      (lambda (,name)
	(set! *stack-closure* (make-stack-closure false '() ,name))
	,expr*))))

;;this no longer appears to be the only correct pattern, a (letrec () appears
;;before this let, so I just make two tests, and do the appropriate thing
;;JBANK

(define triveval/cps-expr-pattern1
  `(LETREC ()
     (LET ((,triveval/?cont-variable
            (CALL (QUOTE ,%fetch-continuation)
                  (QUOTE #F))))
       ,triveval/?body)))

(define triveval/cps-expr-pattern1-2
  `(LET ()
     (LET ((,triveval/?cont-variable
            (CALL (QUOTE ,%fetch-continuation)
                  (QUOTE #F))))
       ,triveval/?body)))

(define triveval/cps-expr-pattern2
  `(LET ((,triveval/?cont-variable
            (CALL (QUOTE ,%fetch-continuation)
                  (QUOTE #F))))
       ,triveval/?body))

(define (cps-program1? expr)
  (or (form/match triveval/cps-expr-pattern1  expr)
      (form/match triveval/cps-expr-pattern1-2  expr)))

(define (cps-program2? expr)
  (form/match triveval/cps-expr-pattern2 expr))

(define (%cps-rewrite expr)
  `(let-syntax ((cps-lambda
		 (macro (param-list body)
		   (list '%cps-proc/make%
			 (list 'LAMBDA param-list body)))))
     ,(form/replace expr '((LAMBDA CPS-LAMBDA)))))

(define (cps-rewrite expr)
  `(call-with-current-continuation
    (lambda (,(car (car (cadr expr))))	; cont variable
      ,(%cps-rewrite (caddr expr)))))
  
(define-structure (variable-cache
		   (conc-name variable-cache/)
		   (constructor variable-cache/make))
  env name)

(define (make-read-variable-cache env name)
  (variable-cache/make env name))

(define (make-write-variable-cache env name)
  (variable-cache/make env name))

(define (variable-cache-ref cache name)
  name					; ignored
  (lexical-reference (variable-cache/env cache)
		     (variable-cache/name cache)))

(define (variable-cache-set! cache value name)
  name					; ignored
  (lexical-assignment (variable-cache/env cache)
		      (variable-cache/name cache)
		      value))

(define (safe-variable-cache-ref cache name)
  name					; ignored
  (let ((env (variable-cache/env cache))
	(name (variable-cache/name cache)))
    (if (lexical-unassigned? env name)
	%unassigned
	(lexical-reference env name))))

(define (variable-cell-ref cache)
  (let ((env (variable-cache/env cache))
	(name (variable-cache/name cache)))
    (if (lexical-unassigned? env name)
	%unassigned
	(lexical-reference env name))))

(define (variable-cell-set! cache value)
  (lexical-assignment (variable-cache/env cache)
		      (variable-cache/name cache)
		      value))

(define-structure (operator-cache
		   (conc-name operator-cache/)
		   (constructor operator-cache/make))
  env name arity)

(define (make-operator-variable-cache env name arity)
  (operator-cache/make env name arity))

(define (make-remote-operator-variable-cache package name arity)
  (operator-cache/make (->environment package) name arity))

(define (invoke-operator-cache name cache . args)
  name					; ignored
  (let ((arity (operator-cache/arity cache)))
    (if (not (= (length args) arity))
	(error "Operator cache called with wrong number of arguments"
	       args arity)
	(apply (lexical-reference (operator-cache/env cache)
				  (operator-cache/name cache))
	       args))))
  
(define (cell/make value name)
  name					; ignored
  (make-cell value))

(define (cell-ref cell name)
  name					; ignored
  (cell-contents cell))

(define (cell-set! cell value name)
  name					; ignored
  (set-cell-contents! cell value))

(define (make-closure proc names . values)
  names					; ignored
  (make-entity proc (list->vector values)))

(define (closure-ref closure index name)
  name					; ignored
  (vector-ref (entity-extra closure) index))

(define (closure-set! closure index value name)
  name					; ignored
  (vector-set! (entity-extra closure) index value))

(define *stack-closure*)

(define (fetch-stack-closure names)
  names					; ignored
  (let ((closure *stack-closure*))
    (set! *stack-closure*)		; clear for gc
    closure))

(define (make-stack-closure proc names . values)
  names					; ignored
  (make-entity (lambda (closure . args)
		 (set! *stack-closure* closure)
		 (apply proc args))
	       (list->vector values)))

(define (stack-closure-ref closure index name)
  name					; ignored
  (vector-ref (entity-extra closure) index))

(define (projection/2/0 x y)
  y					; ignored
  x)

(define (%unknown . all)
  all					; ignored
  (error "Unknown operator"))

;; *** These two do not currently work for #!optional or #!rest! ***

(define (make-closure/compatible proc names . values)
  (let ((proc (cps-proc/handler proc)))
    (apply make-closure
	   (lambda (closure . args)
	     (call-with-current-continuation
	      (lambda (cont)
		(set! *stack-closure*
		      (apply make-stack-closure
			     false
			     '()
			     (cons cont
				   (append (reverse args)
					   (list closure)))))
		(apply proc (cons* cont closure args)))))
	   names
	   values)))

(define *trivial-closures*		; to preserve eq-ness
  (make-eq-hash-table))

(define (make-trivial-closure/compatible proc)
  (let ((proc (cps-proc/handler proc)))
    (or (hash-table/get *trivial-closures* proc false)
	(let ((new
	       (lambda args
		 (call-with-current-continuation
		  (lambda (cont)
		    (set! *stack-closure*
			  (apply make-stack-closure
				 false
				 '()
				 (cons cont (reverse args))))
		    (apply proc (cons cont args)))))))
	  (hash-table/put! *trivial-closures* proc new)
	  new))))

(define internal-apply/compatible
  (%cps-proc/make%
   (lambda (stack-closure nargs operator)
     nargs				; ignored
     (let ((elements (vector->list (entity-extra stack-closure))))
       (apply call
	      operator
	      (car elements)
	      (reverse (cdr elements)))))))

(define invoke-operator-cache/compatible
  (%cps-proc/make%
   (lambda (stack-closure desc cache)
     (let ((elements (vector->list (entity-extra stack-closure))))
       (apply call
	      (let ((cache
		     (or cache
			 (make-remote-operator-variable-cache
			  '()
			  (car desc)
			  (cadr desc)))))
		(lexical-reference (operator-cache/env cache)
				   (operator-cache/name cache)))
	      (car elements)
	      (reverse (cdr elements)))))))

(define *operator->procedure*
  (make-eq-hash-table 311))

(define (operator->procedure rator)
  (if (not (symbol? rator))
      rator
      (hash-table/get *operator->procedure* rator rator)))

(define (init-operators!)
  (let* ((table *operator->procedure*)
	 (declare-operator
	  (lambda (token handler)
	    (hash-table/put! table token handler))))

    (declare-operator %invoke-operator-cache invoke-operator-cache)
    (declare-operator %invoke-remote-cache invoke-operator-cache)
    (declare-operator %variable-cache-ref variable-cache-ref)
    (declare-operator %variable-cache-set! variable-cache-set!)
    (declare-operator %safe-variable-cache-ref safe-variable-cache-ref)
    (declare-operator %unassigned? (lambda (obj) (eq? obj %unassigned)))
    (declare-operator %make-promise (lambda (proc) (delay (proc))))
    (declare-operator %make-cell cell/make)
    (declare-operator %make-static-binding cell/make)
    (declare-operator %cell-ref cell-ref)
    (declare-operator %static-binding-ref cell-ref)
    (declare-operator %cell-set! cell-set!)
    (declare-operator %static-binding-set! cell-set!)
    (declare-operator %cons cons)
    (declare-operator %vector vector)
    (declare-operator %*lookup
		      (lambda (env name depth offset)
			depth offset	; ignored
			(lexical-reference env name)))
    (declare-operator %*set!
		      (lambda (env name depth offset value)
			depth offset	; ignored
			(lexical-assignment env name value)))
    (declare-operator %*unassigned?
		      (lambda (env name depth offset)
			depth offset	; ignored
			(lexical-unassigned? env name)))
    (declare-operator %*define local-assignment)
    (declare-operator %*define* define-multiple)
    (declare-operator %*make-environment *make-environment)
    (declare-operator %execute execute)
    (declare-operator %fetch-environment fetch-environment)
    (declare-operator %fetch-continuation
		      (lambda ()
			(error "Fetch-continuation executed!")))
    (declare-operator %make-read-variable-cache make-read-variable-cache)
    (declare-operator %make-write-variable-cache make-write-variable-cache)
    (declare-operator %make-operator-variable-cache
		      make-operator-variable-cache)
    (declare-operator %make-remote-operator-variable-cache
		      make-remote-operator-variable-cache)
    (declare-operator %copy-program %copy-program)
    (declare-operator %make-heap-closure make-closure)
    (declare-operator %make-trivial-closure identity-procedure)
    (declare-operator %heap-closure-ref closure-ref)
    (declare-operator %heap-closure-set! closure-set!)
    (declare-operator %make-stack-closure make-stack-closure)
    (declare-operator %stack-closure-ref stack-closure-ref)
    (declare-operator %fetch-stack-closure fetch-stack-closure)
    (declare-operator %internal-apply funcall)
    (declare-operator %primitive-apply funcall)
    ; (declare-operator %invoke-continuation identity-procedure)
    (declare-operator %vector-index vector-index)

    (declare-operator %machine-fixnum? machine-fixnum?)
    (declare-operator %small-fixnum? small-fixnum?)
    (declare-operator %+ +)
    (declare-operator %- -)
    (declare-operator %* *)
    (declare-operator %/ /)
    (declare-operator %quotient quotient)
    (declare-operator %remainder remainder)
    (declare-operator %= =)
    (declare-operator %< <)
    (declare-operator %> >)
    (declare-operator %vector-cons make-vector)
    (declare-operator %string-allocate string-allocate)
    (declare-operator %floating-vector-cons flo:vector-cons)

    ;; Compatiblity operators:

    (declare-operator %make-return-address
		      (lambda (obj)
			obj		; ignored
			(error "make-return-address executed!")))

    (declare-operator %variable-read-cache projection/2/0)
    (declare-operator %variable-write-cache projection/2/0)
    (declare-operator %variable-cell-ref variable-cell-ref)
    (declare-operator %hook-variable-cell-ref variable-cell-ref)
    (declare-operator %hook-safe-variable-cell-ref variable-cell-ref)
    (declare-operator %variable-cell-set! variable-cell-set!)
    (declare-operator %hook-variable-cell-set! variable-cell-set!)
    (declare-operator %reference-trap? (lambda (obj) (eq? obj %unassigned)))
    (declare-operator %primitive-apply/compatible internal-apply/compatible)))

;; This makes cps procs and ordinary procs intermixable

(set-record-type-application-method!
 cps-proc
 (lambda (the-proc . args)
   (call-with-current-continuation
    (lambda (cont)
      (apply (cps-proc/handler the-proc) cont args)))))

(init-operators!)