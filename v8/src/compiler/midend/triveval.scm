#| -*-Scheme-*-

$Id: fb734e79b7bbdaba72a00c3aa5e9b1f476b55abd $

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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
      (call-with-values
       (lambda ()
	 (collect-operands cont operands))
       (lambda (cont operands)
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
		  (cont (apply rator operands)))))))))

(define (collect-operands cont operands)
  ;; (values cont operands)
  (if (not (stack-closure? cont))
      (values cont operands)
      (let ((proc (stack-closure/proc cont)))
	(if (or (compound-procedure? proc)
		(not proc))
	    (values cont operands)
	    (values proc
		    (append operands
			    (vector->list
			     (stack-closure/values cont))))))))

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
  (set! *stack-closure* false)
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
(define triveval/?env-variable (->pattern-variable 'ENV-VARIABLE))
(define triveval/?body (->pattern-variable 'BODY))
(define triveval/?ignore (->pattern-variable 'IGNORE))
(define triveval/?frame (->pattern-variable 'FRAME))
(define triveval/?frame-vector (->pattern-variable 'FRAME-VECTOR))

(define triveval/compatible-expr-pattern
  `(LAMBDA (,triveval/?cont-variable ,triveval/?env-variable)
     ,triveval/?body))

(define (compatible-program? expr)
  (let ((result (form/match triveval/compatible-expr-pattern expr)))
    (and result
	 (let ((cont (cadr (assq triveval/?cont-variable result)))
	       (env  (cadr (assq triveval/?env-variable result))))
	   (and (continuation-variable? cont)
		(environment-variable? env))))))

(define (compatible-rewrite expr)
  (let ((expr* (%cps-rewrite (caddr expr)))
	(cont-name (car (cadr expr)))
	(env-name (cadr (cadr expr))))
    `(call-with-current-continuation
      (lambda (,cont-name)
	(let ((,env-name *last-env*))
	  ,expr*)))))

;;this no longer appears to be the only correct pattern, a (letrec () ...)
;;appears before this let, so I just make two tests, and do the
;;appropriate thing
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
		   (call-with-values
		    (lambda ()
		      ((access lambda-list/parse
			       (->environment '(compiler midend)))
		       (cdr param-list)))
		    (lambda (required optional rest aux)
		      aux		; ignored
		      (let ((max-reg
			     ((access rtlgen/number-of-argument-registers
				      (->environment '(compiler midend)))))
			    (names
			     (append required optional (if rest
							   (list rest)
							   '()))))

			(list
			 '%cps-proc/make%
			 (list 'LAMBDA
			       param-list
			       (if (<= (length names) max-reg)
				   body
				   (let ((stack-names
					  (list-tail names max-reg)))
				     `(begin
					(set! *stack-closure*
					      (make-stack-closure
					       #f
					       '#(,@stack-names)
					       ,@stack-names))
					,body)))))))))))
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

(define (variable-cache-ref cache ignore-traps? name)
  ignore-traps? name			; ignored
  (lexical-reference (variable-cache/env cache)
		     (variable-cache/name cache)))

(define (variable-cache-set! cache value ignore-traps? name)
  ignore-traps? name			; ignored
  (lexical-assignment (variable-cache/env cache)
		      (variable-cache/name cache)
		      value))

(define (safe-variable-cache-ref cache ignore-traps? name)
  ignore-traps? name			; ignored
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

(define *stack-closure* false)

(define-structure (%stack-closure
		   (conc-name %stack-closure/)
		   (constructor %stack-closure/make))
  proc
  names
  values)

(define (fetch-stack-closure names)
  names					; ignored
  (let ((closure *stack-closure*))
    (set! *stack-closure* false)	; clear for gc
    closure))

(define (make-stack-closure proc names . values)
  (make-entity (lambda (closure . args)
		 (set! *stack-closure* closure)
		 (apply proc args))
	       (%stack-closure/make
		proc
		names
		(list->vector values))))

(define (stack-closure-ref closure index name)
  name					; ignored
  (vector-ref (%stack-closure/values (entity-extra closure)) index))

(define (stack-closure? object)
  (and (entity? object)
       (%stack-closure? (entity-extra object))))

(define (stack-closure/proc object)
  (%stack-closure/proc (entity-extra object)))

(define (stack-closure/values object)
  (%stack-closure/values (entity-extra object)))

(define (projection/2/0 x y)
  y					; ignored
  x)

(define (%unknown . all)
  all					; ignored
  (error "Unknown operator"))

(define internal-apply/compatible
  (%cps-proc/make%
   (lambda (stack-closure nargs operator)
     nargs				; ignored
     (let ((elements (vector->list (stack-closure/values stack-closure))))
       (apply call
	      operator
	      (car elements)
	      (reverse (cdr elements)))))))

(define *operator->procedure*
  (make-eq-hash-table))

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
		      (lambda (env name value depth offset)
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
    (declare-operator %internal-apply-unchecked funcall)
    (declare-operator %primitive-apply funcall)
    ; (declare-operator %invoke-continuation identity-procedure)
    (declare-operator %vector-index vector-index)

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