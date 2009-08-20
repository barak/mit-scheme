#| -*-Scheme-*-

$Id$

Copyright (c) 1994-1999 Massachusetts Institute of Technology

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

;;;; Closure converter
;;; package: (compiler midend)

(declare (usual-integrations))

(define *closconv-operator-and-operand-illegal?* true)

(define (closconv/top-level program #!optional after-cps?)
  (closconv/bind-parameters
   (and (not (default-object? after-cps?))
	after-cps?)
   (lambda ()
     (let* ((env (closconv/env/%make 'STATIC false))
	    (program* (closconv/expr env (lifter/letrecify program))))
       (closconv/analyze! env program*)
       (if after-cps?
	   (errcont/rewrite program*))
       program*))))

(define-macro (define-closure-converter keyword bindings . body)
  (let ((proc-name (symbol-append 'CLOSCONV/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (CLOSCONV/REMEMBER ,code
				  FORM))))))))


(define-closure-converter LOOKUP (env name)
  (closconv/lookup* env name 'ORDINARY))

(define-closure-converter LAMBDA (env lambda-list body)
  (call-with-values
   (lambda () (closconv/lambda* 'DYNAMIC env lambda-list body))
   (lambda (expr* env*)
     (set-closconv/env/close?! env* true)
     expr*)))

(define-closure-converter LET (env bindings body)
  (let* ((env* (closconv/env/make
		(binding-context-type 'LET
				      (closconv/env/context env)
				      bindings)
		env
		(map car bindings)))
	 (expr* `(LET ,(closconv/bindings env* env bindings)
		   ,(closconv/expr env* body))))
    (set-closconv/env/form! env* expr*)
    expr*))

(define-closure-converter LETREC (env bindings body)
  (let* ((env* (closconv/env/make
		(binding-context-type 'LETREC
				      (closconv/env/context env)
				      bindings)
		env
		(map car bindings)))
	 (expr* `(LETREC ,(closconv/bindings env* env* bindings)
		   ,(closconv/expr env* body))))
    (set-closconv/env/form! env* expr*)
    expr*))

(define-closure-converter CALL (env rator cont #!rest rands)
  (let* ((rands (cons cont rands))
	 (default
	   (lambda ()
	     `(CALL ,(closconv/expr env rator)
		    ,@(closconv/expr* env rands)))))
    (cond ((LOOKUP/? rator)
	   (let* ((name (lookup/name rator))
		  (rator* (closconv/remember
			   (closconv/lookup* env name 'OPERATOR)
			   rator)))
	     `(CALL ,rator*
		    ,@(closconv/expr* env rands))))
	  ((LAMBDA/? rator)
	   (let ((ll   (lambda/formals rator))
		 (body (lambda/body rator)))
	     (guarantee-simple-lambda-list ll)
	     (guarantee-argument-list rands (length ll))
	     (let ((bindings (map list ll rands)))
	       (call-with-values
		   (lambda ()
		     (closconv/lambda*
		      (binding-context-type 'CALL
					    (closconv/env/context env)
					    bindings)
		      env ll body))
		 (lambda (rator* env*)
		   (let ((bindings* (closconv/bindings env* env bindings)))
		     `(CALL ,(closconv/remember rator* rator)
			    ,@(map cadr bindings*))))))))
	  (else
	   (default)))))

(define-closure-converter QUOTE (env object)
  env
  `(QUOTE ,object))

(define-closure-converter DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-closure-converter BEGIN (env #!rest actions)
  `(BEGIN ,@(closconv/expr* env actions)))

(define-closure-converter IF (env pred conseq alt)
  `(IF ,(closconv/expr env pred)
       ,(closconv/expr env conseq)
       ,(closconv/expr env alt)))

(define (closconv/expr env expr)
  ;; This copies the expression and returns the copy.  It
  ;; simultaneously builds an environment representation (see the data
  ;; structure closconv/expr, below) by mutating the ENV argument.
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)   (closconv/quote env expr))
    ((LOOKUP)  (closconv/lookup env expr))
    ((LAMBDA)  (closconv/lambda env expr))
    ((LET)     (closconv/let env expr))
    ((DECLARE) (closconv/declare env expr))
    ((CALL)    (closconv/call env expr))
    ((BEGIN)   (closconv/begin env expr))
    ((IF)      (closconv/if env expr))
    ((LETREC)  (closconv/letrec env expr))
    (else (illegal expr))))

(define (closconv/expr* env exprs)
  (map (lambda (expr)
	 (closconv/expr env expr))
       exprs))

(define (closconv/remember new old)
  (code-rewrite/remember new old))

(define (closconv/remember* new old)
  (code-rewrite/remember* new (code-rewrite/original-form old))
  new)

(define (closconv/remember*! new old)
  (code-rewrite/remember*! new (code-rewrite/original-form old))
  new)

(define (closconv/new-name prefix)
  (new-variable prefix))

;;;; Parameterization for invocation before and after cps conversion

;; Before CPS

(define (closconv/closure/new-name/pre-cps)
  (new-closure-variable))

(define (closconv/closure/sort-variables/pre-cps variable-refs)
  (if (there-exists? variable-refs continuation-variable?)
      (internal-error "Closing over continuation variable before CPS"
		      variable-refs))
  variable-refs)

(define (closconv/closure/make-handler/pre-cps closure-name params body
					       captured)
  captured				; ignored
  `(LAMBDA (,(car params) ,closure-name ,@(cdr params))
     ,body))

(define (closconv/closure/make-trivial/pre-cps handler)
  `(CALL (QUOTE ,%make-trivial-closure) (QUOTE #F) ,handler))

(define (closconv/closure/make-set!/pre-cps closure-name index name*)
  `(CALL (QUOTE ,%heap-closure-set!) (QUOTE #F) (LOOKUP ,closure-name)
	 ,index (LOOKUP ,name*) (QUOTE ,name*)))

;; After CPS

(define (closconv/closure/new-name/post-cps)
  (let ((name (closconv/new-name 'FRAME)))
    (declare-variable-property! name '(FRAME-VARIABLE))
    name))

(define (closconv/closure/sort-variables/post-cps variable-refs)
  (call-with-values
   (lambda ()
     (list-split variable-refs
		 (lambda (free-ref)
		   (continuation-variable?
		    (closconv/binding/name (car free-ref))))))
   (lambda (cont-refs non-cont-refs)
     (append cont-refs non-cont-refs))))

(define (closconv/closure/make-handler/post-cps closure-name params body
						captured)
  `(LAMBDA ,params
     (LET ((,closure-name
	    (CALL (QUOTE ,%fetch-stack-closure)
		  (QUOTE #F)
		  (QUOTE ,captured))))
       ,body)))

(define (closconv/closure/make-trivial/post-cps handler)
  ;; This gets invoked on lambda expressions that appear in several
  ;; places (e.g. args to %make-heap-closure, %make-trivial-closure, etc.)
  handler)

(define (closconv/closure/make-set!/post-cps closure-name index name*)
  closure-name index			; ignored
  (internal-error "Assigning closure after CPS conversion?" name*))

(define %make-closure %make-heap-closure)
(define %closure-ref %heap-closure-ref)

(let-syntax ((define-closconv-parameter
	       (macro (name)
		 `(define ,name ,(symbol-append name '/pre-cps)))))
  (define-closconv-parameter closconv/closure/sort-variables)
  (define-closconv-parameter closconv/closure/make-handler)
  (define-closconv-parameter closconv/closure/make-trivial)
  (define-closconv-parameter closconv/closure/make-set!)
  (define-closconv-parameter closconv/closure/new-name))

(define (closconv/bind-parameters after-cps? thunk)
  (let ((bind-parameters
	 (lambda (lift? sort handler trivial
			constructor refer
			set new-name)
	   (fluid-let ((*lift-closure-lambdas?* lift?)
		       (closconv/closure/sort-variables sort)
		       (closconv/closure/make-handler handler)
		       (closconv/closure/make-trivial trivial)
		       (%make-closure constructor)
		       (%closure-ref refer)
		       (closconv/closure/make-set! set)
		       (closconv/closure/new-name new-name))
	     (thunk)))))
    (if after-cps?
	(bind-parameters false
			 closconv/closure/sort-variables/post-cps
			 closconv/closure/make-handler/post-cps
			 closconv/closure/make-trivial/post-cps
			 %make-stack-closure
			 %stack-closure-ref
			 closconv/closure/make-set!/post-cps
			 closconv/closure/new-name/post-cps)
	(bind-parameters *lift-closure-lambdas?*
			 closconv/closure/sort-variables/pre-cps
			 closconv/closure/make-handler/pre-cps
			 closconv/closure/make-trivial/pre-cps
			 %make-heap-closure
			 %heap-closure-ref
			 closconv/closure/make-set!/pre-cps
			 closconv/closure/new-name/pre-cps))))

(define-structure
    (closconv/env
     (conc-name closconv/env/)
     (constructor closconv/env/%make (context parent)))
  (context  false read-only true)	; Dynamic or static
  (parent   false read-only true)
  (children '())
  (bound    '())			; list of closconv/binding structures

  ;; a list of (closconv/binding reference reference ...)
  (free '())

  (form false)

  ;; CLOSE?: Should be considered for having its form closed
  ;; (i.e. converted to a %make-xxx-closure)
  (close? false read-only false)

  ;; CLOSED-OVER: slots required in closure object: either #F, #T (closed,
  ;; but no slots), or a list of (closconv/binding reference) elements
  ;; from free.
  (closed-over false)

  (binding false read-only false))      ; known self-reference binding

(define-structure
    (closconv/binding
     (conc-name closconv/binding/)
     (constructor closconv/binding/make (name env))
     (print-procedure
      (standard-unparser-method 'CLOSCONV/BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write (closconv/binding/name binding) port)))))
      
  (name false read-only true)
  (env false read-only true)
  (operator-refs '())
  (ordinary-refs '())
  (value false))

(define (closconv/env/make context parent bound-names)
  (let ((env (closconv/env/%make context parent)))
    (set-closconv/env/bound!
     env
     (map (lambda (name)
	    (closconv/binding/make name env))
	  bound-names))
    (set-closconv/env/children! parent
				(cons env (closconv/env/children parent)))
    env))

(define (closconv/lookup* env name kind)
  ;; kind = 'OPERATOR or 'ORDINARY
  (let ((ref `(LOOKUP ,name)))
    (let walk-spine ((env env))
      (cond ((not env)
	     (free-var-error name))
	    ((closconv/binding/find (closconv/env/bound env) name)
	     => (lambda (binding)
		  (case kind
		    ((ORDINARY)
		     (set-closconv/binding/ordinary-refs!
		      binding
		      (cons ref (closconv/binding/ordinary-refs binding))))
		    ((OPERATOR)
		     (set-closconv/binding/operator-refs!
		      binding
		      (cons ref (closconv/binding/operator-refs binding))))
		    (else
		     (internal-error "closconv/lookup* Illegal kind" kind)))
		  binding))
	    (else
	     (let* ((binding (walk-spine (closconv/env/parent env)))
		    (free    (closconv/env/free env))
		    (place   (assq binding free)))
	       (if (not place)
		   (set-closconv/env/free! env (cons (list binding ref) free))
		   (set-cdr! place (cons ref (cdr place))))
	       binding))))
    ref))

(define (closconv/binding/find bindings name)
  (let find ((bindings bindings))
    (and (not (null? bindings))
	 (let ((binding (car bindings)))
	   (if (not (eq? name (closconv/binding/name (car bindings))))
	       (find (cdr bindings))
	       binding)))))

(define (closconv/lambda* context env lambda-list body)
  ;; (values expr* env*)
  (let* ((env* (closconv/env/make context
				  env
				  (lambda-list->names lambda-list)))
	 (expr* `(LAMBDA ,lambda-list
		   ,(closconv/expr env* body))))
    (set-closconv/env/form! env* expr*)
    (values expr* env*)))

(define (closconv/lambda** context env lam-expr)
  ;; (values expr* env*)
  (call-with-values
   (lambda ()
     (closconv/lambda* context
		       env
		       (lambda/formals lam-expr)
		       (lambda/body lam-expr)))
   (lambda (expr* env*)
     (values (closconv/remember expr* lam-expr)
	     env*))))

(define (closconv/bindings env* env bindings)
  ;; ENV* is the environment to which the bindings are being added
  ;; ENV is the environment in which the form part of the binding is
  ;;     to be evaluated (i.e. it will be EQ? to ENV* for LETREC but
  ;;     not for LET)
  (map (lambda (binding)
	  (let ((name (car binding))
		(value (cadr binding)))
	    (list
	     name
	     (if (or (not (pair? value))
		     (not (eq? (car value) 'LAMBDA)))
		 (closconv/expr env value)
		 (call-with-values
		  (lambda ()
		    (closconv/lambda** 'DYNAMIC ; bindings are dynamic
				       env
				       value))
		  (lambda (value* env**)
		    (let ((binding
			   (or (closconv/binding/find (closconv/env/bound env*)
						      name)
			       (internal-error "Missing binding" name))))
		      (set-closconv/env/binding! env** binding)
		      (set-closconv/binding/value! binding env**)
		      value*)))))))
	bindings))

;;;; The Analyzer/Converter Proper

(define (closconv/analyze! env program)
  (closconv/contaminate! env)
  (closconv/rewrite! env)
  program)

(define (closconv/contaminate! env)
  (cond ((closconv/env/closed-over env))   ; Already figured out
	((closconv/env/close? env)
	 (closconv/close! env))
	((not (closconv/env/binding env))) ; No known self-binding
	((not (null? (closconv/binding/ordinary-refs
		      (closconv/env/binding env))))
	 ;; Self-binding is referenced other than by a call
	 (closconv/close! env)))
  (for-each closconv/contaminate! (closconv/env/children env)))

(define (closconv/close! env)
  (let ((closed-over
	 (list-transform-negative (closconv/env/free env)
	   (lambda (free-ref)
	     (closconv/static-binding? (car free-ref))))))
    (set-closconv/env/closed-over!
     env
     (if (or (null? closed-over)
	     ;; Do not close if only free reference is self!
	     (and (null? (cdr closed-over))
		  (closconv/self-reference? env (car (car closed-over)))))
	 true
	 closed-over))
    (for-each (lambda (free-ref)
		(let* ((binding (car free-ref))
		       (env* (closconv/binding/value binding)))
		  (if (and env*
			   (not (closconv/env/closed-over env*)))
		      (closconv/close! env*))))
	      closed-over)))

(define (closconv/static-binding? binding)
  (and (eq? (closconv/env/context (closconv/binding/env binding)) 'STATIC)
       (not (pseudo-static-variable? (closconv/binding/name binding)))))

(define (closconv/self-reference? env binding)
  (let ((value (closconv/binding/value binding)))
    (and value
	 (eq? value env))))

(define (closconv/rewrite! env)
  ;; This must work from the root to the leaves, because a reference
  ;; may be rewritten multiple times as it is copied from closure
  ;; to closure.
  (let ((form (closconv/env/form env))
	(closed-over (closconv/env/closed-over env)))
    (cond ((or (not form)
	       (LET/? form))
	   (if closed-over
	       (internal-error "Form can't be closed" form))
	   (for-each closconv/rewrite! (closconv/env/children env)))
	  ((LETREC/? form)
	   ;; Handled specially because it must ensure that recursive
	   ;; references work, and the LETREC must remain syntactically
	   ;; acceptable (only lambda bindings allowed).
	   (if closed-over
	       (internal-error "Form can't be closed" form))
	   (let ((closed
		  (list-transform-positive (closconv/env/bound env)
		    (lambda (binding)
		      (let ((value (closconv/binding/value binding)))
			(and value
			     (closconv/env/closed-over value)))))))
	     (if (null? closed)
		 (closconv/rewrite/letrec/trivial! env)
		 (closconv/rewrite/letrec! env closed))))
	  ((LAMBDA/? form)
	   (cond ((closconv/env/binding env) => closconv/verify-binding))
	   (cond ((pair? closed-over)
		  (closconv/rewrite/lambda! env '()))
		 (closed-over
		  (closconv/rewrite/lambda/trivial! env)))
	   (for-each closconv/rewrite! (closconv/env/children env)))
	  (else
	   (internal-error "Unknown binding form" form)))))

(define (closconv/rewrite/lambda/trivial! env)
  (closconv/maybe-lift! env
			(let ((form (closconv/env/form env)))
			  (closconv/remember* (form/preserve form)
					      form))
			closconv/closure/make-trivial))

(define (closconv/verify-binding binding)
  (if (and (not (null? (closconv/binding/operator-refs binding)))
	   (not (null? (closconv/binding/ordinary-refs binding)))
	   *closconv-operator-and-operand-illegal?*)
      (internal-error "Binding is both operator and operand" binding)))

(define (closconv/rewrite/lambda! env circular)
  ;; Env is a LAMBDA env
  (let ((closure-name (closconv/closure/new-name))
	(closed-over*
	 (closconv/closure/sort-variables (closconv/env/closed-over env))))
    (let* ((self-binding  (closconv/env/binding env)) ;possibly #F
	   (closed-over			; Remove self-reference if present
	    (cond ((and self-binding (assq self-binding closed-over*))
		   => (lambda (free-ref)
			(delq free-ref closed-over*)))
		  (else
		   closed-over*)))
	   (closed-over-names
	    (list->vector (map (lambda (binding.refs)
				 (closconv/binding/name (car binding.refs)))
			       closed-over)))
	   (captured
	    (map (lambda (binding.refs)
		   (if (memq (car binding.refs) circular)
		       `(QUOTE ,#f)
		       (form/preserve (cadr binding.refs))))
		 closed-over))
	   (form (closconv/env/form env)))

      ;; Rewrite references to closed variables and self
      (for-each
       (lambda (free-ref)
	 (let* ((binding    (car free-ref))
		(name       (closconv/binding/name binding))
		(references (cdr free-ref)))

	   (define (reference-expression)
	     `(CALL (QUOTE ,%closure-ref)
		    (QUOTE #F)
		    (LOOKUP ,closure-name)
		    (QUOTE ,closed-over-names)
		    (QUOTE ,name)))
	   (define (dbg-reference-expression)
	     (dbg/make-closure-ref %closure-ref
				   closure-name closed-over-names name))
	   (define (self-reference-expression)
	     `(LOOKUP ,closure-name))
	   (define (rewrite-self-reference! ref)
	     (form/rewrite! ref (self-reference-expression)))
	   (define (rewrite-other-reference! ref)
	     (form/rewrite! ref (reference-expression)))

	   (dbg-info/remember name
			      (if (eq? binding self-binding)
				  (self-reference-expression)
				  (dbg-reference-expression)))

	   (for-each (if (eq? (car free-ref) self-binding)
			 rewrite-self-reference!
			 rewrite-other-reference!)
		     references)))
       closed-over*)

      ;; Convert to closure and maybe lift to top level
      (closconv/maybe-lift!
       env
       (closconv/remember*
	(closconv/closure/make-handler closure-name
				       (lambda/formals form)
				       (lambda/body form)
				       closed-over-names)
	form)
       (lambda (handler)
	 `(CALL (QUOTE ,%make-closure)
		(QUOTE #F)
		,handler
		(QUOTE ,closed-over-names)
		,@captured)))
      closed-over-names)))

(define (closconv/maybe-lift! env handler transform)
  (form/rewrite! (closconv/env/form env)
		 (if *lift-closure-lambdas?*
		     (let ((handler-name
			    (let ((binding (closconv/env/binding env)))
			      (or (and binding
				       (variable/rename
					(closconv/binding/name binding)))
				  (closconv/new-name 'LAMBDA)))))
		       (closconv/lift! env handler-name handler)
		       (transform `(LOOKUP ,handler-name)))
		     (transform handler))))

(define (closconv/rewrite/letrec/trivial! env)
  (for-each closconv/rewrite! (closconv/env/children env)))

(define (closconv/rewrite/letrec! env closed*)
  ;; Env is a LETREC env
  (for-each closconv/verify-binding closed*)
  (call-with-values
   (lambda ()
     (list-split closed*
		 (lambda (binding)
		   (let ((value (closconv/binding/value binding)))
		     (pair? (closconv/env/closed-over value))))))
   (lambda (closed trivial)
     ;; IMPORTANT: This assumes that make-trivial-closure can be called
     ;; multiple times for the same lambda expression and returns
     ;; EQ? results!
     (for-each
      (lambda (binding)
	(let ((val-form
	       (closconv/env/form (closconv/binding/value binding))))
	  (for-each (lambda (ref)
		      (let* ((ref* (form/preserve ref))
			     (new (closconv/closure/make-trivial ref*)))
			(form/rewrite! ref new)
			(closconv/remember*! ref val-form)))
		    (closconv/binding/ordinary-refs binding))))
      trivial)
     (let* ((envs (map closconv/binding/value closed))
	    (circular
	     (map
	      (lambda (env)
		(let ((closed-over (closconv/env/closed-over env)))
		  (list-transform-positive closed
		    (lambda (binding)
		      (assq binding closed-over)))))
	      envs)))
       (let* ((circ-results (map closconv/rewrite/lambda! envs circular))
	      (form (closconv/env/form env)))
	 (form/rewrite!
	  form

	  (bind* (map closconv/binding/name closed)
		 (map closconv/env/form envs)
		 (beginnify
		  (append-map*
		   (list
		    (let ((ok (delq* closed (closconv/env/bound env))))
		      (if (null? ok)
			  (caddr form)
			  (let ((ok-names (map closconv/binding/name ok)))
			    `(LETREC ,(list-transform-positive (cadr form)
					(lambda (binding)
					  (memq (car binding) ok-names)))
			       ,(caddr form))))))
		   (lambda (binding captured-names circular)
		     (let ((name (closconv/binding/name binding))
			   (l (vector->list captured-names)))
		       (append-map
			(lambda (binding)
			  (let ((name* (closconv/binding/name binding)))
			    (if (not (memq name* l))
				'()
				(list
				 (closconv/closure/make-set!
				  name
				  `(QUOTE ,captured-names)
				  name*)))))
			circular)))
		   closed circ-results circular)))))
       (let ((envs (map* envs closconv/binding/value trivial)))
	 (for-each (lambda (closed-env)
		     (for-each closconv/rewrite!
			       (closconv/env/children closed-env)))
		   envs)
	 (for-each closconv/rewrite!
		   (delq* envs (closconv/env/children env))))))))

(define closconv/lift!
  (lifter/make (lambda (env)
		 (let loop ((env env))
		   (cond ((not env)
			  (internal-error "No static frame" env))
			 ((eq? (closconv/env/context env) 'STATIC)
			  (closconv/env/form env))
			 (else
			  (loop (closconv/env/parent env))))))))