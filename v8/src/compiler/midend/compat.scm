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

;;;; Compatibility package
;;
;;  Decides which parameters are passed on the stack. Primitives get all
;;  their parameters on the stack in an interpreter-like stack-frame.
;;  Procedures get some arguments in registers and the rest on the
;;  stack, with earlier arguments deeper to facilitate lexprs.  The
;;  number of parameters passed in registers is determined by the
;;  back-end (*rtlgen/argument-registers*)
;;
;;  Also expands cache operators to full form.


;;; package: (compiler midend)

(declare (usual-integrations))

(define (compat/top-level program)
  (let ((result (form/match compat/expression-pattern program)))
    (if (not result)
	(internal-error "Expression does not bind continuation" program))
    (compat/remember
     (compat/expr '()			; Nothing known about stack yet
      (let ((continuation-variable
	     (cadr (assq compat/?cont-variable result)))
	    (body (cadr (assq compat/?expr-body result))))
	(let ((result (form/match compat/needs-environment-pattern body)))
	  (if result
	      `(LAMBDA (,continuation-variable
			,(cadr (assq compat/?env-variable result)))
		 ,(cadr (assq compat/?expr-body result)))
	      `(LAMBDA (,continuation-variable
			,(new-ignored-variable 'IGNORED-ENVIRONMENT))
		 ,body)))))
     program)))

(define compat/?cont-variable (->pattern-variable 'CONT-VARIABLE))
(define compat/?env-variable (->pattern-variable 'ENV-VARIABLE))
(define compat/?frame-variable (->pattern-variable 'FRAME-VARIABLE))
(define compat/?frame-vector (->pattern-variable 'FRAME-VECTOR))
(define compat/?expr-body (->pattern-variable 'EXPR-BODY))
(define compat/?body (->pattern-variable 'BODY))

(define compat/expression-pattern
  `(LET ((,compat/?cont-variable
	  (CALL (QUOTE ,%fetch-continuation)
		(QUOTE #F))))
     ,compat/?expr-body))

(define compat/needs-environment-pattern
  `(LET ((,compat/?env-variable
	  (CALL (QUOTE ,%fetch-environment)
		(QUOTE #F))))
     ,compat/?expr-body))

(define compat/frame-pattern
  `(LET ((,compat/?frame-variable
	  (CALL (QUOTE ,%fetch-stack-closure)
		(QUOTE #F)
		(QUOTE ,compat/?frame-vector))))
     ,compat/?body))

(define-macro (define-compatibility-rewrite keyword bindings . body)
  (let ((proc-name (symbol-append 'COMPAT/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	       (COMPAT/REMEMBER ,code FORM))))))))

(define-compatibility-rewrite LOOKUP (env name)
  (let ((place (assq name env)))
    (if (not place)
	`(LOOKUP ,name)
	;; Important to copy value so that different debugging info can be
        ;; attached to each copy, since each variable reference might
        ;; have had different debugging info. Note: It is unlikely
        ;; that a variable will have debuggig info this late phase
        ;; sequence [SRA].
	(form/copy (cadr place)))))

(define-compatibility-rewrite LAMBDA (env lambda-list body)
  env					; ignored
  (compat/rewrite-lambda lambda-list body 
			 (compat/choose-stack-formals 1 lambda-list)))

(define-compatibility-rewrite LET (env bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (compat/expr env (cadr binding))))
	      bindings)
     ,(compat/expr env body)))  

(define-compatibility-rewrite LETREC (env bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (compat/expr env (cadr binding))))
		 bindings)
     ,(compat/expr env body)))

(define-compatibility-rewrite QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-compatibility-rewrite BEGIN (env #!rest actions)
  `(BEGIN ,@(compat/expr* env actions)))

(define-compatibility-rewrite DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-compatibility-rewrite IF (env pred conseq alt)
  `(IF ,(compat/expr env pred)
       ,(compat/expr env conseq)
       ,(compat/expr env alt)))

(define-compatibility-rewrite CALL (env rator cont #!rest rands)
  (compat/rewrite-call env form rator cont rands))

(define (compat/rewrite-call env form rator cont rands)
  (define (possibly-pass-some-args-on-stack)
    (compat/standard-call-handler env form rator cont rands))

  (define (dont-split-cookie-call)
    `(CALL ,(compat/expr env rator)
	   ,(compat/expr env cont)
	   ,@(compat/expr* env rands)))

  (cond ((not (QUOTE/? rator))
	 (possibly-pass-some-args-on-stack))
	((rewrite-operator/compat? (quote/text rator))
	 => (lambda (handler)
	      (handler env form rator cont rands)))
	;; Hooks into the compiler interface, when they must tail into another
        ;; computation, are now called with the default (args. in
        ;; registers) calling convention.  This is not a problem
        ;; because they have fixed low arity.
	((and (operator/satisfies? (quote/text rator) '(OUT-OF-LINE-HOOK))
	      (not (operator/satisfies? (quote/text rator)
					'(SPECIAL-INTERFACE)))
	      (not (equal? cont '(QUOTE #F))))
	 (compat/out-of-line env rator cont rands))
	(else
	 (dont-split-cookie-call))))

(define (compat/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (compat/quote env expr))
    ((LOOKUP)   (compat/lookup env expr))
    ((LAMBDA)   (compat/lambda env expr))
    ((LET)      (compat/let env expr))
    ((DECLARE)  (compat/declare env expr))
    ((CALL)     (compat/call env expr))
    ((BEGIN)    (compat/begin env expr))
    ((IF)       (compat/if env expr))
    ((LETREC)   (compat/letrec env expr))
    (else
     (illegal expr))))

(define (compat/expr* env exprs)
  (map (lambda (expr)
	 (compat/expr env expr))
       exprs))

(define (compat/remember new old)
  (code-rewrite/remember new old))

(define (compat/remember* new old)
  (code-rewrite/remember new old))

(define (compat/new-name prefix)
  (new-variable prefix))

(define (compat/lambda-list->frame lambda-list)
  (let ((names (lambda-list->names lambda-list)))
    (let ((first (car names)))
      (if (not (continuation-variable? first))
	  (internal-error "No continuation variable found" lambda-list))
      (list->vector (cons first (reverse (cdr names)))))))

(define (compat/rewrite-lambda formals body formals-on-stack)
  (define (compat/new-env frame-variable old-frame-vector new-frame-vector)
    ;; The new environment maps names to %stack-closure-refs and layout
    ;; vectors to new, extended vectors
    (let ((alist  (map (lambda (name)
			 (list name
			       `(CALL (QUOTE ,%stack-closure-ref)
				      (QUOTE #F)
				      (LOOKUP ,frame-variable)
				      (QUOTE ,new-frame-vector)
				      (QUOTE ,name))))
		       formals-on-stack)))
      (if old-frame-vector
	  (cons (list old-frame-vector new-frame-vector)
		alist)
	  alist)))

  (define (make-new-lambda frame-variable old-frame-vector new-frame-vector
			   body)
    `(LAMBDA ,formals
       (LET ((,frame-variable
	      (CALL (QUOTE ,%fetch-stack-closure)
		    (QUOTE #F)
		    (QUOTE ,new-frame-vector))))
	 ,(compat/expr (compat/new-env
			frame-variable old-frame-vector new-frame-vector)
		       body))))
  
  (cond ((null? formals-on-stack)
	 `(LAMBDA ,formals
	    ,(compat/expr '() body)))
	((form/match compat/frame-pattern body)
	 => (lambda (match)
	      (let* ((old-frame-vector
		      (cadr (assq compat/?frame-vector match)))
		     (new-frame-vector 
		      (list->vector (append (vector->list old-frame-vector)
					    formals-on-stack))))
		(make-new-lambda
		 (cadr (assq compat/?frame-variable match))
		 old-frame-vector
		 new-frame-vector
		 (cadr (assq compat/?body match))))))
	(else
	 (let ((frame (compat/new-name 'FRAME)))
	   (declare-variable-property! frame '(FRAME-VARIABLE))
	   (make-new-lambda  frame
			     #F
			     (list->vector formals-on-stack)
			     body)))))
	   
(define (compat/choose-stack-formals special-argument-count lambda-list)
  ;; SPECIAL-ARGUMENT-COUNT is the number of arguments passed by a special
  ;; mechanism, usually 1 for the continuation, or 2 for the
  ;; continuation and heap closure.
  (call-with-values
      (lambda ()
	(%compat/split-register&stack special-argument-count
				      (lambda-list->names lambda-list)))
    (lambda (register-formals stack-formals)
      register-formals			; ignored
      stack-formals)))


(define (compat/split-register&stack expressions)
  (%compat/split-register&stack 0 expressions))

(define (%compat/split-register&stack special-arguments args-or-formals)
  ;;(values for-regsiters for-stack)
  (let* ((len    (length args-or-formals))
	 (argument-register-count
	  (+ special-arguments
	     (vector-length *rtlgen/argument-registers*))))
    (if (> len argument-register-count)
	(values (list-head args-or-formals argument-register-count)
		(list-tail args-or-formals argument-register-count))
	(values args-or-formals
		'()))))

(define (compat/expression->name expr)
  (cond ((LOOKUP/? expr)
	 (lookup/name expr))
	((CALL/%stack-closure-ref? expr)
	 (quote/text (CALL/%stack-closure-ref/name expr)))
	(else
	 (compat/new-name 'ARG))))

(define (compat/uniquify-append prefix addends)
  ;; append addends, ensuring that each is a unique name
  (define (uniquify names)
    (if (null? names)
	'()
	(let ((unique-tail (uniquify (cdr names))))
	  (cons (if (or (memq (car names) unique-tail)
			(memq (car names) prefix))
		    (variable/rename (car names))
		    (car names))
		unique-tail))))
  (append prefix (uniquify addends)))


(define (compat/rewrite-call/split env operator continuation
				   extra-operands ; e.g. arity
				   register-operands stack-operands)
  
  (define (pushed-arg-name form)
    (compat/expression->name form))

  (define (make-call new-continuation)
    (let ((operator*        (compat/expr env operator))
	  (continuation*    (compat/expr env new-continuation))
	  (extra-operands*  (compat/expr* env extra-operands))
	  (operands*        (compat/expr* env register-operands)))
      `(CALL ,operator* ,continuation* ,@extra-operands* ,@operands*)))

  (define (make-pushing-call continuation old-frame old-pushed-expressions)
    (make-call
     `(CALL ',%make-stack-closure
	    '#F
	    ,continuation
	    ',(list->vector
	       (compat/uniquify-append
		(vector->list old-frame)
		(map pushed-arg-name stack-operands)))
	    ,@old-pushed-expressions
	    ,@stack-operands)))

  (cond ((null? stack-operands)
	 (make-call continuation))
	((call/%make-stack-closure? continuation)
	 ;; extend the stack closure with parameters
	 (make-pushing-call 
	  (call/%make-stack-closure/lambda-expression continuation)
	  (quote/text (call/%make-stack-closure/vector continuation))
	  (call/%make-stack-closure/values continuation)))
	(else
	 ;; introduce a new stack closure for extra parameters
	 (make-pushing-call continuation
			    '#()
			    '()))))

(define *compat-rewritten-operators*
  (make-eq-hash-table))

(define-integrable (rewrite-operator/compat? rator)
  (hash-table/get *compat-rewritten-operators* rator false))

(define (define-rewrite/compat operator handler)
  (hash-table/put! *compat-rewritten-operators* operator handler))

(define (compat/standard-call-handler env form rator cont rands)
  form ;ignored
  (call-with-values (lambda () (compat/split-register&stack rands))
    (lambda (reg-rands stack-rands)
      (compat/rewrite-call/split env rator cont '() reg-rands stack-rands))))

(let* ((compat/invocation-cookie
	(lambda (n)
	  (lambda (env form rator cont rands)
	    form			;ignored
	    (call-with-values
		(lambda () (compat/split-register&stack (list-tail rands n)))
	      (lambda (reg-rands stack-rands)
		(compat/rewrite-call/split
		 env rator cont
		 (list-head rands n)
		 reg-rands
		 stack-rands))))))
       (invocation+2-handler (compat/invocation-cookie 2)))

  ;; These are kinds of calls which have extra arguments like arity
  ;; or cache.
  (define-rewrite/compat %invoke-operator-cache     invocation+2-handler)
  (define-rewrite/compat %invoke-remote-cache       invocation+2-handler)
  (define-rewrite/compat %internal-apply            invocation+2-handler)
  (define-rewrite/compat %internal-apply-unchecked  invocation+2-handler)

  ;; Continuations receive multiple arguments like normal procedures.
  (define-rewrite/compat %invoke-continuation    compat/standard-call-handler))


(define-rewrite/compat %stack-closure-ref
  (lambda (env form rator cont rands)
    form rator cont
    ;; rands = (<frame> '<vector> '<name>)
    ;; Copy, possibly replacing vector
    `(CALL (QUOTE ,%stack-closure-ref)
	   (QUOTE #F)
	   ,(compat/expr env (first rands))
	   ,(compat/expr env
			 (let ((vector-arg  (second rands)))
			   (if (QUOTE/? vector-arg)
			       (cond ((assq (quote/text vector-arg) env)
				      => (lambda (old.new)
					   `(QUOTE ,(second old.new))))
				     (else vector-arg))
			       (internal-error
				"Illegal (unquoted) %stack-closure-ref vector"
				rands))))
	   ,(compat/expr env (third rands)))))

(define-rewrite/compat %make-heap-closure
  ;; The lambda expression in a heap closure is special the closure
  ;; formal is passed by a special mechanism
  (lambda (env form rator cont rands)
    rator				; ignored
    (let ((lam-expr  (first rands)))
      (if (not (LAMBDA/? lam-expr))
	  (internal-error "%make-heap-closure is missing a LAMBDA-expression"
			  rands))
      (let ((lambda-list  (lambda/formals lam-expr)))
	(if (or (< (length lambda-list) 2)
		(not (closure-variable? (second lambda-list))))
	    (internal-error
	     "%make-heap-closure LAMBDA-expression has bad formals" lam-expr))
	`(CALL (QUOTE ,%make-heap-closure)
	       ,(compat/expr env cont)
	       ,(compat/remember
		 (compat/rewrite-lambda
		  lambda-list
		  (lambda/body lam-expr)
		  (compat/choose-stack-formals 2 lambda-list))
		 lam-expr)
	       . ,(compat/expr* env (cdr rands)))))))

(define-rewrite/compat %variable-cache-ref
  ;; (CALL %variable-cache-ref '#F <read-variable-cache> 'IGNORE-TRAPS? 'NAME)
  ;;       ------ rator ------ cont -------- rands -----------
  (lambda (env form rator cont rands)
    rator				; ignored
    (define (equivalent form*) (compat/remember* form* form))
    (let ((cont  (compat/expr env cont))
	  (cell  (compat/expr env (first rands)))
	  (ignore-traps? (compat/expr env (second rands)))
	  (quoted-name (compat/expr env (third rands))))
      (compat/verify-hook-continuation cont)
      (compat/verify-cache cell quoted-name)
      (let* ((%continue
	      (if (not (QUOTE/? cont))
		  (lambda (expr)
		    `(CALL (QUOTE ,%invoke-continuation)
			   ,cont
			   ,expr))
		  (lambda (expr) expr)))
	     (name (quote/text quoted-name))
	     (cell-name
	      (new-variable-cache-variable name `(VARIABLE-CACHE ,name)))
	     (value-name (compat/new-name name)))
	(if (quote/text ignore-traps?)
	    (%continue `(CALL (QUOTE ,%variable-cell-ref)
			      (QUOTE #F)
			      (CALL (QUOTE ,%variable-read-cache) (QUOTE #F)
				    ,cell ,quoted-name)))
	    `(LET ((,cell-name
		    (CALL (QUOTE ,%variable-read-cache) (QUOTE #F)
			  ,cell ,quoted-name)))
	       (LET ((,value-name (CALL (QUOTE ,%variable-cell-ref)
					(QUOTE #F)
					(LOOKUP ,cell-name))))
		 (IF (CALL (QUOTE ,%reference-trap?)
			   (QUOTE #F)
			   (LOOKUP ,value-name))
		     ,(equivalent
		       `(CALL (QUOTE ,%hook-variable-cell-ref)
			      ,cont
			      (LOOKUP ,cell-name)))
		     ,(%continue `(LOOKUP ,value-name))))))))))

(define-rewrite/compat %safe-variable-cache-ref
  (lambda (env form rator cont rands)
    ;; (CALL ',%safe-variable-cache-ref '#F <read-variable-cache>
    ;;       'IGNORE-TRAPS? 'NAME)
    ;;       --------- rator --------- cont -------- rands -----------
    rator				; ignored
    (define (equivalent form*) (compat/remember* form* form))
    (let ((cont  (compat/expr env cont))
	  (cell  (compat/expr env (first rands)))
	  (ignore-traps? (compat/expr env (second rands)))
	  (quoted-name (compat/expr env (third rands))))
      (compat/verify-hook-continuation cont)
      (compat/verify-cache cell quoted-name)
      (let* ((%continue
	      (if (not (QUOTE/? cont))
		  (lambda (expr)
		    `(CALL (QUOTE ,%invoke-continuation)
			   ,cont
			   ,expr))
		  (lambda (expr) expr)))
	     (name (quote/text quoted-name))
	     (cell-name
	      (new-variable-cache-variable name `(VARIABLE-CACHE ,name)))
	     (value-name (compat/new-name name)))
	`(LET ((,cell-name
		(CALL (QUOTE ,%variable-read-cache) (QUOTE #F)
		      ,cell ,quoted-name)))
	   (LET ((,value-name (CALL (QUOTE ,%variable-cell-ref)
				    (QUOTE #F)
				    (LOOKUP ,cell-name))))
	     ,(if (quote/text ignore-traps?)
		  (%continue `(LOOKUP ,value-name))
		  `(IF (IF (CALL (QUOTE ,%reference-trap?)
				 (QUOTE #F)
				 (LOOKUP ,value-name))
			   (CALL (QUOTE ,%unassigned?)
				 (QUOTE #F)
				 (LOOKUP ,value-name))
			   (QUOTE #T))
		       ,(%continue `(LOOKUP ,value-name))
		       ,(equivalent
			 `(CALL (QUOTE ,%hook-safe-variable-cell-ref)
				,cont
				(LOOKUP ,cell-name)))))))))))

;; NOTE: This is never in value position because envconv expands
;; all cell sets into begins.  In particular, this means that cont
;; should always be #F!
;; The expansion in envconv implies that SET! for value is more
;; expensive than necessary, since it could use the same cell
;; for the read and the write.

(define-rewrite/compat %variable-cache-set!
  (lambda (env form rator cont rands)
    ;; (CALL ',%variable-cache-set! '#F <write-variable-cache> 'IGNORE-TRAPS? 'NAME)
    ;;       ------- rator -------- cont -------- rands -----------
    rator				; ignored
    (define (equivalent form*) (compat/remember* form* form))
    (let ((cont          (compat/expr env cont))
	  (cell          (compat/expr env (first rands)))
	  (value         (compat/expr env (second rands)))
	  (ignore-traps? (compat/expr env (third rands)))
	  (quoted-name   (compat/expr env (fourth rands))))
      ;; (compat/verify-hook-continuation cont)
      (if (not (equal? cont '(QUOTE #F)))
	  (internal-error "Unexpected continuation to variable cache assignment"
			  cont))
      (compat/verify-cache cell quoted-name)
      (let* ((name (quote/text quoted-name))
	     (cell-name
	      (new-variable-cache-variable name `(ASSIGNMENT-CACHE ,name)))
	     (old-value-name (compat/new-name name))
	     (value-name     (compat/new-name 'VALUE)))
	`(LET ((,value-name ,value))
	   (LET ((,cell-name
		  (CALL (QUOTE ,%variable-write-cache) (QUOTE #F)
			,cell ,quoted-name)))
	     ,(if (quote/text ignore-traps?)

		  `(CALL (QUOTE ,%variable-cell-set!)
			 ,cont
			 (LOOKUP ,cell-name)
			 (LOOKUP ,value-name))
		  
		  `(LET ((,old-value-name (CALL (QUOTE ,%variable-cell-ref)
						(QUOTE #F)
						(LOOKUP ,cell-name))))
		     (IF (IF (CALL (QUOTE ,%reference-trap?)
				   (QUOTE #F)
				   (LOOKUP ,old-value-name))
			     (CALL (QUOTE ,%unassigned?)
				   (QUOTE #F)
				   (LOOKUP ,old-value-name))
			     (QUOTE #T))
			 (CALL (QUOTE ,%variable-cell-set!)
			       ,cont
			       (LOOKUP ,cell-name)
			       (LOOKUP ,value-name))
			 ,(equivalent
			   `(CALL (QUOTE ,%hook-variable-cell-set!)
				  ,cont
				  (LOOKUP ,cell-name)
				  (LOOKUP ,value-name))))))))))))

(define (compat/verify-cache cell name)
  (if (and (LOOKUP/? cell)
	   (QUOTE/? name))
      'ok
      (internal-error "Unexpected arguments to variable cache operation"
		      cell name)))

(define (compat/verify-hook-continuation cont)
  (if (or (QUOTE/? cont)
	  (LOOKUP/? cont)
	  (CALL/%stack-closure-ref? cont))
      'ok
      (internal-error "Unexpected continuation to out-of-line hook" cont)))


(define (compat/out-of-line env rator cont rands)
  ;; We should not get complex continuations for the out-of-line operators,
  ;; but we do, so we have to cope.
  (define (normal)
    `(CALL ,(compat/expr env rator)
	   ,(compat/expr env cont)
	   ,@(compat/expr* env rands)))
  (cond ((QUOTE/? cont) (normal))
	((LOOKUP/? cont)  (normal))
	((CALL/%stack-closure-ref? cont) (normal))
	(else
	 (if compiler:guru?
	     (internal-warning "Unexpected continuation for hook"
			       `(CALL ,rator ,cont ,@rands)))
	 `(CALL (QUOTE ,%invoke-continuation)
		,(compat/expr env cont)
		(CALL ,(compat/expr env rator)
		      (QUOTE #F)
		      ,@(compat/expr* env rands))))))


;; PRIMITIVE procedures are reflected into the standard C coded primitives,
;; so there is there no reason to target the machine registers --
;; they'd wind up on the Scheme stack anyway since that's the only
;; place C can see them.

(define-rewrite/compat %primitive-apply
  (lambda (env form rator cont rands)
    form				; ignored
    (let ((quote-arity      (first rands))
	  (quote-primitive  (second rands)))
      (let ((arity      (quote/text quote-arity))
	    (primitive  (quote/text quote-primitive)))
	(if (and (primitive-procedure? primitive)
		 (exact-nonnegative-integer? arity)
		 (let ((prim-arity (primitive-procedure-arity primitive)))
		   (or (negative? prim-arity) ; i.e. LEXPR
		       (eqv? arity prim-arity))))
	    (compat/->stack-closure
	     env cont (cddr rands)
	     (lambda (cont*)
	       `(CALL (QUOTE ,%primitive-apply/compatible)
		      ,cont*
		      ,quote-arity
		      ,quote-primitive)))
	    ;; If the procedure it is not a primitive of the purported arity
	    ;; then invoking it with %internal-apply will generate a runtime
	    ;; error.
	    (compat/expr env
			 (compat/remember
			  `(CALL (QUOTE ,%internal-apply)
				 ,cont
				 ,@rands)
			  form)))))))

(define (compat/->stack-closure env cont rands gen)
  (define (compat/->stack-names rands)
    (compat/uniquify-append
     '()
     (map compat/expression->name rands)))

  (define (compat/->stack-frame names)
    (list->vector (cons (car names) (reverse (cdr names)))))

  (let* ((cont (compat/expr env cont)))
    (define (fail)
      (internal-error "Illegal continuation" cont))
    (define (default cont-name cont)
      (let ((names
	     (cons cont-name (compat/->stack-names rands))))
	`(CALL (QUOTE ,%make-stack-closure)
	       (QUOTE #F)
	       (QUOTE #F)		; magic cookie
	       (QUOTE ,(compat/->stack-frame names))
	       ,cont
	       ,@(compat/expr* env (reverse rands)))))
    (cond ((LOOKUP/? cont)
	   (gen (default (lookup/name cont) cont)))
	  ((CALL/%make-stack-closure? cont)
	   (let ((cont-var (new-continuation-variable)))
	     `(CALL
	       (LAMBDA (,cont-var)
		 ,(gen (default cont-var `(LOOKUP ,cont-var))))
	       ,cont)))
	  ((CALL/%stack-closure-ref? cont)
	   (gen (default (cadr (list-ref cont 5)) cont)))
	  (else (fail)))))

(let ()
  (define (define-primitive-call rator arity name)
    (let ((prim (make-primitive-procedure name)))
      (define-rewrite/compat rator
	(lambda (env form rator cont rands)
	  form rator			; ignored
	  (compat/->stack-closure
	   env cont rands
	   (lambda (cont*)
	     `(CALL (QUOTE ,%primitive-apply/compatible)
		    ,cont*
		    (QUOTE ,arity)
		    (QUOTE ,prim))))))))

  (define (define-truncated-call rator arity name)
    (let ((prim (make-primitive-procedure name)))
      (define-rewrite/compat rator
	(lambda (env form rator cont rands)
	  form rator			; ignored
	  (compat/->stack-closure
	   env cont (list-head rands arity)
	   (lambda (cont*)
	     `(CALL (QUOTE ,%primitive-apply/compatible)
		    ,cont*
		    (QUOTE ,arity)
		    (QUOTE ,prim))))))))

  (define (define-global-call rator arity name)
    (define-rewrite/compat rator
      (lambda (env form rator cont rands)
	form rator			; ignored
	(let ((desc (list name (or arity (length rands)))))
	  ;; This way ensures it works with very small numbers of
	  ;; argument registers:
	  (compat/rewrite-call env
			       form
			       `(QUOTE ,%invoke-remote-cache)
			       cont
			       (cons* `(QUOTE ,desc)
				      `(QUOTE #F)
				      rands))))))

  (define-primitive-call %*define 3 'LOCAL-ASSIGNMENT)
  (define-primitive-call %execute 2 'SCODE-EVAL)

  (define-global-call %*define* 3 'DEFINE-MULTIPLE)
  (define-global-call %*make-environment false '*MAKE-ENVIRONMENT)
  (define-global-call %copy-program 1 'COPY-PROGRAM)

  ;; *** Until the full version is implemented ***
  ;; The parameters dropped are the expected depth and offset.

  (define-truncated-call %*lookup 2 'LEXICAL-REFERENCE)
  (define-truncated-call %*set! 3 'LEXICAL-ASSIGNMENT)
  (define-truncated-call %*unassigned? 2 'LEXICAL-UNASSIGNED?))


#| Test:

(set! *rtlgen/argument-registers* '#(2 6))

(let ((fv1 '#(save1 save2 save3)))
  (kmp/pp
   (compat/expr
    '()
    `(call (lookup proc)
	   (call ',%make-stack-closure
		 '#f
		 (lambda (k val1 val2 val3 val4)
		   (let ((frame (call ',%fetch-stack-closure '#f ',fv1)))
		     (call (lookup val4)
			   (call ',%stack-closure-ref
				 '#F
				 (lookup frame)
				 ',fv1
				 'save2)
			   (lookup val2)
			   '1000)))
		 ',fv1
		 's1
		 's2
		 's3)
	   'arg1
	   'arg2
	   'arg3))))
|#