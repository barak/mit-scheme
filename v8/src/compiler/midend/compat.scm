#| -*-Scheme-*-

$Id: compat.scm,v 1.5 1995/02/14 00:58:08 adams Exp $

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

;;;; Compatibility package
;;   Decides which parameters are passed on the stack. Primitives get all
;;   their parameters on the stack in an interpreter-like stack-frame.
;;   Procedures get some arguments in registers and the rest on the
;;   stack, with earlier arguments deeper to facilitate lexprs.
;;   The number of parameters passed in registers is determined by the
;;   back-end (*rtlgen/arguments-registers*)


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
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (COMPAT/REMEMBER ,code FORM))))))))

(define-compatibility-rewrite LOOKUP (env name)
  (let ((place (assq name env)))
    (if (not place)
	`(LOOKUP ,name)
	;; Important to copy value so that different debugging info
	;; can be attached to each copy, since each variable reference
	;; might have had different debugging info.
	(form/copy (cadr place)))))

(define-compatibility-rewrite LAMBDA (env lambda-list body)
  env					; ignored
  (compat/rewrite-lambda lambda-list body 
			 (compat/choose-stack-formals 1 lambda-list)))

(define-compatibility-rewrite LET (env bindings body)
  `(LET ,(lmap (lambda (binding)
		 (list (car binding)
		       (compat/expr env (cadr binding))))
	       bindings)
     ,(compat/expr env body)))  

(define-compatibility-rewrite LETREC (env bindings body)
  `(LETREC ,(lmap (lambda (binding)
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
  (compat/rewrite-call env rator cont rands))

(define (compat/rewrite-call env rator cont rands)
  (define (possibly-pass-some-args-on-stack)
    (compat/standard-call-handler env rator cont rands))

  (define (dont-split-cookie-call)
    `(CALL ,(compat/expr env rator)
	   ,(compat/expr env cont)
	   ,@(compat/expr* env rands)))

  (cond ((not (QUOTE/? rator))
	 (possibly-pass-some-args-on-stack))
	((rewrite-operator/compat? (quote/text rator))
	 => (lambda (handler)
	      (handler env rator cont rands)))
	#| Hooks into the compiler interface, when they must tail
	into another computation, are now called with the default
	(args. in registers) calling convention.  This is not a
	problem because they have fixed arity. |#
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
    ((SET! UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (compat/expr* env exprs)
  (lmap (lambda (expr)
	  (compat/expr env expr))
	exprs))

(define (compat/remember new old)
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
    ;; The new environment maps names to %stack-closure-refs and %vector-index
    ;; vectors to new, extended vectors
    (let ((alist  (lmap (lambda (name)
			  (list name
				`(CALL (QUOTE ,%stack-closure-ref)
				       (QUOTE #F)
				       (LOOKUP ,frame-variable)
				       (CALL (QUOTE ,%vector-index)
					     (QUOTE #F)
					     (QUOTE ,new-frame-vector)
					     (QUOTE ,name))
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
	   
(define (compat/choose-stack-formals special-arguments lambda-list)
  ;; SPECIAL-ARGUMENTS is the number of arguments passed by a special
  ;; mechanism, usually 1 for the continuation, or 2 for the
  ;; continuation and heap closure.
  (with-values
      (lambda ()
	(%compat/split-register&stack special-arguments
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
				   register-operands stack-operands)
  
  (define (pushed-arg-name form)
    (compat/expression->name form))

  (define (make-call new-continuation)
    `(CALL ,(compat/expr env operator)
	   ,(compat/expr env new-continuation)
	   ,@(compat/expr* env register-operands)))

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

(define (compat/standard-call-handler env rator cont rands)
  (with-values (lambda () (compat/split-register&stack rands))
    (lambda (reg-rands stack-rands)
      (compat/rewrite-call/split env rator cont reg-rands stack-rands))))

(let* ((compat/invocation-cookie
	(lambda (n)
	  (lambda (env rator cont rands)
	    (with-values
		(lambda () (compat/split-register&stack (list-tail rands n)))
	      (lambda (reg-rands stack-rands)
		(compat/rewrite-call/split
		 env rator cont
		 (append (list-head rands n) reg-rands)
		 stack-rands))))))
       (invocation+2-handler (compat/invocation-cookie 2)))

  ;; These are kinds of calls which have extra arguments like arity or cache
  (define-rewrite/compat %invoke-operator-cache invocation+2-handler)
  (define-rewrite/compat %invoke-remote-cache   invocation+2-handler)
  (define-rewrite/compat %internal-apply        invocation+2-handler)
  (define-rewrite/compat %invoke-continuation   compat/standard-call-handler))


(define-rewrite/compat %vector-index
  (lambda (env rator cont rands)
    rator cont
    ;; rands = ('<vector> '<name>)
    ;; Copy, possibly replacing vector
    `(CALL (QUOTE ,%vector-index)
	   (QUOTE #F)
	   ,(compat/expr env
			 (let ((vector-arg  (first rands)))
			   (if (QUOTE/? vector-arg)
			       (cond ((assq (quote/text vector-arg) env)
				      => (lambda (old.new)
					   `(QUOTE ,(second old.new))))
				     (else vector-arg))
			       (internal-error
				"Illegal (unquoted) %vector-index arguments"
				rands))))
	   ,(compat/expr env (second rands)))))
		       

(define-rewrite/compat %make-heap-closure
  ;; The lambda expression in a heap closure is special the closure
  ;; formal is passed by a special mechanism
  (lambda (env rator cont rands)
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
  ;; (CALL ',%variable-cache-ref '#F <read-variable-cache> 'NAME)
  ;;       ------- rator ------- cont -------- rands -----------
  (lambda (env rator cont rands)
    rator				; ignored
    (let ((cont  (compat/expr env cont))
	  (cell  (compat/expr env (first rands)))
	  (quoted-name (compat/expr env (second rands))))
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
	(if (compat/ignore-reference-traps? name)
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
		     (CALL (QUOTE ,%hook-variable-cell-ref)
			   ,cont
			   (LOOKUP ,cell-name))
		     ,(%continue `(LOOKUP ,value-name))))))))))

(define-rewrite/compat %safe-variable-cache-ref
  (lambda (env rator cont rands)
    ;; (CALL ',%safe-variable-cache-ref '#F <read-variable-cache> 'NAME)
    ;;       --------- rator --------- cont -------- rands -----------
    rator				; ignored
    (let ((cont  (compat/expr env cont))
	  (cell  (compat/expr env (first rands)))
	  (quoted-name (compat/expr env (second rands))))
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
	     ,(if (compat/ignore-reference-traps? name)
		  (%continue `(LOOKUP ,value-name))
		  `(IF (IF (CALL (QUOTE ,%reference-trap?)
				 (QUOTE #F)
				 (LOOKUP ,value-name))
			   (CALL (QUOTE ,%unassigned?)
				 (QUOTE #F)
				 (LOOKUP ,value-name))
			   (QUOTE #T))
		       ,(%continue `(LOOKUP ,value-name))
		       (CALL (QUOTE ,%hook-safe-variable-cell-ref)
			     ,cont
			     (LOOKUP ,cell-name))))))))))


;;;  These predicates should determine the right answers from declarations:

(define (compat/ignore-reference-traps? name)
  name
  #F)

(define (compat/ignore-assignment-traps? name)
  name
  #F)

;; NOTE: This is never in value position because envconv expands
;; all cell sets into begins.  In particular, this means that cont
;; should always be #F!
;; The expansion in envconv implies that SET! for value is more
;; expensive than necessary, since it could use the same cell
;; for the read and the write.

(define-rewrite/compat %variable-cache-set!
  (lambda (env rator cont rands)
    ;; (CALL ',%variable-write-cache '#F <write-variable-cache> 'NAME)
    ;;       -------- rator -------- cont -------- rands -----------
    rator				; ignored
    (let ((cont  (compat/expr env cont))
	  (cell  (compat/expr env (first rands)))
	  (value (compat/expr env (second rands)))
	  (quoted-name (compat/expr env (third rands))))
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
	     ,(if (compat/ignore-assignment-traps? name)

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
			 (CALL (QUOTE ,%hook-variable-cell-set!)
			       ,cont
			       (LOOKUP ,cell-name)
			       (LOOKUP ,value-name)))))))))))

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


(let ((known-operator->primitive
       (lambda (env rator cont rands)
	 (compat/->stack-closure
	  env cont (cddr rands)
	  (lambda (cont*)
	    `(CALL ,(compat/remember `(QUOTE ,%primitive-apply/compatible)
				     rator)
		   ,cont*
		   ,(compat/expr env (car rands)) ; Primitive
		   ,(compat/expr env (cadr rands)))))))) ; Arity

  ;; Because these are reflected into the standard C coded primitives,
  ;; there's no reason to target the machine registers -- they'd wind
  ;; up on the Scheme stack anyway since that's the only place C can
  ;; see them!
  (define-rewrite/compat %primitive-apply known-operator->primitive))


(define (compat/->stack-closure env cont rands gen)
  (define (compat/->stack-names rands)
    (compat/uniquify-append
     '()
     (lmap compat/expression->name
	   rands)))

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
	(lambda (env rator cont rands)
	  rator				; ignored
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
	(lambda (env rator cont rands)
	  rator				; ignored
	  (compat/->stack-closure
	   env cont (list-head rands arity)
	   (lambda (cont*)
	     `(CALL (QUOTE ,%primitive-apply/compatible)
		    ,cont*
		    (QUOTE ,arity)
		    (QUOTE ,prim))))))))

  (define (define-global-call rator arity name)
    (define-rewrite/compat rator
      (lambda (env rator cont rands)
	rator				; ignored
	(let ((desc (list name (or arity (length rands)))))
	  ;; This way ensures it works with very small numbers of
	  ;; argument registers:
	  (compat/rewrite-call env
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
				 (call ',%vector-index '#F ',fv1 'save2)
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