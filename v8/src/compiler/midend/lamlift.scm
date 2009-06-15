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

;;;; Lambda lifter
;;; package: (compiler midend)

(declare (usual-integrations))

(define (lamlift/top-level program)
  (let* ((env (lamlift/env/%make 'STATIC #F 0))
	 (program* (lamlift/expr env (lifter/letrecify program))))
    (lamlift/analyze! env)
    (lamlift/remember program* program)))

(define lamlift/*lift-stubs-aggressively?* #F)

(define-macro (define-lambda-lifter keyword bindings . body)
  (let ((proc-name (symbol-append 'LAMLIFT/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (LAMLIFT/REMEMBER ,code
				 FORM))))))))

(define-lambda-lifter LOOKUP (env name)
  (call-with-values
   (lambda () (lamlift/lookup* env name 'ORDINARY))
   (lambda (ref binding)
     (set-lamlift/binding/operand-uses! binding
      (cons ref (lamlift/binding/operand-uses binding)))
     ref)))

(define-lambda-lifter LAMBDA (env lambda-list body)
  (call-with-values
      (lambda ()
	(lamlift/lambda* 'DYNAMIC env lambda-list body))
    (lambda (expr* env*)
      env*				; ignored
      expr*)))

(define (lamlift/lambda* context env lambda-list body)
  ;; (values expr* env*)
  (let* ((env* (lamlift/env/make
		context env (lambda-list->names lambda-list)))
	 (expr* `(LAMBDA ,lambda-list ,(lamlift/expr env* body))))
    (set-lamlift/env/form! env* expr*)
    (values expr* env*)))

(define (lamlift/lambda** context env lam-expr)
  ;; (values expr* env*)
  (call-with-values
   (lambda ()
     (lamlift/lambda* context
		      env
		      (lambda/formals lam-expr)
		      (lambda/body lam-expr)))
   (lambda (expr* env*)
     (values (lamlift/remember expr* lam-expr)
	     env*))))

(define-lambda-lifter LET (env bindings body)
  (lamlift/let* 'LET env bindings body))

(define-lambda-lifter LETREC (env bindings body)
  (lamlift/let* 'LETREC env bindings body))

(define-lambda-lifter CALL (env rator cont #!rest rands)
  (cond ((LOOKUP/? rator)
	 (call-with-values
	     (lambda () (lamlift/lookup* env (lookup/name rator) 'OPERATOR))
	   (lambda (rator* binding)
	     (let ((result
		    `(CALL ,(lamlift/remember rator* rator)
			   ,(lamlift/expr env cont)
			   ,@(lamlift/expr* env rands))))
	       (set-lamlift/binding/calls!
		binding
		(cons result (lamlift/binding/calls binding)))
	       result))))
	((LAMBDA/? rator)
	 (let ((ll   (lambda/formals rator))
	       (cont+rands (cons cont rands)))
	   (guarantee-simple-lambda-list ll)
	   (guarantee-argument-list cont+rands (length ll))
	   (let ((bindings (map list ll cont+rands)))
	     (call-with-values
		 (lambda ()
		   (lamlift/lambda**
		    (binding-context-type 'CALL
					  (lamlift/env/context env)
					  bindings)
		    env rator))
	       (lambda (rator* env*)
		 (let ((bindings* (lamlift/bindings env* env bindings)))
		   (set-lamlift/env/split?! env* 'UNNECESSARY)
		   `(CALL ,rator*
			  ,@(map cadr bindings*))))))))
	(else
	 `(CALL ,(lamlift/expr env rator)
		,(lamlift/expr env cont)
		,@(lamlift/expr* env rands)))))

(define-lambda-lifter QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-lambda-lifter DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-lambda-lifter BEGIN (env #!rest actions)
  `(BEGIN ,@(lamlift/expr* env actions)))

(define-lambda-lifter IF (env pred conseq alt)
  `(IF ,(lamlift/expr env pred)
       ,(lamlift/expr env conseq)
       ,(lamlift/expr env alt)))

(define (lamlift/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (lamlift/quote env expr))
    ((LOOKUP)   (lamlift/lookup env expr))
    ((LAMBDA)   (lamlift/lambda env expr))
    ((LET)      (lamlift/let env expr))
    ((DECLARE)  (lamlift/declare env expr))
    ((CALL)     (lamlift/call env expr))
    ((BEGIN)    (lamlift/begin env expr))
    ((IF)       (lamlift/if env expr))
    ((LETREC)   (lamlift/letrec env expr))
    (else
     (illegal expr))))

(define (lamlift/expr* env exprs)
  (map (lambda (expr)
	 (lamlift/expr env expr))
       exprs))

(define (lamlift/remember new old)
  (code-rewrite/remember new old))

(define (lamlift/split new old)
  (let ((old* (code-rewrite/original-form old)))
    (if old*
	(code-rewrite/remember*
	 new
	 (if (new-dbg-procedure? old*)
	     (new-dbg-procedure/copy old*)
	     old*)))
    new))

(define (lamlift/new-name prefix)
  (new-variable prefix))

(define-structure
    (lamlift/env
     (conc-name lamlift/env/)
     (constructor lamlift/env/%make (context parent depth))
     (print-procedure
      (standard-unparser-method 'LAMLIFT/ENV
	(lambda (env port)
	  (write-char #\space port)
	  (write (lamlift/env/context env) port)
	  (write-char #\space port)
	  (write (car (or (lamlift/env/form env) '(ROOT))) port)
	  (write-char #\space port)
	  (write (lamlift/env/depth env) port)))))

  (context  false  read-only true)	; STATIC or DYNAMIC
  (parent   false  read-only true)	; #F or another environment
  (children '()    read-only false)
  (depth    0      read-only true)	; depth from root
  (bound   '()     read-only false)	; A list of LAMLIFT/BINDINGs

  ;; Each of the next two slots is a list of associations between bindings
  ;; and lists of references: Each association is a list headed by a
  ;; binding, with the rest of the list being a list of references:
  ;; (LAMLIFT/BINDING reference reference ...) where reference is
  ;; (LOOKUP <var>)
  (free-ordinary-refs '() read-only false)
  (free-operator-refs '() read-only false)

  (form false read-only false)

  ;; When this is a lambda's env and the lambda is bound to a name, BINDING
  ;; is that LAMLIFT/BINDING.  #F implies either this frame is an anonymous
  ;; lambda or a let(rec) frame.
  (binding false read-only false)

  (split? 'YES read-only false)		; 'YES, 'NO, or 'UNNECESSARY

  ;; Formals to be added (formerly free variables)
  (extended '() read-only false)

  ;; The new parent for this frame should we choose to drift it up.  This
  ;; is the highest frame that could be a parent without adding new
  ;; extra parameters.
  (drift-frame #F read-only false)
  )

(define-structure
    (lamlift/binding
     (conc-name lamlift/binding/)
     (constructor lamlift/binding/make (name env))
     (print-procedure
      (standard-unparser-method 'LAMLIFT/BINDING
	(lambda (v port)
	  (write-char #\space port)
	  (write-string (symbol-name (lamlift/binding/name v))
			port)))))

  (name #F read-only true)
  (env  #F read-only true)		; a LAMLIFT/ENV
  (calls '() read-only false)		; List of call sites
  (operand-uses '() read-only false)	; List of operand use (LOOKUP <name>)
  (value #F read-only false))		; a LAMLIFT/ENV for use in body

(define-integrable (lamlift/binding/operator-only? binding)
  (null? (lamlift/binding/operand-uses binding)))

(define (lamlift/env/make context parent names)
  (let* ((depth  (if parent (1+ (lamlift/env/depth parent)) 0))
	 (env    (lamlift/env/%make context parent depth)))
    (set-lamlift/env/bound! env
			    (map (lambda (name)
				   (lamlift/binding/make name env))
				 names))
    (set-lamlift/env/children! parent (cons env (lamlift/env/children parent)))
    env))

(define (lamlift/lookup* env name kind)
  ;; (values copied-reference-form binding)
  (define (traverse fetch store!)
    (let walk-spine ((env env))
      (cond ((not env)
	     (free-var-error name))
	    ((lamlift/binding/find (lamlift/env/bound env) name)
	     => (lambda (binding)
		  (values `(LOOKUP ,(lamlift/binding/name binding))
			  binding)))
	    (else
	     (call-with-values
		 (lambda () (walk-spine (lamlift/env/parent env)))
	       (lambda (ref binding)
		 (let* ((free (fetch env))
			(place (assq binding free)))
		   (if (not place)
		       (store! env (cons (list binding ref) free))
		       (set-cdr! place (cons ref (cdr place))))
		   (values ref binding))))))))

  (case kind
    ((ORDINARY)
     (traverse lamlift/env/free-ordinary-refs
	       set-lamlift/env/free-ordinary-refs!))
    ((OPERATOR)
     (traverse lamlift/env/free-operator-refs
	       set-lamlift/env/free-operator-refs!))
    (else
     (internal-error "Unknown reference kind" kind))))

(define (lamlift/binding/find bindings name)
  (let find ((bindings bindings))
    (and (not (null? bindings))
	 (let ((binding (car bindings)))
	   (if (not (eq? name (lamlift/binding/name (car bindings))))
	       (find (cdr bindings))
	       binding)))))

(define (lamlift/renames env names)
  (map (lambda (name)
	 (cons name
	       (if (not (lamlift/bound? env name))
		   name
		   (variable/rename name))))
       names))

(define (lamlift/rename-lambda-list lambda-list pairs)
  (map (lambda (token)
	 (let ((pair (assq token pairs)))
	   (if (not pair)
	       token
	       (cdr pair))))
       lambda-list))

(define (lamlift/bound? env name)
  (let loop ((env env))
    (and env
	 (or (lamlift/binding/find (lamlift/env/bound env) name)
	     (loop (lamlift/env/parent env))))))

(define (lamlift/let* keyword outer-env bindings body)
  (let* ((inner-env (lamlift/env/make
		     (binding-context-type keyword
					   (lamlift/env/context outer-env)
					   bindings)
		     outer-env
		     (map car bindings)))
	 (expr* `(,keyword
		    ,(lamlift/bindings
		      inner-env
		      (if (eq? keyword 'LETREC) inner-env outer-env)
		      bindings)
		  ,(lamlift/expr inner-env body))))
    (set-lamlift/env/form! inner-env expr*)
    expr*))

(define (lamlift/bindings binding-env body-env bindings)
  (map (lambda (binding)
	 (let ((name (car binding))
	       (value (cadr binding)))
	   (list
	    name
	    (if (not (LAMBDA/? value))
		(lamlift/expr body-env value)
		(call-with-values
		    (lambda ()
		      (lamlift/lambda** 'DYNAMIC ; bindings are dynamic
					body-env
					value))
		  (lambda (value* lambda-body-env)
		    (let ((binding
			   (or (lamlift/binding/find
				(lamlift/env/bound binding-env) name)
			       (internal-error "Missing binding" name))))
		      (set-lamlift/env/binding! lambda-body-env binding)
		      (set-lamlift/binding/value! binding lambda-body-env)
		      value*)))))))
       bindings))

(define (lamlift/analyze! env)
  (lamlift/decide-split! env)
  (lamlift/decide! env)
  ;;(bkpt 'about-to-rewrite)
  (lamlift/rewrite! env)
)

(define (lamlift/decide-split! env)
  (cond ((lamlift/env/binding env)	; This LAMBDA has a known binding
	 => (lambda (binding)
	      (if (lamlift/binding/operator-only? binding)
		  (set-lamlift/env/split?! env 'NO)))))
  (for-each lamlift/decide-split! (lamlift/env/children env)))

(define (lamlift/decide! env)
  (let ((form (lamlift/env/form env)))
    (cond ((or (eq? form #F)		; root env
	       (LET/? form))
	   (lamlift/decide!* (lamlift/env/children env)))
	  ((LETREC/? form)
	   (lamlift/decide/letrec! env))
	  ((LAMBDA/? form)
	   (lamlift/decide/lambda! env)
	   (lamlift/decide!* (lamlift/env/children env)))
	  (else
	   (internal-error "Unknown binding form" form)))))

(define (lamlift/decide!* envs)
  (for-each lamlift/decide! envs))

(define (lamlift/decide/lambda! env)
  (case (lamlift/env/split? env)
    ((NO YES)
     (set-lamlift/env/extended! env (lamlift/decide/imports env '())))
    ((UNNECESSARY)
     (set-lamlift/env/extended! env '()))
    (else
     (internal-error "Unknown split field" env))))

(define (lamlift/decide/imports env avoid)
  ;; Find the names of all free references in ENV except those in AVOID.
  ;; Requires that ?? all LAMBDA siblings already have their
  ;; LAMLIFT/ENV/EXTENDED slot calculated, as we have to pass their
  ;; extensions as well.  Note that the order of the result is
  ;; dependent of the order in which the references were accumulated
  ;; and so is not related to the orders of parameters in any
  ;; lambda-list.
  (define (filter-refs refs avoid)
    ;; Remove static bindings and members of AVOID from REFS
    (list-transform-negative refs
      (lambda (free-ref)
	(let ((binding (car free-ref)))
	  (or (lamlift/static-binding? binding)
	      (lamlift/binding-lifts-to-static-frame? binding)
	      (memq binding avoid))))))
  (union-map*
   (map (lambda (free-ref)
	  ;; Extract the name of the variable
	  (cadr (cadr free-ref)))
	(filter-refs (lamlift/env/free-ordinary-refs env)
		     '()))
   (lambda (free-ref)
     (let* ((binding  (car free-ref))
	    (value    (lamlift/binding/value binding)))
       ;; If this free reference is visibly bound to a LAMBDA
       ;; expression, then the free variables of that LAMBDA are also
       ;; free variables of this expression; otherwise, just the
       ;; variable itself.
       (if (not value)
	   (list (cadr (cadr free-ref)))
	   (lamlift/env/extended value))))
   (filter-refs (lamlift/env/free-operator-refs env)
		avoid)))

(define (lamlift/static-binding? binding)
  (and (eq? (lamlift/env/context (lamlift/binding/env binding)) 'STATIC)
       (not (pseudo-static-variable? (lamlift/binding/name binding)))))

(define (lamlift/binding-lifts-to-static-frame? binding)
  (let ((value (lamlift/binding/value binding)))
    (and value
	 (let ((drift-frame  (lamlift/env/drift-frame value)))
	   (and drift-frame
		(eq? (lamlift/env/context drift-frame) 'STATIC))))))



(define (lamlift/applicate! call reorder lambda-list var extra-args)
  (form/rewrite!
   call
   `(CALL (LOOKUP ,var)
	  ,@(reorder (append extra-args
			     (lambda-list/applicate call lambda-list
			      (call/cont-and-operands call)))))))

(define (lamlift/reorderer original final)
  ;; This is slow...
  (lambda (args)
    (let ((pairs (map list original args)))
      (map (lambda (final)
	     (cadr (assq final pairs)))
	   final))))

(define (lamlift/decide/letrec! letrec-env)

  (define (decide-remaining-children! child-bindings-done)
    (let ((children-done (map lamlift/binding/value child-bindings-done)))
      (for-each (lambda (child)
		  (lamlift/decide!* (lamlift/env/children child)))
	children-done)
      (lamlift/decide!*
       (delq* children-done (lamlift/env/children letrec-env)))))

  (let ((bound (lamlift/env/bound letrec-env)))
    ;; All these cases are optimizations.
    (cond ((null? bound)
	   (decide-remaining-children! '()))
	  ((and (eq? (lamlift/env/context letrec-env) 'STATIC)
		(for-all? bound
		  (lambda (binding)
		    (let ((env* (lamlift/binding/value binding)))
		      (eq? (lamlift/env/split? env*) 'NO)))))
	   ;; A static frame with none of the LAMBDAs appearing in
	   ;; operand position (i.e. no splitting)
	   (decide-remaining-children! bound))
	  ((eq? (lamlift/env/context letrec-env) 'STATIC)
	   (let ((splits (list-transform-negative bound
			   (lambda (binding)
			     (let ((env* (lamlift/binding/value binding)))
			       (eq? (lamlift/env/split? env*) 'NO))))))
	     (for-each
		 (lambda (binding)
		   (let ((env* (lamlift/binding/value binding)))
		     ;; No bindings need be added before lifting this,
		     ;; because all free references from a static frame
		     ;; are to static variables and hence lexically
		     ;; visible after lifting.
		     (set-lamlift/env/extended! env* '())))
	       splits)
	     (decide-remaining-children! splits)))
	  (else
	   (lamlift/decide/letrec!/dynamic-frame letrec-env)
	   (decide-remaining-children! bound)))))

(define (lamlift/decide/letrec!/dynamic-frame letrec-env)

  (define (letrec-binding? binding)
    (eq? (lamlift/binding/env binding) letrec-env))

  (define (letrec-self-references list-of-binding.reference)
    (list-transform-positive  list-of-binding.reference
      (lambda (binding.reference)
	(letrec-binding? (car binding.reference)))))

  (define (letrec-other-references list-of-binding.reference)
    (list-transform-negative  list-of-binding.reference
      (lambda (binding.reference)
	(letrec-binding? (car binding.reference)))))

  (define (make-adj-list list-of-binding.reference)
    (map (lambda (binding.reference)
	   (lamlift/binding/value (car binding.reference)))
	 (letrec-self-references list-of-binding.reference)))

  (define (lamlift/env/free-all-refs env)
    (append (lamlift/env/free-ordinary-refs env)
	    (lamlift/env/free-operator-refs env)))
    
  ;; remember that components are lists of nodes
  (define-integrable component-exemplar car)

  (let* ((nodes  (map lamlift/binding/value (lamlift/env/bound letrec-env)))

	 (reference-adj
	  (eq?-memoize
	   (lambda (node-env)
	     (make-adj-list  (lamlift/env/free-all-refs node-env)))))
	 (reference-components
	  (strongly-connected-components nodes reference-adj))
	 (reference-dag-adj (s-c-c->adj reference-components reference-adj))

	 (call-adj
	  (eq?-memoize
	   (lambda (node-env)
	     (make-adj-list (lamlift/env/free-operator-refs node-env)))))
	 (call-components  (strongly-connected-components nodes call-adj))
	 (call-dag-adj     (s-c-c->adj call-components call-adj)))

    (define (component-free-dynamic-names component-members)
      ;; calculate ordinary extended parameters
      (union-map*
       '()
       (lambda (node)
	 (lamlift/decide/imports node (lamlift/env/bound letrec-env)))
       component-members))

    (define (combine-names my-new-names callees-new-names)
      ;;* Ought to reorder CALLEES-NEW-NAMES to reduce amount of register
      ;;  shuffling and take into account existing arguments.
      ;;* This version ensures that the arguments passed to callees preceed the
      ;;  new extra arguments, and the new argument list is coherent with at
      ;;  least one callee.
      ;;* Alternatively we could add a new phase much later to reorder internal
      ;;  procedure parameter lists.
      (define (adjoin names set) (append set (delq* set names)))
      (adjoin my-new-names (fold-left adjoin '() callees-new-names)))

    (define (component-drift-frame-depth component maximum-from-dag-children)
      ;; Search for a drift frame by depth, picking the deepest frame that
      ;; imposes a restriction.

      (define (binding/drifted-frame-depth binding)
	;; Find the depth of a binding, taking into account that it might be a
        ;; binding to a lambda that was drifted up from some outer frame.
	(define (default) (lamlift/env/depth (lamlift/binding/env binding)))
	(let ((value   (lamlift/binding/value binding)))
	  (if value
	      (let  ((drift-frame  (lamlift/env/drift-frame value)))
		(if drift-frame
		    (lamlift/env/depth drift-frame)
		    (default)))
	      (default))))

      (define (maximum-over-binding.references-list list maximum)
	(if (null? list)
	    maximum
	    (maximum-over-binding.references-list
	     (cdr list)
	     (max maximum (binding/drifted-frame-depth (car (car list)))))))

      (define (node-maximum maximum node)
	(maximum-over-binding.references-list
	 (letrec-other-references (lamlift/env/free-ordinary-refs node))
	 (maximum-over-binding.references-list
	  (letrec-other-references (lamlift/env/free-operator-refs node))
	  maximum)))

      (fold-left node-maximum maximum-from-dag-children component))

    (let ((depth-of-static-frame
	   (lamlift/env/depth (lamlift/find-static-frame letrec-env))))
      ;; This has to be a walk, not a sum: COMPONENT-DRIFT-FRAME-DEPTH
      ;; (indirectly) uses the drift-frame slot, so this has to be set
      ;; immediately.
      (dfs-dag-walk reference-components reference-dag-adj
	(lambda (component children)
	  (let* ((children-depths
		  (map (lambda (c)
			 (lamlift/env/depth
			  (lamlift/env/drift-frame (component-exemplar c))))
		       children))
		 (drift-depth
		  (component-drift-frame-depth
		   component
		   (fold-left max depth-of-static-frame children-depths)))
		 (drift-frame
		  (lamlift/env/depth->frame letrec-env drift-depth)))
	    (for-each (lambda (node)
			(set-lamlift/env/drift-frame! node drift-frame))
		      component)))))

    (let ((component->extra-names
	   (dfs-dag-sum call-components call-dag-adj
	     (lambda (component callees-extendeds)
	       (combine-names (component-free-dynamic-names component)
			      callees-extendeds)))))
      (distribute-component-property
       call-components component->extra-names set-lamlift/env/extended!))))


(define (lamlift/env/find-frame start-env predicate?)
  (let loop ((env start-env))
    (cond ((not env)
	   (internal-error "Cant find frame satisfying" predicate? start-env))
	  ((predicate? env)
	   env)
	  (else
	   (loop (lamlift/env/parent env))))))

(define (lamlift/find-static-frame env)
  (define (static-frame? env)
    (eq? (lamlift/env/context env) 'STATIC))
  (lamlift/env/find-frame env static-frame?))

(define (lamlift/env/depth->frame env depth)
  (lamlift/env/find-frame env (lambda (e) (= depth (lamlift/env/depth e)))))

(define lamlift/lift!
  (lifter/make
   (lambda (env) (lamlift/env/form (lamlift/find-static-frame env)))))

(define (lamlift/rewrite! env)
  (let ((form (lamlift/env/form env)))
    (cond ((or (eq? form #F)		; root env
	       (LET/? form))
	   (lamlift/rewrite!* (lamlift/env/children env)))
	  ((LETREC/? form)
	   (lamlift/rewrite!* (lamlift/env/children env)))
	  ((LAMBDA/? form)
	   (lamlift/rewrite!* (lamlift/env/children env))
	   (lamlift/rewrite/lambda! env))
	  (else
	   (internal-error "Unknown binding form" form)))))

(define (lamlift/rewrite!* envs)
  (for-each lamlift/rewrite! envs))

(define (lamlift/rewrite/lambda! env)
  (if (not (eq? (lamlift/env/split? env) 'UNNECESSARY))
      (lamlift/rewrite/lambda/finish! env)))

(define (lamlift/rewrite/lambda/finish! env)
  (define (make-new-name)
    (lamlift/new-name
     (if (lamlift/env/binding env)
	 (lamlift/binding/name (lamlift/env/binding env))
	 'LAMBDA)))
  (let* ((form              (lamlift/env/form env))
	 (orig-lambda-list  (lambda/formals form))
	 (extra-formals     (lamlift/env/extended env))
	 (lifted-name       (make-new-name))
	 (split?            (or (not (eq? (lamlift/env/split? env) 'NO))
				(hairy-lambda-list? orig-lambda-list))))
    (let* ((lambda-list**
	    (append extra-formals (lambda-list->names orig-lambda-list)))
	   (lifted-lambda-list
	    ;; continuation variable always leftmost
	    (call-with-values
		(lambda ()
		  (list-split lambda-list** continuation-variable?))
	      (lambda (cont-vars other-vars)
		(let ((cont-vars 
		       ;; Discard ignored continuation variables
		       (list-transform-positive cont-vars
			 referenced-continuation-variable?)))
		  (if (or (null? cont-vars)
			  (not (null? (cdr cont-vars))))
		      (internal-error
		       "Creating LAMBDA with non-unique continuation"
		       env))
		  (append cont-vars other-vars))))))
      ;; If this LAMBDA expression has a name, find all call sites and
      ;; rewrite to pass additional arguments
      (cond ((lamlift/env/binding env)
	     => (lambda (binding)
		  (dbg-info/remember
		   (lamlift/binding/name binding)
		   (if (null? extra-formals)
		       lifted-name
		       `(CALL 'un-lambda-lift '#F (LOOKUP ,lifted-name))))
		  (let ((reorder
			 (lamlift/reorderer lambda-list** lifted-lambda-list)))
		    (for-each
			(lambda (call)
			  (lamlift/applicate!
			   call reorder orig-lambda-list lifted-name
			   (map (lambda (arg-name) `(LOOKUP ,arg-name))
				extra-formals)))
		      (lamlift/binding/calls binding))))))
      (let ((lifted-form `(LAMBDA ,lifted-lambda-list ,(lambda/body form)))
	    (stub-lambda
	     (lambda (body-lambda-name)
	       ;; Should be modified to preserve complete alpha renaming
	       `(LAMBDA ,orig-lambda-list
		  (CALL (LOOKUP ,body-lambda-name)
			,@(map (lambda (name)
				 (if (or *after-cps-conversion?*
					 (not (continuation-variable? name)))
				     `(LOOKUP ,name)
				     `(QUOTE #F)))
			       lifted-lambda-list)))))
            (lift-stub?
             (or 
              ;; The stub can drift to a static frame, the stub is named,
              ;; and there are operand uses that expect it to be in a static
	      ;; frame (because we did not add the static-liftable stubs to
	      ;; the extended parameter lists)
              (and (lamlift/env/drift-frame env)
		   (eq? (lamlift/env/context (lamlift/env/drift-frame env))
			'STATIC)
		   (lamlift/env/binding env)
		   (not (null? (lamlift/binding/operand-uses
				(lamlift/env/binding env)))))
	      ;; Add your favourite other reasons here:
	      lamlift/*lift-stubs-aggressively?*
	      #F))
	    (lift-to-drift-frame
	     (lambda (name lambda-form)
	       ((lifter/make 
		 (lambda (env)
		   (lamlift/env/form (lamlift/env/drift-frame env))))
		env name lambda-form))))
	     
	;; Rewrite the stub to call the split version with additional arguments
	(lamlift/split lifted-form form)
	(form/rewrite!
	 form
	 (cond (lift-stub?
		(let ((stub-name  (make-new-name))
		      (binding    (lamlift/env/binding env)))
		  (dbg-info/remember (lamlift/binding/name binding) stub-name)
		  (for-each
		      (lambda (reference)
			(form/rewrite! reference `(LOOKUP ,stub-name)))
		    (lamlift/binding/operand-uses binding))
		  (lift-to-drift-frame stub-name (stub-lambda lifted-name))
		  `(QUOTE #F)))
	       (split?
		(stub-lambda lifted-name))
	       (else `(QUOTE #F))))
	(lamlift/lift! env lifted-name lifted-form)))))
