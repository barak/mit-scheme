#| -*-Scheme-*-

$Id: envconv.scm,v 1.5 1994/11/26 22:06:52 gjr Exp $

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

;;;; Environment Converter
;;; package: (compiler midend)

(declare (usual-integrations))

;; ENVCONV replaces instances of
;;  (LOOKUP <name>)
;; where <name> is bound in a reified frame with either of
;; 1.
;;  (CALL (QUOTE ,%*lookup) (QUOTE #F) (LOOKUP ,env-variable)
;;	  (QUOTE <name>) (QUOTE <depth>) (QUOTE <offset>))
;;  where <depth> and <offset> represent the lexical address of the binding
;;  of <name> from the referencing frame.
;; 2.
;;  (CALL (QUOTE ,%variable-cache-ref) (QUOTE #F)
;;        (LOOKUP <cache-name>) (QUOTE <name>))
;;  where <cache-name> is a new variable bound to
;;  (CALL (QUOTE ,%make-read-variable-cache) (QUOTE #F)
;;        (LOOKUP ,env-variable) (QUOTE <name>))
;;
;; (UNASSIGNED? <name>), (SET! <name> <value>), and (CALL (LOOKUP <name>) ...)
;; are translated simiarly
;;
;; Variable references to variables bound in reified frames are considered
;; captured by closest reified frame to the frame in which the reference
;; occurs.  References to such captured variables may be implemented using
;; calls or variable caches.
;; The environment optimization level determines which of these frames
;; use variable cells:
;; A. If LOW, none.
;; B. If MEDIUM, only those whose context is TOP-LEVEL. (maybe ONCE-ONLY too?)
;; C. If HIGH, all.

;; Parameters

(define envconv/optimization-level 'MEDIUM)
(define envconv/variable-caches-must-be-static? true)
(define envconv/top-level-name (intern "#[top-level]"))

(define *envconv/compile-by-procedures?* false)
(define *envconv/procedure-result?* false)
(define *envconv/copying?*)
(define *envconv/separate-queue*)
(define *envconv/top-level-program*)
(define *envconv/debug/walking-queue* #F)

(define (envconv/top-level program)
  (fluid-let ((*envconv/copying?* false)
	      (*envconv/separate-queue* '())
	      (*envconv/top-level-program* program))
    (let ((result (envconv/trunk 'TOP-LEVEL program
				 (lambda (copy? program*)
				   copy? ; ignored
				   program*))))
      (fluid-let ((*envconv/debug/walking-queue* #T))
	(for-each envconv/do-compile!
	  (reverse *envconv/separate-queue*)))
      result)))

(define-macro (define-environment-converter keyword bindings . body)
  (let ((proc-name (symbol-append 'ENVCONV/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
	  (let ((handler (lambda ,(cons (car bindings) names) ,@body)))
	    (named-lambda (,proc-name env form)
	      (envconv/remember ,code
				form
				(envconv/env/block env)))))))))

;;;; Environment-sensitive forms

(define-environment-converter LOOKUP (env name)
  (envconv/new-reference env name `(LOOKUP ,name)))

(define-environment-converter UNASSIGNED? (env name)
  (envconv/new-reference env name `(UNASSIGNED? ,name)))

(define-environment-converter SET! (env name value)
  (let ((value* (envconv/expr-with-name env value name)))
    (envconv/new-reference env name `(SET! ,name ,value*))))

(define (envconv/lambda env form name)
  (let ((lambda-list (lambda/formals form))
	(body (lambda/body form)))
    (if (or (not (eq? (envconv/env/context env) 'TOP-LEVEL))
	    (not *envconv/compile-by-procedures?*)
	    *envconv/procedure-result?*
	    (eq? form *envconv/top-level-program*))
	(envconv/lambda* 'ARBITRARY env form)
	(envconv/compile-separately form name true env))))

(define (envconv/lambda* context* env form)
  (let ((lambda-list (lambda/formals form))
	(body (lambda/body form)))
    (let ((form*
	   (envconv/binding-body context*
				 env
				 ;; Ignore continuation
				 (cdr (lambda-list->names lambda-list)) 
				 body
				 (lambda (body*)
				   `(LAMBDA ,lambda-list
				      ,body*)))))
      (envconv/remember form*
			form
			(if (LAMBDA/? form*)
			    (let* ((body (lambda/body form*))
				   (body-info
				    (code-rewrite/original-form body)))
			      (cond ((not body-info) false)
				    ((new-dbg-procedure? body-info)
				     (new-dbg-block/parent
				      (new-dbg-procedure/block body-info)))
				    (else
				     (new-dbg-expression/block body-info))))
			    (envconv/env/block env))))))

(define-environment-converter LET (env bindings body)
  (let ((bindings* (lmap (lambda (binding)
			   (list (car binding)
				 (envconv/expr env (cadr binding))))
			 bindings)))
    (envconv/binding-body (let ((context (envconv/env/context env)))
			    (if (eq? context 'TOP-LEVEL)
				'ONCE-ONLY
				context))
			  env
			  (lmap car bindings)
			  body
			  (lambda (body*)
			    `(LET ,bindings*
			       ,body*)))))

;;;; Forms removed

(define-environment-converter THE-ENVIRONMENT (env)
  (envconv/env/reify! env)
  `(LOOKUP ,(envconv/env/reified-name env)))

(define-environment-converter ACCESS (env name envxpr)
  (cond ((equal? envxpr `(THE-ENVIRONMENT))
	 (envconv/lookup env `(LOOKUP ,name)))
	;; The linker cannot currently hack this
	((envconv/package-reference? envxpr)
	 (envconv/package-lookup (envconv/package-name envxpr) name))
	(else
	 `(CALL (QUOTE ,%*lookup)
		(QUOTE #F)
		,(envconv/expr env envxpr)
		(QUOTE ,name)
		;; No lexical information known
		(QUOTE #f)
		(QUOTE #f)))))

(define-environment-converter DEFINE (env name value)
  (let ((value* (envconv/expr-with-name env value name)))
    (cond ((not (envconv/env/parent env))
	   ;; Incremental at top-level
	   (envconv/env/reify! env)
	   `(CALL (QUOTE ,%*define)
		  (QUOTE #F)
		  (LOOKUP ,(envconv/env/reified-name env))
		  (QUOTE ,name)
		  ,value*))
	  ((envconv/env/locally-bound? env name)
	   (envconv/new-reference env name `(SET! ,name ,value*)))
	  (else
	   (internal-error "Unscanned definition encountered"
			   `(DEFINE ,name ,value))))))

#|
  (define-environment-converter IN-PACKAGE (env envxpr bodyxpr)
    (if (equal? envxpr `(THE-ENVIRONMENT))
	(envconv/expr env bodyxpr)
	(envconv/trunk/new (envconv/env/context env)
			   (envconv/expr env envxpr)
			   bodyxpr)))
|#

(define-environment-converter IN-PACKAGE (env env-expr body-expr)
  (if (equal? env-expr `(THE-ENVIRONMENT))
      (envconv/expr env body-expr)
      (envconv/split-subprogram
       (or (eq? (envconv/env/context env) 'ARBITRARY)
	   *envconv/copying?*)
       body-expr
       (envconv/expr env env-expr))))

;;;; Environment-insensitive forms

;; CALL is conceptually insensitive, but common cases are optimized.

(define-environment-converter CALL (env rator cont #!rest rands)
  (define (default)
    `(CALL ,(if (LAMBDA/? rator)
		(envconv/lambda*
		  (if (eq? (envconv/env/context env) 'ARBITRARY)
		      'ARBITRARY
		      'ONCE-ONLY)
		  env rator)
		(envconv/expr env rator))
	   ,(envconv/expr env cont)
	   ,@(envconv/expr* env rands)))

  (cond ((LOOKUP/? rator)
	 (let ((name (lookup/name rator)))
	   (envconv/new-reference
	    env
	    name
	    `(CALL ,(envconv/remember `(LOOKUP ,name)
				      rator
				      (envconv/env/block env))
		   ,(envconv/expr env cont)
		   ,@(envconv/expr* env rands)))))
	((ACCESS/? rator)
	 (if (not (envconv/package-reference? (access/env-expr rator)))
	     (default)
	     (begin
	       (envconv/env/reify-top-level! env)
	       (envconv/new-reference
		env
		envconv/top-level-name
		`(CALL ,(envconv/remember
			 `(ACCESS ,(access/name rator)
				  ,(envconv/expr env (access/env-expr rator)))
			 rator
			 (envconv/env/block env))
		       ,(envconv/expr env cont)
		       ,@(envconv/expr* env rands))))))
	(else
	 (default))))

(define-environment-converter BEGIN (env #!rest actions)
  `(BEGIN ,@(envconv/expr* env actions)))

(define-environment-converter IF (env pred conseq alt)
  `(IF ,(envconv/expr env pred)
       ,(envconv/expr env conseq)
       ,(envconv/expr env alt)))

(define-environment-converter OR (env pred alt)
  `(OR ,(envconv/expr env pred)
       ,(envconv/expr env alt)))

(define-environment-converter DELAY (env expr)
  `(DELAY ,(envconv/expr env expr)))

(define-environment-converter QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-environment-converter DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

;;;; Dispatcher

(define (envconv/expr-with-name env expr name)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (envconv/quote env expr))
    ((LOOKUP)
     (envconv/lookup env expr))
    ((LAMBDA)
     (envconv/lambda env expr name))
    ((DECLARE)
     (envconv/declare env expr))
    ((CALL)
     (envconv/call env expr))
    ((BEGIN)
     (envconv/begin env expr))
    ((IF)
     (envconv/if env expr))
    ((SET!)
     (envconv/set! env expr))
    ((UNASSIGNED?)
     (envconv/unassigned? env expr))
    ((OR)
     (envconv/or env expr))
    ((DELAY)
     (envconv/delay env expr))
    ((ACCESS)
     (envconv/access env expr))
    ((DEFINE)
     (envconv/define env expr))
    ((IN-PACKAGE)
     (envconv/in-package env expr))
    ((THE-ENVIRONMENT)
     (envconv/the-environment env expr))
#|
    ((LET)
     (envconv/let env expr))
|#
    ((LET LETREC)
     (not-yet-legal expr))
    (else
     (illegal expr))))

(define (envconv/expr env expr)
  (envconv/expr-with-name env expr #f))

(define (envconv/expr* env exprs)
  (lmap (lambda (expr)
	  (envconv/expr env expr))
	exprs))

(define (envconv/remember new old block)
  (call-with-values
   (lambda () (code-rewrite/original-form*/previous old))
   (lambda (available? dbg-info)
     (if available?
	 (if (new-dbg-procedure? dbg-info)
	     (begin
	       (if (not (new-dbg-procedure/block dbg-info))
		   (set-new-dbg-procedure/block! dbg-info block))
	       (code-rewrite/remember* new dbg-info))
	     (begin
	       (if (not (new-dbg-expression/block dbg-info))
		   (set-new-dbg-expression/block! dbg-info block))
	       (code-rewrite/remember* new dbg-info))))))
  new)

(define (envconv/split new old)
  (let ((old* (code-rewrite/original-form old)))
    (if old*
	(code-rewrite/remember* new
				(if (new-dbg-procedure? old*)
				    (new-dbg-procedure/copy old*)
				    old*)))
    new))

(define (envconv/new-name prefix)
  (new-variable prefix))

;;;; Environment utilities

(define-structure (envconv/env
		   (conc-name envconv/env/)
		   (constructor envconv/env/%make (context parent block)))
  (context false read-only true)
  (reified-name false read-only false)
  (depth (if parent
	     (1+ (envconv/env/depth parent))
	     0)
	 read-only true)
  (nearest-reified false read-only false)
  (parent false read-only true)
  (children '() read-only false)
  (bindings '() read-only false)
  (number 0 read-only false)
  (captured '() read-only false)
  (wrapper false read-only false)
  (body false read-only false)
  (result false read-only false)
  (block false read-only false))

(define-structure
    (envconv/binding
     (conc-name envconv/binding/)
     (constructor envconv/binding/make (name env number))
     (print-procedure
      (standard-unparser-method 'ENVCONV/BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write-string (symbol-name (envconv/binding/name binding)) port)))))

  (name false read-only true)
  (env false read-only true)
  (number false read-only true)
  (references '() read-only false))

(define-structure (envconv/separate-compilation-key
		   (conc-name envconv/key/)
		   (constructor envconv/key/make
				(form name procedure? env)))
  (form false read-only false)		; The form to compile later
  (name false read-only false)		; Name, if any, for procedures
  (procedure? false read-only false)	; Must generate a procedure?
  (env false read-only false))		; Environment when enqueued

(define (envconv/env/make context parent)
  (let ((env
	 (envconv/env/%make
	  context parent
	  (new-dbg-block/make (if (eq? context 'TOP-LEVEL)
				  'FIRST-CLASS
				  'NESTED)
			      (and parent
				   (envconv/env/block parent))))))
    (if parent
	(set-envconv/env/children! parent
				   (cons env (envconv/env/children parent))))
    env))

(define-integrable (envconv/env/reified? env)
  (envconv/env/reified-name env))

(define (envconv/env/reify! env)
  (if (not (envconv/env/reified? env))
      (let ((env-var (new-environment-variable)))
	(set-envconv/env/reified-name! env env-var)
	(let ((block (envconv/env/block env)))
	  (if block
	      (set-new-dbg-block/type! block 'FIRST-CLASS)))	      
	(let ((parent (envconv/env/parent env)))
	  (and parent
	       (envconv/env/reify! parent))))))

(define (envconv/env/reify-top-level! env)
  (if (not (envconv/env/reified? env))
      (let ((parent (envconv/env/parent env)))
	(if (not parent)
	    (envconv/env/reify! env)
	    (envconv/env/reify-top-level! parent)))))

(define (envconv/new-reference env name reference)
  (let ((binding (envconv/env/lookup! env name)))
    (set-envconv/binding/references!
     binding
     (cons (cons env reference)
	   (envconv/binding/references binding)))
    reference))

(define (envconv/env/lookup! env name)
  (let spine-loop ((frame env) (frame* false))
    (cond ((not frame)
	   (let* ((number (envconv/env/number frame*))
		  (binding (envconv/binding/make name frame* number)))
	     (set-envconv/env/number! frame* (1+ number))
	     (envconv/env/reify! frame*)
	     (set-envconv/env/bindings!
	      frame*
	      (cons binding (envconv/env/bindings frame*)))
	     binding))
	  ((envconv/env/lookup/local frame name))
	  (else
	   (spine-loop (envconv/env/parent frame) frame)))))

(define (envconv/env/lookup/local env name)
  (let rib-loop ((bindings (envconv/env/bindings env)))
    (cond ((null? bindings)
	   false)
	  ((eq? name (envconv/binding/name (car bindings)))
	   (car bindings))
	  (else
	   (rib-loop (cdr bindings))))))

(define (envconv/env/locally-bound? env name)
  (envconv/env/lookup/local env name))

#|
(define (envconv/trunk/new context envcode program)
  (envconv/trunk context program
   (lambda (copy? program*)
     (envconv/split-subprogram copy? program* envcode))))
|#

(define (envconv/trunk context program wrapper)
  (let* ((copying* (or (eq? context 'ARBITRARY) *envconv/copying?*))
	 (env (envconv/env/make 'TOP-LEVEL #f))
	 (result (fluid-let ((*envconv/copying?* copying*))
		   (envconv/expr env program)))
	 (needs? (or (envconv/env/reified? env)
		     (not (null? (envconv/env/bindings env)))))
	 (program*
	  (envconv/env/setup!
	   env result
	   (lambda (result)
	     (wrapper copying*
		      (if (not needs?)
			  result
			  `(LET ((,(envconv/env/reified-name env)
				  (CALL (QUOTE ,%fetch-environment)
					(QUOTE #F))))
			     ,result)))))))
    (envconv/remember program* program (envconv/env/block env))
    (envconv/process-root! env program*)))

(define (envconv/binding-body context* env names body body-wrapper)
  (let* ((env* (envconv/env/make context* env))
	 (body*
	  (begin
	    (let loop ((number 0)
		       (names* names)
		       (bindings '()))
	      (if (null? names*)
		  (let ((block (envconv/env/block env*)))
		    (if block
			(set-new-dbg-block/variables!
			 block
			 (map (lambda (name)
				(new-dbg-variable/make name block))
			      names)))		    
		    (set-envconv/env/bindings! env* bindings)
		    (set-envconv/env/number! env* number))
		  (loop (1+ number)
			(cdr names*)
			(cons (envconv/binding/make (car names*) env* number)
			      bindings))))
	    (envconv/expr env* body))))
    (envconv/env/setup!
     env* body*
     (if (not (envconv/env/reified? env*))
	 body-wrapper
	 (lambda (body*)
	   (body-wrapper
	    (envconv/bind-new-environment env* names body*)))))))

(define (envconv/env/setup! env result wrapper)
  (let ((result* (wrapper result)))
    (set-envconv/env/body! env result)
    (set-envconv/env/wrapper! env wrapper)
    (set-envconv/env/result! env result*)
    result*))

(define (envconv/bind-new-environment env* names body*)
  (bind (envconv/env/reified-name env*)
	`(CALL (QUOTE ,%*make-environment)
	       (QUOTE #F)
	       (LOOKUP ,(envconv/env/reified-name (envconv/env/parent env*)))
	       (QUOTE ,(list->vector (cons lambda-tag:make-environment
					   names)))
	       ,@(lmap (lambda (name)
			 `(LOOKUP ,name))
		       names))
	body*))

(define (envconv/process-root! top-level-env top-level-program)
  (if (envconv/env/reified? top-level-env)
      (begin
	(envconv/shorten-paths! top-level-env)
	(envconv/capture! top-level-env)
	(envconv/rewrite-references! top-level-env)))
  top-level-program)

(define (envconv/shorten-paths! env)
  (set-envconv/env/nearest-reified!
   env
   (if (envconv/env/reified? env)
       env
       (envconv/env/nearest-reified (envconv/env/parent env))))
  (for-each envconv/shorten-paths! (envconv/env/children env)))    

(define (envconv/capture! env)
  (if (envconv/env/reified? env)
      (begin
	(for-each
	 (lambda (binding)
	   (let loop ((refs (envconv/binding/references binding)))
	     (if (not (null? refs))
		 (let* ((ref   (car refs))
			(env*  (envconv/env/nearest-reified (car ref)))
			(place (assq binding (envconv/env/captured env*))))
		   (if (not place)
		       (set-envconv/env/captured!
			env*
			(cons (list binding (cdr ref))
			      (envconv/env/captured env*)))
		       (set-cdr! place
				 (cons  (cdr ref) (cdr place))))
		   (loop (cdr refs))))))
	 (envconv/env/bindings env))
	(for-each envconv/capture! (envconv/env/children env)))))

(define (envconv/rewrite-references! env)
  (if (envconv/env/reified? env)
      (begin
	(if (not (null? (envconv/env/captured env)))
	    (let ((process-captures!
		   (case envconv/optimization-level
		     ((LOW) envconv/use-calls!)
		     ((MEDIUM)
		      (if (envconv/medium/cache? (envconv/env/context env))
			  envconv/use-caches!
			  envconv/use-calls!))
		     ((HIGH) envconv/use-caches!)
		     (else
		      (configuration-error "Illegal switch setting"
					   'ENVCONV/OPTIMIZATION-LEVEL
					   envconv/optimization-level)))))
	      (process-captures! env)))
	(for-each envconv/rewrite-references! (envconv/env/children env)))))

(define (envconv/medium/cache? context)
  (eq? context 'TOP-LEVEL))

(define (envconv/use-calls! env)
  (let ((env-name (envconv/env/reified-name env)))
    (for-each
     (lambda (capture)
       (let ((binding (car capture)))
	 (let ((var-name (envconv/binding/name binding))
	       (binding-env (envconv/binding/env binding)))
	   (let* ((depth (and (envconv/env/parent binding-env)
			      (- (envconv/env/depth env)
				 (envconv/env/depth binding-env))))
		  (offset (and depth (envconv/binding/number binding))))
	     (for-each
	      (lambda (reference)
		(let ((simple-var
		       (lambda ()
			 `(CALL (QUOTE ,%*lookup)
				(QUOTE #f)
				(LOOKUP ,env-name)
				(QUOTE ,var-name)
				(QUOTE ,depth)
				(QUOTE ,offset)))))
		  (form/rewrite!
		   reference
		   (case (car reference)
		     ((LOOKUP)
		      (simple-var))
		     ((SET!)
		      `(CALL (QUOTE ,%*set!)
			     (QUOTE #F)
			     (LOOKUP ,env-name)
			     (QUOTE ,var-name)
			     ,(set!/expr reference)
			     (QUOTE ,depth)
			     (QUOTE ,offset)))
		     ((UNASSIGNED?)
		      `(CALL (QUOTE ,%*unassigned?)
			     (QUOTE #F)
			     (LOOKUP ,env-name)
			     (QUOTE ,var-name)
			     (QUOTE ,depth)
			     (QUOTE ,offset)))
		     ((CALL)
		      (let ((rator (call/operator reference)))
			(case (car rator)
			  ((LOOKUP)
			   (form/rewrite! rator (simple-var)))
			  ((ACCESS)
			   ;; Only done for packages
			   (form/rewrite!
			    rator
			    (envconv/package-lookup
			     (envconv/package-name (access/env-expr rator))
			     (access/name rator))))
			  (else
			   (internal-error "Unknown reference kind"
					   reference))))
		      reference)
		     (else
		      (internal-error "Unknown reference kind"
				      reference))))))
	      (cdr capture))))))
     (envconv/env/captured env))))

(define (envconv/use-caches! env)
  (let ((env-name (envconv/env/reified-name env)))
    (define (local-operator-variable-cache-maker ignore name arity)
      ignore				; ignored
      `(CALL (QUOTE ,%make-operator-variable-cache)
	     (QUOTE #F)
	     (LOOKUP ,env-name)
	     (QUOTE ,name)
	     (QUOTE ,arity)))

    (define (remote-operator-variable-cache-maker package-name name arity)
      `(CALL (QUOTE ,%make-remote-operator-variable-cache)
	     (QUOTE #F)
	     (QUOTE ,package-name)
	     (QUOTE ,name)
	     (QUOTE ,arity)))

    (define (read-variable-cache-maker name)
      `(CALL (QUOTE ,%make-read-variable-cache)
	     (QUOTE #F)
	     (LOOKUP ,env-name)
	     (QUOTE ,name)))

    (define (write-variable-cache-maker name)
      `(CALL (QUOTE ,%make-write-variable-cache)
	     (QUOTE #F)
	     (LOOKUP ,env-name)
	     (QUOTE ,name)))

    (define (new-cell! kind name maker)
      (let ((place (assq name (cdr kind))))
	(if place
	    (cadr place)
	    (let ((cell-name
		   (envconv/new-name (symbol-append name (car kind)))))
	      (declare-variable-property! cell-name '(VARIABLE-CELL))
	      (set-cdr! kind
			(cons (list name cell-name (maker name))
			      (cdr kind)))
	      cell-name))))

    (define (new-operator-cell! name arity refs by-arity maker extra)
      (define (new-cell!)
	(let ((cell-name
	       (envconv/new-name
		(symbol-append name '-
			       (string->symbol (number->string arity))
			       (car refs)))))
	  (declare-variable-property! cell-name '(VARIABLE-CELL))
	  (set-cdr! refs
		    (cons (list name cell-name
				(maker extra name arity))
			  (cdr refs)))
	  cell-name))
      
      (let ((place (assq name (cdr by-arity))))
	(if (not place)
	    (let ((cell-name (new-cell!)))
	      (set-cdr! by-arity
			(cons (list name (cons arity cell-name))
			      (cdr by-arity)))
	      cell-name)
	    (let ((place* (assq arity (cdr place))))
	      (if (not place*)
		  (let ((cell-name (new-cell!)))
		    (set-cdr! place
			      (cons (cons arity cell-name) (cdr place)))
		    cell-name)
		  (cdr place*))))))

    (let ((read-refs (list '-READ-CELL))
	  (write-refs (list '-WRITE-CELL))
	  (exe-refs (list '-EXECUTE-CELL))
	  (exe-by-arity (list 'EXE-BY-ARITY))
	  (remote-exe-refs (list '-REMOTE-EXECUTE-CELL))
	  (remote-exe-by-package '()))

      (for-each
       (lambda (capture)
	 (let ((binding (car capture)))
	   (let ((var-name (envconv/binding/name binding)))
	     (for-each
	      (lambda (reference)
		(form/rewrite!
		    reference
		  (case (car reference)
		    ((LOOKUP)
		     (let ((cell-name
			    (new-cell! read-refs var-name
				       read-variable-cache-maker)))
		       `(CALL (QUOTE ,%variable-cache-ref)
			      (QUOTE #F)
			      (LOOKUP ,cell-name)
			      (QUOTE ,var-name))))
		    ((SET!)
		     (let ((write-cell-name
			    (new-cell! write-refs var-name
				       write-variable-cache-maker))
			   (read-cell-name
			    (new-cell! read-refs var-name
				       read-variable-cache-maker))
			   (temp-name (envconv/new-name var-name)))
		       (bind temp-name
			     `(CALL (QUOTE ,%safe-variable-cache-ref)
				    (QUOTE #F)
				    (LOOKUP ,read-cell-name)
				    (QUOTE ,var-name))
			     `(BEGIN
				(CALL (QUOTE ,%variable-cache-set!)
				      (QUOTE #F)
				      (LOOKUP ,write-cell-name)
				      ,(set!/expr reference)
				      (QUOTE ,var-name))
				(LOOKUP ,temp-name)))))
		    ((UNASSIGNED?)
		     (let ((cell-name (new-cell! read-refs var-name
						 read-variable-cache-maker)))
		       `(CALL (QUOTE ,%unassigned?)
			      (QUOTE #F)
			      (CALL (QUOTE ,%safe-variable-cache-ref)
				    (QUOTE #F)
				    (LOOKUP ,cell-name)
				    (QUOTE ,var-name)))))

		    ((CALL)
		     (let ((rator (call/operator reference)))
		       (define (operate %invoke name refs by-arity maker extra)
			 (let* ((arity (length (cdddr reference)))
				(cell-name
				 (new-operator-cell!
				  name
				  arity
				  refs by-arity maker extra)))
			   (form/rewrite! rator `(LOOKUP ,cell-name))
			   `(CALL (QUOTE ,%invoke)
				  ,(call/continuation reference)
				  (QUOTE (,name ,arity))
				  ,rator
				  ,@(cdddr reference))))

		       (case (car rator)
			 ((LOOKUP)
			  (operate %invoke-operator-cache
				   var-name exe-refs exe-by-arity
				   local-operator-variable-cache-maker
				   false))
			 ((ACCESS)
			  (let ((package (envconv/package-name
					  (access/env-expr rator))))
			    (operate
			     %invoke-remote-cache
			     (access/name rator) remote-exe-refs
			     (or (assoc package remote-exe-by-package)
				 (let ((new (list package)))
				   (set! remote-exe-by-package
					 (cons new remote-exe-by-package))
				   new))
			     remote-operator-variable-cache-maker
			     package)))
			 (else
			  (internal-error "Unknown reference kind"
					  reference)))))
		    (else
		     (internal-error "Unknown reference kind"
				     reference)))))
	      (cdr capture)))))
	(envconv/env/captured env))

      ;; Rewrite top-level to bind caches, separately compile, and
      ;; copy if necessary, according to context.
      (form/rewrite! (envconv/env/result env)
		     ((envconv/env/wrapper env)
		      (envconv/wrap-with-cache-bindings
		       env
		       (append (cdr read-refs)
			       (cdr write-refs)
			       (cdr exe-refs)
			       (cdr remote-exe-refs))
		       (let ((form (envconv/env/body env)))
			 (envconv/split (form/preserve form)
					form))))))))

(define (envconv/wrap-with-cache-bindings env cells body)
  (let ((body*
	 `(CALL (LAMBDA (,(new-continuation-variable) ,@(lmap cadr cells))
		  ,body)
		(QUOTE #F)
		,@(lmap caddr cells))))
    (if (or (eq? (envconv/env/context env) 'TOP-LEVEL)
	    (not envconv/variable-caches-must-be-static?))
	body*
	(envconv/split-subprogram
	 (eq? (envconv/env/context env) 'ARBITRARY)
	 `(LET ((,(envconv/env/reified-name env)
		 (CALL (QUOTE ,%fetch-environment) (QUOTE #F))))
	    ,body*)
	 `(LOOKUP ,(envconv/env/reified-name env))))))

(define (envconv/split-subprogram copy? program envcode)
  (let ((program* (envconv/compile-separately program #f #f #f)))
    `(CALL (QUOTE ,%execute)
	   (QUOTE #F)
	   ,(if copy?
		`(CALL (QUOTE ,%copy-program) (QUOTE #F) ,program*)
		program*)
	   ,envcode)))

(define (envconv/compile-separately form name procedure? env)
  (let* ((form* `(QUOTE ,form))
	 (key   (envconv/key/make form* name procedure? env)))
    ;;(if *envconv/debug/walking-queue*
    ;;	(internal-error
    ;;	 "ENVCONV/COMPILE-SEPARATELY: Walking queue" key))
    (set! *envconv/separate-queue*
	  (cons key *envconv/separate-queue*))
    form*))

(define (envconv/do-compile! key)
  ;; *** Worry about debugging info propagation ***
  ;; It should not be difficult since it performs a single traversal
  ;; through the compiler.  However, the sequence of transforms
  ;; needs to be collected and integrated into the current one.
  ;; KEY is (form procedure? . name)
  (let ((form (envconv/key/form key))
	(procedure? (envconv/key/procedure? key))
	(name (envconv/key/name key))
	(env  (envconv/key/env key)))
    (call-with-values
     (lambda ()
       (compile-recursively (quote/text form) procedure? name))
     (lambda (compiled must-be-called?)
       (if must-be-called?
	   (let ((env-var-name
		  (and env (envconv/env/reified-name env))))
	     (if env-var-name
		 (let ((proc-name (envconv/new-name
				   (or name 'ENVCONV-PROCEDURE))))
		   (form/rewrite! form
		     `(LET ((,proc-name (QUOTE ,compiled)))
			(CALL (LOOKUP ,proc-name)
			      (QUOTE #F)
			      (LOOKUP ,env-var-name)))))
		 (internal-error
		  "ENVCONV/DO-COMPILE!: environment not reified"
		  key)))
	   (form/rewrite! form `(QUOTE ,compiled)))))))

;; The linker knows how to make global operator references,
;; but could be taught how to make arbitrary package references.
;; *** IMPORTANT: These must be captured! ****

(define %system-global-environment #f)

(define (envconv/package-reference? expr)
  (equal? expr `(QUOTE ,%system-global-environment)))

(define (envconv/package-name expr)
  expr					; ignored
  #f)

(define (envconv/package-lookup package name)
  package				; ignored
  `(CALL (QUOTE ,%*lookup)
	 (QUOTE #F)
	 (QUOTE ,%system-global-environment)
	 (QUOTE ,name)
	 (QUOTE #f)
	 (QUOTE #f)))