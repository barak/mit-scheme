#| -*-Scheme-*-

$Id: 664ce98c493e9df73ae6c820f38fb013a33e30d6 $

Copyright (c) 1995, 1999 Massachusetts Institute of Technology

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

;;;; Constant folder for closure and stack closure indices
;;; package: (compiler midend)

(declare (usual-integrations))

(define (frag/top-level program)
  (frag/expr program))

(define-macro (define-fragmenter keyword bindings . body)
  (let ((proc-name (symbol-append 'FRAG/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,names ,@body)))
	     (NAMED-LAMBDA (,proc-name FORM)
	       (FRAG/REMEMBER ,code FORM))))))))

(define-fragmenter LOOKUP (name)
  `(LOOKUP ,name))

(define (frag/body expr)
  (cond ((LOOKUP/? expr) (frag/expr expr))
	((QUOTE/? expr)  (frag/expr expr))
	((LAMBDA/? expr) (frag/expr expr))
	((form/static? expr) (form/copy expr))
	(else
	 (let ((body-var  (new-variable 'BODY)))
	   `(LET ((,body-var
		   (LAMBDA  (,(frag/ignored-continuation))
		     ,(frag/expr expr))))
	      (CALL (LOOKUP ,body-var) '#F))))))

(define-fragmenter LAMBDA (lambda-list body)
  `(LAMBDA ,lambda-list
     ,(frag/body body)))

(define-fragmenter LET (bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (frag/expr (cadr binding))))
	      bindings)
     ,(if (for-all? bindings
	    (lambda (b)
	      (or (pseudo-static-variable? (car b))
		  (form/static? (cadr b)))))
	  (frag/expr body)
	  (frag/body body))))

(define-fragmenter LETREC (bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (frag/expr (cadr binding))))
		 bindings)
     ,(frag/body body)))

(define-fragmenter IF (pred conseq alt)
  (frag* (list pred conseq alt)
	 (lambda (parts*)
	   `(IF ,@parts*))))

(define-fragmenter QUOTE (object)
  `(QUOTE ,object))

(define-fragmenter DECLARE (#!rest anything)
  `(DECLARE ,@anything))

(define-fragmenter BEGIN (#!rest actions)
  (frag* actions
	 (lambda (actions*)
	   `(BEGIN ,@actions*))))

(define-fragmenter CALL (rator cont #!rest rands)
  (let ((parts  (cons* rator cont rands)))
    (frag* parts
	   (lambda (parts*)
	     `(CALL ,@parts*)))))


(define (frag/expr expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (frag/quote expr))
    ((LOOKUP)   (frag/lookup expr))
    ((LAMBDA)   (frag/lambda expr))
    ((LET)      (frag/let expr))
    ((DECLARE)  (frag/declare expr))
    ((CALL)     (frag/call expr))
    ((BEGIN)    (frag/begin expr))
    ((IF)       (frag/if expr))
    ((LETREC)   (frag/letrec expr))
    (else       (illegal expr))))

(define (frag* exprs receiver)
  (let* ((names  (map (lambda (e)
		       (if (or (QUOTE/? e) (LOOKUP/? e) (LAMBDA/? e)
			       (form/static? e))
			   #F
			   (new-variable 'FRAG)))
		     exprs))
	(bds (list-transform-positive
		 (map (lambda (n r)
			(and n
			     `(,n (LAMBDA (,(frag/ignored-continuation))
				    ,(frag/expr r)))))
		      names exprs)
	       identity-procedure))
	(exprs*  (map (lambda (n r)
			(if n
			    `(CALL (LOOKUP ,n) '#F)
			    (frag/expr r)))
		      names exprs)))
    (if (null? bds)
	(receiver exprs*)
	`(LET ,bds ,(receiver exprs*)))))

(define (frag/remember new old)
  (code-rewrite/remember new old))

(define (frag/ignored-continuation)
  (new-continuation-variable))


(define (frag/worth-while? expr)
  (define (worth-while? expr)
    (if (not (pair? expr))
	(illegal expr))
    (case (car expr)
      ((QUOTE)    #F)
      ((LOOKUP)   #F)
      ((DECLARE)  #F)
      ((LAMBDA)   (or (hairy-lambda-list? (lambda/formals expr))
		      (worth-while? (lambda/body expr))))
      ((LET)      (let-like (let/bindings expr) (let/body expr)))
      ((LETREC)   (let-like (letrec/bindings expr) (letrec/body expr)))
      ((CALL)     (worth-while?* (cdr expr)))
      ((BEGIN)    (worth-while?* (cdr expr)))
      ((IF)       (worth-while?* (cdr expr)))
      (else       (illegal expr))))
  (define (let-like bindings body)
    (or	(worth-while? body)
	(there-exists? bindings (lambda (b) (worth-while? (second b))))))
  (define (worth-while?* exprs)
    (there-exists? exprs worth-while?))
  (worth-while? expr))


(define-structure
    (specializer/info
     (conc-name specializer/info/)
     (constructor specializer/info/make (name lambda letrec)))
  (name #F read-only true)		; binding name
  (lambda #F read-only true)		; lambda expression
  (letrec #F read-only true)		; the LETREC in which binding occurs
  (specializations '())			; list((key name lambda*))
  )

(define *specialization-table*) ; label->specializer/info
(define *lambda-queue*)



(define (specialize/enqueue-lambda! form)
  (a:pp `(queue-lambda: ,form))
  (if (not (LAMBDA/? form))
      (internal-error "not a lambda:" form))
  (queue/enqueue! *lambda-queue* form))

(define-macro (define-specializer keyword bindings . body)
  (let ((proc-name (symbol-append 'SPECIALIZER/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     ENV			; in case not used
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
	       ,code)))))))

(define-specializer QUOTE (object)
  object
  unspecific)

(define-specializer LOOKUP (name)
  name
  (let  ((place (assq name env)))
    (if place
	(form/rewrite! form (form/preserve (cdr place)))))
  unspecific)

(define-specializer LAMBDA (lambda-list body)
  (let ((env*
	 (map* env
	       (lambda (name) (cons name `(LOOKUP ,name)))
	       (lambda-list->names lambda-list))))
    (specialize/expr! env* body))
  (if (hairy-lambda-list? lambda-list)
      (specialize/enqueue-lambda! form)))

(define-specializer LET (bindings body)
  (let ((env*
	 (map* env (lambda (b) (cons (car b) `(LOOKUP ,(car b)))) bindings)))
    (for-each (lambda (binding)
		(specialize/expr! env (cadr binding)))
      bindings)
    (specialize/expr! env* body)))

(define-specializer LETREC (bindings body)
  (let ((env*
	 (map* env (lambda (b) (cons (car b) `(LOOKUP ,(car b)))) bindings)))
    (for-each (lambda (binding)
		(specialize/expr! env* (cadr binding)))
      bindings)
    (specialize/expr! env* body)
    (if (specialize/simple? body)
	(form/rewrite! form body))))

(define-specializer IF (pred conseq alt)
  (specialize/expr! env pred)
  (cond ((equal? pred '(QUOTE #F))
	 (specialize/expr! env alt)
	 (form/rewrite! form alt))
	((QUOTE/? pred)
	 (specialize/expr! env conseq)
	 (form/rewrite! form conseq))
	(else
	 (specialize/expr! env conseq)
	 (specialize/expr! env alt))))

(define-specializer DECLARE (#!rest anything)
  anything
  unspecific)

(define-specializer BEGIN (#!rest actions)
  (for-each (lambda (action)
	      (specialize/expr! env action))
    actions))

(define-specializer CALL (rator cont #!rest rands)
  cont
  (specialize/expr! env rator)
  (for-each (lambda (expr)
	      (specialize/expr! env expr))
    rands)
  (cond ((and (QUOTE/? rator)
	      (specializer/rewrite? (quote/text rator)))
	 => (lambda (handler!)
	      (apply handler! form rands)))
	((and (LOOKUP/? rator)
	      (hash-table/get *specialization-table* (lookup/name rator) #F))
	 => (lambda (info)
	      (specialize-call! info env form rands)))
	(else unspecific)))


(define *id* 0)
(define (make-id) (set! *id* (+ *id* 1)) *id*)

(define (specialize-call! info env form rands)
  (define (extract-parameter-placeholders form so-far)
    (cond ((and (placeholder? form)
		(not (contains-placeholder? (placeholder/value form))))
	   ;; Dont extract placeholders which are constants
	   so-far)
	  ;;((memq form so-far) so-far)
	  ((placeholder? form)
	   (if (placeholder/name form)
	       (cons form so-far)
	       (extract-parameter-placeholders (placeholder/value form) so-far)))
	  ((pair? form)
	   (extract-parameter-placeholders
	    (cdr form)
	    (extract-parameter-placeholders (car form) so-far)))
	  (else so-far)))
  (define (immutable? v)
    (or (number? v) (object-type? v (object-type #F)) (char? v)))
  (define (substitute/1? form)		; propogate info
    (or (PLACEHOLDER-QUOTE/? form)
	(and (QUOTE/? form)
	     (immutable? (quote/text form)))))
  (define (substitute/2? form)		; keep parameters
    (or (and (PLACEHOLDER-QUOTE/? form)
	     (not (placeholder?
		   (placeholder/value (placeholder-quote/object form)))))
	(and (QUOTE/? form)
	     (immutable? (quote/text form)))))

  (let* ((id (make-id))
	 (lam-expr  (specializer/info/lambda info))
	 (formals   (lambda/formals lam-expr))
	 (body      (lambda/body lam-expr)))
    (if (and (there-exists? rands PLACEHOLDER-QUOTE/?)
	     (not (hairy-lambda-list? formals))
	     (= (length rands) (length (cdr formals)))) ; paranoia
	
	(let* ((names*  (map variable/rename formals))
	       (cont*   (car names*))
	       (ph*     (extract-parameter-placeholders rands '()))
	       (ph-env* (map (lambda (p)
			       (cons p `(LOOKUP ,(new-variable (placeholder/name p)))))
			     ph*))
	       (env*    (map* (cons (cons (car formals) `(LOOKUP ,cont*))
				    ph-env*)
			      (lambda (f n v)
				(if (substitute/1? v)
				    (cons f v)
				    (cons f `(LOOKUP ,n))))
			      (cdr formals)
			      (cdr names*)
			      rands))
	       (body*   (form/copy body)))
	  (a:pp `(,id old-body: ,body))
	  (a:pp `(,id parameter-placeholders: ,@ph*))
	  (specialize/expr! env* body*)
	  (a:pp `(,id new-body: ,body*))
	  (cond ((QUOTE/? body*)
		 (form/rewrite! form body*))
		((PLACEHOLDER-QUOTE/? body*)
		 (form/rewrite! form body*))
		(else
		 (let* ((procedure-name
			 (variable/rename (specializer/info/name info)))
			(placeholders (remove-placeholders! env* body*))
			;; make new lambda list & call expressions
			(actuals
			 (append (list-transform-negative rands substitute/2?)
				 ;;(map (lambda (p) (cdr (assq p env)))
				 ;;     placeholders)
				 (map quote-placeholder placeholders)))
			(new-formals
			 (cons cont*
			       (let loop ((rs rands) (fs (cdr names*)))
				 (cond ((null? rs)
					(map (lambda (p)
					       (lookup/name (cdr (assq p env*))))
					     placeholders))
				       ((substitute/2? (car rs))
					(a:pp `(elide-arg: ,(car fs) ,(car rs)))
					(loop (cdr rs) (cdr fs)))
				       (else
					(cons (car fs) (loop (cdr rs) (cdr fs)))))))))
		   (a:pp `(,id parameter-placeholders: ,placeholders))
		   (a:pp `(,id rands: ,rands names*: ,names*))
		   (a:pp `(,id new-formals: ,new-formals))
		   (form/rewrite! form
		     `(CALL (LOOKUP ,procedure-name)
			    '#F
			    ,@actuals))
		   (a:pp `(call: ,form))
		   (remember-specialization!
		    info
		    procedure-name
		    `(LAMBDA ,new-formals ,body*))))))
	(a:pp `(declined: ,rands)))))


(define (remember-specialization! info proc-name lam-expr)
  (a:pp `(remember-specialization! ,info ,proc-name ,lam-expr))
  (let ((letrec-form  (specializer/info/letrec info)))
    (set-car! (cdr letrec-form)
	      (cons (list proc-name lam-expr)
		    (cadr letrec-form)))))
  
(define (remove-placeholders! env form) ; -> list (placeholder)
  ;; remove placeholders, replacing with new names.
  ;;  Return alist from placeholders to name
  (let walk ((form form) (ps '()))
    (cond ((PLACEHOLDER-QUOTE/? form)
	   (let ((placeholder  (placeholder-quote/object form)))
	     (cond ((symbol? (placeholder/name placeholder))
		    ;; named placeholder: rewrite as lookup.
		    (let ((pair  (assq placeholder env)))
		      (if (not pair)
			  (internal-error "Not bound" form env))
		      (set-placeholder/name-used?! placeholder #T)
		      (form/rewrite! form (cdr pair))
		      (if (memq placeholder ps) ps (cons placeholder ps))))
		   ((pair? (placeholder/name placeholder))
		    ;; An expression residual: substitute & recurse
		    (set-placeholder/name-used?! placeholder #T)
		    (form/rewrite! form
		      (tree-copy (placeholder/name placeholder)))
		    (walk form
			  (if (memq placeholder ps) ps (cons placeholder ps))))
		   ((placeholder? (placeholder/value placeholder))
		    ;; unnamed placeholder: a `constructed' residual
		    (nasty-residual placeholder))
		   ((contains-placeholder? (placeholder/value placeholder))
		    (nasty-residual placeholder))
		   (else
		    (form/rewrite! form
		      `(QUOTE ,(placeholder/value placeholder)))
		    ps))))
	  ((QUOTE/? form) ps)
	  ((LOOKUP/? form) ps)
	  ((LAMBDA/? form) (walk (lambda/body form) ps))
	  ((or (LET/? form) (LETREC/? form))
	   (let loop ((bds (second form)) (ps ps))
	     (if (null? bds)
		 (walk (third form) ps)
		 (loop (cdr bds) (walk (cadr (first bds)) ps)))))
	  (else
	   (let loop ((forms (cdr form)) (ps ps))
	     (if (null? forms)
		 ps
		 (loop (cdr forms) (walk (car forms) ps))))))))


(define (contains-placeholder? datum)
  (cond ((placeholder? datum)  #T)
	((pair? datum)
	 (or (contains-placeholder? (car datum))
	     (contains-placeholder? (cdr datum))))
	((eq? datum '())        #F)
	((eq? datum #F)         #F)
	((eq? datum #T)         #F)
	((eq? datum unspecific) #F)
	((number? datum)        #F)
	((symbol? datum)        #F)
	((string? datum)        #F)
	(else #T)))			; conservative approximation


(define (specialize/simple? expr)
  (or (PLACEHOLDER-QUOTE/? expr)
      (QUOTE/? expr)))

(define (specialize/expr! env expr)
  ;; Rewrite EXPR.
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((PLACEHOLDER-QUOTE) unspecific)
    ((QUOTE)    (specializer/quote env expr))
    ((LOOKUP)   (specializer/lookup env expr))
    ((LAMBDA)   (specializer/lambda env expr))
    ((LET)      (specializer/let env expr))
    ((DECLARE)  (specializer/declare env expr))
    ((CALL)     (specializer/call env expr))
    ((BEGIN)    (specializer/begin env expr))
    ((IF)       (specializer/if env expr))
    ((LETREC)   (specializer/letrec env expr))
    (else       (illegal expr))))

;;  Placeholders `wrap' every pointer in a placeholder value.
;;  They are escaped in the source with a PLACEHOLDER-QUOTE form.

(define (PLACEHOLDER-QUOTE/? form)
  (and (pair? form)
       (eq? (car form) 'PLACEHOLDER-QUOTE)))

(define (placeholder-quote/object form)
  (if (not (PLACEHOLDER-QUOTE/? form))
      (internal-error "placeholder-quote/object of" form))
  (second form))

(define (quote-placeholder placeholder)
  (if (not (placeholder? placeholder))
      (internal-error "not a placeholder:" placeholder))
  `(PLACEHOLDER-QUOTE ,placeholder))

(define-structure
    (placeholder
     (conc-name placeholder/)
     (constructor %make-placeholder (name)))
  (name  #F read-only true)		; #F or name of variable
  ;; either this placeholder (a self-reference), or a structure containing
  ;; placeholders or a simple (non-container) constant.
  (value #F read-only false)
  ;; A flag - is this residual used at all the specialized code?
  (name-used? #F read-only false))

(define (make-placeholder name #!optional value)
  (let ((p  (%make-placeholder name)))
    (if (default-object? value)
	(set-placeholder/value! p p)
	(set-placeholder/value! p value))
    p))

(define (arity/top-level program)
  ;; These should be put in a fluid-let when debugging is done:
  (set! *specialization-table* (make-eq-hash-table))
  (set! *lambda-queue* (queue/make))
  (set! *id* 0)
  (let ((program* (copier/top-level program (lambda (old new) new))))
    (let walk ((expr program*))
      ;; Find all interesting lambdas and keeping LETREC bindings.
      (cond ((LETREC/? expr)
	     (walk (letrec/body expr))
	     (for-each
		 (lambda (binding)
		   (hash-table/put! *specialization-table*
				    (car binding)
				    (specializer/info/make
				     (car binding)
				     (cadr binding)
				     expr))
		   (walk (cadr binding)))
	       (letrec/bindings expr)))
	    ((LET/? expr)
	     (walk (let/body expr))
	     (for-each (lambda (binding) (walk (cadr binding)))
	       (let/bindings expr)))
	    ((QUOTE/? expr))
	    ((LOOKUP/? expr))
	    ((LAMBDA/? expr)
	     (if (hairy-lambda-list? (lambda/formals expr))
		 (specialize/enqueue-lambda! expr))
	     (walk (lambda/body expr)))
	    (else (for-each walk (cdr expr)))))
    ;;(queue/drain! *lambda-queue* arity/specialize-lambda!)
    (if (not (eq? (car (queue/tail *lambda-queue*)) '*HEAD*))
	(arity/specialize-lambda! (car (queue/tail *lambda-queue*))))
    program*))


;;; Search the specialization space.
;;
;;  Idea:
;;
;;  Generate a specialization for all |optional|+1 defaultings.  If there
;;  is no #!rest argument we are done.
;;
;;  Now generate many #!rest expansions as possible until (1) it fails or
;;  (2) the last placeholder in the rest list is not used.  Then
;;  generate a default specializations with a rest argument, by
;;  searching for progressively shorter lists, keeping the knowledge
;;  that the list is at least long enough to satisy the existing
;;  specializations.

(define (nasty-residual placeholder)
  (if *arity/failure*
      (*arity/failure* (cons #F placeholder))
      (internal-error "Nasty residual & no handler" placeholder)))

(define *arity/failure* #F)

(define (arity/specialize-lambda! form)
  (let ((body    (lambda/body form))
	(formals (lambda/formals form)))
    (call-with-values
	(lambda () (lambda-list/parse formals))
      (lambda (required optional rest aux)
	;; REQUIRED includes continuation.
	
	(a:pp 'specialize-lambda:)
	(a:pp form)
	(let* ((specializations '())
	       (low   (length required))
	       (high  (if rest
			  120 ;; (+ low (length optional) 4)
			  (+ low (length optional)))))
	  (define (done)
	    (arity/rewrite-arity-dispatched-procedure!
	     form '(QUOTE default) low (reverse! specializations))
	    (a:pp `(transfomed-procedure: ,form)))
	  (define (failed)
	    'failed)
	  (define (finish-up-search new-lambda.ph)
	    ;; 
	    (internal-warning "Bag out " new-lambda.ph)
	    (arity/rewrite-arity-dispatched-procedure!
	     form 
	     `(LAMBDA (,(first (lambda/formals form))
		       ,(new-variable 'UNUSED-SELF)
		       ,@(cdr (lambda/formals form)))
		,(lambda/body form))
	     low (reverse! specializations))
	    (a:pp `(transfomed-procedure: ,form)))
	  (let loop ((arity low))
	    (let ((new-lambda.ph
		   (call-with-current-continuation
		    (lambda (k)
		      (set! *arity/failure* k)
		      (arity/generate-specialization
		       required optional rest body arity #F #F)))))
	      (cond ((LAMBDA/? (car new-lambda.ph))
		     (set! specializations
			   (cons (car new-lambda.ph) specializations))
		     (a:pp `(low: ,low high: ,high arity: ,arity
			     ph: ,(cdr new-lambda.ph)))
		     (cond ((= arity high)
			    (done))
			   ((or (not (cdr new-lambda.ph))
				(placeholder/name-used? (cdr new-lambda.ph)))
			    (loop (+ arity 1)))
			   (rest  ;; unused rest slot
			    (finish-up-search new-lambda.ph))
			   (else
			    (done))))
		    ((< arity (+ low (length optional)))
		     ;; Could not even do #!OPTIONALs
		     (failed))
		    (else
		     (finish-up-search new-lambda.ph))))))))))



(define (arity/rewrite-arity-dispatched-procedure!
	 form default low specializations)
  (sample/1 '(arity/dispatched-procedures histogram)
	    (length specializations))
  (internal-warning "Arity dispatch with"
		    (length specializations)
		    (error-irritant/noise " cases starting at arity")
		    low)
  (form/rewrite! form
    `(CALL ',%make-entity
	   '#F
	   ,default
	   (CALL ',%vector
		 '#F
		 ',%arity-dispatcher-tag
		 ,@(make-list (- low 1) '(QUOTE #F))
		 ,@specializations))))


(define (arity/generate-specialization required optional rest body arity rest? rest-arity)
  ;; ARITY is at least enough to satisfy the REQUIREDs returns a pair of
  ;; (1) a new LAMBDA expression and (2) the last (possibly a #!rest)
  ;; placeholder
  (define (generate new-ll env last-placeholder)
    (a:pp '----------)
    (a:pp `(lambda-list: ,new-ll env: ,env))
    (let ((body  (form/copy body)))
      (a:pp `(before: ,body))
      (specialize/expr! env body)
      (remove-placeholders! env body)
      (cons `(LAMBDA ,new-ll ,body) last-placeholder)))

  (let ((new-required (map variable/rename required)))
    (let loop ((env (map (lambda (n n*) (cons n `(LOOKUP ,n*)))
			 required
			 new-required))
	       (optional optional)
	       (new-args '())
	       (position (length required)))
      (cond ((= position arity)
	     ;; Default the optionals & rest
	     (let loop ((env env) (optional optional))
	       (cond ((null? optional)
		      (if rest
			  (generate (append new-required (reverse new-args))
				    (cons (cons rest
						(quote-placeholder
						 (make-placeholder #F '())))
					  env)
				    #F)
			  (generate (append new-required (reverse new-args))
				    env
				    #F)))
		     (else
		      (loop (cons (cons (car optional)
					(quote-placeholder
					 (make-placeholder #F %unassigned)))
				  env)
			    (cdr optional))))))
	    ((null? optional)
	     (let* ((rest-list-args 
		     (make-initialized-list (- arity position)
		       (lambda (i) i (variable/rename rest))))
		    (placeholders   (map make-placeholder rest-list-args))
		    (new-rest-arg   (and rest? (variable/rename rest)))
		    (terminal-ph    (if rest?
					(make-placeholder new-rest-arg)
					(make-placeholder #F '())))
		    (rest-list-value
		     (let walk ((lst placeholders))
		       (if (null? lst)
			   terminal-ph
			   (make-placeholder #F (cons (car lst) (walk (cdr lst))))))))
	       (define (bind-ph ph name) (cons ph `(LOOKUP ,name)))
	       (generate (append new-required
				 (reverse new-args)
				 rest-list-args
				 (if rest? (list #!rest new-rest-arg) '()))
			 (append (if rest?
				     (list (bind-ph terminal-ph new-rest-arg))
				     '())
				 (map bind-ph placeholders rest-list-args)
				 (cons (cons rest
					     (quote-placeholder rest-list-value))
				       env))
			 (if new-rest-arg
			     new-rest-arg
			     (car (last-pair placeholders))))))
	    (else
	     (let* ((name   (car optional))
		    (name*  (variable/rename name))
		    (ph     (make-placeholder name*)))
	       (loop (cons* (cons name (quote-placeholder ph))
			    (cons ph `(LOOKUP ,name*))
			    env)
		     (cdr optional)
		     (cons name* new-args)
		     (+ position 1))))))))


(define *specializer/rewriters* (make-eq-hash-table))

(define (specializer/rewrite? operator)
  (hash-table/get *specializer/rewriters* operator #F))

(define (define-specializer-rewriter name handler)
  (hash-table/put! *specializer/rewriters* name handler))

(define-specializer-rewriter (make-primitive-procedure 'CAR)
  (lambda (form arg)
    (cond ((PLACEHOLDER-QUOTE/? arg)
	   (let ((ph (placeholder-quote/object arg)))
	     (if (pair? (placeholder/value ph))
		 (form/rewrite! form
		   (quote-placeholder (car (placeholder/value ph)))))))
	  (else unspecific))))

(define-specializer-rewriter (make-primitive-procedure 'CDR)
  (lambda (form arg)
    (cond ((PLACEHOLDER-QUOTE/? arg)
	   (let ((ph (placeholder-quote/object arg)))
	     (if (pair? (placeholder/value ph))
		 (form/rewrite! form
		   (quote-placeholder (cdr (placeholder/value ph)))))))
	  (else unspecific))))

(let ()
  (define (safe-unary-type-test name pred)
    ;; PRED cannot look `into' containers (e.g. pairs), as these will have
    ;; placeholders inside.
    (define-specializer-rewriter name
      (lambda (form arg)
	(cond ((PLACEHOLDER-QUOTE/? arg)
	       (let ((ph (placeholder-quote/object arg)))
		 (if (not (placeholder? (placeholder/value ph)))
		     (form/rewrite! form
		       `(QUOTE ,(pred (placeholder/value ph)))))))
	      ((QUOTE/? arg)
	       (form/rewrite! form `(QUOTE ,(pred (quote/text arg)))))
	      (else unspecific)))))

  (safe-unary-type-test  (make-primitive-procedure 'NULL?) null?)
  (safe-unary-type-test  (make-primitive-procedure 'PAIR?) pair?)
  ;;(safe-unary-type-test  %unassigned? (lambda (x) (eq? x %unassigned)))
  )

(define-specializer-rewriter %unassigned?
  (lambda (form arg)
    (a:pp form)
    (cond ((PLACEHOLDER-QUOTE/? arg)
	   (let ((ph (placeholder-quote/object arg)))
	     ;; This rewrites `unknown' placeholders to booleans too:
	     (form/rewrite! form
	       `(QUOTE ,(eq? (placeholder/value ph) %unassigned)))))
	  (else unspecific))))


(define a:pp (lambda (thing) thing unspecific))
;(define a:pp pp)