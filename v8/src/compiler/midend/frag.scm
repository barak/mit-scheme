#| -*-Scheme-*-

$Id: frag.scm,v 1.2 1995/03/30 20:04:35 adams Exp $

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
  (pp `(queue-lambda: ,form))
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
	(form/rewrite! form (cdr place))))
  unspecific)

(define-specializer LAMBDA (lambda-list body)
  (let ((env*
	 (map* env (lambda (name) (cons name `(LOOKUP ,name)))
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

(define (specialize-call! info env form rands)
  (define (extract-placeholders form so-far)
    (cond ((placeholder? form) (cons form so-far))
	  ((pair? form)
	   (extract-placeholders (cdr form)
				 (extract-placeholders (car form) so-far)))
	  (else so-far)))
  (define (substitute? form)
    (define (immutable? v)
      (or (number? v) (object-type? v (object-type #F)) (char? v)))
    (or (PLACEHOLDER-QUOTE/? form)
	(and (QUOTE/? form)
	     (immutable? (quote/text form)))))
  (let* ((lam-expr  (specializer/info/lambda info))
	 (formals   (lambda/formals lam-expr))
	 (body      (lambda/body lam-expr)))
    (if (and (there-exists? rands PLACEHOLDER-QUOTE/?)
	     (not (hairy-lambda-list? formals)))
	(let* ((names*  (map variable/rename formals))
	       (ph*     (map (lambda (p)
			       (cons p `(LOOKUP ,(new-variable (placeholder/name p)))))
			     (extract-placeholders rands '())))
	       (env*    (map* ph*
			      (lambda (f n v)
				(if (substitute? v)
				    (cons f v)
				    (cons f `(LOOKUP ,n))))
			      (cdr formals)
			      (cdr names*)
			      rands))
	       (body*   (form/copy body)))
	  (specialize/expr! env* body*)
	  (pp `(new-body: ,body*))
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
			 (append (list-transform-negative rands substitute?)
				  (map (lambda (p) (cdr (assq p env)))
				       placeholders)))
			(new-formals
			 (let loop ((rs rands) (fs names*))
			    (cond ((null? rs)
				   (map (lambda (p) (cdr (assq p env*)))
					placeholders))
				  ((substitute? (car rs))
				   (loop (cdr rs) (cdr fs)))
				  (else
				   (cons (car fs) (loop (cdr rs) (cdr fs))))))))
		   (form/rewrite! form
		     `(CALL (LOOKUP ,procedure-name)
			    '#F
			    ,@actuals))
		   (pp `(call: ,form))
		   (remember-specialization!
		    info
		    procedure-name
		    `(LAMBDA ,new-formals ,body*))))))
	(pp `(declined: ,rands)))))


(define (remember-specialization! info proc-name lam-expr)
  (pp `(remember-specialization! ,info ,proc-name ,lam-expr))
  (let ((letrec-form  (specializer/info/letrec info)))
    (set-car! (cdr letrec-form)
	      (cons (list proc-name lam-expr)
		    (cadr letrec-form)))))
  
(define (remove-placeholders! env form) ; -> list (placeholder)
  ;; remove placeholders, replacing with new names.
  ;;  Return alist from placeholders to name
  (let walk ((form form) (ps '()))
    (cond ((PLACEHOLDER-QUOTE/? form)
	   (let ((text (placeholder-quote/text form)))
	     (cond ((placeholder? text)
		    (let ((pair  (assq text env)))
		      (if (not pair)
			  (internal-error "Not bound" form env))
		      (form/rewrite! form (cdr pair))
		      (if (memq text ps) ps (cons text ps))))
		   ((contains-placeholder? text)
		    (nasty-residual))
		   (else
		    (form/rewrite! form `(QUOTE ,text))))))
	  ((QUOTE/? form) ps)
	  ((LOOKUP/? form) ps)
	  ((LAMBDA/? form) (walk (lambda/formals form) ps))
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

(define (PLACEHOLDER-QUOTE/? expr)
  (and (pair? expr)
       (eq? (car expr) 'PLACEHOLDER-QUOTE)))

(define (placeholder-quote/text expr) (second expr))

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

(define (make-placeholder-quote value)
  (if (contains-placeholder? value)
      `(PLACEHOLDER-QUOTE ,value)
      `(QUOTE ,value)))

(define (specialize/simple? expr)
  (or (QUOTE/? expr)
      (PLACEHOLDER-QUOTE/? expr)))

(define (specialize/expr! env expr)
  ;; Rewrite EXPR.
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((PLACEHOLDER-QUOTE))
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

(define-structure
    (placeholder
     (conc-name placeholder/)
     (constructor make-placeholder))
  (name  #F read-only true)		; #F or name of variable
  (value #F read-only true)		; either this placeholder
					; or structure containing placeholders
)

(define (arity/top-level program)
  ;; These should be put in a fluid-let when debugging is done:
  (set! *specialization-table* (make-eq-hash-table))
  (set! *lambda-queue* (queue/make))
  (let ((program* (copier/top-level program (lambda (old new) new))))
    (let walk ((expr program*))
      (cond ((LETREC/? expr)
	     (for-each
		 (lambda (binding)
		   (hash-table/put! *specialization-table*
				    (car binding)
				    (specializer/info/make
				     (car binding)
				     (cadr binding)
				     form)))
	       (letrec/bindings expr))
	     (walk (letrec/body expr)))
	    ((LET/? expr) (walk (let/body expr)))
	    ((QUOTE/? expr))
	    ((LOOKUP/? expr))
	    ((LAMBDA/? expr)
	     (if (hairy-lambda-list? (lambda/formals expr))
		 (specialize/enqueue-lambda! expr))
	     (walk (lambda/body expr)))
	    (else (for-each walk (cdr expr))))
      (queue/drain! *lambda-queue* arity/specialize-lambda!))
    program*))


(define (arity/specialize-lambda! form)
  (let ((body    (lambda/body form))
	(formals (lambda/formals form)))
    (call-with-values
	(lambda () (lambda-list/parse formals))
      (lambda (required optional rest aux)
	;; required includes continuation.
	(pp 'specialize-lambda:)
	(pp form)
	(let* ((low   (length required))
	       (high  (if rest
			  (+ low (length optional) 5)
			  (+ low (length optional))))
	       (specializations
		(let loop ((arity low) (specializations '()))
		  (if (<= arity high)
		      (let ((new-lambda
			     (arity/generate-specialization form arity #F)))
			(pp `(after: ,new-lambda))
			(loop (+ arity 1) (cons new-lambda specializations)))
		      (reverse specializations)))))
	  (form/rewrite! form
	    `(CALL 'make-multiple-arity-procedure
		   '#F
		   ',low
		   ,@specializations))
	  (pp `(transfomed-procedure: ,form)))))))


(define (arity/generate-specialization lam-expr arity rest?)
  ; ARITY is at least enough to satisfy the requireds
  (define (generate new-ll env)
    (pp '----------)
    (pp `(lambda-list: ,new-ll env: ,env))
    (let ((body  (form/copy (lambda/body lam-expr))))
      (pp `(before: ,body))
      (specialize/expr! env body)
      (remove-placeholders! env body)
      `(LAMBDA ,new-ll ,body)))

  (let ((formals (lambda/formals lam-expr)))
    (let loop ((env '()) (old-ll formals) (new-ll '()) (position 0))
      (cond ((= position arity)
	     (let loop ((env env) (old-ll old-ll))
	       (cond ((null? old-ll)
		      (generate (reverse new-ll) env))
		     ((eq? (car old-ll) '#!optional)
		      (loop env (cdr old-ll)))
		     ((eq? (car old-ll) '#!rest)
		      (generate (reverse new-ll)
				(cons (cons (second old-ll) `(QUOTE ())) env)))
		     (else
		      (loop (cons (cons (car old-ll)
					`(PLACEHOLDER-QUOTE ,%unassigned))
				  env)
			    (cdr old-ll))))))
	    ((eq? (car old-ll) '#!optional)
	     (loop env (cdr old-ll) new-ll position))
	    ((eq? (car old-ll) '#!rest)
	     (let* ((rest           (second old-ll))
		    (rest-list-args (make-initialized-list (- arity position)
				      (lambda (i) i (variable/rename rest))))
		    (placeholders   (map make-placeholder rest-list-args))
		    (new-rest-arg   (and rest? (variable/rename rest)))
		    (new-rest-ph    (and rest? (make-placeholder new-rest-arg)))
		    (rest-list-value (if rest?
					 (append placeholders new-rest-ph)
					 placeholders)))
	       (define (bind-ph ph name) (cons ph `(LOOKUP ,name)))
	       (generate (append (reverse new-ll) rest-list-args
				 (if rest? (list '#!rest new-rest-arg) '()))
			 (append (if rest?
				     (list (bind-ph new-rest-ph new-rest-arg))
				     '())
				 (map bind-ph placeholders rest-list-args)
				 (cons (cons rest
					   `(PLACEHOLDER-QUOTE ,rest-list-value))
				       env)))))
	    (else
	     (let* ((name   (car old-ll))
		    (name*  (variable/rename name)))
	       (loop (cons (cons name `(LOOKUP ,name*)) env)
		     (cdr old-ll)
		     (cons name* new-ll)
		     (+ position 1))))))))



(define *specializer/rewriters* (make-eq-hash-table))

(define (specializer/rewrite? operator)
  (hash-table/get *specializer/rewriters* operator #F))

(define (define-specializer-rewriter name handler)
  (hash-table/put! *specializer/rewriters* name handler))

(define-specializer-rewriter (make-primitive-procedure 'CAR)
  (lambda (form arg)
    (cond ((PLACEHOLDER-QUOTE/? arg)
	   (if (pair? (placeholder-quote/text arg))
	       (form/rewrite! form
		 (make-placeholder-quote (car (placeholder-quote/text arg))))))
	  (else unspecific))))

(define-specializer-rewriter (make-primitive-procedure 'CDR)
  (lambda (form arg)
    (cond ((PLACEHOLDER-QUOTE/? arg)
	   (if (pair? (placeholder-quote/text arg))
	       (form/rewrite! form
		 (make-placeholder-quote (cdr (placeholder-quote/text arg))))))
	  (else unspecific))))


(let ()
  (define (safe-unary-predicate name pred)
    (define-specializer-rewriter name
      (lambda (form arg)
	(cond ((PLACEHOLDER-QUOTE/? arg)
	       (if (not (placeholder? (placeholder-quote/text arg)))
		   (form/rewrite! form
		     `(QUOTE ,(pred (placeholder-quote/text arg))))))
	      ((QUOTE/? arg)
	       (form/rewrite! form `(QUOTE ,(pred (quote/text arg)))))
	      (else unspecific)))))

  (safe-unary-predicate  (make-primitive-procedure 'NULL?) null?)
  (safe-unary-predicate  %unassigned? (lambda (x) (eq? x %unassigned))))
