#| -*-Scheme-*-

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

;;;; coercing operators to compiled procedures - a way of lifting
;;;; apply-time checks out of loops (and closures).

#|
This phase replaces

  (LAMBDA (... F ...)
     ...
     (CALL (LOOKUP F) '#F <e1> ... <en>) ...)

With
  (LAMBDA (... F ...)
    (CALL
     (LAMBDA (<cont> F-2)
       ...
       (CALL '%internal-apply-unchecked '#F (LOOKUP F-2) '2 <e1> .. <en>))
     (CALL 'coerce-to-compiled-procedure '#F F '<n>)))

At the moment it is pretty naïve about inserting this kind of code.
For the right kind of program (sort, feeley-like closure compiler) it
wins by about 10%.

|#

;;; package: (compiler midend)

(declare (usual-integrations))

(define (coerce/top-level program)
  (coerce/expr #F program))

(define-macro (define-coercer keyword bindings . body)
  (let ((proc-name (symbol-append 'COERCE/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
	       (COERCE/REMEMBER ,code FORM))))))))

(define-coercer LOOKUP (name)
  (coerce/env/lookup*! env name `(LOOKUP ,name) 'ORDINARY))

(define-coercer LAMBDA (lambda-list body)
  (let ((env* (coerce/env/make
	       'LAMBDA
	       env
	       (map coerce/binding/make (lambda-list->names lambda-list)))))
    (coerce/lambda* env* lambda-list body)))

(define (coerce/lambda* env* lambda-list body)
  (let ((body* (coerce/expr env* body)))
    (set-coerce/env/form! env* body*)
    (coerce/lambda/finish! env*)
    `(LAMBDA ,lambda-list ,body*)))

(define (coerce/lambda/finish! env)
  (let binding-loop ((bindings (coerce/env/bindings env)))
    (if (null? bindings)
	'done
	(let* ((binding (car bindings))
	       (name    (coerce/binding/name binding)))
	  (if (not (coerce/binding/lambda? binding))
	      (let ref-loop ((refs (coerce/binding/operator-refs binding))
			     (arity-map '()))
		(if (null? refs)
		    (begin
		      (for-each (lambda (arity.refs)
				  (coerce/rewrite! env name
						   (car arity.refs)
						   (cdr arity.refs)))
			arity-map)
		      (binding-loop (cdr bindings)))
		    (let* ((ref  (car refs))
			   (text (coerce/reference/form/call ref))
			   (len  (length (call/operands text)))
			   (arity.refs (assv len arity-map)))
		      (cond (arity.refs
			     (set-cdr! arity.refs
				       (cons ref (cdr arity.refs)))
			     (ref-loop (cdr refs) arity-map))
			    (else
			     (ref-loop (cdr refs)
				       (cons (list len ref) arity-map))))))))))))

(define (coerce/rewrite! env name arity refs)
  ;; Find highest least
  (define (same-extent? ref)
    (let loop ((env*  (coerce/reference/env ref)))
      (cond ((eq? env* env)  #T)
	    ((eq? (coerce/env/kind env*) 'LAMBDA) #F)
	    (else  (loop (coerce/env/parent env*))))))
  (define (common-env e1 e2)
    (cond ((eq? e1 e2)  e1)
	  ((< (coerce/env/depth e1) (coerce/env/depth e2))
	   (common-env e1 (coerce/env/parent e2)))
	  ((> (coerce/env/depth e1) (coerce/env/depth e2))
	   (common-env (coerce/env/parent e1) e2))
	  (else
	   (common-env (coerce/env/parent e1) (coerce/env/parent e2)))))
  (define (maximize-extent env*)
    (let loop ((chosen env*) (env* env*))
      (cond ((eq? env* env)  chosen)
	    ((eq? (coerce/env/kind env*) 'LAMBDA)
	     (loop (coerce/env/parent env*) (coerce/env/parent env*)))
	    (else
	     (loop chosen (coerce/env/parent env*))))))
  (define (within? env base-env)
    (cond ((eq? env base-env) #T)
	  ((< (coerce/env/depth env) (coerce/env/depth base-env)) #F)
	  (else (within? (coerce/env/parent env) base-env))))
  (call-with-values
      (lambda ()
	(list-split refs same-extent?))
    (lambda (same-extent other-extent)
      same-extent			; ignored, implicit in REFS
      (cond
       ((> arity 120)        'cant)
       ((null? other-extent) 'not-worth-while)
       (else
	(let ((common-env
	       (reduce common-env #F (map coerce/reference/env other-extent))))
	  (let* ((coercion-env (maximize-extent common-env))
		 (name*  (variable/rename name))
		 (form   (coerce/env/form coercion-env))
		 (body   (form/preserve form)))
	    (dbg-info/remember name `(CALL 'uncoerce '#F (LOOKUP ,name*)))
	    (form/rewrite! form
	      (bind name* (coerce/make-coercion name arity) body))
	    (let loop ((refs refs) (replaced 0) (kept 0))
	      (if (null? refs)
		  (if compiler:guru?
		      (let ((t  error-irritant/noise))
			(internal-warning "strength reduced call"
					  (t "\n;Reduced call to") name
					  (t " with") arity 
					  (t " args.  Operators replaced:")
					  replaced
					  (t ", unchanged:")
					  kept)))
		  (let ((ref  (car refs)))
		    (cond ((within? (coerce/reference/env ref)
				    coercion-env)
			   (coerce/rewrite-call!
			    (coerce/reference/form/call ref)
			    arity name*)
			   (loop (cdr refs) (+ replaced 1) kept))
			  (else
			   'leave-it-alone
			   (loop (cdr refs) replaced (+ kept 1))))))))))))))

(define coerce/make-coercion
  (let ((coerce-to-compiled
	 (make-primitive-procedure 'COERCE-TO-COMPILED-PROCEDURE)))
    (lambda (name len)
      `(CALL ',coerce-to-compiled '#F (LOOKUP ,name) ',len)
      `(IF (IF (CALL ',%compiled-entry? '#F (LOOKUP ,name))
	       (CALL ',%compiled-entry-maximum-arity? '#F
		     ',(+ len 1)
		     (LOOKUP ,name))
	       '#F)
	   (LOOKUP ,name)
	   (CALL ',%invoke-remote-cache
		 '#F
		 '(coerce-to-compiled-procedure 2)
		 'bogus-execute-cell
		 (LOOKUP ,name)
		 ',len)))))

(define (coerce/rewrite-call! call arity coerced-operator)
  (form/rewrite! call
    `(CALL ',%internal-apply-unchecked
	   ,(call/continuation call)
	   ',arity
	   (LOOKUP ,coerced-operator)
	   ,@(call/operands call))))


(define-coercer LET (bindings body)
  (let* ((names   (map car bindings))
	 (values  (map cadr bindings))
	 (inner-env
	  (coerce/env/make 'LET env (map coerce/binding/make names))))
    (let ((body*  (coerce/expr inner-env body)))
      (set-coerce/env/form! inner-env body*)
      `(LET ,(map (lambda (name value)
		    (list name (coerce/expr env value)))
		  names values)
	 ,body*))))

(define-coercer LETREC (bindings body)
  (let* ((names   (map car bindings))
	 (values  (map cadr bindings))
	 (inner-env
	  (coerce/env/make 'LETREC env (map coerce/binding/make names))))
    (let ((form*  
	   `(LETREC ,(map (lambda (name value)
			    (list name (coerce/expr inner-env value)))
			  names values)
	      ,(coerce/expr inner-env body))))
      (set-coerce/env/form! inner-env form*)
      form*)))


(define-coercer IF (pred conseq alt)
  (let ((env1  (coerce/env/make 'CONDITIONAL env '()))
	(env2  (coerce/env/make 'CONDITIONAL env '())))
    (let ((conseq*  (coerce/expr env1 conseq))
	  (alt*     (coerce/expr env2 alt)))
      (set-coerce/env/form! env1 conseq*)
      (set-coerce/env/form! env2 alt*)
      `(IF ,(coerce/expr env pred) ,conseq* ,alt*))))

(define-coercer QUOTE (object)
  env
  `(QUOTE ,object))

(define-coercer DECLARE (#!rest anything)
  env
  `(DECLARE ,@anything))

(define-coercer BEGIN (#!rest actions)
  `(BEGIN ,@(coerce/expr* env actions)))

(define-coercer CALL (rator cont #!rest rands)
  (define (default rator*)
    `(CALL ,rator*
	   ,(coerce/expr env cont)
	   ,@(coerce/expr* env rands)))
  (define (make-bds lambda-list)
    (let loop ((ll    lambda-list)
	       (bds   '())
	       (rands (cons cont rands)))
      (cond ((null? ll) bds)
	    ((eq? (car ll) #!optional)
	     (loop (cdr ll) bds rands))
	    ((or (null? rands)
		 (eq? #!rest (car ll))
		 (eq? #!aux (car ll)))
	     (map* bds coerce/binding/make (lambda-list->names ll)))
	    (else
	     (loop (cdr ll)
		   (cons (coerce/binding/make2 (car ll) (LAMBDA/? (car rands)))
			 bds)
		   (cdr rands))))))
  (cond ((LAMBDA/? rator)
	 (let* ((formals (lambda/formals rator))
		(env* (coerce/env/make 'LET env (make-bds formals))))
	   (default
	     (coerce/lambda* env* formals (lambda/body rator)))))
	((LOOKUP/? rator)
	 (let* ((name  (lookup/name rator))
		(call  (default `(LOOKUP ,name))))
	   (coerce/env/lookup*! env name call 'OPERATOR)))
	(else
	 (default (coerce/expr env rator)))))

(define (coerce/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (coerce/quote env expr))
    ((LOOKUP)   (coerce/lookup env expr))
    ((LAMBDA)   (coerce/lambda env expr))
    ((LET)      (coerce/let env expr))
    ((DECLARE)  (coerce/declare env expr))
    ((CALL)     (coerce/call env expr))
    ((BEGIN)    (coerce/begin env expr))
    ((IF)       (coerce/if env expr))
    ((LETREC)   (coerce/letrec env expr))
    (else
     (illegal expr))))

(define (coerce/expr* env exprs)
  (map (lambda (expr)
	 (coerce/expr env expr))
	exprs))

(define (coerce/remember new old)
  (code-rewrite/remember new old))



(define (coerce/reference/make env form) (cons form env))
(define (coerce/reference/form ref) (car ref))
(define (coerce/reference/env ref) (cdr ref))

(define (coerce/reference/form/call ref)
  ;; One complexity is that a call site may already have been rewritten to
  ;; be a binding for some inner coerced procedure.  This happens at
  ;; the call site for F in the following example:
  ;;   (lambda (f)
  ;;     (lambda (g)
  ;;	   (f (lambda (x) (g (+ x 1))))))
  ;; By the time we get to rewrite the call to F it looks like this:
  ;;   ((lambda (g*) (f (lambda (x) (g* (+ x 1))))) <coerce-g>)
  ;; So this code `dereferences' to the original call site
  (define (bad)
    (internal-error "Bad call site reference" ref))
  (let loop ((form (coerce/reference/form ref)))
    (cond ((not (CALL/? form))
	   (bad))
	  ((LOOKUP/? (call/operator form))
	   form)
	  ((LAMBDA/? (call/operator form))
	   (loop (lambda/body (call/operator form))))
	  (else (bad)))))

(define-structure
    (coerce/binding
     (conc-name coerce/binding/)
     (constructor coerce/binding/make (name))
     (constructor coerce/binding/make2 (name lambda?))
     (print-procedure
      (standard-unparser-method 'COERCE/BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write-string (symbol-name (coerce/binding/name binding)) port)))))

  (name #F read-only true)
  (lambda? #F read-only false)		; Bound to a known lambda?
  (ordinary-refs '() read-only false)
  (operator-refs '() read-only false))

(define-structure
    (coerce/env
     (conc-name coerce/env/)
     (constructor coerce/env/%make)
     (print-procedure
      (standard-unparser-method 'COERCE/ENV
	(lambda (env port)
	  (write-char #\Space port)
	  (write (coerce/env/kind env) port)
	  (write-char #\Space port)
	  (write (coerce/env/depth env) port)
	  (write-char #\Space port)
	  (write (map coerce/binding/name (coerce/env/bindings env))
		 port)))))

  (bindings '() read-only true)
  (parent #F read-only true)
  (depth  0  read-only true)
  ;; kind = LAMBDA | CONDITIONAL | LET
  (kind   #F read-only true)
  (form   #F read-only false))

(define (coerce/env/make kind parent bindings)
  (coerce/env/%make bindings
		    parent
		    (if parent (+ (coerce/env/depth parent) 1) 0)
		    kind
		    #F))

(define coerce/env/frame-lookup
  (association-procedure (lambda (x y) (eq? x y)) coerce/binding/name))

(define (coerce/env/lookup*! env name reference kind)
  ;; kind = 'OPERATOR, 'ORDINARY
  (let frame-loop ((frame env))
    (cond ((not frame)
	   (free-var-error name)
	   ;;reference
	   )
	  ((coerce/env/frame-lookup name (coerce/env/bindings frame))
	   => (lambda (binding)
		(let ((ref  (coerce/reference/make env reference)))
		  (case kind
		    ((OPERATOR)
		     (set-coerce/binding/operator-refs!
		      binding
		      (cons ref (coerce/binding/operator-refs binding))))
		    ((ORDINARY)
		     (set-coerce/binding/ordinary-refs!
		      binding
		      (cons ref (coerce/binding/ordinary-refs binding))))
		    (else
		     (internal-error "coerce/lookup*! bad KIND" kind))))
		reference))
	  (else (frame-loop (coerce/env/parent frame))))))
