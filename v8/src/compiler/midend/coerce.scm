#| -*-Scheme-*-

$Id: coerce.scm,v 1.1 1995/03/20 02:44:31 adams Exp $

Copyright (c) 1995 Massachusetts Institute of Technology

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
wins by 8-10%.  This could be even better if
COERCE-TO-COMPILED-PROCEDURE understood arity dispatched entities
(merely a matter of extending the primitive).

It loses big-time (up to a factor of 2) on other kinds of program
because it is stupid:

 . It does this transformation for all lambda-bindings that are used
   in operator position like F, including those which are really
   LET-bindings.  It should only do this if the call site in in a
   lambda expression that will be a loop or a closure - i.e. has
   potential for many repeated executions.

 . The new binding is inserted as high as possible in the lambda with
   the original binding.  In code which has branches with calls to F
   with different number of arguments in each branch (like the system
   code for MAP and FOR-EACH) this is a disaster as one of the
   coercions is guaranteed to cons a trampoline.  The coercion needs
   to be restricted to the branch where it applies.

 . The coercion could be much better engineered - a quick check to
   prevent the call to the primitive in the `no-op' case would be a
   big benefit, and perhaps so would a preserving call or hook or
   compiler utility for the out-of-line case.

 . The HP-PA LAP code for INVOCATION:REGISTER with a continuation
   could be one insn shorter.

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
	       env
	       (map coerce/binding/make (lambda-list->names lambda-list)))))
    (let ((body* (coerce/expr env* body)))
      (coerce/lambda/finish! env* lambda-list body*))))

(define coerce/lambda/finish!
  (let ((coerce-to-compiled
	 (make-primitive-procedure 'COERCE-TO-COMPILED-PROCEDURE)))
    (lambda (env lambda-list body)
      (define (rewrite-call! call arity coerced-operator)
	;;(form/rewrite! (call/operator call) 
	;;  `(LOOKUP ,coerced-operator))
	(form/rewrite! call
	  `(CALL ',%internal-apply-unchecked
		 ,(call/continuation call)
		 ',arity
		 (LOOKUP ,coerced-operator)
		 ,@(call/operands call))))
      (define (make-coercion name len)
	`(CALL ',coerce-to-compiled '#F (LOOKUP ,name) ',len)
	`(IF (IF (CALL ',%compiled-entry? '#F (LOOKUP ,name))
		 (CALL ',%compiled-entry-maximum-arity? '#F
		       ',(+ len 1)
		       (LOOKUP ,name))
		 '#F)
	     (LOOKUP ,name)
	     (CALL ',coerce-to-compiled '#F (LOOKUP ,name) ',len)))
      (let ((names  '())
	    (values '()))
	(let loop ((bindings (coerce/env/bindings env)))
	  (if (null? bindings)
	      `(LAMBDA ,lambda-list
		 ,(if (null? names)
		      body
		      (bind* names values body)))
	      (let* ((binding (car bindings))
		     (name    (coerce/binding/name binding)))
		(let ref-loop ((refs (coerce/binding/operator-refs binding))
			       (arity-map '()))
		  (if (null? refs)
		      (loop (cdr bindings))
		      (let* ((ref  (car refs))
			     (len  (length (call/operands ref)))
			     (arity.name (assv len arity-map)))
			(cond (arity.name
			       (rewrite-call! (car refs) len (cdr arity.name))
			       (ref-loop (cdr refs) arity-map))
			      ((<= 0 len 120)
			       (let*  ((name*  (variable/rename name)))
				 (rewrite-call! (car refs) len name*)
				 (set! names (cons name* names))
				 (set! values
				       (cons (make-coercion name len)
					     values))
				 (ref-loop (cdr refs) (cons (cons len name*) arity-map))))
			      (else
			       (ref-loop (cdr refs) arity-map)))))))))))))

(define-coercer LET (bindings body)
  `(LET ,(map (lambda (binding)
		 (list (car binding)
		       (coerce/expr env (cadr binding))))
	       bindings)
     ,(coerce/expr env body)))

(define-coercer LETREC (bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (coerce/expr env (cadr binding))))
		 bindings)
     ,(coerce/expr env body)))

(define-coercer IF (pred conseq alt)
  `(IF ,(coerce/expr env pred)
       ,(coerce/expr env conseq)
       ,(coerce/expr env alt)))

(define-coercer QUOTE (object)
  env
  `(QUOTE ,object))

(define-coercer DECLARE (#!rest anything)
  env
  `(DECLARE ,@anything))

(define-coercer BEGIN (#!rest actions)
  `(BEGIN ,@(coerce/expr* env actions)))

(define-coercer CALL (rator cont #!rest rands)
  (define (default)
    `(CALL ,(coerce/expr env rator)
	   ,(coerce/expr env cont)
	   ,@(coerce/expr* env rands)))
  (cond ((LAMBDA/? rator)
	;;`(CALL (LAMBDA ,(lambda/formals rator)
	;;	  ,(coerce/expr env (lambda/body rator)))
	;;	,(coerce/expr env cont)
	;;	,@(coerce/expr* env rands))
	 (default))
	((LOOKUP/? rator)
	 (let* ((name  (lookup/name rator))
		(call  `(CALL (LOOKUP ,name) ,(coerce/expr env cont)
			      ,@(coerce/expr* env rands))))
	   ;;(coerce/env/lookup*! env name call 'OPERATOR))
	   ;; This helps us not to trap `non-closed' bindings:
	   (coerce/env/lookup*! (coerce/env/parent env) name call 'OPERATOR))
	 )
	(else
	 (default))))

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



(define-structure
    (coerce/binding
     (conc-name coerce/binding/)
     (constructor coerce/binding/make (name))
     (print-procedure
      (standard-unparser-method 'COERCE/BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write-string (symbol-name (coerce/binding/name binding)) port)))))

  (name false read-only true)
  (ordinary-refs '() read-only false)
  (operator-refs '() read-only false))

(define-structure
    (coerce/env
     (conc-name coerce/env/)
     (constructor coerce/env/make (parent bindings))
     (print-procedure
      (standard-unparser-method 'COERCE/ENV
	(lambda (env port)
	  (write-char #\Space port)
	  (write (map coerce/binding/name (coerce/env/bindings env))
		 port)))))

  (bindings '() read-only true)
  (parent #F read-only true)
  ;; FREE-CALLS is used to mark calls to names free in this frame but bound
  ;; in the parent frame.  Used to detect mutual recursion in LETREC.
  (free-calls '() read-only false))


(define coerce/env/frame-lookup
  (association-procedure (lambda (x y) (eq? x y)) coerce/binding/name))

(define (coerce/env/lookup*! env name reference kind)
  ;; kind = 'OPERATOR, 'ORDINARY
  (let frame-loop ((env env))
    (cond ((not env)
	   ;;(free-var-error name)
	   reference
	   )
	  ((coerce/env/frame-lookup name (coerce/env/bindings env))
	   => (lambda (binding)
		(case kind
		  ((OPERATOR)
		   (set-coerce/binding/operator-refs!
		    binding
		    (cons reference (coerce/binding/operator-refs binding))))
		  ((ORDINARY)
		   (set-coerce/binding/ordinary-refs!
		    binding
		    (cons reference (coerce/binding/ordinary-refs binding))))
		  (else
		   (internal-error "coerce/lookup*! bad KIND" kind)))
		reference))
	  (else (frame-loop (coerce/env/parent env))))))
