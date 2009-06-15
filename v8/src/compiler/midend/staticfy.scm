#| -*-Scheme-*-

$Id$

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

;;;; Static binding annotator
;;; package: (compiler midend)

(declare (usual-integrations))

(define (staticfy/top-level program)
  (staticfy/expr (staticfy/env/make 'STATIC false '()) program))

(define-macro (define-staticfier keyword bindings . body)
  (let ((proc-name (symbol-append 'STATICFY/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
	  (let ((handler (lambda ,(cons (car bindings) names) ,@body)))
	    (named-lambda (,proc-name env form)
	      (staticfy/remember ,code
				 form))))))))

(define-staticfier LOOKUP (env name)
  (staticfy/lookup* env name `(LOOKUP ,name)))

(define-staticfier LAMBDA (env lambda-list body)
  `(LAMBDA ,lambda-list
     ,(staticfy/expr (staticfy/bind 'DYNAMIC
				    env
				    (lambda-list->names lambda-list))
		     body)))

(define-staticfier LETREC (env bindings body)
  (let ((env* (staticfy/bind (staticfy/env/context env)
			     env
			     (map car bindings))))
    `(LETREC ,(map (lambda (binding)
		     (list (car binding)
			   (staticfy/expr env* (cadr binding))))
		   bindings)
       ,(staticfy/expr env* body))))

(define-staticfier QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-staticfier DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-staticfier BEGIN (env #!rest actions)
  `(BEGIN ,@(staticfy/expr* env actions)))

(define-staticfier IF (env pred conseq alt)
  `(IF ,(staticfy/expr env pred)
       ,(staticfy/expr env conseq)
       ,(staticfy/expr env alt)))

(define-staticfier CALL (env cont rator #!rest rands)
  (if (or (not (pair? rator))
	  (not (eq? (car rator) 'LAMBDA))
	  (eq? (staticfy/env/context env) 'DYNAMIC)
	  (not (equal? cont '(QUOTE #F))))
      `(CALL ,(staticfy/expr env rator)
	     ,(staticfy/expr env cont)
	     ,@(staticfy/expr* env rands))
      (staticfy/let* (lambda (bindings* body*)
		       (staticfy/pseudo-letify rator bindings* body*))
		     env
		     (map list (cdr (cadr rator)) rands)
		     (caddr rator))))

(define-staticfier LET (env bindings body)
  (if (eq? (staticfy/env/context env) 'DYNAMIC)
      `(LET ,(map (lambda (binding)
		    (list (car binding)
			  (staticfy/expr env (cadr binding))))
		  bindings)
	 ,(staticfy/expr (staticfy/bind 'DYNAMIC env (map car bindings))
			 body))
      (staticfy/let* staticfy/letify
		     env
		     bindings
		     body)))
    
(define (staticfy/letify bindings body)
  `(LET ,bindings ,body))

(define (staticfy/pseudo-letify rator bindings body)
  `(CALL ,(staticfy/remember
	   `(LAMBDA (,(car (cadr rator)) ,@(map car bindings))
	      ,body)
	   rator)
	 (QUOTE #F)
	 ,@(map cadr bindings)))

(define (staticfy/let* letify env bindings body)
  (let* ((bindings* (map (lambda (binding)
			   (list (car binding)
				 (staticfy/expr env (cadr binding))))
			 bindings))
	 (env* (staticfy/bind (staticfy/env/context env)
			      env
			      (map car bindings)))
	 (body* (staticfy/expr env* body)))
    (call-with-values
	(lambda ()
	  (list-split bindings*
		      (lambda (binding*)
			(staticfy/simple? (cadr binding*)))))
      (lambda (simple hairy)
	(if (null? hairy)
	    (letify bindings* body*)
	    (begin
	      (for-each
		  (lambda (hairy)
		    (let* ((name (car hairy))
			   (binding (assq name (staticfy/env/bindings env*))))
		      (for-each
			  (lambda (ref)
			    (form/rewrite!
				ref
			      `(CALL (QUOTE ,%static-binding-ref)
				     (QUOTE #F)
				     (LOOKUP ,name)
				     (QUOTE ,name))))
			(cdr binding))))
		hairy)
	      (letify
	       (map (lambda (binding*)
		      (if (memq binding* simple)
			  simple
			  (let ((name (car binding*)))
			    (list name
				  `(CALL (QUOTE ,%make-static-binding)
					 (QUOTE #F)
					 (QUOTE ,%unassigned)
					 (QUOTE ,name))))))
		    bindings*)
	       (beginnify
		(append
		 (let ((actions*
			(map (lambda (hairy)
			       (let ((name (car hairy)))
				 `(CALL (QUOTE ,%static-binding-set!)
					(QUOTE #F)
					(LOOKUP ,name)
					,(cadr hairy)
					(QUOTE ,name))))
			     hairy)))
		   (case *order-of-argument-evaluation*
		     ((ANY LEFT-TO-RIGHT) actions*)
		     ((RIGHT-TO_LEFT) (reverse actions*))
		     (else
		      (configuration-error
		       "Unknown order of argument evaluation"
		       *order-of-argument-evaluation*))))
		 (list body*))))))))))

(define (staticfy/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (staticfy/quote env expr))
    ((LOOKUP)
     (staticfy/lookup env expr))
    ((LAMBDA)
     (staticfy/lambda env expr))
    ((LET)
     (staticfy/let env expr))
    ((DECLARE)
     (staticfy/declare env expr))
    ((CALL)
     (staticfy/call env expr))
    ((BEGIN)
     (staticfy/begin env expr))
    ((IF)
     (staticfy/if env expr))
    ((LETREC)
     (staticfy/letrec env expr))
    ((SET! UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (staticfy/expr* env exprs)
  (map (lambda (expr)
	 (staticfy/expr env expr))
       exprs))

(define (staticfy/remember new old)
  (code-rewrite/remember new old))

(define (staticfy/new-name prefix)
  (new-variable prefix))

(define staticfy/guaranteed-static-operators
  (list %make-operator-variable-cache
	%make-remote-operator-variable-cache
	%make-read-variable-cache
	%make-write-variable-cache
	%fetch-environment))

(define (staticfy/simple? form)
  (and (pair? form)
       (or (eq? (car form) 'QUOTE)
	   (and (eq? (car form) 'CALL)
		(pair? (cadr form))
		(eq? (car (cadr form)) 'QUOTE)
		(memq (cadr (cadr form))
		      staticfy/guaranteed-static-operators)))))

(define-structure (staticfy/env
		   (conc-name staticfy/env/)
		   (constructor staticfy/env/make))
  (context false read-only true)
  (parent false read-only true)
  (bindings '() read-only true))

(define (staticfy/lookup* env name ref)
  (let loop ((env env))
    (cond ((not env)
	   (free-var-error name))
	  ((assq name (staticfy/env/bindings env))
	   => (lambda (binding)
		(set-cdr! binding (cons ref (cdr binding)))))
	  (else
	   (loop (staticfy/env/parent env)))))
  ref)

(define-integrable (staticfy/bind context env names)
  (staticfy/env/make context
		     env
		     (map list names)))