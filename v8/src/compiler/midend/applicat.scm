#| -*-Scheme-*-

$Id: applicat.scm,v 1.2 1995/02/02 19:35:50 adams Exp $

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

;;;; Use special pseudo primitives to call funky stuff
;;; package: (compiler midend)

(declare (usual-integrations))

(define (applicat/top-level program)
  (applicat/expr '() program))

(define-macro (define-applicator keyword bindings . body)
  (let ((proc-name (symbol-append 'APPLICAT/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (APPLICAT/REMEMBER ,code
				  FORM))))))))

(define-applicator LOOKUP (env name)
  env					; ignored
  `(LOOKUP ,name))

(define-applicator LAMBDA (env lambda-list body)
  `(LAMBDA ,lambda-list
     ,(applicat/expr (append (lmap (lambda (name)
				      (list name false))
				    (lambda-list->names lambda-list))
			      env)
		      body)))

(define-applicator QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-applicator DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-applicator BEGIN (env #!rest actions)
  `(BEGIN ,@(applicat/expr* env actions)))

(define-applicator IF (env pred conseq alt)
  `(IF ,(applicat/expr env pred)
       ,(applicat/expr env conseq)
       ,(applicat/expr env alt)))

(define-applicator CALL (env rator cont #!rest rands)
  (define (default)
    `(CALL (QUOTE ,%internal-apply)
	   ,(applicat/expr env cont)
	   (QUOTE ,(length rands))
	   ,(applicat/expr env rator)
	   ,@(applicat/expr* env rands)))
  (cond ((QUOTE/? rator)
	 (cond ((and (known-operator? (cadr rator))
		     (not (and (primitive-procedure? (cadr rator))
			       (memq (primitive-procedure-name (cadr rator))
				     compiler:primitives-with-no-open-coding))))
		`(CALL ,(applicat/expr env rator)
		       ,(applicat/expr env cont)
		       ,@(applicat/expr* env rands)))
	       ((primitive-procedure? (cadr rator))
		`(CALL (QUOTE ,%primitive-apply)
		       ,(applicat/expr env cont)
		       (QUOTE ,(length rands))
		       ,(applicat/expr env rator)
		       ,@(applicat/expr* env rands)))
	       (else
		(default))))
	((LOOKUP/? rator)
	 (let ((place (assq (cadr rator) env)))
	   (if (or (not place) (not (cadr place)))
	       (default)
	       `(CALL ,(applicat/expr env rator)
		      ,(applicat/expr env cont)
		      ,@(applicat/expr* env rands)))))
	((LAMBDA/? rator)
	 (let* ((lambda-list (cadr rator))
		(rator* `(LAMBDA ,lambda-list
			   ,(applicat/expr
			     (append
			      (map (lambda (name rand)
				     (list name
					   (and (pair? rand)
						(eq? (car rand) 'LAMBDA))))
				   lambda-list
				   rands)
			      env)
			     (caddr rator)))))
	   `(CALL ,(applicat/remember rator* rator)
		  ,(applicat/expr env cont)
		  ,@(applicat/expr* env rands))))
	(else
	 (default))))

(define-applicator LET (env bindings body)
  `(LET ,(lmap (lambda (binding)
		 (list (car binding)
		       (applicat/expr env (cadr binding))))
	       bindings)
     ,(applicat/expr
       (append (lmap (lambda (binding)
		       (list (car binding)
			     (let ((value (cadr binding)))
			       (and (pair? value)
				    (eq? (car value) 'LAMBDA)))))
		     bindings)
	       env)
       body)))

(define-applicator LETREC (env bindings body)
  (let ((env*
	 (append (lmap (lambda (binding)
			 (list (car binding)
			       (let ((value (cadr binding)))
				 (and (pair? value)
				      (eq? (car value) 'LAMBDA)))))
		       bindings)
		 env)))
    `(LETREC ,(lmap (lambda (binding)
		      (list (car binding)
			    (applicat/expr env* (cadr binding))))
		    bindings)
       ,(applicat/expr env* body))))

(define (applicat/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (applicat/quote env expr))
    ((LOOKUP)
     (applicat/lookup env expr))
    ((LAMBDA)
     (applicat/lambda env expr))
    ((LET)
     (applicat/let env expr))
    ((DECLARE)
     (applicat/declare env expr))
    ((CALL)
     (applicat/call env expr))
    ((BEGIN)
     (applicat/begin env expr))
    ((IF)
     (applicat/if env expr))
    ((LETREC)
     (applicat/letrec env expr))
    ((SET! UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (applicat/expr* env exprs)
  (lmap (lambda (expr)
	  (applicat/expr env expr))
	exprs))

(define (applicat/remember new old)
  (code-rewrite/remember new old))

(define (applicat/new-name prefix)
  (new-variable prefix))