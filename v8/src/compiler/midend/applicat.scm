#| -*-Scheme-*-

$Id: applicat.scm,v 1.6 1996/03/08 22:27:09 adams Exp $

Copyright (c) 1994-1996 Massachusetts Institute of Technology

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
	`(DEFINE (,proc-name ENV FORM)
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (APPLICAT/REMEMBER ,code
				FORM)))))))

(define-applicator LOOKUP (env name)
  env					; ignored
  `(LOOKUP ,name))

(define-applicator LAMBDA (env lambda-list body)
  `(LAMBDA ,lambda-list
     ,(applicat/expr (map* env
			   (lambda (name)
			     (list name false))
			   (lambda-list->names lambda-list))
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
  (define (direct-call)
    `(CALL ,(applicat/expr env rator)
	   ,(applicat/expr env cont)
	   ,@(applicat/expr* env rands)))
  (define (checked-call)
    `(CALL (QUOTE ,%internal-apply)
	   ,(applicat/expr env cont)
	   (QUOTE ,(length rands))
	   ,(applicat/expr env rator)
	   ,@(applicat/expr* env rands)))
  (define (primitive-call)
    `(CALL (QUOTE ,%primitive-apply)
	   ,(applicat/expr env cont)
	   (QUOTE ,(length rands))
	   ,(applicat/expr env rator)
	   ,@(applicat/expr* env rands)))
  (define (check-primitive operator method-if-good)
    (let ((arity (primitive-procedure-arity operator)))
      (if (or (eqv? arity (length rands))
	      (eqv? arity -1))		; VECTOR & %RECORD
	  (method-if-good)
	  (begin
	    (warn
	     (string-append
	      ;;"Primitive "
	      (string-upcase (symbol-name (primitive-procedure-name operator)))
	      " called with wrong number of arguments")
	     (form->source-irritant form))
	    (checked-call)))))
  (cond ((QUOTE/? rator)
	 (let* ((operator   (quote/text rator))
		(known?     (known-operator? operator))
		(primitive? (primitive-procedure? operator)))
	   (cond ((and known? primitive?)
		  (if (memq (primitive-procedure-name operator)
			    compiler:primitives-with-no-open-coding)
		      (primitive-call)
		      (check-primitive operator direct-call)))
		 (known?      (direct-call))
		 (primitive?  (check-primitive operator primitive-call))
		 (else        (checked-call)))))
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
			     (map* env
				   (lambda (name rand)
				     (list name (LAMBDA/? rand)))
				   lambda-list
				   rands)
			     (caddr rator)))))
	   `(CALL ,(applicat/remember rator* rator)
		  ,(applicat/expr env cont)
		  ,@(applicat/expr* env rands))))
	(else
	 (default))))

(define-applicator LET (env bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (applicat/expr env (cadr binding))))
	      bindings)
     ,(applicat/expr
       (map* env
	     (lambda (binding)
	       (list (car binding)
		     (let ((value (cadr binding)))
		       (LAMBDA/?  value))))
	     bindings)
       body)))

(define-applicator LETREC (env bindings body)
  (let ((env*  (map* env
		     (lambda (binding)
		       (list (car binding)
			     (let ((value (cadr binding)))
			       (LAMBDA/? value))))
		     bindings)))
    `(LETREC ,(map (lambda (binding)
		     (list (car binding)
			   (applicat/expr env* (cadr binding))))
		   bindings)
       ,(applicat/expr env* body))))

(define (applicat/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (applicat/quote env expr))
    ((LOOKUP)   (applicat/lookup env expr))
    ((LAMBDA)   (applicat/lambda env expr))
    ((LET)      (applicat/let env expr))
    ((DECLARE)  (applicat/declare env expr))
    ((CALL)     (applicat/call env expr))
    ((BEGIN)    (applicat/begin env expr))
    ((IF)       (applicat/if env expr))
    ((LETREC)   (applicat/letrec env expr))
    (else
     (illegal expr))))

(define (applicat/expr* env exprs)
  (map (lambda (expr)
	 (applicat/expr env expr))
       exprs))

(define (applicat/remember new old)
  (code-rewrite/remember new old))

(define (applicat/new-name prefix)
  (new-variable prefix))