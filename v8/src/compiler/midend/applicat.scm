#| -*-Scheme-*-

$Id: e8409b07b788ee3b38950b0df05a859e4ef5279b $

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
	       (checked-call)
	       (direct-call))))
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
	 (checked-call))))

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