#| -*-Scheme-*-

$Id: alpha.scm,v 1.2 1994/11/20 00:40:41 jmiller Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (alphaconv/top-level program)
  (alphaconv/expr (alphaconv/state/make alphaconv/remember)
		  '()
		  program))

(define-macro (define-alphaconv keyword bindings . body)
  (let ((proc-name (symbol-append 'ALPHACONV/ keyword)))
    (call-with-values
	(lambda () (%matchup (cddr bindings) '(handler state env) '(cdr form)))
      (lambda (names code)
	`(define ,proc-name
	   (let ((handler (lambda ,(cons* (car bindings) (cadr bindings) names) ,@body)))
	     (named-lambda (,proc-name state env form)
	       ,code)))))))

(define-alphaconv LOOKUP (state env name)
  env					; ignored
  `(LOOKUP ,(alphaconv/env/lookup name env)))

(define-alphaconv LAMBDA (state env lambda-list body)
  (let* ((names     (lambda-list->names lambda-list))
	 (new-names (alphaconv/renamings env names))
	 (env*      (alphaconv/env/extend env names new-names)))
    `(LAMBDA ,(alphaconv/rename-lambda-list lambda-list new-names)
       ,(alphaconv/expr state env* body))))

(define (alphaconv/rename-lambda-list lambda-list new-names)
  (let loop ((ll lambda-list) (nn new-names) (result '()))
    (cond ((null? ll) (reverse! result))
	  ((memq (car ll) '(#!AUX #!OPTIONAL #!REST))
	   (loop (cdr ll) nn (cons (car ll) result)))
	  (else
	   (loop (cdr ll) (cdr nn) (cons (car nn) result))))))

(define-alphaconv CALL (state env rator cont #!rest rands)
  `(CALL ,(alphaconv/expr state env rator)
	 ,(alphaconv/expr state env cont)
	 ,@(alphaconv/expr* state env rands)))

(define-alphaconv LET (state env bindings body)
  (alphaconv/let-like 'LET state env bindings body))

(define-alphaconv LETREC (state env bindings body)
  (alphaconv/let-like 'LETREC state env bindings body))

(define (alphaconv/let-like keyword state env bindings body)
  (let* ((names     (lmap car bindings))
	 (new-names (alphaconv/renamings env names))
	 (inner-env (alphaconv/env/extend env names new-names))
	 (expr-env  (if (eq? keyword 'LETREC) inner-env env))
	 (bindings* (map (lambda (new-name binding)
			   (list new-name
				 (alphaconv/expr state expr-env (second binding))))
			 new-names
			 bindings)))
    `(,keyword  ,bindings*  ,(alphaconv/expr state inner-env body))))

(define-alphaconv QUOTE (state env object)
  env					; ignored
  `(QUOTE ,object))

(define-alphaconv DECLARE (state env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-alphaconv BEGIN (state env #!rest actions)
  `(BEGIN ,@(alphaconv/expr* state env actions)))

(define-alphaconv IF (state env pred conseq alt)
  `(IF ,(alphaconv/expr state env pred)
       ,(alphaconv/expr state env conseq)
       ,(alphaconv/expr state env alt)))

(define-alphaconv SET! (state env name value)
  `(SET! ,(alphaconv/env/lookup name env) ,(alphaconv/expr state env value)))

(define-alphaconv UNASSIGNED? (state env name)
  env					; ignored
  `(UNASSIGNED? ,(alphaconv/env/lookup name env)))

(define-alphaconv OR (state env pred alt)
  `(OR ,(alphaconv/expr state env pred)
       ,(alphaconv/expr state env alt)))

(define-alphaconv DELAY (state env expr)
  `(DELAY ,(alphaconv/expr state env expr)))

(define (alphaconv/expr state env expr)
  (if (not (pair? expr))
      (illegal expr))
  (let ((new-expr
	 (case (car expr)
	   ((QUOTE)
	    (alphaconv/quote state env expr))
	   ((LOOKUP)
	    (alphaconv/lookup state env expr))
	   ((LAMBDA)
	    (alphaconv/lambda state env expr))
	   ((LET)
	    (alphaconv/let state env expr))
	   ((DECLARE)
	    (alphaconv/declare state env expr))
	   ((CALL)
	    (alphaconv/call state env expr))
	   ((BEGIN)
	    (alphaconv/begin state env expr))
	   ((IF)
	    (alphaconv/if state env expr))
	   ((LETREC)
	    (alphaconv/letrec state env expr))
	   ((SET!)
	    (alphaconv/set! state env expr))
	   ((UNASSIGNED?)
	    (alphaconv/unassigned? state env expr))
	   ((OR)
	    (alphaconv/or state env expr))
	   ((DELAY)
	    (alphaconv/delay state env expr))
	   ((ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
	    (no-longer-legal expr))
	   (else
	    (illegal expr)))))
    ((alphaconv/state/remember state) new-expr expr)))

(define (alphaconv/expr* state env exprs)
  (lmap (lambda (expr)
	  (alphaconv/expr state env expr))
	exprs))

(define-integrable (alphaconv/remember new old)
  old					; ignored for now and forever
  new)

(define-structure
  (alphaconv/state
   (conc-name alphaconv/state/)
   (constructor alphaconv/state/make))
  remember)
  


(define-structure
  (alphaconv/binding
   (conc-name alphaconv/binding/)
   (constructor alphaconv/binding/make (name renaming))
   (print-procedure
    (standard-unparser-method 'ALPHACONV/BINDING
      (lambda (binding port)
	(write-char #\space port)
	(write-string (symbol-name (alphaconv/binding/name binding)) port)))))

  (name false read-only true)
  (renaming false read-only true))

(define alphaconv/env/lookup 
  (let ((finder (association-procedure eq? alphaconv/binding/name)))
    (lambda (name env)
      (cond ((finder name env)
	     => (lambda (binding)
		  (alphaconv/binding/renaming binding)))
	    (else
	     name)))))

(define (alphaconv/env/extend env names new-names)
  (map* env
	alphaconv/binding/make
	names
	new-names))

(define (alphaconv/renamings env names)
  env					; ignored
  (map (lambda (name)
	 (variable/rename name))
       names))