#| -*-Scheme-*-

$Id: copier.scm,v 1.2 1994/11/20 00:41:36 jmiller Exp $

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

(declare (usual-integrations))

(define (copier/top-level program remember)
  (copier/expr remember program))

(define-macro (define-copier-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'COPIER/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdr bindings) '(handler state) '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
	  (let ((handler (lambda ,(cons (car bindings) names) ,@body)))
	    (named-lambda (,proc-name state form)
	      (copier/remember ,code
			       form))))))))

(define-copier-handler LOOKUP (state name)
  state					; ignored
  `(LOOKUP ,name))

(define-copier-handler LAMBDA (state lambda-list body)
  `(LAMBDA ,lambda-list
     ,(copier/expr state body)))

(define-copier-handler CALL (state rator cont #!rest rands)
  `(CALL ,(copier/expr state rator)
	 ,(copier/expr state cont)
	 ,@(copier/expr* state rands)))

(define-copier-handler LET (state bindings body)
  `(LET ,(lmap (lambda (binding)
		 (list (car binding)
		       (copier/expr state (cadr binding))))
	       bindings)
     ,(copier/expr state body)))

(define-copier-handler LETREC (state bindings body)
  `(LETREC ,(lmap (lambda (binding)
		    (list (car binding)
			  (copier/expr state (cadr binding))))
		  bindings)
     ,(copier/expr state body)))

(define-copier-handler QUOTE (state object)
  state					; ignored
  `(QUOTE ,object))

(define-copier-handler DECLARE (state #!rest anything)
  state					; ignored
  `(DECLARE ,@anything))

(define-copier-handler BEGIN (state #!rest actions)
  `(BEGIN ,@(copier/expr* state actions)))

(define-copier-handler IF (state pred conseq alt)
  `(IF ,(copier/expr state pred)
       ,(copier/expr state conseq)
       ,(copier/expr state alt)))

(define-copier-handler SET! (state name value)
  `(SET! ,name ,(copier/expr state value)))

(define-copier-handler ACCESS (state name env-expr)
  `(ACCESS ,name ,(copier/expr state env-expr)))

(define-copier-handler UNASSIGNED? (state name)
  state					; ignored
  `(UNASSIGNED? ,name))

(define-copier-handler OR (state pred alt)
  `(OR ,(copier/expr state pred)
       ,(copier/expr state alt)))

(define-copier-handler DELAY (state expr)
  `(DELAY ,(copier/expr state expr)))

(define-copier-handler DEFINE (state name value)
  `(DEFINE ,name ,(copier/expr state value)))

(define-copier-handler IN-PACKAGE (state envexpr bodyexpr)
  `(IN-PACKAGE ,(copier/expr state envexpr)
               ,(copier/expr state bodyexpr)))

(define-copier-handler THE-ENVIRONMENT (state)
  state					; ignored
  `(THE-ENVIRONMENT))


(define (copier/expr state expr)
  (if (not (pair? expr))
      (illegal expr))
  (state (case (car expr)
	   ((QUOTE)
	    (copier/quote state expr))
	   ((LOOKUP)
	    (copier/lookup state expr))
	   ((LAMBDA)
	    (copier/lambda state expr))
	   ((LET)
	    (copier/let state expr))
	   ((DECLARE)
	    (copier/declare state expr))
	   ((CALL)
	    (copier/call state expr))
	   ((BEGIN)
	    (copier/begin state expr))
	   ((IF)
	    (copier/if state expr))
	   ((LETREC)
	    (copier/letrec state expr))
	   ((SET!)
	    (copier/set! state expr))
	   ((UNASSIGNED?)
	    (copier/unassigned? state expr))
	   ((OR)
	    (copier/or state expr))
	   ((DELAY)
	    (copier/delay state expr))
	   ((ACCESS)
	    (copier/access state expr))
	   ((DEFINE)
	    (copier/define state expr))
	   ((IN-PACKAGE)
	    (copier/in-package state expr))
	   ((THE-ENVIRONMENT)
	    (copier/the-environment state expr))
	   (else
	    (illegal expr)))
	 expr))

(define (copier/expr* state exprs)
  (lmap (lambda (expr)
	  (copier/expr state expr))
	exprs))

(define-integrable (copier/remember new old)
  old					; ignored for now and forever
  new)
