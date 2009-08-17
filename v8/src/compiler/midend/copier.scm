#| -*-Scheme-*-

$Id: 662373e4cf1cc05b7c9920d5178ff227920791a4 $

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
	      form			; possible fnord
	      ,code)))))))

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
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (copier/expr state (cadr binding))))
	      bindings)
     ,(copier/expr state body)))

(define-copier-handler LETREC (state bindings body)
  `(LETREC ,(map (lambda (binding)
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
	   ((QUOTE)	    (copier/quote state expr))
	   ((LOOKUP)	    (copier/lookup state expr))
	   ((LAMBDA)	    (copier/lambda state expr))
	   ((LET)	    (copier/let state expr))
	   ((DECLARE)	    (copier/declare state expr))
	   ((CALL)	    (copier/call state expr))
	   ((BEGIN)	    (copier/begin state expr))
	   ((IF)	    (copier/if state expr))
	   ((LETREC)	    (copier/letrec state expr))
	   ((SET!)	    (copier/set! state expr))
	   ((UNASSIGNED?)   (copier/unassigned? state expr))
	   ((OR)	    (copier/or state expr))
	   ((DELAY)	    (copier/delay state expr))
	   ((ACCESS)	    (copier/access state expr))
	   ((DEFINE)	    (copier/define state expr))
	   ((IN-PACKAGE)    (copier/in-package state expr))
	   ((THE-ENVIRONMENT)	    (copier/the-environment state expr))
	   (else
	    (illegal expr)))
	 expr))

(define (copier/expr* state exprs)
  (map (lambda (expr)
	 (copier/expr state expr))
       exprs))
