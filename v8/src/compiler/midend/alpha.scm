#| -*-Scheme-*-

$Id: a9bdd4780203760acc32b2a7d0e0e049ee09b766 $

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

(define (alphaconv/top-level program)
  (alphaconv/expr (alphaconv/state/make alphaconv/remember)
		  '()
		  program))

(define-macro (define-alphaconv keyword bindings . body)
  (let ((proc-name (symbol-append 'ALPHACONV/ keyword)))
    (call-with-values
	(lambda () (%matchup (cddr bindings) '(handler state env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name STATE ENV FORM)
	     ;; All handlers inherit FORM (and others) from the
	     ;; surrounding scope.
	     (LET ((HANDLER
		    (LAMBDA ,(cons* (car bindings) (cadr bindings) names)
		      ,@body)))
	       ,code)))))))

(define-alphaconv LOOKUP (state env name)
  state env				; ignored
  `(LOOKUP ,(alphaconv/env/lookup name env)))

(define-alphaconv LAMBDA (state env lambda-list body)
  (let* ((names     (lambda-list->names lambda-list))
	 (new-names (alphaconv/renamings env names))
	 (env*      (alphaconv/env/extend env names new-names)))
    (alphaconv/remember-renames form env*)
    (for-each (lambda (old new)
		;; Introduced names, e.g. ENV-14 from envconv
		(if (uninterned-symbol? old)
		    (dbg-info/remember old new)))
      names new-names)
    `(LAMBDA ,(alphaconv/rename-lambda-list lambda-list new-names)
       ,(alphaconv/expr state env* body))))

(define (alphaconv/rename-lambda-list lambda-list new-names)
  (let loop ((ll lambda-list) (nn new-names) (result '()))
    (cond ((null? ll) (reverse! result))
	  ((or (eq? #!optional (car ll))
	       (eq? #!rest (car ll))
	       (eq? #!aux (car ll)))
	   (loop (cdr ll) nn (cons (car ll) result)))
	  (else
	   (loop (cdr ll) (cdr nn) (cons (car nn) result))))))

(define (alphaconv/remember-renames form env*)
  (let ((info (code-rewrite/original-form/previous form)))
    (and info
	 (new-dbg-procedure? info)
	 (let ((block (new-dbg-procedure/block info)))
	   (and block
		(for-each-vector-element (new-dbg-block/variables block)
		  (lambda (var)
		    (let ((new-name
			   (alphaconv/env/lookup (new-dbg-variable/name var)
						 env*)))
		      (dbg-info/remember var new-name)))))))))

(define-alphaconv CALL (state env rator cont #!rest rands)
  `(CALL ,(alphaconv/expr state env rator)
	 ,(alphaconv/expr state env cont)
	 ,@(alphaconv/expr* state env rands)))

(define-alphaconv LET (state env bindings body)
  (alphaconv/let-like 'LET state env bindings body))

(define-alphaconv LETREC (state env bindings body)
  (alphaconv/let-like 'LETREC state env bindings body))

(define (alphaconv/let-like keyword state env bindings body)
  (let* ((names     (map car bindings))
	 (new-names (alphaconv/renamings env names))
	 (inner-env (alphaconv/env/extend env names new-names))
	 (expr-env  (if (eq? keyword 'LETREC) inner-env env))
	 (bindings*
	  (map (lambda (new-name binding)
		 (list new-name
		       (alphaconv/expr state expr-env (second binding))))
	       new-names
	       bindings)))
    `(,keyword  ,bindings*  ,(alphaconv/expr state inner-env body))))

(define-alphaconv QUOTE (state env object)
  state env				; ignored
  `(QUOTE ,object))

(define-alphaconv DECLARE (state env #!rest anything)
  state env				; ignored
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
  state env				; ignored
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
	   ((QUOTE)	    (alphaconv/quote state env expr))
	   ((LOOKUP)	    (alphaconv/lookup state env expr))
	   ((LAMBDA)	    (alphaconv/lambda state env expr))
	   ((LET)	    (alphaconv/let state env expr))
	   ((DECLARE)	    (alphaconv/declare state env expr))
	   ((CALL)	    (alphaconv/call state env expr))
	   ((BEGIN)	    (alphaconv/begin state env expr))
	   ((IF)	    (alphaconv/if state env expr))
	   ((LETREC)	    (alphaconv/letrec state env expr))
	   ((SET!)	    (alphaconv/set! state env expr))
	   ((UNASSIGNED?)   (alphaconv/unassigned? state env expr))
	   ((OR)	    (alphaconv/or state env expr))
	   ((DELAY)	    (alphaconv/delay state env expr))
	   (else
	    (illegal expr)))))
    ((alphaconv/state/remember state) new-expr expr)))

(define (alphaconv/expr* state env exprs)
  (map (lambda (expr)
	 (alphaconv/expr state env expr))
       exprs))

(define-integrable (alphaconv/remember new old)
  (code-rewrite/remember new old))

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