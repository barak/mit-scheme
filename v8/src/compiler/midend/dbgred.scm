#| -*-Scheme-*-

$Id: dbgred.scm,v 1.1 1995/01/30 16:17:17 adams Exp $

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

;;;; Reduce debugging expressions to canonical form
;;; package: (compiler midend)

(declare (usual-integrations))

(define *dbgt*)
(define (dbg-reduce/top-level program)
  (set! *dbgt* (make-eq-hash-table))
  (dbg-reduce/expr (dbg-reduce/initial-env) program)
  program)


(define-macro (define-dbg-reducer keyword bindings . body)
  (let ((proc-name (symbol-append 'DBG-REDUCE/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     ;; All handlers inherit ENV and FORM from the surrounding scope.
	     (LET ((HANDLER
		    (LAMBDA ,(cons* (car bindings) names) ,@body)))
	       ,code)))))))

(define-dbg-reducer LOOKUP (name)
  name ; unused
  (dbg-reduce/reduce form env))

(define-dbg-reducer LAMBDA (lambda-list body)
  ;; redefine dynamic frame
  (define (dbg-reduce/parse-frame)
    ;;(match body
    ;;  ((LET ((_  (CALL ',%fetch-stack-closure _ '(? frame-vector))))) =>
    ;;   deal)
    ;;  (else no-deal))
    (let ((frame-vector
	    (and (LET/? body)
		 (pair? (let/bindings body))
		 (CALL/%fetch-stack-closure?
		  (second (first (let/bindings body))))
		 (QUOTE/text 
		  (CALL/%fetch-stack-closure/vector
		   (second (first (let/bindings body))))))))
      (let* ((args   (lambda-list->names lambda-list))
	     (nargs  (length args)))
	(map* (if frame-vector 
	'?
		  '())
	      (lambda (arg index)
		(cons arg index))
	      args
	      (iota nargs))
	'())))
  
  (let ((env* (dbg-reduce/env/new-frame env (dbg-reduce/parse-frame))))
    (dbg-reduce/reduce form env*)
    (dbg-reduce/expr env* body)))

(define-dbg-reducer LET (bindings body)
  (for-each (lambda (binding)
	      (dbg-reduce/expr env (cadr binding)))
	    bindings)
  (dbg-reduce/expr env body))

(define-dbg-reducer LETREC (bindings body)
  ;; add static bindings
  (let ((env* (dbg-reduce/env/extend-static env (map car bindings))))
    (for-each (lambda (binding)
		(dbg-reduce/expr env* (cadr bindings)))
	      bindings)
    (dbg-reduce/expr env* body)))

(define-dbg-reducer IF (pred conseq alt)
  (dbg-reduce/reduce form env)
  (dbg-reduce/expr env pred)
  (dbg-reduce/expr env conseq)
  (dbg-reduce/expr env alt))

(define-dbg-reducer QUOTE (object)
  env object				; unused
  (dbg-reduce/reduce form env))

(define-dbg-reducer DECLARE (#!rest anything)
  env anything				; unused
  (dbg-reduce/reduce form env))

(define-dbg-reducer BEGIN (#!rest actions)
  (dbg-reduce/reduce form env)
  (dbg-reduce/expr* actions))

(define-dbg-reducer CALL (rator cont #!rest rands)
  (dbg-reduce/reduce form env)
  (dbg-reduce/expr env rator)
  (dbg-reduce/expr env cont)
  (dbg-reduce/expr* env rands))

(define (dbg-reduce/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (dbg-reduce/quote expr))
    ((LOOKUP)
     (dbg-reduce/lookup expr))
    ((LAMBDA)
     (dbg-reduce/lambda expr))
    ((LET)
     (dbg-reduce/let expr))
    ((DECLARE)
     (dbg-reduce/declare expr))
    ((CALL)
     (dbg-reduce/call expr))
    ((BEGIN)
     (dbg-reduce/begin expr))
    ((IF)
     (dbg-reduce/if expr))
    ((LETREC)
     (dbg-reduce/letrec expr))
    ((SET! UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (dbg-reduce/expr* env exprs)
  (lmap (lambda (expr)
	  (dbg-reduce/expr env expr))
	exprs))

(define-structure
    (dbg-reduce/env
     (conc-name dbg-reduce/env/)
     (constructor dbg-reduce/env/%make))
  ;; Static objects: a list of `labels'
  static				
  ;; Dynamic objects (in current frame).  A list of (name . offset) pairs
  frame					
  )

(define (dbg-reduce/initial-env)
  (dbg-reduce/env/%make '() '(())))

(define (dbg-reduce/env/new-frame env frame*)
  (dbg-reduce/env/%make (dbg-reduce/env/static env)
			frame*))

(define (dbg-reduce/env/extend-static env static*)
  (dbg-reduce/env/%make (append static* (dbg-reduce/env/static env))
			(dbg-reduce/env/frame env)))

(define (dbg-reduce/reduce form env)
  ;; rewrite the debugging info for FORM
  (hash-table/put! *dbgt* form env)
  unspecific)
