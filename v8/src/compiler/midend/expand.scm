#| -*-Scheme-*-

$Id: expand.scm,v 1.4 1995/02/27 23:05:55 adams Exp $

Copyright (c) 1994-1995 Massachusetts Institute of Technology

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

;;;; Simple special form expansion
;;; package: (compiler midend)

(declare (usual-integrations))

(define (expand/top-level program)
  (expand/expr program))

(define-macro (define-expander keyword bindings . body)
  (let ((proc-name (symbol-append 'EXPAND/ keyword)))
    (call-with-values
	(lambda ()
	  (%matchup bindings '(HANDLER) '(CDR FORM)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,names ,@body)))
	     (NAMED-LAMBDA (,proc-name FORM)
	       (EXPAND/REMEMBER ,code
				FORM))))))))

;;;; Core forms: simply expand components

(define-expander QUOTE (object)
  `(QUOTE ,object))

(define-expander LOOKUP (name)
  `(LOOKUP ,name))

(define-expander SET! (name value)
  `(SET! ,name ,(expand/expr value)))

#|
(define-expander LAMBDA (lambda-list body)
  (expand/rewrite/lambda lambda-list (expand/expr body)))
|#

(define (expand/lambda form)
  (expand/remember
   (let ((lambda-list (lambda/formals form))
	 (body        (expand/expr (lambda/body form))))
     (cond ((memq '#!AUX lambda-list)
	    => (lambda (tail)
		 (let ((rest  (list-prefix lambda-list tail))
		       (auxes (cdr tail)))
		   (if (null? auxes)
		       `(LAMBDA ,rest ,body)
		       (let ((body*
			      `(LET ,(lmap (lambda (aux)
					     (list aux `(QUOTE ,%unassigned)))
					   auxes)
				 ,(expand/aux/sort auxes body))))
			 (expand/split-block body* form)
			 `(LAMBDA ,rest
			    ,body*))))))
	   (else
	    `(LAMBDA ,lambda-list ,body))))
   form))

(define (expand/split-block new-form form)
  (let ((info (code-rewrite/original-form/previous form)))
    (and info
	 (new-dbg-procedure? info)
	 (expand/remember*
	  new-form
	  (new-dbg-expression/make2 false
				    (new-dbg-procedure/block info))))))

(define-expander LET (bindings body)
  (expand/let* expand/letify bindings body))

(define-expander DECLARE (#!rest anything)
  `(DECLARE ,@anything))

(define-expander CALL (rator cont #!rest rands)
  `(CALL ,(expand/expr rator)
	 ,(expand/expr cont)
	 ,@(expand/expr* rands)))

(define-expander BEGIN (#!rest actions)
  (expand/code-compress (expand/expr* actions)))

(define-expander IF (pred conseq alt)
  `(IF ,(expand/expr pred)
       ,(expand/expr conseq)
       ,(expand/expr alt)))

;;;; Sort AUX bindings so that ASSCONV will do a better job.

(define (expand/aux/sort auxes body)
  (if (not (BEGIN/? body))
      body
      (let loop ((actions  (simplify-actions (cdr body)))
		 (last     false)
		 (decls    '())
		 (early    '())
		 (late     '()))

	(define (done)
	  (beginnify
	   (append decls
		   (reverse early)
		   (reverse late)
		   (cond ((not (null? actions))
			  actions)
			 ((not last)
			  (user-error "Empty body" body))
			 (else
			  ;; MIT Scheme semantics: the value of a
			  ;; DEFINE is the name defined.
			  (list `(QUOTE ,(set!/name last))))))))

	(if (or (null? actions)
		(not (pair? (car actions))))
	    (done)
	    (let ((action (car actions)))
	      (case (car action)
		((SET!)
		 (if (not (memq (set!/name action) auxes))
		     (done)
		     (let ((value (set!/expr action))
			   (next
			    (lambda (early* late*)
			      (loop (cdr actions) action
				    decls early* late*))))
		       (set! auxes (delq (set!/name action) auxes))
		       (if (or (QUOTE/? value)
			       (LAMBDA/? value))
			   (next (cons action early) late)
			   (next early (cons action late))))))
		((DECLARE)
		 (loop (cdr actions)
		       last (cons action decls)
		       early late))
		(else
		 (done))))))))

;;;; Derived forms: macro expand

(define-expander UNASSIGNED? (name)
  `(CALL (QUOTE ,%unassigned?) (QUOTE #F) (LOOKUP ,name)))

(define-expander OR (pred alt)
  ;; Trivial optimization here.
  (let ((new-pred  (expand/expr pred))
	(new-alt   (expand/expr alt)))

    (define (default)
      (let ((new-name (expand/new-name 'OR)))
	(bind new-name
	      new-pred
	      `(IF (LOOKUP ,new-name)
		   (LOOKUP ,new-name)
		   ,new-alt))))

    (case (car new-pred)
      ((QUOTE)
       (case (boolean/discriminate (cadr new-pred))
	 ((TRUE)    new-pred)
	 ((FALSE)   new-alt)
	 (else	    (default))))
      ((LOOKUP)
       `(IF ,new-pred ,new-pred ,new-alt))
      ((CALL)
       (let ((rator (cadr new-pred)))
	 (if (and (QUOTE/? rator)
		  (operator/satisfies? (quote/text rator) '(PROPER-PREDICATE)))
	     `(IF ,new-pred (QUOTE #T) ,new-alt)
	     (default))))
      (else
       (default)))))

(define-expander DELAY (expr)
  `(CALL (QUOTE ,%make-promise)
	 (QUOTE #F)
	 (LAMBDA (,(new-continuation-variable))
	   ,(expand/expr expr))))

(define (expand/expr expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)        (expand/quote expr))
    ((LOOKUP)       (expand/lookup expr))
    ((LAMBDA)       (expand/lambda expr))
    ((LET)          (expand/let expr))
    ((DECLARE)      (expand/declare expr))
    ((CALL)         (expand/call expr))
    ((BEGIN)        (expand/begin expr))
    ((IF)           (expand/if expr))
    ((SET!)         (expand/set! expr))
    ((UNASSIGNED?)  (expand/unassigned? expr))
    ((OR)           (expand/or expr))
    ((DELAY)        (expand/delay expr))
    ((LETREC)       (not-yet-legal expr))
    (else           (illegal expr))))

(define (expand/expr* exprs)
  (map expand/expr exprs))

(define (expand/remember new old)
  (code-rewrite/remember new old))

(define (expand/remember* new old)
  (code-rewrite/remember* new old))

(define (expand/new-name prefix)
  (new-variable prefix))

(define (expand/let* letify bindings body)
  (let ((bindings*  (map (lambda (binding)
			    (list (car binding)
				  (expand/expr (cadr binding))))
			 bindings)))
    (let ((body*  (expand/expr body)))
      (if (null? bindings*)
	  body*
	  (letify bindings* body*)))))

(define (expand/letify bindings body)
  `(LET ,bindings
     ,body))

(define (expand/pseudo-letify rator bindings body)
  (pseudo-letify rator bindings body expand/remember))

(define (expand/bindify lambda-list operands)
  (map (lambda (name operand) (list name operand))
       (lambda-list->names lambda-list)
       (lambda-list/applicate lambda-list operands)))

(define (expand/code-compress actions)
  (define (->vector exprs)
    (if (not (for-all? exprs
	       (lambda (expr)
		 (and (pair? expr)
		      (eq? (car expr) 'QUOTE)))))
	`(CALL (QUOTE ,%vector)
	       (QUOTE #F)
	       ,@exprs)
	`(QUOTE ,(list->vector (lmap cadr exprs)))))

  (define (->multi-define defns)
    `(CALL (QUOTE ,%*define*)
	   (QUOTE #F)
	   ,(list-ref (car defns) 3)
	   (QUOTE ,(list->vector (lmap (lambda (defn)
					 (cadr (list-ref defn 4)))
				       defns)))
	   ,(->vector
	     (lmap (lambda (defn)
		     (list-ref defn 5))
		   defns))))

  (define (collect defns actions)
    (cond ((null? defns) actions)
	  ((null? (cdr defns))
	   (append defns actions))
	  (else
	   (cons (->multi-define (reverse defns))
		 actions))))

  (let loop ((actions actions)
	     (defns '())
	     (actions* '()))
    (if (null? actions)
	(beginnify (reverse (collect defns actions*)))
	(let ((action (car actions)))
	  (cond ((not (and (CALL/%*define? action)
			   (expand/code-compress/trivial?
			    (call/%*define/value action))))
		 (loop (cdr actions)
		       '()
		       (cons action
			     (collect defns actions*))))
		((or (null? defns)
		     (not (equal? (list-ref action 3)
				  (list-ref (car defns) 3))))
		 (loop (cdr actions)
		       (list action)
		       (collect defns actions*)))
		(else
		 (loop (cdr actions)
		       (cons action defns)
		       actions*)))))))

(define (expand/code-compress/trivial? expr)
  (or (QUOTE/? expr)
      (and (LAMBDA/? expr)
	   #| (let ((params (cadr expr)))
		(if (or (null? params)
			(null? cdr params)
			(not (null? (cddr params))))
		    (internal-error
		     "EXPAND/CODE-COMPRESS/TRIVIAL? param error"
		     params)
		    (ignored-variable? (second params))))
           |# )))
