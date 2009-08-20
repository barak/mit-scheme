#| -*-Scheme-*-

$Id$

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

;;;; Expansion of simple special forms
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
     (cond ((memq #!aux lambda-list)
	    => (lambda (tail)
		 (let ((rest  (list-prefix lambda-list tail))
		       (auxes (cdr tail)))
		   (if (null? auxes)
		       `(LAMBDA ,rest ,body)
		       (let ((body*
			      `(LET ,(map (lambda (aux)
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
				    (new-dbg-procedure/block info)
				    #F)))))

(define-expander LET (bindings body)
  (let ((bindings*  (map (lambda (binding)
			    (list (car binding)
				  (expand/expr (cadr binding))))
			 bindings)))
    (let ((body*  (expand/expr body)))
      (if (null? bindings*)
	  body*
	  `(LET ,bindings*
	     ,body*)))))

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

;;;; Sort assignments to AUX bindings so that ASSCONV will do a better job.
;;
;; Note: reordering the sequence of assignments makes sense only because
;; AUX bindings correspond directly to internal defines, and the order
;; of internal defines is not specified.

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
       (case (boolean/discriminate (quote/text new-pred))
	 ((TRUE)    new-pred)
	 ((FALSE)   new-alt)
	 (else	    (default))))
      ((LOOKUP)
       `(IF ,new-pred ,new-pred ,new-alt))
      ((CALL)
       (let ((rator (call/operator new-pred)))
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

(define (expand/code-compress actions)
  ;; Reduce sequences of operations that define variables in the *same*
  ;; first-class environment (%*define) into a single multi-define
  ;; (%*define*).  Only do this for variables which are defined to
  ;; simple expressions that can't generate errors or otherwise
  ;; capture the continuation (e.g. constants, compiled procedure
  ;; constants, or immediately constructed procedures).
  
  (define (->multi-values-vector exprs)
    (if (for-all? exprs QUOTE/?)
	`(QUOTE ,(list->vector (map quote/text exprs)))
	`(CALL (QUOTE ,%vector)
	       (QUOTE #F)
	       ,@exprs)))

  (define (->multi-define defns)
    `(CALL (QUOTE ,%*define*)
	   (QUOTE #F)
	   ,(call/%*define/environment (car defns))
	   (QUOTE ,(list->vector
		    (map (lambda (defn)
			   (quote/text (call/%*define/variable-name defn)))
			 defns)))
	   ,(->multi-values-vector
	     (map call/%*define/value defns))))

  (define (collect defns actions)
    (cond ((null? defns) actions)
	  ((null? (cdr defns))
	   (append defns actions))
	  (else
	   (cons (->multi-define (reverse defns))
		 actions))))

  (define (expand/code-compress/trivial? expr)
    (or (QUOTE/? expr)
	(LAMBDA/? expr)))

  (let loop ((actions actions)
	     (defns '())
	     (actions* '()))
    (define (next defns actions*)
      (loop (cdr actions) defns actions*))
    (if (null? actions)
	(beginnify (reverse (collect defns actions*)))
	(let ((action (car actions)))
	  (cond ((not (and (CALL/%*define? action)
			   (expand/code-compress/trivial?
			    (call/%*define/value action))))
		 (next '()
		       (cons action
			     (collect defns actions*))))
		((or (null? defns)
		     (not (equal? (call/%*define/environment action)
				  (call/%*define/environment (car defns)))))
		 (next (list action)
		       (collect defns actions*)))
		(else
		 (next (cons action defns)
		       actions*)))))))
