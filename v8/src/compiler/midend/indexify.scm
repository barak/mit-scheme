#| -*-Scheme-*-

$Id: indexify.scm,v 1.2 1995/01/28 04:04:40 adams Exp $

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

;;;; Constant folder for closure and stack closure indices
;;; package: (compiler midend)

(declare (usual-integrations))

(define (indexify/top-level program)
  (indexify/expr program))

(define-macro (define-indexifier keyword bindings . body)
  (let ((proc-name (symbol-append 'INDEXIFY/ keyword)))
    (call-with-values
     (lambda () (%matchup bindings '(handler) '(cdr form)))
     (lambda (names code)
       `(DEFINE ,proc-name
	  (LET ((HANDLER (LAMBDA ,names ,@body)))
	    (NAMED-LAMBDA (,proc-name FORM)
	      (INDEXIFY/REMEMBER ,code FORM))))))))

(define-indexifier LOOKUP (name)
  `(LOOKUP ,name))

(define-indexifier LAMBDA (lambda-list body)
  `(LAMBDA ,lambda-list
     ,(indexify/expr body)))

(define-indexifier LET (bindings body)
  `(LET ,(lmap (lambda (binding)
		 (list (car binding)
		       (indexify/expr (cadr binding))))
	       bindings)
     ,(indexify/expr body)))

(define-indexifier LETREC (bindings body)
  `(LETREC ,(lmap (lambda (binding)
		    (list (car binding)
			  (indexify/expr (cadr binding))))
		  bindings)
     ,(indexify/expr body)))

(define-indexifier IF (pred conseq alt)
  `(IF ,(indexify/expr pred)
       ,(indexify/expr conseq)
       ,(indexify/expr alt)))

(define-indexifier QUOTE (object)
  `(QUOTE ,object))

(define-indexifier DECLARE (#!rest anything)
  `(DECLARE ,@anything))

(define-indexifier BEGIN (#!rest actions)
  `(BEGIN ,@(indexify/expr* actions)))

(define-indexifier CALL (rator cont #!rest rands)
  (cond ((or (not (QUOTE/? rator))
	     (not (eq? (QUOTE/text rator) %vector-index)))
	 `(CALL ,(indexify/expr rator)
		,(indexify/expr cont)
		,@(indexify/expr* rands)))
	((or (not (equal? cont '(QUOTE #F)))
	     (not (= (length rands) 2))
	     (not (QUOTE/? (first rands)))
	     (not (QUOTE/? (second rands))))
	 (internal-error "Unexpected use of %vector-index"
			 `(CALL ,rator ,cont ,@rands)))
	(else
	 `(QUOTE ,(vector-index (QUOTE/text (first rands))
				(QUOTE/text (second rands)))))))

(define (indexify/expr expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (indexify/quote expr))
    ((LOOKUP)
     (indexify/lookup expr))
    ((LAMBDA)
     (indexify/lambda expr))
    ((LET)
     (indexify/let expr))
    ((DECLARE)
     (indexify/declare expr))
    ((CALL)
     (indexify/call expr))
    ((BEGIN)
     (indexify/begin expr))
    ((IF)
     (indexify/if expr))
    ((LETREC)
     (indexify/letrec expr))
    ((SET! UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (indexify/expr* exprs)
  (lmap (lambda (expr)
	  (indexify/expr expr))
	exprs))

(define (indexify/remember new old)
  (code-rewrite/remember new old))