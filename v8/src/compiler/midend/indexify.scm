#| -*-Scheme-*-

$Id: 32c0b5371e440ba740422ff42f0cb90717a489d5 $

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

;;;; Constant folder for closure and stack closure indices
;;; package: (compiler midend)

(declare (usual-integrations))

(define (indexify/top-level program)
  (indexify/do-dbg-info!)
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
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (indexify/expr (cadr binding))))
	      bindings)
     ,(indexify/expr body)))

(define-indexifier LETREC (bindings body)
  `(LETREC ,(map (lambda (binding)
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
    ((QUOTE)    (indexify/quote expr))
    ((LOOKUP)   (indexify/lookup expr))
    ((LAMBDA)   (indexify/lambda expr))
    ((LET)      (indexify/let expr))
    ((DECLARE)  (indexify/declare expr))
    ((CALL)     (indexify/call expr))
    ((BEGIN)    (indexify/begin expr))
    ((IF)       (indexify/if expr))
    ((LETREC)   (indexify/letrec expr))
    (else       (illegal expr))))

(define (indexify/expr* exprs)
  (map (lambda (expr)
	 (indexify/expr expr))
       exprs))

(define (indexify/remember new old)
  (code-rewrite/remember new old))

(define (indexify/do-dbg-info!)
  (define (rewrite-indexifies! expr)
    (cond ((dbg/stack-closure-ref? expr)
	   (rewrite-indexifies! (vector-ref expr 1)))
          ((dbg/heap-closure-ref? expr)
	   (rewrite-indexifies! (vector-ref expr 1)))
	  ((QUOTE/? expr))
	  ((LOOKUP/? expr))
	  ((and (CALL/? expr)
		(QUOTE/? (call/operator expr))
		(eq? %vector-index (quote/text (call/operator expr)))
		(for-all? (call/cont-and-operands expr) QUOTE/?))
	   (internal-error "%vector-index found in DBG info")
	   (let ((rands (call/operands expr)))
	     (form/rewrite! expr
	       `(QUOTE ,(vector-index (QUOTE/text (first rands))
				      (QUOTE/text (second rands)))))))
	  ((pair? expr)
	   (map rewrite-indexifies! expr))))
	  
  (dbg-info/for-all-dbg-expressions! rewrite-indexifies!))
   