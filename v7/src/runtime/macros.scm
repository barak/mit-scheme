#| -*-Scheme-*-

$Id: macros.scm,v 1.4 2001/12/20 06:34:37 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; More Special Forms
;;; package: (runtime macros)

(declare (usual-integrations))

(define (initialize-package!)
  (for-each (lambda (keyword transform)
	      (syntax-table/define system-global-environment
				   keyword
				   transform))
	    '(AND
	      CASE
	      CONS-STREAM
	      DEFINE-INTEGRABLE
	      DO
	      LET*
	      LETREC
	      MAKE-ENVIRONMENT
	      QUASIQUOTE
	      SEQUENCE)
	    (list transform/and
		  transform/case
		  transform/cons-stream
		  transform/define-integrable
		  transform/do
		  transform/let*
		  transform/letrec
		  transform/make-environment
		  transform/quasiquote
		  transform/sequence)))

(define (make-absolute-reference name)
  `(ACCESS ,name #F))

(define (transform/and . expressions)
  (if (null? expressions)
      '#T
      (let loop ((expressions expressions))
	(if (null? (cdr expressions))
	    (car expressions)
	    `(IF ,(car expressions)
		 ,(loop (cdr expressions))
		 #F)))))

(define (transform/cons-stream head tail)
  `(,(make-absolute-reference 'CONS) ,head (DELAY ,tail)))

(define (transform/make-environment . body)
  `((NAMED-LAMBDA (,lambda-tag:make-environment)
      ,@body
      (THE-ENVIRONMENT))))

(define (transform/sequence . actions)
  `(BEGIN . ,actions))

;;;; Quasiquote

(define (transform/quasiquote expression)
  (descend-quasiquote expression 0 finalize-quasiquote))

(define (descend-quasiquote x level return)
  (cond ((pair? x) (descend-quasiquote-pair x level return))
	((vector? x) (descend-quasiquote-vector x level return))
	(else (return 'QUOTE x))))

(define (descend-quasiquote-pair x level return)
  (define (descend-quasiquote-pair* level)
    (descend-quasiquote (car x) level
      (lambda (car-mode car-arg)
	(descend-quasiquote (cdr x) level
	  (lambda (cdr-mode cdr-arg)
	    (cond ((and (eq? car-mode 'QUOTE)
			(eq? cdr-mode 'QUOTE))
		   (return 'QUOTE x))
		  ((eq? car-mode 'UNQUOTE-SPLICING)
		   (if (and (eq? cdr-mode 'QUOTE)
			    (null? cdr-arg))
		       (return 'UNQUOTE car-arg)
		       (return (make-absolute-reference 'APPEND)
			       (list car-arg
				     (finalize-quasiquote cdr-mode cdr-arg)))))
		  ((and (eq? cdr-mode 'QUOTE)
			(null? cdr-arg))
		   (return 'LIST
			   (list (finalize-quasiquote car-mode car-arg))))
		  ((and (eq? cdr-mode 'QUOTE)
			(list? cdr-arg))
		   (return 'LIST
			   (cons (finalize-quasiquote car-mode car-arg)
				 (map (lambda (el)
					(finalize-quasiquote 'QUOTE el))
				      cdr-arg))))
		  ((memq cdr-mode '(LIST CONS))
		   (return cdr-mode
			   (cons (finalize-quasiquote car-mode car-arg)
				 cdr-arg)))
		  (else
		   (return
		    'CONS
		    (list (finalize-quasiquote car-mode car-arg)
			  (finalize-quasiquote cdr-mode cdr-arg))))))))))
  (cond ((and (eq? (car x) 'QUASIQUOTE)
	      (pair? (cdr x))
	      (null? (cddr x)))
	 (descend-quasiquote-pair* (1+ level)))
	((and (or (eq? (car x) 'UNQUOTE)
		  (eq? (car x) 'UNQUOTE-SPLICING))
	      (pair? (cdr x))
	      (null? (cddr x)))
	 (if (zero? level)
	     (return (car x) (cadr x))
	     (descend-quasiquote-pair* (- level 1))))
	(else
	 (descend-quasiquote-pair* level))))

(define (descend-quasiquote-vector x level return)
  (descend-quasiquote (vector->list x) level
    (lambda (mode arg)
      (case mode
	((QUOTE)
	 (return 'QUOTE x))
	((LIST)
	 (return (make-absolute-reference 'VECTOR) arg))
	(else
	 (return (make-absolute-reference 'LIST->VECTOR)
		 (list (finalize-quasiquote mode arg))))))))

(define (finalize-quasiquote mode arg)
  (case mode
    ((QUOTE) `',arg)
    ((UNQUOTE) arg)
    ((UNQUOTE-SPLICING) (error ",@ in illegal context" arg))
    ((LIST) `(,(make-absolute-reference 'LIST) ,@arg))
    ((CONS)
     (if (= (length arg) 2)
	 `(,(make-absolute-reference 'CONS) ,@arg)
	 `(,(make-absolute-reference 'CONS*) ,@arg)))
    (else `(,mode ,@arg))))

(define (transform/case expr . clauses)
  (let ((need-temp? (not (symbol? expr))))
    (let ((the-expression (if need-temp? (generate-uninterned-symbol) expr)))
      (define (process-clauses clauses)
	(if (null? clauses)
	    '()
	    (let ((selector (caar clauses))
		  (rest (process-clauses (cdr clauses))))
	      (if (null? selector)
		  rest
		  `((,(cond ((eq? selector 'ELSE)
			     (if (not (null? (cdr clauses)))
				 (error "CASE SYNTAX: ELSE not last clause"
					clauses))
			     'ELSE)
			    ((pair? selector)
			     (transform selector))
			    (else
			     (single-clause selector)))
		     ,@(cdar clauses))
		    ,@rest)))))

      (define (check-selector selector)
	(or (null? selector)
	    (and (eq-testable? (car selector))
		 (check-selector (cdr selector)))))

      (define (eq-testable? selector)
	(or (symbol? selector)
	    (char? selector)		;**** implementation dependent.
	    (fix:fixnum? selector)	;**** implementation dependent.
	    (eq? selector false)
	    (eq? selector true)))

      (define (single-clause selector)
	`(,(if (eq-testable? selector) 'EQ? 'EQV?) ,the-expression ',selector))

      (define (transform selector)
	;; Optimized for speed in compiled code.
	(cond ((null? (cdr selector))
	       (single-clause (car selector)))
	      ((null? (cddr selector))
	       `(OR ,(single-clause (car selector))
		    ,(single-clause (cadr selector))))
	      ((null? (cdddr selector))
	       `(OR ,(single-clause (car selector))
		    ,(single-clause (cadr selector))
		    ,(single-clause (caddr selector))))
	      ((null? (cddddr selector))
	       `(OR ,(single-clause (car selector))
		    ,(single-clause (cadr selector))
		    ,(single-clause (caddr selector))
		    ,(single-clause (cadddr selector))))
	      (else
	       `(,(if (check-selector selector) 'MEMQ 'MEMV)
		 ,the-expression ',selector))))

      (let ((body `(COND ,@(process-clauses clauses))))
	(if need-temp?
	    `(let ((,the-expression ,expr))
	       ,body)
	    body)))))

(define (transform/let* bindings . body)
  (guarantee-let-bindings bindings 'LET* #f)
  (define (do-one bindings)
    (if (null? bindings)
	`(BEGIN ,@body)
	`(LET (,(car bindings))
	   ,(do-one (cdr bindings)))))
  (if (null? bindings)
      `(LET () ,@body)			; To allow internal definitions
      (do-one bindings)))

(define (transform/letrec bindings . body)
  (guarantee-let-bindings bindings 'LETREC #f)
  `(LET ()
     ,@(map (lambda (binding) `(DEFINE ,@binding)) bindings)
     (LET ()				; Internal definitions must be in
					; nested contour.
       ,@body)))

(define (transform/do bindings test . body)
  (guarantee-let-bindings bindings 'DO #t)
  (let ((the-name (string->uninterned-symbol "do-loop")))
    `(LET ,the-name
	  ,(map (lambda (binding)
		  (if (or (null? (cdr binding))
			  (null? (cddr binding)))
		      binding
		      `(,(car binding) ,(cadr binding))))
		bindings)
       ,(process-cond-clause test false
	  `(BEGIN
	     ,@body
	     (,the-name ,@(map (lambda (binding)
				 (if (or (null? (cdr binding))
					 (null? (cddr binding)))
				     (car binding)
				     (caddr binding)))
			       bindings)))))))

(define (guarantee-let-bindings bindings keyword do-like?)
  (if (not (and (list? bindings)
		(for-all? bindings
		  (lambda (binding)
		    (and (list? binding)
			 (not (null? binding))
			 (symbol? (car binding))
			 (or (null? (cdr binding))
			     (null? (cddr binding))
			     (and do-like? (null? (cdddr binding)))))))))
      (error "SYNTAX: Bad bindings:" keyword bindings)))

(define (process-cond-clause clause else-permitted? rest)
  (if (or (null? clause) (not (list? clause)))
      (error "cond-clause syntax: not a non-empty list:" clause))
  (cond ((eq? 'ELSE (car clause))
	 (if (not else-permitted?)
	     (error "cond-clause syntax: ELSE not permitted:" clause))
	 (if (null? (cdr clause))
	     (error "cond-clause syntax: ELSE missing expressions:" clause))
	 `(BEGIN ,@(cdr clause)))
	((null? (cdr clause))
	 `(OR ,(car clause) ,rest))
	((eq? '=> (cadr clause))
	 (if (null? (cddr clause))
	     (error "cond-clause syntax: => missing recipient:" clause))
	 (if (not (null? (cdddr clause)))
	     (error "cond-clause syntax: misformed => clause:" clause))
	 (let ((predicate (string->uninterned-symbol "predicate")))
	   `(LET ((,predicate ,(car clause)))
	      (IF ,predicate
		  (,(caddr clause) ,predicate)
		  ,rest))))
	(else
	 (if (null? (cdr clause))
	     (error "cond-clause syntax: missing expressions:" clause))
	 `(IF ,(car clause)
	      (BEGIN ,@(cdr clause))
	      ,rest))))

(define transform/define-integrable
  (lambda (pattern . body)
    (parse-define-syntax pattern body
      (lambda (name body)
	`(BEGIN (DECLARE (INTEGRATE ,pattern))
		(DEFINE ,name ,@body)))
      (lambda (pattern body)
	`(BEGIN (DECLARE (INTEGRATE-OPERATOR ,(car pattern)))
		(DEFINE ,pattern
		  ,@(if (list? (cdr pattern))
			`((DECLARE
			   (INTEGRATE
			    ,@(lambda-list->bound-names (cdr pattern)))))
			'())
		  ,@body))))))

(define (parse-define-syntax pattern body if-variable if-lambda)
  (cond ((pair? pattern)
	 (let loop ((pattern pattern) (body body))
	   (cond ((pair? (car pattern))
		  (loop (car pattern) `((LAMBDA ,(cdr pattern) ,@body))))
		 ((symbol? (car pattern))
		  (if-lambda pattern body))
		 (else
		  (error "Illegal name" (car pattern))))))
	((symbol? pattern)
	 (if-variable pattern body))
	(else
	 (error "Illegal name" pattern))))

(define (lambda-list->bound-names lambda-list)
  (cond ((null? lambda-list)
	 '())
	((pair? lambda-list)
	 (let ((lambda-list
		(if (eq? (car lambda-list) lambda-optional-tag)
		    (begin (if (not (pair? (cdr lambda-list)))
			       (error "Missing optional variable" lambda-list))
			   (cdr lambda-list))
		    lambda-list)))
	   (cons (let ((parameter (car lambda-list)))
		   (if (pair? parameter) (car parameter) parameter))
		 (lambda-list->bound-names (cdr lambda-list)))))
	(else
	 (if (not (symbol? lambda-list))
	     (error "Illegal rest variable" lambda-list))
	 (list lambda-list))))