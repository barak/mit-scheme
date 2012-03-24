#| -*- Mode: Scheme; keyword-style: none -*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; MIT/GNU Scheme macros

(declare (usual-integrations))

;;;; SRFI features

(define-syntax :cond-expand
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((if-error (lambda () (ill-formed-syntax form))))
       (if (syntax-match? '(+ (DATUM * FORM)) (cdr form))
	   (let loop ((clauses (cdr form)))
	     (let ((req (caar clauses))
		   (if-true (lambda () `(,(rename 'BEGIN) ,@(cdar clauses)))))
	       (if (and (identifier? req)
			(compare (rename 'ELSE) req))
		   (if (null? (cdr clauses))
		       (if-true)
		       (if-error))
		   (let req-loop
		       ((req req)
			(if-true if-true)
			(if-false
			 (lambda ()
			   (if (null? (cdr clauses))
			       (if-error)
			       (loop (cdr clauses))))))
		     (cond ((identifier? req)
			    (if (any (lambda (feature)
				       (compare (rename feature) req))
				     supported-srfi-features)
				(if-true)
				(if-false)))
			   ((and (syntax-match? '(IDENTIFIER DATUM) req)
				 (compare (rename 'NOT) (car req)))
			    (req-loop (cadr req)
				      if-false
				      if-true))
			   ((and (syntax-match? '(IDENTIFIER * DATUM) req)
				 (compare (rename 'AND) (car req)))
			    (let and-loop ((reqs (cdr req)))
			      (if (pair? reqs)
				  (req-loop (car reqs)
					    (lambda () (and-loop (cdr reqs)))
					    if-false)
				  (if-true))))
			   ((and (syntax-match? '(IDENTIFIER * DATUM) req)
				 (compare (rename 'OR) (car req)))
			    (let or-loop ((reqs (cdr req)))
			      (if (pair? reqs)
				  (req-loop (car reqs)
					    if-true
					    (lambda () (or-loop (cdr reqs))))
				  (if-false))))
			   (else
			    (if-error)))))))
	   (if-error))))))

(define supported-srfi-features
  '(MIT
    MIT/GNU
    SWANK		       ;Provides SWANK module for SLIME
    SRFI-0		       ;COND-EXPAND
    SRFI-1		       ;List Library
    SRFI-2		       ;AND-LET*
    SRFI-6		       ;Basic String Ports
    SRFI-8		       ;RECEIVE
    SRFI-9		       ;DEFINE-RECORD-TYPE
    SRFI-23		       ;ERROR
    SRFI-27		       ;Sources of Random Bits
    SRFI-30		       ;Nested Multi-Line Comments (#| ... |#)
    SRFI-62		       ;S-expression comments
    SRFI-69		       ;Basic Hash Tables
    ))

(define-syntax :receive
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (if (syntax-match? '(R4RS-BVL FORM + FORM) (cdr form))
	 (let ((r-lambda (rename 'LAMBDA)))
	   `(,(rename 'CALL-WITH-VALUES)
	     (,r-lambda () ,(caddr form))
	     (,r-lambda ,(cadr form) ,@(cdddr form))))
	 (ill-formed-syntax form)))))

(define-syntax :define-record-type
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (if (syntax-match? '(IDENTIFIER
			  (IDENTIFIER * IDENTIFIER)
			  IDENTIFIER
			  * (IDENTIFIER IDENTIFIER ? IDENTIFIER))
			(cdr form))
	 (let ((type (cadr form))
	       (constructor (car (caddr form)))
	       (c-tags (cdr (caddr form)))
	       (predicate (cadddr form))
	       (fields (cddddr form))
	       (de (rename 'DEFINE)))
	   `(,(rename 'BEGIN)
	     (,de ,type (,(rename 'MAKE-RECORD-TYPE) ',type ',(map car fields)))
	     (,de ,constructor (,(rename 'RECORD-CONSTRUCTOR) ,type ',c-tags))
	     (,de ,predicate (,(rename 'RECORD-PREDICATE) ,type))
	     ,@(append-map
		(lambda (field)
		  (let ((name (car field)))
		    (cons `(,de ,(cadr field)
				(,(rename 'RECORD-ACCESSOR) ,type ',name))
			  (if (pair? (cddr field))
			      `((,de ,(caddr field)
				     (,(rename 'RECORD-MODIFIER) ,type ',name)))
			      '()))))
		fields)))
	 (ill-formed-syntax form)))))

(define-syntax :define
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (receive (name value) (parse-define-form form rename)
       `(,keyword:define ,name ,value)))))

(define (parse-define-form form rename)
  (cond ((syntax-match? '((DATUM . MIT-BVL) + FORM) (cdr form))
	 (parse-define-form
	  `(,(car form) ,(caadr form)
			,(if (identifier? (caadr form))
			     `(,(rename 'NAMED-LAMBDA) ,@(cdr form))
			     `(,(rename 'LAMBDA) ,(cdadr form) ,@(cddr form))))
	  rename))
	((syntax-match? '(IDENTIFIER ? EXPRESSION) (cdr form))
	 (values (cadr form)
		 (if (pair? (cddr form))
		     (caddr form)
		     (unassigned-expression))))
	(else
	 (ill-formed-syntax form))))

(define named-let-strategy 'internal-definition)

(define-syntax :let
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (cond ((syntax-match? '(IDENTIFIER (* (IDENTIFIER ? EXPRESSION)) + FORM)
			   (cdr form))
	    (let ((name (cadr form))
		  (bindings (caddr form))
		  (body (cdddr form)))
	      (let ((vars (map car bindings))
		    (vals (map (lambda (binding)
				 (if (pair? (cdr binding))
				     (cadr binding)
				     (unassigned-expression)))
			       bindings)))
		(case named-let-strategy
		  ((fixed-point)
		   (let ((iter (make-synthetic-identifier 'ITER))
			 (kernel (make-synthetic-identifier 'KERNEL))
			 (temps (map (lambda (b)
				       (declare (ignore b))
				       (make-synthetic-identifier 'TEMP)) bindings))
			 (r-lambda (rename 'LAMBDA))
			 (r-declare (rename 'DECLARE)))
		     `((,r-lambda (,kernel)
			  (,kernel ,kernel ,@vals))
		       (,r-lambda (,iter ,@vars)
			  ((,r-lambda (,name)
			      (,r-declare (INTEGRATE-OPERATOR ,name))
			      ,@body)
			   (,r-lambda ,temps
			      (,r-declare (INTEGRATE ,@temps))
			      (,iter ,iter ,@temps)))))))
		  ((internal-definition)
		   `((,(rename 'LET) ()
		      (,(rename 'DEFINE) (,name ,@vars) ,@body)
		      ,name)
		     ,@vals))
		  ((letrec)
		   `((,(rename 'LETREC)
		      ((,name (,(rename 'NAMED-LAMBDA) (,name ,@vars)
			       ,@body)))
		      ,name)
		     ,@vals))
		  ((letrec*)
		   `((,(rename 'LETREC*)
		      ((,name (,(rename 'NAMED-LAMBDA) (,name ,@vars)
			       ,@body)))
		      ,name)
		     ,@vals))
		  (else (error "Unrecognized named-let-strategy: " named-let-strategy))))))
	   ((syntax-match? '((* (IDENTIFIER ? EXPRESSION)) + FORM) (cdr form))
	    `(,keyword:let ,@(cdr (normalize-let-bindings form))))
	   (else
	    (ill-formed-syntax form))))))

(define (normalize-let-bindings form)
  `(,(car form) ,(map (lambda (binding)
			(if (pair? (cdr binding))
			    binding
			    (list (car binding) (unassigned-expression))))
		      (cadr form))
		,@(cddr form)))

(define-syntax :let*
  (er-macro-transformer
   (lambda (form rename compare)
     compare			;ignore
     (expand/let* form (rename 'LET)))))

(define-syntax :let*-syntax
  (er-macro-transformer
   (lambda (form rename compare)
     compare			;ignore
     (expand/let* form (rename 'LET-SYNTAX)))))

(define (expand/let* form let-keyword)
  (syntax-check '(KEYWORD (* DATUM) + FORM) form)
  (let ((bindings (cadr form))
	(body (cddr form)))
    (if (pair? bindings)
	(let loop ((bindings bindings))
	  (if (pair? (cdr bindings))
	      `(,let-keyword (,(car bindings)) ,(loop (cdr bindings)))
	      `(,let-keyword ,bindings ,@body)))
	`(,let-keyword ,bindings ,@body))))

(define-syntax :and
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (syntax-check '(KEYWORD * EXPRESSION) form)
     (let ((operands (cdr form)))
       (if (pair? operands)
	   (let ((if-keyword (rename 'IF)))
	     (let loop ((operands operands))
	       (if (pair? (cdr operands))
		   `(,if-keyword ,(car operands)
				 ,(loop (cdr operands))
				 #F)
		   (car operands))))
	   `#T)))))

(define-syntax :case
  (er-macro-transformer
   (lambda (form rename compare)
     (syntax-check '(KEYWORD EXPRESSION + (DATUM * EXPRESSION)) form)
     (letrec
	 ((process-clause
	   (lambda (clause rest)
	     (cond ((null? (car clause))
		    (process-rest rest))
		   ((and (identifier? (car clause))
			 (compare (rename 'ELSE) (car clause))
			 (null? rest))
		    `(,(rename 'BEGIN) ,@(cdr clause)))
		   ((list? (car clause))
		    `(,(rename 'IF) ,(process-predicate (car clause))
				    (,(rename 'BEGIN) ,@(cdr clause))
				    ,(process-rest rest)))
		   (else
		    (syntax-error "Ill-formed clause:" clause)))))
	  (process-rest
	   (lambda (rest)
	     (if (pair? rest)
		 (process-clause (car rest) (cdr rest))
		 (unspecific-expression))))
	  (process-predicate
	   (lambda (items)
	     ;; Optimize predicate for speed in compiled code.
	     (cond ((null? (cdr items))
		    (single-test (car items)))
		   ((null? (cddr items))
		    `(,(rename 'OR) ,(single-test (car items))
				    ,(single-test (cadr items))))
		   ((null? (cdddr items))
		    `(,(rename 'OR) ,(single-test (car items))
				    ,(single-test (cadr items))
				    ,(single-test (caddr items))))
		   ((null? (cddddr items))
		    `(,(rename 'OR) ,(single-test (car items))
				    ,(single-test (cadr items))
				    ,(single-test (caddr items))
				    ,(single-test (cadddr items))))
		   (else
		    `(,(rename
			(if (for-all? items eq-testable?) 'MEMQ 'MEMV))
		      ,(rename 'TEMP)
		      ',items)))))
	  (single-test
	   (lambda (item)
	     `(,(rename (if (eq-testable? item) 'EQ? 'EQV?))
	       ,(rename 'TEMP)
	       ',item)))
	  (eq-testable?
	   (lambda (item)
	     (or (symbol? item)
		 (boolean? item)
		 ;; remainder are implementation dependent:
		 (char? item)
		 (fix:fixnum? item)))))
       `(,(rename 'LET) ((,(rename 'TEMP) ,(cadr form)))
			,(process-clause (caddr form)
					 (cdddr form)))))))

(define-syntax :cond
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((clauses (cdr form)))
       (if (not (pair? clauses))
	   (syntax-error "Form must have at least one clause:" form))
       (let loop ((clause (car clauses)) (rest (cdr clauses)))
	 (expand/cond-clause clause rename compare (null? rest)
			     (if (pair? rest)
				 (loop (car rest) (cdr rest))
				 (unspecific-expression))))))))

(define-syntax :do
  (er-macro-transformer
   (lambda (form rename compare)
     (syntax-check '(KEYWORD (* (IDENTIFIER EXPRESSION ? EXPRESSION))
			     (+ FORM)
			     * FORM)
		   form)
     (let ((bindings (cadr form))
	   (r-loop (rename 'DO-LOOP)))
       `(,(rename 'LET)
	 ,r-loop
	 ,(map (lambda (binding)
		 (list (car binding) (cadr binding)))
	       bindings)
	 ,(expand/cond-clause (caddr form) rename compare #f
			      `(,(rename 'BEGIN)
				,@(cdddr form)
				(,r-loop ,@(map (lambda (binding)
						  (if (pair? (cddr binding))
						      (caddr binding)
						      (car binding)))
						bindings)))))))))

(define (expand/cond-clause clause rename compare else-allowed? alternative)
  (if (not (and (pair? clause) (list? (cdr clause))))
      (syntax-error "Ill-formed clause:" clause))
  (cond ((and (identifier? (car clause))
	      (compare (rename 'ELSE) (car clause)))
	 (if (not else-allowed?)
	     (syntax-error "Misplaced ELSE clause:" clause))
	 (if (or (not (pair? (cdr clause)))
		 (and (identifier? (cadr clause))
		      (compare (rename '=>) (cadr clause))))
	     (syntax-error "Ill-formed ELSE clause:" clause))
	 `(,(rename 'BEGIN) ,@(cdr clause)))
	((not (pair? (cdr clause)))
	 `(,(rename 'OR) ,(car clause) ,alternative))
	((and (identifier? (cadr clause))
	      (compare (rename '=>) (cadr clause)))
	 (if (not (and (pair? (cddr clause))
		       (null? (cdddr clause))))
	     (syntax-error "Ill-formed => clause:" clause))
	 (let ((r-temp (rename 'TEMP)))
	   `(,(rename 'LET) ((,r-temp ,(car clause)))
			    (,(rename 'IF) ,r-temp
					   (,(caddr clause) ,r-temp)
					   ,alternative))))
	(else
	 `(,(rename 'IF) ,(car clause)
			 (,(rename 'BEGIN) ,@(cdr clause))
			 ,alternative))))

(define-syntax :quasiquote
  (er-macro-transformer
   (lambda (form rename compare)

     (define (descend-quasiquote x level return)
       (cond ((pair? x) (descend-quasiquote-pair x level return))
	     ((vector? x) (descend-quasiquote-vector x level return))
	     (else (return 'QUOTE x))))

     (define (descend-quasiquote-pair x level return)
       (cond ((not (and (pair? x)
			(identifier? (car x))
			(pair? (cdr x))
			(null? (cddr x))))
	      (descend-quasiquote-pair* x level return))
	     ((compare (rename 'QUASIQUOTE) (car x))
	      (descend-quasiquote-pair* x (+ level 1) return))
	     ((compare (rename 'UNQUOTE) (car x))
	      (if (zero? level)
		  (return 'UNQUOTE (cadr x))
		  (descend-quasiquote-pair* x (- level 1) return)))
	     ((compare (rename 'UNQUOTE-SPLICING) (car x))
	      (if (zero? level)
		  (return 'UNQUOTE-SPLICING (cadr x))
		  (descend-quasiquote-pair* x (- level 1) return)))
	     (else
	      (descend-quasiquote-pair* x level return))))

     (define (descend-quasiquote-pair* x level return)
       (descend-quasiquote (car x) level
	 (lambda (car-mode car-arg)
	   (descend-quasiquote (cdr x) level
	     (lambda (cdr-mode cdr-arg)
	       (cond ((and (eq? car-mode 'QUOTE) (eq? cdr-mode 'QUOTE))
		      (return 'QUOTE x))
		     ((eq? car-mode 'UNQUOTE-SPLICING)
		      (if (and (eq? cdr-mode 'QUOTE) (null? cdr-arg))
			  (return 'UNQUOTE car-arg)
			  (return 'APPEND
				  (list car-arg
					(finalize-quasiquote cdr-mode
							     cdr-arg)))))
		     ((and (eq? cdr-mode 'QUOTE) (list? cdr-arg))
		      (return 'LIST
			      (cons (finalize-quasiquote car-mode car-arg)
				    (map (lambda (element)
					   (finalize-quasiquote 'QUOTE
								element))
					 cdr-arg))))
		     ((eq? cdr-mode 'LIST)
		      (return 'LIST
			      (cons (finalize-quasiquote car-mode car-arg)
				    cdr-arg)))
		     (else
		      (return
		       'CONS
		       (list (finalize-quasiquote car-mode car-arg)
			     (finalize-quasiquote cdr-mode cdr-arg))))))))))

     (define (descend-quasiquote-vector x level return)
       (descend-quasiquote (vector->list x) level
	 (lambda (mode arg)
	   (case mode
	     ((QUOTE) (return 'QUOTE x))
	     ((LIST) (return 'VECTOR arg))
	     (else
	      (return 'LIST->VECTOR
		      (list (finalize-quasiquote mode arg))))))))

     (define (finalize-quasiquote mode arg)
       (case mode
	 ((QUOTE) `(,(rename 'QUOTE) ,arg))
	 ((UNQUOTE) arg)
	 ((UNQUOTE-SPLICING) (syntax-error ",@ in illegal context:" arg))
	 (else `(,(rename mode) ,@arg))))

     (syntax-check '(KEYWORD EXPRESSION) form)
     (descend-quasiquote (cadr form) 0 finalize-quasiquote))))

;;;; SRFI 2: AND-LET*

;;; The SRFI document is a little unclear about the semantics, imposes
;;; the weird restriction that variables may be duplicated (citing
;;; LET*'s similar restriction, which doesn't actually exist), and the
;;; reference implementation is highly non-standard and hard to
;;; follow.  This passes all of the tests except for the one that
;;; detects duplicate bound variables, though.

(define-syntax :and-let*
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (let ((%and (rename 'AND))
	   (%let (rename 'LET))
	   (%begin (rename 'BEGIN)))
       (cond ((syntax-match? '(() * FORM) (cdr form))
	      `(,%begin #T ,@(cddr form)))
	     ((syntax-match? '((* DATUM) * FORM) (cdr form))
	      (let ((clauses (cadr form))
		    (body (cddr form)))
		(define (expand clause recur)
		  (cond ((syntax-match? 'IDENTIFIER clause)
			 (recur clause))
			((syntax-match? '(EXPRESSION) clause)
			 (recur (car clause)))
			((syntax-match? '(IDENTIFIER EXPRESSION) clause)
			 (let ((tail (recur (car clause))))
			   (and tail `(,%let (,clause) ,tail))))
			(else #f)))
		(define (recur clauses make-body)
		  (expand (car clauses)
			  (let ((clauses (cdr clauses)))
			    (if (null? clauses)
				make-body
				(lambda (conjunct)
				  `(,%and ,conjunct
					  ,(recur clauses make-body)))))))
		(or (recur clauses
			   (if (null? body)
			       (lambda (conjunct) conjunct)
			       (lambda (conjunct)
				 `(,%and ,conjunct (,%begin ,@body)))))
		    (ill-formed-syntax form))))
	     (else
	      (ill-formed-syntax form)))))))

(define-syntax :access
  (er-macro-transformer
   (lambda (form rename compare)
     rename compare			;ignore
     (cond ((syntax-match? '(IDENTIFIER EXPRESSION) (cdr form))
	    `(,keyword:access ,@(cdr form)))
	   ((syntax-match? '(IDENTIFIER IDENTIFIER + FORM) (cdr form))
	    `(,keyword:access ,(cadr form) (,(car form) ,@(cddr form))))
	   (else
	    (ill-formed-syntax form))))))

(define-syntax :cons-stream
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (syntax-check '(KEYWORD EXPRESSION EXPRESSION) form)
     `(,(rename 'CONS) ,(cadr form)
		       (,(rename 'DELAY) ,(caddr form))))))

(define-syntax :define-integrable
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (let ((r-begin (rename 'BEGIN))
	   (r-declare (rename 'DECLARE))
	   (r-define (rename 'DEFINE)))
       (cond ((syntax-match? '(IDENTIFIER EXPRESSION) (cdr form))
	      `(,r-begin
		(,r-declare (INTEGRATE ,(cadr form)))
		(,r-define ,@(cdr form))))
	     ((syntax-match? '((IDENTIFIER * IDENTIFIER) + FORM) (cdr form))
	      `(,r-begin
		(,r-declare (INTEGRATE-OPERATOR ,(caadr form)))
		(,r-define ,(cadr form)
			   (,r-declare (INTEGRATE ,@(cdadr form)))
			   ,@(cddr form))))
	     (else
	      (ill-formed-syntax form)))))))

(define-syntax :fluid-let
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (syntax-check '(KEYWORD (* (FORM ? EXPRESSION)) + FORM) form)
     (let ((left-hand-sides (map car (cadr form)))
	   (right-hand-sides (map cdr (cadr form)))
	   (r-define (rename 'DEFINE))
	   (r-lambda (rename 'LAMBDA))
	   (r-let (rename 'LET))
	   (r-set! (rename 'SET!))
	   (r-shallow-fluid-bind (rename 'SHALLOW-FLUID-BIND))
	   (r-unspecific (rename 'UNSPECIFIC)))
       (let ((temporaries
	      (map (lambda (lhs)
		     (make-synthetic-identifier
		      (if (identifier? lhs) lhs 'TEMPORARY)))
		   left-hand-sides))
	     (swap! (make-synthetic-identifier 'SWAP!))
	     (body `(,r-lambda () ,@(cddr form))))
	 `(,r-let ,(map cons temporaries right-hand-sides)
	    (,r-define (,swap!)
	      ,@(map (lambda (lhs temporary)
		       `(,r-set! ,lhs (,r-set! ,temporary (,r-set! ,lhs))))
		     left-hand-sides
		     temporaries)
	      ,r-unspecific)
	    (,r-shallow-fluid-bind ,swap! ,body ,swap!)))))))

(define (unspecific-expression)
  `(,keyword:unspecific))

(define (unassigned-expression)
  `(,keyword:unassigned))

(define-syntax :begin0
  (syntax-rules ()
    ((BEGIN0 form0 form1+ ...)
     (LET ((RESULT form0))
       form1+ ...
       RESULT))))

(define-syntax :assert
  (syntax-rules ()
    ((ASSERT condition)
     (IF (NOT condition)
         (ERROR "Assertion failed:" 'condition)))))
