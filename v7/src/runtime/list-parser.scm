#| -*-Scheme-*-

$Id: list-parser.scm,v 1.3 2008/09/03 06:08:16 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Parsing language for flat lists
;;; package: (runtime list-parser)

(declare (usual-integrations))

(define-syntax list-parser
  (sc-macro-transformer
   (lambda (form env)
     (if (syntax-match? '(FORM) (cdr form))
	 (compile-top-level (cadr form) env)
	 (ill-formed-syntax form)))))

(define (compile-top-level pattern env)
  (fluid-let ((name-counters (make-strong-eq-hash-table)))
    (make-lambda '(ITEMS WIN LOSE)
      (lambda (items win lose)
	(optimize-result (compile-pattern pattern env items win lose))))))

(define (compile-pattern pattern env items win lose)
  (cond ((rewrite-pattern pattern)
	 => (lambda (pattern)
	      (compile-pattern pattern env items win lose)))
	((and (pair? pattern)
	      (interned-symbol? (car pattern))
	      (let ((c (get-pattern-compiler (car pattern))))
		(and (syntax-match? (car c) (cdr pattern))
		     (cdr c))))
	 => (lambda (compiler)
	      (compiler pattern env items win lose)))
	(else
	 (error "Unrecognized list pattern:" pattern))))

(define (rewrite-pattern pattern)
  (cond ((identifier? pattern)
	 `(SEXP ,pattern))
	((or (char? pattern)
	     (string? pattern)
	     (number? pattern)
	     (boolean? pattern)
	     (null? pattern))
	 `(QUOTE ,pattern))
	((syntax-match? '('+ * FORM) pattern)
	 `(SEQ ,@(cdr pattern) (* ,@(cdr pattern))))
	(else #f)))

(define (get-pattern-compiler name)
  (let ((p (assq name pattern-compilers)))
    (and p
	 (cdr p))))

(define (define-pattern-compiler template compiler)
  (let ((name (car template))
	(value (cons (cdr template) compiler)))
    (let ((p (assq name pattern-compilers)))
      (if p
	  (set-cdr! p value)
	  (begin
	    (set! pattern-compilers
		  (cons (cons name value)
			pattern-compilers))
	    unspecific)))))

(define pattern-compilers '())

(define (terminal items lose make-test make-body)
  (make-let '(ITEMS LOSE)
	    (list items lose)
    (lambda (items lose)
      `(IF ,(make-test items)
	   ,(make-body items lose)
	   (,lose)))))

(define (wrap-list-parser parser)
  (lambda (items)
    (parser items
	    (lambda (items vals lose)
	      (if (null? items)
		  (list-parser-vals->list vals)
		  (lose)))
	    (lambda ()
	      #f))))

(define-pattern-compiler '(MATCH-NULL)
  (lambda (pattern env items win lose)
    pattern env
    (terminal items lose
	      (lambda (items)
		`(NULL? ,items))
	      (lambda (items lose)
		`(,win ,items ,(null-vals) ,lose)))))

(define-pattern-compiler '(MATCH-ANY)
  (lambda (pattern env items win lose)
    pattern env
    (terminal items lose
	      (lambda (items)
		`(PAIR? ,items))
	      (lambda (items lose)
		`(,win (CDR ,items) ,(single-val `(CAR ,items)) ,lose)))))

(define-pattern-compiler '(MATCH-IF EXPRESSION)
  (lambda (pattern env items win lose)
    (terminal items lose
	      (lambda (items)
		`(AND (PAIR? ,items)
		      (,(close-syntax (cadr pattern) env) (CAR ,items))))
	      (lambda (items lose)
		`(,win (CDR ,items) ,(single-val `(CAR ,items)) ,lose)))))

(define-pattern-compiler '(NOISE-IF EXPRESSION)
  (lambda (pattern env items win lose)
    (terminal items lose
	      (lambda (items)
		`(AND (PAIR? ,items)
		      (,(close-syntax (cadr pattern) env) (CAR ,items))))
	      (lambda (items lose)
		`(,win (CDR ,items) ,(null-vals) ,lose)))))

(define-pattern-compiler '(QUOTE DATUM)
  (lambda (pattern env items win lose)
    env
    (terminal items lose
	      (let ((datum (cadr pattern)))
		(lambda (items)
		  `(AND (PAIR? ,items)
			(,(cond ((or (symbol? datum)
				     (char? datum)
				     (boolean? datum)
				     (null? datum))
				 'EQ?)
				((number? datum) 'EQV?)
				(else 'EQUAL?))
			 (CAR ,items)
			 ',datum))))
	      (lambda (items lose)
		`(,win (CDR ,items) ,(null-vals) ,lose)))))

(define-pattern-compiler '(VALUES * EXPRESSION)
  (lambda (pattern env items win lose)
    `(,win ,items
	   ,(let ((vals
		   (map (lambda (expr)
			  (single-val (close-syntax expr env)))
			(cdr pattern))))
	      (if (pair? vals)
		  (let loop ((vals vals))
		    (if (pair? (cdr vals))
			(join-vals (car vals) (loop (cdr vals)))
			(car vals)))
		  (null-vals)))
	   ,lose)))

(define-pattern-compiler '(LIST * FORM)
  (lambda (pattern env items win lose)
    (terminal items lose
	      (lambda (items)
		`(PAIR? ,items))
	      (lambda (items lose)
		(compile-pattern `(SEQ ,@(cdr pattern))
				 env
				 `(CAR ,items)
				 (make-winner
				  (lambda (items* vals lose)
				    (fork-loser lose
				      (lambda (lose)
					`(IF (NULL? ,items*)
					     (,win (CDR ,items) ,vals ,lose)
					     (,lose))))))
				 lose)))))

(define-pattern-compiler '(SEXP EXPRESSION)
  (lambda (pattern env items win lose)
    `(,(close-syntax (cadr pattern) env) ,items ,win ,lose)))

(define-pattern-compiler '(NOISE FORM)
  (lambda (pattern env items win lose)
    (compile-pattern (cadr pattern)
		     env
		     items
		     (make-winner
		      (lambda (items vals lose)
			vals
			`(,win ,items ,(null-vals) ,lose)))
		     lose)))

(define-pattern-compiler '(? * FORM)
  (lambda (pattern env items win lose)
    (compile-pattern `(SEQ ,@(cdr pattern))
		     env
		     items
		     win
		     (make-loser
		      `(,win ,items ,(null-vals) ,lose)))))

(define-pattern-compiler '(* * FORM)
  (lambda (pattern env items win lose)
    (make-loop '(ITEMS VALS LOSE)
	       (list items (null-vals) lose)
      (lambda (loop items* vals lose*)
	(compile-pattern `(SEQ ,@(cdr pattern))
			 env
			 items*
			 (make-winner
			  (lambda (items vals* lose)
			    `(,loop ,items
				    ,(join-vals vals vals*)
				    ,lose)))
			 (make-loser
			  `(,win ,items* ,vals ,lose*)))))))

(define-pattern-compiler '(SEQ * FORM)
  (lambda (pattern env items win lose)
    (let ((patterns (cdr pattern)))
      (if (pair? patterns)
	  (if (pair? (cdr patterns))
	      (let loop
		  ((patterns patterns)
		   (items items)
		   (vals (null-vals))
		   (lose lose))
		(if (pair? patterns)
		    (compile-pattern (car patterns)
				     env
				     items
				     (make-winner
				      (lambda (items vals* lose)
					(loop (cdr patterns)
					      items
					      (join-vals vals vals*)
					      lose)))
				     lose)
		    `(,win ,items ,vals ,lose)))
	      (compile-pattern (car patterns) env items win lose))
	  `(,win ,items ,(null-vals) ,lose)))))

(define-pattern-compiler '(ALT * FORM)
  (lambda (pattern env items win lose)
    (let ((patterns (cdr pattern)))
      (if (pair? patterns)
	  (fork-winner win
	    (lambda (win)
	      (let loop ((patterns patterns))
		(let ((k
		       (lambda (lose)
			 (compile-pattern (car patterns) env items win lose))))
		  (if (pair? (cdr patterns))
		      (fork-loser (make-loser (loop (cdr patterns)))
				  k)
		      (k lose))))))
	  `(,lose)))))

(define-pattern-compiler '(MAP EXPRESSION FORM)
  (lambda (pattern env items win lose)
    (compile-pattern (caddr pattern)
		     env
		     items
		     (make-winner
		      (lambda (items vals lose)
			`(,win ,items
			       (map ,(close-syntax (cadr pattern) env)
				    (LIST-PARSER-VALS->LIST ,vals))
			       ,lose)))
		     lose)))

(define-pattern-compiler '(ENCAPSULATE EXPRESSION FORM)
  (lambda (pattern env items win lose)
    (compile-pattern (caddr pattern)
		     env
		     items
		     (make-winner
		      (lambda (items vals lose)
			`(,win ,items
			       ,(single-val
				 `(APPLY ,(close-syntax (cadr pattern) env)
					 (LIST-PARSER-VALS->LIST ,vals)))
			       ,lose)))
		     lose)))

(define-pattern-compiler '(TRANSFORM EXPRESSION FORM)
  (lambda (pattern env items win lose)
    (compile-pattern (caddr pattern)
		     env
		     items
		     (make-winner
		      (lambda (items vals lose)
			`(,win ,items
			       (APPLY ,(close-syntax (cadr pattern) env)
				      (LIST-PARSER-VALS->LIST ,vals))
			       ,lose)))
		     lose)))

(define (make-winner procedure)
  (make-lambda '(ITEMS VALS LOSE) procedure))

(define (make-loser body)
  (make-lambda '() (lambda () body)))

(define (fork-winner win procedure)
  (make-let '(WIN) (list win) procedure))

(define (fork-loser lose procedure)
  (make-let '(LOSE) (list lose) procedure))

(define (make-lambda names make-body)
  (call-with-new-names names
    (lambda names
      `(LAMBDA ,names
	 ,(apply make-body names)))))

(define (make-let names args make-body)
  (call-with-new-names names
    (lambda names
      `((LAMBDA ,names
	  ,(apply make-body names))
	,@args))))

(define (make-loop names inits make-body)
  (call-with-new-names (cons 'LOOP names)
    (lambda names
      `(LET ,(car names)
	 ,(map (lambda (name init)
		 `(,name ,init))
	       (cdr names)
	       inits)
	 ,(apply make-body names)))))

(define (call-with-new-names names procedure)
  (apply procedure
	 (map (lambda (name)
		(let ((n (hash-table-ref/default name-counters name 0)))
		  (hash-table-set! name-counters name (+ n 1))
		  (symbol name '. n)))
	      names)))

(define name-counters)

(define (join-vals vals1 vals2)
  `(CONS ,vals1 ,vals2))

(define (single-val val)
  `(CONS ',single-val-marker ,val))

(define (null-vals)
  ''())

;; Needed at runtime by parsers:
(define (list-parser-vals->list vals)
  (let loop ((vals vals) (items '()) (k reverse!))
    (if (pair? vals)
	(if (eq? (car vals) single-val-marker)
	    (k (cons (cdr vals) items))
	    (loop (car vals)
		  items
		  (lambda (items)
		    (loop (cdr vals)
			  items
			  k))))
	(k items))))

(define (list-parser-vals-length vals)
  (if (pair? vals)
      (let loop ((vals vals))
	(if (eq? (car vals) single-val-marker)
	    1
	    (+ (loop (car vals))
	       (loop (cdr vals)))))
      0))

(define (list-parser-vals-ref vals index)
  (if (not (pair? vals))
      (error:bad-range-argument index 'LIST-PARSER-VALS-REF))
  (let loop ((vals vals) (i 0) (stack '()))
    (if (eq? (car vals) single-val-marker)
	(if (< i index)
	    (begin
	      (if (not (pair? stack))
		  (error:bad-range-argument index 'LIST-PARSER-VALS-REF))
	      (loop (car stack)
		    (+ i 1)
		    (cdr stack)))
	    (cdr vals))
	(loop (car vals)
	      i
	      (cons (cdr vals) stack)))))

(define single-val-marker
  '|#[(runtime list-parser)single-val-marker]|)

;;;; Optimization

;;; Made easier by two facts: each bound name is unique, and we never
;;; copy expressions.

(define (optimize-result expr)
  (if enable-optimizer?
      (optimize-cons (optimize-lets expr))
      expr))

(define enable-optimizer? #t)

(define (optimize-lets expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     rewrite-reference
	     rewrite-lambda
	     rewrite-loop
	     (lambda (expr loop)
	       (let ((expr (rewrite-combination expr loop)))
		 (if (syntax-match? '('LAMBDA (* SYMBOL) EXPRESSION)
				    (car expr))
		     (optimize-let (cadar expr)
				   (cdr expr)
				   (caddar expr)
				   loop)
		     expr)))))

(define (optimize-let names vals body loop)
  (let ((vals (map loop vals))
	(body (loop body)))
    (let ((bindings
	   (remove (lambda (b*) (= (car b*) 0))
		   (map (lambda (name value)
			  (cons (count-refs-in name body)
				(cons name value)))
			names
			vals))))
      (receive (to-substitute to-keep)
	   (partition (lambda (b*)
			(or (= (car b*) 1)
			    (symbol? (cddr b*))))
		      bindings)
	(let ((new-body
	       (optimize-lets
		(if (pair? to-substitute)
		    (substitute (map cdr to-substitute) body)
		    body))))
	  (if (pair? to-keep)
	      `((LAMBDA ,(map cadr to-keep) ,new-body)
		,@(map cddr to-keep))
	      new-body))))))

(define (optimize-cons expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     rewrite-reference
	     rewrite-lambda
	     rewrite-loop
	     (lambda (expr loop)
	       (let ((expr (rewrite-combination expr loop)))
		 (if (and (eq? (car expr) 'CONS)
			  (not (equal? (cadr expr) `',single-val-marker)))
		     (optimize-cons-1 (cadr expr) (caddr expr))
		     expr)))))

(define (optimize-cons-1 car-expr cdr-expr)
  (let ((car-expr (optimize-cons car-expr))
	(cdr-expr (optimize-cons cdr-expr)))
    (cond ((equal? car-expr (null-vals)) cdr-expr)
	  ((equal? cdr-expr (null-vals)) car-expr)
	  (else `(CONS ,car-expr ,cdr-expr)))))

(define (count-refs-in name expr)
  (walk-expr expr
	     (lambda (expr) expr 0)
	     (lambda (expr) expr 0)
	     (lambda (expr) (if (eq? expr name) 1 0))
	     (lambda (expr loop) (loop (caddr expr)))
	     (lambda (expr loop)
	       (+ (apply +
			 (map (lambda (binding)
				(loop (cadr binding)))
			      (caddr expr)))
		  (loop (cadddr expr))))
	     (lambda (expr loop) (apply + (map loop expr)))))

(define (substitute bindings expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     (lambda (expr)
	       (let ((expr (rewrite-reference expr)))
		 (let ((p (assq expr bindings)))
		   (if p
		       (cdr p)
		       expr))))
	     rewrite-lambda
	     rewrite-loop
	     rewrite-combination))

(define (walk-expr expr
		   if-constant if-quote if-reference
		   if-lambda if-loop if-combination)
  (let loop ((expr expr))
    (cond ((syntax-match? '('LAMBDA (* SYMBOL) EXPRESSION) expr)
	   (if-lambda expr loop))
	  ((syntax-match? '('LET SYMBOL (* (SYMBOL EXPRESSION)) EXPRESSION)
			  expr)
	   (if-loop expr loop))
	  ((syntax-match? '('QUOTE EXPRESSION) expr)
	   (if-quote expr))
	  ((syntax-match? '(+ EXPRESSION) expr)
	   (if-combination expr loop))
	  ((syntax-match? 'IDENTIFIER expr)
	   (if-reference expr))
	  (else
	   (if-constant expr)))))

(define (rewrite-constant expr)
  expr)

(define (rewrite-quote expr)
  expr)

(define (rewrite-reference expr)
  expr)

(define (rewrite-lambda expr loop)
  `(LAMBDA ,(cadr expr)
     ,(loop (caddr expr))))

(define (rewrite-loop expr loop)
  `(LET ,(cadr expr)
     ,(map (lambda (binding)
	     (list (car binding)
		   (loop (cadr binding))))
	   (caddr expr))
     ,(loop (cadddr expr))))

(define (rewrite-combination expr loop)
  (map loop expr))