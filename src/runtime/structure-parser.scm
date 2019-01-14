#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Parsing language for structured objects
;;; package: (runtime structure-parser)

(declare (usual-integrations))

(define-syntax object-parser
  (sc-macro-transformer
   (lambda (form env)
     (if (syntax-match? '(form) (cdr form))
	 (compile-top-level (cadr form) 'object env)
	 (ill-formed-syntax form)))))

(define (apply-object-parser parser object)
  (parser object
	  (lambda (vals lose)
	    lose
	    (structure-parser-values->list vals))
	  (lambda ()
	    #f)))

(define-syntax list-parser
  (sc-macro-transformer
   (lambda (form env)
     (if (syntax-match? '(* form) (cdr form))
	 (compile-top-level `(seq ,@(cdr form)) 'list env)
	 (ill-formed-syntax form)))))

(define (apply-list-parser parser items)
  (parser items
	  (lambda (items vals lose)
	    (if (null? items)
		(structure-parser-values->list vals)
		(lose)))
	  (lambda ()
	    #f)))

(define-syntax vector-parser
  (sc-macro-transformer
   (lambda (form env)
     (if (syntax-match? '(* form) (cdr form))
	 (compile-top-level `(seq ,@(cdr form)) 'vector env)
	 (ill-formed-syntax form)))))

(define (apply-vector-parser parser vector #!optional start end)
  (let ((end (if (default-object? end) (vector-length vector) end)))
    (parser vector
	    (if (default-object? start) 0 start)
	    end
	    (lambda (start vals lose)
	      (if (fix:= start end)
		  (structure-parser-values->list vals)
		  (lose)))
	    (lambda ()
	      #f))))

;;;; Compiler

(define (compile-top-level pattern caller-context env)
  (parameterize ((name-counters (make-strong-eq-hash-table)))
    (optimize-result
     (compile-pattern pattern caller-context env))))

(define (compile-pattern pattern caller-context env)
  (let ((pattern* (rewrite-pattern pattern)))
    (let ((pc
	   (and (pair? pattern*)
		(get-pattern-compiler (car pattern*) caller-context))))
      (if (not pc)
	  (error "Unrecognized pattern:" pattern))
      (if (not (syntax-match? (pc-syntax pc) (cdr pattern*)))
	  (error "Ill-formed pattern:" pattern))
      (let ((callee-context (pc-context pc))
	    (call-generic
	     (lambda ()
	       ((pc-compiler pc) pattern* caller-context env)))
	    (call-specific
	     (lambda ()
	       ((pc-compiler pc) pattern* env))))
	(cond ((list? callee-context)
	       (if (not (memq caller-context callee-context))
		   (error "Pattern used in wrong context:" pattern))
	       (call-generic))
	      ((eq? callee-context caller-context)
	       (call-specific))
	      ((eq? callee-context 'object)
	       ((get-context-method 'call-object-method caller-context)
		(call-specific)))
	      (else
	       (call-generic)))))))

(define (rewrite-pattern pattern)
  (cond ((identifier? pattern)
	 (rewrite-pattern `(sexp ,pattern)))
	((or (char? pattern)
	     (string? pattern)
	     (number? pattern)
	     (boolean? pattern)
	     (null? pattern))
	 (rewrite-pattern `(quote ,pattern)))
	((syntax-match? '('+ * form) pattern)
	 (rewrite-pattern `(seq ,@(cdr pattern) (* ,@(cdr pattern)))))
	((syntax-match? '('? * form) pattern)
	 (rewrite-pattern `(alt (seq ,@(cdr pattern)) (values))))
	(else pattern)))

(define (get-pattern-compiler name caller-context)
  (find (lambda (pc)
	  (and (eq? (pc-name pc) name)
	       (let ((callee-context (pc-context pc)))
		 (or (list? callee-context)
		     (eq? callee-context caller-context)
		     (eq? callee-context 'object)
		     (eq? callee-context 'any)))))
	pattern-compilers))

(define (define-pattern-compiler template context compiler)
  (set! pattern-compilers
	(let ((name (car template)))
	  (cons (make-pc name (cdr template) context compiler)
		(let ((listify
		       (lambda (item)
			 (if (list? item) item (list item)))))
		  (remove! (let ((c1 (listify context)))
			     (lambda (pc)
			       (and (eq? (pc-name pc) name)
				    (any (lambda (c)
					   (memq c c1))
					 (listify (pc-context pc))))))
			   pattern-compilers)))))
  unspecific)

(define pattern-compilers '())

(define-record-type <pc>
    (make-pc name syntax context compiler)
    pc?
  (name pc-name)
  (syntax pc-syntax)
  (context pc-context)
  (compiler pc-compiler))

(define (get-context-method name context)
  (let ((v
	 (find (lambda (v)
		 (and (eq? (vector-ref v 0) name)
		      (eq? (vector-ref v 1) context)))
	       context-methods)))
    (if (not v)
	(error "Missing context method:" name context))
    (vector-ref v 2)))

(define (define-context-method name context procedure)
  (let ((v
	 (find (lambda (v)
		 (and (eq? (vector-ref v 0) name)
		      (eq? (vector-ref v 1) context)))
	       context-methods)))
    (if v
	(vector-set! v 2 procedure)
	(begin
	  (set! context-methods
		(cons (vector name context procedure)
		      context-methods))
	  unspecific))))

(define context-methods '())

;;;; Object context

(define-pattern-compiler '(match-any) 'object
  (lambda (pattern env)
    pattern env
    (make-object-parser
     (lambda (item win lose)
       `(,win ,(single-val item) ,lose)))))

(define-pattern-compiler '(match-if expression) 'object
  (lambda (pattern env)
    (make-object-parser
     (lambda (item win lose)
       `(if (,(close-syntax (cadr pattern) env) ,item)
	    (,win ,(single-val item) ,lose)
	    (,lose))))))

(define-pattern-compiler '(noise-if expression) 'object
  (lambda (pattern env)
    (make-object-parser
     (lambda (item win lose)
       `(if (,(close-syntax (cadr pattern) env) ,item)
	    (,win ,(null-vals) ,lose)
	    (,lose))))))

(define-pattern-compiler '(match datum) 'object
  (lambda (pattern env)
    env
    (make-object-parser
     (lambda (item win lose)
       `(if (,(equality-predicate (cadr pattern)) ,item ',(cadr pattern))
	    (,win ,(single-val item) ,lose)
	    (,lose))))))

(define-pattern-compiler '(quote datum) 'object
  (lambda (pattern env)
    env
    (make-object-parser
     (lambda (item win lose)
       `(if (,(equality-predicate (cadr pattern)) ,item ',(cadr pattern))
	    (,win ,(null-vals) ,lose)
	    (,lose))))))

(define (equality-predicate datum)
  (cond ((or (symbol? datum)
	     (char? datum)
	     (boolean? datum)
	     (null? datum))
	 'eq?)
	((number? datum) 'eqv?)
	(else 'equal?)))

(define-context-method 'values 'object
  (lambda (vals)
    (make-object-parser
     (lambda (item win lose)
       item
       `(,win ,vals ,lose)))))

(define-context-method 'alt 'object
  (lambda (make-body)
    (make-object-parser
     (lambda (item win lose)
       (make-body (lambda (callee lose)
		    `(,callee ,item ,win ,lose))
		  lose)))))

(define-context-method 'transform-vals 'object
  (lambda (callee transform)
    (make-object-parser
     (lambda (item win lose)
       `(,callee ,item
		 ,(make-object-winner
		   (lambda (vals lose)
		     (transform vals
				lose
				(lambda (vals lose)
				  `(,win ,vals ,lose)))))
		 ,lose)))))

(define-pattern-compiler '(cons form form) 'object
  (lambda (pattern env)
    (make-object-parser
     (lambda (item win lose)
       `(if (pair? ,item)
	    (,(compile-pattern (cadr pattern) 'object env)
	     (car ,item)
	     ,(make-object-winner
	       (lambda (vals lose)
		 `(,(compile-pattern (caddr pattern) 'object env)
		   (cdr ,item)
		   ,(make-object-winner
		     (lambda (vals* lose)
		       `(,win ,(join-vals vals vals*)
			      ,lose)))
		   ,lose)))
	     ,lose)
	    (,lose))))))

(define-pattern-compiler '(list * form) 'object
  (lambda (pattern env)
    (make-object-parser
     (lambda (item win lose)
       `(,(compile-pattern `(seq ,@(cdr pattern) (end)) 'list env)
	 ,item
	 ,(make-list-winner
	   (lambda (items vals lose)
	     items
	     `(,win ,vals ,lose)))
	 ,lose)))))

(define-pattern-compiler '(vector * form) 'object
  (lambda (pattern env)
    (make-object-parser
     (lambda (item win lose)
       `(if (vector? ,item)
	    (,(compile-pattern `(seq ,@(cdr pattern) (end)) 'vector env)
	     ,item
	     0
	     (vector-length ,item)
	     ,(make-vector-winner
	       (lambda (start vals lose)
		 start
		 `(,win ,vals ,lose)))
	     ,lose)
	    (,lose))))))

;;;; Generic patterns

(define-pattern-compiler '(sexp expression) 'any
  (lambda (pattern context env)
    context
    (close-syntax (cadr pattern) env)))

(define-pattern-compiler '(values * expression) 'any
  (lambda (pattern context env)
    ((get-context-method 'values context)
     (apply join-vals
	    (map (lambda (expr)
		   (single-val (close-syntax expr env)))
		 (cdr pattern))))))

(define-pattern-compiler '(alt * form) 'any
  (lambda (pattern context env)
    ((get-context-method 'alt context)
     (lambda (make-call lose)
       (let loop ((patterns (cdr pattern)))
	 (if (pair? patterns)
	     (make-call (compile-pattern (car patterns) context env)
			(make-loser (loop (cdr patterns))))
	     `(,lose)))))))

(define-pattern-compiler '(* * form) '(list vector)
  (lambda (pattern context env)
    ((get-context-method '* context)
     (lambda (location lose make-call make-termination)
       (make-loop `((location ,location)
		    (vals ,(null-vals))
		    (lose ,lose))
	 (lambda (loop location vals lose)
	   (make-call (compile-pattern `(seq ,@(cdr pattern)) context env)
		      location
		      (lambda (location vals* lose)
			`(,loop ,location
				,(join-vals vals vals*)
				,lose))
		      (make-loser (make-termination location vals lose)))))))))

(define-pattern-compiler '(seq * form) '(list vector)
  (lambda (pattern context env)
    (let ((callees
	   (map (lambda (pattern)
		  (compile-pattern pattern context env))
		(cdr pattern))))
      (if (and (pair? callees)
	       (null? (cdr callees)))
	  (car callees)
	  ((get-context-method 'seq context)
	   (lambda (location lose make-recursion make-termination)
	     (if (pair? callees)
		 (let loop
		     ((callees callees)
		      (location location)
		      (vals (null-vals))
		      (lose lose))
		   (if (pair? callees)
		       (make-recursion (car callees)
				       location
				       (lambda (location vals* lose)
					 (loop (cdr callees)
					       location
					       (join-vals vals vals*)
					       lose))
				       lose)
		       (make-termination location vals lose)))
		 (make-termination location (null-vals) lose))))))))

(define-pattern-compiler '(noise form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (cadr pattern) context env)
     (lambda (vals lose make-win)
       vals
       (make-win (null-vals) lose)))))

(define-pattern-compiler '(map expression form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (caddr pattern) context env)
     (lambda (vals lose make-win)
       (make-win `(map-structure-parser-values
		   ,(close-syntax (cadr pattern) env)
		   ,vals)
		 lose)))))

(define-pattern-compiler '(encapsulate expression form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (caddr pattern) context env)
     (lambda (vals lose make-win)
       (make-win (single-val
		  (call-out (close-syntax (cadr pattern) env)
			    vals))
		 lose)))))

(define-pattern-compiler '(qualify expression form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (caddr pattern) context env)
     (lambda (vals lose make-win)
       `(if ,(call-out (close-syntax (cadr pattern) env)
		       vals)
	    ,(make-win vals lose)
	    (,lose))))))

(define-pattern-compiler '(disqualify expression form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (caddr pattern) context env)
     (lambda (vals lose make-win)
       `(if (not ,(call-out (close-syntax (cadr pattern) env)
			    vals))
	    ,(make-win vals lose)
	    (,lose))))))

(define-pattern-compiler '(transform expression form) 'any
  (lambda (pattern context env)
    ((get-context-method 'transform-vals context)
     (compile-pattern (caddr pattern) context env)
     (lambda (vals lose make-win)
       (make-let `((vals
		    ,(call-out (close-syntax (cadr pattern) env)
			       vals)))
	 (lambda (vals)
	   `(if ,vals
		,(make-win `(list->structure-parser-values ,vals)
			   lose)
		(,lose))))))))

(define-pattern-compiler '(object form) '(list vector)
  (lambda (pattern context env)
    ((get-context-method 'call-object-method context)
     (compile-pattern (cadr pattern) 'object env))))

;;;; List context

(define-pattern-compiler '(end) 'list
  (lambda (pattern env)
    pattern env
    (make-list-parser
     (lambda (items win lose)
       `(if (null? ,items)
	    (,win ,items ,(null-vals) ,lose)
	    (,lose))))))

(define-context-method 'call-object-method 'list
  (lambda (callee)
    (make-list-parser
     (lambda (items win lose)
       `(if (pair? ,items)
	    (,callee (car ,items)
		     ,(make-object-winner
		       (lambda (vals lose)
			 `(,win (cdr ,items) ,vals ,lose)))
		     ,lose)
	    (,lose))))))

(define-context-method 'seq 'list
  (lambda (make-body)
    (make-list-parser
     (lambda (items win lose)
       (make-body items
		  lose
		  (lambda (callee items recurse lose)
		    `(,callee ,items
			      ,(make-list-winner recurse)
			      ,lose))
		  (lambda (items vals lose)
		    `(,win ,items ,vals ,lose)))))))

(define-context-method 'values 'list
  (lambda (vals)
    (make-list-parser
     (lambda (items win lose)
       `(,win ,items ,vals ,lose)))))

(define-context-method 'alt 'list
  (lambda (make-body)
    (make-list-parser
     (lambda (items win lose)
       (make-body (lambda (callee lose)
		    `(,callee ,items ,win ,lose))
		  lose)))))

(define-context-method '* 'list
  (lambda (make-body)
    (make-list-parser
     (lambda (items win lose)
       (make-body items
		  lose
		  (lambda (callee items recurse lose)
		    `(,callee ,items
			      ,(make-list-winner recurse)
			      ,lose))
		  (lambda (items vals lose)
		    `(,win ,items ,vals ,lose)))))))

(define-context-method 'transform-vals 'list
  (lambda (callee transform)
    (make-list-parser
     (lambda (items win lose)
       `(,callee ,items
		 ,(make-list-winner
		   (lambda (items vals lose)
		     (transform vals
				lose
				(lambda (vals lose)
				  `(,win ,items ,vals ,lose)))))
		 ,lose)))))

;;;; Vector context

(define-pattern-compiler '(end) 'vector
  (lambda (pattern env)
    pattern env
    (make-vector-parser
     (lambda (vector start end win lose)
       vector
       `(if (fix:= ,start ,end)
	    (,win ,end ,(null-vals) ,lose)
	    (,lose))))))

(define-context-method 'call-object-method 'vector
  (lambda (callee)
    (make-vector-parser
     (lambda (vector start end win lose)
       `(if (fix:< ,start ,end)
	    (,callee (vector-ref ,vector ,start)
		     ,(make-object-winner
		       (lambda (vals lose)
			 `(,win (fix:+ ,start 1) ,vals ,lose)))
		     ,lose)
	    (,lose))))))

(define-context-method 'seq 'vector
  (lambda (make-body)
    (make-vector-parser
     (lambda (vector start end win lose)
       (make-body start
		  lose
		  (lambda (callee start recurse lose)
		    `(,callee ,vector ,start ,end
			      ,(make-vector-winner recurse)
			      ,lose))
		  (lambda (start vals lose)
		    `(,win ,start ,vals ,lose)))))))

(define-context-method 'values 'vector
  (lambda (vals)
    (make-vector-parser
     (lambda (vector start end win lose)
       vector end
       `(,win ,start ,vals ,lose)))))

(define-context-method 'alt 'vector
  (lambda (make-body)
    (make-vector-parser
     (lambda (vector start end win lose)
       (make-body (lambda (callee lose)
		    `(,callee ,vector ,start ,end ,win ,lose))
		  lose)))))

(define-context-method '* 'vector
  (lambda (make-body)
    (make-vector-parser
     (lambda (vector start end win lose)
       (make-body start
		  lose
		  (lambda (callee start recurse lose)
		    `(,callee ,vector
			      ,start
			      ,end
			      ,(make-vector-winner recurse)
			      ,lose))
		  (lambda (start vals lose)
		    `(,win ,start ,vals ,lose)))))))

(define-context-method 'transform-vals 'vector
  (lambda (callee transform)
    (make-vector-parser
     (lambda (vector start end win lose)
       `(,callee ,vector ,start ,end
		 ,(make-vector-winner
		   (lambda (start vals lose)
		     (transform vals
				lose
				(lambda (vals lose)
				  `(,win ,start ,vals ,lose)))))
		 ,lose)))))

;;;; Values abstraction

(define (join-vals . valss)
  (reduce-right (lambda (vals1 vals2)
		  `(cons ,vals1 ,vals2))
		(null-vals)
		valss))

(define (single-val val)
  `(cons ',single-val-marker ,val))

(define (null-vals)
  ''())

(define single-val-marker
  '|#[(runtime object-parser)single-val-marker]|)

;;; The next three procedures are used by object parsers at runtime.

(define (structure-parser-values->list vals)
  (let loop ((vals* vals) (tail '()))
    (cond ((null? vals*)
	   tail)
	  ((pair? vals*)
	   (if (eq? (car vals*) single-val-marker)
	       (cons (cdr vals*) tail)
	       (loop (car vals*)
		     (loop (cdr vals*)
			   tail))))
	  (else
	   (error:not-a structure-parser-values?
			vals
			'structure-parser-values->list)))))

(define (list->structure-parser-values items)
  (map (lambda (item)
	 (cons single-val-marker item))
       items))

(define (structure-parser-values . items)
  (list->structure-parser-values items))

(define (map-structure-parser-values procedure vals)
  (let loop ((vals* vals))
    (cond ((null? vals*)
	   vals*)
	  ((pair? vals*)
	   (if (eq? (car vals*) single-val-marker)
	       (cons single-val-marker
		     (procedure (cdr vals*)))
	       (cons (loop (car vals*))
		     (loop (cdr vals*)))))
	  (else
	   (error:not-a structure-parser-values? vals
			'map-structure-parser-values)))))

(define (structure-parser-values? object)
  (let loop ((object object))
    (or (null? object)
	(and (pair? object)
	     (or (eq? (car object) single-val-marker)
		 (and (loop (car object))
		      (loop (cdr object))))))))

(define (structure-parser-values-length vals)
  (let loop ((vals* vals))
    (cond ((null? vals*)
	   0)
	  ((pair? vals*)
	   (if (eq? (car vals*) single-val-marker)
	       1
	       (+ (loop (car vals*))
		  (loop (cdr vals*)))))
	  (else
	   (error:not-a structure-parser-values?
			vals
			'structure-parser-values-length)))))

(define (structure-parser-values-ref vals index)
  (let ((caller 'structure-parser-values-ref))

    (define (loop vals* i stack)
      (cond ((null? vals*)
	     (pop i stack))
	    ((pair? vals*)
	     (if (eq? (car vals*) single-val-marker)
		 (if (< i index)
		     (pop (+ i 1) stack)
		     (cdr vals*))
		 (push vals* i stack)))
	    (else
	     (error:not-a structure-parser-values? vals caller))))

    (define (push vals* i stack)
      (loop (car vals*)
	    i
	    (cons (cdr vals*) stack)))

    (define (pop i stack)
      (if (not (pair? stack))
	  (error:bad-range-argument index caller))
      (loop (car stack)
	    i
	    (cdr stack)))

    (loop vals 0 '())))

;;;; Helpers for code generation

(define (make-object-parser make-body)
  (make-lambda '(item win lose) make-body))

(define (make-object-winner make-body)
  (make-lambda '(vals lose) make-body))

(define (make-list-parser make-body)
  (make-lambda '(items win lose) make-body))

(define (make-list-winner make-body)
  (make-lambda '(items vals lose) make-body))

(define (make-vector-parser make-body)
  (make-lambda '(vector start end win lose) make-body))

(define (make-vector-winner make-body)
  (make-lambda '(start vals lose) make-body))

(define (make-loser body)
  (make-lambda '() (lambda () body)))

(define (call-out procedure vals)
  `(apply ,procedure (structure-parser-values->list ,vals)))

(define (make-lambda names make-body)
  (call-with-new-names names
    (lambda names
      `(lambda ,names
	 ,(apply make-body names)))))

(define (make-let bindings make-body)
  (let ((names (map car bindings))
	(args (map cadr bindings)))
    (call-with-new-names names
      (lambda names
	`((lambda ,names
	    ,(apply make-body names))
	  ,@args)))))

(define (make-loop bindings make-body)
  (let ((names (map car bindings))
	(inits (map cadr bindings)))
    (call-with-new-names (cons 'loop names)
      (lambda names
	`(let ,(car names)
	   ,(map (lambda (name init)
		   `(,name ,init))
		 (cdr names)
		 inits)
	   ,(apply make-body names))))))

(define (call-with-new-names names procedure)
  (apply procedure
	 (map (lambda (name)
		(let* ((t (name-counters))
		       (n (hash-table-ref/default t name 0)))
		  (hash-table-set! t name (+ n 1))
		  (symbol name '. n)))
	      names)))

(define name-counters)

(define (initialize-package!)
  (set! name-counters (make-unsettable-parameter unspecific))
  unspecific)

;;;; Optimizer

;;; Made easier because: (1) each bound name is unique; (2) we never
;;; copy expressions; and (3) there are no side-effects.

(define (optimize-result expr)
  (fixup-lambdas
   (if enable-optimizer?
       (peephole-optimizer (optimize-lets expr))
       expr)))

(define enable-optimizer? #t)

(define (optimize-lets expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     rewrite-reference
	     rewrite-lambda
	     rewrite-loop
	     (lambda (expr loop)
	       (let ((expr (rewrite-form expr loop)))
		 (if (syntax-match? '('lambda (* symbol) expression)
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
			    (substitutable? (cddr b*))))
		      bindings)
	(let ((new-body
	       (optimize-lets
		(if (pair? to-substitute)
		    (substitute (map cdr to-substitute) body)
		    body))))
	  (if (pair? to-keep)
	      `((lambda ,(map cadr to-keep) ,new-body)
		,@(map cddr to-keep))
	      new-body))))))

(define (substitutable? expr)
  (or (symbol? expr)
      (number? expr)
      (syntax-match? `('car ,substitutable?) expr)
      (syntax-match? `('cdr ,substitutable?) expr)
      (syntax-match? `('vector-length ,substitutable?) expr)
      (syntax-match? `('fix:+ ,substitutable? ,substitutable?) expr)
      (syntax-match? `('fix:< ,substitutable? ,substitutable?) expr)
      (syntax-match? `('fix:= ,substitutable? ,substitutable?) expr)
      (syntax-match? `('vector-ref ,substitutable? ,substitutable?) expr)))

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
	     rewrite-form))

(define (fixup-lambdas expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     rewrite-reference
	     (lambda (expr loop)
	       (let ((names (cadr expr))
		     (body (loop (caddr expr))))
		 `(lambda ,names
		    ,@(filter (lambda (name)
				(= (count-refs-in name body) 0))
			      names)
		    ,body)))
	     rewrite-loop
	     rewrite-form))

(define (peephole-optimizer expr)
  (walk-expr expr
	     rewrite-constant
	     rewrite-quote
	     rewrite-reference
	     rewrite-lambda
	     rewrite-loop
	     (lambda (expr loop)
	       (let ((expr (rewrite-form expr loop)))
		 (let loop ((optimizers (get-peephole-optimizers expr)))
		   (if (pair? optimizers)
		       ((car optimizers) expr
					 peephole-optimizer
					 (lambda () (loop (cdr optimizers))))
		       expr))))))

(define (define-peephole-optimizer pattern optimizer)
  (set! peephole-optimizers
	(cons (cons pattern optimizer)
	      peephole-optimizers))
  unspecific)

(define (get-peephole-optimizers expr)
  (map cdr
       (filter (lambda (entry)
		 (syntax-match? (car entry) expr))
	       peephole-optimizers)))

(define peephole-optimizers '())

(define-peephole-optimizer `('cons expression expression)
  (lambda (expr win lose)
    (cond ((equal? (cadr expr) (null-vals)) (win (caddr expr)))
	  ((equal? (caddr expr) (null-vals)) (win (cadr expr)))
	  (else (lose)))))

(define-peephole-optimizer `('fix:+ ,fix:fixnum? ,fix:fixnum?)
  (lambda (expr win lose)
    lose
    (win (fix:+ (cadr expr) (caddr expr)))))

(define-peephole-optimizer `('fix:+ ('fix:+ expression ,fix:fixnum?)
				    ,fix:fixnum?)
  (lambda (expr win lose)
    lose
    (win `(fix:+ ,(cadr (cadr expr))
		 ,(fix:+ (caddr (cadr expr)) (caddr expr))))))

(define-peephole-optimizer `('fix:< ,fix:fixnum? ,fix:fixnum?)
  (lambda (expr win lose)
    lose
    (win (fix:< (cadr expr) (caddr expr)))))

(define-peephole-optimizer `('fix:< ('fix:+ expression ,fix:fixnum?)
				    ,fix:fixnum?)
  (lambda (expr win lose)
    lose
    (let ((base (cadr (cadr expr)))
	  (a (caddr (cadr expr)))
	  (b (caddr expr)))
      (if (fix:<= a b)
	  (win `(fix:< ,base ,(fix:- b a)))
	  ;; We know that BASE is >= 0.
	  (win '#f)))))

(define-peephole-optimizer '('if #f expression expression)
  (lambda (expr win lose)
    lose
    (win (cadddr expr))))

(define-peephole-optimizer '('if #t expression expression)
  (lambda (expr win lose)
    lose
    (win (caddr expr))))

(define-peephole-optimizer '('if expression
				 ('if expression expression expression)
				 expression)
  (lambda (expr win lose)
    (if (equal? (cadddr (caddr expr))
		(cadddr expr))
	(win `(if (and ,(cadr expr)
		       ,(cadr (caddr expr)))
		  ,(caddr (caddr expr))
		  ,(cadddr expr)))
	(lose))))

(define-peephole-optimizer '('and * expression)
  (lambda (expr win lose)
    (cond ((null? (cdr expr))
	   (win '#t))
	  ((null? (cddr expr))
	   (win (cadr expr)))
	  ((memq '#t (cdr expr))
	   (win (delq '#t (cdr expr))))
	  ((memq '#f (cdr expr))
	   (win '#f))
	  ((any (lambda (expr)
		  (syntax-match? '('and * expression) expr))
		(cdr expr))
	   (win `(and
		  ,@(append-map (lambda (expr)
				  (if (syntax-match? '('and * expression) expr)
				      (cdr expr)
				      (list expr)))
				(cdr expr)))))
	  (else (lose)))))

(define (walk-expr expr
		   if-constant if-quote if-reference
		   if-lambda if-loop if-form)
  (let loop ((expr expr))
    (cond ((syntax-match? '('lambda (* symbol) expression) expr)
	   (if-lambda expr loop))
	  ((syntax-match? '('let symbol (* (symbol expression)) expression)
			  expr)
	   (if-loop expr loop))
	  ((syntax-match? '('quote expression) expr)
	   (if-quote expr))
	  ((syntax-match? '(+ expression) expr)
	   (if-form expr loop))
	  ((syntax-match? 'identifier expr)
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
  `(lambda ,(cadr expr)
     ,(loop (caddr expr))))

(define (rewrite-loop expr loop)
  `(let ,(cadr expr)
     ,(map (lambda (binding)
	     (list (car binding)
		   (loop (cadr binding))))
	   (caddr expr))
     ,(loop (cadddr expr))))

(define (rewrite-form expr loop)
  (map loop expr))

;;; Edwin Variables:
;;; lisp-indent/make-lambda: 1
;;; lisp-indent/make-let: 1
;;; lisp-indent/make-loop: 1
;;; End:
