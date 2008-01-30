#| -*-Scheme-*-

$Id: macros.scm,v 1.22 2008/01/30 20:02:39 cph Exp $

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

;;;; Macros

(declare (usual-integrations))

(define-syntax define-class
  (rsc-macro-transformer
   (let ((lose
	  (lambda (s a)
	    (error (string-append "Malformed " s ":") a))))
     (lambda (form environment)
       (if (syntax-match? '(DATUM (* EXPRESSION) * DATUM) (cdr form))
	   (let ((name (cadr form))
		 (superclasses (caddr form))
		 (slot-arguments
		  (map (lambda (arg) (canonicalize-slot-argument arg lose))
		       (cdddr form))))
	     (call-with-values
		 (lambda ()
		   (parse-define-class-name name environment lose))
	       (lambda (name post-definitions separator)
		 (let ((pre-definitions
			(extract-generic-definitions!
			 slot-arguments name separator environment lose)))
		   `(,(close-syntax 'BEGIN environment)
		     ,@pre-definitions
		     (,(close-syntax 'DEFINE environment)
		      ,name
		      (,(absolute 'MAKE-CLASS environment)
		       ',name
		       (,(absolute 'LIST environment) ,@superclasses)
		       (,(absolute 'LIST environment)
			,@(map (lambda (arg)
				 (if (null? (cdr arg))
				     `',arg
				     `(,(absolute 'LIST environment)
				       ',(car arg)
				       ,@(let loop ((plist (cdr arg)))
					   (if (pair? plist)
					       (cons* `',(car plist)
						      (cadr plist)
						      (loop (cddr plist)))
					       '())))))
			       slot-arguments))))
		     ,@post-definitions)))))
	   (ill-formed-syntax form))))))

(define (canonicalize-slot-argument arg lose)
  (cond ((symbol? arg)
	 (list arg))
	((and (pair? arg)
	      (symbol? (car arg))
	      (list? (cdr arg)))
	 (let loop ((plist (cdr arg)))
	   (if (pair? plist)
	       (begin
		 (if (not (and (symbol? (car plist))
			       (pair? (cdr plist))))
		     (lose "slot argument" arg))
		 (loop (cddr plist)))))
	 (list-copy arg))
	(else
	 (lose "slot argument" arg))))

(define (parse-define-class-name name environment lose)
  (call-with-values (lambda () (parse-define-class-name-1 name lose))
    (lambda (class-name alist)
      (let ((post-definitions '())
	    (separator #f))
	(let ((post-def
	       (lambda (def)
		 (set! post-definitions (cons def post-definitions))
		 unspecific)))
	  (for-each
	   (lambda (option)
	     (case (car option)
	       ((PREDICATE)
		(let ((pn
		       (cond ((null? (cdr option))
			      (default-predicate-name class-name))
			     ((and (pair? (cdr option))
				   (or (symbol? (cadr option))
				       (false? (cadr option)))
				   (null? (cddr option)))
			      (cadr option))
			     (else
			      (lose "class option" option)))))
		  (if pn
		      (post-def
		       `(,(close-syntax 'DEFINE environment)
			 ,pn
			 (,(absolute 'INSTANCE-PREDICATE environment)
			  ,class-name))))))
	       ((CONSTRUCTOR)
		(call-with-values
		    (lambda ()
		      (parse-constructor-option class-name lose option))
		  (lambda (name slots ii-args)
		    (post-def
		     `(,(close-syntax 'DEFINE environment)
		       ,name
		       (,(absolute 'INSTANCE-CONSTRUCTOR environment)
			,class-name
			',slots
			,@(map (lambda (x) `',x) ii-args)))))))
	       ((SEPARATOR)
		(if (or separator
			(not (and (pair? (cdr option))
				  (string? (cadr option))
				  (null? (cddr option)))))
		    (lose "class option" option))
		(set! separator (cadr option))
		unspecific)
	       (else
		(lose "class option" option))))
	   (if (assq 'PREDICATE alist)
	       alist
	       (cons '(PREDICATE) alist))))
	(values class-name post-definitions (or separator "-"))))))

(define (parse-define-class-name-1 name lose)
  (cond ((symbol? name)
	 (values name '()))
	((and (pair? name)
	      (symbol? (car name))
	      (list? (cdr name)))
	 (values (car name)
		 (map (lambda (option)
			(if (pair? option)
			    option
			    (list option)))
		      (cdr name))))
	(else (lose "class name" name))))

(define (parse-constructor-option class-name lose option)
  (cond ((syntax-match? `(SYMBOL (* SYMBOL) . ,optional?) (cdr option))
	 (values (cadr option) (caddr option) (cdddr option)))
	((syntax-match? `((* SYMBOL) . ,optional?) (cdr option))
	 (values (default-constructor-name class-name)
		 (cadr option)
		 (cddr option)))
	(else
	 (lose "class option" option))))

(define (optional? x)
  (or (null? x) (and (pair? x) (null? (cdr x)))))

(define (default-predicate-name class-name)
  (intern (string-append (strip-angle-brackets class-name) "?")))

(define (default-constructor-name class-name)
  (intern (string-append "make-" (strip-angle-brackets class-name))))

(define (make-named-lambda name required optional rest body environment)
  (let ((bvl
	 (append required
		 (if (null? optional)
		     '()
		     `(#!OPTIONAL ,@optional))
		 (or rest '()))))
    (if name
	`(,(close-syntax 'NAMED-LAMBDA environment) (,name ,@bvl) ,@body)
	`(,(close-syntax 'LAMBDA environment) ,bvl ,@body))))

(define (absolute name environment)
  (close-syntax `(ACCESS ,name #F) environment))

(define (extract-generic-definitions! slot-arguments name separator environment
				      lose)
  (let ((definitions '()))
    (for-each
     (lambda (arg)
       (if (and (pair? arg)
		(symbol? (car arg))
		(list? (cdr arg)))
	   (let loop ((plist (cdr arg)) (prev arg))
	     (if (and (pair? plist) (pair? (cdr plist)))
		 (if (eq? 'DEFINE (car plist))
		     (begin
		       (let ((keyword?
			      (lambda (element)
				(or (eq? 'ACCESSOR element)
				    (eq? 'MODIFIER element)
				    (eq? 'INITPRED element)))))
			 (if (not (or (eq? 'STANDARD (cadr plist))
				      (keyword? (cadr plist))
				      (and (list? (cadr plist))
					   (for-all? (cadr plist) keyword?))))
			     (lose "DEFINE property" arg)))
		       (set-cdr! prev (cddr plist))
		       (set! definitions
			     (append! (translate-define-arg (cadr plist)
							    name
							    separator
							    arg
							    environment)
				      definitions)))
		     (loop (cddr plist) (cdr plist)))))))
     slot-arguments)
    definitions))

(define (translate-define-arg arg name separator slot-argument environment)
  (let ((translate
	 (lambda (keyword standard? arity generate)
	   (if (or (and standard? (eq? 'STANDARD arg))
		   (eq? keyword arg)
		   (and (pair? arg) (memq keyword arg)))
	       ((lambda (name)
		  `((,(close-syntax 'DEFINE environment)
		     ,name
		     (,(absolute 'MAKE-GENERIC-PROCEDURE environment)
		      ,arity
		      ',name))))
		(or (plist-lookup keyword (cdr slot-argument) #f)
		    (let ((name (intern
				 (generate
				  (string-append (strip-angle-brackets name)
						 separator
						 (symbol->string
						  (car slot-argument)))))))
		      (set-cdr! slot-argument
				(cons* keyword name (cdr slot-argument)))
		      name)))
	       '()))))
    (append (translate 'ACCESSOR #t 1
		       (lambda (root) root))
	    (translate 'MODIFIER #t 2
		       (lambda (root) (string-append "set-" root "!")))
	    (translate 'INITPRED #f 1
		       (lambda (root) (string-append root "-initialized?"))))))

(define (plist-lookup key plist default)
  (let loop ((plist plist))
    (if (and (pair? plist) (pair? (cdr plist)))
	(if (eq? key (car plist))
	    (cadr plist)
	    (loop (cddr plist)))
	default)))

(define (strip-angle-brackets symbol)
  (let ((s (symbol->string symbol)))
    (if (and (fix:>= (string-length s) 2)
	     (char=? #\< (string-ref s 0))
	     (char=? #\> (string-ref s (fix:- (string-length s) 1))))
	(substring s 1 (fix:- (string-length s) 1))
	s)))

(define-syntax define-generic
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER MIT-BVL) (cdr form))
	 (call-with-values (lambda () (parse-mit-lambda-list (caddr form)))
	   (lambda (required optional rest)
	     `(,(close-syntax 'DEFINE environment)
	       ,(cadr form)
	       (,(absolute 'MAKE-GENERIC-PROCEDURE environment)
		',(let ((low (length required)))
		    (if rest
			(cons low #f)
			(let ((n (length optional)))
			  (if (> n 0)
			      (cons low (+ low n))
			      low))))
		',(cadr form)))))
	 (ill-formed-syntax form)))))

(define-syntax define-method
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER DATUM + EXPRESSION) (cdr form))
	 (call-with-values
	     (lambda () (parse-specialized-lambda-list (caddr form)))
	   (lambda (required specializers optional rest)
	     (let ((name (cadr form)))
	       (capture-syntactic-environment
		(lambda (instance-environment)
		  `(,(absolute 'ADD-METHOD environment)
		    ,name
		    ,(make-method-sexp name required optional rest specializers
				       (cdddr form)
				       environment
				       instance-environment)))))))
	 (ill-formed-syntax form)))))

(define-syntax define-computed-method
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER DATUM + EXPRESSION) (cdr form))
	 (call-with-values
	     (lambda () (parse-specialized-lambda-list (caddr form)))
	   (lambda (required specializers optional rest)
	     (let ((name (cadr form)))
	       `(,(absolute 'ADD-METHOD environment)
		 ,name
		 (,(absolute 'MAKE-COMPUTED-METHOD environment)
		  (,(absolute 'LIST environment) ,@specializers)
		  ,(make-named-lambda name required optional rest (cdddr form)
				      environment))))))
	 (ill-formed-syntax form)))))

(define-syntax define-computed-emp
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER EXPRESSION DATUM + EXPRESSION) (cdr form))
	 (call-with-values
	     (lambda () (parse-specialized-lambda-list (cadddr form)))
	   (lambda (required specializers optional rest)
	     (let ((name (cadr form)))
	       `(,(absolute 'ADD-METHOD environment)
		 ,name
		 (,(absolute 'MAKE-COMPUTED-EMP environment)
		  ,(caddr form)
		  (,(absolute 'LIST environment) ,@specializers)
		  ,(make-named-lambda name required optional rest (cddddr form)
				      environment))))))
	 (ill-formed-syntax form)))))

(define-syntax method
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(DATUM + EXPRESSION) (cdr form))
	 (call-with-values
	     (lambda () (parse-specialized-lambda-list (cadr form)))
	   (lambda (required specializers optional rest)
	     (capture-syntactic-environment
	      (lambda (instance-environment)
		(make-method-sexp #f required optional rest specializers
				  (caddr form)
				  environment
				  instance-environment)))))
	 (ill-formed-syntax form)))))

(define (make-method-sexp name required optional rest specializers body
			  environment instance-environment)
  (let ((normal
	 (lambda ()
	   (call-with-values
	       (lambda ()
		 (call-next-method-used? body
					 environment
					 instance-environment))
	     (lambda (body used?)
	       (let ((s `(,(absolute 'LIST environment) ,@specializers))
		     (l
		      (make-named-lambda name required optional rest body
					 environment)))
		 (if used?
		     `(,(absolute 'MAKE-CHAINED-METHOD environment)
		       ,s
		       (,(close-syntax 'LAMBDA environment) (CALL-NEXT-METHOD)
							    ,l))
		     `(,(absolute 'MAKE-METHOD environment) ,s ,l)))))))
	(match-identifier
	 (lambda (identifier)
	   (lambda (identifier*)
	     (identifier=? environment identifier
			   instance-environment identifier*)))))
    (if (and (null? optional)
	     (not rest)
	     (not (and (pair? specializers)
		       (eq? '<OBJECT> (car specializers)))))
	(case (length required)
	  ((1)
	   (cond ((match `((,(match-identifier 'SLOT-VALUE)
			    ,(car required)
			    ',symbol?))
			 body)
		  `(,(absolute 'SLOT-ACCESSOR-METHOD environment)
		    ,(car specializers)
		    ,(caddar body)))
		 ((match `((,(match-identifier 'SLOT-INITIALIZED?)
			    ,(car required)
			    ',symbol?))
			 body)
		  `(,(absolute 'SLOT-INITPRED-METHOD environment)
		    ,(car specializers)
		    ,(caddar body)))
		 (else (normal))))
	  ((2)
	   (if (and (null? (cdr specializers))
		    (match
		     `((,(match-identifier 'SET-SLOT-VALUE!)
			,(car required)
			',symbol?
			,(cadr required)))
		     body))
	       `(,(absolute 'SLOT-MODIFIER-METHOD environment)
		 ,(car specializers)
		 ,(caddar body))
	       (normal)))
	  (else (normal)))
	(normal))))

(define (match pattern instance)
  (cond ((procedure? pattern)
	 (pattern instance))
	((pair? pattern)
	 (and (pair? instance)
	      (match (car pattern) (car instance))
	      (match (cdr pattern) (cdr instance))))
	(else
	 (eqv? pattern instance))))

(define (call-next-method-used? body environment instance-environment)
  (if (pair? body)
      (let ((body
	     (let loop ((body body))
	       (if (and (identifier? (car body))
			(pair? (cdr body)))
		   (if (identifier=? instance-environment (car body)
				     environment 'CALL-NEXT-METHOD)
		       (loop (cdr body))
		       (cons (car body) (loop (cdr body))))
		   body))))
	(values body
		(let ((l
		       (syntax `(,(close-syntax 'LAMBDA environment)
				 (CALL-NEXT-METHOD)
				 ,@body)
			       instance-environment)))
		  (free-variable? (car (lambda-bound l))
				  (lambda-body l)))))
      (values body #f)))

(define free-variable?
  (letrec
      ((do-expr
	(lambda (name expr)
	  ((scode-walk scode-walker expr) name expr)))
       (do-exprs
	(lambda (name exprs)
	  (and (pair? exprs)
	       (or (do-expr name (car exprs))
		   (do-exprs name (cdr exprs))))))
       (scode-walker
	(make-scode-walker
	 (lambda (name expr) name expr #f)
	 `((ACCESS
	    ,(lambda (name expr)
	       name
	       (if (access-environment expr)
		   (illegal expr)
		   #f)))
	   (ASSIGNMENT
	    ,(lambda (name expr)
	       (or (eq? name (assignment-name expr))
		   (do-expr name (assignment-value expr)))))
	   (COMBINATION
	    ,(lambda (name expr)
	       (or (do-expr name (combination-operator expr))
		   (do-exprs name (combination-operands expr)))))
	   (COMMENT
	    ,(lambda (name expr)
	       (do-expr name (comment-expression expr))))
	   (CONDITIONAL
	    ,(lambda (name expr)
	       (do-exprs name (conditional-components expr list))))
	   (DELAY
	    ,(lambda (name expr)
	       (do-expr name (delay-expression expr))))
	   (DISJUNCTION
	    ,(lambda (name expr)
	       (do-exprs name (disjunction-components expr list))))
	   (DEFINITION
	    ,(lambda (name expr)
	       (and (not (eq? name (definition-name expr)))
		    (do-expr name (definition-value expr)))))
	   (LAMBDA
	    ,(lambda (name expr)
	       (lambda-components expr
		 (lambda (lname required optional rest auxiliary decls body)
		   lname decls
		   (and (not (or (memq name required)
				 (memq name optional)
				 (eq? name rest)
				 (memq name auxiliary)))
			(do-expr name body))))))
	   (SEQUENCE
	    ,(lambda (name expr)
	       (do-exprs name (sequence-actions expr))))
	   (VARIABLE
	    ,(lambda (name expr)
	       (eq? name (variable-name expr)))))))
       (illegal (lambda (expr) (error "Illegal expression:" expr))))
    do-expr))

(define (parse-specialized-lambda-list bvl)
  (letrec
      ((parse-required
	(lambda (bvl required)
	  (cond ((null? bvl)
		 (finish required '() #f))
		((pair? bvl)
		 (cond ((eq? #!optional (car bvl))
			(parse-optional (cdr bvl) required '()))
		       ((eq? #!rest (car bvl))
			(parse-rest (cdr bvl) required '()))
		       ((or (identifier? (car bvl))
			    (and (pair? (car bvl))
				 (identifier? (caar bvl))
				 (pair? (cdar bvl))
				 (null? (cddar bvl))))
			(parse-required (cdr bvl)
					(cons (car bvl) required)))
		       (else
			(illegal-element bvl))))
		((identifier? bvl)
		 (finish required '() bvl))
		(else
		 (illegal-tail bvl)))))
       (parse-optional
	(lambda (bvl required optional)
	  (cond ((null? bvl)
		 (finish required optional #f))
		((pair? bvl)
		 (cond ((eq? #!optional (car bvl))
			(error "#!optional may not recur:" bvl))
		       ((eq? #!rest (car bvl))
			(parse-rest (cdr bvl) required optional))
		       ((identifier? (car bvl))
			(parse-optional (cdr bvl)
					required
					(cons (car bvl) optional)))
		       (else
			(illegal-element bvl))))
		((identifier? bvl)
		 (finish required optional bvl))
		(else
		 (illegal-tail bvl)))))
       (parse-rest
	(lambda (bvl required optional)
	  (if (and (pair? bvl)
		   (null? (cdr bvl)))
	      (if (identifier? (car bvl))
		  (finish required optional (car bvl))
		  (illegal-element bvl))
	      (illegal-tail bvl))))
       (finish
	(lambda (required optional rest)
	  (let ((required (reverse! required))
		(optional (reverse! optional)))
	    (do ((names (append required optional (if rest (list rest) '()))
			(cdr names)))
		((null? names))
	      (if (memq (car names) (cdr names))
		  (error "Lambda list has duplicate parameter:"
			 (car names)
			 (error-irritant/noise " in")
			 bvl)))
	    (call-with-values
		(lambda () (extract-required-specializers required))
	      (lambda (required specializers)
		(values required specializers optional rest))))))
       (illegal-tail
	(lambda (bvl)
	  (error "Illegal parameter list tail:" bvl)))
       (illegal-element
	(lambda (bvl)
	  (error "Illegal parameter list element:" (car bvl)))))
    (parse-required bvl '())))

(define (extract-required-specializers required)
  (let loop ((required required) (names '()) (specializers '()))
    (if (pair? required)
	(if (pair? (car required))
	    (loop (cdr required)
		  (cons (caar required) names)
		  (cons (cadar required) specializers))
	    (loop (cdr required)
		  (cons (car required) names)
		  (cons '<OBJECT> specializers)))
	(values (reverse! names)
		(reverse! (let loop ((specializers specializers))
			    (if (and (pair? specializers)
				     (eq? '<OBJECT> (car specializers))
				     (pair? (cdr specializers)))
				(loop (cdr specializers))
				specializers)))))))