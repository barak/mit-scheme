;;; -*-Scheme-*-
;;;
;;; $Id: macros.scm,v 1.12 2001/12/23 17:21:00 cph Exp $
;;;
;;; Copyright (c) 1993-2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Macros

(declare (usual-integrations))

(define-syntax define-class
  (non-hygienic-macro-transformer
   (lambda (name superclasses . slot-arguments)
     (let ((lose
	    (lambda (s a)
	      (error (string-append "Malformed " s ":") a))))
       (call-with-values (lambda () (parse-define-class-name name lose))
	 (lambda (name post-definitions separator)
	   (if (not (list? superclasses))
	       (lose "superclasses" superclasses))
	   (let ((pre-definitions
		  (extract-generic-definitions! slot-arguments name separator
						lose)))
	     `(BEGIN
		,@pre-definitions
		(DEFINE ,name
		  (,(make-absolute-reference 'MAKE-CLASS)
		   ',name
		   (,(make-absolute-reference 'LIST) ,@superclasses)
		   (,(make-absolute-reference 'LIST)
		    ,@(map
		       (lambda (arg)
			 (cond ((symbol? arg)
				`',arg)
			       ((and (pair? arg)
				     (symbol? (car arg))
				     (list? (cdr arg)))
				`(,(make-absolute-reference 'LIST)
				  ',(car arg)
				  ,@(let loop ((plist (cdr arg)))
				      (cond ((null? plist)
					     '())
					    ((and (symbol? (car plist))
						  (pair? (cdr plist)))
					     (cons* `',(car plist)
						    (cadr plist)
						    (loop (cddr plist))))
					    (else
					     (lose "slot argument" arg))))))
			       (else
				(lose "slot argument" arg))))
		       slot-arguments))))
		,@post-definitions))))))))

(define (parse-define-class-name name lose)
  (call-with-values (lambda () (parse-define-class-name-1 name lose))
    (lambda (class-name alist)
      (let ((post-definitions '())
	    (separator #f))
	(let ((alist
	       (if (assq 'PREDICATE alist)
		   alist
		   (cons '(PREDICATE) alist)))
	      (post-def
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
			     (else (lose "class option" option)))))
		  (if pn
		      (post-def
		       `(DEFINE ,pn
			  (,(make-absolute-reference 'INSTANCE-PREDICATE)
			   ,class-name))))))
	       ((CONSTRUCTOR)
		(call-with-values
		    (lambda ()
		      (parse-constructor-option class-name lose option))
		  (lambda (name slots ii-args)
		    (post-def
		     `(DEFINE ,name
			(,(make-absolute-reference 'INSTANCE-CONSTRUCTOR)
			 ,class-name
			 ',slots
			 ,@(map (lambda (x) `',x) ii-args)))))))
	       ((SEPARATOR)
		(if (or separator
			(null? (cdr option))
			(not (string? (cadr option)))
			(not (null? (cddr option))))
		    (lose "class option" option))
		(set! separator (cadr option))
		unspecific)
	       (else (lose "class option" option))))
	   alist))
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
  (cond ((match `(,symbol? ,list-of-symbols? . ,optional?) (cdr option))
	 (values (cadr option) (caddr option) (cdddr option)))
	((match `(,list-of-symbols? . ,optional?) (cdr option))
	 (values (default-constructor-name class-name)
		 (cadr option)
		 (cddr option)))
	(else
	 (lose "class option" option))))

(define (list-of-symbols? x)
  (list-of-type? x symbol?))

(define (optional? x)
  (or (null? x) (and (pair? x) (null? (cdr x)))))

(define (default-predicate-name class-name)
  (intern (string-append (strip-angle-brackets class-name) "?")))

(define (default-constructor-name class-name)
  (intern (string-append "make-" (strip-angle-brackets class-name))))

(define (make-named-lambda name required optional rest body)
  (let ((bvl
	 (append required
		 (if (null? optional)
		     '()
		     `(#!OPTIONAL ,@optional))
		 (or rest '()))))
    (if name
	`(NAMED-LAMBDA (,name ,@bvl) ,@body)
	`(LAMBDA ,bvl ,@body))))

(define (make-absolute-reference name)
  `(ACCESS ,name #F))

(define (extract-generic-definitions! slot-arguments name separator lose)
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
							    arg)
				      definitions)))
		     (loop (cddr plist) (cdr plist)))))))
     slot-arguments)
    definitions))

(define (translate-define-arg arg name separator slot-argument)
  (let ((translate
	 (lambda (keyword standard? arity generate)
	   (if (or (and standard? (eq? 'STANDARD arg))
		   (eq? keyword arg)
		   (and (pair? arg) (memq keyword arg)))
	       `((DEFINE
		   ,(or (plist-lookup keyword (cdr slot-argument) #f)
			(let ((name
			       (intern
				(generate
				 (string-append (strip-angle-brackets name)
						separator
						(symbol->string
						 (car slot-argument)))))))
			  (set-cdr! slot-argument
				    (cons* keyword name (cdr slot-argument)))
			  name))
		   (,(make-absolute-reference 'MAKE-GENERIC-PROCEDURE)
		    ,arity)))
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
  (non-hygienic-macro-transformer
   (lambda (name lambda-list)
     (if (not (symbol? name))
	 (error "Malformed generic procedure name:" name))
     (call-with-values (lambda () (parse-lambda-list lambda-list #f))
       (lambda (required optional rest)
	 `(DEFINE ,name
	    (,(make-absolute-reference 'MAKE-GENERIC-PROCEDURE)
	     ',(let ((low (length required)))
		 (cond (rest (cons low #f))
		       ((null? optional) low)
		       (else (cons low (+ low (length optional))))))
	     ',name)))))))

(define-syntax define-method
  (non-hygienic-macro-transformer
   (lambda (name lambda-list . body)
     (transform-define-method name lambda-list body
       (lambda (name required specializers optional rest body)
	 `(,(make-absolute-reference 'ADD-METHOD)
	   ,name
	   ,(make-method-sexp name required optional rest specializers
			      body)))))))

(define-syntax define-computed-method
  (non-hygienic-macro-transformer
   (lambda (name lambda-list . body)
     (transform-define-method name lambda-list body
       (lambda (name required specializers optional rest body)
	 `(,(make-absolute-reference 'ADD-METHOD)
	   ,name
	   (,(make-absolute-reference 'MAKE-COMPUTED-METHOD)
	    (,(make-absolute-reference 'LIST) ,@specializers)
	    ,(make-named-lambda name required optional rest body))))))))

(define (transform-define-method name lambda-list body generator)
  (if (not (symbol? name))
      (error "Malformed generic procedure name:" name))
  (call-with-values (lambda () (parse-lambda-list lambda-list #t))
    (lambda (required optional rest)
      (call-with-values (lambda () (extract-required-specializers required))
	(lambda (required specializers)
	  (generator name required specializers optional rest body))))))

(define-syntax define-computed-emp
  (non-hygienic-macro-transformer
   (lambda (name key lambda-list . body)
     (if (not (symbol? name))
	 (error "Malformed generic procedure name:" name))
     (call-with-values (lambda () (parse-lambda-list lambda-list #t))
       (lambda (required optional rest)
	 (call-with-values (lambda () (extract-required-specializers required))
	   (lambda (required specializers)
	     `(,(make-absolute-reference 'ADD-METHOD)
	       ,name
	       (,(make-absolute-reference 'MAKE-COMPUTED-EMP)
		,key
		(,(make-absolute-reference 'LIST) ,@specializers)
		,(make-named-lambda name required optional rest body))))))))))

(define-syntax method
  (non-hygienic-macro-transformer
   (lambda (lambda-list . body)
     (call-with-values (lambda () (parse-lambda-list lambda-list #t))
       (lambda (required optional rest)
	 (call-with-values (lambda () (extract-required-specializers required))
	   (lambda (required specializers)
	     (make-method-sexp #f required optional rest specializers
			       body))))))))

(define (extract-required-specializers required)
  (let loop ((required required) (names '()) (specializers '()))
    (cond ((null? required)
	   (values (reverse! names)
		   (reverse! (let loop ((specializers specializers))
			       (if (and (not (null? specializers))
					(eq? '<OBJECT> (car specializers))
					(not (null? (cdr specializers))))
				   (loop (cdr specializers))
				   specializers)))))
	  ((pair? (car required))
	   (loop (cdr required)
		 (cons (caar required) names)
		 (cons (cadar required) specializers)))
	  (else
	   (loop (cdr required)
		 (cons (car required) names)
		 (cons '<OBJECT> specializers))))))

(define (make-method-sexp name required optional rest specializers body)
  (let ((normal
	 (lambda ()
	   (call-with-values (lambda () (call-next-method-used? body))
	     (lambda (body used?)
	       (let ((s `(,(make-absolute-reference 'LIST) ,@specializers))
		     (l (make-named-lambda name required optional rest body)))
		 (if used?
		     `(,(make-absolute-reference 'MAKE-CHAINED-METHOD)
		       ,s
		       (LAMBDA (CALL-NEXT-METHOD) ,l))
		     `(,(make-absolute-reference 'MAKE-METHOD) ,s ,l))))))))
    (if (and (null? optional)
	     (not rest)
	     (not (eq? '<OBJECT> (car specializers))))
	(case (length required)
	  ((1)
	   (cond ((match `((SLOT-VALUE ,(car required) ',symbol?)) body)
		  `(,(make-absolute-reference 'SLOT-ACCESSOR-METHOD)
		    ,(car specializers)
		    ,(caddar body)))
		 ((match `((SLOT-INITIALIZED? ,(car required) ',symbol?)) body)
		  `(,(make-absolute-reference 'SLOT-INITPRED-METHOD)
		    ,(car specializers)
		    ,(caddar body)))
		 (else (normal))))
	  ((2)
	   (if (and (null? (cdr specializers))
		    (match `((SET-SLOT-VALUE! ,(car required)
					      ',symbol?
					      ,(cadr required)))
			   body))
	       `(,(make-absolute-reference 'SLOT-MODIFIER-METHOD)
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

(define (call-next-method-used? body)
  (if (null? body)
      (values body #f)
      (let ((body
	     (let loop ((body body))
	       (cond ((or (not (symbol? (car body)))
			  (null? (cdr body)))
		      body)
		     ((eq? (car body) 'CALL-NEXT-METHOD)
		      (loop (cdr body)))
		     (else
		      (cons (car body) (loop (cdr body))))))))
	(values body
		(free-variable? 'CALL-NEXT-METHOD (syntax* body))))))

(define free-variable?
  (letrec
      ((do-expr
	(lambda (name expr)
	  ((scode-walk scode-walker expr) name expr)))
       (do-exprs
	(lambda (name exprs)
	  (if (null? exprs)
	      '()
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

(define (parse-lambda-list lambda-list allow-specializers?)
  (let ((required '())
	(optional '())
	(rest #f))
    (letrec
	((parse-required
	  (lambda (lambda-list)
	    (cond ((null? lambda-list)
		   (finish))
		  ((pair? lambda-list)
		   (cond ((or (valid-name? (car lambda-list))
			      (and allow-specializers?
				   (pair? (car lambda-list))
				   (valid-name? (caar lambda-list))
				   (pair? (cdar lambda-list))
				   (null? (cddar lambda-list))))
			  (set! required (cons (car lambda-list) required))
			  (parse-required (cdr lambda-list)))
			 ((eq? #!optional (car lambda-list))
			  (parse-optional (cdr lambda-list)))
			 ((eq? #!rest (car lambda-list))
			  (parse-rest (cdr lambda-list)))
			 (else
			  (illegal-element lambda-list))))
		  ((symbol? lambda-list)
		   (set! rest lambda-list)
		   (finish))
		  (else
		   (illegal-tail lambda-list)))))
	 (parse-optional
	  (lambda (lambda-list)
	    (cond ((null? lambda-list)
		   (finish))
		  ((pair? lambda-list)
		   (cond ((valid-name? (car lambda-list))
			  (set! optional (cons (car lambda-list) optional))
			  (parse-optional (cdr lambda-list)))
			 ((eq? #!optional (car lambda-list))
			  (error "#!optional may not recur:" lambda-list))
			 ((eq? #!rest (car lambda-list))
			  (parse-rest (cdr lambda-list)))
			 (else
			  (illegal-element lambda-list))))
		  ((symbol? lambda-list)
		   (set! rest lambda-list)
		   (finish))
		  (else
		   (illegal-tail lambda-list)))))
	 (parse-rest
	  (lambda (lambda-list)
	    (if (and (pair? lambda-list)
		     (null? (cdr lambda-list)))
		(if (valid-name? (car lambda-list))
		    (begin
		      (set! rest (car lambda-list))
		      (finish))
		    (illegal-element lambda-list))
		(illegal-tail lambda-list))))
	 (valid-name?
	  (lambda (element)
	    (and (symbol? element)
		 (not (eq? #!optional element))
		 (not (eq? #!rest element)))))
	 (finish
	  (lambda ()
	    (values (reverse! required)
		    (reverse! optional)
		    rest)))
	 (illegal-tail
	  (lambda (lambda-list)
	    (error "Illegal parameter list tail:" lambda-list)))
	 (illegal-element
	  (lambda (lambda-list)
	    (error "Illegal parameter list element:" (car lambda-list)))))
      (parse-required lambda-list))))