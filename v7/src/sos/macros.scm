;;; -*-Scheme-*-
;;;
;;; $Id: macros.scm,v 1.2 1997/06/04 22:15:31 cph Exp $
;;;
;;; Copyright (c) 1993-97 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Macros

(declare (usual-integrations))

(define (transform:define-class name superclasses . slot-arguments)
  (let ((lose
	 (lambda (s a)
	   (serror 'DEFINE-CLASS (string-append "Malformed " s ":") a))))
    (call-with-values (lambda () (parse-define-class-name name lose))
      (lambda (name post-definitions)
	(if (not (list? superclasses))
	    (lose "superclasses" superclasses))
	(let ((pre-definitions
	       (extract-generic-definitions! slot-arguments name lose)))
	  `(BEGIN
	     ,@pre-definitions
	     (DEFINE ,name
	       (MAKE-CLASS ',name (LIST ,@superclasses)
		 (LIST
		  ,@(map
		     (lambda (arg)
		       (cond ((symbol? arg)
			      `',arg)
			     ((and (pair? arg)
				   (symbol? (car arg))
				   (list? (cdr arg)))
			      `(LIST ',(car arg)
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
	     ,@post-definitions))))))

(define (parse-define-class-name name lose)
  (cond ((symbol? name)
	 (values name
		 `((DEFINE ,(default-predicate-name name)
		     (INSTANCE-PREDICATE ,name)))))
	((and (pair? name)
	      (symbol? (car name))
	      (list? (cdr name)))
	 (values
	  (car name)
	  (append-map
	   (lambda (option)
	     (case (car option)
	       ((PREDICATE)
		(let ((pn
		       (cond ((null? (cdr option))
			      (default-predicate-name (car name)))
			     ((and (pair? (cdr option))
				   (or (symbol? (cadr option))
				       (false? (cadr option)))
				   (null? (cddr option)))
			      (cadr option))
			     (else (lose "class option" option)))))
		  (if pn
		      `((DEFINE ,pn (INSTANCE-PREDICATE ,(car name))))
		      '())))
	       ((CONSTRUCTOR)
		(cond ((and (pair? (cdr option))
			    (symbol? (cadr option))
			    (pair? (cddr option))
			    (and (list? (caddr option))
				 (for-all? (caddr option) symbol?))
			    (null? (cdddr option)))
		       `((DEFINE ,(cadr option)
			   (INSTANCE-CONSTRUCTOR ,(car name)
						 ',(caddr option)))))
		      ((and (pair? (cdr option))
			    (and (list? (cadr option))
				 (for-all? (cadr option) symbol?))
			    (null? (cddr option)))
		       `((DEFINE ,(default-constructor-name (car name))
			   (INSTANCE-CONSTRUCTOR ,(car name)
						 ',(cadr option)))))
		      (else
		       (lose "class option" option))))
	       (else (lose "class option" option))))
	   (map (lambda (option)
		  (if (pair? option)
		      option
		      (list option)))
		(cdr name)))))
	(else (lose "class name" name))))

(define (default-predicate-name class-name)
  (symbol-append (strip-angle-brackets class-name) '?))

(define (default-constructor-name class-name)
  (symbol-append 'make- (strip-angle-brackets class-name)))

(define (extract-generic-definitions! slot-arguments name lose)
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
							    arg)
				      definitions)))
		     (loop (cddr plist) (cdr plist)))))))
     slot-arguments)
    definitions))

(define (translate-define-arg arg name slot-argument)
  (let ((translate
	 (lambda (keyword standard? arity generate)
	   (if (or (and standard? (eq? 'STANDARD arg))
		   (eq? keyword arg)
		   (and (pair? arg) (memq keyword arg)))
	       `((DEFINE
		   ,(or (plist-lookup keyword (cdr slot-argument) #f)
			(let ((name
			       (generate
				(symbol-append (strip-angle-brackets name)
					       '-
					       (car slot-argument)))))
			  (set-cdr! slot-argument
				    (cons* keyword name (cdr slot-argument)))
			  name))
		   (MAKE-GENERIC-PROCEDURE ,arity)))
	       '()))))
    (append (translate 'ACCESSOR #t 1
		       (lambda (root) root))
	    (translate 'MODIFIER #t 2
		       (lambda (root) (symbol-append 'set- root '!)))
	    (translate 'INITPRED #f 1
		       (lambda (root) (symbol-append root '-initialized?))))))

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
	(string->symbol (substring s 1 (fix:- (string-length s) 1)))
	symbol)))

(define (transform:define-generic name lambda-list)
  (let ((mname 'DEFINE-GENERIC))
    (if (not (symbol? name))
	(serror mname "Malformed generic procedure name:" name))
    (call-with-values (lambda () (parse-lambda-list lambda-list #f mname))
      (lambda (required optional rest)
	`(DEFINE ,name
	   (MAKE-GENERIC-PROCEDURE
	    ',(let ((low (length required)))
		(cond (rest (cons low #f))
		      ((null? optional) low)
		      (else (cons low (+ low (length optional))))))
	    ',name))))))

(define (transform:define-method name lambda-list . body)
  (%transform:define-method name lambda-list body 'DEFINE-METHOD
			    generate-method-definition))

(define (transform:define-computed-method name lambda-list . body)
  (%transform:define-method name lambda-list body 'DEFINE-COMPUTED-METHOD
			    generate-computed-method-definition))

(define (%transform:define-method name lambda-list body mname generator)
  (if (not (symbol? name))
      (serror mname "Malformed generic procedure name:" name))
  (call-with-values (lambda () (parse-lambda-list lambda-list #t mname))
    (lambda (required optional rest)
      (call-with-values (lambda () (extract-required-specializers required))
	(lambda (required specializers)
	  (generator name required specializers optional rest body))))))

(define (generate-method-definition name required specializers optional rest
				    body)
  `(ADD-METHOD ,name
     ,(make-method-sexp name required optional rest specializers body)))

(define (generate-computed-method-definition name required specializers
					     optional rest body)
  `(ADD-METHOD ,name
     (MAKE-COMPUTED-METHOD (LIST ,@specializers)
       ,(make-named-lambda name required optional rest body))))

(define (transform:define-computed-emp name key lambda-list . body)
  (let ((mname 'DEFINE-COMPUTED-EMP))
    (if (not (symbol? name))
	(serror mname "Malformed generic procedure name:" name))
    (call-with-values (lambda () (parse-lambda-list lambda-list #t mname))
      (lambda (required optional rest)
	(call-with-values (lambda () (extract-required-specializers required))
	  (lambda (required specializers)
	    `(ADD-METHOD ,name
	       (MAKE-COMPUTED-EMP ,key (LIST ,@specializers)
		 ,(make-named-lambda name required optional rest body)))))))))

(define (transform:method lambda-list . body)
  (call-with-values (lambda () (parse-lambda-list lambda-list #t 'METHOD))
    (lambda (required optional rest)
      (call-with-values (lambda () (extract-required-specializers required))
	(lambda (required specializers)
	  (make-method-sexp #f required optional rest specializers body))))))

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
	       `(,(if used? 'MAKE-CHAINED-METHOD 'MAKE-METHOD)
		 (LIST ,@specializers)
		 ,(make-named-lambda name
				     (if used?
					 (cons 'CALL-NEXT-METHOD required)
					 required)
				     optional
				     rest
				     body)))))))
    (if (and (null? optional)
	     (not rest)
	     (not (eq? '<OBJECT> (car specializers))))
	(case (length required)
	  ((1)
	   (cond ((match `((SLOT-VALUE ,(car required) ',symbol?)) body)
		  `(SLOT-ACCESSOR-METHOD ,(car specializers) ,(caddar body)))
		 ((match `((SLOT-INITIALIZED? ,(car required) ',symbol?)) body)
		  `(SLOT-INITPRED-METHOD ,(car specializers) ,(caddar body)))
		 (else (normal))))
	  ((2)
	   (if (and (null? (cdr specializers))
		    (match `((SET-SLOT-VALUE! ,(car required)
					      ',symbol?
					      ,(cadr required)))
			   body))
	       `(SLOT-MODIFIER-METHOD ,(car specializers) ,(caddar body))
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
		(free-variable? 'CALL-NEXT-METHOD
				(syntax* body))))))

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
	   (DEFINITION ,(lambda (name expr) name (illegal expr)))
	   (IN-PACKAGE ,(lambda (name expr) name (illegal expr)))
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

(define (parse-lambda-list lambda-list allow-specializers? specform)
  specform
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

(define (serror procedure message . objects)
  procedure
  (apply error message objects))