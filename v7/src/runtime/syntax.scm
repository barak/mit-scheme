;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/syntax.scm,v 13.41 1987/01/23 00:21:11 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; SYNTAX: S-Expressions -> SCODE

(declare (usual-integrations))

(define lambda-tag:unnamed
  (make-named-tag "UNNAMED-PROCEDURE"))

(define *fluid-let-type* 'shallow)

(define lambda-tag:shallow-fluid-let
  (make-named-tag "SHALLOW-FLUID-LET-PROCEDURE"))

(define lambda-tag:deep-fluid-let
  (make-named-tag "DEEP-FLUID-LET-PROCEDURE"))

(define lambda-tag:common-lisp-fluid-let
  (make-named-tag "COMMON-LISP-FLUID-LET-PROCEDURE"))

(define lambda-tag:let
  (make-named-tag "LET-PROCEDURE"))

(define lambda-tag:make-environment
  (make-named-tag "MAKE-ENVIRONMENT-PROCEDURE"))

(define lambda-tag:make-package
  (make-named-tag "MAKE-PACKAGE-PROCEDURE"))

(define syntax)
(define syntax*)
(define macro-spreader)

(define enable-scan-defines!)
(define with-scan-defines-enabled)
(define disable-scan-defines!)
(define with-scan-defines-disabled)

;; Enable shallow vs fluid binding for FLUID-LET
(define shallow-fluid-let!)
(define deep-fluid-let!)
(define common-lisp-fluid-let!)

(define system-global-syntax-table)
(define syntax-table?)
(define make-syntax-table)
(define extend-syntax-table)
(define copy-syntax-table)
(define syntax-table-ref)
(define syntax-table-define)
(define syntax-table-shadow)
(define syntax-table-undefine)

(define syntaxer-package)
(let ((external-make-sequence make-sequence)
      (external-make-lambda make-lambda))
(set! syntaxer-package (the-environment))

;;;; Dispatch Point

(define (syntax-expression expression)
  (cond ((pair? expression)
	 (let ((quantum (syntax-table-ref syntax-table (car expression))))
	   (if quantum
	       (fluid-let ((saved-keyword (car expression)))
		 (quantum expression))
	       (make-combination (syntax-expression (car expression))
				 (syntax-expressions (cdr expression))))))
	((symbol? expression)
	 (make-variable expression))
	(else
	 expression)))

(define (syntax-expressions expressions)
  (if (null? expressions)
      '()
      (cons (syntax-expression (car expressions))
	    (syntax-expressions (cdr expressions)))))

(define ((spread-arguments kernel) expression)
  (apply kernel (cdr expression)))

(define saved-keyword
  (make-interned-symbol ""))

(define (syntax-error message . irritant)
  (error (string-append message
			": "
			(symbol->string saved-keyword)
			" SYNTAX")
	 (cond ((null? irritant) *the-non-printing-object*)
	       ((null? (cdr irritant)) (car irritant))
	       (else irritant))))

(define (syntax-sequence subexpressions)
  (if (null? subexpressions)
      (syntax-error "No subforms in sequence")
      (make-sequence (syntax-sequentially subexpressions))))

(define (syntax-sequentially expressions)
  (if (null? expressions)
      '()
      ;; force eval order.
      (let ((first (syntax-expression (car expressions))))
	(cons first
	      (syntax-sequentially (cdr expressions))))))

(define (syntax-bindings bindings receiver)
  (cond ((null? bindings)
	 (receiver '() '()))
	((and (pair? (car bindings))
	      (symbol? (caar bindings)))
	 (syntax-bindings (cdr bindings)
	   (lambda (names values)
	     (receiver (cons (caar bindings) names)
		       (cons (expand-binding-value (cdar bindings)) values)))))
	(else
	 (syntax-error "Badly-formed binding" (car bindings)))))

;;;; Expanders

(define (expand-access chain cont)
  (if (symbol? (car chain))
      (cont (if (null? (cddr chain))
		(syntax-expression (cadr chain))
		(expand-access (cdr chain) make-access))
	    (car chain))
      (syntax-error "Non-symbolic variable" (car chain))))

(define (expand-binding-value rest)
  (cond ((null? rest) unassigned-object)
	((null? (cdr rest)) (syntax-expression (car rest)))
	(else (syntax-error "Too many forms in value" rest))))

(define expand-conjunction
  (let ()
    (define (expander forms)
      (if (null? (cdr forms))
	  (syntax-expression (car forms))
	  (make-conjunction (syntax-expression (car forms))
			    (expander (cdr forms)))))
    (named-lambda (expand-conjunction forms)
      (if (null? forms)
	  true
	  (expander forms)))))

(define expand-disjunction
  (let ()
    (define (expander forms)
      (if (null? (cdr forms))
	  (syntax-expression (car forms))
	  (make-disjunction (syntax-expression (car forms))
			    (expander (cdr forms)))))
    (named-lambda (expand-disjunction forms)
      (if (null? forms)
	  false
	  (expander forms)))))

(define (expand-lambda pattern actions receiver)
  (define (loop pattern body)
    (if (pair? (car pattern))
	(loop (car pattern)
	      (make-lambda (cdr pattern) body))
	(receiver pattern body)))
  ((if (pair? pattern) loop receiver) pattern (syntax-lambda-body actions)))

(define (syntax-lambda-body body)
  (syntax-sequence
   (if (and (not (null? body))
	    (not (null? (cdr body)))
	    (string? (car body)))
       (cdr body)		;discard documentation string.
       body)))

;;;; Quasiquote

(define quasiquote-keyword 'QUASIQUOTE)
(define unquote-keyword 'UNQUOTE)
(define unquote-splicing-keyword 'UNQUOTE-SPLICING)

(define expand-quasiquote)
(let ()

(define (expand expression)
  (if (pair? expression)
      (cond ((eq? (car expression) unquote-keyword)
	     (cadr expression))
	    ((eq? (car expression) quasiquote-keyword)
	     (expand (expand (cadr expression))))
	    ((eq? (car expression) unquote-splicing-keyword)
	     (error "EXPAND-QUASIQUOTE: Misplaced ,@" expression))
	    ((and (pair? (car expression))
		  (eq? (caar expression) unquote-splicing-keyword))
	     (expand-spread (cadr (car expression))
			    (expand (cdr expression))))
	    (else
	     (expand-pair (expand (car expression))
			  (expand (cdr expression)))))
      (list 'QUOTE expression)))

(define (expand-pair a d)
  (cond ((pair? d)
	 (cond ((eq? (car d) 'QUOTE)
		(cond ((and (pair? a) (eq? (car a) 'QUOTE))
		       (list 'QUOTE (cons (cadr a) (cadr d))))
		      ((list? (cadr d))
		       (cons* 'LIST
			      a
			      (map (lambda (element)
				     (list 'QUOTE element))
				   (cadr d))))
		      (else
		       (list 'CONS a d))))
	       ((eq? (car d) 'CONS)
		(cons* 'CONS* a (cdr d)))
	       ((memq (car d) '(LIST CONS*))
		(cons* (car d) a (cdr d)))
	       (else
		(list 'CONS a d))))
	(else
	 (list 'CONS a d))))

(define (expand-spread a d)
  (cond ((pair? d)
	 (cond ((eq? (car d) 'QUOTE)
		(cond ((and (pair? a) (eq? (car a) 'QUOTE))
		       (list 'QUOTE (append (cadr a) (cadr d))))
		      ((null? (cadr d))
		       a)
		      (else
		       (list 'APPEND a d))))
	       ((eq? (car d) 'APPEND)
		(cons* (car d) a (cdr d)))
	       (else
		(list 'APPEND a d))))
	(else
	 (list 'APPEND a d))))

(set! expand-quasiquote
(named-lambda (expand-quasiquote expression)
  (syntax-expression (expand expression))))

)

;;;; Basic Syntax

(define syntax-SCODE-QUOTE-form
  (spread-arguments
   (lambda (expression)
     (make-quotation (syntax-expression expression)))))

(define syntax-QUOTE-form
  (spread-arguments identity-procedure))

(define syntax-THE-ENVIRONMENT-form
  (spread-arguments make-the-environment))

(define syntax-UNASSIGNED?-form
  (spread-arguments make-unassigned?))

(define syntax-UNBOUND?-form
  (spread-arguments make-unbound?))

(define syntax-ACCESS-form
  (spread-arguments
   (lambda chain
     (expand-access chain make-access))))

(define syntax-SET!-form
  (spread-arguments
   (lambda (name . rest)
     ((syntax-extended-assignment name)
      (expand-binding-value rest)))))

(define syntax-DEFINE-form
  (spread-arguments
   (lambda (pattern . rest)
     (cond ((symbol? pattern)
	    (make-definition pattern
			     (expand-binding-value
			      (if (and (= (length rest) 2)
				       (string? (cadr rest)))
				  (list (car rest))
				  rest))))
	   ((pair? pattern)
	    (expand-lambda pattern rest
	      (lambda (pattern body)
		(make-definition (car pattern)
				 (make-named-lambda (car pattern) (cdr pattern)
						    body)))))
	   (else
	    (syntax-error "Bad pattern" pattern))))))

(define syntax-SEQUENCE-form
  (spread-arguments
   (lambda actions
     (syntax-sequence actions))))

(define syntax-IN-PACKAGE-form
  (spread-arguments
   (lambda (environment . body)
     (make-in-package (syntax-expression environment)
		      (syntax-sequence body)))))

(define syntax-DELAY-form
  (spread-arguments
   (lambda (expression)
     (make-delay (syntax-expression expression)))))

(define syntax-CONS-STREAM-form
  (spread-arguments
   (lambda (head tail)
     (make-combination* cons
			(syntax-expression head)
			(make-delay (syntax-expression tail))))))

;;;; Conditionals

(define syntax-IF-form
  (spread-arguments
   (lambda (predicate consequent . rest)
     (make-conditional (syntax-expression predicate)
		       (syntax-expression consequent)
		       (cond ((null? rest)
			      false)
			     ((null? (cdr rest))
			      (syntax-expression (car rest)))
			     (else
			      (syntax-error "Too many forms" (cdr rest))))))))

(define syntax-COND-form
  (let ()
    (define (process-cond-clauses clause rest)
      (cond ((eq? (car clause) 'ELSE)
	     (if (null? rest)
		 (syntax-sequence (cdr clause))
		 (syntax-error "ELSE not last clause" rest)))
	    ((null? rest)
	     (if (cdr clause)
		 (make-conjunction (syntax-expression (car clause))
				   (syntax-sequence (cdr clause)))
		 (syntax-expression (car clause))))
	    ((null? (cdr clause))
	     (make-disjunction (syntax-expression (car clause))
			       (process-cond-clauses (car rest)
						     (cdr rest))))
	    ((and (pair? (cdr clause))
		  (eq? (cadr clause) '=>))
	     (syntax-expression
	      `((ACCESS COND-=>-HELPER SYNTAXER-PACKAGE '())
		,(car clause)
		(DELAY ,@(cddr clause))
		(DELAY (COND ,@rest)))))
	    (else
	     (make-conditional (syntax-expression (car clause))
			       (syntax-sequence (cdr clause))
			       (process-cond-clauses (car rest)
						     (cdr rest))))))
    (spread-arguments
     (lambda (clause . rest)
       (process-cond-clauses clause rest)))))

(define (cond-=>-helper form1-result thunk2 thunk3)
  (if form1-result
      ((force thunk2) form1-result)
      (force thunk3)))

(define (make-funcall name . args)
  (make-combination (make-variable name) args))

(define syntax-CONJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-conjunction forms))))

(define syntax-DISJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-disjunction forms))))

;;;; Procedures

(define syntax-LAMBDA-form
  (spread-arguments
   (lambda (pattern . body)
     (make-lambda pattern (syntax-lambda-body body)))))

(define syntax-NAMED-LAMBDA-form
  (spread-arguments
   (lambda (pattern . body)
     (expand-lambda pattern body
       (lambda (pattern body)
	 (make-named-lambda (car pattern) (cdr pattern) body))))))

(define syntax-LET-form
  (spread-arguments
   (lambda (name-or-pattern pattern-or-first . rest)
     (if (symbol? name-or-pattern)
	 (syntax-bindings pattern-or-first
	   (lambda (names values)
	     (make-combination (make-named-lambda name-or-pattern names
						  (syntax-sequence rest))
			       values)))
	 (syntax-bindings name-or-pattern
	   (lambda (names values)
	     (make-closed-block
	      lambda-tag:let names values
	      (syntax-sequence (cons pattern-or-first rest)))))))))

(define syntax-MAKE-PACKAGE-form
  (spread-arguments
   (lambda (name bindings . body)
     (if (symbol? name)
	 (syntax-bindings bindings
	   (lambda (names values)
	     (make-closed-block
	      lambda-tag:make-package
	      (cons name names)
	      (cons unassigned-object values)
	      (make-sequence* (make-assignment name the-environment-object)
			      (if (null? body)
				  the-environment-object
				  (make-sequence* (syntax-sequence body)
						  the-environment-object))))))
	 (syntax-error "Bad package name" name)))))

(define syntax-MAKE-ENVIRONMENT-form
  (spread-arguments
   (lambda body
     (make-closed-block
      lambda-tag:make-environment '() '()
      (if (null? body)
	  the-environment-object
	  (make-sequence* (syntax-sequence body) the-environment-object))))))

;;;; Syntax Extensions

(define syntax-LET-SYNTAX-form
  (spread-arguments
   (lambda (bindings . body)
     (syntax-bindings bindings
       (lambda (names values)
	 (fluid-let ((syntax-table
		      (extend-syntax-table
		       (map (lambda (name value)
			      (cons name (syntax-eval value)))
			    names
			    values)
		       syntax-table)))
	   (syntax-sequence body)))))))

(define syntax-USING-SYNTAX-form
  (spread-arguments
   (lambda (table . body)
     (let ((table* (syntax-eval (syntax-expression table))))
       (if (not (syntax-table? table*))
	   (syntax-error "Not a syntax table" table))
       (fluid-let ((syntax-table table*))
	 (syntax-sequence body))))))

(define syntax-DEFINE-SYNTAX-form
  (spread-arguments
   (lambda (name value)
     (cond ((symbol? name)
	    (syntax-table-define syntax-table name
	      (syntax-eval (syntax-expression value)))
	    name)
	   ((and (pair? name) (symbol? (car name)))
	    (syntax-table-define syntax-table (car name)
	      (let ((transformer
		     (syntax-eval (syntax-NAMED-LAMBDA-form
				   `(NAMED-LAMBDA ,name ,value)))))
		(lambda (expression)
		  (apply transformer (cdr expression)))))
	    (car name))
	   (else (syntax-error "Bad syntax description" name))))))

(define (syntax-MACRO-form expression)
  (make-combination* (expand-access '(MACRO-SPREADER '()) make-access)
		     (syntax-LAMBDA-form expression)))

(define (syntax-DEFINE-MACRO-form expression)
  (syntax-table-define syntax-table (caadr expression)
    (macro-spreader (syntax-eval (syntax-NAMED-LAMBDA-form expression))))
  (caadr expression))

(set! macro-spreader
(named-lambda ((macro-spreader transformer) expression)
  (syntax-expression (apply transformer (cdr expression)))))

;;;; Grab Bag

(define (syntax-ERROR-LIKE-form procedure-name)
  (spread-arguments
   (lambda (message . rest)
     (make-combination* (make-variable procedure-name)
			(syntax-expression message)
			(cond ((null? rest)
			       ;; Slightly crockish, but prevents
			       ;; hidden variable reference.
			       (make-access (make-null)
					    '*THE-NON-PRINTING-OBJECT*))
			      ((null? (cdr rest))
			       (syntax-expression (car rest)))
			      (else
			       (make-combination
				(make-access (make-null) 'LIST)
				(syntax-expressions rest))))
			(make-the-environment)))))

(define syntax-ERROR-form
  (syntax-ERROR-LIKE-form 'ERROR-PROCEDURE))

(define syntax-BKPT-form
  (syntax-ERROR-LIKE-form 'BREAKPOINT-PROCEDURE))

(define syntax-QUASIQUOTE-form
  (spread-arguments expand-quasiquote))

;;;; FLUID-LET

(define syntax-FLUID-LET-form-shallow
  (spread-arguments
   (lambda (bindings . body)
     (define (syntax-fluid-bindings bindings receiver)
       (if (null? bindings)
	   (receiver '() '() '() '())
	   (syntax-fluid-bindings
	    (cdr bindings)
	    (syntax-fluid-binding (car bindings) receiver))))

     (define (syntax-fluid-binding binding receiver)
       (if (pair? binding)
	   (let ((transfer 
		  (let ((assignment (syntax-extended-assignment (car binding))))
		    (lambda (target source)
		      (make-assignment
		       target
		       (assignment
			(make-assignment source unassigned-object))))))
		 (value (expand-binding-value (cdr binding)))
		 (inside-name (string->uninterned-symbol "INSIDE-PLACEHOLDER"))
		 (outside-name (string->uninterned-symbol "OUTSIDE-PLACEHOLDER")))
	     (lambda (names values transfers-in transfers-out)
	       (receiver (cons* inside-name outside-name names)
			 (cons* value unassigned-object values)
			 (cons (transfer outside-name inside-name) transfers-in)
			 (cons (transfer inside-name outside-name) transfers-out))))
	   (syntax-error "Binding not a list" binding)))
     
     (if (null? bindings)
	 (syntax-sequence body)
	 (syntax-fluid-bindings bindings
           (lambda (names values transfers-in transfers-out)
	     (make-closed-block
	      lambda-tag:shallow-fluid-let names values
	      (make-combination*
	       (make-variable 'DYNAMIC-WIND)
	       (make-thunk (make-sequence transfers-in))
	       (make-thunk (syntax-sequence body))
	       (make-thunk (make-sequence transfers-out))))))))))

(define (make-fluid-let-like prim procedure-tag)
  (define (syntax-fluid-bindings bindings receiver)
    (if (null? bindings)
	(receiver '() '())
	(syntax-fluid-bindings
	 (cdr bindings)
	 (syntax-fluid-binding (car bindings) receiver))))

  (define (syntax-fluid-binding binding receiver)
    (if (pair? binding)
	(let ((value (expand-binding-value (cdr binding)))
	      (var-or-access (syntax-fluid-let-name (car binding))))
	  (lambda (names values)
	    (receiver (cons var-or-access names)
		      (cons value values))))
	(syntax-error "Binding not a list" binding)))

  (define (syntax-fluid-let-name name)
    (let ((syntaxed (syntax-expression name)))
      (if (or (variable? syntaxed) (access? syntaxed))
	  syntaxed
	  (syntax-error "binding name illegal"))))
  
  (let ((with-saved-fluid-bindings
	 (make-primitive-procedure 'with-saved-fluid-bindings)))
    (spread-arguments
     (lambda (bindings . body)
       (syntax-fluid-bindings bindings
         (lambda (names values)
	   (define (accum-assignments names values)
	     (mapcar make-fluid-assign names values))
	   (define (make-fluid-assign name-or-access value)
	     (cond ((variable? name-or-access)
		    (make-combination
 		     prim
		     `(,the-environment-object
		       ,(make-quotation name-or-access)
		       ,value)))
		   ((access? name-or-access)
		    (access-components
		     name-or-access
		     (lambda (env name)
		       (make-combination
			prim
			`(,env ,name ,value)))))
		   (else
		    (syntax-error
		     "Target of FLUID-LET not a symbol or ACCESS form"
		     name-or-access))))
	   (make-combination
	    (internal-make-lambda procedure-tag '() '() '()
  	     (make-combination
	      with-saved-fluid-bindings
	      (list
	       (make-thunk
		(make-sequence 
		 (append (accum-assignments names values)
			 (list (syntax-sequence body))))))))
            '())))))))
	
(define syntax-FLUID-LET-form-deep
  ;; (FLUID-LET <bvl> . <body>) =>
  ;;    (WITH-SAVED-FLUID-BINDINGS
  ;;      (lambda ()
  ;;        (ADD-FLUID! (the-environment) <access-or-symbol> <value>)
  ;;        ...
  ;;        <fluid-let-body>))
  (let ((add-fluid-binding! 	
	 (make-primitive-procedure 'add-fluid-binding!)))
    (make-fluid-let-like add-fluid-binding! lambda-tag:deep-fluid-let)))

(define syntax-FLUID-LET-form-common-lisp
  ;; This -- groan -- is for Common Lisp support
  ;; (FLUID-BIND <bvl> . <body>) =>
  ;;    (WITH-SAVED-FLUID-BINDINGS
  ;;      (lambda ()
  ;;        (ADD-FLUID! (the-environment) <access-or-symbol> <value>)
  ;;        ...
  ;;        <fluid-let-body>))
  (let ((make-fluid-binding! 	
	 (make-primitive-procedure 'make-fluid-binding!)))
    (make-fluid-let-like make-fluid-binding! lambda-tag:common-lisp-fluid-let)))

;;;; Extended Assignment Syntax

(define (syntax-extended-assignment expression)
  (invert-expression (syntax-expression expression)))

(define (invert-expression target)
  (cond ((variable? target)
	 (invert-variable (variable-name target)))
	((access? target)
	 (access-components target invert-access))
	(else
	 (syntax-error "Bad target" target))))

(define ((invert-variable name) value)
  (make-assignment name value))

(define ((invert-access environment name) value)
  (make-combination* lexical-assignment environment name value))

;;;; Declarations

;;; All declarations are syntactically checked; the resulting
;;; DECLARATION objects all contain lists of standard declarations.
;;; Each standard declaration is a proper list with symbolic keyword.

(define syntax-LOCAL-DECLARE-form
  (spread-arguments
   (lambda (declarations . body)
     (make-declaration (process-declarations declarations)
		       (syntax-sequence body)))))

(define syntax-DECLARE-form
  (spread-arguments
   (lambda declarations
     (make-block-declaration (map process-declaration declarations)))))

(define (process-declarations declarations)
  (if (list? declarations)
      (map process-declaration declarations)
      (syntax-error "Illegal declaration list" declarations)))

(define (process-declaration declaration)
  (cond ((symbol? declaration)
	 (list declaration))
	((and (list? declaration)
	      (not (null? declaration))
	      (symbol? (car declaration)))
	 declaration)
	(else
	 (syntax-error "Illegal declaration" declaration))))

;;;; SCODE Constructors

(define unassigned-object
 (make-unassigned-object))

(define the-environment-object
  (make-the-environment))

(define (make-conjunction first second)
  (make-conditional first second false))

(define (make-combination* operator . operands)
  (make-combination operator operands))

(define (make-sequence* . operands)
  (make-sequence operands))

(define (make-sequence operands)
  (internal-make-sequence operands))

(define (make-thunk body)
  (make-lambda '() body))

(define (make-lambda pattern body)
  (make-named-lambda lambda-tag:unnamed pattern body))

(define (make-named-lambda name pattern body)
  (if (not (symbol? name))
      (syntax-error "Name of lambda expression must be a symbol" name))
  (parse-lambda-list pattern
    (lambda (required optional rest)
      (internal-make-lambda name required optional rest body))))

(define (make-closed-block tag names values body)
  (make-combination (internal-make-lambda tag names '() '() body)
		    values))

;;;; Lambda List Parser

(define (parse-lambda-list lambda-list receiver)
  (let ((required (list '()))
	(optional (list '())))
    (define (parse-parameters cell)
      (define (loop pattern)
	(cond ((null? pattern) (finish false))
	      ((symbol? pattern) (finish pattern))
	      ((not (pair? pattern)) (bad-lambda-list pattern))
	      ((eq? (car pattern) (access lambda-rest-tag lambda-package))
	       (if (and (pair? (cdr pattern)) (null? (cddr pattern)))
		   (cond ((symbol? (cadr pattern)) (finish (cadr pattern)))
			 ((and (pair? (cadr pattern))
			       (symbol? (caadr pattern)))
			  (finish (caadr pattern)))
			 (else (bad-lambda-list (cdr pattern))))
		   (bad-lambda-list (cdr pattern))))
	      ((eq? (car pattern) (access lambda-optional-tag lambda-package))
	       (if (eq? cell required)
		   ((parse-parameters optional) (cdr pattern))
		   (bad-lambda-list pattern)))
	      ((symbol? (car pattern))
	       (set-car! cell (cons (car pattern) (car cell)))
	       (loop (cdr pattern)))
	      ((and (pair? (car pattern)) (symbol? (caar pattern)))
	       (set-car! cell (cons (caar pattern) (car cell)))
	       (loop (cdr pattern)))
	      (else (bad-lambda-list pattern))))
      loop)

    (define (finish rest)
      (receiver (reverse! (car required))
		(reverse! (car optional))
		rest))

    (define (bad-lambda-list pattern)
      (syntax-error "Illegally-formed lambda-list" pattern))

    ((parse-parameters required) lambda-list)))

;;;; Scan Defines

(define no-scan-make-sequence
  external-make-sequence)

(define (scanning-make-sequence actions)
  (scan-defines (external-make-sequence actions)
    make-open-block))

(define (no-scan-make-lambda name required optional rest body)
  (external-make-lambda name required optional rest '() '() body))

(define scanning-make-lambda
  make-lambda*)

(define internal-make-sequence)
(define internal-make-lambda)

(set! enable-scan-defines!
(named-lambda (enable-scan-defines!)
  (set! internal-make-sequence scanning-make-sequence)
  (set! internal-make-lambda scanning-make-lambda)))

(set! with-scan-defines-enabled
(named-lambda (with-scan-defines-enabled thunk)
  (fluid-let ((internal-make-sequence scanning-make-sequence)
	      (internal-make-lambda scanning-make-lambda))
    (thunk))))

(set! disable-scan-defines!
(named-lambda (disable-scan-defines!)
  (set! internal-make-sequence no-scan-make-sequence)
  (set! internal-make-lambda no-scan-make-lambda)))

(set! with-scan-defines-disabled
(named-lambda (with-scan-defines-disabled thunk)
  (fluid-let ((internal-make-sequence no-scan-make-sequence)
	      (internal-make-lambda no-scan-make-lambda))
    (thunk))))

(define ((fluid-let-maker marker which-kind) #!optional name)
  (if (unassigned? name) (set! name 'FLUID-LET))
  (if (eq? name 'FLUID-LET) (set! *fluid-let-type* marker))
  (add-syntax! name which-kind))
  
(set! shallow-fluid-let!
      (fluid-let-maker 'shallow syntax-fluid-let-form-shallow))
(set! deep-fluid-let!
      (fluid-let-maker 'deep syntax-fluid-let-form-deep))
(set! common-lisp-fluid-let!
      (fluid-let-maker 'common-lisp syntax-fluid-let-form-common-lisp))

;;;; Top Level Syntaxers

(define syntax-table)

(define syntax-environment
  (in-package system-global-environment
    (make-environment)))

;;; The top level procedures, when not given an argument, use whatever
;;; the current syntax table is.  This is reasonable only while inside
;;; a syntaxer quantum, since at other times there is current table.

(define ((make-syntax-top-level syntaxer) expression #!optional table)
  (if (unassigned? table)
      (syntaxer expression)
      (begin (check-syntax-table table 'SYNTAX)
	     (fluid-let ((syntax-table table))
	       (syntaxer expression)))))

(set! syntax (make-syntax-top-level syntax-expression))
(set! syntax* (make-syntax-top-level syntax-sequence))

(define (syntax-eval scode)
  (scode-eval scode syntax-environment))

;;;; Syntax Table

(define syntax-table-tag
  '(SYNTAX-TABLE))

(set! syntax-table?
(named-lambda (syntax-table? object)
  (and (pair? object)
       (eq? (car object) syntax-table-tag))))

(define (check-syntax-table table name)
  (if (not (syntax-table? table))
      (error "Not a syntax table" name table)))

(set! make-syntax-table
(named-lambda (make-syntax-table #!optional parent)
  (cons syntax-table-tag
	(cons '()
	      (if (unassigned? parent)
		  '()
		  (cdr parent))))))

(set! extend-syntax-table
(named-lambda (extend-syntax-table alist #!optional table)
  (if (unassigned? table) (set! table (current-syntax-table)))
  (check-syntax-table table 'EXTEND-SYNTAX-TABLE)
  (cons syntax-table-tag (cons alist (cdr table)))))

(set! copy-syntax-table
(named-lambda (copy-syntax-table #!optional table)
  (if (unassigned? table) (set! table (current-syntax-table)))
  (check-syntax-table table 'COPY-SYNTAX-TABLE)
  (cons syntax-table-tag
	(map (lambda (alist)
	       (map (lambda (pair)
		      (cons (car pair) (cdr pair)))
		    alist))
	     (cdr table)))))

(set! syntax-table-ref
(named-lambda (syntax-table-ref table name)
  (define (loop frames)
    (and (not (null? frames))
	 (let ((entry (assq name (car frames))))
	   (if entry
	       (cdr entry)
	       (loop (cdr frames))))))
  (check-syntax-table table 'SYNTAX-TABLE-REF)
  (loop (cdr table))))

(set! syntax-table-define
(named-lambda (syntax-table-define table name quantum)
  (check-syntax-table table 'SYNTAX-TABLE-DEFINE)
  (let ((entry (assq name (cadr table))))
    (if entry
	(set-cdr! entry quantum)
	(set-car! (cdr table)
		  (cons (cons name quantum)
			(cadr table)))))))

(set! syntax-table-shadow
(named-lambda (syntax-table-shadow table name)
  (check-syntax-table table 'SYNTAX-TABLE-SHADOW)
  (let ((entry (assq name (cadr table))))
    (if entry
	(set-cdr! entry false)
	(set-car! (cdr table)
		  (cons (cons name false)
			(cadr table)))))))

(set! syntax-table-undefine
(named-lambda (syntax-table-undefine table name)
  (check-syntax-table table 'SYNTAX-TABLE-UNDEFINE)
  (if (assq name (cadr table))
      (set-car! (cdr table) 
		(del-assq! name (cadr table))))))

;;;; Default Syntax

(enable-scan-defines!)

(set! system-global-syntax-table
      (cons syntax-table-tag
	    `(((ACCESS           . ,syntax-ACCESS-form)
	       (AND              . ,syntax-CONJUNCTION-form)
	       (BEGIN            . ,syntax-SEQUENCE-form)
	       (BKPT             . ,syntax-BKPT-form)
	       (COND             . ,syntax-COND-form)
	       (CONS-STREAM      . ,syntax-CONS-STREAM-form)
	       (DECLARE          . ,syntax-DECLARE-form)
	       (DEFINE           . ,syntax-DEFINE-form)
	       (DEFINE-SYNTAX    . ,syntax-DEFINE-SYNTAX-form)
	       (DEFINE-MACRO     . ,syntax-DEFINE-MACRO-form)
	       (DELAY            . ,syntax-DELAY-form)
	       (ERROR            . ,syntax-ERROR-form)
	       (FLUID-LET        . ,syntax-FLUID-LET-form-shallow)
	       (IF               . ,syntax-IF-form)
	       (IN-PACKAGE       . ,syntax-IN-PACKAGE-form)
	       (LAMBDA           . ,syntax-LAMBDA-form)
	       (LET              . ,syntax-LET-form)
	       (LET-SYNTAX       . ,syntax-LET-SYNTAX-form)
	       (LOCAL-DECLARE    . ,syntax-LOCAL-DECLARE-form)
	       (MACRO            . ,syntax-MACRO-form)
	       (MAKE-ENVIRONMENT . ,syntax-MAKE-ENVIRONMENT-form)
	       (MAKE-PACKAGE     . ,syntax-MAKE-PACKAGE-form)
	       (NAMED-LAMBDA     . ,syntax-NAMED-LAMBDA-form)
	       (OR               . ,syntax-DISJUNCTION-form)
	       ;; The funniness here prevents QUASIQUOTE from being
	       ;; seen as a nested backquote.
	       (,'QUASIQUOTE       . ,syntax-QUASIQUOTE-form)
	       (QUOTE            . ,syntax-QUOTE-form)
	       (SCODE-QUOTE      . ,syntax-SCODE-QUOTE-form)
	       (SEQUENCE         . ,syntax-SEQUENCE-form)
	       (SET!             . ,syntax-SET!-form)
	       (THE-ENVIRONMENT  . ,syntax-THE-ENVIRONMENT-form)
	       (UNASSIGNED?      . ,syntax-UNASSIGNED?-form)
	       (UNBOUND?         . ,syntax-UNBOUND?-form)
	       (USING-SYNTAX     . ,syntax-USING-SYNTAX-form)
	       ))))

;;; end SYNTAXER-PACKAGE
)

;;; Edwin Variables:
;;; Scheme Environment: syntaxer-package
;;; End:

)