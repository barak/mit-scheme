;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/syntax.scm,v 13.47 1987/05/29 16:49:07 cph Exp $
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
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; SYNTAX: S-Expressions -> SCODE

(declare (usual-integrations))

(define lambda-tag:unnamed
  (make-named-tag "UNNAMED-PROCEDURE"))

(define *fluid-let-type*
  'SHALLOW)

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

(define expand-quasiquote)
(let ()

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
		       (return (system 'APPEND)
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
  (case (car x)
    ((QUASIQUOTE) (descend-quasiquote-pair* (1+ level)))
    ((UNQUOTE UNQUOTE-SPLICING)
     (if (zero? level)
	 (return (car x) (cadr x))
	 (descend-quasiquote-pair* (- level 1))))
    (else (descend-quasiquote-pair* level))))

(define (descend-quasiquote-vector x level return)
  (descend-quasiquote (vector->list x) level
    (lambda (mode arg)
      (case mode
	((QUOTE)
	 (return 'QUOTE x))
	((LIST)
	 (return (system 'VECTOR) arg))
	(else
	 (return (system 'LIST->VECTOR)
		 (list (finalize-quasiquote mode arg))))))))

(define (finalize-quasiquote mode arg)
  (case mode
    ((QUOTE) `',arg)
    ((UNQUOTE) arg)
    ((UNQUOTE-SPLICING) (error ",@ in illegal context" arg))
    ((LIST) `(,(system 'LIST) ,@arg))
    ((CONS)
     (if (= (length arg) 2)
	 `(,(system 'CONS) ,@arg)
	 `(,(system 'CONS*) ,@arg)))
    (else `(,mode ,@arg))))

(define (system name)
  `(ACCESS ,name #F))

(set! expand-quasiquote
  (named-lambda (expand-quasiquote expression)
    (syntax-expression (descend-quasiquote expression 0 finalize-quasiquote))))

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
     ((invert-expression (syntax-expression name))
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
		       (cond ((null? rest) undefined-conditional-branch)
			     ((null? (cdr rest))
			      (syntax-expression (car rest)))
			     (else
			      (syntax-error "Too many forms" (cdr rest))))))))

(define syntax-CONJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-conjunction forms))))

(define syntax-DISJUNCTION-form
  (spread-arguments
   (lambda forms
     (expand-disjunction forms))))

(define syntax-COND-form
  (let ()
    (define (process-cond-clauses clause rest)
      (cond ((eq? (car clause) 'ELSE)
	     (if (null? rest)
		 (syntax-sequence (cdr clause))
		 (syntax-error "ELSE not last clause" rest)))
	    ((null? (cdr clause))
	     (make-disjunction (syntax-expression (car clause))
			       (if (null? rest)
				   undefined-conditional-branch
				   (process-cond-clauses (car rest)
							 (cdr rest)))))
	    ((and (pair? (cdr clause))
		  (eq? (cadr clause) '=>))
	     (syntax-expression
	      `((ACCESS COND-=>-HELPER SYNTAXER-PACKAGE '())
		,(car clause)
		(LAMBDA () ,@(cddr clause))
		(LAMBDA ()
		  ,(if (null? rest)
		       undefined-conditional-branch
		       `(COND ,@rest))))))
	    (else
	     (make-conditional (syntax-expression (car clause))
			       (syntax-sequence (cdr clause))
			       (if (null? rest)
				   undefined-conditional-branch
				   (process-cond-clauses (car rest)
							 (cdr rest)))))))
    (spread-arguments
     (lambda (clause . rest)
       (process-cond-clauses clause rest)))))

(define (cond-=>-helper form1-result thunk2 thunk3)
  (if form1-result
      ((thunk2) form1-result)
      (thunk3)))

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
	 (if (pair? pattern)
	     (make-named-lambda (car pattern) (cdr pattern) body)
	     (syntax-error "Illegal named-lambda list" pattern))))))

(define syntax-LET-form
  (spread-arguments
   (lambda (name-or-pattern pattern-or-first . rest)
     (if (symbol? name-or-pattern)
	 (syntax-bindings pattern-or-first
	   (lambda (names values)
	     (make-letrec (list name-or-pattern)
			  (list (make-named-lambda name-or-pattern names
						   (syntax-sequence rest)))
			  (make-combination (make-variable name-or-pattern)
					    values))))
	 (syntax-bindings name-or-pattern
	   (lambda (names values)
	     (make-closed-block
	      lambda-tag:let names values
	      (syntax-sequence (cons pattern-or-first rest)))))))))

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
  (let ()

    (define (syntax-fluid-bindings bindings receiver)
      (if (null? bindings)
	  (receiver '() '() '() '())
	  (syntax-fluid-bindings (cdr bindings)
	    (lambda (names values transfers-in transfers-out)
	      (let ((binding (car bindings)))
		(if (pair? binding)
		    (let ((transfer
			   (let ((reference (syntax-expression (car binding))))
			     (let ((assignment (invert-expression reference)))
			       (lambda (target source)
				 (make-assignment
				  target
				  (assignment
				   (make-assignment source
						    unassigned-object)))))))
			  (value (expand-binding-value (cdr binding)))
			  (inside-name
			   (string->uninterned-symbol "INSIDE-PLACEHOLDER"))
			  (outside-name
			   (string->uninterned-symbol "OUTSIDE-PLACEHOLDER")))
		      (receiver (cons* inside-name outside-name names)
				(cons* value unassigned-object values)
				(cons (transfer outside-name inside-name)
				      transfers-in)
				(cons (transfer inside-name outside-name)
				      transfers-out)))
		    (syntax-error "Binding not a pair" binding)))))))

    (spread-arguments
     (lambda (bindings . body)
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
		 (make-thunk (make-sequence transfers-out)))))))))))

(define syntax-FLUID-LET-form-deep)
(define syntax-FLUID-LET-form-common-lisp)
(let ()

(define (make-fluid-let primitive procedure-tag)
  ;; (FLUID-LET ((<access-or-symbol> <value>) ...) . <body>) =>
  ;;    (WITH-SAVED-FLUID-BINDINGS
  ;;      (LAMBDA ()
  ;;        (ADD-FLUID! (THE-ENVIRONMENT) <access-or-symbol> <value>)
  ;;        ...
  ;;        <body>))
  (let ((with-saved-fluid-bindings
	 (make-primitive-procedure 'WITH-SAVED-FLUID-BINDINGS #t)))
    (spread-arguments
     (lambda (bindings . body)
       (syntax-fluid-bindings bindings
         (lambda (names values)
	   (make-combination
	    (internal-make-lambda procedure-tag '() '() '()
  	     (make-combination
	      with-saved-fluid-bindings
	      (list
	       (make-thunk
		(make-sequence 
		 (map*
		  (list (syntax-sequence body))
		  (lambda (name-or-access value)
		    (cond ((variable? name-or-access)
			   (make-combination
			    primitive
			    (list the-environment-object
				  (make-quotation name-or-access)
				  value)))
			  ((access? name-or-access)
			   (access-components name-or-access
			     (lambda (env name)
			       (make-combination primitive
						 (list env name value)))))
			  (else
			   (syntax-error
			    "Target of FLUID-LET not a symbol or ACCESS form"
			    name-or-access))))
		  names values))))))
            '())))))))

(define (syntax-fluid-bindings bindings receiver)
  (if (null? bindings)
      (receiver '() '())
      (syntax-fluid-bindings
       (cdr bindings)
       (lambda (names values)
	 (let ((binding (car bindings)))
	   (if (pair? binding)
	       (receiver (cons (let ((name (syntax-expression (car binding))))
				 (if (or (variable? name)
					 (access? name))
				     name
				     (syntax-error "Binding name illegal"
						   (car binding))))
			       names)
			 (cons (expand-binding-value (cdr binding)) values))
	       (syntax-error "Binding not a pair" binding)))))))

(set! syntax-FLUID-LET-form-deep
      (make-fluid-let (make-primitive-procedure 'ADD-FLUID-BINDING! #t)
		      lambda-tag:deep-fluid-let))

(set! syntax-FLUID-LET-form-common-lisp
      ;; This -- groan -- is for Common Lisp support
      (make-fluid-let (make-primitive-procedure 'MAKE-FLUID-BINDING! #t)
		      lambda-tag:common-lisp-fluid-let))

;;; end special FLUID-LETs.
)

;;;; Extended Assignment Syntax

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

;;; These two procedures use `error' instead of `syntax-error' because
;;; they are called when the syntaxer is not running.

(define (process-declarations declarations)
  (if (list? declarations)
      (map process-declaration declarations)
      (error "SYNTAX: Illegal declaration list" declarations)))

(define (process-declaration declaration)
  (cond ((symbol? declaration)
	 (list declaration))
	((and (list? declaration)
	      (not (null? declaration))
	      (symbol? (car declaration)))
	 declaration)
	(else
	 (error "SYNTAX: Illegal declaration" declaration))))

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

(define (make-letrec names values body)
  (make-closed-block lambda-tag:let '() '()
		     (make-sequence (append! (map make-definition names values)
					     (list body)))))

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
      (fluid-let-maker 'SHALLOW syntax-fluid-let-form-shallow))
(set! deep-fluid-let!
      (fluid-let-maker 'DEEP syntax-fluid-let-form-deep))
(set! common-lisp-fluid-let!
      (fluid-let-maker 'COMMON-LISP syntax-fluid-let-form-common-lisp))

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