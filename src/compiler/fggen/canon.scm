#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Scode canonicalization.
;;; package: (compiler fg-generator)

;;; canonicalize/top-level translates scode expressions into
;;; equivalent scode expressions where all implicit first class
;;; environment operations have been made explicit.

(declare (usual-integrations))

#|
This program translates expressions depending on the context
in which they appear, the value of the global switch
compiler:package-optimization-level, and the variables which are
bound by "visible" surrounding lambda expressions.

1) Allowed levels for compiler:package-optimization-level are:

All levels except HYBRID treat all packages uniformly.

NONE:	no optimization is to be performed.

LOW:	variable manipulation and closure operations in package bodies
	are translated into explicit primitive calls (to
	LEXICAL-REFERENCE, etc.)

HYBRID:	once-only? package bodies are treated as in HIGH below.
	The rest are treated as in LOW above.

HIGH:	package bodies are treated as top level expressions to be
	processed independently.  They are copied as necessary to
	avoid inefficiencies (or incorrectness) due to shared lexical
	addresses, etc.

2) The context in which an expression appears is described by an
argument to canonicalize/expression.  The context argument can take
the following values:

FIRST-CLASS:	Treat every expression as if it appeared in a first class
		environment.  This is used by the LOW optimization level.

TOP-LEVEL:	The expression appears at top level of the original
		expression.  It is not surrounded by any lambda
		expressions in the input form.  It is assumed that
		such expressions are only executed (evaluated) once.

ONCE-ONLY:	The expression will be executed only once (as long as
		the corresponding top level expression is executed
		only once), although it appears surrounded by some
		lambda expression.  Currently this context occurs only
		in the body of (potentially nested) top level LET
		expressions.

ARBITRARY:	The expression may be executed more than once.  It
		appears surrounded by some lambda expressions which
		have not been proven to be invoked at most once.
|#

;;;; Data structures, top level and switches

(define-structure canout		; canonicalize-output
  expr					; expression
  safe?					; safe? if no (THE-ENVIRONMENT)
  needs?				; requires environment binding
  splice?)				; top level can be moved

(define *top-level-declarations*)
(define *top-level-definitions*)

(define (canonicalize/top-level expression)
  (if (eq? compiler:package-optimization-level 'NONE)
      (values expression #f)
      (fluid-let ((*top-level-declarations* '())
		  (*top-level-definitions* '()))
	(let ((result
	       (canonicalize/expression
		expression '()
		(if (and compiler:cache-free-variables?
			 (not (eq? compiler:package-optimization-level 'LOW)))
		    'TOP-LEVEL
		    'FIRST-CLASS))))
	  (values
	   (if (canout-needs? result)
	       (canonicalize/bind-environment (canout-expr result)
					      (scode/make-the-environment)
					      expression)
	       (canout-expr result))
	   *top-level-definitions*)))))

(define (canonicalize/optimization-low? context)
  (or (eq? context 'FIRST-CLASS)
      (eq? compiler:package-optimization-level 'LOW)
      (and (eq? compiler:package-optimization-level 'HYBRID)
	   (eq? context 'ARBITRARY))))

;;;; Combiners, and trivial compound expessions

(declare (integrate-operator
	  canonicalize/combine-unary canonicalize/unary
	  canonicalize/combine-binary canonicalize/binary
	  canonicalize/combine-ternary canonicalize/ternary
	  canonicalize/trivial))

(define (canonicalize/trivial obj bound context)
  bound context ;; ignored
  (make-canout obj true false true))

(define (canonicalize/combine-unary combiner a)
  (make-canout (combiner (canout-expr a))
	       (canout-safe? a)
	       (canout-needs? a)
	       (canout-splice? a)))

(define ((canonicalize/unary open close) expression bound context)
  (open expression
	(lambda (exp)
	  (canonicalize/combine-unary close
	   (canonicalize/expression exp bound context)))))

(define (canonicalize/combine-binary combiner a b)
  (make-canout (combiner (canout-expr a) (canout-expr b))
	       (and (canout-safe? a) (canout-safe? b))
	       (or (canout-needs? a) (canout-needs? b))
	       (and (canout-splice? a) (canout-splice? b))))

(define ((canonicalize/binary open close) expression bound context)
  (open expression
	(lambda (a b)
	  (canonicalize/combine-binary close
	   (canonicalize/expression a bound context)
	   (canonicalize/expression b bound context)))))

(define (canonicalize/combine-ternary combiner a b c)
  (make-canout (combiner (canout-expr a) (canout-expr b) (canout-expr c))
	       (and (canout-safe? a) (canout-safe? b) (canout-safe? c))
	       (or (canout-needs? a) (canout-needs? b) (canout-needs? c))
	       (and (canout-splice? a) (canout-splice? b) (canout-splice? c))))

(define ((canonicalize/ternary open close) expression bound context)
  (open expression
	(lambda (a b c)
	  (canonicalize/combine-ternary close
	   (canonicalize/expression a bound context)
	   (canonicalize/expression b bound context)
	   (canonicalize/expression c bound context)))))

(define canonicalize/constant
  canonicalize/trivial)

(define (canonicalize/error operator operands bound context)
  (canonicalize/combine-binary scode/make-combination
   (canonicalize/expression operator bound context)
   (combine-list
    (list (canonicalize/expression (car operands) bound context)
	  (canonicalize/expression (cadr operands) bound context)
	  (canonicalize/trivial (caddr operands) bound context)))))

;;;; Caching first class environments

(define environment-variable
  (intern "#[environment]"))

(define (canonicalize/bind-environment body exp original-expression)
  (define (normal)
    (scode/make-directive
     (scode/make-combination
      (scode/make-lambda lambda-tag:let
			 (list environment-variable) '() false '()
			 '()
			 body)
      (list exp))
     '(PROCESSED)
     original-expression))

  (define (comment body recvr)
    (scode/comment-components
     body
     (lambda (text nbody)
       (if (and (scode/comment-directive? text 'ENCLOSE)
		(scode/combination? nbody))
	   (scode/combination-components
	    nbody
	    (lambda (operator operands)
	      (if (and (eq? operator (ucode-primitive SCODE-EVAL))
		       (scode/quotation? (car operands))
		       (scode/variable? (cadr operands))
		       (eq? (scode/variable-name (cadr operands))
			    environment-variable))
		  (recvr (scode/quotation-expression (car operands)))
		  (normal))))
	   (normal)))))
 
  (cond ((scode/variable? body)
	 (let ((name (scode/variable-name body)))
	   (if (eq? name environment-variable)
	       exp
	       (scode/make-combination
		(ucode-primitive LEXICAL-REFERENCE)
		(list exp name)))))
	((not (scode/the-environment? exp))
	 (normal))
	((scode/combination? body)
	 (scode/combination-components
	  body
	  (lambda (operator operands)
	    (if (or (not (scode/comment? operator))
		    (not (null? operands)))
		(normal)
		(comment operator
			 (lambda (nopr)
			   (scode/make-combination nopr '())))))))
	((scode/comment? body)
	 (comment body (lambda (nbody) nbody)))
	(else (normal))))

(define (combine-list elements)
  (if (null? elements)
      (make-canout '() true false true)
      (canonicalize/combine-binary cons
       (car elements)
       (combine-list (cdr elements)))))

;;;; Variables and assignment

;; Variables and assignment are treated asymmetrically:
;; Assignments to free variables in non ARBITRARY contexts are
;; performed by using LEXICAL-ASSIGNMENT to avoid creating an
;; assignment cache which will be used only once.  Variable references
;; will only use LEXICAL-REFERENCE in FIRST-CLASS contexts.
;; The reason for this asymmetry is that a common programming style is
;; to bind some names at top level, and then assign them from within a
;; once-only context to initialize them.  Lowering the space
;; requirements of the assignment is more important than increasing
;; the speed since the assignment will only be done once.  This
;; decision penalizes certain assignments, but oh well...

(define (canonicalize/variable var bound context)
  (let ((name (scode/variable-name var)))
    (cond ((eq? name environment-variable)
	   (make-canout var true true false))
	  ((not (eq? context 'FIRST-CLASS))
	   (make-canout var true false (if (memq name bound) true false)))
	  ((memq name bound)
	   (make-canout var true false true))
	  (else
	   (make-canout
	    (scode/make-combination (ucode-primitive LEXICAL-REFERENCE)
	     (list (scode/make-variable environment-variable)
		   name))
	    true true false)))))

(define (canonicalize/assignment expr bound context)
  (scode/assignment-components
   expr
   (lambda (name old-value)
     (let ((value (canonicalize/expression old-value bound context)))
       (cond ((eq? context 'ARBITRARY)
	      (canonicalize/combine-binary scode/make-assignment
	       (make-canout name true false (if (memq name bound) true false))
	       value))
	     ((memq name bound)
	      (canonicalize/combine-binary scode/make-assignment
	       (make-canout name true false true)
	       value))
	     (else
	      (make-canout
	       (scode/make-combination (ucode-primitive LEXICAL-ASSIGNMENT)
		(list (scode/make-variable environment-variable)
		      name
		      (canout-expr value)))
	       (canout-safe? value)
	       true false)))))))

;;;; Hairy expressions

(define (canonicalize/definition expression bound context)
  (scode/definition-components expression
    (lambda (name value)
      (let ((value (canonicalize/expression value bound context)))
	(if (memq context '(ONCE-ONLY ARBITRARY))
	    (error "canonicalize/definition: unscanned definition"
		   expression))
	(single-definition name value context)))))

(define (canonicalize/the-environment expr bound context)
  expr bound context ;; ignored
  (make-canout (scode/make-variable environment-variable)
	       false true false))

(define (canonicalize/lambda expr bound context)
  (let ((canout
	 (canonicalize/lambda* expr bound (if (eq? context 'FIRST-CLASS)
					      'FIRST-CLASS
					      'ARBITRARY))))
    (if (or (not (eq? context 'TOP-LEVEL))
	    (not (canout-safe? canout))
	    (canout-needs? canout)
	    (not compiler:compile-by-procedures?))
	canout
	(make-canout
	 (scode/make-directive
	  (if (null? *top-level-declarations*)
	      (canout-expr canout)
	      (make-open-block '()
			       *top-level-declarations*
			       (canout-expr canout)))
	  '(COMPILE-PROCEDURE)
	  expr)
	 true
	 (canout-needs? canout)
	 (canout-splice? canout)))))

(define (canonicalize/sequence expr bound context)
  (cond ((not (scode/open-block? expr))
	 (scode/sequence-components expr
	  (lambda (actions)
	    (canonicalize/combine-unary
	     scode/make-sequence
	     (combine-list (map (lambda (act)
				  (canonicalize/expression act bound context))
				actions))))))
	((or (eq? context 'ONCE-ONLY)
	     (eq? context 'ARBITRARY)
	     (and (eq? context 'FIRST-CLASS)
		  (not (null? bound))))
	 (error "canonicalize/sequence: open block in bad context"
		expr context))
	(else
	 (scode/open-block-components
	  expr
	  (lambda (names decls body)
	    (fluid-let ((*top-level-declarations*
			 (append decls *top-level-declarations*)))
	      (let ((body (unscan-defines names decls body)))
		((if (and (eq? context 'TOP-LEVEL)
			  compiler:compress-top-level?
			  (> (length names) 1))
		     canonicalize/compressing
		     canonicalize/expression)
		 body
		 bound
		 context))))))))

(define (%single-definition name value)
  (scode/make-combination
   (ucode-primitive local-assignment)
   (list (scode/make-variable environment-variable)
	 name
	 (canout-expr value))))

(define (single-definition name value context)
  (if (and (eq? context 'TOP-LEVEL)
	   (not (memq name *top-level-definitions*)))
      (set! *top-level-definitions* (cons name *top-level-definitions*)))
  (make-canout (%single-definition name value)
	       (canout-safe? value)
	       true
	       false))

;; To reduce code space, split into two blocks, one with constants,
;; the other with expressions to be evaluated.

(define (multi-definition names* values* context)
  (define (collect names values wrapper)
    (if (null? (cdr values))
	(%single-definition (car names) (car values))
	(scode/make-combination
	 (scode/make-absolute-reference 'DEFINE-MULTIPLE)
	 (list (scode/make-variable environment-variable)
	       (list->vector names)
	       (wrapper (scode/make-combination (ucode-primitive vector)
						(map canout-expr values)))))))

  (define (join left right)
    (scode/make-sequence (list left right)))

  (define (directive-wrapper frob)
    (scode/make-directive frob '(CONSTANTIFY) frob))

  (define (pseudo-constant? value)
    (let ((value (canout-expr value)))
      (or (scode/constant? value)
	  (scode/lambda? value)
	  ;; Lambdas may be wrapped in directives
	  (and (scode/comment? value)
	       (scode/comment-directive? (scode/comment-text value)
					 'COMPILE-PROCEDURE)))))

  (if (eq? context 'TOP-LEVEL)
      (set! *top-level-definitions*
	    (lset-union eq? names* *top-level-definitions*)))
  (let loop ((names names*) (values values*) (last 'NONE)
	     (knames '()) (kvals '()) (vnames '()) (vvals '()))
    (cond ((null? names)
	   (make-canout
	    (cond ((null? vvals)
		   (collect names* values* directive-wrapper))
		  ((or (null? kvals) (null? (cdr kvals)))
		   (collect names* values* identity-procedure))
		  (else
		   (let ((vnames (reverse vnames)) (vvals (reverse vvals))
			 (knames (reverse knames)) (kvals (reverse kvals)))
		     (if (eq? last 'CONSTANT)
			 (join (collect vnames vvals identity-procedure)
			       (collect knames kvals directive-wrapper))
			 (join (collect knames kvals directive-wrapper)
			       (collect vnames vvals identity-procedure))))))
	    (for-all? values canout-safe?)
	    true
	    false))
	  ((pseudo-constant? (car values))
	   (loop (cdr names) (cdr values) 'CONSTANT
		 (cons (car names) knames)
		 (cons (car values) kvals)
		 vnames vvals))
	  (else
	   (loop (cdr names) (cdr values) 'EVALUATED
		 knames kvals
		 (cons (car names) vnames)
		 (cons (car values) vvals))))))

;; Collect continguous simple definitions into multi-definitions
;; in an attempt to make the top-level code smaller.
;; Note: MULTI-DEFINITION can reorder the definitions, so this
;; code must be careful.  Currently it only collects 
;; lambda expressions or expressions with no free variables.
;; Note: call-with-current-continuation at top-level may
;; expose this, but unless the programmer goes out of his/her
;; way to hide the reference (or use the primitive), it won't happen.

(define (canonicalize/compressing expr bound context)
  (define (give-up)
    (canonicalize/expression expr bound context))

  (if (or (not (scode/sequence? expr))
	  (scode/open-block? expr))
      (give-up)
      (scode/sequence-components
       expr
       (lambda (actions)
	 (define (add-group group groups)
	   (cond ((null? group)
		  groups)
		 ((null? (cdr group))
		  (let ((element (car group)))
		    (cons (single-definition (car element)
					     (cadr element)
					     context)
			  groups)))
		 (else
		  (let ((group (reverse group)))
		    (cons (multi-definition (map car group)
					    (map cadr group)
					    context)
			  groups)))))

	 (define (collect actions groups group)
	   (if (null? actions)
	       (canonicalize/combine-unary scode/make-sequence
					   (combine-list
					    (reverse
					     (add-group group groups))))
	       (let ((next (car actions)))
		 (if (not (scode/definition? next))
		     (let ((out (canonicalize/expression next
							 bound context)))
		       (if (not (canout-safe? out))
			   (give-up)
			   (collect (cdr actions)
				    (cons out
					  (add-group group groups))
				    '())))
		     (scode/definition-components
		      next
		      (lambda (name value)
			(let ((value*
			       (canonicalize/expression value bound context)))
			  (cond ((not (canout-safe? value*))
				 (give-up))
				((or (scode/lambda? value)
				     ;; This means that there are no free vars.
				     (canout-splice? value*))
				 (collect (cdr actions)
					  groups
					  (cons (list name value*)
						group)))
				(else
				 (collect (cdr actions)
					  (cons (single-definition name value*
								   context)
						(add-group group groups))
					  '()))))))))))

	 (collect actions '() '())))))

;;;; Hairier expressions

(define-syntax is-operator?
  (sc-macro-transformer
   (lambda (form environment)
     (let ((value (close-syntax (cadr form) environment))
	   (name (caddr form)))
       `(OR (EQ? ,value (UCODE-PRIMITIVE ,name))
	    (AND (SCODE/ABSOLUTE-REFERENCE? ,value)
		 (EQ? (SCODE/ABSOLUTE-REFERENCE-NAME ,value) ',name)))))))

(define (canonicalize/combination expr bound context)
  (scode/combination-components
   expr
   (lambda (operator operands)
     (cond ((lambda? operator)
	    (canonicalize/let operator operands bound context))
	   ((and (is-operator? operator lexical-unassigned?)
		 (scode/the-environment? (car operands))
		 (symbol? (cadr operands)))
	    (canonicalize/unassigned? (cadr operands) expr bound context))
	   ((and (is-operator? operator error-procedure)
		 (scode/the-environment? (caddr operands)))
	    (canonicalize/error operator operands bound context))
	   (else
	    (canonicalize/combine-binary
	     scode/make-combination
	     (canonicalize/expression operator bound context)
	     (combine-list
	      (map (lambda (op)
		     (canonicalize/expression op bound context))
		   operands))))))))

(define (canonicalize/unassigned? name expr bound context)
  (cond ((not (eq? context 'FIRST-CLASS))
	 (make-canout expr true false (if (memq name bound) true false)))
	((memq name bound)
	 (make-canout expr true false true))
	(else
	 (make-canout
	  (scode/make-combination
	   (ucode-primitive LEXICAL-UNASSIGNED?)
	   (list (scode/make-variable environment-variable)
		 name))
	  true true false))))

(define (canonicalize/let operator operands bound context)
  (canonicalize/combine-binary scode/make-combination
   (canonicalize/lambda* operator bound
			 (if (eq? context 'TOP-LEVEL)
			     'ONCE-ONLY
			     context))
   (let ((process-arguments
	  (lambda ()
	    (combine-list
	     (map (lambda (op)
		    (canonicalize/expression op bound context))
		  operands)))))
     (if (and compiler:compile-by-procedures?
	      (eq? context 'TOP-LEVEL))
	 (fluid-let ((compiler:compile-by-procedures? false))
	   (process-arguments))
	 (process-arguments)))))

;;;; Protect from further canonicalization

(define (canonicalize/comment expr bound context)
  (scode/comment-components
   expr
   (lambda (text body)
     (if (not (and (scode/comment-directive? text 'PROCESSED 'ENCLOSE)
		   (scode/combination? body)))
	 (canonicalize/combine-unary
	  (lambda (body*)
	    (scode/make-comment text body*))
	  (canonicalize/expression body bound context))
	 (scode/combination-components
	  body
	  (lambda (operator operands)
	    (if (and (eq? operator (ucode-primitive SCODE-EVAL))
		     (scode/the-environment? (cadr operands)))
		(make-canout
		 (scode/make-directive
		  (scode/make-combination
		   operator
		   (list (car operands)
			 (scode/make-variable environment-variable)))
		  (cadr text)
		  (caddr text))
		 false true false)
		(make-canout expr true true false))))))))

;;;; Utility for hairy expressions

(define (scode/make-evaluation exp env arbitrary? original-expression)
  (define (default)
    (scode/make-directive
     (scode/make-combination
      (ucode-primitive SCODE-EVAL)
      (list (let ((nexp
		   (scode/make-directive
		    (scode/make-quotation exp)
		    '(COMPILE)
		    original-expression)))
	      (if arbitrary?
		  (scode/make-combination
		   (scode/make-absolute-reference 'COPY-PROGRAM)
		   (list nexp))
		  nexp))
	    env))
     '(PROCESSED)
     original-expression))

  (cond ((scode/the-environment? exp)
	 env)
	((or (not (scode/combination? exp))
	     (not (scode/the-environment? env)))
	 (default))
	;; For the following optimization it is assumed that
	;; scode/make-evaluation is called only in restricted ways.
	(else
	 (scode/combination-components
	  exp
	  (lambda (operator operands)
	    (if (or (not (null? operands))
		    (not (scode/lambda? operator)))
		(default)
		(scode/lambda-components
		 operator
		 (lambda (name req opt rest aux decls body)
		   name req opt rest aux decls ;; ignored
		   (if (not (scode/comment? body))
		       (default)
		       (scode/comment-components
			body
			(lambda (text expr)
			  expr ;; ignored
			  (if (not (scode/comment-directive? text 'PROCESSED))
			      (default)
			      exp))))))))))))

;;;; Hair cubed

#|
;; The Old Code

(define (canonicalize/lambda* expr bound context)
  (scode/lambda-components expr
    (lambda (name required optional rest auxiliary decls body)
      (define (wrap code)
	(make-canout
	 (scode/make-directive
	  (scode/make-combination (ucode-primitive SCODE-EVAL)
	    (list (scode/make-quotation
		   (scode/make-lambda
		    name required optional rest '() decls code))
		  (scode/make-variable environment-variable)))
	  '(ENCLOSE)
	  expr)
	 false true false))
      (let ((nbody
	     (canonicalize/expression
	      body
	      (append required optional
		      (if rest (list rest) '())
		      auxiliary bound)
	      context)))
	(if (canout-safe? nbody)
	    (make-canout
	     (scode/make-lambda name required optional rest auxiliary
				decls
				(canout-expr nbody))
	     true
	     (canout-needs? nbody)
	     (canout-splice? nbody))
	    (let* ((nbody
		    (canonicalize/expression
		     (unscan-defines auxiliary decls (canout-expr nbody))
		     '()
		     (if (canonicalize/optimization-low? context)
			 'FIRST-CLASS
			 'TOP-LEVEL)))
		   (nexpr
		    (canonicalize/bind-environment (canout-expr nbody)
						   (scode/make-the-environment)
						   body)))
	      (wrap
	       (if (canonicalize/optimization-low? context)
		   nexpr
		   (scode/make-evaluation nexpr
					  (scode/make-the-environment)
					  (eq? context 'ARBITRARY)
					  expr)))))))))
|#

(define (canonicalize/lambda* expr bound context)
  (scode/lambda-components expr
    (lambda (name required optional rest auxiliary decls body)
      (let ((nbody (canonicalize/expression
		    body
		    (append required optional
			    (if rest (list rest) '())
			    auxiliary bound)
		    context)))

	(cond ((canout-safe? nbody)
	       (make-canout
		(scode/make-lambda name required optional rest auxiliary
				   decls
				   (canout-expr nbody))
		true
		(canout-needs? nbody)
		(canout-splice? nbody)))
	      ((not compiler:avoid-scode?)
	       ;; Old way of handling 1st-class environments
	       (make-canout
		(scode/make-directive
		 (scode/make-combination
		  (ucode-primitive SCODE-EVAL)
		  (list
		   (scode/make-quotation
		    (scode/make-lambda
		     name required optional rest '()
		     decls
		     (let* ((env-code (scode/make-the-environment))
			    (nbody
			     (canonicalize/expression
			      (unscan-defines auxiliary decls (canout-expr nbody))
			      '()
			      (if (canonicalize/optimization-low? context)
				  'FIRST-CLASS
				  'TOP-LEVEL)))
			    (nexpr
			     (canonicalize/bind-environment (canout-expr nbody)
							    env-code
							    body)))
      
		       (if (canonicalize/optimization-low? context)
			   nexpr
			   (scode/make-evaluation nexpr
						  (scode/make-the-environment)
						  (eq? context 'ARBITRARY)
						  expr)))))
		   (scode/make-variable environment-variable)))
		 '(ENCLOSE)
		 expr)
		false true false))

	      (else
	       (make-canout
		(scode/make-directive
		 (scode/make-lambda
		  name required optional rest '()
		  decls
		  (let* ((names
			  (append required optional (if rest (list rest) '())))
			 (env-code
			  (scode/make-combination
			   (scode/make-absolute-reference '*MAKE-ENVIRONMENT)
			   (cons* (scode/make-variable environment-variable)
				  (list->vector
				   (cons lambda-tag:unnamed names))
				  (map scode/make-variable names)))))

		    (if (and (scode/the-environment? body)
			     (null? auxiliary))
			env-code
			(let* ((uexpr (unscan-defines auxiliary decls (canout-expr nbody)))
			       (nexpr
				(canout-expr
				 (canonicalize/expression
				  uexpr
				  '()
				  (if (canonicalize/optimization-low? context)
				      'FIRST-CLASS
				      'TOP-LEVEL)))))

			   (if (canonicalize/optimization-low? context)
			       (canonicalize/bind-environment nexpr env-code uexpr)
			       (scode/make-evaluation
				(canonicalize/bind-environment
				 nexpr
				 (scode/make-the-environment)
				 uexpr)
				env-code
				(eq? context 'ARBITRARY)
				expr))))))
		 '(PROCESSED)
		 expr)
		false true false)))))))

;;;; Dispatch

(define canonicalize/expression
  (let ((dispatch-vector
	 (make-vector (microcode-type/code-limit) canonicalize/constant)))

    (letrec-syntax
	((dispatch-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     `(VECTOR-SET! DISPATCH-VECTOR ,(microcode-type (cadr form))
			   ,(close-syntax (caddr form) environment)))))

	 (dispatch-entries
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((handler (close-syntax (caddr form) environment)))
	       `(BEGIN
		  ,@(map (lambda (type)
			   `(DISPATCH-ENTRY ,type ,handler))
			 (cadr form)))))))
	 (standard-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((name (cadr form)))
	       `(DISPATCH-ENTRY ,name
				,(close-syntax (symbol-append 'CANONICALIZE/
							      name)
					       environment))))))

	 (nary-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((nary (cadr form))
		   (name (caddr form)))
	       `(DISPATCH-ENTRY ,name
				,(close-syntax
				  `(,(symbol-append 'CANONICALIZE/ nary)
				    ,(symbol-append 'SCODE/ name '-COMPONENTS)
				    ,(symbol-append 'SCODE/MAKE- name))
				  environment))))))

	 (binary-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     environment
	     `(NARY-ENTRY BINARY ,(cadr form))))))

      ;; quotations are treated as constants.
      (binary-entry access)
      (standard-entry assignment)
      (standard-entry comment)
      (nary-entry ternary conditional)
      (standard-entry definition)
      (nary-entry unary delay)
      (binary-entry disjunction)
      (standard-entry variable)
      (standard-entry the-environment)
      (dispatch-entries (combination-1 combination-2 combination
				       primitive-combination-0
				       primitive-combination-1
				       primitive-combination-2
				       primitive-combination-3)
			canonicalize/combination)
      (dispatch-entries (lambda lexpr extended-lambda) canonicalize/lambda)
      (dispatch-entries (sequence-2 sequence-3) canonicalize/sequence))
    (named-lambda (canonicalize/expression expression bound context)
      ((vector-ref dispatch-vector (object-type expression))
       expression bound context))))