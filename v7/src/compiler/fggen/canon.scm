#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fggen/canon.scm,v 1.8 1990/04/03 04:50:50 jinx Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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
		expression or an in-package special form.  It is not
		surrounded by any lambda expressions in the input form.
		It is assumed that such expressions are only executed
		(evaluated) once.

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

(define (canonicalize/top-level expression)
  (if (eq? compiler:package-optimization-level 'NONE)
      expression
      (fluid-let ((*top-level-declarations* '()))
	(let ((result
	       (canonicalize/expression
		expression '()
		(if (and compiler:cache-free-variables?
			 (not (eq? compiler:package-optimization-level 'LOW)))
		    'TOP-LEVEL
		    'FIRST-CLASS))))
	  (if (canout-needs? result)
	      (canonicalize/bind-environment (canout-expr result)
					     (scode/make-the-environment)
					     expression)
	      (canout-expr result))))))

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
	(make-canout (scode/make-combination
		      (ucode-primitive local-assignment)
		      (list (scode/make-variable environment-variable)
			    name
			    (canout-expr value)))
		     (canout-safe? value)
		     true
		     false)))))

(define (canonicalize/the-environment expr bound context)
  expr bound context ;; ignored
  (make-canout (scode/make-variable environment-variable)
	       false true false))

(define (canonicalize/lambda expr bound context)
  (let ((canout
	 (canonicalize/lambda* expr bound
			       (if (eq? context 'FIRST-CLASS)
				   'FIRST-CLASS
				   'ARBITRARY))))
    (if (and (eq? context 'TOP-LEVEL)
	     (canout-safe? canout)
	     compiler:compile-by-procedures?)
	(make-canout
	 (scode/make-directive
	  (if (null? *top-level-declarations*)
	      (canout-expr canout)
	      (make-open-block '() *top-level-declarations* (canout-expr canout)))
	  '(COMPILE-PROCEDURE)
	  expr)
	 true
	 (canout-needs? canout)
	 (canout-splice? canout))
	canout)))

(define (canonicalize/sequence expr bound context)
  (cond ((not (scode/open-block? expr))
	 (scode/sequence-components expr
	  (lambda (actions)
	    (canonicalize/combine-unary
	     scode/make-sequence
	     (combine-list
	      (map (lambda (act)
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
	      (canonicalize/expression
	       (unscan-defines names decls body)
	       bound
	       context)))))))

;;;; Hairier expressions

(let-syntax ((is-operator?
	      (macro (value name)
		`(or (eq? ,value (ucode-primitive ,name))
		     (and (scode/absolute-reference? ,value)
			  (eq? (scode/absolute-reference-name ,value)
			       ',name))))))

  (define (canonicalize/combination expr bound context)
    (scode/combination-components
     expr
     (lambda (operator operands)
       (cond ((lambda? operator)
	      (canonicalize/let operator operands bound context))
	     ((and (is-operator? operator LEXICAL-UNASSIGNED?)
		   (scode/the-environment? (car operands))
		   (symbol? (cadr operands)))
	      (canonicalize/unassigned? (cadr operands) expr bound context))
	     ((and (is-operator? operator ERROR-PROCEDURE)
		   (scode/the-environment? (caddr operands)))
	      (canonicalize/error operator operands bound context))
	     (else
	      (canonicalize/combine-binary
	       scode/make-combination
	       (canonicalize/expression operator bound context)
	       (combine-list
		(map (lambda (op)
		       (canonicalize/expression op bound context))
		     operands)))))))))

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
   (combine-list
    (map (lambda (op)
	   (canonicalize/expression op bound context))
	 operands))))

;;;; Protect from further canonicalization

(define (canonicalize/comment expr bound context)
  (scode/comment-components
   expr
   (lambda (text body)
     (if (not (and (scode/comment-directive? text 'PROCESSED 'ENCLOSE)
		   (scode/combination? body)))
	 (canonicalize/combine-binary
	  scode/make-comment
	  (canonicalize/expression text bound context)
	  (canonicalize/expression body bound context))
	 (scode/combination-components
	  body
	  (lambda (operator operands)
	    (if (scode/the-environment? (cadr operands))
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

;;;; Hair squared

(define (canonicalize/in-package expr bound context)
  (scode/in-package-components
   expr
   (lambda (environment expression)
     (let ((nexpr (canonicalize/expression
		   expression '()
		   (if (canonicalize/optimization-low? context)
		       'FIRST-CLASS
		       'TOP-LEVEL)))
	   (nenv (canonicalize/expression environment bound context)))

       (define (good expr)
	 (canonicalize/combine-unary
	  (lambda (env)
	    (scode/make-evaluation expr
				   env
				   (and (not (eq? context 'TOP-LEVEL))
					(not (eq? context 'ONCE-ONLY)))
				   expr))
	  nenv))

       (cond ((canout-splice? nexpr)
	      ;; Random optimization.  The in-package expression has no
	      ;; free variables.  Turn it into a sequence.
	      (canonicalize/combine-unary scode/make-sequence
					  (combine-list (list nenv nexpr))))
	     ((canonicalize/optimization-low? context)
	      (canonicalize/combine-unary
	       (lambda (exp)
		 (canonicalize/bind-environment (canout-expr nexpr)
						exp
						expr))
	       nenv))
	     ((not (canout-needs? nexpr))
	      (good (canout-expr nexpr)))
	     (else
	      (good
	       (canonicalize/bind-environment (canout-expr nexpr)
					      (scode/make-the-environment)
					      expr))))))))

;;;; Hair cubed

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

;;;; Dispatch

(define canonicalize/expression
  (let ((dispatch-vector
	 (make-vector (microcode-type/code-limit) canonicalize/constant)))

    (let-syntax
	((dispatch-entry
	  (macro (type handler)
	    `(VECTOR-SET! DISPATCH-VECTOR ,(microcode-type type) ,handler)))

	 (dispatch-entries
	  (macro (types handler)
	    `(BEGIN ,@(map (lambda (type)
			     `(DISPATCH-ENTRY ,type ,handler))
			   types))))
	 (standard-entry
	  (macro (name)
	    `(DISPATCH-ENTRY ,name ,(symbol-append 'CANONICALIZE/ name))))

	 (nary-entry
	  (macro (nary name)
	    `(DISPATCH-ENTRY ,name
			     (,(symbol-append 'CANONICALIZE/ nary)
			      ,(symbol-append 'SCODE/ name '-COMPONENTS)
			      ,(symbol-append 'SCODE/MAKE- name)))))

	 (binary-entry
	  (macro (name)
	    `(NARY-ENTRY binary ,name))))

      ;; quotations are treated as constants.
      (binary-entry access)
      (standard-entry assignment)
      (standard-entry comment)
      (nary-entry ternary conditional)
      (standard-entry definition)
      (nary-entry unary delay)
      (binary-entry disjunction)
      (standard-entry variable)
      (standard-entry in-package)
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