#| -*-Scheme-*-

$Id: pardec.scm,v 4.8 1993/08/03 22:40:23 jacob Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Parse Declarations
;;; package: (scode-optimizer declarations)

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Main Entry Points

(define (declarations/parse block declarations)
  (make-declaration-set declarations
			(append-map (lambda (declaration)
				      (parse-declaration block declaration))
				    declarations)))

(define (declarations/make-null)
  (make-declaration-set '() '()))

(define (declarations/original declaration-set)
  (declaration-set/original declaration-set))

(define (declarations/bind operations declaration-set)
  (let loop
      ((operations operations)
       (declarations (declaration-set/declarations declaration-set)))
    (if (null? declarations)
	operations
	(loop (let ((declaration (car declarations)))
		((if (declaration/overridable? declaration)
		     operations/bind-global
		     operations/bind)
		 operations
		 (declaration/operation declaration)
		 (declaration/variable declaration)
		 (declaration/value declaration)))
	      (cdr declarations)))))

(define (declarations/map declaration-set per-variable per-value)
  (make-declaration-set
   (declaration-set/original declaration-set)
   (map (lambda (declaration)
	  (make-declaration (declaration/operation declaration)
			    (per-variable (declaration/variable declaration))
			    (let ((value (declaration/value declaration)))
			      (and value
				   (per-value value)))
			    (declaration/overridable? declaration)))
	(declaration-set/declarations declaration-set))))

(define (declarations/known? declaration)
  (assq (car declaration) known-declarations))

;;;; Data Structures

(define-structure (declaration-set
		   (type vector)
		   (named
		    (string->symbol
		     "#[(scode-optimizer declarations)declaration-set]"))
		   (constructor make-declaration-set)
		   (conc-name declaration-set/))
  (original false read-only true)
  (declarations false read-only true))

(define-structure (declaration
		   (type vector)
		   (named
		    (string->symbol
		     "#[(scode-optimizer declarations)declaration]"))
		   (constructor make-declaration)
		   (conc-name declaration/))
  ;; OPERATION is the name of the operation that is to be performed by
  ;; this declaration.
  (operation false read-only true)

  ;; The variable that this declaration affects.
  (variable false read-only true)

  ;; The value associated with this declaration.  The meaning of this
  ;; field depends on OPERATION.
  (value false read-only true)

  ;; OVERRIDABLE? means that a user-defined variable of the same name
  ;; will override this declaration.  It also means that this
  ;; declaration should not be written out to the ".ext" file.
  (overridable? false read-only true))

(define (make-declarations operation variables values overridable?)
  (if (eq? values 'NO-VALUES)
      (map (lambda (variable)
	     (make-declaration operation variable false overridable?))
	   variables)
      (map (lambda (variable value)
	     (make-declaration operation variable value overridable?))
	   variables
	   values)))

(define (parse-declaration block declaration)
  (let ((association (assq (car declaration) known-declarations)))
    (if (not association)
	'()
	((cdr association) block (cdr declaration)))))

(define (define-declaration operation parser)
  (let ((entry (assq operation known-declarations)))
    (if entry
	(set-cdr! entry parser)
	(set! known-declarations
	      (cons (cons operation parser)
		    known-declarations))))
  operation)

(define known-declarations
  '())

;;;; Integration Declarations

(define-declaration 'USUAL-INTEGRATIONS
  ;; This is written in a strange way because the obvious way to write
  ;; it is quadratic in the number of names being declared.  Since
  ;; there are typically over 300 names, this matters some.  I believe
  ;; this algorithm is linear in the number of names.
  (lambda (block deletions)
    (let ((deletions
	   (append sf/usual-integrations-default-deletions deletions))
	  (declarations '())
	  (remaining '()))
      (let ((do-deletions
	     (lambda (names vals)
	       (if (null? deletions)
		   (values names vals)
		   (let deletion-loop
		       ((names names)
			(vals vals)
			(names* '())
			(vals* '()))
		     (cond ((null? names)
			    (values names* vals*))
			   ((memq (car names) deletions)
			    (deletion-loop (cdr names)
					   (cdr vals)
					   names*
					   vals*))
			   (else
			    (deletion-loop (cdr names)
					   (cdr vals)
					   (cons (car names) names*)
					   (cons (car vals) vals*))))))))
	    (constructor
	     (lambda (operation)
	       (lambda (name value)
		 (let ((variable (block/lookup-name block name false)))
		   (if variable
		       (set! declarations
			     (cons (make-declaration operation
						     variable
						     value
						     true)
				   declarations))
		       (set! remaining
			     (cons (vector operation name value)
				   remaining))))
		 unspecific))))
	(call-with-values
	    (lambda ()
	      (do-deletions usual-integrations/expansion-names
			    usual-integrations/expansion-values))
	  (lambda (expansion-names expansion-values)
	    (for-each (constructor 'EXPAND)
		      expansion-names
		      expansion-values)))
	(call-with-values
	    (lambda ()
	      (do-deletions usual-integrations/constant-names
			    usual-integrations/constant-values))
	  (lambda (constant-names constant-values)
	    (for-each (constructor 'INTEGRATE)
		      constant-names
		      constant-values))))
      (map* declarations
	    (let ((top-level-block
		   (let loop ((block block))
		     (if (block/parent block)
			 (loop (block/parent block))
			 block))))
	      (lambda (remaining)
		(make-declaration
		 (vector-ref remaining 0)
		 (variable/make&bind! top-level-block (vector-ref remaining 1))
		 (vector-ref remaining 2)
		 true)))
	    remaining))))

(define (define-integration-declaration operation)
  (define-declaration operation
    (lambda (block names)
      (make-declarations operation
			 (block/lookup-names block names true)
			 'NO-VALUES
			 false))))

(define-integration-declaration 'INTEGRATE)
(define-integration-declaration 'INTEGRATE-OPERATOR)
(define-integration-declaration 'INTEGRATE-SAFELY)

(define-declaration 'INTEGRATE-EXTERNAL
  (lambda (block specifications)
    (append-map
     (lambda (pathname)
       (call-with-values (lambda () (read-externs-file pathname))
	 (lambda (externs-block externs)
	   (if externs-block
	       (change-type/block externs-block))
	   (append-map
	    (lambda (extern)
	      (let ((operation (vector-ref extern 0))
		    (name (vector-ref extern 1))
		    (value (vector-ref extern 2)))
		(if (and (eq? 'EXPAND operation)
			 (dumped-expander? value))
		    (parse-declaration block
				       (dumped-expander/declaration value))
		    (begin
		      (change-type/expression value)
		      (list
		       (make-declaration operation
					 (block/lookup-name block name true)
					 (make-integration-info
					  (copy/expression/extern block value))
					 true))))))
	    externs))))
     (append-map (lambda (specification)
		   (let ((value
			  (scode-eval
			   (syntax specification
				   system-global-syntax-table)
			   syntaxer/default-environment)))
		     (if (pair? value)
			 (map ->pathname value)
			 (list (->pathname value)))))
		 specifications))))

(define (operations->external operations environment)
  (let ((block (block/make false false '())))
    (values
     block
     (delq! false
	    (operations/map-external operations
	      (lambda (operation variable value)
		(let ((finish
		       (lambda (value)
			 (vector operation
				 (variable/name variable)
				 (copy/expression/extern block value)))))
		  (cond ((not value)
			 (variable/final-value variable
					       environment
					       finish
					       (lambda () false)))
			((integration-info? value)
			 (finish (integration-info/expression value)))
			((dumpable-expander? value)
			 (vector operation
				 (variable/name variable)
				 (dumpable-expander->dumped-expander value)))
			(else
			 (error "Unrecognized extern value:" value))))))))))

;;;; Flag Declarations

(for-each (lambda (flag)
	    (define-declaration flag
	      (lambda (block tail)
		(if (not (null? tail))
		    (error "This declaration does not take arguments:"
			   (cons flag tail)))
		(if (not (memq flag (block/flags block)))
		    (set-block/flags! block (cons flag (block/flags block))))
		'())))
	  '(AUTOMAGIC-INTEGRATIONS
	    ETA-SUBSTITUTION
	    OPEN-BLOCK-OPTIMIZATIONS
	    NO-AUTOMAGIC-INTEGRATIONS
	    NO-ETA-SUBSTITUTION
	    NO-OPEN-BLOCK-OPTIMIZATIONS))

(define-declaration 'IGNORE
  (lambda (block names)
    (for-each (lambda (variable)
		(if variable
		    (variable/can-ignore! variable)))
	      (block/lookup-names block names false))
    '()))

;;;; Reductions and Expansions
;;; See "reduct.scm" for description of REDUCE-OPERATOR and REPLACE-OPERATOR.

(define-declaration 'REDUCE-OPERATOR
  (lambda (block reduction-rules)
    (check-declaration-syntax 'REDUCE-OPERATOR reduction-rules)
    (map (lambda (rule)
	   (make-declaration 'EXPAND
			     (block/lookup-name block (car rule) true)
			     (make-dumpable-expander (reducer/make rule block)
						     `(REDUCE-OPERATOR ,rule))
			     false))
	 reduction-rules)))

(define-declaration 'REPLACE-OPERATOR
  (lambda (block replacements)
    (check-declaration-syntax 'REPLACE-OPERATOR replacements)
    (map (lambda (replacement)
	   (make-declaration 'EXPAND
			     (block/lookup-name block (car replacement) true)
			     (make-dumpable-expander
			      (replacement/make replacement block)
			      `(REPLACE-OPERATOR ,replacement))
			     false))
	 replacements)))

(define (check-declaration-syntax kind declarations)
  (if (not (and (list? declarations)
		(for-all? declarations
		  (lambda (declaration)
		    (and (pair? declaration)
			 (symbol? (car declaration))
			 (list? (cdr declaration)))))))
      (error "Bad declaration:" kind declarations)))

(define (make-dumpable-expander expander declaration)
  (make-entity (lambda (self expr operands if-expanded if-not-expanded block)
		 self			; ignored
		 (expander expr operands if-expanded if-not-expanded block))
	       (cons '*DUMPABLE-EXPANDER* declaration)))

(define (dumpable-expander? object)
  (and (entity? object)
       (let ((extra (entity-extra object)))
	 (and (pair? extra)
	      (eq? '*DUMPABLE-EXPANDER* (car extra))))))

(define (dumpable-expander->dumped-expander expander)
  (cons dumped-expander-tag (cdr (entity-extra expander))))

(define (dumped-expander? object)
  (and (pair? object)
       (eq? dumped-expander-tag (car object))))

(define (dumped-expander/declaration expander)
  (cdr expander))

(define dumped-expander-tag
  (string->symbol "#[(scode-optimizer declarations)dumped-expander]"))

;;; Expansions.  These should be used with great care, and require
;;; knowing a fair amount about the internals of sf.  This declaration
;;; is purely a hook, with no convenience.

(define-declaration 'EXPAND-OPERATOR
  (lambda (block expanders)
    block				;ignored
    (map (lambda (expander)
	   (make-declaration 'EXPAND
			     (block/lookup-name block (car expander) true)
			     (eval (cadr expander)
				   expander-evaluation-environment)
			     false))
	 expanders)))