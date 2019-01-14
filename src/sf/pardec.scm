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

;;;; SCode Optimizer: Parse Declarations
;;; package: (scode-optimizer declarations)

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Main Entry Points

(define (declarations/parse block declarations)
  (let ((declarations (merge-usual-integrations declarations)))
    (make-declaration-set declarations
			  (append-map (lambda (declaration)
					(if (eq? (car declaration)
						 'target-metadata)
					    '()
					    (parse-declaration block
							       declaration)))
				      declarations))))

(define (merge-usual-integrations declarations)
  (let loop ((declarations declarations) (exclusions 'none) (other '()))
    (if (pair? declarations)
	(if (eq? (caar declarations) 'usual-integrations)
	    (loop (cdr declarations)
		  (if (eq? exclusions 'none)
		      (cdar declarations)
		      (append exclusions (cdar declarations)))
		  other)
	    (loop (cdr declarations)
		  exclusions
		  (cons (car declarations) other)))
	(if (eq? exclusions 'none)
	    (reverse! other)
	    (cons `(usual-integrations ,@exclusions)
		  (reverse! other))))))

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
		((case (declaration/binding-level declaration)
		   ((local)     operations/bind)
		   ((top-level) operations/bind-top-level)
		   ((global)    operations/bind-global)
		   (else
		    (error "Unrecognized binding level"
			   (declaration/binding-level declaration))))
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
			    (declaration/binding-level declaration)))
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
  (original #f read-only #t)
  (declarations #f read-only #t))

(define-structure (declaration
		   (type vector)
		   (named
		    (string->symbol
		     "#[(scode-optimizer declarations)declaration]"))
		   (constructor make-declaration)
		   (conc-name declaration/))
  ;; OPERATION is the name of the operation that is to be performed by
  ;; this declaration.
  (operation #f read-only #t)

  ;; The variable that this declaration affects.
  (variable #f read-only #t)

  ;; The value associated with this declaration.  The meaning of this
  ;; field depends on OPERATION.
  (value #f read-only #t)

  ;; BINDING-LEVEL indicates whether the declaration is `global',
  ;; 'top-level' or 'local'.  Only 'local' declarations are written out
  ;; to the ".ext" file.

  ;; Usual-integrations are bound at the `global' level, external
  ;; declarations are bound at the 'top-level' level.  This prevents
  ;; confusion between external integrations that have the same name
  ;; as usual ones.
  (binding-level #f read-only #t))

(define (make-declarations operation variables values binding-level)
  (if (eq? values 'no-values)
      (map (lambda (variable)
	     (make-declaration operation variable #f binding-level))
	   variables)
      (map (lambda (variable value)
	     (make-declaration operation variable value binding-level))
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

(define (known-declaration? operation)
  (or (eq? operation 'expand) ; this one is special
      (assq operation known-declarations)))

;;;; Integration Declarations

(define-declaration 'usual-integrations
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
		 (let ((variable (block/lookup-name block name #f)))
		   (if variable
		       (set! declarations
			     (cons (make-declaration operation
						     variable
						     value
						     'global)
				   declarations))
		       (set! remaining
			     (cons (vector operation name value)
				   remaining))))
		 unspecific))))
	(receive (expansion-names expansion-values)
	    (do-deletions usual-integrations/expansion-names
			  usual-integrations/expansion-values)
	  (for-each (constructor 'expand)
		    expansion-names
		    expansion-values))
	(receive (constant-names constant-values)
	    (do-deletions usual-integrations/constant-names
			  usual-integrations/constant-values)
	  (for-each (constructor 'integrate)
		    constant-names
		    constant-values)))
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
		 'global)))
	    remaining))))

;;; The corresponding case for R7RS is much simpler since the imports are
;;; explicit.

(define (r7rs-usual-integrations block imports)
  (make-declaration-set '()
    (let ((globals (standard-library-globals imports)))
      (let ((constructor
	     (lambda (operation)
	       (lambda (name value)
		 (let ((global
			(find (lambda (global)
				(eq? (cdr global) name))
			      globals)))
		   (and global
			(make-declaration operation
					  (block/lookup-name block
							     (car global)
							     #f)
					  value
					  'global)))))))
	(append (filter-map (constructor 'expand)
			    usual-integrations/expansion-names
			    usual-integrations/expansion-values)
		(filter-map (constructor 'integrate)
			    usual-integrations/constant-names
			    usual-integrations/constant-values))))))

(define (define-integration-declaration operation)
  (define-declaration operation
    (lambda (block names)
      (make-declarations operation
			 (block/lookup-names block names #t)
			 'no-values
			 'local))))

(define-integration-declaration 'integrate)
(define-integration-declaration 'integrate-operator)

(define-declaration 'integrate-external
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
		(if (and (eq? 'expand operation)
			 (dumped-expander? value))
		    (parse-declaration block
				       (dumped-expander/declaration value))
		    (begin
		      (change-type/expression value)
		      (list
		       (make-declaration operation
					 (if (symbol? name)
					     (block/lookup-name block name #t)
					     name)
					 (make-integration-info
					  (copy/expression/extern block value))
					 'top-level))))))
	    externs))))
     (append-map (lambda (specification)
		   (let ((value
			  (eval specification system-global-environment)))
		     (if (pair? value)
			 (map ->pathname value)
			 (list (->pathname value)))))
		 specifications))))

(define (operations->external operations environment)
  (let ((block (block/make #f #f '())))
    (values
     block
     (delq! #f
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
					       (lambda () #f)))
			((integration-info? value)
			 (finish (integration-info/expression value)))
			((dumpable-expander? value)
			 (vector operation
				 (if (variable? variable)
				     (variable/name variable)
				     variable)
				 (dumpable-expander->dumped-expander value)))
			(else
			 (error "Unrecognized extern value:" value))))))))))

;;;; Flag Declarations

;; IGNORABLE suppresses warnings about the variable not being used.
;; This is useful in macros that bind variables that the body may
;; not actually use.
(define-declaration 'ignorable
  (lambda (block names)
    (for-each (lambda (name)
		(let ((variable (block/lookup-name block name #f)))
		  (if variable
		      (variable/may-ignore! variable)
		      (warn "ignoring IGNORABLE declaration of free variable"
			    name))))
	      names)
    '()))

;; IGNORE causes warnings if an ignored variable actually ends
;; up being used.  Mentioning the variable in a sequence will
;; have the effect of marking it IGNORED.
(define-declaration 'ignore
  (lambda (block names)
    (let ((variables
	   (let loop
	       ((names names)
		(variables '()))
	     (if (pair? names)
		 (let* ((name (car names))
			(variable (block/lookup-name block name #f)))
		   (if variable
		       (begin
			 (variable/must-ignore! variable)
			 (loop (cdr names) (cons variable variables)))
		       (begin
			 (warn "ignoring IGNORE declaration of free variable"
			       name)
			 (loop (cdr names) variables))))
		 variables))))
      (make-declarations 'ignore
			 variables
			 'no-values
			 'local))))

;;;; Reductions and Expansions
;;; See "reduct.scm" for description of REDUCE-OPERATOR and REPLACE-OPERATOR.

(define-declaration 'reduce-operator
  (lambda (block reduction-rules)
    (check-declaration-syntax 'reduce-operator reduction-rules)
    (map (lambda (rule)
	   (make-declaration 'expand
			     (block/lookup-name block (car rule) #t)
			     (make-dumpable-expander (reducer/make rule block)
						     `(reduce-operator ,rule))
			     'local))
	 reduction-rules)))

(define (check-declaration-syntax kind declarations)
  (if (not (and (list? declarations)
		(every (lambda (declaration)
			 (and (pair? declaration)
			      (symbol? (car declaration))
			      (list? (cdr declaration))))
		       declarations)))
      (error "Bad declaration:" kind declarations)))

(define-declaration 'replace-operator
  (lambda (block replacements)
    (if (not (and (list? replacements)
		  (every (lambda (replacement)
			   (and (pair? replacement)
				(or (symbol? (car replacement))
				    (and (pair? (car replacement))
					 (eq? 'primitive (caar replacement))
					 (pair? (cdar replacement))
					 (symbol? (cadar replacement))
					 (or (null? (cddar replacement))
					     (and (pair? (cddar replacement))
						  (null?
						   (cdddar replacement))))))
				(list? (cdr replacement))))
			 replacements)))
	(error "Bad declaration:" 'replace-operator replacements))
    (map (lambda (replacement)
	   (make-declaration
	    'expand
	    (let ((name (car replacement)))
	      (cond ((symbol? name)
		     (block/lookup-name block name #t))
		    ((and (pair? name)
			  (eq? (car name) 'primitive))
		     (make-primitive-procedure (cadr name)
					       (and (not (null? (cddr name)))
						    (caddr name))))
		    (else
		     (error "Illegal name in replacement:" name))))
	    (make-dumpable-expander
	     (replacement/make replacement block)
	     `(replace-operator ,replacement))
	    'local))
	 replacements)))

(define (make-dumpable-expander expander declaration)
  (make-entity (lambda (self expr operands block)
		 self			; ignored
		 (expander expr operands block))
	       (cons '*dumpable-expander* declaration)))

(define (dumpable-expander? object)
  (and (entity? object)
       (let ((extra (entity-extra object)))
	 (and (pair? extra)
	      (eq? '*dumpable-expander* (car extra))))))

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

(define-declaration 'expand-operator
  (lambda (block expanders)
    (map (lambda (expander)
	   (make-declaration 'expand
			     (block/lookup-name block (car expander) #t)
			     (eval (cadr expander)
				   expander-evaluation-environment)
			     'local))
	 expanders)))