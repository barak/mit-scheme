#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/pardec.scm,v 3.0 1987/03/10 13:25:13 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define (declarations/known? declaration)
  (assq (car declaration) known-declarations))

(define (declarations/parse block declarations)
  (return-2
   declarations
   (accumulate
    (lambda (declaration bindings)
      (let ((association (assq (car declaration) known-declarations)))
	(if (not association)
	    bindings
	    (transmit-values (cdr association)
	      (lambda (before-bindings? parser)
		(let ((block
		       (if before-bindings?
			   (let ((block (block/parent block)))
			     (if (block/parent block)
				 (warn "Declaration not at top level"
				       declaration))
			     block)
			   block)))
		  (parser block (bindings/cons block before-bindings?) bindings
			  (cdr declaration))))))))
    (return-2 '() '())
    declarations)))

(define (declarations/rename declarations rename)
  (declarations/map declarations
    (lambda (bindings)
      (map (lambda (binding)
	     (transmit-values binding
	       (lambda (applicator binder names)
		 (return-3 applicator binder (map rename names)))))
	   bindings))))

(define (declarations/binders declarations)
  (transmit-values declarations
    (lambda (original bindings)
      (call-multiple (lambda (bindings)
		       (lambda (operations)
			 (accumulate (lambda (binding operations)
				       (transmit-values binding
					 (lambda (applicator binder names)
					   (applicator binder operations
						       names))))
				     operations bindings)))
		     bindings))))

(define (declarations/original declarations)
  (transmit-values declarations
    (lambda (original bindings)
      original)))

(define (declarations/map declarations procedure)
  (transmit-values declarations
    (lambda (original bindings)
      (return-2 original (call-multiple procedure bindings)))))

(define (bindings/cons block before-bindings?)
  (lambda (bindings applicator names global?)
    (let ((result
	   (if global?
	       (return-3 applicator operations/bind-global names)
	       (return-3 applicator operations/bind
			 (block/lookup-names block names)))))
      (transmit-values bindings
	(lambda (before-bindings after-bindings)
	  (if before-bindings?
	      (return-2 (cons result before-bindings) after-bindings)
	      (return-2 before-bindings (cons result after-bindings))))))))

(define (bind/values table/cons table operation export? names values)
  (table/cons table
	      (lambda (binder operations names)
		(binder operations operation export? names values))
	      names
	      (not export?)))

(define (bind/no-values table/cons table operation export? names)
  (table/cons table
	      (lambda (binder operations names)
		(binder operations operation export? names))
	      names
	      false))

(define (accumulate cons table items)
  (let loop ((table table) (items items))
    (if (null? items)
	table
	(loop (cons (car items) table) (cdr items)))))

(define (define-declaration name before-bindings? parser)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (return-2 before-bindings? parser))
	(set! known-declarations
	      (cons (cons name (return-2 before-bindings? parser))
		    known-declarations)))))

(define known-declarations
  '())

;;;; Integration of System Constants

(define-declaration 'USUAL-INTEGRATIONS true
  (lambda (block table/cons table deletions)
    (let ((finish
	   (lambda (table operation names values)
	     (transmit-values
		 (if (null? deletions)
		     (return-2 names values)
		     (let deletion-loop ((names names) (values values))
		       (cond ((null? names) (return-2 '() '()))
			     ((memq (car names) deletions)
			      (deletion-loop (cdr names) (cdr values)))
			     (else
			      (cons-multiple
			       (return-2 (car names) (car values))
			       (deletion-loop (cdr names) (cdr values)))))))
	       (lambda (names values)
		 (bind/values table/cons table operation false names
			      values))))))
      (finish (finish table 'INTEGRATE
		      usual-integrations/constant-names
		      usual-integrations/constant-values)
	      'EXPAND
	      usual-integrations/expansion-names
	      usual-integrations/expansion-values))))

(define-declaration 'INTEGRATE-PRIMITIVE-PROCEDURES false
  (lambda (block table/cons table specifications)
    (transmit-values
	(let loop ((specifications specifications))
	  (if (null? specifications)
	      (return-2 '() '())
	      (cons-multiple (parse-primitive-specification
			      block
			      (car specifications))
			     (loop (cdr specifications)))))
      (lambda (names values)
	(bind/values table/cons table 'INTEGRATE true names values)))))

(define (parse-primitive-specification block specification)
  (let ((finish
	 (lambda (variable-name primitive-name)
	   (return-2 (block/lookup-name block variable-name)
		     (make-primitive-procedure
		      (constant->integration-info primitive-name))))))
    (cond ((and (pair? specification)
		(symbol? (car specification))
		(pair? (cdr specification))
		(symbol? (cadr specification))
		(null? (cddr specification)))
	   (finish (first specification) (second specification)))
	  ((symbol? specification) (finish specification specification))
	  (else (error "Bad primitive specification" specification)))))

;;;; Integration of User Code

(define-declaration 'INTEGRATE false
  (lambda (block table/cons table names)
    (bind/no-values table/cons table 'INTEGRATE true names)))

(define-declaration 'INTEGRATE-OPERATOR false
  (lambda (block table/cons table names)
    (bind/no-values table/cons table 'INTEGRATE-OPERATOR true names)))

(define-declaration 'INTEGRATE-EXTERNAL true
  (lambda (block table/cons table specifications)
    (accumulate
     (lambda (extern table)
       (bind/values table/cons table (vector-ref extern 1) false
		    (list (vector-ref extern 0))
		    (list
		     (expression->integration-info
		      (transform/expression-with-block
		       block
		       (vector-ref extern 2))))))
     table
     (mapcan read-externs-file
	     (mapcan specification->pathnames specifications)))))

(define (specification->pathnames specification)
  (let ((value
	 (scode-eval (syntax specification system-global-syntax-table)
		     (access syntax-environment syntaxer-package))))
    (if (pair? value)
	(map ->pathname value)
	(list (->pathname value)))))

(define (expression->integration-info expression)
  (lambda ()
    expression))

(define (operations->external operations environment)
  (operations/extract-external operations
    (lambda (variable operation info if-ok if-not)
      (let ((finish
	     (lambda (value)
	       (if-ok
		(vector (variable/name variable)
			operation
			(cgen/expression-with-declarations value))))))
	(if info
	    (finish info)
	    (variable/final-value variable environment finish if-not))))))