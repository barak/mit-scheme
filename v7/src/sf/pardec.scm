#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/pardec.scm,v 3.6 1988/03/22 17:38:09 jrm Rel $

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
(declare (open-block-optimizations))
(declare (automagic-integrations))
(declare (eta-substitution))

(define (declarations/make-null)
  (declarations/make '() '() '()))

(define (declarations/parse block declarations)
  (transmit-values
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
		     (parser block
			     (bindings/cons block before-bindings?)
			     bindings
			     (cdr declaration))))))))
       (return-2 '() '())
       declarations)
    (lambda (before after)
      (declarations/make declarations before after))))

(define (bindings/cons block before-bindings?)
  (lambda (bindings global? operation export? names values)
    (let ((result
	   (binding/make global? operation export?
			 (if global?
			     names
			     (block/lookup-names block names true))
			 values)))
      (transmit-values bindings
	(lambda (before after)
	  (if before-bindings?
	      (return-2 (cons result before) after)
	      (return-2 before (cons result after))))))))

(declare (integrate-operator bind/general bind/values bind/no-values))

(define (bind/general table/cons table global? operation export? names values)
  (declare (integrate table/cons table global? operation export? names values))
  (table/cons table global? operation export? names values))

(define (bind/values table/cons table operation export? names values)
  (declare (integrate table/cons table operation export? names values))
  (table/cons table (not export?) operation export? names values))

(define (bind/no-values table/cons table operation export? names)
  (declare (integrate table/cons table operation export? names))
  (table/cons table false operation export? names 'NO-VALUES))

(define (declarations/known? declaration)
  (assq (car declaration) known-declarations))

(define (define-declaration name before-bindings? parser)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (return-2 before-bindings? parser))
	(set! known-declarations
	      (cons (cons name (return-2 before-bindings? parser))
		    known-declarations)))))

(define known-declarations
  '())

(define (accumulate cons table items)
  (let loop ((table table) (items items))
    (if (null? items)
	table
	(loop (cons (car items) table) (cdr items)))))

(define (declarations/binders declarations)
  (let ((procedure
	 (lambda (bindings)
	   (lambda (operations)
	     (accumulate (lambda (binding operations)
			   ((if (binding/global? binding)
				operations/bind-global
				operations/bind)
			    operations
			    (binding/operation binding)
			    (binding/export? binding)
			    (binding/names binding)
			    (binding/values binding)))
			 operations
			 bindings)))))
    (return-2 (procedure (declarations/before declarations))
	      (procedure (declarations/after declarations)))))

(define (declarations/for-each-variable declarations procedure)
  (declarations/for-each-binding declarations
    (lambda (binding)
      (if (not (binding/global? binding))
	  (for-each procedure (binding/names binding))))))

(define (declarations/for-each-binding declarations procedure)
  (for-each procedure (declarations/before declarations))
  (for-each procedure (declarations/after declarations)))

(define (declarations/map declarations per-name per-value)
  (declarations/map-binding declarations
    (lambda (binding)
      (let ((global? (binding/global? binding))
	    (names (binding/names binding))
	    (values (binding/values binding)))
	(binding/make global?
		      (binding/operation binding)
		      (binding/export? binding)
		      (if global? names (map per-name names))
		      (if (eq? values 'NO-VALUES)
			  'NO-VALUES
			  (map per-value values)))))))

(define (declarations/map-binding declarations procedure)
  (declarations/make (declarations/original declarations)
		     (map procedure (declarations/before declarations))
		     (map procedure (declarations/after declarations))))

(define (declarations/integrated-variables declarations)
  (mapcan (lambda (binding)
	    (if (and (eq? 'INTEGRATE (binding/operation binding))
		     (eq? 'NO-VALUES (binding/values binding)))
		(list-copy (binding/names binding))
		'()))
	  (declarations/after declarations)))

(declare (integrate-operator declarations/make declarations/original
			     declarations/before declarations/after))

(define (declarations/make original before after)
  (declare (integrate original before after))
  (vector original before after))

(define (declarations/original declarations)
  (declare (integrate declarations))
  (vector-ref declarations 0))

(define (declarations/before declarations)
  (declare (integrate declarations))
  (vector-ref declarations 1))

(define (declarations/after declarations)
  (declare (integrate declarations))
  (vector-ref declarations 2))

(declare (integrate-operator binding/make binding/global? binding/operation
			     binding/export? binding/names binding/values))

(define (binding/make global? operation export? names values)
  (declare (integrate global? operation export? names values))
  (vector global? operation export? names values))

(define (binding/global? binding)
  (declare (integrate binding))
  (vector-ref binding 0))

(define (binding/operation binding)
  (declare (integrate binding))
  (vector-ref binding 1))

(define (binding/export? binding)
  (declare (integrate binding))
  (vector-ref binding 2))

(define (binding/names binding)
  (declare (integrate binding))
  (vector-ref binding 3))

(define (binding/values binding)
  (declare (integrate binding))
  (vector-ref binding 4))

;;;; Integration of System Constants

(define-declaration 'USUAL-INTEGRATIONS true
  (lambda (block table/cons table deletions)
    block ; ignored
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
  block ; ignored
  (let ((finish
	 (lambda (variable-name primitive-name)
	   (return-2 variable-name
		     (constant->integration-info
		      (make-primitive-procedure primitive-name))))))
    (cond ((and (pair? specification)
		(symbol? (car specification))
		(pair? (cdr specification))
		(symbol? (cadr specification))
		(null? (cddr specification)))
	   (finish (first specification) (second specification)))
	  ((symbol? specification) (finish specification specification))
	  (else (error "Bad primitive specification" specification)))))

;;; Special declarations courtesy JRM

;; I return the operations table unmodified, but bash on the
;; block.  This actually works pretty well.

;; One problem here with this multiple values hack is that
;; table is a multiple value -- yuck!

(define-declaration 'AUTOMAGIC-INTEGRATIONS false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block 
		      (cons 'AUTOMAGIC-INTEGRATIONS (block/flags block)))
    table))

(define-declaration 'ETA-SUBSTITUTION false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block
		      (cons 'ETA-SUBSTITUTION (block/flags block)))
    table))

(define-declaration 'OPEN-BLOCK-OPTIMIZATIONS false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block
		      (cons 'OPEN-BLOCK-OPTIMIZATIONS (block/flags block)))
    table))

(define-declaration 'NO-AUTOMAGIC-INTEGRATIONS false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block 
		      (cons 'NO-AUTOMAGIC-INTEGRATIONS (block/flags block)))
    table))

(define-declaration 'NO-ETA-SUBSTITUTION false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block
		      (cons 'NO-ETA-SUBSTITUTION (block/flags block)))
    table))

(define-declaration 'NO-OPEN-BLOCK-OPTIMIZATIONS false
  (lambda (block table/cons table names)
    table/cons
    names
    (block/set-flags! block
		      (cons 'NO-OPEN-BLOCK-OPTIMIZATIONS 
			    (block/flags block)))
    table))


;;;; Integration of User Code

(define-declaration 'INTEGRATE false
  (lambda (block table/cons table names)
    block ; ignored
    (bind/no-values table/cons table 'INTEGRATE true names)))

(define-declaration 'INTEGRATE-OPERATOR false
  (lambda (block table/cons table names)
    block ; ignored
    (bind/no-values table/cons table 'INTEGRATE-OPERATOR true names)))

(define-declaration 'INTEGRATE-EXTERNAL true
  (lambda (block table/cons table specifications)
    block ; ignored
    (accumulate
     (lambda (extern table)
       (bind/values table/cons table (vector-ref extern 1) false
		    (list (vector-ref extern 0))
		    (list
		     (intern-type (vector-ref extern 2)
				  (vector-ref extern 3)))))
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

(define (operations->external operations environment)
  (operations/extract-external operations
    (lambda (variable operation info if-ok if-not)
      (let ((finish
	     (lambda (value)
	       (if-ok
		(transmit-values (copy/expression/extern value)
		  (lambda (block expression)
		    (vector (variable/name variable)
			    operation
			    block
			    expression)))))))
	(if info
	    (transmit-values info
	      (lambda (value uninterned)
		uninterned ; ignored
		(finish value)))
	    (variable/final-value variable environment finish if-not))))))

;;;; User provided expansions and processors

(define expander-evaluation-environment
  (access package/expansion
	  package/scode-optimizer))

(define-declaration 'EXPAND-OPERATOR true
  (lambda (block table/cons table expanders)
    block ; ignored
    (bind/general table/cons table false 'EXPAND false
		  (map car expanders)
		  (map (lambda (expander)
			 (eval (cadr expander)
			       expander-evaluation-environment))
		       expanders))))