#| -*-Scheme-*-

$Id: pardec.scm,v 4.6 1992/11/04 10:17:33 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
	 (open-block-optimizations)
	 (automagic-integrations)
	 (eta-substitution)
	 (integrate-external "object"))

(define (declarations/make-null)
  (declarations/make '() '() '()))

(define (declarations/parse block declarations)
  (let ((bindings
	 (accumulate
	  (lambda (bindings declaration)
	    (parse-declaration block bindings/cons bindings declaration))
	  (cons '() '())
	  declarations)))
    (declarations/make declarations (car bindings) (cdr bindings))))

(define (parse-declaration block table/conser bindings declaration)
  (let ((association (assq (car declaration) known-declarations)))
    (if (not association)
	bindings
	(let ((before-bindings? (car (cdr association)))
	      (parser (cdr (cdr association))))
	  (let ((block
		 (if before-bindings?
		     (let ((block (block/parent block)))
		       (if (block/parent block)
			   (warn "Declaration not at top level"
				 declaration))
		       block)
		     block)))
	    (parser block
		    (table/conser block before-bindings?)
		    bindings
		    (cdr declaration)))))))

(define (bindings/cons block before-bindings?)
  (lambda (bindings global? operation export? names values)
    (let ((result
	   (binding/make global? operation export?
			 (if global?
			     names
			     (block/lookup-names block names true))
			 values)))
      (if before-bindings?
	  (cons (cons result (car bindings)) (cdr bindings))
	  (cons (car bindings) (cons result (cdr bindings)))))))

(define-integrable (bind/general table/cons table global? operation export?
				 names values)
  (table/cons table global? operation export? names values))

(define-integrable (bind/values table/cons table operation export? names
				values)
  (table/cons table (not export?) operation export? names values))

(define-integrable (bind/no-values table/cons table operation export? names)
  (table/cons table false operation export? names 'NO-VALUES))

;; before-bindings? should be true if binding <name> should nullify
;; the declaration.  It should be false if a binding and the
;; declaration can "coexist".

(define (define-declaration name before-bindings? parser)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (cons before-bindings? parser))
	(set! known-declarations
	      (cons (cons name (cons before-bindings? parser))
		    known-declarations)))))

(define-integrable (declarations/known? declaration)
  (assq (car declaration) known-declarations))

(define known-declarations
  '())

(define (accumulate cons table items)
  (let loop ((table table) (items items))
    (if (null? items)
	table
	(loop (cons table (car items)) (cdr items)))))

(define (declarations/binders declarations)
  (let ((procedure
	 (lambda (bindings)
	   (lambda (operations)
	     (accumulate (lambda (operations binding)
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
    (values (procedure (declarations/before declarations))
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
  (append-map (lambda (binding)
		(if (and (eq? 'INTEGRATE (binding/operation binding))
			 (eq? 'NO-VALUES (binding/values binding)))
		    (binding/names binding)
		    '()))
	      (declarations/after declarations)))

(define-structure (declarations
		   (type vector)
		   (constructor declarations/make)
		   (conc-name declarations/))
  (original false read-only true)
  (before false read-only true)
  (after false read-only true))

(define-structure (binding
		   (type vector)
		   (constructor binding/make)
		   (conc-name binding/))
  (global? false read-only true)
  (operation false read-only true)
  (export? false read-only true)
  (names false read-only true)
  (values false read-only true))

;;;; Integration of System Constants

(define-declaration 'USUAL-INTEGRATIONS true
  (lambda (block table/cons table deletions)
    block				;ignored
    (let* ((deletions (append sf/usual-integrations-default-deletions
			      deletions))
	   (finish
	    (lambda (table operation names vals)
	      (with-values
		  (lambda ()
		    (if (null? deletions)
			(values names vals)
			(let deletion-loop ((names names) (vals vals))
			  (cond ((null? names) (values '() '()))
				((memq (car names) deletions)
				 (deletion-loop (cdr names) (cdr vals)))
				(else
				 (with-values
				     (lambda ()
				       (deletion-loop (cdr names) (cdr vals)))
				   (lambda (names* vals*)
				     (values (cons (car names) names*)
					     (cons (car vals) vals*)))))))))
		(lambda (names vals)
		  (bind/values table/cons table operation false names vals))))))
      (finish (finish table 'INTEGRATE
		      usual-integrations/constant-names
		      usual-integrations/constant-values)
	      'EXPAND
	      usual-integrations/expansion-names
	      usual-integrations/expansion-values))))

#|
The following are allowed:

symbol				; obvious.
(symbol)			; obvious.
(symbol1 symbol2)		; use symbol1 for primitive named symbol2.
(symbol number)			; primitive symbol has arity number.
(symbol1 symbol2 number)	; use symbol1 for primitive named symbol2
				;   with arity number.

|#

(define (parse-primitive-specification block specification)
  block					;ignored
  (let ((fail
	 (lambda ()
	   (error "Bad primitive specification" specification)))
	(finish
	 (lambda (variable-name arguments)
	   (values variable-name
		   (constant->integration-info
		    (apply make-primitive-procedure arguments))))))
    (cond ((symbol? specification)
	   (finish specification (list specification)))
	  ((or (not (pair? specification))
	       (not (symbol? (car specification))))
	   (fail))
	  ((null? (cdr specification))
	   (finish (car specification) specification))
	  ((not (null? (cddr specification)))
	   (if (and (null? (cdddr specification))
		    (symbol? (cadr specification))
		    (number? (caddr specification)))
	       (finish (car specification) (cdr specification))
	       (fail)))
	  ((symbol? (cadr specification))
	   (finish (car specification) (cdr specification)))
	  ((number? (cadr specification))
	   (finish (car specification) specification))
	  (else
	   (fail)))))

;;; Special declarations courtesy JRM
;;; I return the operations table unmodified, but bash on the
;;; block.  This actually works pretty well.

(for-each (lambda (flag)
	    (define-declaration flag false
	      (lambda (block table/cons table names)
		table/cons names			;ignore
		(set-block/flags! block (cons flag (block/flags block)))
		table)))
	  '(AUTOMAGIC-INTEGRATIONS
	    ETA-SUBSTITUTION
	    OPEN-BLOCK-OPTIMIZATIONS
	    NO-AUTOMAGIC-INTEGRATIONS
	    NO-ETA-SUBSTITUTION
	    NO-OPEN-BLOCK-OPTIMIZATIONS))

;;;; Integration of User Code

(define-declaration 'INTEGRATE false
  (lambda (block table/cons table names)
    block				;ignored
    (bind/no-values table/cons table 'INTEGRATE true names)))

(define-declaration 'INTEGRATE-OPERATOR false
  (lambda (block table/cons table names)
    block				;ignored
    (bind/no-values table/cons table 'INTEGRATE-OPERATOR true names)))

(define-declaration 'INTEGRATE-EXTERNAL true
  (lambda (block table/cons table specifications)
    (accumulate
     (lambda (table extern)
       (let ((operation (vector-ref extern 1))
	     (vref2 (vector-ref extern 2))
	     (vref3 (vector-ref extern 3)))
	 (if (and (eq? operation 'EXPAND)
		  (eq? vref2 '*DUMPED-EXPANDER*))
	     (parse-declaration
	      block
	      (lambda (block before-bindings?)
		block				; ignored
		(if before-bindings?
		    (warn "INTEGRATE-EXTERNAL: before-bindings expander"
			  (car vref3)))
		table/cons)
	      table
	      vref3)
	     (bind/general table/cons table true
			   operation false
			   (list (vector-ref extern 0))
			   (list (intern-type vref2 vref3))))))
     table
     (append-map! read-externs-file
		  (append-map! specification->pathnames specifications)))))

(define-declaration 'INTEGRATE-SAFELY false
  (lambda (block table/cons table names)
    block				;ignored
    (bind/no-values table/cons table 'INTEGRATE-SAFELY true names)))

(define-declaration 'IGNORE false
  (lambda (block table/cons table names)
    (declare (ignore table/cons))
    (for-each (lambda (var)
		(and var
		     (variable/can-ignore! var)))
	      (block/lookup-names block names false))
    table))

(define (specification->pathnames specification)
  (let ((value
	 (scode-eval (syntax specification system-global-syntax-table)
		     syntaxer/default-environment)))
    (if (pair? value)
	(map ->pathname value)
	(list (->pathname value)))))

(define (operations->external operations environment)
  (operations/extract-external operations
    (lambda (variable operation info if-ok if-not)
      (let ((finish
	     (lambda (value)
	       (if-ok
		(with-values (lambda () (copy/expression/extern value))
		  (lambda (block expression)
		    (vector (variable/name variable)
			    operation
			    block
			    expression))))))
	    (fail
	     (lambda ()
	       (error "operations->external: Unrecognized processor" info))))

	(cond ((not info)
	       (variable/final-value variable environment finish if-not))
	      ((integration-info? info)
	       (finish (integration-info/expression info)))
	      ((entity? info)
	       (let ((xtra (entity-extra info)))
		 (if (or (not (pair? xtra))
			 (not (eq? '*DUMPABLE-EXPANDER* (car xtra))))
		     (fail))
		 (if-ok
		  (vector (variable/name variable)
			  operation
			  '*DUMPED-EXPANDER*
			  (cdr xtra)))))
	      (else
	       (fail)))))))

;;;; User provided reductions and expansions.
;; See reduct.scm for description of REDUCE-OPERATOR and REPLACE-OPERATOR.

(define-declaration 'REDUCE-OPERATOR false
  (lambda (block table/cons table reduction-rules)
    block				;ignored
    (check-declaration-syntax 'REDUCE-OPERATOR reduction-rules)
    (bind/general table/cons table false 'EXPAND true
		  (map car reduction-rules)
		  (map (lambda (rule)
			 (dumpable-expander
			  'REDUCE-OPERATOR
			  rule
			  (reducer/make rule block)))
		       reduction-rules))))

(define-declaration 'REPLACE-OPERATOR false
  (lambda (block table/cons table replacements)
    block
    (check-declaration-syntax 'REPLACE-OPERATOR replacements)
    (bind/general table/cons table false 'EXPAND true
		  (map car replacements)
		  (map (lambda (replacement)
			 (dumpable-expander
			  'REPLACE-OPERATOR
			  replacement
			  (replacement/make replacement block)))
		       replacements))))

(define (dumpable-expander declaration text expander)
  (make-entity (lambda (self operands if-expanded if-not-expanded block)
		 self			; ignored
		 (expander operands if-expanded if-not-expanded block))
	       (cons '*DUMPABLE-EXPANDER*
		     (list declaration text))))

(define (check-declaration-syntax kind decls)
  (if (or (not (list? decls))
	  (there-exists? decls
	    (lambda (decl)
	      (or (not (pair? decl))
		  (not (list? (cdr decl)))
		  (not (symbol? (car decl)))))))
      (error "Bad declaration" kind decls)))

;;; Expansions.  These should be used with great care, and require
;;; knowing a fair amount about the internals of sf.  This declaration
;;; is purely a hook, with no convenience.

(define-declaration 'EXPAND-OPERATOR true
  (lambda (block table/cons table expanders)
    block				;ignored
    (bind/general table/cons table false 'EXPAND false
		  (map car expanders)
		  (map (lambda (expander)
			 (eval (cadr expander)
			       expander-evaluation-environment))
		       expanders))))