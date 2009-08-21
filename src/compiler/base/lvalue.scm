#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Left (Hand Side) Values
;;; package: (compiler)

(declare (usual-integrations))

;; IMPORTANT: Change transform/make-lvalue and the call to
;; define-type-definition in macros.scm whenever a field is added or
;; deleted!

(define-root-type lvalue
  generation		;generation mark for graph walking
  alist			;property list
  initial-forward-links	;lvalues that sink values directly from here
  initial-backward-links ;lvalues that source values directly to here
  forward-links		;transitive closure of initial-forward-links
  backward-links	;transitive closure of initial-backward-links
  initial-values	;rvalues that are possible sources
  values-cache		;(see `lvalue-values')
  known-value		;either #F or the rvalue which is the unique value
  applications		;applications whose operators are this lvalue
  passed-in?		;true iff this lvalue gets an unknown value
  passed-out?		;true iff this lvalue passes its value to unknown place
  source-links		;backward links with circularities removed
  )

;;; Note that the rvalues stored in `initial-values', `values-cache',
;;; and `known-value' are NEVER references.

(define *lvalues*)

;;; converted to a macro.
;;; (define (make-lvalue tag . extra)
;;;   (let ((lvalue
;;; 	 (list->vector
;;; 	  (cons* tag #f '() '() '() '() '() '() 'NOT-CACHED
;;; 		 #f '() #f #f '() extra))))
;;;     (set! *lvalues* (cons lvalue *lvalues*))
;;;     lvalue))

(define (add-lvalue-application! lvalue application)
  (set-lvalue-applications! lvalue
			    (cons application
				  (lvalue-applications lvalue))))

(define-lvalue variable
  block		;block in which variable is defined
  name		;name of variable [symbol]
  assignments	;true iff variable appears in an assignment
  in-cell?	;true iff variable requires cell at runtime
  normal-offset	;offset of variable within `block'
  declarations	;list of declarations for this variable
  closed-over?	;true iff a closure references it freely.
  register	;register for parameters passed in registers
  stack-overwrite-target?
		;true iff variable is the target of a stack overwrite
  indirection	;alias for this variable (variable . boolean) or #f
  source-node	;virtual-return that initializes this variable, or #f
  )

(define continuation-variable/type variable-in-cell?)
(define set-continuation-variable/type! set-variable-in-cell?!)

(define (make-variable block name)
  (make-lvalue variable-tag block name '() #f #f '() #f #f #f #f #f))

(define variable-assoc
  (association-procedure eq? variable-name))

(define (variable-offset block variable)
  (if (closure-block? block)
      (cdr (assq variable (block-closure-offsets block)))
      (variable-normal-offset variable)))

(define-vector-tag-unparser variable-tag
  (standard-unparser (symbol->string 'VARIABLE)
    (lambda (state variable)
      (unparse-object state (variable-name variable)))))

(define-integrable (lvalue/variable? lvalue)
  (eq? (tagged-vector/tag lvalue) variable-tag))

(define-syntax define-named-variable
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let* ((name (cadr form))
	    (symbol
	     (intern (string-append "#[" (symbol->string name) "]"))))
       `(BEGIN (DEFINE-INTEGRABLE
		 (,(symbol-append 'MAKE- name '-VARIABLE) BLOCK)
		 (MAKE-VARIABLE BLOCK ',symbol))
	       (DEFINE-INTEGRABLE
		 (,(symbol-append 'VARIABLE/ name '-VARIABLE?) LVALUE)
		 (EQ? (VARIABLE-NAME LVALUE) ',symbol))
	       (DEFINE (,(symbol-append name '-VARIABLE?) LVALUE)
		 (AND (VARIABLE? LVALUE)
		      (EQ? (VARIABLE-NAME LVALUE) ',symbol))))))))

(define-named-variable continuation)
(define-named-variable value)

(define (variable/register variable)
  (let ((maybe-delayed-register (variable-register variable)))
    (if (promise? maybe-delayed-register)
	(force maybe-delayed-register)
	maybe-delayed-register)))

;;;; Linking

;;; Eventually, links may be triples consisting of a source, a sink,
;;; and a set of paths.  Each path will be an ordered sequence of
;;; actions.  Actions will keep track of what paths they are part of,
;;; and paths will keep track of what links they are part of.  But for
;;; now, this significantly cheaper representation will do.

(define (lvalue-connect! lvalue rvalue)
  (if (rvalue/reference? rvalue)
      (lvalue-connect!:lvalue lvalue (reference-lvalue rvalue))
      (lvalue-connect!:rvalue lvalue rvalue)))

(define (lvalue-connect!:rvalue lvalue rvalue)
  (if (not (memq rvalue (lvalue-initial-values lvalue)))
      (set-lvalue-initial-values! lvalue
				  (cons rvalue
					(lvalue-initial-values lvalue)))))

(define (lvalue-connect!:lvalue to from)
  (if (not (memq from (lvalue-initial-backward-links to)))
      (begin
	(set-lvalue-initial-backward-links!
	 to
	 (cons from (lvalue-initial-backward-links to)))
	(set-lvalue-initial-forward-links!
	 from
	 (cons to (lvalue-initial-forward-links from)))))
  (letrec ((connect
	    (lambda (to from)
	      (if (not (memq from (lvalue-backward-links to)))
		  (begin
		    (set-lvalue-backward-links!
		     to
		     (cons from (lvalue-backward-links to)))
		    (set-lvalue-forward-links!
		     from
		     (cons to (lvalue-forward-links from)))
		    (for-each (lambda (from) (connect to from))
			      (lvalue-backward-links from))
		    (for-each (lambda (to) (connect to from))
			      (lvalue-forward-links to)))))))
    (connect to from)))

(define (lvalue-values lvalue)
  ;; No recursion is needed here because the dataflow graph is
  ;; transitively closed when this is run.
  (if (eq? 'NOT-CACHED (lvalue-values-cache lvalue))
      (let ((values
	     (eq-set-union* (lvalue-initial-values lvalue)
			    (map lvalue-initial-values
				 (lvalue-backward-links lvalue)))))
	(set-lvalue-values-cache! lvalue values)
	values)
      (lvalue-values-cache lvalue)))

(define (reset-lvalue-cache! lvalue)
  (set-lvalue-values-cache! lvalue 'NOT-CACHED)
  (for-each (lambda (lvalue)
	      (set-lvalue-values-cache! lvalue 'NOT-CACHED))
	    (lvalue-forward-links lvalue)))

;;;; Attributes

(package (with-new-lvalue-marks lvalue-marked? lvalue-mark!)

  (define-export (with-new-lvalue-marks thunk)
    (fluid-let ((*generation* (make-generation)))
      (thunk)))

  (define-export (lvalue-marked? lvalue)
    (eq? (lvalue-generation lvalue) *generation*))

  (define-export (lvalue-mark! lvalue)
    (set-lvalue-generation! lvalue *generation*))

  (define *generation*)

  (define make-generation
    (let ((generation 0))
      (named-lambda (make-generation)
	(let ((value generation))
	  (set! generation (1+ generation))
	  value)))))

(define (lvalue-get lvalue key)
  (let ((entry (assq key (lvalue-alist lvalue))))
    (and entry
	 (cdr entry))))

(define (lvalue-put! lvalue key item)
  (let ((entry (assq key (lvalue-alist lvalue))))
    (if entry
	(set-cdr! entry item)
	(set-lvalue-alist! lvalue
			   (cons (cons key item) (lvalue-alist lvalue))))))

(define (lvalue-remove! lvalue key)
  (set-lvalue-alist! lvalue (del-assq! key (lvalue-alist lvalue))))

(define (variable-assigned! variable assignment)
  (set-variable-assignments!
   variable
   (cons assignment (variable-assignments variable))))

(define-integrable (variable-assigned? variable)
  (not (null? (variable-assignments variable))))

;; Note:
;; If integration of known block values (first class environments) is
;; ever done, the package "optimization" transformations in
;; fggen/canon and fggen/fggen may break.  There is a hidden reference
;; to the environment variable from lambda expressions closed in that
;; context.  The variable can be eliminated if there are no references
;; and there are no lambda expressions implicitely referencing it.

(define (lvalue-integrated? lvalue)
  (let ((value (lvalue-known-value lvalue)))
    (and value
	 (or (rvalue/constant? value)
	     (and (rvalue/procedure? value)
		  (procedure/virtually-open? value))
	     (lvalue-get lvalue 'INTEGRATED))
	 (if (lvalue/variable? lvalue)
	     (let ((block (variable-block lvalue)))
	       (if (stack-block? block)
		   (let ((procedure (block-procedure block)))
		     (cond ((procedure-always-known-operator? procedure)
			    #t)
			   ((or (memq lvalue
				      (cdr (procedure-required procedure)))
				(memq lvalue (procedure-optional procedure))
				(eq? lvalue (procedure-rest procedure)))
			    #f)
			   (else #t)))
		   #t))
	     #t))))

(define (variable-unused? variable)
  (or (lvalue-integrated? variable)
      (variable-indirection variable)))

(define (lvalue=? lvalue lvalue*)
  (or (eq? lvalue lvalue*)
      (eq-set-same-set? (lvalue/source-set lvalue)
			(lvalue/source-set lvalue*))))

(define (lvalue/unique-source lvalue)
  (let ((source-set (lvalue/source-set lvalue)))
    (and (pair? source-set)
	 (null? (cdr source-set))
	 (car source-set))))

(define (lvalue/source-set lvalue)
  (list-transform-positive
      (eq-set-adjoin lvalue (lvalue-backward-links lvalue))
    lvalue/source?))

(define (lvalue/external-source-set lvalue)
  (list-transform-positive
      (eq-set-adjoin lvalue (lvalue-backward-links lvalue))
    lvalue/external-source?))

(define (lvalue/source? lvalue)
  (or (lvalue/external-source? lvalue)
      (lvalue/internal-source? lvalue)))

(define-integrable (lvalue/external-source? lvalue)
  ;; (number? (lvalue-passed-in? lvalue))
  (let ((passed-in? (lvalue-passed-in? lvalue)))
    (and passed-in?
	 (not (eq? passed-in? 'INHERITED)))))

(define-integrable (lvalue/internal-source? lvalue)
  (not (null? (lvalue-initial-values lvalue))))

(define (variable-in-known-location? context variable)
  (or (variable/value-variable? variable)
      (let ((definition-block (variable-block variable)))
	(or (not (ic-block? definition-block))
	    ;; If the block has no procedure, then we know nothing
	    ;; about the locations of its bindings.
	    (let ((reference-block (reference-context/block context)))
	      (and (rvalue/procedure? (block-procedure reference-block))
		   ;; If IC reference in same block as definition,
		   ;; then incremental definitions cannot screw us.
		   (eq? reference-block definition-block)
		   ;; Make sure that IC variables are bound!  A
		   ;; variable that is not bound by the code being
		   ;; compiled still has a "definition" block, which
		   ;; is the outermost IC block of the expression in
		   ;; which the variable is referenced.
		   (memq variable
			 (block-bound-variables reference-block))))))))