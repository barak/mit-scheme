#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/lvalue.scm,v 4.7 1988/11/01 04:47:24 jinx Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Left (Hand Side) Values

(declare (usual-integrations))

;; IMPORTANT: Change transform/make-lvalue and the call to
;; define-type-definition in macros.scm whenever a field is added or
;; deleted!

(define-root-type lvalue
  forward-links		;lvalues that sink values from here
  backward-links	;lvalues that source values to here
  initial-values	;rvalues that are possible sources
  values-cache		;(see `lvalue-values')
  known-value		;either #F or the rvalue which is the unique value
  applications		;applications whose operators are this lvalue
  passed-in?		;true iff this lvalue gets an unknown value
  passed-out?		;true iff this lvalue passes its value to unknown place
  marks			;attribute marks list (see `lvalue-mark-set?')
  source-links		;backward links with circularities removed
  )

;;; Note that the rvalues stored in `initial-values', `values-cache',
;;; and `known-value' are NEVER references.

(define *lvalues*)

;;; converted to a macro.
;;; (define (make-lvalue tag . extra)
;;;   (let ((lvalue
;;; 	 (list->vector
;;; 	  (cons* tag '() '() '() 'NOT-CACHED false '() false false '()
;;; 		 extra))))
;;;     (set! *lvalues* (cons lvalue *lvalues*))
;;;     lvalue))

(define (add-lvalue-application! lvalue application)
  (set-lvalue-applications! lvalue
			    (cons application
				  (lvalue-applications lvalue))))

(define-lvalue variable
  block		;block in which variable is defined
  name		;name of variable [symbol]
  assigned?	;true iff variable appears in an assignment
  in-cell?	;true iff variable requires cell at runtime
  (normal-offset ;offset of variable within `block'
   popping-limit) ;popping-limit for continuation variables
  declarations	;list of declarations for this variable
  )

(define continuation-variable/type variable-in-cell?)
(define set-continuation-variable/type! set-variable-in-cell?!)

(define (make-variable block name)
  (make-lvalue variable-tag block name false false false '()))

(define variable-assoc
  (association-procedure eq? variable-name))

(define (variable-offset block variable)
  (if (closure-block? block)
      (cdr (assq variable (block-closure-offsets block)))
      (variable-normal-offset variable)))

(define-vector-tag-unparser variable-tag
  (standard-unparser "VARIABLE"    (lambda (state variable)
      (unparse-object state (variable-name variable)))))

(define-integrable (lvalue/variable? lvalue)
  (eq? (tagged-vector/tag lvalue) variable-tag))

(let-syntax
    ((define-named-variable
      (macro (name)
	(let ((symbol
	       (string->symbol
		(string-append "#["
			       (string-downcase (symbol->string name))
			       "]"))))
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
  (define-named-variable value))

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
  (if (not (memq from (lvalue-backward-links to)))
      (begin
	(set-lvalue-backward-links! to (cons from (lvalue-backward-links to)))
	(set-lvalue-forward-links! from (cons to (lvalue-forward-links from)))
	(for-each (lambda (from)
		    (lvalue-connect!:lvalue to from))
		  (lvalue-backward-links from))
	(for-each (lambda (to)
		    (lvalue-connect!:lvalue to from))
		  (lvalue-forward-links to)))))

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

;;;; Attribute Marking

(define (lvalue-mark-set! lvalue mark)
  (if (not (memq mark (lvalue-marks lvalue)))
      (set-lvalue-marks! lvalue (cons mark (lvalue-marks lvalue)))))

(define (lvalue-mark-clear! lvalue mark)
  (set-lvalue-marks! lvalue (delq! mark (lvalue-marks lvalue))))

(define-integrable (lvalue-mark-set? lvalue mark)
  (memq mark (lvalue-marks lvalue)))
#|
(define-integrable (variable-auxiliary! variable)
  (set-variable-auxiliary?! variable true))

(define (variable-assigned! variable)
  (set-variable-assignments! variable (1+ (variable-assignments variable))))

(define (variable-assigned? variable)
  (> (variable-assignments variable)
     (if (variable-auxiliary? variable) 1 0)))
|#
(define-integrable (variable-assigned! variable)
  (set-variable-assigned?! variable true))

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
		  (procedure/virtually-open? value))))))

(define (lvalue=? lvalue lvalue*)
  (or (eq? lvalue lvalue*)
      (eq-set-same-set? (lvalue/source-set lvalue)
			(lvalue/source-set lvalue*))))

(define (lvalue/unique-source lvalue)
  (let ((source-set (lvalue/source-set lvalue)))
    (and (not (null? source-set))
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
  (eq? 'SOURCE (lvalue-passed-in? lvalue)))

(define-integrable (lvalue/internal-source? lvalue)
  (not (null? (lvalue-initial-values lvalue))))

(define (variable-in-known-location? block variable)
  (or (variable/value-variable? variable)
      (let ((definition-block (variable-block variable)))
	(or (not (ic-block? definition-block))
	    ;; If the block has no procedure, then we know nothing about
	    ;; the locations of its bindings.
	    (and (rvalue/procedure? (block-procedure block))
		 ;; If IC reference in same block as definition, then
		 ;; incremental definitions cannot screw us.
		 (eq? block definition-block)
		 ;; Make sure that IC variables are bound!  A variable
		 ;; that is not bound by the code being compiled still has
		 ;; a "definition" block, which is the outermost IC block
		 ;; of the expression in which the variable is referenced.
		 (memq variable (block-bound-variables block)))))))
