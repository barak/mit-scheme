#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/envopt.scm,v 1.2 1988/11/17 05:12:25 jinx Exp $

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

;;;; Procedure environment optimization

(declare (usual-integrations))

(package (optimize-environments!)

(define-export (optimize-environments! procedures&continuations)
  ;; Does this really have to ignore continuations?
  ;; Is this only because we implement continuations differently?
  (let ((procedures (list-transform-negative
			procedures&continuations
		      procedure-continuation?)))
    (if (not compiler:optimize-environments?)
	(for-each
	 (lambda (proc)
	   ;; This is needed by the next pass.
	   (set-procedure-target-block! proc
					(procedure-closing-block proc)))
	 procedures)
	(begin
	  (for-each initialize-target-block! procedures)
	  (transitive-closure false examine-procedure! procedures)
	  (for-each choose-target-block! procedures)))))

#|
;; All the commented out code would be used if the compiler was based
;; on the concept of quantities, rather than on the concept of locations
;; (variables).  The relevant question would then be
;; "What quantities not computed internally does this code use?" rather than
;; "What locations does this code reference freely?"
;;
;; Until we understand better the relationship between circularities in the
;; control graph and assignment, we will not be able to move to the quantity
;; world (which is ultimately functional).

(define (for-each-callee! block procedure)
  (for-each-block-descendent! block
    (lambda (block*)
      (for-each (lambda (application)
		  (for-each (lambda (value)
			      (if (and (rvalue/procedure? value)
				       (not (procedure-continuation? value)))
				  (procedure value)))
			    (rvalue-values
			     (application-operator application))))
		(block-applications block*)))))

(define (check-bound-variable! procedure block variable)
  (let ((value (lvalue-known-value variable)))
    (if (and value
	     (rvalue/procedure? value)
	     ;; 1. Worry about procedures which receive their
	     ;;    descendants as arguments.  How can we distinguish
	     ;;    that from letrec in the case of children?
	     ;; 2. Do we really have to worry?  Internal
	     ;;    procedures should move as a block with the parent,
	     ;;    only depending on free variables and other
	     ;;    external stuff, and irrelevant of whether they are
	     ;;    closures or not.
	     (not (block-ancestor-or-self? (procedure-block value) block)))
	(add-caller&callee! procedure value))))

(define (check-callee! procedure block callee)
  ;; Here we do not need to worry about such things ***
  (if (not (block-ancestor-or-self? (procedure-block callee) block))
      (add-caller&callee! procedure callee)))
|#

(define (initialize-target-block! procedure)
  (let ((block (procedure-block procedure)))
    (let loop ((target-block (find-outermost-block block))
	       (free-vars (block-free-variables block)))
      (if (null? free-vars)
	  (begin
	    #|
	    ;; It seems that enabling this makes the analysis worse for no
	    ;; good reason.  I should understand why.
	    ;; Abstractly, as long as the compiler is variable/location based
	    ;; rather than quantity/fixed-point based, looking at the free
	    ;; variables should be sufficient.
	    (for-each (lambda (var)
			(check-bound-variable! procedure block var))
		      (block-bound-variables block))
	    (for-each-callee!
	     block
	     (lambda (callee)
	       (check-callee! procedure block callee)))
	    |#
	    (set-procedure-target-block! procedure target-block))
	  (let ((value (lvalue-known-value (car free-vars)))
		(new-block (variable-block (car free-vars))))
	    (cond ((and value (rvalue/constant? value))
		   (loop target-block (cdr free-vars)))
		  ((and value (rvalue/procedure? value))
		   (add-caller&callee! procedure value)
		   (loop target-block (cdr free-vars)))
		  ((block-ancestor? new-block target-block)
		   ;; The current free variable is bound in a block
		   ;; closer than the current target block.
		   ;; This block is a better (closer) limit.
		   (loop new-block (cdr free-vars)))
		  (else
		   ;; The current free variable is bound in a block
		   ;; which encloses the current target block,
		   ;; the limit is therefore the current target block.
		   (loop target-block (cdr free-vars)))))))))

;; Note that when this is run there are no closures yet.
;; The closure analysis happens after this pass.

(define (examine-procedure! procedure)
  (let ((original (procedure-target-block procedure))
	(block (procedure-block procedure)))
    (let loop ((dependencies (procedure-free-callees procedure))
	       (target-block original))
      ;; (constraint (block-ancestor-or-self? block target-block))
      (cond ((not (null? dependencies))
	     (let ((this-block (procedure-target-block (car dependencies))))
	       (if (block-ancestor-or-self? this-block block)
		   (loop (cdr dependencies) target-block)
		   (let ((merge-block
			  (block-nearest-common-ancestor block this-block)))
		     (loop (cdr dependencies)
			   (if (block-ancestor? merge-block target-block)
			       merge-block
			       target-block))))))
	    ((not (eq? target-block original))
	     (set-procedure-target-block! procedure target-block)
	     (enqueue-nodes! (procedure-free-callers procedure)))
	    (else 'DONE)))))

(define (application-is-call-to? application block)
  (and (application/combination? application)
       (let ((op (rvalue-known-value (application-operator application))))
	 (and op
	      (rvalue/procedure? op)
	      (eq? (application-block application) block)))))

(define (choose-target-block! procedure)
  (let ((callers (procedure-free-callers procedure))
	(closing-block (procedure-closing-block procedure)))
    ;; Clean up
    (if (not compiler:preserve-data-structures?)
	(begin
	  (set-procedure-free-callees! procedure '())
	  (set-procedure-free-callers! procedure '())))
    ;; The following conditional makes some cases of LET-like procedures
    ;; track their parents in order to avoid closing over the same
    ;; variables twice.
    (if (or (not (null? callers))
	    (not (procedure-always-known-operator? procedure))
	    (not (for-all?
		  (procedure-applications procedure)
		  (lambda (app)
		    (application-is-call-to? app closing-block)))))
	(let ((target-block (procedure-target-block procedure)))
	  (if (and (not (eq? closing-block target-block))
		   (block-ancestor? closing-block target-block))
	      (let ((myself (procedure-block procedure)))
		(set-procedure-target-block! procedure closing-block)
		(set-procedure-closing-block! procedure target-block)
		(set-block-children!
		 closing-block
		 (delq! myself (block-children closing-block)))
		(set-block-disowned-children!
		 closing-block
		 (cons myself (block-disowned-children closing-block)))
		(set-block-children!
		 target-block
		 (cons myself (block-children target-block))))
	      (set-procedure-target-block! procedure closing-block)))
	(set-procedure-target-block! procedure closing-block))
    'DONE))

;;; Utilities

(define (add-caller&callee! procedure on-whom)
  (if (not (procedure-continuation? on-whom))
      (begin
	(add-free-callee! procedure on-whom)
	(add-free-caller! on-whom procedure))))

(define (add-free-callee! procedure on-whom)
  (let ((bucket (procedure-free-callees procedure)))
    (cond ((null? bucket)
	   (set-procedure-free-callees! procedure (list on-whom)))
	  ((not (memq on-whom bucket))
	   (set-procedure-free-callees! procedure (cons on-whom bucket))))
    'DONE))

(define (add-free-caller! procedure on-whom)
  (let ((bucket (procedure-free-callers procedure)))
    (cond ((null? bucket)
	   (set-procedure-free-callers! procedure (list on-whom)))
	  ((not (memq on-whom bucket))
	   (set-procedure-free-callers! procedure (cons on-whom bucket))))
    'DONE))

)