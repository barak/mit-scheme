#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/contan.scm,v 4.1 1987/12/04 19:27:35 cph Exp $

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

;;;; Continuation Analysis

(declare (usual-integrations))

(package (continuation-analysis)

;;; Determine when static or dynamic links are to be used.  For static
;;; links, we compute the `block-stack-link' which is the set of
;;; blocks which might be immediately adjacent (away from the top of
;;; the stack) to the given block on the stack.  If it is possible to
;;; find the parent in a consistent way with any one of these adjacent
;;; blocks, we do not need a static link.  Otherwise, we set
;;; `block-stack-link' to the empty list and use a static link.

;;; For dynamic links, we compute the popping limit of a procedure's
;;; continuation variable, which is the farthest ancestor of the
;;; procedure's block that is be popped when invoking the
;;; continuation.  If we cannot compute the limit statically (value is
;;; #F), we must use a dynamic link.

;;; This code takes advantage of the fact that the continuation
;;; variable is not referenced in blocks other than the procedure's
;;; block.  This may change if call/cc is handled specially.

(define-export (continuation-analysis blocks procedures)
  (for-each (lambda (procedure)
	      (if (procedure-continuation? procedure)
		  (begin
		    (set-continuation/lvalues! procedure '())
		    (set-continuation/dynamic-link?! procedure false))))
	    procedures)
  (for-each (lambda (block)
	      (if (stack-block? block)
		  (analyze-continuation block)))
	    blocks)
  (for-each (lambda (block)
	      (if (stack-block? block)
		  (let ((lvalue (stack-block/continuation-lvalue block)))
		    (if (not (variable-popping-limit lvalue))
			(force-dynamic-link! lvalue)))))
	    blocks)
  (for-each (lambda (block)
	      (if (stack-block? block)
		  (lvalue-mark-clear! (stack-block/continuation-lvalue block)
				      dynamic-link-marker)))
	    blocks))

(define (force-dynamic-link! lvalue)
  (if (not (lvalue-mark-set? lvalue dynamic-link-marker))
      (begin
	(lvalue-mark-set! lvalue dynamic-link-marker)
	(for-each (lambda (continuation)
		    (if (not (continuation/dynamic-link? continuation))
			(begin
			  (set-continuation/dynamic-link?! continuation true)
			  (for-each (lambda (lvalue)
				      (if (variable-popping-limit lvalue)
					  (force-dynamic-link! lvalue)))
				    (continuation/lvalues continuation)))))
		  (lvalue-values lvalue)))))

(define dynamic-link-marker
  "dynamic-link")

(define (analyze-continuation block)
  (let ((lvalue (stack-block/continuation-lvalue block)))
    (for-each (lambda (continuation)
		(set-continuation/lvalues!
		 continuation
		 (cons lvalue (continuation/lvalues continuation))))
	      (lvalue-values lvalue))
    (set-variable-popping-limit!
     lvalue
     (if (stack-parent? block)
	 (let ((external (stack-block/external-ancestor block)))
	   (let ((joins (continuation-join-blocks block lvalue external)))
	     (set-block-stack-link! block (adjacent-blocks block lvalue joins))
	     (and (not (null? joins))
		  (null? (cdr joins))
		  (or (car joins) external))))
	 block))))

(define (adjacent-blocks block lvalue joins)
  (let ((parent (block-parent block)))
    (transmit-values
	(discriminate-items joins
			    (lambda (join)
			      (or (eq? join block)
				  (eq? join parent))))
      (lambda (internal external)
	(cond ((null? internal)
	       ;; The procedure is never invoked as a subproblem.
	       ;; Therefore its ancestor frame and all intermediate
	       ;; frames are always immediately adjacent on the stack.
	       (list parent))
	      ((and (null? external)
		    (null? (cdr internal))
		    ;; Eliminate pathological case of procedure which
		    ;; is always invoked as a subproblem of itself.
		    ;; This can be written but the code can never be
		    ;; invoked.
		    (not (block-ancestor-or-self? (car internal) block)))
	       ;; The procedure is always invoked as a subproblem, and
	       ;; all of the continuations are closed in the same
	       ;; block.  Therefore we can reach the ancestor frame by
	       ;; reference to that block.
	       (map continuation/block (lvalue-values lvalue)))
	      (else
	       ;; The relative position of the ancestor frame is not
	       ;; statically determinable.
	       '()))))))

(define (continuation-join-blocks block lvalue external)
  (let ((ancestry (memq external (block-ancestry block '()))))
    (let ((blocks
	   (map->eq-set
	    (lambda (block*)
	      (let ((ancestry* (memq external (block-ancestry block* '()))))
		(and ancestry*
		     (let loop
			 ((ancestry (cdr ancestry))
			  (ancestry* (cdr ancestry*)))
		       (cond ((null? ancestry) block)
			     ((and (not (null? ancestry*))
				   (eq? (car ancestry) (car ancestry*)))
			      (loop (cdr ancestry) (cdr ancestry*)))
			     (else (car ancestry)))))))
	    (map->eq-set continuation/closing-block
			 (lvalue-values lvalue)))))
      (if (lvalue-passed-in? lvalue)
	  (eq-set-adjoin false blocks)
	  blocks))))

)