#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/contan.scm,v 4.2 1987/12/30 06:44:19 cph Exp $

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
;;; procedure's block that is to be popped when invoking the
;;; continuation.  If we cannot compute the limit statically (value is
;;; #F), we must use a dynamic link.

;;; This code takes advantage of the fact that the continuation
;;; variable is not referenced in blocks other than the procedure's
;;; block.  This may change if call/cc is handled specially.

(define-export (continuation-analysis blocks)
  (for-each (lambda (block)
	      (if (stack-block? block)
		  (set-variable-popping-limit!
		   (stack-block/continuation-lvalue block)
		   true)))
	    blocks)
  (for-each (lambda (block)
	      (if (stack-block? block)
		  (let ((lvalue (stack-block/continuation-lvalue block)))
		    (if (eq? (variable-popping-limit lvalue) true)
			(set-variable-popping-limit!
			 lvalue
			 (analyze-continuation block lvalue))))))
	    blocks))

(define (analyze-continuation block lvalue)
  (if (stack-parent? block)
      (let ((external (stack-block/external-ancestor block))
	    (blocks (map continuation/block (lvalue-values lvalue))))
	(let ((closing-blocks (map->eq-set block-parent blocks)))
	  (let ((join-blocks
		 (continuation-join-blocks block
					   lvalue
					   external
					   closing-blocks)))
	    (set-block-stack-link!
	     block
	     (if (null? (lvalue-initial-values lvalue))
		 ;; In this case, the procedure is always invoked
		 ;; as a reduction.
		 (block-parent block)
		 (and (null? (cdr blocks))
		      (always-subproblem? block join-blocks)
		      (not (null? closing-blocks))
		      (null? (cdr closing-blocks))
		      ;; The procedure is always invoked as a
		      ;; subproblem, all of the continuations are
		      ;; closed in the same block, and all are the
		      ;; same size.  We can consistently find the
		      ;; parent block from the continuation.
		      (car blocks))))
	    (let ((popping-limits
		   (map->eq-set
		    (lambda (join)
		      (cond ((not join) external)
			    ((eq? join block) block)
			    (else
			     (block-farthest-uncommon-ancestor block join))))
		    join-blocks)))
	      (and (not (null? popping-limits))
		   (null? (cdr popping-limits))
		   (car popping-limits))))))
      block))

(define (always-subproblem? block join-blocks)
  (and (not (null? join-blocks))
       (null? (cdr join-blocks))
       (or (eq? (car join-blocks) block)
	   (eq? (car join-blocks) (block-parent block)))))

(define (continuation-join-blocks block lvalue external closing-blocks)
  (let ((ancestry (memq external (block-ancestry block '()))))
    (let ((join-blocks
	   (map->eq-set
	    (lambda (block*)
	      (let ((ancestry* (memq external (block-ancestry block* '()))))
		(and ancestry*
		     (let loop
			 ((ancestry (cdr ancestry))
			  (ancestry* (cdr ancestry*))
			  (join (car ancestry)))
		       (if (and (not (null? ancestry))
				(not (null? ancestry*))
				(eq? (car ancestry) (car ancestry*)))
			   (loop (cdr ancestry) (cdr ancestry*) (car ancestry))
			   join)))))
	    closing-blocks)))
      (if (lvalue-passed-in? lvalue)
	  (eq-set-adjoin false join-blocks)
	  join-blocks))))

)