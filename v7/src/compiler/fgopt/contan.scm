#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/contan.scm,v 4.7 1988/12/15 17:24:42 cph Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

#|

The continuation analysis is responsible for determining when static
or dynamic links are to be used.

Static links
------------

We compute the `block-stack-link': this is another block, which is
known to be immediately adjacent (away from the top of the stack) to
the given block on the stack, and is also a descendent of the parent.
If we can't compute a favorable block of this type, we set
`block-stack-link' to #F and use a static link.  Static links are
currently avoided in only two cases:

- The procedure is always invoked in a position which is tail
recursive with respect to the parent.  In this case the parent block
is the stack link.  Note that this includes the case where the
continuation is always externally supplied (passed in).

- The procedure is always invoked with a particular continuation which
has the procedure's parent as an ancestor.  The parent frame can then
be found from the continuation's frame.  The adjacent block is the
continuation's block.

Remarks:

This analysis can be improved in the following way: Multiple
continuations as in the second case above are fine as long as the
parent can be obtained from all of them by the same access path.

Dynamic links
-------------

We compute the "popping limits" of a procedure's continuation
variable.  A popping limit is the farthest ancestor of the procedure's
block that is to be popped when invoking a known continuation; what we
collect is the set of popping limits for all of the known
continuations.  If this set is not a singleton, we must use a dynamic
link.  However, even if the set is not a singleton, it is useful
information: many tail recursive combinations do not need to use the
dynamic link to adjust the stack, because they are only going to
discard that portion of the stack that is common to all of the popping
limits.

This code takes advantage of the fact that the continuation variable
is not referenced in blocks other than the procedure's block.  This
may change if call-with-current-continuation is handled specially.

|#

(define (continuation-analysis blocks)
  (for-each
   (lambda (block)
     (if (stack-block? block)
	 (begin
	   (set-block-stack-link! block (compute-block-stack-link block))
	   (let ((popping-limits (compute-block-popping-limits block)))
	     (set-block-popping-limits! block popping-limits)
	     (set-block-popping-limit! block
				       (and (not (null? popping-limits))
					    (null? (cdr popping-limits))
					    (car popping-limits)))))))
   blocks))

(define (compute-block-stack-link block)
  (and (stack-parent? block)
       (let ((lvalue (stack-block/continuation-lvalue block))
	     (parent (block-parent block)))
	 (if (with-new-lvalue-marks
	      (lambda ()
		(let ((end (stack-block/continuation-lvalue parent)))
		  (define (loop lvalue)
		    (lvalue-mark! lvalue)
		    (and (not (lvalue/external-source? lvalue))
			 (null? (lvalue-initial-values lvalue))
			 (memq end (lvalue-backward-links lvalue))
			 (for-all? (lvalue-initial-backward-links lvalue)
				   next)))

		  (define (next lvalue)
		    (if (lvalue-marked? lvalue)
			true
			(loop lvalue)))

		  (lvalue-mark! end)
		  (loop lvalue))))

	     ;; Most interesting case: we're always in a tail
	     ;; recursive position with respect to our parent.  Note
	     ;; that we didn't bother to check whether any of the
	     ;; intermediate procedures were closures: if that is
	     ;; true, we'd better be a closure as well.
	     parent

	     ;; Acceptable substitute: we're a subproblem of someone
	     ;; who is a child of the parent.
	     (let ((value (lvalue-known-value lvalue)))
	       (and value
		    (let ((block (continuation/block value)))
		      (and (block-ancestor? block parent)
			   block))))))))

(define (compute-block-popping-limits block)
  (let ((external (stack-block/external-ancestor block)))
    (map->eq-set
     (lambda (join)
       (cond ((not join) external)
	     ((eq? join block) block)
	     (else (block-farthest-uncommon-ancestor block join))))
     (let ((lvalue (stack-block/continuation-lvalue external))
	   (ancestry (block-partial-ancestry block external)))
       (eq-set-union
	(eq-set-adjoin false (join-blocks lvalue external ancestry))
	(with-new-lvalue-marks
	 (lambda ()
	   (define (loop lvalue)
	     (lvalue-mark! lvalue)
	     (if (lvalue/external-source? lvalue)
		 (error "internal continuation is external source" lvalue))
	     (eq-set-union
	      (join-blocks lvalue external ancestry)
	      (map-union next (lvalue-initial-backward-links lvalue))))

	   (define (next lvalue)
	     (if (lvalue-marked? lvalue)
		 '()
		 (loop lvalue)))

	   (lvalue-mark! lvalue)
	   (next (stack-block/continuation-lvalue block)))))))))

(define (join-blocks lvalue external ancestry)
  (map->eq-set
   (lambda (block*)
     (and (block-ancestor-or-self? block* external)
	  (let loop
	      ((ancestry ancestry)
	       (ancestry* (block-partial-ancestry block* external))
	       (join external))
	    (if (and (not (null? ancestry))
		     (not (null? ancestry*))
		     (eq? (car ancestry) (car ancestry*)))
		(loop (cdr ancestry)
		      (cdr ancestry*)
		      (car ancestry))
		join))))
   (map->eq-set block-parent
		(map continuation/block
		     (lvalue-initial-values lvalue)))))