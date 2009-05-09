#| -*-Scheme-*-

$Id: contan.scm,v 4.15 2008/01/30 20:01:44 cph Exp $

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

(define (setup-block-static-links! blocks)
  (for-each
   (lambda (block)
     (if (stack-block? block)
	 (set-block-static-link?! block (compute-block-static-link? block))))
   blocks))

(define (compute-block-static-link? block)
  ;; (and (not (block/no-free-references? block)) ...)
  (let ((parent (block-parent block)))
    (and parent
	 (cond ((stack-block? parent) (not (block-stack-link block)))
	       ((ic-block? parent) (ic-block/use-lookup? parent))
	       (else true)))))

(define (block/no-free-references? block)
  (and (for-all? (block-free-variables block)
	 (lambda (variable)
	   (or (lvalue-integrated? variable)
	       (let ((block (variable-block variable)))
		 (and (ic-block? block)
		      (not (ic-block/use-lookup? block)))))))
       (let loop ((block* block))
	 (and (not
	       (there-exists? (block-applications block*)
		 (lambda (application)
		   (let ((block*
			  (if (application/combination? application)
			      (let ((adjustment
				     (combination/frame-adjustment
				      application)))
				(and adjustment
				     (cdr adjustment)))
			      (block-popping-limit
			       (reference-context/block
				(application-context application))))))
		     (and block* (block-ancestor? block block*))))))
	      (for-all? (block-children block*) loop)))))

(define (compute-block-popping-limits block)
  (let ((external (stack-block/external-ancestor block)))
    (map->eq-set
     (lambda (join)
       (cond ((not join) external)
	     ((eq? join block) block)
	     (else (block-farthest-uncommon-ancestor block join))))
     (let ((external-lvalue (stack-block/continuation-lvalue external))
	   (ancestry (block-partial-ancestry block external)))
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
	    (cond ((lvalue-marked? lvalue)
		   '())
		  ((eq? lvalue external-lvalue)
		   (lvalue-mark! lvalue)
		   (eq-set-adjoin false
				  (join-blocks lvalue external ancestry)))
		  (else
		   (loop lvalue))))

	  (next (stack-block/continuation-lvalue block))))))))

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