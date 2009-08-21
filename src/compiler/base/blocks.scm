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

;;;; Environment model data structures
;;; package: (compiler)

(declare (usual-integrations))

#|

Interpreter compatible (hereafter, IC) blocks are vectors with an
implementation dependent number of reserved slots at the beginning,
followed by the variable bindings for that frame, in the usual order.
The parent of such a frame is always an IC block or a global block,
but extracting a pointer to that parent from the frame is again
implementation dependent and possibly a complex operation.  During the
execution of an IC procedure, the block pointer is kept in the ENV
register.

Perfect closure blocks are vectors whose slots contain the values for
the free variables in a closure procedure.  The ordering of these
slots is arbitrary.

Imperfect closure blocks are similar, except that the first slot of
the vector points to the parent, which is always an IC block.

Stack blocks are contiguous regions of the stack.  A stack block
pointer is the address of that portion of the block which is nearest
to the top of the stack (on the 68000, the most negative address in
the block.)

In closure and stack blocks, variables which the analyzer can
guarantee will not be modified have their values stored directly in
the block.  For all other variables, the binding slot in the block
contains a pointer to a cell which contains the value.

Note that blocks of type CONTINUATION never have any children.  This
is because the body of a continuation is always generated separately
from the continuation, and then "glued" into place afterwards.

|#

(define-rvalue block
  type			;block type (see below)
  parent		;lexically enclosing parent
  children		;lexically enclosed children
  disowned-children	;children whose `parent' used to be this block
  frame-size		;for stack-allocated frames, size in words
  procedure		;procedure for which this is invocation block, if any
  bound-variables	;list of variables bound by this block
  free-variables	;list of variables free in this block or any children
  variables-nontransitively-free
  			;list of variables free in this block
  declarations		;list of declarations
  applications		;list of applications lexically within this block
  interned-variables	;alist of interned SCode variable objects
  closure-offsets	;for closure block, alist of bound variable offsets
  debugging-info	;dbg-block, if used
  (stack-link		;for stack block, adjacent block on stack
   shared-block)	;for multi closures, the official block
  (static-link?		;for stack block, true iff static link to parent
   entry-number)	;for multi closures, entry number
  (popping-limits	;for stack block (see continuation analysis)
   grafted-blocks)	;for multi closures, list of blocks that share
  popping-limit		;for stack block (see continuation analysis)
  layout-frozen?	;used by frame reuse to tell parameter
			;analysis not to alter this block's layout
			;(i.e., don't make any of the block's
			;procedure's parameters be passed by register)
  type-checks		;true, false, or a list (<default> <check>
  range-checks		;  <no-check>)
  )

(define *blocks*)

(define (make-block parent type)
  (let ((block
	 (make-rvalue block-tag (enumeration/name->index block-types type)
		      parent '() '() #f #f '()'() '() '() '() '() '()
		      #f #f 'UNKNOWN 'UNKNOWN 'UNKNOWN #f
		      (if parent
			  (block-type-checks parent)
			  compiler:generate-type-checks?)
		      (if parent
			  (block-range-checks parent)
			  compiler:generate-range-checks?))))
    (if parent
	(set-block-children! parent (cons block (block-children parent))))
    (set! *blocks* (cons block *blocks*))
    block))

(define-vector-tag-unparser block-tag
  (lambda (state block)
    ((standard-unparser
      (symbol->string 'BLOCK)
      (lambda (state block)
	(unparse-object state
			(enumeration/index->name block-types
						 (block-type block)))
	(let ((procedure (block-procedure block)))
	  (if (and procedure (rvalue/procedure? procedure))
	      (begin
		(unparse-string state " ")
		(unparse-label state (procedure-label procedure)))))))
     state block)))

(define-integrable (rvalue/block? rvalue)
  (eq? (tagged-vector/tag rvalue) block-tag))

(define (add-block-application! block application)
  (set-block-applications! block
			   (cons application (block-applications block))))

(define (intern-scode-variable! block name)
  (let ((entry (assq name (block-interned-variables block))))
    (if entry
	(cdr entry)
	(let ((variable (scode/make-variable name)))
	  (set-block-interned-variables!
	   block
	   (cons (cons name variable) (block-interned-variables block)))
	  variable))))

(define block-passed-out?
  rvalue-%passed-out?)

(define (block/generate-type-checks? block primitive)
  (block/generate-checks? block primitive block-type-checks))

(define (block/generate-range-checks? block primitive)
  (block/generate-checks? block primitive block-range-checks))

(define (block/generate-checks? block primitive block-checks)
  (let ((checks (block-checks block)))
    (if (boolean? checks)
	checks
	(let ((primitive
	       (if (scode/primitive-procedure? primitive)
		   (primitive-procedure-name primitive)
		   primitive))
	      (default (car checks))
	      (do-check (cadr checks))
	      (dont-check (caddr checks)))
	  (cond ((memq primitive do-check) #t)
		((memq primitive dont-check) #f)
		(else default))))))

;;;; Block Type

(define-enumeration block-type
  (closure	;heap-allocated closing frame, compiler format
   continuation	;continuation invocation frame
   expression	;execution frame for expression (indeterminate type)
   ic		;interpreter compatible heap-allocated frame
   procedure	;invocation frame for procedure (indeterminate type)
   stack	;invocation frame for procedure, stack-allocated
   ))

(define (ic-block? block)
  (let ((type (block-type block)))
    (or (eq? type block-type/ic)
	(eq? type block-type/expression))))

(define-integrable (closure-block? block)
  (eq? (block-type block) block-type/closure))

(define-integrable (stack-block? block)
  (eq? (block-type block) block-type/stack))

(define-integrable (continuation-block? block)
  (eq? (block-type block) block-type/continuation))

(define (block/external? block)
  (and (stack-block? block)
       (not (stack-parent? block))))

(define (block/internal? block)
  (and (stack-block? block)
       (stack-parent? block)))

(define (stack-parent? block)
  (and (block-parent block)
       (stack-block? (block-parent block))))

(define (ic-block/use-lookup? block)
  (or (rvalue/procedure? (block-procedure block))
      (not compiler:cache-free-variables?)))

;;;; Block Inheritance

(define (block-ancestor-or-self? block block*)
  (or (eq? block block*)
      (block-ancestor? block block*)))

(define (block-ancestor? block block*)
  (let loop ((block (block-parent block)))
    (and block
	 (or (eq? block block*)
	     (loop (block-parent block))))))

(define-integrable (block-child? block block*)
  (eq? block (block-parent block*)))

(define-integrable (block-sibling? block block*)
  ;; Assumes that at least one block has a parent.
  (eq? (block-parent block) (block-parent block*)))

(define (block-nearest-common-ancestor block block*)
  (let loop
      ((join #f)
       (ancestry (block-ancestry block))
       (ancestry* (block-ancestry block*)))
    (if (and (pair? ancestry)
	     (pair? ancestry*)
	     (eq? (car ancestry) (car ancestry*)))
	(loop (car ancestry) (cdr ancestry) (cdr ancestry*))
	join)))

(define (block-farthest-uncommon-ancestor block block*)
  (let loop
      ((ancestry (block-ancestry block))
       (ancestry* (block-ancestry block*)))
    (and (pair? ancestry)
	 (if (and (pair? ancestry*)
		  (eq? (car ancestry) (car ancestry*)))
	     (loop (cdr ancestry) (cdr ancestry*))
	     (car ancestry)))))

(define (block-ancestry block)
  (let loop ((block (block-parent block)) (path (list block)))
    (if block
	(loop (block-parent block) (cons block path))
	path)))

(define (block-partial-ancestry block ancestor)
  ;; (assert (or (not ancestor) (block-ancestor-or-self? block ancestor)))
  (if (eq? block ancestor)
      '()
      (let loop ((block (block-parent block)) (path (list block)))
	(if (eq? block ancestor)
	    path
	    (loop (block-parent block) (cons block path))))))

(define (find-outermost-block block)
  ;; Should this check whether it is an expression/ic block or not?
  (if (block-parent block)
      (find-outermost-block (block-parent block))
      block))

(define (stack-block/external-ancestor block)
  (let ((parent (block-parent block)))
    (if (and parent (stack-block? parent))
	(stack-block/external-ancestor parent)
	block)))

(define (block/external-ancestor block)
  (if (stack-block? block)
      (stack-block/external-ancestor block)
      block))

(define (stack-block/ancestor-distance block offset join)
  (let loop ((block block) (n offset))
    (if (eq? block join)
	n
	(loop (block-parent block)
	      (+ n (block-frame-size block))))))

(define (for-each-block-descendant! block procedure)
  (let loop ((block block))
    (procedure block)
    (for-each loop (block-children block))))

(define-integrable (stack-block/static-link? block)
  (block-static-link? block))

(define-integrable (stack-block/continuation-lvalue block)
  (procedure-continuation-lvalue (block-procedure block)))

(define (block/dynamic-link? block)
  (and (stack-block? block)
       (stack-block/dynamic-link? block)))

(define (stack-block/dynamic-link? block)
  (and (stack-parent? block)
       (internal-block/dynamic-link? block)))

(define-integrable (internal-block/dynamic-link? block)
  (not (block-popping-limit block)))

(define-integrable (original-block-parent block)
  ;; This only works for the invocation blocks of procedures (not
  ;; continuations), and it assumes that all procedures' target-block
  ;; fields have been initialized (i.e. the environment optimizer has
  ;; been run).
  (let ((procedure (block-procedure block)))
    (and procedure
	 (rvalue/procedure? procedure)
	 (procedure-target-block procedure))))

(define (transfer-block-child! child block block*)
  (let ((original-parent (original-block-parent child)))
    (set-block-children! block (delq! child (block-children block)))
    (if (eq? block original-parent)
	(set-block-disowned-children!
	 block
	 (cons child (block-disowned-children block))))
    (set-block-parent! child block*)
    (if block*
	(begin
	  (set-block-children! block* (cons child (block-children block*)))
	  (if (eq? block* original-parent)
	      (set-block-disowned-children!
	       block*
	       (delq! child (block-disowned-children block*))))))))

(define-integrable (block-number-of-entries block)
  (block-entry-number block))

(define (closure-block-entry-number block)
  (if (eq? block (block-shared-block block))
      0
      (block-entry-number block)))

(define (closure-block-first-offset block)
  (let ((block* (block-shared-block block)))
    (closure-first-offset (block-entry-number block*)
			  (if (eq? block block*)
			      0
			      (block-entry-number block)))))

(define (block-nearest-closure-ancestor block)
  (let loop ((block block) (last #f))
    (and block
	 (if (stack-block? block)
	     (loop (block-parent block) block)
	     (and (closure-block? block)
		  last)))))