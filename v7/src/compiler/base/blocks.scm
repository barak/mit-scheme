#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/blocks.scm,v 4.4 1988/11/01 04:46:18 jinx Exp $

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

;;;; Environment model data structures

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
  free-variables	;list of variables free in this block
  declarations		;list of declarations
  applications		;list of applications lexically within this block
  interned-variables	;alist of interned SCode variable objects
  closure-offsets	;for closure block, alist of bound variable offsets
  frame			;debugging information (???)
  stack-link		;for internal block, adjacent block on stack
  )

(define *blocks*)

(define (make-block parent type)
  (let ((block
	 (make-rvalue block-tag (enumeration/name->index block-types type)
		      parent '() '() false false '() '() '() '() '() '() false
		      false 'UNKNOWN)))
    (if parent
	(set-block-children! parent (cons block (block-children parent))))
    (set! *blocks* (cons block *blocks*))
    block))

(define-vector-tag-unparser block-tag
  (lambda (state block)
    ((standard-unparser
      "BLOCK"      (and (let ((procedure (block-procedure block)))
	     (and procedure (rvalue/procedure? procedure)))
	   (lambda (state block)
	     (unparse-object state
			     (procedure-label (block-procedure block))))))
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

;;;; Block Type

(define-enumeration block-type
  (closure	;heap-allocated closing frame, compiler format
   continuation	;continuation invocation frame
   expression	;execution frame for expression (indeterminate type)
   ic		;interpreter compatible heap-allocated frame
   procedure	;invocation frame for procedure (indeterminate type)
   stack	;invocation frame for procedure, stack-allocated
   ))

(define-integrable (ic-block? block)
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

(define-integrable (ic-block/use-lookup? block)
  (or (rvalue/procedure? (block-procedure block))
      (not compiler:cache-free-variables?)))

;;;; Block Inheritance

(define (block-ancestor-or-self? block block*)
  (or (eq? block block*)
      (block-ancestor? block block*)))

(define (block-ancestor? block block*)
  (define (loop block)
    (and block
	 (or (eq? block block*)
	     (loop (block-parent block)))))
  (loop (block-parent block)))

(define-integrable (block-child? block block*)
  (eq? block (block-parent block*)))

(define-integrable (block-sibling? block block*)
  ;; Assumes that at least one block has a parent.
  (eq? (block-parent block) (block-parent block*)))

(define (block-nearest-common-ancestor block block*)
  (let loop
      ((join false)
       (ancestry (block-ancestry block '()))
       (ancestry* (block-ancestry block* '())))
    (if (and (not (null? ancestry))
	     (not (null? ancestry*))
	     (eq? (car ancestry) (car ancestry*)))
	(loop (car ancestry) (cdr ancestry) (cdr ancestry*))
	join)))

(define (block-farthest-uncommon-ancestor block block*)
  (let loop
      ((ancestry (block-ancestry block '()))
       (ancestry* (block-ancestry block* '())))
    (and (not (null? ancestry))
	 (if (and (not (null? ancestry*))
		  (eq? (car ancestry) (car ancestry*)))
	     (loop (cdr ancestry) (cdr ancestry*))
	     (car ancestry)))))

(define (block-ancestry block path)
  (if (block-parent block)
      (block-ancestry (block-parent block) (cons block path))
      (cons block path)))

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

(define (for-each-block-descendent! block procedure)
  (let loop ((block block))
    (procedure block)
    (for-each loop (block-children block))))

(define-integrable (internal-block/parent-known? block)
  (block-stack-link block))

(define (stack-block/static-link? block)
  (and (block-parent block)
       (or (not (stack-block? (block-parent block)))
	   (not (internal-block/parent-known? block)))))

(define-integrable (stack-block/continuation-lvalue block)
  (procedure-continuation-lvalue (block-procedure block)))

(define (block/dynamic-link? block)
  (and (stack-block? block)
       (stack-block/dynamic-link? block)))

(define (stack-block/dynamic-link? block)
  (and (stack-parent? block)
       (internal-block/dynamic-link? block)))

(define-integrable (internal-block/dynamic-link? block)
  (not (variable-popping-limit (stack-block/continuation-lvalue block))))