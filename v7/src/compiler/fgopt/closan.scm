#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/closan.scm,v 4.1 1987/12/04 19:27:30 cph Exp $

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

;;;; Closure Analysis

(declare (usual-integrations))

#|

The closure analysis operates by identifying the "closing limit" of
each procedure, which is defined as the nearest ancestor of the
procedure's closing block which is active during the procedure's
lifetime.  The closing limit is false whenever the extent of the
procedure is not fully known, or if the procedure must be fully closed
for any reason (including canonicalization).

Procedures that are called from a closed procedure must inherit that
procedure's closing limit since only the blocks farther away than the
closing limit can be assumed to exist when those procedures are
called.

The procedure's free variables which are bound in blocks up to the
closing limit (exclusive) must be consed in the heap.  Other free
variables don't necessarily need to be allocated on the heap, provided
that there is a known way to get to them.

This analysis is maximal in that it is required for ANY closure
construction mechanism that optimizes by means of a stack, because use
of a stack associates procedure extent with block scope.  For many
simple techniques it generates more information than is needed.

|#

(package (identify-closure-limits!)

(define-export (identify-closure-limits! procedures applications assignments)
  (for-each initialize-closure-limit! procedures)
  (for-each close-application-arguments! applications)
  (for-each close-assignment-values! assignments))

(define (initialize-closure-limit! procedure)
  (if (not (procedure-continuation? procedure))
      (set-procedure-closing-limit!
       procedure
       (and (not (procedure-passed-out? procedure))
	    (procedure-closing-block procedure)))))

(define (close-application-arguments! application)
  (close-values!
   (application-operand-values application)
   (let ((procedure (rvalue-known-value (application-operator application))))
     (and procedure
	  (rvalue/procedure? procedure)
	  (procedure-always-known-operator? procedure)
	  (procedure-block procedure)))))

(define (close-assignment-values! assignment)
  (close-rvalue! (assignment-rvalue assignment)
		 (variable-block (assignment-lvalue assignment))))

(define-integrable (close-rvalue! rvalue binding-block)
  (close-values! (rvalue-values rvalue) binding-block))

(define (close-values! values binding-block)
  (for-each (lambda (value)
	      (if (and (rvalue/procedure? value)
		       (not (procedure-continuation? value)))
		  (close-procedure! value binding-block)))
	    values))

(define (close-procedure! procedure binding-block)
  (let ((closing-limit (procedure-closing-limit procedure)))
    (let ((new-closing-limit
	   (and binding-block
		closing-limit
		(block-nearest-common-ancestor binding-block closing-limit))))
      (if (not (eq? new-closing-limit closing-limit))
	  (begin
	    (set-procedure-closing-limit! procedure new-closing-limit)
	    (for-each-block-descendent! (procedure-block procedure)
	      (lambda (block)
		(for-each (lambda (application)
			    (close-rvalue! (application-operator application)
					   new-closing-limit))
			  (block-applications block)))))))))

)