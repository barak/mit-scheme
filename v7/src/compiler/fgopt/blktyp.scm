#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/blktyp.scm,v 4.1 1987/12/04 19:23:42 cph Exp $

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

;;;; Environment Type Assignment

(declare (usual-integrations))

(package (setup-block-types!)

(define-export (setup-block-types! root-block)
  (define (loop block)
    ;; **** Why is this here?  Leave comment.
    (set-block-applications! block '())
    (enumeration-case block-type (block-type block)
      ((PROCEDURE)
       (if (block-passed-out? block)
	   (block-type! block block-type/ic)
	   (begin
	     (block-type! block block-type/stack)
	     (maybe-close-procedure! block))))
      ((CONTINUATION)
       (for-each loop (block-children block)))
      ((EXPRESSION)
       (if (not (block-passed-out? block))
	   (error "Expression block not passed out" block))
       (block-type! block block-type/ic))
      (else
       (error "Illegal block type" block))))

  (define (block-type! block type)
    (set-block-type! block type)
    (for-each loop (block-children block)))

  (loop root-block))

(define (maybe-close-procedure! block)
  (let ((procedure (block-procedure block)))
    (if (close-procedure? procedure)
	(let ((parent (block-parent block)))
	  (set-procedure-closure-block! procedure parent)
	  (set-block-parent!
	   block
	   ((find-closure-bindings parent)
	    (list-transform-negative (block-free-variables block)
	      (lambda (lvalue)
		(eq? (lvalue-known-value lvalue) procedure)))
	    '()))
	  (set-block-children! parent (delq! block (block-children parent)))
	  (set-block-disowned-children!
	   parent
	   (cons block (block-disowned-children parent)))))))

(define (find-closure-bindings block)
  (lambda (free-variables bound-variables)
    (if (or (not block) (ic-block? block))
	(let ((grandparent (and (not (null? free-variables)) block)))
	  (if (null? bound-variables)
	      grandparent
	      (make-closure-block grandparent
				  free-variables
				  bound-variables
				  (and block (block-procedure block)))))
	(transmit-values
	    (filter-bound-variables (block-bound-variables block)
				    free-variables
				    bound-variables)
	  (find-closure-bindings (block-parent block))))))

(define (filter-bound-variables bindings free-variables bound-variables)
  (cond ((null? bindings)
	 (return-2 free-variables bound-variables))
	((memq (car bindings) free-variables)
	 (filter-bound-variables (cdr bindings)
				 (delq! (car bindings) free-variables)
				 (cons (car bindings) bound-variables)))
	(else
	 (filter-bound-variables (cdr bindings)
				 free-variables
				 bound-variables))))

(define (make-closure-block parent free-variables bound-variables frame)
  (let ((block (make-block parent 'CLOSURE)))
    (set-block-free-variables! block free-variables)
    (set-block-bound-variables! block bound-variables)
    (set-block-frame! block
		      (and frame
			   (rvalue/procedure? frame)
			   (procedure-name frame)))
    (set-block-closure-offsets!
     block
     (let loop
	 ((variables (block-bound-variables block))
	  (offset (if (and parent (ic-block/use-lookup? parent)) 2 1)))
       (cond ((null? variables) '())
	     ((lvalue-integrated? (car variables))
	      (loop (cdr variables) offset))
	     (else
	      (cons (cons (car variables) offset)
		    (loop (cdr variables) (1+ offset)))))))
    block))

)