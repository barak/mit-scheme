#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/blktyp.scm,v 4.4 1988/04/15 02:06:00 jinx Exp $

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
  (if (close-procedure? (block-procedure block))      (close-procedure! block)))

(define (close-procedure! block)
  (let ((procedure (block-procedure block))
	(parent (block-parent block)))
    ;; Note: this should be innocuous if there is already a closure block.
    ;; In particular, if there is a closure block which happens to be a
    ;; reference placed there by the first-class environment transformation
    ;; in fggen/fggen and fggen/canon, and it is replaced by the line below,
    ;; the presumpt first-class environment is not really used as one, so
    ;; the procedure is being "demoted" from first-class to closure.
    (set-procedure-closure-block! procedure parent)
    (((find-closure-bindings
       (lambda (closure-frame-block size)
	 (set-block-parent! block closure-frame-block)
	 (set-procedure-closure-size! procedure size)))
      parent)
     (list-transform-negative (block-free-variables block)
       (lambda (lvalue)
	 (eq? (lvalue-known-value lvalue) procedure)))
     '())
    (set-block-children! parent (delq! block (block-children parent)))
    (set-block-disowned-children!
     parent
     (cons block (block-disowned-children parent)))))

(define (find-closure-bindings receiver)
  (define (find-internal block)
    (lambda (free-variables bound-variables)
      (if (or (not block) (ic-block? block))
	  (let ((grandparent (and (not (null? free-variables)) block)))
	    (if (null? bound-variables)
		(receiver grandparent (if grandparent 1 0))
		(make-closure-block receiver
				    grandparent
				    free-variables
				    bound-variables
				    (and block (block-procedure block)))))
	  (transmit-values
	   (filter-bound-variables (block-bound-variables block)
				   free-variables
				   bound-variables)
	   (find-internal (block-parent block))))))
  find-internal)

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

;; Note: The use of closure-block-first-offset below implies that
;; closure frames are not shared between different closures.
;; This may have to change if we ever do simultaneous closing of multiple
;; procedures sharing structure.

(define (make-closure-block recvr parent free-variables bound-variables frame)
  (let ((block (make-block parent 'CLOSURE))
	(extra (if (and parent (ic-block/use-lookup? parent)) 1 0)))
    (set-block-free-variables! block free-variables)
    (set-block-bound-variables! block bound-variables)
    (set-block-frame! block
		      (and frame
			   (rvalue/procedure? frame)
			   (procedure-name frame)))
    (let loop ((variables (block-bound-variables block))
	       (offset (+ closure-block-first-offset extra))
	       (table '())
	       (size extra))
      (cond ((null? variables)
	     (set-block-closure-offsets! block table)
	     (recvr block size))
	    ((lvalue-integrated? (car variables))
	     (loop (cdr variables) offset table size))
	    (else
	     (loop (cdr variables)
		   (1+ offset)
		   (cons (cons (car variables) offset)
			 table)
		   (1+ size)))))))

)