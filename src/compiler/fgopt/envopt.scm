#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Procedure environment optimization
;;; package: (compiler fg-optimizer environment-optimization)

(declare (usual-integrations))

(define (optimize-environments! procedures&continuations)
  ;; Does this really have to ignore continuations?
  ;; Is this only because we implement continuations differently?
  (let ((procedures
	 (list-transform-negative procedures&continuations
	   procedure-continuation?)))
    (if compiler:optimize-environments?
	(begin
	  (for-each initialize-target-block! procedures)
	  (transitive-closure #f examine-procedure! procedures)
	  (for-each choose-target-block! procedures))
	(for-each
	 (lambda (proc)
	   ;; This is needed by the next pass.
	   (set-procedure-target-block! proc
					(procedure-closing-block proc)))
	 procedures))))

(define (initialize-target-block! procedure)
  (let ((block (procedure-block procedure)))
    (let loop
	((target-block (find-outermost-block block))
	 (free-vars (block-free-variables block)))
      (if (pair? free-vars)
	  (let ((value (lvalue-known-value (car free-vars)))
		(new-block (variable-block (car free-vars))))
	    ;; Should this piece of code deal with sets
	    ;; of values rather than known values only?
	    (cond ((and value (rvalue/constant? value))
		   (loop target-block (cdr free-vars)))
		  ((and value (rvalue/procedure? value))
		   (add-caller&callee! procedure value (car free-vars))
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
		   (loop target-block (cdr free-vars)))))
	  (set-procedure-target-block! procedure target-block)))))

;; Note that when this is run there are no closures yet.
;; The closure analysis happens after this pass.

(define (examine-procedure! procedure)
  (let ((original (procedure-target-block procedure))
	(block (procedure-block procedure)))
    (let loop ((dependencies (procedure-free-callees procedure))
	       (target-block original))
      ;; (constraint (block-ancestor-or-self? block target-block))
      (cond ((pair? dependencies)
	     (let ((this-block (procedure-target-block (caar dependencies))))
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
	     (enqueue-nodes! (procedure-free-callers procedure)))))))

(define (choose-target-block! procedure)
  (let ((block (procedure-block procedure))
	(parent (procedure-closing-block procedure))
	(target-block (procedure-target-block procedure)))
    ;; This now becomes `original-block-parent' of the procedure's
    ;; invocation block.
    (set-procedure-target-block! procedure parent)
    (if (not (eq? parent target-block))
	(transfer-block-child! block parent target-block))))

(define (add-caller&callee! caller callee variable)
  (if (not (procedure-continuation? callee))
      (begin
	(let ((entries (procedure-free-callees caller))
	      (block (variable-block variable)))
	  (let ((entry (assq callee entries)))
	    (if entry
		(if (not (memq block (cdr entry)))
		    (set-cdr! entry (cons block (cdr entry))))
		(set-procedure-free-callees! caller
					     (cons (list callee block)
						   entries)))))
	(let ((callers (procedure-free-callers callee)))
	  (if (not (memq caller callers))
	      (set-procedure-free-callers! callee (cons caller callers)))))))