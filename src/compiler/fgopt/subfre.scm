#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Subproblem Free Variables
;;; package: (compiler fg-optimizer subproblem-free-variables)

(declare (usual-integrations))

(define (compute-subproblem-free-variables parallels)
  (with-analysis-state
   (lambda ()
     (for-each (lambda (parallel)
		 (for-each
		  (lambda (subproblem)
		    (set-subproblem-free-variables! subproblem 'UNKNOWN))
		  (parallel-subproblems parallel)))
	       parallels)
     (for-each (lambda (parallel)
		 (for-each walk-subproblem (parallel-subproblems parallel)))
	       parallels))))

(define (new-subproblem/compute-free-variables! subproblem)
  (with-analysis-state (lambda () (walk-subproblem subproblem))))

(define (walk-subproblem subproblem)
  (let ((free (subproblem-free-variables subproblem)))
    (case free
      ((UNKNOWN)
       (set-subproblem-free-variables! subproblem 'BEING-COMPUTED)
       (let ((free
	      (let ((free (walk-rvalue (subproblem-rvalue subproblem))))
		(if (subproblem-canonical? subproblem)
		    (eq-set-union
		     free
		     (walk-node (subproblem-entry-node subproblem)))
		    free))))
	 (set-subproblem-free-variables! subproblem free)
	 free))
      ((BEING-COMPUTED)
       (error "loop in subproblem free-variable analysis" subproblem))
      (else
       free))))

(define (walk-procedure proc)
  (define (default)
    (list-transform-negative
	(block-free-variables (procedure-block proc))
      lvalue-integrated?))

  (define (closure)
    (eq-set-union
     (default)
     (block-bound-variables
      (block-shared-block (procedure-closing-block proc)))))

  (if (or (not (procedure/closure? proc))
	  (procedure/trivial-closure? proc))
      (default)
      (let ((how (procedure-closure-cons proc)))
	(case (car how)
	  ((NORMAL)
	   (closure))
	  ((DESCENDANT)
	   ;; This should automatically imply saving the ancestor
	   ;; for stack overwrites since that is how the free
	   ;; variables will be obtained.
	   (closure))
	  ((INDIRECTED)
	   ;; In reality, only the indirection variable or the default
	   ;; set is needed, depending on where the reference occurs.
	   ;; This is always safe, however.
	   (eq-set-adjoin (cdr how) (closure)))
	  (else
	   (error "walk-procedure: Unknown closure method" proc))))))

(define (walk-operator rvalue)
  (enumeration-case rvalue-type (tagged-vector/index rvalue)
    ((REFERENCE) (walk-lvalue (reference-lvalue rvalue) walk-operator))
    ((PROCEDURE)
     (if (procedure-continuation? rvalue)
	 (walk-next (continuation/entry-node rvalue) '())
	 (map-union walk-procedure
		    (eq-set-union (list rvalue)
				  (procedure-callees rvalue)))))
    (else '())))

(define (walk-rvalue rvalue)
  (enumeration-case rvalue-type (tagged-vector/index rvalue)
    ((REFERENCE) (walk-lvalue (reference-lvalue rvalue) walk-rvalue))
    ((PROCEDURE)
     (if (procedure-continuation? rvalue)
	 (walk-next (continuation/entry-node rvalue) '())
	 (walk-procedure rvalue)))
    (else '())))

(define (walk-lvalue lvalue walk-rvalue)
  (let ((value (lvalue-known-value lvalue)))
    (if value
	(if (lvalue-integrated? lvalue)
	    (walk-rvalue value)
	    (eq-set-adjoin lvalue (walk-rvalue value)))
	(if (and (variable? lvalue)
		 (variable-indirection lvalue))
	    (walk-lvalue (car (variable-indirection lvalue))
			 walk-rvalue)
	    (list lvalue)))))

(define *nodes*)

(define free-variables-tag
  "free-variables-tag")

(define (with-analysis-state thunk)
  (fluid-let ((*nodes* '()))
    (let ((value (with-new-node-marks thunk)))
      (for-each (lambda (node) (cfg-node-remove! node free-variables-tag))
		*nodes*)
      value)))

(define (walk-node node)
  (if (node-marked? node)
      (let ((free (cfg-node-get node free-variables-tag)))
	(if (eq? free 'BEING-COMPUTED)
	    (error "loop in node free-variable analysis" node))
	free)
      (begin
	(node-mark! node)
	(set! *nodes* (cons node *nodes*))
	(cfg-node-put! node free-variables-tag 'BEING-COMPUTED)
	(let ((free (walk-node-no-memoize node)))
	  (cfg-node-put! node free-variables-tag free)
	  free))))

(define (walk-node-no-memoize node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (walk-next (snode-next node)
		(map-union walk-subproblem (parallel-subproblems node))))
    ((APPLICATION)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-operator (application-operator node))
		    (map-union walk-rvalue (application-operands node)))))
    ((VIRTUAL-RETURN)
     (walk-next
      (snode-next node)
      (let ((operator (virtual-return-operator node))
	    (free (walk-rvalue (virtual-return-operand node))))
	(cond ((not (virtual-continuation? operator))
	       (eq-set-union (walk-rvalue operator) free))
	      ((virtual-continuation/reified? operator)
	       (eq-set-union
		(walk-rvalue (virtual-continuation/reification operator))
		free))
	      (else free)))))
    ((ASSIGNMENT)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-lvalue (assignment-lvalue node) walk-rvalue)
		    (walk-rvalue (assignment-rvalue node)))))
    ((DEFINITION)
     (walk-next
      (snode-next node)
      (eq-set-union (walk-lvalue (definition-lvalue node) walk-rvalue)
		    (walk-rvalue (definition-rvalue node)))))
    ((TRUE-TEST)
     (walk-next (pnode-consequent node)
		(walk-next (pnode-alternative node)
			   (walk-rvalue (true-test-rvalue node)))))
    ((FG-NOOP)
     (walk-next (snode-next node) '()))))

(define (walk-next next free)
  (if next
      (eq-set-union (walk-node next) free)
      free))

(define (map-union procedure items)
  (if (null? items)
      '()
      (let loop ((items (cdr items)) (set (procedure (car items))))
	(if (null? items)
	    set
	    (loop (cdr items)
		  (eq-set-union (procedure (car items)) set))))))