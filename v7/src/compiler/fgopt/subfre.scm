#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/subfre.scm,v 1.7 1990/05/06 00:34:56 jinx Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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