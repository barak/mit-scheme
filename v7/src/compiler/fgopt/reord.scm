#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/reord.scm,v 1.1 1988/12/12 21:33:00 cph Rel $

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

;;;; Parallel assignment problem

(declare (usual-integrations))

#|

Reordering algorithm for operands in tail recursive combinations.  The
problem is NP-hard, so the solution below is not optimal, but it does
pretty well.

The program below solves the 1-4 vars case optimally, and does an
almost perfect job on 5 (it loses in less than 2% of the cases).  The
behavior of the program is conceptually quadratic, but since lists are
used to represent the adjacency matrix (rather than bit strings), it
could perform cubically if the matrix was dense.  In practice, the
matrix is often very sparse, so quadratic is a better expectation of
performance.

The program below is guaranteed to find an ordering which requires no
temporaries if one exists.  Thus if the reordering found requires none
or one temporary, it is an optimal solution.

The algorithm is a greedy algorithm:

- It chooses a variable on which no others depend first, it then
removes it from the graph.  This guarantees the optimality when no
temporaries are needed.

- If there are none, it chooses a variable according to a set of
heuristics, and removes it from the graph.  The collection of
heuristics has been found (empirically) to be complete for n = 3 or 4,
and to do fairly well for n = 5.  All of the heuristics choose one of
the nodes with the highest degree (most dependencies + dependents)
giving preference to dependencies, dependents, or balance.

Note that "self-loops" (edges from a variable to itself) are
eliminated at the outset, since they don't have any effect on the
number of assignments of any ordering.

|#

;;;; Graph Abstraction

(define-structure (node
		   (constructor make-node
				(target
				 value
				 original-dependencies
				 original-dependents)))
  ;; An assignment representing a target variable (or static link) and
  ;; an expression which will be assigned to the target.
  (target false read-only true)
  (value false read-only true)

  ;; The set of assignments on whose targets the value of this
  ;; assignment depends.
  original-dependencies

  ;; The set of assignments whose values depend on this assignment's
  ;; target.
  original-dependents

  ;; Copies of the above; modified during the reordering algorithm.
  (dependencies (list-copy original-dependencies))
  (dependents (list-copy original-dependents)))

(define (make-node-set targets values dependency-sets)
  (map (lambda (target value dependencies)
	 (make-node target
		    value
		    dependencies
		    (let loop
			((targets targets)
			 (dependency-sets dependency-sets))
		      (cond ((null? targets)
			     '())
			    ;; Why no self-dependents?
			    ((and (not (eq? target (car targets)))
				  (memq target (car dependency-sets)))
			     (cons (car targets)
				   (loop (cdr targets)
					 (cdr dependency-sets))))
			    (else
			     (loop (cdr targets)
				   (cdr dependency-sets)))))))
       targets
       values
       dependency-sets))

(define-integrable (copy-node-set nodes)
  (map node-copy nodes))

(define (node-copy node)
  (make-node (node-target node)
	     (node-value node)
	     (node-original-dependencies node)
	     (node-original-dependents node)))

;;;; Reordering

(define (reorder-assignments nodes)
  ;; Optimize trivial cases
  (let ((n-nodes (length nodes)))
    (case n-nodes
      ((0 1)
       nodes)
      ((2)
       (if (zero? (add-up-cost nodes))
	   nodes
	   (reverse nodes)))
      ((3)
       (reorder! nodes find-index-most/dependencies))
      (else
       (let loop ((heuristics heuristics) (nodes nodes) (cost n-nodes))
	 (if (null? heuristics)
	     nodes
	     (let* ((nodes* (reorder! (copy-node-set nodes) (car heuristics)))
		    (cost* (add-up-cost nodes*)))
	       (cond ((< cost* 2) nodes*)
		     ((< cost* cost) (loop (cdr heuristics) nodes* cost*))
		     (else (loop (cdr heuristics) nodes cost))))))))))

(define (add-up-cost nodes)
  (if (null? nodes)
      0
      (let loop ((nodes nodes) (cost 0))
	(if (null? (cdr nodes))
	    cost
	    (loop (cdr nodes)
		  (if (first-node-needs-temporary? nodes) (1+ cost) cost))))))

(define (first-node-needs-temporary? nodes)
  (there-exists? (cdr nodes)
    (let ((target (node-target (car nodes))))
      (lambda (node)
	(memq target (node-original-dependencies node))))))

(define (reorder! nodes find-index)
  ;; This is expensive.  It could be done for all at once,
  ;; but for now...
  (let ((nodes (list->vector nodes)))
    (let ((last (-1+ (vector-length nodes))))
      (let loop ((index 0))
	(if (< index last)
	    (begin
	      (let* ((i (find-index nodes index last))
		     (node (vector-ref nodes i))
		     (target (node-target node)))
		(let loop ((low index))
		  (if (<= low last)
		      (begin
			(let ((node* (vector-ref nodes low)))
			  (if (not (eq? node* node))
			      (begin
				(set-node-dependencies!
				 node*
				 (delq! target (node-dependencies node*)))
				(set-node-dependents!
				 node*
				 (delq! target (node-dependents node*))))))
			(loop (1+ low)))))
		(vector-set! nodes i (vector-ref nodes index))
		(vector-set! nodes index node))
	      (loop (1+ index))))))
    (vector->list nodes)))

;;;; Heuristics

(define (find-index-maker decision)
  (lambda (nodes low high)
    (let ((node (vector-ref nodes low)))
      (if (null? (node-dependents node))
	  low
	  (let loop
	      ((i (1+ low))
	       (index low)
	       (dependencies (length (node-dependencies node)))
	       (dependents (length (node-dependents node))))
	    (if (> i high)
		index
		(let ((node (vector-ref nodes i)))
		  (if (null? (node-dependents node))
		      i
		      (let ((dependencies* (length (node-dependencies node)))
			    (dependents* (length (node-dependents node))))
			(if (decision dependencies dependents
				      dependencies* dependents*)
			    (loop (1+ i) i dependencies* dependents*)
			    (loop (1+ i)
				  index dependencies dependents)))))))))))

#|

;;; This version chooses the node with the most dependencies.
;;; Among equals it gives preference to those with the most total.

(define find-index-most-dependencies
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (if (= dependencies* dependencies)
	 (> dependents* dependents)
	 (> dependencies* dependencies)))))

;;; This version chooses the node with the most dependents.
;;; Among equals it gives preference to those with the most total.

(define find-index-most-dependents
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (if (= dependents* dependents)
	 (> dependencies* dependencies)
	 (> dependents* dependents)))))

|#

;;; This version chooses the node with the most total edges.
;;; Among equals it gives preference to those with an approximately
;;; equal number of dependencies and dependents.

(define find-index-most/balanced
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (let ((total (+ dependencies dependents))
	   (total* (+ dependencies* dependents*)))
       (if (= total* total)
	   (< (abs (- dependencies* dependents*))
	      (abs (- dependencies dependents)))
	   (> total* total))))))

;;; This version chooses the node with the most total edges.
;;; Among equals it gives preference to those with the most
;;; dependencies.

(define find-index-most/dependencies
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (let ((total (+ dependencies dependents))
	   (total* (+ dependencies* dependents*)))
       (if (= total* total)
	   (> dependencies* dependencies)
	   (> total* total))))))

;;; This version chooses the node with the most total edges.
;;; Among equals it gives preference to those with the most
;;; dependents.

(define find-index-most/dependents
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (let ((total (+ dependencies dependents))
	   (total* (+ dependencies* dependents*)))
       (if (= total* total)
	   (> dependents* dependents)
	   (> total* total))))))

;;; The following two are like the two above but have preference to
;;; the right rather than the left.

(define find-index-most/dependencies-
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (let ((total (+ dependencies dependents))
	   (total* (+ dependencies* dependents*)))
       (if (= total* total)
	   (>= dependencies* dependencies)
	   (> total* total))))))

(define find-index-most/dependents-
  (find-index-maker
   (lambda (dependencies dependents dependencies* dependents*)
     (let ((total (+ dependencies dependents))
	   (total* (+ dependencies* dependents*)))
       (if (= total* total)
	   (>= dependents* dependents)
	   (> total* total))))))

(define heuristics
  (list find-index-most/dependencies
	find-index-most/dependents
	find-index-most/dependencies-
	find-index-most/dependents-
	find-index-most/balanced))