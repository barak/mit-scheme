#| -*-Scheme-*-

$Id: parass.scm,v 1.2 1995/03/08 05:14:24 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; Parallel assignment code
;;; package: (compiler)

(declare (usual-integrations))

(define (parallel-assignment dependencies)
  ;; Each dependency is a list whose car is the target and
  ;; whose cdr is the list of locations containing the (old)
  ;; values needed to compute the new contents of the target.
  (let ((pairs (map (lambda (dependency)
		      (cons (car dependency)
			    (topo-node/make dependency)))
		    dependencies)))
    (for-each
	(lambda (pair)
	  (let ((before (cdr pair)))
	    (for-each
		(lambda (dependent)
		  (let ((pair (assq dependent pairs)))
		    (if pair
			(let ((after (cdr pair)))
			  ;; For parallel assignment,
			  ;; self-dependence is irrelevant.
			  (if (not (eq? after before))
			      (begin
				(set-topo-node/before!
				 after
				 (cons before (topo-node/before after)))
				(set-topo-node/after!
				 before
				 (cons after (topo-node/after before)))))))))
	      (cdr (topo-node/contents before)))))
      pairs)
    ;; *** This should use the heuristics for n < 6 ***
    (let loop ((nodes*  (reverse (sort-topologically (map cdr pairs))))
	       (result  '())
	       (needed-to-right '()))
      (if (null? nodes*)
	  result
	  (let* ((node        (car nodes*))
		 (dependency  (topo-node/contents node))
		 (references  (cdr dependency)))
	    (loop (cdr nodes*)
		  (cons (vector (topo-node/early? node)
				dependency
				(eq-set-difference references needed-to-right))
			result)
		  (eq-set-union references needed-to-right)))))))

(define-structure (topo-node
		   (conc-name topo-node/)
		   (constructor topo-node/make (contents)))
  (contents false read-only true)
  (before   '()   read-only false)
  (after    '()   read-only false)
  (nbefore  false read-only false)
  (early?   false read-only false)
  (dequeued false read-only false))

(define (sort-topologically nodes)
  (let* ((nnodes   (length nodes))
	 (buckets  (make-vector (+ 1 nnodes) '())))
    (define (update! node)
      (set-topo-node/dequeued! node true)
      (for-each (lambda (node*)
		  (if (not (topo-node/dequeued node*))
		      (let* ((nbefore (topo-node/nbefore node*))
			     (nbefore* (- nbefore 1)))
			(set-topo-node/nbefore! node* nbefore*)
			(vector-set! buckets
				     nbefore
				     (delq node*
					   (vector-ref buckets nbefore)))
			(vector-set! buckets
				     nbefore*
				     (cons node*
					   (vector-ref buckets nbefore*))))))
	(topo-node/after node)))

    (define (phase-2 left accum)
      ;; There must be a cycle, remove an early block
      ;; (bkpt "Foo")
      (let loop ((index 1))
	(cond ((>= index nnodes)
	       (error "Could not find a node, but some are left" left))
	      ((null? (vector-ref buckets index))
	       (loop (+ index 1)))
	      (else
	       (let* ((bucket (vector-ref buckets index))
		      (node (car bucket)))
		 (set-topo-node/early?! node true)
		 (vector-set! buckets index (cdr bucket))
		 (update! node)
		 (phase-1 (- left 1) (cons node accum)))))))

    (define (phase-1 left accum)
      (cond ((= left 0)
	     (reverse accum))
	    ((null? (vector-ref buckets 0))
	     (phase-2 left accum))
	    (else
	     (let ((node (car (vector-ref buckets 0))))
	       (vector-set! buckets 0 (cdr (vector-ref buckets 0)))
	       (update! node)
	       (phase-1 (- left 1) (cons node accum))))))

    (for-each (lambda (node)
		(let ((n (length (topo-node/before node))))
		  (set-topo-node/nbefore! node n)
		  (vector-set! buckets
			       n
			       (cons node (vector-ref buckets n)))))
      nodes)
    (phase-1 nnodes '())))