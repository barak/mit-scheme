#| -*-Scheme-*-

$Id$

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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