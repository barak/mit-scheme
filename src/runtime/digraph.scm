#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Directed graphs
;;; package: (runtime directed-graph)

(declare (usual-integrations))

(define (make-digraph vertex-list neighbors-of)
  (guarantee list? vertex-list 'make-digraph)
  (guarantee unary-procedure? neighbors-of 'make-digraph)

  (define (vertices)
    vertex-list)

  (define (topological-sort)
    (clr:topological-sort this))

  (define (strong-components)
    (gabow:strong-components this))

  (define this
    (bundle digraph?
	    neighbors-of
	    strong-components
	    topological-sort
	    vertices))
  this)

(define digraph?
  (make-bundle-predicate 'digraph))

(define (digraph->nodes digraph make-node)
  (let ((table (make-strong-eqv-hash-table)))
    (map (lambda (vertex)
           (let ((node
                  (make-node vertex
                             (let ((neighbors (digraph 'neighbors-of vertex)))
                               (delay
                                 (map (lambda (vertex)
                                        (hash-table-ref table vertex))
                                      neighbors))))))
             (hash-table-set! table vertex node)
             node))
         (digraph 'vertices))))

(define (make-generic-node vertex neighbors-promise)
  (let ((state 'unvisited)
        (predecessor #f))

    (define (get-vertex)
      vertex)

    (define (neighbors-of)
      (force neighbors-promise))

    (define (unvisited?)
      (eq? state 'unvisited))

    (define (set-predecessor! node)
      (require-state 'unvisited)
      (if predecessor (error "Can't re-set predecessor:" node))
      (set! predecessor node)
      unspecific)

    (define (visit!)
      (require-state 'unvisited)
      (set! state 'visited)
      unspecific)

    (define (revisit!)
      (require-state 'visited 'finished))

    (define (finish!)
      (require-state 'visited)
      (set! state 'finished)
      unspecific)

    (define (describe-self)
      (require-state 'finished)
      `(,vertex
        (predecessor ,predecessor)))

    (define (require-state . expected)
      (if (not (memq state expected))
          (error "Wrong state:" state expected)))

    (bundle generic-node?
	    get-vertex
            describe-self
            finish!
            neighbors-of
            revisit!
            set-predecessor!
            unvisited?
            visit!)))

(define generic-node?
  (make-bundle-predicate 'generic-node))

(define (extend-node-maker make-node extension)
  (lambda (vertex neighbors-promise)
    (extension (make-node vertex neighbors-promise))))

(define (extend-node predicate node extension)
  (bundle-combine predicate node-combiner node extension))

(define (node-combiner name vals)
  (case name
    ((visit! revisit! finish!)
     (lambda ()
       (for-each (lambda (val) (val)) vals)))
    ((describe-self)
     (lambda ()
       (append-map (lambda (val) (val)) vals)))
    (else
     (car vals))))

(define (general-depth-first-search digraph make-node)
  (let ((nodes (digraph->nodes digraph make-node)))

    (define (visit node)
      (node 'visit!)
      (for-each (lambda (neighbor)
                  (if (neighbor 'unvisited?)
                      (begin
			(neighbor 'set-predecessor! node)
			(visit neighbor))
                      (neighbor 'revisit!)))
		(node 'neighbors-of))
      (node 'finish!))

    (for-each (lambda (node)
		(if (node 'unvisited?)
		    (visit node)))
	      nodes)
    nodes))

;;;; Topological sort

;;; Cormen, Thomas H.; Leiserson, Charles E.; Rivest, Ronald L.;
;;; Stein, Clifford (2001) [1990]. Introduction to Algorithms (2nd
;;; ed.). MIT Press and McGraw-Hill. ISBN 0-262-03293-7.

(define (clr:depth-first-search digraph)
  (general-depth-first-search digraph (clr-node-maker)))

(define (clr-node-maker)
  (let ((clock
         (let ((t 0))
           (lambda ()
             (set! t (+ t 1))
             t))))
    (extend-node-maker make-generic-node
      (lambda (delegate)
        (let ((visited)
              (finished))

          (define (visit!)
            (set! visited (clock))
            unspecific)

          (define (finish!)
            (set! finished (clock))
            unspecific)

          (define (describe-self)
            `((visited ,visited)
              (finished ,finished)))

          (extend-node clr-node?
		       delegate
                       (bundle #f visit! finish! describe-self)))))))

(define clr-node?
  (make-bundle-predicate 'clr-node))
(set-predicate<=! clr-node? generic-node?)

(define (clr:topological-sort digraph)
  (let ((accum (make-accum)))
    (general-depth-first-search digraph (clr-tsort-node-maker accum))
    (accum 'get)))

(define (clr-tsort-node-maker accum)
  (extend-node-maker (clr-node-maker)
    (lambda (delegate)
      (define (finish!)
	(accum 'add! (delegate 'get-vertex)))
      (extend-node clr-tsort-node?
		   delegate
		   (bundle #f finish!)))))

(define clr-tsort-node?
  (make-bundle-predicate 'clr-tsort-node))
(set-predicate<=! clr-tsort-node? clr-node?)

(define (make-accum)
  (let ((elts '()))

    (define (add! elt)
      (set! elts (cons elt elts))
      unspecific)

    (define (get)
      elts)

    (bundle #f add! get)))

;;; Gabow, Harold N. (2000), "Path-based depth-first search for strong
;;; and biconnected components", Information Processing Letters 74
;;; (3-4): 107-114, doi:10.1016/S0020-0190(00)00051-X

(define (gabow:strong-components digraph)
  (let ((nodes (general-depth-first-search digraph (gabow-node-maker)))
        (max-component -1))
    (for-each (let ((c0 (+ (length nodes) 1)))
                (lambda (node)
                  (let ((c (- (node 'get-index) c0)))
                    (if (> c max-component)
                        (set! max-component c))
                    (node 'set-index! c))))
              nodes)
    (refactor-strong-graph (+ max-component 1) nodes)))

(define (refactor-strong-graph n-components nodes)
  (let ((vertices (make-vector n-components '()))
        (edges (make-vector n-components '())))
    (for-each
     (lambda (node)
       (let ((c (node 'get-index)))
         (vector-set! vertices c
                      (cons (node 'get-vertex)
                            (vector-ref vertices c)))
         (vector-set! edges c
                      (lset-union =
                                  (delv! c
                                         (delete-duplicates!
                                          (map (lambda (node)
                                                 (node 'get-index))
                                               (node 'neighbors-of))
                                          =))
                                  (vector-ref edges c)))))
     nodes)
    (let ((alist
           (map (lambda (v e)
                  (cons v
                        (map (lambda (e)
                               (vector-ref vertices e))
                             e)))
                (vector->list vertices)
                (vector->list edges))))
      (make-digraph (map car alist)
                    (lambda (vertex)
                      (cdr (assq vertex alist)))))))

(define (gabow-node-maker)
  (let ((s '())
	(sn 0)
        (b '())
        (c 0))
    (extend-node-maker make-generic-node
      (lambda (delegate)
	;; Gets called once for each node:
	(set! c (+ c 1))
	(let ((index 0))

	  (define (get-index)
	    index)

	  (define (set-index! i)
	    (set! index i)
	    unspecific)

	  (define (visit!)
	    (set! s (cons this s))
	    (set! sn (+ sn 1))
	    (set! index sn)
	    (set! b (cons sn b))
	    unspecific)

	  (define (revisit!)
	    (let loop ()
	      (if (< index (car b))
		  (begin
		    (set! b (cdr b))
		    (loop)))))

	  (define (finish!)
	    (if (= index (car b))
		(begin
		  (set! b (cdr b))
		  (set! c (+ c 1))
		  (let loop ()
		    (if (<= index sn)
			(begin
			  ((car s) 'set-index! c)
			  (set! s (cdr s))
			  (set! sn (- sn 1))
			  (loop)))))))

	  (define (describe-self)
	    `((index ,index)))

	  (define this
	    (extend-node gabow-node?
			 delegate
			 (bundle #f
				 get-index set-index! visit!
				 revisit! finish! describe-self)))
	  this)))))

(define gabow-node?
  (make-bundle-predicate 'gabow-node))
(set-predicate<=! gabow-node? generic-node?)