#| -*-Scheme-*-

$Id: graph.scm,v 1.3 1995/06/27 22:01:57 adams Exp $

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

(declare (usual-integrations))

(load-option 'hash-table)

(define make-attribute make-eq-hash-table)

(define (set-attribute! object attribute value)
  (hash-table/put! attribute object value))

(define (get-attribute object attribute)
  (hash-table/get attribute object #F))

(define (adj-transpose vertices adj)
  ;; Given a graph (vertices and adjacency matrix) construct the
  ;; inverse adjacency matrix
  (define adj/T (make-attribute))
  (for-every vertices
    (lambda (v)
      (for-every (adj v)
	(lambda (u)
	  (set-attribute! u adj/T (cons v (or (get-attribute u adj/T) '())))))))
  (lambda (v)
    (or (get-attribute v adj/T) '())))

(define (strongly-connected-components vertices adj)

  ;; Inputs: a list of VERTICES, and a function ADJ from a vertex to the
  ;; adjacency list for that vertex.  Return a list of components,
  ;; where each component is a list of vertices.
  ;;
  ;; Example:
  ;;  (define vertices '(c d b a e f g h))
  ;;  (define (adj v)
  ;;    (case v
  ;;      ((a) '(b))
  ;;      ((b) '(c f e))
  ;;      ((c) '(g d))
  ;;      ((d) '(c h))
  ;;      ((e) '(f a))
  ;;      ((f) '(g))
  ;;      ((g) '(f h))
  ;;      ((h) '(h))
  ;;      (else (error "Bad vertex" v))))
  ;;  (strongly-connected-components vertices adj)
  ;;   =>   ((h) (f g) (d c) (e a b))
  ;;
  ;; Reference: Algorithm and example from: Cormen, Leiserson & Rivest,
  ;; Introduction to ALGORITHMS, p489
  
  (define (dfs-1 vertices adj)

    (define time 0)
    (define seen? (make-attribute))
    (define finish (make-attribute))

    (define (visit u)
      (set-attribute! u seen? #T)
      (for-each (lambda (v)
		  (if (not (get-attribute v seen?))
		      (visit v)))
		(adj u))
      (set! time (+ time 1))
      (set-attribute! u finish time))

    (for-each (lambda (vertex)
		(if (not (get-attribute vertex seen?))
		    (visit vertex)))
	      vertices)

    (lambda (v) (get-attribute v finish)))
  

  (define (dfs-2 vertices adj)

    (define seen? (make-attribute))
    (define components '())
    (define component '())

    (define (visit u)
      (set-attribute! u seen? #T)
      (set! component (cons u component))
      (for-each (lambda (v)
		  (if (not (get-attribute v seen?))
		      (visit v)))
		(adj u)))

    (for-each (lambda (vertex)
		(if (not (get-attribute vertex seen?))
		    (begin (set! component '())
			   (visit vertex)
			   (set! components (cons component components)))))
	      vertices)
    components)

  (let ((finish (dfs-1 vertices adj)))
    (dfs-2 (sort vertices (lambda (u v) (> (finish u) (finish v))))
	   (adj-transpose vertices adj))))


(define (distribute-component-property components component->property
				       vertex-acknowledge-property!)
  ;; For each component to something to every member of that component based on
  ;; some property of the component.
  (for-each (lambda (component)
	      (let ((property  (component->property component)))
		(for-each (lambda (vertex)
			    (vertex-acknowledge-property! vertex property))
			  component)))
	    components))


(define (s-c-c->adj components adj)
  ;; Given a list of strongly connected components and the adjacency relation
  ;; over the vertices in those components, return the adjacency matrix for
  ;; the strongly connect components themselves.
  (define new-adj (make-attribute))
  (define elements (make-attribute))
  (define (adjoin elem set)
    (if (memq elem set)
	set
	(cons elem set)))
  (define (v->s-c-c vertex) (get-attribute vertex elements))
  (define (result s-c-c)
    (or (get-attribute s-c-c new-adj)
	(error "S-C-C->ADJ: No such strongly connected component"
	       s-c-c components)))
  ;; Elements maps a vertex to the strongly connected component containing it
  (for-every components
    (lambda (component)
      (for-every component
	(lambda (vertex)
	  (set-attribute! vertex elements component)))))
  (for-every components
    (lambda (component)
      (set-attribute! component new-adj '())))
  ;; Calculate the adjacency matrix
  (for-every components
    (lambda (component)
      (for-every component
	(lambda (vertex)
	  (let ((adjacent (adj vertex)))
	    (for-every adjacent
	      (lambda (adj-vertex)
		(let ((new-component (v->s-c-c adj-vertex)))
		  (if (not (eq? new-component component))
		      (set-attribute! component new-adj
			(adjoin new-component (result component))))))))))))
  result)


(define (make-in-cycle? vertices adj)
  ;; Takes: a set (list) of vertices and an adjacency function from that
  ;; set to a list of neighbours.  Returns: a predicate on a vertex
  ;; determining if that vertex is on a cycle

  (define vertex->component (make-attribute))

  (for-each (lambda (component)
	      (for-each (lambda (vertex)
			  (set-attribute! vertex vertex->component component))
		component))
    (strongly-connected-components vertices adj))

  (lambda (vertex)
    (let ((component  (get-attribute vertex vertex->component)))
      (and (pair? component)
	   (or (pair? (cdr component))
	       (memq vertex (adj vertex)))))))


(define (make-breaks-cycle? vertices adj #!optional break-vertices)
  ;; Takes VERTICES & ADJ as above.  Decides which elemenent of a cycle are
  ;; `harmless' and which should break-points to break cycles.
  ;; BREAK-VERTICES is a list of vertices where we want to break already.

  (define seen? (make-attribute))
  ;; The seen? marker is either
  ;; . #F - never,
  ;; . #T - breaks cycle,
  ;; .  <n> found to be safe in dfs generation <n>
  (define generation 0)

  (define (visit u)
    (let ((attr (get-attribute u seen?)))
      (cond ((eq? attr #T)  #T)
	    ((eq? attr #F)
	     (set-attribute! u seen? generation)
	     (for-each visit (adj u)))
	    ((= generation attr)
	     (set-attribute! u seen? #T)
	     #T)
	    (else
	     #F))))
  
  (if (not (default-object? break-vertices))
      (for-each (lambda (u) (set-attribute! u seen? #T))  break-vertices))

  ;; slight improvement - look for trivial loops first
  (for-each (lambda (u)
	      (if (memq u (adj u))
		  (set-attribute! u seen? #T)))
	    vertices)

  (lambda (v)
    (set! generation (1+ generation))
    (visit v)
    (if (eq? #T (get-attribute v seen?))
	#T
	#F)))


(define (dfs-dag-walk vertices adj operation)
  ;; Visit all nodes in the graph defined by VERTICES and ADJ, performing
  ;; OPERATION at every vertex.  OPERATION takes the current vertex and a
  ;; list of vertices as returned by ADJ.  The DFS ensures (provided the
  ;; graph is a DAG) that OPERATION has already been called on all the
  ;; members of this list, and is visited exactly once.
  ;;
  ;; Example: sum the values over the children:
  ;; (dfs-dag-walk Vertices Adj
  ;;   (lambda (vertex children)
  ;;     (set-vertex-value!
  ;;      vertex
  ;;      (apply + (vertex-value vertex) (map vertex-value children)))))

  (define seen? (make-attribute))
  (define (visit u)
    (if (not (get-attribute u seen?))
	(let ((adj-list  (adj u)))
	  (set-attribute! u seen? #T)
	  (for-each visit adj-list)
	  (operation u adj-list))))
  (for-each visit vertices))



(define (dfs-dag-sum vertices adj function)
  ;; Returns a procedure on members of VERTICES which returns the DFS sum
  ;; function FUNCTION of a vertex.  FUNCTION takes the current vertex and
  ;; a list of values for the vertices returned by ADJ.  The DFS ensures
  ;; (provided the graph is a DAG) that FUNCTION has already been computed
  ;; for all ADJacent vertices and that FUNCTION is called at most once for
  ;; any vertex.
  ;;
  ;; Note: the procedure returned is lazy, and should be forced if your program
  ;; relies upon a side-effect produced by FUNCTION.
  ;;
  ;; Example: sum the values over the children:
  ;; ((dfs-dag-walk Vertices Adj
  ;;    (lambda (vertex children-values)
  ;;       (apply + (vertex-value vertex) chilren)))
  ;;  a-vertex) => the-sum

  (define seen? (make-attribute))
  (define value (make-attribute))
  (define (visit u)
    (if (not (get-attribute u seen?))
	(begin
	  (set-attribute! u seen? #T)
	  (let ((result  (function u (map visit (adj u)))))
	    (set-attribute! u value result)
	    result))
	(get-attribute u value)))
  vertices	;; ignored
  visit)
