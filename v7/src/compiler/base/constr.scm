#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/constr.scm,v 1.2 1991/10/30 20:49:47 cph Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;; Procedures for managing a set of ordering constraints

(define-structure (constraint
		   (conc-name constraint/)
		   (constructor
		    &make-constraint (element)))
  (element false read-only true)
  (graph-head false)
  (afters '())
  (generation)
  (closed? true))

(define-structure (constraint-graph
		   (conc-name constraint-graph/)
		   (constructor make-constraint-graph ()))
  (entry-nodes '())
  (closed? true))

(define (make-constraint element #!optional graph-head afters)
  (let ((constraint (&make-constraint element)))
    (if (and (not (default-object? graph-head))
	     (constraint-graph? graph-head))
	(begin
	  (set-constraint/graph-head! constraint graph-head)
	  (set-constraint-graph/entry-nodes!
	   graph-head
	   (cons constraint (constraint-graph/entry-nodes graph-head)))))
    (if (not (default-object? afters))
	(for-each
	 (lambda (after) (constraint-add! constraint after))
	 afters))
    constraint))

(define (find-constraint element graph-head)

  (define (loop children)
    (if (pair? children)
	(or (search (car children))
	    (loop (cdr children)))
	false))

  (define (search constraint)
    (if (eqv? element (constraint/element constraint))
	constraint
	(loop (constraint/afters constraint))))
  
  (loop (constraint-graph/entry-nodes graph-head)))

(define (find-or-make-constraint element graph-head
				 #!optional afters)
  (or (find-constraint element graph-head)
      (if (default-object? afters)
	  (make-constraint element graph-head)
	  (make-constraint element graph-head afters))))
          

(define (constraint-add! before after)
  (if (eq? (constraint/element before) (constraint/element after))
      (error "A node cannot be constrained to come after itself" after))
  (set-constraint/afters! before (cons after (constraint/afters before)))
  (let ((c-graph (constraint/graph-head after)))
    (if c-graph
	(set-constraint-graph/entry-nodes! 
	 c-graph
	 (delq! after (constraint-graph/entry-nodes c-graph)))))
  (set-constraint/closed?! before false)
  (if (constraint/graph-head before)
      (set-constraint-graph/closed?!
       (constraint/graph-head before)
       false)))

(define (add-constraint-element! before-element after-element
				 graph-head)
  (find-or-make-constraint
   before-element
   graph-head
   (list after-element)))

(define (add-constraint-set! befores afters graph-head)
  (let ((after-constraints
	 (map (lambda (after)
		(find-or-make-constraint after graph-head))
	      afters)))
    (for-each
     (lambda (before)
       (find-or-make-constraint before graph-head after-constraints))
     befores)))

(define (close-constraint-graph! c-graph)
  (with-new-constraint-marks
   (lambda ()
     (for-each close-constraint-node!
	       (constraint-graph/entry-nodes c-graph))))
  (set-constraint-graph/closed?! c-graph true))

(define (close-constraint-node! node)
  (with-new-constraint-marks
   (lambda ()
     (&close-constraint-node! node))))

(define (&close-constraint-node! node)
  (transitively-close-dag!
   node
   constraint/afters
   (lambda (before afters)
     (set-constraint/afters!
      before
      (append
       (constraint/afters before)
       (if (memq node afters)
	   (error
	    "Illegal cycle in constraint graph involving node:"
	    node)
	   afters))))
   constraint-marked?
   (lambda (node)
     (constraint-mark! node)
     (set-constraint/closed?! node true))))

(define (transitively-close-dag! node select update! marked? mark!)
  (let transitively-close*! ((node node))
    (let ((elements (select node)))
      (if (or (null? elements) (marked? node))
	  elements
	  (begin
	    (mark! node)
	    (update! node (append-map transitively-close*! elements))
	    (select node))))))

(define (order-per-constraints elements constraint-graph)
  (order-per-constraints/extracted
   elements
   constraint-graph
   identity-procedure))

(define (order-per-constraints/extracted things
					 constraint-graph
					 element-extractor)
;;; This orders a set of things according to the constraints where the
;;; things are not elements of the constraint-graph nodes but elements
;;; can be extracted from the things by element-extractor
  (let loop ((linearized-constraints
	      (reverse-postorder
	       (constraint-graph/entry-nodes constraint-graph)
	       constraint/afters
	       with-new-constraint-marks
	       constraint-mark!
	       constraint-marked?))
	     (things things)
	     (result '()))
    (if (and (pair? linearized-constraints)
	     (pair? things))
	(let ((match (list-search-positive
			 things
		       (lambda (thing)
			 (eqv?
			  (constraint/element
			   (car linearized-constraints))
			  (element-extractor thing))))))
	  (loop (cdr linearized-constraints)
		(delv match things)
		(if (and match
			 (not (memv match result)))
		    (cons match result)
		    result)))
	(reverse! result))))

(define (legal-ordering-per-constraints? element-ordering constraint-graph)
  (let loop ((ordering element-ordering)
	     (nodes (constraint-graph/entry-nodes constraint-graph)))

    (define (depth-first-search? node)
      (if (or (null? node) (constraint-marked? node))
	  false
	  (begin
	    (constraint-mark! node)
	    (if (eq? (constraint/element node) (car ordering))
		(loop (cdr ordering) (constraint/afters node))
		(multiple-search? (constraint/afters node))))))

    (define (multiple-search? nodes)
      (if (null? nodes)
	  false
	  (or (depth-first-search? (car nodes))
	      (multiple-search? (cdr nodes)))))

    (if (null? ordering)
	true
	(with-new-constraint-marks
	 (lambda ()
	   (multiple-search? nodes))))))

(define (reverse-postorder entry-nodes get-children
			   with-new-node-marks node-mark!
			   node-marked?)

  (define result)
  
  (define (loop node)
    (node-mark! node)
    (for-each next (get-children node))
    (set! result (cons node result)))

  (define (next node)
    (and node
	 (not (node-marked? node))
	 (loop node)))
    
  (define (doit node)
    (set! result '())
    (loop node)
    (reverse! result))

  (with-new-node-marks
   (lambda ()
     (append-map! doit entry-nodes))))

(define *constraint-generation*)

(define (with-new-constraint-marks thunk)
  (fluid-let ((*constraint-generation* (make-constraint-generation)))
    (thunk)))

(define make-constraint-generation
  (let ((constraint-generation 0))
    (named-lambda (make-constraint/generation)
      (let ((value constraint-generation))
	(set! constraint-generation (1+ constraint-generation))
	value))))

(define (constraint-marked? constraint)
  (eq? (constraint/generation constraint) *constraint-generation*))

(define (constraint-mark! constraint)
  (set-constraint/generation! constraint *constraint-generation*))

