#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Red-Black Trees
;;; package: (runtime rb-tree)

;;; Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 14, "Red-Black Trees".

;;; Properties of Red-Black Trees:
;;; 1. Every node is either red or black.
;;; 2. Every leaf (#F) is black.
;;; 3. If a node is red, then both its children are black.
;;; 4. Every simple path from a node to a descendent leaf contains the
;;;    same number of black nodes.
;;; These algorithms additionally assume:
;;; 5. The root of a tree is black.

(declare (usual-integrations))

(define-structure (tree
		   (predicate rb-tree?)
		   (constructor make-tree (key=? key<?)))
  (root #f)
  (key=? #f read-only #t)
  (key<? #f read-only #t))

(define (make-rb-tree key=? key<?)
  ;; Optimizations to work around compiler that codes known calls to
  ;; these primitives much more efficiently than unknown calls.
  (make-tree (cond ((eq? key=? eq?) (lambda (x y) (eq? x y)))
		   ((eq? key=? fix:=) (lambda (x y) (fix:= x y)))
		   ((eq? key=? flo:=) (lambda (x y) (flo:= x y)))
		   (else key=?))
	     (cond ((eq? key<? fix:<) (lambda (x y) (fix:< x y)))
		   ((eq? key<? flo:<) (lambda (x y) (flo:< x y)))
		   (else key<?))))

(define-integrable (guarantee-rb-tree tree procedure)
  (if (not (rb-tree? tree))
      (error:wrong-type-argument tree "red/black tree" procedure)))

(define-structure (node
		   (constructor make-node (key datum)))
  key
  datum
  (up #f)
  (left #f)
  (right #f)
  (color #f))

;;; The algorithms are left/right symmetric, so abstract "directions"
;;; permit code to be used for either symmetry:

(define-integrable (b->d left?)
  (if left? 'left 'right))

(define-integrable (-d d)
  (if (eq? 'left d) 'right 'left))

(define-integrable (get-link+ p d)
  (if (eq? 'left d)
      (node-left p)
      (node-right p)))

(define-integrable (set-link+! p d l)
  (if (eq? 'left d)
      (set-node-left! p l)
      (set-node-right! p l)))

(define-integrable (get-link- p d)
  (if (eq? 'right d)
      (node-left p)
      (node-right p)))

(define-integrable (set-link-! p d l)
  (if (eq? 'right d)
      (set-node-left! p l)
      (set-node-right! p l)))

(define (rotate+! tree x d)
  ;; Assumes (NOT (NOT (GET-LINK- X D))).
  (let ((y (get-link- x d)))
    (let ((beta (get-link+ y d)))
      (set-link-! x d beta)
      (if beta (set-node-up! beta x)))
    (let ((u (node-up x)))
      (set-node-up! y u)
      (cond ((not u)
	     (set-tree-root! tree y))
	    ((eq? x (get-link+ u d))
	     (set-link+! u d y))
	    (else
	     (set-link-! u d y))))
    (set-link+! y d x)
    (set-node-up! x y)))

(define-integrable (rotate-! tree x d)
  (rotate+! tree x (-d d)))

(define (rb-tree/insert! tree key datum)
  (guarantee-rb-tree tree 'rb-tree/insert!)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)) (y #f) (d #f))
      (cond ((not x)
	     (let ((z (make-node key datum)))
	       (without-interruption
		(lambda ()
		  (set-node-up! z y)
		  (cond ((not y) (set-tree-root! tree z))
			((eq? 'left d) (set-node-left! y z))
			(else (set-node-right! y z)))
		  (set-node-color! z 'red)
		  (insert-fixup! tree z)))))
	    ((key=? key (node-key x)) (set-node-datum! x datum))
	    ((key<? key (node-key x)) (loop (node-left x) x 'left))
	    (else (loop (node-right x) x 'right))))))

(define (insert-fixup! tree x)
  ;; Assumptions: X is red, and the only possible violation of the
  ;; tree properties is that (NODE-UP X) is also red.
  (let loop ((x x))
    (let ((u (node-up x)))
      (if (and u (eq? 'red (node-color u)))
	  (let ((d (b->d (eq? u (node-left (node-up u))))))
	    (let ((y (get-link- (node-up u) d)))
	      (if (and y (eq? 'red (node-color y)))
		  ;; case 1
		  (begin
		    (set-node-color! u 'black)
		    (set-node-color! y 'black)
		    (set-node-color! (node-up u) 'red)
		    (loop (node-up u)))
		  (let ((x
			 (if (eq? x (get-link- u d))
			     ;; case 2
			     (begin
			       (rotate+! tree u d)
			       u)
			     x)))
		    ;; case 3
		    (let ((u (node-up x)))
		      (set-node-color! u 'black)
		      (set-node-color! (node-up u) 'red)
		      (rotate-! tree (node-up u) d)))))))))
  (set-node-color! (tree-root tree) 'black))

(define (alist->rb-tree alist key=? key<?)
  ;; Is there a more efficient way to do this?
  (let ((tree (make-rb-tree key=? key<?)))
    (do ((alist alist (cdr alist)))
	((null? alist))
      (rb-tree/insert! tree (caar alist) (cdar alist)))
    tree))

(define (rb-tree/delete! tree key)
  (guarantee-rb-tree tree 'rb-tree/delete!)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)))
      (cond ((not x) unspecific)
	    ((key=? key (node-key x)) (delete-node! tree x))
	    ((key<? key (node-key x)) (loop (node-left x)))
	    (else (loop (node-right x)))))))

(define (delete-node! tree z)
  (without-interruption
   (lambda ()
     (let ((z
	    (if (and (node-left z) (node-right z))
		(let ((y (next-node z)))
		  (set-node-key! z (node-key y))
		  (set-node-datum! z (node-datum y))
		  y)
		z)))
       (let ((x (or (node-left z) (node-right z)))
	     (u (node-up z)))
	 (if x (set-node-up! x u))
	 (cond ((not u) (set-tree-root! tree x))
	       ((eq? z (node-left u)) (set-node-left! u x))
	       (else (set-node-right! u x)))
	 (if (eq? 'black (node-color z))
	     (delete-fixup! tree x u)))))))

(define (delete-fixup! tree x u)
  (let loop ((x x) (u u))
    (if (or (not u)
	    (and x (eq? 'red (node-color x))))
	(if x (set-node-color! x 'black))
	(let ((d (b->d (eq? x (node-left u)))))
	  (let ((w
		 (let ((w (get-link- u d)))
		   (if (eq? 'red (node-color w))
		       ;; case 1
		       (begin
			 (set-node-color! w 'black)
			 (set-node-color! u 'red)
			 (rotate+! tree u d)
			 (get-link- u d))
		       w)))
		(case-4
		 (lambda (w)
		   (set-node-color! w (node-color u))
		   (set-node-color! u 'black)
		   (set-node-color! (get-link- w d) 'black)
		   (rotate+! tree u d)
		   (set-node-color! (tree-root tree) 'black))))
	    (if (let ((n- (get-link- w d)))
		  (and n-
		       (eq? 'red (node-color n-))))
		(case-4 w)
		(let ((n+ (get-link+ w d)))
		  (if (or (not n+)
			  (eq? 'black (node-color n+)))
		      ;; case 2
		      (begin
			(set-node-color! w 'red)
			(loop u (node-up u)))
		      ;; case 3
		      (begin
			(set-node-color! n+ 'black)
			(set-node-color! w 'red)
			(rotate-! tree w d)
			(case-4 (get-link- u d)))))))))))

(define (rb-tree/lookup tree key default)
  (guarantee-rb-tree tree 'rb-tree/lookup)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)))
      (cond ((not x) default)
	    ((key=? key (node-key x)) (node-datum x))
	    ((key<? key (node-key x)) (loop (node-left x)))
	    (else (loop (node-right x)))))))

(define (rb-tree/copy tree)
  (guarantee-rb-tree tree 'rb-tree/copy)
  (let ((result (make-rb-tree (tree-key=? tree) (tree-key<? tree))))
    (set-tree-root!
     result
     (let loop ((node (tree-root tree)) (up #f))
       (and node
	    (let ((node* (make-node (node-key node) (node-datum node))))
	      (set-node-color! node* (node-color node))
	      (set-node-up! node* up)
	      (set-node-left! node* (loop (node-left node) node*))
	      (set-node-right! node* (loop (node-right node) node*))
	      node*))))
    result))

(define (rb-tree/height tree)
  (guarantee-rb-tree tree 'rb-tree/height)
  (let loop ((node (tree-root tree)))
    (if node
	(+ 1 (max (loop (node-left node)) (loop (node-right node))))
	0)))

(define (rb-tree/size tree)
  (guarantee-rb-tree tree 'rb-tree/size)
  (let loop ((node (tree-root tree)))
    (if node
	(+ 1 (loop (node-left node)) (loop (node-right node)))
	0)))

(define (rb-tree/empty? tree)
  (guarantee-rb-tree tree 'rb-tree/empty?)
  (not (tree-root tree)))

(define (rb-tree/equal? x y datum=?)
  (guarantee-rb-tree x 'rb-tree/equal?)
  (guarantee-rb-tree y 'rb-tree/equal?)
  (let ((key=? (tree-key=? x)))
    (and (eq? key=? (tree-key=? y))
	 (let loop ((nx (min-node x)) (ny (min-node y)))
	   (if (not nx)
	       (not ny)
	       (and ny
		    (key=? (node-key nx) (node-key ny))
		    (datum=? (node-datum nx) (node-datum ny))
		    (loop (next-node nx) (next-node ny))))))))

(define (rb-tree->alist tree)
  (guarantee-rb-tree tree 'rb-tree->alist)
  (let ((node (min-node tree)))
    (if node
	(let ((result (list (cons (node-key node) (node-datum node)))))
	  (let loop ((node (next-node node)) (prev result))
	    (if node
		(let ((pair (list (cons (node-key node) (node-datum node)))))
		  (set-cdr! prev pair)
		  (loop (next-node node) pair))))
	  result)
	'())))

(define (rb-tree/key-list tree)
  (guarantee-rb-tree tree 'rb-tree/key-list)
  (let ((node (min-node tree)))
    (if node
	(let ((result (list (node-key node))))
	  (let loop ((node (next-node node)) (prev result))
	    (if node
		(let ((pair (list (node-key node))))
		  (set-cdr! prev pair)
		  (loop (next-node node) pair))))
	  result)
	'())))

(define (rb-tree/datum-list tree)
  (guarantee-rb-tree tree 'rb-tree/datum-list)
  (let ((node (min-node tree)))
    (if node
	(let ((result (list (node-datum node))))
	  (let loop ((node (next-node node)) (prev result))
	    (if node
		(let ((pair (list (node-datum node))))
		  (set-cdr! prev pair)
		  (loop (next-node node) pair))))
	  result)
	'())))

(define (rb-tree/min tree default)
  (guarantee-rb-tree tree 'rb-tree/min)
  (let ((node (min-node tree)))
    (if node
	(node-key node)
	default)))

(define (rb-tree/min-datum tree default)
  (guarantee-rb-tree tree 'rb-tree/min-datum)
  (let ((node (min-node tree)))
    (if node
	(node-datum node)
	default)))

(define (rb-tree/min-pair tree)
  (guarantee-rb-tree tree 'rb-tree/min-pair)
  (let ((node (min-node tree)))
    (and node
	 (node-pair node))))

(define (rb-tree/delete-min! tree default)
  (guarantee-rb-tree tree 'rb-tree/delete-min!)
  (let ((node (min-node tree)))
    (if node
	(let ((key (node-key node)))
	  (delete-node! tree node)
	  key)
	default)))

(define (rb-tree/delete-min-datum! tree default)
  (guarantee-rb-tree tree 'rb-tree/delete-min-datum!)
  (let ((node (min-node tree)))
    (if node
	(let ((datum (node-datum node)))
	  (delete-node! tree node)
	  datum)
	default)))

(define (rb-tree/delete-min-pair! tree)
  (guarantee-rb-tree tree 'rb-tree/delete-min-pair!)
  (let ((node (min-node tree)))
    (and node
	 (let ((pair (node-pair node)))
	   (delete-node! tree node)
	   pair))))

(define (rb-tree/max tree default)
  (guarantee-rb-tree tree 'rb-tree/max)
  (let ((node (max-node tree)))
    (if node
	(node-key node)
	default)))

(define (rb-tree/max-datum tree default)
  (guarantee-rb-tree tree 'rb-tree/max-datum)
  (let ((node (max-node tree)))
    (if node
	(node-datum node)
	default)))

(define (rb-tree/max-pair tree)
  (guarantee-rb-tree tree 'rb-tree/max-pair)
  (let ((node (max-node tree)))
    (and node
	 (node-pair node))))

(define (rb-tree/delete-max! tree default)
  (guarantee-rb-tree tree 'rb-tree/delete-max!)
  (let ((node (max-node tree)))
    (if node
	(let ((key (node-key node)))
	  (delete-node! tree node)
	  key)
	default)))

(define (rb-tree/delete-max-datum! tree default)
  (guarantee-rb-tree tree 'rb-tree/delete-max-datum!)
  (let ((node (max-node tree)))
    (if node
	(let ((datum (node-datum node)))
	  (delete-node! tree node)
	  datum)
	default)))

(define (rb-tree/delete-max-pair! tree)
  (guarantee-rb-tree tree 'rb-tree/delete-max-pair!)
  (let ((node (max-node tree)))
    (and node
	 (let ((pair (node-pair node)))
	   (delete-node! tree node)
	   pair))))

(define (min-node tree)
  (and (tree-root tree)
       (let loop ((x (tree-root tree)))
	 (if (node-left x)
	     (loop (node-left x))
	     x))))

(define (max-node tree)
  (and (tree-root tree)
       (let loop ((x (tree-root tree)))
	 (if (node-right x)
	     (loop (node-right x))
	     x))))

(define (next-node x)
  (if (node-right x)
       (let loop ((x (node-right x)))
	 (if (node-left x)
	     (loop (node-left x))
	     x))
      (let loop ((x x))
	(let ((y (node-up x)))
	  (if (and y (eq? x (node-right y)))
	      (loop y)
	      y)))))

(define-integrable (node-pair node)
  (cons (node-key node) (node-datum node)))