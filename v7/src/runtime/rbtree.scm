#| -*-Scheme-*-

$Id: rbtree.scm,v 1.3 1993/10/07 06:03:46 cph Exp $

Copyright (c) 1993 Massachusetts Institute of Technology

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
  (if left? 'LEFT 'RIGHT))

(define-integrable (-d d)
  (if (eq? 'LEFT d) 'RIGHT 'LEFT))

(define-integrable (get-link+ p d)
  (if (eq? 'LEFT d)
      (node-left p)
      (node-right p)))

(define-integrable (set-link+! p d l)
  (if (eq? 'LEFT d)
      (set-node-left! p l)
      (set-node-right! p l)))

(define-integrable (get-link- p d)
  (if (eq? 'RIGHT d)
      (node-left p)
      (node-right p)))

(define-integrable (set-link-! p d l)
  (if (eq? 'RIGHT d)
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
  (guarantee-rb-tree tree 'RB-TREE/INSERT!)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)) (y #f) (d #f))
      (cond ((not x)
	     (let ((z (make-node key datum)))
	       (without-interrupts
		(lambda ()
		  (set-node-up! z y)
		  (cond ((not y) (set-tree-root! tree z))
			((eq? 'LEFT d) (set-node-left! y z))
			(else (set-node-right! y z)))
		  (set-node-color! z 'RED)
		  (insert-fixup! tree z)))))
	    ((key=? key (node-key x)) (set-node-datum! x datum))
	    ((key<? key (node-key x)) (loop (node-left x) x 'LEFT))
	    (else (loop (node-right x) x 'RIGHT))))))

(define (insert-fixup! tree x)
  ;; Assumptions: X is red, and the only possible violation of the
  ;; tree properties is that (NODE-UP X) is also red.
  (let loop ((x x))
    (let ((u (node-up x)))
      (if (and u (eq? 'RED (node-color u)))
	  (let ((d (b->d (eq? u (node-left (node-up u))))))
	    (let ((y (get-link- (node-up u) d)))
	      (if (and y (eq? 'RED (node-color y)))
		  ;; case 1
		  (begin
		    (set-node-color! u 'BLACK)
		    (set-node-color! y 'BLACK)
		    (set-node-color! (node-up u) 'RED)
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
		      (set-node-color! u 'BLACK)
		      (set-node-color! (node-up u) 'RED)
		      (rotate-! tree (node-up u) d)))))))))
  (set-node-color! (tree-root tree) 'BLACK))

(define (alist->rb-tree alist key=? key<?)
  ;; Is there a more efficient way to do this?
  (let ((tree (make-rb-tree key=? key<?)))
    (do ((alist alist (cdr alist)))
	((null? alist))
      (rb-tree/insert! tree (caar alist) (cdar alist)))
    tree))

(define-integrable (without-interrupts thunk)
  (let ((interrupt-mask (set-interrupt-enables! interrupt-mask/gc-ok)))
    (thunk)
    (set-interrupt-enables! interrupt-mask)
    unspecific))

(define (rb-tree/delete! tree key)
  (guarantee-rb-tree tree 'RB-TREE/DELETE!)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)))
      (cond ((not x) unspecific)
	    ((key=? key (node-key x)) (delete-node! tree x))
	    ((key<? key (node-key x)) (loop (node-left x)))
	    (else (loop (node-right x)))))))

(define (delete-node! tree z)
  (without-interrupts
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
	 (if (eq? 'BLACK (node-color z))
	     (delete-fixup! tree x u)))))))

(define (delete-fixup! tree x u)
  (let loop ((x x) (u u))
    (if (or (not u)
	    (and x (eq? 'RED (node-color x))))
	(if x (set-node-color! x 'BLACK))
	(let ((d (b->d (eq? x (node-left u)))))
	  (let ((w
		 (let ((w (get-link- u d)))
		   (if (eq? 'RED (node-color w))
		       ;; case 1
		       (begin
			 (set-node-color! w 'BLACK)
			 (set-node-color! u 'RED)
			 (rotate+! tree u d)
			 (get-link- u d))
		       w)))
		(case-4
		 (lambda (w)
		   (set-node-color! w (node-color u))
		   (set-node-color! u 'BLACK)
		   (set-node-color! (get-link- w d) 'BLACK)
		   (rotate+! tree u d)
		   (set-node-color! (tree-root tree) 'BLACK))))
	    (if (let ((n- (get-link- w d)))
		  (and n-
		       (eq? 'RED (node-color n-))))
		(case-4 w)
		(let ((n+ (get-link+ w d)))
		  (if (or (not n+)
			  (eq? 'BLACK (node-color (get-link+ w d))))
		      ;; case 2
		      (begin
			(set-node-color! w 'RED)
			(loop u (node-up u)))
		      ;; case 3
		      (begin
			(set-node-color! n+ 'BLACK)
			(set-node-color! w 'RED)
			(rotate-! tree w d)
			(case-4 (get-link- u d)))))))))))

(define (rb-tree/lookup tree key default)
  (guarantee-rb-tree tree 'RB-TREE/LOOKUP)
  (let ((key=? (tree-key=? tree))
	(key<? (tree-key<? tree)))
    (let loop ((x (tree-root tree)))
      (cond ((not x) default)
	    ((key=? key (node-key x)) (node-datum x))
	    ((key<? key (node-key x)) (loop (node-left x)))
	    (else (loop (node-right x)))))))

(define (rb-tree/copy tree)
  (guarantee-rb-tree tree 'RB-TREE/COPY)
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
  (guarantee-rb-tree tree 'RB-TREE/HEIGHT)
  (let loop ((node (tree-root tree)))
    (if node
	(+ 1 (max (loop (node-left node)) (loop (node-right node))))
	0)))

(define (rb-tree/size tree)
  (guarantee-rb-tree tree 'RB-TREE/SIZE)
  (let loop ((node (tree-root tree)))
    (if node
	(+ 1 (loop (node-left node)) (loop (node-right node)))
	0)))

(define (rb-tree/empty? tree)
  (guarantee-rb-tree tree 'RB-TREE/EMPTY?)
  (not (tree-root tree)))

(define (rb-tree/equal? x y datum=?)
  (guarantee-rb-tree x 'RB-TREE/EQUAL?)
  (guarantee-rb-tree y 'RB-TREE/EQUAL?)
  (let ((key=? (tree-key=? x)))
    (and (eq? key=? (tree-key=? y))
	 (let loop ((nx (first-node x)) (ny (first-node y)))
	   (if (not nx)
	       (not ny)
	       (and ny
		    (key=? (node-key nx) (node-key ny))
		    (datum=? (node-datum nx) (node-datum ny))
		    (loop (next-node nx) (next-node ny))))))))

(define (rb-tree->alist tree)
  (guarantee-rb-tree tree 'RB-TREE->ALIST)
  (let loop ((node (first-node tree)))
    (if node
	(cons (cons (node-key node) (node-datum node))
	      (loop (next-node node)))
	'())))

(define (rb-tree/key-list tree)
  (guarantee-rb-tree tree 'RB-TREE/KEY-LIST)
  (let loop ((node (first-node tree)))
    (if node
	(cons (node-key node) (loop (next-node node)))
	'())))

(define (rb-tree/datum-list tree)
  (guarantee-rb-tree tree 'RB-TREE/DATUM-LIST)
  (let loop ((node (first-node tree)))
    (if node
	(cons (node-datum node) (loop (next-node node)))
	'())))

(define (first-node tree)
  (and (tree-root tree)
       (let loop ((x (tree-root tree)))
	 (if (node-left x)
	     (loop (node-left x))
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