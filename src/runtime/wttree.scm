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


Copyright (c) 1993-1994 Stephen Adams

This program was written by Stephen Adams, based on the following
reference:

  Stephen Adams, Implemeting Sets Efficiently in a Functional
     Language, CSTR 92-10, Department of Electronics and Computer
     Science, University of Southampton, 1992.
  <http://groups.csail.mit.edu/mac/users/adams/BB/>

The data structure was originally introduced in

  J. Nievergelt and E.M. Reingold, `Binary search trees of bounded
     balance', Proceedings of the fourth ACM Symposium on Theory of
     Computing, pp. 137--142, 1972.

The algorithm proposed by Nievergelt & Reingold requires the ratio of
sizes of the two subtrees of each node to be bounded by an irrational
factor, which is slow to evaluate in integer arithmetic.  Adams
proposed a simpler balance condition involvig only a single integer
multiplication, but it turns out to be wrong, as explained in

   Yoichi Hirai and Kazuhiko Yamamoto, `Balancing weight-balanced
     trees', Journal of Functional Programming 21(3), pp. 287--307,
     2011.
   <https://yoichihirai.com/bst.pdf>

We previously used Hirai & Yamamoto's proposed parameters, but there is
a much simpler balancing condition, with much simpler analysis, which
we adopt here, described in

  Salvador Roura, `A new method for balancing binary search trees',
     Automata, Languages, and Programming, Orejas, F., Spirakis, P.,
     & van Leeuwen, J. (eds), Lecture Notes in Computer Science 2076,
     pp. 469--480, Springer, 2001.

|#

;;;; Weight-balanced tree (wt-tree) Operations
;;; package: (runtime wt-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (usual-integrations))


;;;  A tree type is a collection of those procedures that depend on the ordering
;;;  relation.

(define-structure
    (tree-type
     (conc-name tree-type/)
     (constructor %make-tree-type))
  (key<?       #f read-only #t)
  (alist->tree #f read-only #t)
  (add         #f read-only #t)
  (insert!     #f read-only #t)
  (delete      #f read-only #t)
  (delete!     #f read-only #t)
  (member?     #f read-only #t)
  (lookup      #f read-only #t)
  ;;min        ; ?  also delmin, max, delmax, delmin!, delmax!
  (split-lt    #f read-only #t)
  (split-gt    #f read-only #t)
  (union       #f read-only #t)
  (union-merge #f read-only #t)
  (intersection #f read-only #t)
  (difference  #f read-only #t)
  (subset?     #f read-only #t)
  (rank        #f read-only #t))

;;;  Tree representation
;;;
;;;  WT-TREE is a wrapper for trees of nodes
;;;
(define-structure
    (wt-tree
     (conc-name tree/)
     (constructor %make-wt-tree))
  (type #f read-only #t)
  (root #f read-only #f))

;;;  Nodes are the thing from which the real trees are built.

(define-integrable (make-node k v l r w) (vector w l k r v))
(define-integrable (node/k node) (vector-ref node 2))
(define-integrable (node/v node) (vector-ref node 4))
(define-integrable (node/l node) (vector-ref node 1))
(define-integrable (node/r node) (vector-ref node 3))
(define-integrable (node/w node) (vector-ref node 0))

(define-integrable empty  'empty)
(define-integrable (empty? x) (eq? x 'empty))

(define-integrable (node/size node)
  (if (empty? node) 0  (node/w node)))

(define-integrable (node/weight node)
  (node/size node))

(define-integrable (log2< a b)
  (and (fix:< a b) (fix:< (fix:lsh (fix:and a b) 1) b)))

(define-integrable (overweight? a b)
  (log2< a (fix:lsh b -1)))

(define-integrable (single? a b)
  (not (log2< b a)))

(define-integrable (node/singleton k v) (make-node k v empty empty 1))

(define-integrable (with-n-node node receiver)
  (receiver (node/k node) (node/v node) (node/l node) (node/r node)))


;;;
;;;  Constructors for building node trees of various complexity
;;;

(define (n-join k v l r)
  (make-node k v l r (+ 1 (node/size l) (node/size r))))
(declare (integrate-operator n-join))

(define (single-l a.k a.v x r)
  (with-n-node r
    (lambda (b.k b.v y z) (n-join b.k b.v (n-join a.k a.v x y) z))))

(define (double-l a.k a.v x r)
  (with-n-node r
    (lambda (c.k c.v r.l z)
      (with-n-node r.l
	(lambda (b.k b.v y1 y2)
	  (n-join b.k b.v
		  (n-join a.k a.v x y1)
		  (n-join c.k c.v y2 z)))))))

(define (single-r b.k b.v l z)
  (with-n-node l
    (lambda (a.k a.v x y) (n-join a.k a.v x (n-join b.k b.v y z)))))

(define (double-r c.k c.v l z)
  (with-n-node l
    (lambda (a.k a.v x l.r)
      (with-n-node l.r
	(lambda (b.k b.v y1 y2)
	  (n-join b.k b.v
		  (n-join a.k a.v x y1)
		  (n-join c.k c.v y2 z)))))))

(define (t-join k v l r)
  (let ((l.w (node/weight l))
	(r.w (node/weight r)))
    (cond ((overweight? l.w r.w)
	   ;; right is too big
	   (let ((r.l.w (node/weight (node/l r)))
		 (r.r.w (node/weight (node/r r))))
	     (if (single? r.l.w r.r.w)
		 (single-l k v l r)
		 (double-l k v l r))))
	  ((overweight? r.w l.w)
	   ;; left is too big
	   (let ((l.l.w (node/weight (node/l l)))
		 (l.r.w (node/weight (node/r l))))
	     (if (single? l.r.w l.l.w)
		 (single-r k v l r)
		 (double-r k v l r))))
	  (else (n-join k v l r)))))

;;;
;;;  Node tree Procedures that are independent of key<?
;;;

(define (node/min node)
  (cond  ((empty? node)           (error:empty 'min))
	 ((empty? (node/l node))  node)
	 (else                   (node/min (node/l node)))))

(define (node/delmin node)
  (cond ((empty? node)           (error:empty 'delmin))
	((empty? (node/l node))  (node/r node))
	(else   (t-join (node/k node) (node/v node)
			(node/delmin (node/l node)) (node/r node)))))

(define (node/concat2 node1 node2)
  (cond ((empty? node1)   node2)
	((empty? node2)   node1)
	(else
	 (let ((min-node (node/min node2)))
	   (t-join (node/k min-node) (node/v min-node)
		   node1 (node/delmin node2))))))

(define (node/inorder-fold procedure base node)
  (define (fold base node)
    (if (empty? node)
	base
	(with-n-node node
	  (lambda (k v l r)
	    (fold (procedure k v (fold base r)) l)))))
  (fold base node))

(define (node/for-each procedure node)
  (if (not (empty? node))
      (with-n-node node
	(lambda (k v l r)
	  (node/for-each procedure l)
	  (procedure k v)
	  (node/for-each procedure r)))))

(define (node/height node)
  (if (empty? node)
      0
      (+ 1 (max (node/height (node/l node)) (node/height (node/r node))))))

(define (node/index node index caller)
  (define (loop node index)
    (let ((size.l  (node/size (node/l node))))
      (cond ((< index size.l)  (loop (node/l node) index))
	    ((> index size.l)  (loop (node/r node) (- index (+ 1 size.l))))
	    (else              node))))
  (let ((bound  (node/size node)))
    (if (not (and (<= 0 index) (< index bound)))
	(error:bad-range-argument index caller))
    (loop node index)))

(define (error:empty owner)
  ((access error system-global-environment)
   "Operation requires non-empty tree:" owner))


(define (make-wt-tree-type key<?)

  (declare (integrate key<?))

  (define-integrable (key>? x y)  (key<? y x))

  (define (node/find k node)
    ;; returns either the node or #f.
    ;; Loop takes D comparisons (D is the depth of the tree) rather than the
    ;; traditional compare-low, compare-high which takes on average
    ;; 1.5(D-1) comparisons
    (define (loop this best)
      (cond ((empty? this)  best)
	    ((key<? k (node/k this))   (loop (node/l this) best))
	    (else (loop (node/r this) this))))
    (let ((best (loop node #f)))
      (cond ((not best)               #f)
	    ((key<? (node/k best) k)  #f)
	    (else                     best))))

  (define (node/rank k node rank)
    (cond ((empty? node)             #f)
	  ((key<? k (node/k node))  (node/rank k (node/l node) rank))
	  ((key>? k (node/k node))
	   (node/rank k (node/r node) (+ 1 rank (node/size (node/l node)))))
	  (else                     (+ rank (node/size (node/l node))))))

  (define (node/add node k v)
    (if (empty? node)
	(node/singleton k v)
	(with-n-node node
	  (lambda (key val l r)
	    (cond ((key<? k key)   (t-join key val (node/add l k v) r))
		  ((key<? key k)   (t-join key val l (node/add r k v)))
		  (else            (n-join key v   l r)))))))

  (define (node/delete x node)
    (if (empty? node)
	empty
	(with-n-node node
	  (lambda (key val l r)
	    (cond ((key<? x key)   (t-join key val (node/delete x l) r))
		  ((key<? key x)   (t-join key val l (node/delete x r)))
		  (else            (node/concat2 l r)))))))

  (define (node/concat tree1 tree2)
    (cond ((empty? tree1)  tree2)
	  ((empty? tree2)  tree1)
	  (else
	   (let ((min-node (node/min tree2)))
	     (node/concat3 (node/k min-node) (node/v min-node) tree1
			   (node/delmin tree2))))))

  (define (node/concat3 k v l r)
    (cond ((empty? l)   (node/add r k v))
	  ((empty? r)   (node/add l k v))
	  (else
	   (let ((w1  (node/weight l))
		 (w2  (node/weight r)))
	     (cond ((overweight? w1 w2)
		    (with-n-node r
				 (lambda (k2 v2 l2 r2)
				   (t-join k2 v2 (node/concat3 k v l l2) r2))))
		   ((overweight? w2 w1)
		    (with-n-node l
				 (lambda (k1 v1 l1 r1)
				   (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
		   (else
		    (n-join k v l r)))))))

  (define (node/split-lt node x)
    (cond ((empty? node)  empty)
	  ((key<? x (node/k node))
	   (node/split-lt (node/l node) x))
	  ((key<? (node/k node) x)
	   (node/concat3 (node/k node) (node/v node) (node/l node)
			 (node/split-lt (node/r node) x)))
	  (else (node/l node))))

  (define (node/split-gt node x)
    (cond ((empty? node)  empty)
	  ((key<? (node/k node) x)
	   (node/split-gt (node/r node) x))
	  ((key<? x (node/k node))
	   (node/concat3 (node/k node) (node/v node)
			 (node/split-gt (node/l node) x) (node/r node)))
	  (else (node/r node))))

  (define (node/union tree1 tree2)
    (cond  ((empty? tree1)  tree2)
	   ((empty? tree2)  tree1)
	   (else
	    (with-n-node tree2
	      (lambda (ak av l r)
		(let ((l1  (node/split-lt tree1 ak))
		      (r1  (node/split-gt tree1 ak)))
		  (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))

  (define (node/union-merge tree1 tree2 merge)
    (cond ((empty? tree1)  tree2)
	  ((empty? tree2)  tree1)
	  (else
	   (with-n-node tree2
	     (lambda (ak av l r)
	       (let* ((node1  (node/find ak tree1))
		      (l1     (node/split-lt tree1 ak))
		      (r1     (node/split-gt tree1 ak))
		      (value  (if node1
				  (merge ak av (node/v node1))
				  av)))
		 (node/concat3 ak value
			       (node/union-merge l1 l merge)
			       (node/union-merge r1 r merge))))))))

  (define (node/difference tree1 tree2)
    (cond ((empty? tree1)   empty)
	  ((empty? tree2)   tree1)
	  (else
	   (with-n-node tree2
	     (lambda (ak av l r)
	       (let ((l1  (node/split-lt tree1 ak))
		     (r1  (node/split-gt tree1 ak)))
		 av
		 (node/concat (node/difference l1 l)
			      (node/difference r1 r))))))))

  (define (node/intersection tree1 tree2)
    (cond ((empty? tree1)   empty)
	  ((empty? tree2)   empty)
	  (else
	   (with-n-node tree2
	     (lambda (ak av l r)
	       (let ((l1  (node/split-lt tree1 ak))
		     (r1  (node/split-gt tree1 ak)))
		 (if (node/find ak tree1)
		     (node/concat3 ak av (node/intersection l1 l)
				   (node/intersection r1 r))
		     (node/concat (node/intersection l1 l)
				  (node/intersection r1 r)))))))))

  (define (node/subset? tree1 tree2)
    (or (empty? tree1)
	(and (<= (node/size tree1) (node/size tree2))
	     (with-n-node tree1
	       (lambda (k v l r)
		 v
		 (cond ((key<? k (node/k tree2))
			(and (node/subset? l (node/l tree2))
			     (node/find k tree2)
			     (node/subset? r tree2)))
		       ((key>? k (node/k tree2))
			(and (node/subset? r (node/r tree2))
			     (node/find k tree2)
			     (node/subset? l tree2)))
		       (else
			(and (node/subset? l (node/l tree2))
			     (node/subset? r (node/r tree2))))))))))


  ;;; Tree interface: stripping off or injecting the tree types

  (define (tree/map-add tree k v)
    (%make-wt-tree (tree/type tree)
		   (node/add (tree/root tree) k v)))

  ;(define (tree/set-add tree k)  (tree/map-add tree k #f))

  (define (tree/insert! tree k v)
    (set-tree/root! tree (node/add (tree/root tree) k v)))

  (define (tree/delete tree k)
    (%make-wt-tree (tree/type tree)
		   (node/delete k (tree/root tree))))

  (define (tree/delete! tree k)
    (set-tree/root! tree (node/delete k (tree/root tree))))

  (define (tree/split-lt tree key)
    (%make-wt-tree (tree/type tree)
		   (node/split-lt (tree/root tree) key)))

  (define (tree/split-gt tree key)
    (%make-wt-tree (tree/type tree)
		   (node/split-gt (tree/root tree) key)))

  (define (tree/union tree1 tree2)
    (%make-wt-tree (tree/type tree1)
		   (node/union (tree/root tree1) (tree/root tree2))))

  (define (tree/union-merge tree1 tree2 merge)
    (%make-wt-tree (tree/type tree1)
		   (node/union-merge (tree/root tree1) (tree/root tree2)
				     merge)))

  (define (tree/intersection tree1 tree2)
    (%make-wt-tree (tree/type tree1)
		   (node/intersection (tree/root tree1) (tree/root tree2))))

  (define (tree/difference tree1 tree2)
    (%make-wt-tree (tree/type tree1)
		  (node/difference (tree/root tree1) (tree/root tree2))))

  (define (tree/subset? tree1 tree2)
    (node/subset? (tree/root tree1) (tree/root tree2)))

  (define (alist->tree alist)
    (define (loop alist node)
      (cond ((null? alist)  node)
	    ((pair? alist)  (loop (cdr alist)
				  (node/add node (caar alist) (cdar alist))))
	    (else
	     (error:wrong-type-argument alist "alist" 'alist->tree))))
    (%make-wt-tree my-type (loop alist empty)))

  (define (tree/get tree key default)
    (let ((node  (node/find key (tree/root tree))))
      (if node
	  (node/v node)
	  default)))

  (define (tree/rank tree key)  (node/rank key (tree/root tree) 0))

  (define (tree/member? key tree)
    (and (node/find key (tree/root tree))
	 #t))

  (define my-type
    (%make-tree-type
     key<?                ;  key<?
     alist->tree          ;  alist->tree
     tree/map-add         ;  add
     tree/insert!         ;  insert!
     tree/delete          ;  delete
     tree/delete!         ;  delete!
     tree/member?         ;  member?
     tree/get             ;  lookup
     tree/split-lt        ;  split-lt
     tree/split-gt        ;  split-gt
     tree/union           ;  union
     tree/union-merge     ;  union-merge
     tree/intersection    ;  intersection
     tree/difference      ;  difference
     tree/subset?         ;  subset?
     tree/rank            ;  rank
     ))

  my-type)



;;;
;;;
;;;

(define (guarantee-tree/report tree procedure)
  (error:wrong-type-argument tree "weight-balanced tree" procedure))

(define-integrable (guarantee-tree tree procedure)
  (if (not (wt-tree? tree))
      (guarantee-tree/report tree procedure)))

(define-integrable (guarantee-tree-type type procedure)
  (if (not (tree-type? type))
      (error:wrong-type-argument type "weight-balanced tree type" procedure)))

(define-integrable (guarantee-compatible-trees/report tree1 tree2 procedure)
  (guarantee-tree tree1 procedure)
  (guarantee-tree tree2 procedure)
  (error "The trees" tree1 'and tree2 'have 'incompatible 'types
	 (tree/type tree1) 'and (tree/type tree2)))

(define-integrable (guarantee-compatible-trees tree1 tree2 procedure)
  (if (or (not (wt-tree? tree1))
	  (not (wt-tree? tree2))
	  (not (eq? (tree/type tree1) (tree/type tree2))))
      (guarantee-compatible-trees/report tree1 tree2 procedure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Exported interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-wt-tree tree-type)
  (%make-wt-tree tree-type empty))

(define (singleton-wt-tree type key value)
  (guarantee-tree-type type 'singleton-wt-tree)
  (%make-wt-tree type (node/singleton key value)))

(define (alist->wt-tree type alist)
  (guarantee-tree-type type 'alist->wt-tree)
  ((tree-type/alist->tree type) alist))

(define (wt-tree/empty? tree)
  (guarantee-tree tree 'wt-tree/empty?)
  (empty? (tree/root tree)))

(define (wt-tree/size tree)
  (guarantee-tree tree 'wt-tree/size)
  (node/size (tree/root tree)))

(define (wt-tree/add tree key datum)
  (guarantee-tree tree 'wt-tree/add)
  ((tree-type/add (tree/type tree)) tree key datum))

(define (wt-tree/delete tree key)
  (guarantee-tree tree 'wt-tree/delete)
  ((tree-type/delete (tree/type tree)) tree key))

(define (wt-tree/add! tree key datum)
  (guarantee-tree tree 'wt-tree/add!)
  ((tree-type/insert! (tree/type tree)) tree key datum))

(define (wt-tree/delete! tree key)
  (guarantee-tree tree 'wt-tree/delete!)
  ((tree-type/delete! (tree/type tree)) tree key))

(define (wt-tree/member? key tree)
  (guarantee-tree tree 'wt-tree/member?)
  ((tree-type/member? (tree/type tree)) key tree))

(define (wt-tree/lookup tree key default)
  (guarantee-tree tree 'wt-tree/lookup)
  ((tree-type/lookup (tree/type tree)) tree key default))

(define (wt-tree/split< tree key)
  (guarantee-tree tree 'wt-tree/split<)
  ((tree-type/split-lt (tree/type tree)) tree key))

(define (wt-tree/split> tree key)
  (guarantee-tree tree 'wt-tree/split>)
  ((tree-type/split-gt (tree/type tree)) tree key))

(define (wt-tree/union tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
  ((tree-type/union (tree/type tree1)) tree1 tree2))

(define (wt-tree/union-merge tree1 tree2 merge)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/union-merge)
  ((tree-type/union-merge (tree/type tree1)) tree1 tree2 merge))

(define (wt-tree/intersection tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
  ((tree-type/intersection (tree/type tree1)) tree1 tree2))

(define (wt-tree/difference tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
  ((tree-type/difference (tree/type tree1)) tree1 tree2))

(define (wt-tree/subset? tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
  ((tree-type/subset? (tree/type tree1)) tree1 tree2))

(define (wt-tree/set-equal? tree1 tree2)
  (and (wt-tree/subset? tree1 tree2)
       (wt-tree/subset? tree2 tree1)))

(define (wt-tree/fold combiner-key-datum-result init tree)
  (guarantee-tree tree 'wt-tree/fold)
  (node/inorder-fold combiner-key-datum-result init (tree/root tree)))

(define (wt-tree/for-each action-key-datum tree)
  (guarantee-tree tree 'wt-tree/for-each)
  (node/for-each action-key-datum (tree/root tree)))

(define (wt-tree/index tree index)
  (guarantee-tree tree 'wt-tree/index)
  (let ((node  (node/index (tree/root tree) index 'wt-tree/index)))
    (and node (node/k node))))

(define (wt-tree/index-datum tree index)
  (guarantee-tree tree 'wt-tree/index-datum)
  (let ((node  (node/index (tree/root tree) index 'wt-tree/index-datum)))
    (and node (node/v node))))

(define (wt-tree/index-pair tree index)
  (guarantee-tree tree 'wt-tree/index-pair)
  (let ((node  (node/index (tree/root tree) index 'wt-tree/index-pair)))
    (and node (cons (node/k node) (node/v node)))))

(define (wt-tree/rank tree key)
  (guarantee-tree tree 'wt-tree/rank)
  ((tree-type/rank (tree/type tree)) tree key))

(define (wt-tree/min tree)
  (guarantee-tree tree 'wt-tree/min)
  (node/k (node/min (tree/root tree))))

(define (wt-tree/min-datum tree)
  (guarantee-tree tree 'wt-tree/min-datum)
  (node/v (node/min (tree/root tree))))

(define (wt-tree/min-pair tree)
  (guarantee-tree tree 'wt-tree/min-pair)
  (let ((node  (node/min (tree/root tree))))
    (cons (node/k node) (node/v node))))

(define (wt-tree/delete-min tree)
  (guarantee-tree tree 'wt-tree/delete-min)
  (%make-wt-tree (tree/type tree) (node/delmin (tree/root tree))))

(define (wt-tree/delete-min! tree)
  (guarantee-tree tree 'wt-tree/delete-min!)
  (set-tree/root! tree (node/delmin (tree/root tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wt-tree/valid? tree)
  (guarantee-tree tree 'wt-tree/valid?)
  (let ((key<? (tree-type/key<? (tree/type tree))))
    (define (balanced? node)
      (or (empty? node)
	  (let ((l (node/l node)) (r (node/r node)))
	    (let ((lw (node/weight l)) (rw (node/weight r)))
	      (and (not (overweight? lw rw))
		   (not (overweight? rw lw))
		   (balanced? l)
		   (balanced? r))))))
    (define (ordered? node not-too-low? not-too-high?)
      (or (empty? node)
	  (let ((k (node/k node)) (l (node/l node)) (r (node/r node)))
	    (and (not-too-low? k)
		 (not-too-high? k)
		 (ordered? l not-too-low? (lambda (k*) (key<? k* k)))
		 (ordered? r (lambda (k*) (key<? k k*)) not-too-high?)))))
    (let ((root (tree/root tree)))
      (and (balanced? root)
	   (ordered? root (lambda (k) k #t) (lambda (k) k #t))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(define ttype (make-wt-tree-type <))

(define number-wt-type
  ((lambda()
     (declare (integrate-operator make-wt-tree-type))
     (make-wt-tree-type  (lambda (x y) (< x y))))))

(define string-wt-type
  ((lambda()
     (declare (integrate-operator make-wt-tree-type))
     (make-wt-tree-type  string<?))))
