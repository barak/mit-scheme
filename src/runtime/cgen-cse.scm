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

;;;; Code generation library: common subexpression elimination
;;; package: (runtime cgen cse)

(declare (usual-integrations))

(add-boot-deps! '(runtime cgen))

(define (cse-cgen-expr expr)
  (if (expr-cse? expr)
      (cnode-expr
       (substitute-cnodes
	(eliminate-unshared-cnodes
	 (generate-cgraph expr))))
      expr))

(define (expr-cse? expr)
  (and (pair? expr)
       (not (eq? 'rename (car expr)))
       (not (eq? 'quote (car expr)))))

(define (generate-cgraph expr)
  (initial-cgraph (containment-trie expr)))

(define (containment-trie expr)
  (let ((trie (make-trie eqv?)))
    (set-trie-value! trie expr)
    (for-each (lambda (subexpr)
                (for-each (lambda (path)
                            (set-trie-value! (trie-intern! trie path) subexpr))
                          (cdr subexpr)))
              (cse-subexprs expr))
    trie))

(define (cse-subexprs expr)
  (let ((table (make-equal-hash-table)))
    (expr-for-each
     (lambda (expr path)
       (if (expr-cse? expr)
           (hash-table-update! table expr
                               (lambda (value) (cons path value))
                               (lambda () (list expr)))))
     expr)
    (map reverse (hash-table-values table))))

(define (initial-cgraph trie)
  (let ((memoize! (make-strong-eq-memoizer)))

    (define (scan-trie rpath trie)
      (let ((subexpr (trie-value trie)))
        (make-cnode (car subexpr)
                    (cdr subexpr)
                    (scan-children rpath trie '()))))

    (define (scan-children rpath trie acc)
      (trie-edge-fold (lambda (key trie* acc)
                        (scan-child (cons key rpath) trie* acc))
                      acc
                      trie))

    (define (scan-child rpath trie acc)
      (if (trie-has-value? trie)
          (cons (cons rpath
                      (memoize! (trie-value trie)
                        (lambda (subexpr)
                          (declare (ignore subexpr))
                          (scan-trie '() trie))))
                acc)
          (scan-children rpath trie acc)))

    (scan-trie '() trie)))

;;;; Eliminate unshared nodes

(define (eliminate-unshared-cnodes root-node)
  (let ((delete? (compute-delete-predicate root-node))
        (memoize! (make-strong-eq-memoizer)))

    (define (scan-cnode cnode)
      (memoize! cnode
        (lambda (cnode)
          (make-cnode (cnode-expr cnode)
                      (cnode-paths cnode)
                      (scan-children '() cnode)))))

    (define (scan-children rpath cnode)
      (append-map (lambda (clink)
                    (scan-child (append rpath (clink-rpath clink))
                                (clink-child clink)))
                  (cnode-children cnode)))

    (define (scan-child rpath child)
      (if (delete? child)
          (scan-children rpath child)
          (list (make-clink rpath (scan-cnode child)))))

    (scan-cnode root-node)))

(define (compute-delete-predicate root-node)
  (let ((ptable (make-strong-eq-hash-table))
	(ctable (make-strong-eq-hash-table)))

    (define (initialize! parent)
      (for-each (lambda (clink)
                  (let ((child (clink-child clink)))
		    (connect! child parent)
                    (initialize! child)))
                (cnode-children parent)))

    (define (get-deletions)
      (hash-table-fold ptable
		       (lambda (child parents acc)
			 (if (and (pair? parents) (null? (cdr parents)))
			     (cons child acc)
			     acc))
		       '()))

    (define (delete! cnode)
      (let ((children (hash-table-ref/default ctable cnode '()))
	    (parents (hash-table-ref ptable cnode)))
	(for-each (lambda (child) (disconnect! child cnode)) children)
	(for-each (lambda (parent) (disconnect! cnode parent)) parents)
	(for-each (lambda (child)
		    (for-each (lambda (parent)
				(connect! child parent))
			      parents))
		  children)))

    (define (connect! child parent)
      (hash-table-update! ptable child
	(lambda (parents) (lset-adjoin eq? parents parent))
	(lambda () '()))
      (hash-table-update! ctable parent
	(lambda (children) (lset-adjoin eq? children child))
	(lambda () '())))

    (define (disconnect! child parent)
      (hash-table-update! ptable child
	(lambda (parents) (delq! parent parents)))
      (hash-table-update! ctable parent
	(lambda (children) (delq! child children))))

    (initialize! root-node)
    (for-each delete! (get-deletions))
    (lambda (cnode) (null? (hash-table-ref ptable cnode)))))

(define (substitute-cnodes root-node)
  (let ((let-groups (compute-let-groups root-node)))
    (let ((cnode->name (compute-cnode->name let-groups))
	  (bindings (compute-bindings let-groups))
	  (memoize! (make-strong-eq-memoizer))
	  (table (make-strong-eq-hash-table)))

      (define (scan-cnode path cnode)
	(memoize! cnode
	  (lambda (cnode)
	    (let ((children
		   (map (lambda (clink)
			  (make-clink (clink-rpath clink)
				      (scan-cnode (clink-apath path clink)
						  (clink-child clink))))
			(cnode-children cnode))))
	      ;; Must come after processing children so that they can be looked
	      ;; up by binding-group-maps.
	      (let ((cnode*
		     (make-cnode (edit-expr path cnode)
				 (cnode-paths cnode)
				 children)))
		(hash-table-set! table cnode cnode*)
		cnode*)))))

      (define (edit-expr path cnode)
	(cse-expr-map (cse-expr-replace (cnode-expr cnode)
					(compute-replacements cnode))
		      (binding-group-maps path cnode)))

      (define (compute-replacements cnode)
	(map (lambda (clink)
	       (cons (clink-rpath clink)
		     (cnode->name (clink-child clink))))
	     (cnode-children cnode)))

      (define (binding-group-maps path cnode)
	(map (lambda (let-group)
	       (cons (car let-group)
		     (lambda (subexpr)
		       (fold-right (lambda (cnodes subexpr*)
				     (cgen:let (cnodes->bindings cnodes)
					       subexpr*))
				   subexpr
				   (cdr let-group)))))
	     (bindings path cnode)))

      (define (cnodes->bindings cnodes)
	(map (lambda (cnode)
	       (list (cnode->name cnode)
		     (cnode-expr (hash-table-ref table cnode))))
	     cnodes))

      (scan-cnode '() root-node))))

(define (compute-let-groups root-node)
  (let ((memoize! (make-strong-eq-memoizer))
	(table (make-equal-hash-table)))
    (let loop ((cnode root-node))
      (memoize! cnode
	(lambda (cnode)
	  (hash-table-update! table (cnode-let-path cnode)
	    (lambda (cnodes) (cons cnode cnodes))
	    (lambda () '()))
	  (for-each (lambda (clink)
		      (loop (clink-child clink)))
		    (cnode-children cnode)))))
    (map (lambda (p)
	   (cons (car p)
		 (group-cnodes-by-dependency (cdr p))))
	 (hash-table->alist table))))

(define (group-cnodes-by-dependency cnodes)
  (let loop ((alist (compute-cnode-dependencies cnodes)))
    (if (pair? alist)
	(let-values (((leaves rest)
		      (partition (lambda (p) (null? (cdr p)))
				 alist)))
	  (if (null? leaves)
	      (error "Unable to group cnodes:" alist))
	  (let ((leaf-nodes (map car leaves)))
	    (cons leaf-nodes
		  (loop
		   (map (lambda (p)
			  (cons (car p)
				(lset-difference eq? (cdr p) leaf-nodes)))
			rest)))))
	'())))

(define (compute-cnode-dependencies cnodes)
  (map (lambda (cnode)
	 (cons cnode
	       (filter (lambda (cnode*)
			 (cnode-strict-ancestor? cnode cnode*))
		       cnodes)))
       cnodes))

(define (compute-cnode->name let-groups)
  (let ((table (make-strong-eq-hash-table)))
    (for-each (lambda (let-group)
		(let ((path (car let-group)))
		  (for-each (lambda (dep-group)
			      (for-each (lambda (cnode)
					  (hash-table-set! table cnode
					    (and path (cgen:new-name 'cse))))
					dep-group))
			    (cdr let-group))))
	      ;; Sort groups so that let names are in lexical order.
	      (sort let-groups
		    (lambda (g1 g2)
		      (let ((p1 (car g1))
			    (p2 (car g2)))
			(if (and p1 p2)
			    (path<? p1 p2)
			    (and (not p1) p2))))))
    (lambda (cnode)
      (hash-table-ref table cnode))))

(define (compute-bindings let-groups)
  (lambda (path cnode)
    (filter-map (lambda (p)
		  (let* ((target-path (car p))
			 (rpath (path-suffix path target-path)))
		    (and rpath
			 (not (any (lambda (clink)
				     (path-suffix (clink-apath path clink)
						  target-path))
				   (cnode-children cnode)))
			 (cons rpath (cdr p)))))
		let-groups)))

(define-record-type <cnode>
    (make-cnode expr paths children)
    cnode?
  (expr cnode-expr)
  (paths cnode-paths)
  (children cnode-children))

(define-integrable make-clink cons)
(define-integrable clink-rpath car)
(define-integrable clink-child cdr)

(define (cnode-let-path cnode)
  (let ((paths (cnode-paths cnode)))
    (and (pair? (cdr paths))
	 (path-shared-prefix* paths))))

(define (cnode-strict-ancestor? cnode1 cnode2)
  (and (not (eq? cnode1 cnode2))
       (any (lambda (path1)
	      (any (lambda (path2)
		     (path-suffix path1 path2))
		   (cnode-paths cnode2)))
	    (cnode-paths cnode1))))

(define (clink-apath apath clink)
  (append apath (clink-rpath clink)))

(define (make-strong-eq-memoizer)
  (let ((table (make-strong-eq-hash-table)))
    (lambda (key get-datum)
      (let ((datum (hash-table-ref/default table key #f)))
        (if (eq? 'pending datum)
            (error "Loop in get-datum:" key get-datum))
        (or datum
            (begin
              (hash-table-set! table key 'pending)
              (let ((datum (get-datum key)))
                (hash-table-set! table key datum)
                datum)))))))

(define (show-cnode-tree cnode)
  `(,cnode
    ,(cnode-expr cnode)
    (children
     ,@(map (lambda (clink)
              (list (clink-rpath clink)
                    (show-cnode-tree (clink-child clink))))
            (sort-clinks (cnode-children cnode))))))

(define (sort-clinks clinks)
  (sort clinks
    (lambda (l1 l2)
      (path<? (clink-rpath l1) (clink-rpath l2)))))

;;;; Paths

(define (path-shared-prefix* paths)

  (define (binary path1 path2)
    (let loop ((scan1 path1) (scan2 path2) (count 0))
      (if (and (pair? scan1)
               (pair? scan2)
               (= (car scan1) (car scan2)))
          (loop (cdr scan1) (cdr scan2) (+ count 1))
          (take path1 count))))

  (guarantee pair? paths 'path-shared-prefix*)
  (fold binary (car paths) (cdr paths)))

;; If PATH1 is a prefix of PATH2, returns the suffix.
;; Otherwise returns #F.
(define (path-suffix path1 path2)
  (if (pair? path1)
      (and (pair? path2)
           (= (car path1) (car path2))
           (path-suffix (cdr path1) (cdr path2)))
      path2))

(define (path=? p1 p2)
  (if (null-list? p1)
      (null-list? p2)
      (and (not (null-list? p2))
           (= (car p1) (car p2))
           (path=? (cdr p1) (cdr p2)))))

(define (path<? p1 p2)
  (if (pair? p1)
      (and (pair? p2)
           (or (< (car p1) (car p2))
               (and (= (car p1) (car p2))
                    (path<? (cdr p1) (cdr p2)))))
      (pair? p2)))

(define (cse-expr-map expr path-alist)
  (if (pair? path-alist)
      (let ((expr* (cse-expr-recur cse-expr-map expr path-alist)))
        (let ((p (assq '() path-alist)))
          (if p
              ((cdr p) expr*)
              expr*)))
      expr))

(define (cse-expr-replace expr path-alist)
  (if (pair? path-alist)
      (let ((p (assq '() path-alist)))
        (if p
            (cdr p)
            (cse-expr-recur cse-expr-replace expr path-alist)))
      expr))

(define (cse-expr-recur proc expr path-alist)
  (if (list? expr)
      (map (lambda (subexpr index)
             (proc subexpr
                   (filter-map (lambda (p)
                                 (and (pair? (car p))
                                      (= index (caar p))
                                      (cons (cdar p) (cdr p))))
                               path-alist)))
           expr
           (iota (length expr)))
      expr))