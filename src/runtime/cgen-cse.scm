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
        (insert-lets
         (eliminate-unshared-cnodes
          (generate-cgraph expr)))))
      expr))

(define (expr-cse? expr)
  (and (pair? expr)
       (not (eq? 'rename (car expr)))
       (not (eq? 'quote (car expr)))))

(define (substitute-cnodes root-node)
  (let loop ((root-node root-node))
    (let ((cnodes (cgraph-nodes root-node)))
      (if (pair? (cdr cnodes))
          (let ((leaf-nodes
                 (filter (lambda (cnode)
                           (null? (cnode-children cnode)))
                         cnodes)))
            (if (null? leaf-nodes)
                (error "Unable to continue processing:" cnodes))
            (loop (substitute-leaf-nodes root-node leaf-nodes)))
          root-node))))

;;;; Convert expr to graph

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
                    (scan-children rpath trie '())
                    #f
                    #f
                    #f)))

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
  (let ((single-parent? (compute-single-parent-map root-node))
        (memoize! (make-strong-eq-memoizer)))

    (define (scan-cnode cnode)
      (memoize! cnode
        (lambda (cnode)
          (make-cnode (cnode-expr cnode)
                      (cnode-paths cnode)
                      (scan-children '() cnode)
                      #f
                      #f
                      #f))))

    (define (scan-children rpath cnode)
      (append-map (lambda (clink)
                    (scan-child (append rpath (clink-rpath clink))
                                (clink-child clink)))
                  (cnode-children cnode)))

    (define (scan-child rpath child)
      (if (single-parent? child)
          (scan-children rpath child)
          (list (make-clink rpath (scan-cnode child)))))

    (scan-cnode root-node)))

(define (compute-single-parent-map root-node)
  (let ((table (make-strong-eq-hash-table)))
    (let loop ((parent root-node))
      (for-each (lambda (clink)
                  (let ((child (clink-child clink)))
                    (hash-table-update! table
                                        child
                                        (lambda (n-parents) (+ n-parents 1))
                                        (lambda () 0))
                    (loop child)))
                (cnode-children parent)))
    (lambda (cnode)
      (= 1 (hash-table-ref table cnode)))))

;;;; Insert lets

(define (insert-lets root-node)
  (let-values (((let-name insertions let-data adjust-path)
                (compute-let-maps root-node)))
    (let ((memoize! (make-strong-eq-memoizer)))

      (define (scan-cnode path cnode)
        (memoize! cnode
          (lambda (cnode)
            (let-values (((let-path let-index) (let-data cnode)))
              (make-cnode (cse-expr-map (cnode-expr cnode)
                                        (filter-path-alist path cnode
                                                           insertions))
                          (map adjust-path (cnode-paths cnode))
                          (scan-children path (cnode-children cnode))
                          (let-name cnode)
                          (and let-path (adjust-path let-path))
                          let-index)))))

      (define (scan-children path clinks)
        (let ((ppath (adjust-path path)))
          (map (lambda (clink)
                 (let ((cpath (adjust-path (clink-apath path clink))))
                   (make-clink (path-suffix ppath cpath)
                               (scan-cnode cpath (clink-child clink)))))
               clinks)))

      (scan-cnode '() root-node))))

(define (compute-let-maps root-node)
  (let ((groups (compute-let-groups root-node)))
    (let ((name-map (compute-let-name-map groups)))
      (values name-map
              (compute-let-insertions groups name-map)
              (compute-let-data-map groups)
              (compute-path-adjustment-map groups)))))

(define (compute-let-groups root-node)
  (let ((table (make-equal-hash-table)))
    (for-each (lambda (cnode)
                (hash-table-update! table
                                    (let ((paths (cnode-paths cnode)))
                                      (and (pair? (cdr paths))
                                           (path-shared-prefix* paths)))
                                    (lambda (cnodes) (cons cnode cnodes))
                                    (lambda () '())))
              (cgraph-nodes root-node))
    (hash-table->alist table)))

(define (compute-let-name-map groups)
  (let ((table (make-strong-eq-hash-table)))
    (for-each (lambda (group)
                (let ((let-path (car group)))
                  (for-each (lambda (cnode)
                              (hash-table-set! table cnode
                                               (and let-path
                                                    (cgen:new-name 'cse))))
                            (cdr group))))
              ;; Sort groups so that let names are in lexical order.
              (sort groups
                (lambda (g1 g2)
                  (let ((p1 (car g1))
                        (p2 (car g2)))
                    (if (and p1 p2)
                        (path<? p1 p2)
                        (and (not p1) p2))))))
    (lambda (cnode)
      (hash-table-ref table cnode))))

(define (compute-let-insertions groups let-name)
  (filter-map (lambda (group)
                (and (car group)
                     (cons (car group)
                           (let ((bindings
                                  (map (lambda (cnode)
                                         (list (let-name cnode) unspecific))
                                       (cdr group))))
                             (lambda (subexpr)
                               (cgen:let bindings subexpr))))))
              groups))

(define (compute-let-data-map groups)
  (let ((indices (make-strong-eq-hash-table)))
    (for-each (lambda (group)
                (let ((let-path (car group))
                      (cnodes (cdr group)))
                  (for-each (lambda (cnode index)
                              (hash-table-set! indices
                                               cnode
                                               (list let-path index)))
                            cnodes
                            (iota (length cnodes)))))
              groups)
    (lambda (cnode)
      (apply values (hash-table-ref indices cnode)))))

(define (compute-path-adjustment-map groups)
  (let ((let-paths (filter-map car groups)))
    (lambda (path)
      (let loop
          ((indices
            (sort (filter-map (let ((n (length path)))
                                (lambda (let-path)
                                  (and (not (path=? path let-path))
                                       (let ((suffix
                                              (path-suffix let-path path)))
                                         (and suffix
                                              (- n (length suffix)))))))
                              let-paths)
                  <))
           (index 0)
           (path path))
        (if (pair? indices)
            (if (= index (car indices))
                (append (let-body-path '())
                        (cons (car path)
                              (loop (cdr indices)
                                    (+ index 1)
                                    (cdr path))))
                (cons (car path)
                      (loop indices
                            (+ index 1)
                            (cdr path))))
            path)))))

;;;; Substitute leaf nodes

(define (substitute-leaf-nodes root-node leaf-nodes)
  (let ((value-insertions (compute-value-insertion-map leaf-nodes))
        (memoize! (make-strong-eq-memoizer)))

    (define (scan-cnode path cnode)
      (memoize! cnode
        (lambda (cnode)
          (make-cnode (cse-expr-replace (cnode-expr cnode)
                                        (value-insertions path cnode))
                      (cnode-paths cnode)
                      (scan-children path (cnode-children cnode))
                      (cnode-name cnode)
                      (cnode-let-path cnode)
                      (cnode-let-index cnode)))))

    (define (scan-children path clinks)
      (filter-map (lambda (clink)
                    (let ((child (clink-child clink)))
                      (and (not (memq child leaf-nodes))
                           (make-clink (clink-rpath clink)
                                       (scan-cnode (clink-apath path clink)
                                                   child)))))
                  clinks))

    (scan-cnode '() root-node)))

(define (compute-value-insertion-map leaf-nodes)
  (let ((let-value-paths (compute-let-value-paths leaf-nodes)))
    (lambda (path cnode)
      (append (filter-path-alist path cnode let-value-paths)
              (filter-map (lambda (clink)
                            (let ((child (clink-child clink)))
                              (and (memq child leaf-nodes)
                                   (cons (clink-rpath clink)
                                         (cnode-name child)))))
                          (cnode-children cnode))))))

(define (compute-let-value-paths leaf-nodes)
  (filter-map (lambda (cnode)
                (let ((let-path (cnode-let-path cnode)))
                  (and let-path
                       (cons (let-value-path let-path (cnode-let-index cnode))
                             (cnode-expr cnode)))))
              leaf-nodes))

(define-record-type <cnode>
    (make-cnode expr paths children name let-path let-index)
    cnode?
  (expr cnode-expr)
  (paths cnode-paths)
  (children cnode-children)
  (name cnode-name)
  (let-path cnode-let-path)
  (let-index cnode-let-index))

(define-integrable make-clink cons)
(define-integrable clink-rpath car)
(define-integrable clink-child cdr)

(define (filter-path-alist path cnode alist)
  (filter-map (lambda (p)
                (let* ((target-path (car p))
                       (rpath (path-suffix path target-path)))
                  (and rpath
                       (not (any (lambda (clink)
                                   (path-suffix (clink-apath path clink)
                                                target-path))
                                 (cnode-children cnode)))
                       (cons rpath (cdr p)))))
              alist))

(define (clink-apath apath clink)
  (append apath (clink-rpath clink)))

(define (cgraph-nodes root-node)
  (let ((seen (make-strong-eq-hash-table)))
    (let loop ((cnode root-node))
      (if (not (hash-table-exists? seen cnode))
          (begin
            (hash-table-set! seen cnode #t)
            (for-each (lambda (clink)
                        (loop (clink-child clink)))
                      (cnode-children cnode)))))
    (hash-table-keys seen)))

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