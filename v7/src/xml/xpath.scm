#| -*-Scheme-*-

$Id: xpath.scm,v 1.1 2003/09/28 04:12:54 cph Exp $

Copyright 2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; XPath implementation

(declare (usual-integrations))

(define-generic nodify-item (item parent index))
(define-generic node-eq? (n1 n2))
(define-generic node-item (node))
(define-generic parent-node (node))
(define-generic node-child-items (node))
(define-generic walk-children+ (node index p a))
(define-generic walk-children- (node index p a))
(define-generic parent-index (node))
(define-generic node-name (node))
(define-generic node-string (node))

(define-class <node> ()
  (item accessor node-item))

(define-method parent-node ((node <node>))
  node
  #f)

(define-method node-child-items ((node <node>))
  node
  '())

(define-generic walk-children+ ((node <node>) index p a)
  node index p a
  unspecific)

(define-generic walk-children- ((node <node>) index p a)
  node index p a
  unspecific)

(define-method node-name ((node <node>))
  node
  #f)

(define-class <parented-node> (<node>)
  (parent accessor parent-node))

(define (parented-node-nodifier item-class node-class)
  (let ((constructor (instance-constructor node-class '(item parent))))
    (define-method nodify-item ((item item-class) parent index)
      index
      (constructor item parent))))

(define-class <parent-node> (<node>))

(define-method walk-children+ ((node <parent-node>) index p a)
  (let loop ((items (node-child-items node)) (i 0) (a a))
    (if (pair? items)
	(loop (cdr items)
	      (fix:+ i 1)
	      (if (fix:> i index)
		  (p (nodify-item (car items) node i) a)
		  a))
	a)))

(define-method walk-children- ((node <parent-node>) index p a)
  (let loop ((items (node-child-items node)) (i 0))
    (if (fix:< i index)
	(p (nodify-item (car items) node i)
	   (loop (cdr items) (fix:+ i 1)))
	a)))

(define-class <content-node> (<parented-node>)
  (index accessor parent-index))

(define (content-node-nodifier item-class node-class)
  (let ((constructor (instance-constructor node-class '(item parent index))))
    (define-method nodify-item ((item item-class) parent index)
      (constructor item parent index))))

(define-method node-eq? ((n1 <node>) (n2 <node>))
  (and (eq? (instance-class n1) (instance-class n2))
       (eq? (node-item n1) (node-item n2))))

(define-method node-eq? ((n1 <parented-node>) (n2 <parented-node>))
  (and (call-next-method n1 n2)
       (node-eq? (parent-node n1) (parent-node n2))))

(define-method node-eq? ((n1 <content-node>) (n2 <content-node>))
  (and (call-next-method n1 n2)
       (fix:= (parent-index n1) (parent-index n2))))

(define-class <root-node> (<parent-node>)
  (child-items accessor node-child-items))

(define make-root-node
  (let ((c (instance-constructor <root-node> '(item child-items))))
    (lambda (doc)
      (c doc
	 (let ((p
		(lambda (item)
		  (or (xml-comment? item)
		      (xml-processing-instructions? item)))))
	   (append! (keep-matching-items (xml-document-misc-1 doc) p)
		    (keep-matching-items (xml-document-misc-2 doc) p)
		    (list (xml-document-root doc))
		    (keep-matching-items (xml-document-misc-3 doc) p)))))))

(define-method node-string ((node <root-node>))
  (node-string
   (let ((doc (node-item node)))
     (nodify-item (xml-document-root doc)
		  node
		  (fix:+ (length (xml-document-misc-1 doc))
			 (length (xml-document-misc-2 doc)))))))


(define-class <element-node> (<parent-node> <content-node>))

(content-node-nodifier <xml-element> <element-node>)

(define-method node-name ((node <element-node>))
  (xml-element-name (node-item node)))

(define-method node-child-items ((node <element-node>))
  (xml-element-content (node-item node)))

(define-method node-string ((node <element-node>))
  (call-with-output-string
    (lambda (port)
      (let loop ((elt (node-item node)))
	(for-each (lambda (item)
		    (cond ((string? item) (write-string item port))
			  ((xml-element? item) (loop item))))
		  (xml-element-contents elt))))))

(define-class <attribute-node> (<parented-node>))
(define-class <namespace-node> (<attribute-node>))

(define make-attribute-node
  (instance-constructor <attribute-node> '(item parent)))

(define make-namespace-node
  (instance-constructor <namespace-node> '(item parent)))

(define-method node-name ((node <attribute-node>))
  (xml-attribute-name (node-item node)))

(define-method node-string ((node <attribute-node>))
  (xml-attribute-value (node-item node)))

(define-method node-name ((node <namespace-node>))
  (let ((name (xml-attribute-name (node-item node))))
    (if (xml-name-qname=? name 'xmlns)
	(null-xml-name-prefix)
	(xml-name-local name))))


(define-class <processing-instructions-node> (<content-node>))

(content-node-nodifier <xml-processing-instructions>
		       <processing-instructions-node>)

(define-method node-name ((node <processing-instructions-node>))
  (xml-processing-instructions-name (node-item node)))

(define-method node-string ((node <element-node>))
  (xml-processing-instructions-text (node-item node)))


(define-class <comment-node> (<content-node>))

(content-node-nodifier <xml-comment> <comment-node>)

(define-method node-string ((node <element-node>))
  (xml-comment-text (node-item node)))


(define-class <text-node> (<content-node>))

(content-node-nodifier <string> <text-node>)

(define-method node-string ((node <element-node>))
  (node-item node))

(define (node-n-children node)
  (length (node-child-items node)))

(define ((ped node-test size env) node pos)
  (node-test node pos size env)
  (fix:+ pos 1))

(define ((axis:self node-test) node env)
  (node-test node 1 (delay 1) env))

(define ((axis:child node-test) node env)
  (walk-children+ node
		  -1
		  (ped node-test
		       (delay (node-n-children parent))
		       env)
		  1))

(define ((axis:following-sibling node-test) node env)
  (let ((parent (parent-node node))
	(index (parent-index node)))
    (walk-children+ parent
		    index
		    (ped node-test
			 (delay (fix:- (node-n-children parent)
				       (fix:+ index 1)))
			 env)
		    1)))

(define ((axis:preceding-sibling node-test) node env)
  (let ((parent (parent-node node))
	(index (parent-index node)))
    (walk-children- parent
		    index
		    (ped node-test (delay index) env)
		    1)))

(define ((axis:descendant node-test) node env)
  (let ((p (ped node-test (delay (count-descendants node 0)) env)))
    (let loop ((node node) (pos 1))
      (walk-children+ node
		      -1
		      (lambda (node pos)
			(loop node (p node pos)))
		      pos))))

(define ((axis:descendant-or-self node-test) node env)
  (let ((p (ped node-test (delay (count-descendants node 1)) env)))
    (let loop ((node node) (pos 1))
      (walk-children+ node
		      -1
		      loop
		      (p node pos)))))

(define (count-descendants node offset)
  (let loop ((node node) (size offset))
    (walk-children+ node
		    -1
		    (lambda (node size)
		      (loop node (fix:+ size 1)))
		    size)))

(define ((axis:following node-test) node env)
  )

(define ((axis:preceding node-test) node env)
  )

(define ((axis:parent node-test) node env)
  (let ((parent (node-parent node)))
    (if parent
	(node-test parent 1 (delay 1) env))))

(define ((axis:ancestor node-test) node env)
  (let ((size (delay (count-ancestors node 0))))
    (let loop ((node node) (pos 1))
      (let ((parent (node-parent node)))
	(if parent
	    (begin
	      (node-test parent pos size env)
	      (loop parent (fix:+ pos 1))))))))

(define ((axis:ancestor-or-self node-test) node env)
  (let ((size (delay (count-ancestors node 1))))
    (let loop ((node node) (pos 1))
      (node-test node pos size env)
      (let ((parent (node-parent node)))
	(if parent
	    (loop parent (fix:+ pos 1)))))))

(define (count-ancestors node offset)
  (let loop ((node node) (size offset))
    (let ((parent (node-parent node)))
      (if parent
	  (loop parent (fix:+ size 1))
	  size))))

(define ((axis:attribute node-test) node env)
  (if (element-node? node)
      (let ((attrs (xml-element-attributes node)))
	(let ((p (ped node-test (delay (count-attributes attrs)) env)))
	  (do ((attrs attrs (cdr attrs))
	       (pos 1
		    (let ((attr (car attrs)))
		      (if (xml-attribute-namespace-decl? attr)
			  pos
			  (p (make-attribute-node attr node) pos)))))
	      ((not (pair? attrs))))))))

(define (count-attributes attrs)
  (do ((attrs attrs (cdr attrs))
       (n 0
	  (if (xml-attribute-namespace-decl? (car attrs))
	      n
	      (fix:+ n 1))))
      ((not (pair? attrs)) n)))

(define ((axis:namespace node-test) node env)
  (if (element-node? node)
      (let ((p (ped node-test (delay (count-namespace-decls node)) env))
	    (seen (make-namespace-hash-table)))
	(do ((node node (parent-node node))
	     (pos 1
		  (do ((attrs (xml-element-attributes (node-item node))
			      (cdr attrs))
		       (pos pos
			    (let ((attr (car attrs)))
			      (if (namespace-unseen? seen attr)
				  (p (make-namespace-node attr node) pos)
				  pos))))
		      ((not (pair? attrs)) pos))))
	    ((not (xml-element? node)))))))

(define (count-namespace-decls node)
  (let ((seen (make-namespace-hash-table)))
    (do ((node node (parent-node node))
	 (n 0
	    (do ((attrs (xml-element-attributes (node-item node)) (cdr attrs))
		 (n n
		    (if (namespace-unseen? seen (car attrs))
			(fix:+ n 1)
			n)))
		((not (pair? attrs))))))
	((not (element-node? node)) n))))

(define (make-namespace-hash-table)
  (make-eq-hash-table))

(define (namespace-unseen? table attr)
  (if (xml-attribute-namespace-decl? attr)
      (let ((qname (xml-name-qname (xml-attribute-name attr))))
	(if (hash-table/get table qname #f)
	    #f
	    (begin
	      (hash-table/put! table qname #t)
	      #t)))
      #f))

(define (node-test:* type predicate)
  (case type
    ((attribute namespace)
     predicate)
    ((element)
     (lambda (node pos size env)
       (if (element-node? node)
	   (predicate node pos size env))))
    (else
     (error:bad-range-argument type 'NODE-TEST:*))))

(define (node-test:name qname)
  (lambda (type predicate)
    (case type
      ((attribute)
       (lambda (node pos size env)
	 (if (xml-name=? (node-name node) (expand-name qname node))
	     (predicate node pos size env))))
      ((namespace)
       (lambda (node pos size env)
	 (if (xml-name=? (node-name node) qname)
	     (predicate node pos size env))))
      ((element)
       (lambda (node pos size env)
	 (if (and (element-node? node)
		  (xml-name=? (node-name node) (expand-name qname node)))
	     (predicate node pos size env))))
      (else
       (error:bad-range-argument type 'NODE-TEST:NAME)))))

(define (expand-name qname node)
  (let ((prefix (xml-name-prefix qname)))
    (if (null-xml-name-prefix? prefix)
	qname
	(make-xml-name qname
		       (let loop ((node node))
			 (if (element-node? node)
			     (or (xml-element-namespace-iri (node-item node)
							    prefix)
				 (loop (parent-node node)))))))))
