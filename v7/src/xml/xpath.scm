#| -*-Scheme-*-

$Id: xpath.scm,v 1.2 2003/09/30 04:33:46 cph Exp $

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
(define-generic node-item (node))
(define-generic parent-node (node))
(define-generic node-children (node))
(define-generic next-node (node))
(define-generic prev-node (node))
(define-generic node-attrs (node))
(define-generic node-ns-decls (node))
(define-generic parent-index (node))
(define-generic node-name (node))
(define-generic node-string (node))

(define-class <node> ()
  (item accessor node-item))

(define-method parent-node ((node <node>)) node #f)
(define-method node-children ((node <node>)) node '())
(define-method next-node ((node <node>)) node #f)
(define-method prev-node ((node <node>)) node #f)
(define-method node-attrs ((node <node>)) node '())
(define-method node-ns-decls ((node <node>)) node '())
(define-method node-name ((node <node>)) node #f)

(define-class <parent-node> (<node>))

(define-class <parented-node> (<node>)
  (parent accessor parent-node))

(define-class <content-node> (<parented-node>)
  (index accessor parent-index))

(define (content-node-nodifier item-class node-class)
  (let ((make-node (instance-constructor node-class '(item parent index))))
    (define-method nodify-item ((item item-class) parent index)
      (make-node item parent index))))

(define-method next-node ((node <content-node>))
  (let ((children (node-children node)))
    (if (pair? children)
	(car children)
	(let ((parent (parent-node node)))
	  (let ((siblings
		 (stream-tail (node-children parent)
			      (fix:+ (parent-index node) 1))))
	    (if (pair? siblings)
		(car siblings)
		(next-node parent)))))))

(define-method prev-node ((node <content-node>))
  (let ((parent (parent-node node))
	(index (parent-index node)))
    (let ((siblings (node-children parent)))
      (if (and (pair? siblings) (fix:> index 0))
	  (let loop ((node (stream-ref siblings (fix:- index 1))))
	    (let ((children (node-children node)))
	      (if (pair? children)
		  (loop (car (stream-last-pair children)))
		  node)))
	  parent))))

(define (nodify items node)
  (stream-map (lambda (item index)
		(nodify-item attr node index))
	      items
	      (let iota ((n 0))
		(cons-stream n (iota (fix:+ n 1))))))

(define (keep-matching predicate items)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (cons-stream (car items) (loop (cdr items)))
	    (loop (cdr items)))
	'())))

(define (delete-matching predicate items)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items))
	    (cons-stream (car items) (loop (cdr items))))
	'())))

(define (expand-name qname node)
  (let ((prefix (xml-name-prefix qname)))
    (if (null-xml-name-prefix? prefix)
	qname
	(make-xml-name qname (expand-prefix prefix node)))))

(define (expand-prefix prefix node)
  (cond ((eq? prefix 'xml) xml-iri)
	((eq? prefix 'xmlns) xmlns-iri)
	(else
	 (let loop ((node node))
	   (or (xml-element-namespace-iri (node-item node) prefix)
	       (let ((parent (parent-node node)))
		 (if (not (element-node? parent))
		     (error:bad-range-argument prefix 'EXPAND-PREFIX))
		 (loop parent)))))))

(define-class <root-node> (<parent-node>)
  (children accessor node-children))

(define make-root-node
  (let ((make-node (instance-constructor <root-node> '(item)))
	(set-node-children! (slot-modifier <root-node> 'children)))
    (lambda (doc)
      (let ((node (make-node doc)))
	(set-node-children!
	 node
	 (nodify (let ((p
			(lambda (item)
			  (or (xml-comment? item)
			      (xml-processing-instructions? item)))))
		   (stream-append (keep-matching p (xml-document-misc-1 doc))
				  (keep-matching p (xml-document-misc-2 doc))
				  (stream (xml-document-root doc))
				  (keep-matching p (xml-document-misc-3 doc))))
		 node))
	node))))

(define-method node-string ((node <root-node>))
  (let loop ((children (node-children node)))
    (if (element-node? (car children))
	(node-string (car children))
	(loop (force (cdr children))))))

(define-class <element-node> (<parent-node> <content-node>)
  (children accessor node-children)
  (attrs accessor node-attrs)
  (ns-decls accessor node-ns-decls))

(let ((make-node (instance-constructor <element-node> '(item parent index)))
      (set-node-children! (slot-modifier <element-node> 'children))
      (set-node-attrs! (slot-modifier <element-node> 'attrs))
      (set-node-ns-decls! (slot-modifier <element-node> 'ns-decls)))
  (define-method nodify-item ((item <xml-element>) parent index)
    (let ((node (make-node item parent index)))
      (set-node-children! node
			  (nodify (list->stream (xml-element-content item))
				  node))
      (set-node-attrs!
       node
       (stream-map (lambda (attr)
		     (make-attribute-node attr node))
		   (delete-matching xml-attribute-namespace-decl?
				    (xml-element-attributes item))))
      (set-node-ns-decls!
       node
       (stream-map (lambda (attr)
		     (make-namespace-node attr node))
		   (keep-matching xml-attribute-namespace-decl?
				  (xml-element-attributes item))))
      node)))

(define-method node-name ((node <element-node>))
  (xml-element-name (node-item node)))

(define-method node-string ((node <element-node>))
  (call-with-output-string
    (lambda (port)
      (let loop ((node node))
	(stream-for-each (lambda (child)
			   (cond ((text-node? child)
				  (write-string (node-string child) port))
				 ((element-node? child)
				  (loop child))))
			 (node-children node))))))

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

(define-method nodify-item ((attr <xml-attribute>) parent index)
  index
  (if (xml-attribute-namespace-decl? attr)
      (make-namespace-node attr parent)
      (make-attribute-node attr parent)))


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

;;;; Axes

(define (axis:self node)
  (cons-stream node '()))

(define (axis:child node)
  (node-children node))

(define (axis:descendant node)
  (stream-append-map axis:descendant-or-self (axis:child node)))

(define (axis:descendant-or-self node)
  (cons-stream node (axis:descendant node)))

(define (axis:parent node)
  (if (parented-node? node)
      (cons-stream (parent-node node) '())
      '()))

(define (axis:ancestor node)
  (if (parented-node? node)
      (axis:ancestor-or-self (parent-node node))
      '()))

(define (axis:ancestor-or-self node)
  (cons-stream node (axis:ancestor node)))

(define (axis:following-sibling node)
  (if (content-node? node)
      (stream-tail (node-children (parent-node node))
		   (fix:+ (parent-index node) 1))
      '()))

(define (axis:preceding-sibling node)
  (if (content-node? node)
      (let ((children (node-children (parent-node node))))
	(let loop ((index (parent-index node)))
	  (if (fix:> index 0)
	      (let ((index (fix:- index 1)))
		(cons-stream (stream-ref children index)
			     (loop index)))
	      '())))
      '()))

(define (axis:following node)
  (let ((next (next-node node)))
    (if next
	(cons-stream next (axis:following next))
	'())))

(define (axis:preceding node)
  (let ((prev (prev-node node)))
    (if prev
	(cons-stream prev (axis:preceding prev))
	'())))

(define (axis:attribute node)
  (node-attrs node))

(define (axis:namespace node)
  (let per-node ((node node) (seen '()))
    (let per-decl ((decls (node-ns-decls node)) (seen seen))
      (if (pair? decls)
	  (let ((decl (car decls)))
	    (let ((qname (xml-name-qname (xml-attribute-name decl))))
	      (if (memq qname seen)
		  (per-decl (force (cdr decls)) seen)
		  (cons-stream decl
			       (per-decl (force (cdr decls))
					 (cons qname seen))))))
	  (let ((parent (parent-node node)))
	    (if parent
		(per-node parent seen)
		'()))))))

;;;; Node tests

(define (node-test:* type)
  (case type
    ((element) (simple-node-test element-node?))
    ((attribute namespace) all-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:*))))

(define ((node-test:name qname) type)
  (case type
    ((element)
     (simple-node-test
      (lambda (node)
	(and (element-node? node)
	     (xml-name=? (node-name node) (expand-name qname node))))))
    ((attribute)
     (simple-node-test
      (lambda (node)
	(xml-name=? (node-name node) (expand-name qname (node-parent node))))))
    ((namespace)
     (simple-node-test
      (lambda (node)
	(xml-name=? (node-name node) qname))))
    (else (error:bad-range-argument type 'NODE-TEST:NAME))))

(define ((node-test:prefix:* prefix) type)
  (case type
    ((element)
     (simple-node-test
      (lambda (node)
	(and (element-node? node)
	     (xml-name-iri=? (node-name node) (expand-prefix prefix node))))))
    ((attribute)
     (simple-node-test
      (lambda (node)
	(xml-name-iri=? (node-name node)
			(expand-prefix prefix (node-parent node))))))
    ((namespace) null-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:PREFIX:*))))

(define (node-test:text type)
  (case type
    ((element) (simple-node-test text-node?))
    ((attribute namespace) null-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:TEXT))))

(define (node-test:comment type)
  (case type
    ((element) (simple-node-test comment-node?))
    ((attribute namespace) null-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:COMMENT))))

(define ((node-test:processing-instruction name) type)
  (case type
    ((element)
     (simple-node-test
      (if name
	  (lambda (node)
	    (and (processing-instructions-node? node)
		 (eq? (node-name node) name)))
	  processing-instructions-node?)))
    ((attribute namespace) null-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:PROCESSING-INSTRUCTION))))

(define (node-test:node type)
  (case type
    ((element attribute namespace) all-node-test)
    (else (error:bad-range-argument type 'NODE-TEST:NODE))))

(define (null-node-test nodes) nodes '())
(define (all-node-test nodes) nodes)
(define ((simple-node-test predicate) nodes) (stream-filter predicate nodes))