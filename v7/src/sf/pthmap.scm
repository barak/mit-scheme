#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/pthmap.scm,v 4.1 1988/06/13 12:30:05 cph Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Pathname Maps

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution))

(define pathname-map/make)
(define pathname-map?)
(define pathname-map/lookup)
(define pathname-map/insert!)
(let ()

(set! pathname-map/make
  (named-lambda (pathname-map/make)
    (cons pathname-map/tag (node/make))))

(set! pathname-map?
  (named-lambda (pathname-map? object)
    (and (pair? object)
	 (eq? (car object) pathname-map/tag))))

(define pathname-map/tag "pathname-map")
(define pathname-map/root-node cdr)

(unparser/set-tagged-pair-method!
 pathname-map/tag
 (unparser/standard-method "PATHNAME-MAP"))

(declare (integrate-operator node/make))

(define (node/make)
  (cons unbound-value '()))

(define unbound-value "unbound-value")
(define node/value car)
(define set-node/value! set-car!)
(define node/alist cdr)
(define set-node/alist! set-cdr!)

(define (node/associate node key)
  (let ((entry (assoc key (node/alist node))))
    (and entry
	 (cdr entry))))

(define (make-node-list pathname)
  (cons-if (pathname-device pathname)
	   (append (pathname-directory pathname)
		   (cons-if (pathname-name pathname)
			    (cons-if (pathname-type pathname)
				     (cons-if (pathname-version pathname)
					      '()))))))

(declare (integrate-operator cons-if))

(define (cons-if item rest)
  (if item
      (cons item rest)
      rest))

(set! pathname-map/lookup
  (named-lambda (pathname-map/lookup map pathname if-found if-not)
    (let ((node
	   (find-node (pathname-map/root-node map)
		      (make-node-list pathname))))
      (if node
	  (let ((value (node/value node)))
	    (if (eq? value unbound-value)
		(if-not)
		(if-found value)))
	  (if-not)))))

(set! pathname-map/insert!
  (named-lambda (pathname-map/insert! map pathname value)
    (set-node/value! (find-or-create-node (pathname-map/root-node map)
					  (make-node-list pathname))
		     value)))

(define (find-node node node-list)
  (if (null? node-list)
      node
      (let ((node (node/associate node (car node-list))))
	(and node
	     (find-node node (cdr node-list))))))

(define (find-or-create-node node node-list)
  (if (null? node-list)
      node
      (let ((next (node/associate node (car node-list))))
	(if next
	    (find-or-create-node next (cdr node-list))
	    (create-node node node-list)))))

(define (create-node node node-list)
  (let ((next (node/make)))
    (set-node/alist! node
		     (cons (cons (car node-list) next)
			   (node/alist node)))
    (if (null? (cdr node-list))
	next
	(create-node next (cdr node-list)))))

)