#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/infnew.scm,v 4.2 1988/04/15 02:08:43 jinx Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Debugging information output.

(declare (usual-integrations))

(define (generation-phase2 label-bindings external-labels)
  (make-compiler-info
   '()
   '()
   (list->vector
    (sort (map (lambda (association)
		 (make-label-info
		  (symbol->string (car association))
		  (cdr association)
		  (let loop ((external-labels external-labels))
		    (cond ((null? external-labels) false)
			  ((eq? (car association) (car external-labels)) true)
			  (else (loop (cdr external-labels)))))))
	       label-bindings)
	  (lambda (x y)
	    (< (label-info-offset x) (label-info-offset y)))))))

(define (generate-vector top-level selector others)
  (let* ((last (length others))
	 (v (make-vector (1+ last) '())))
    (vector-set! v 0 top-level)
    (let loop ((l others))
      (if (null? l)
	  v
	  (let ((desc (car l)))
	    (vector-set! v (car desc) (selector desc))
	    (loop (cdr l)))))))

(define (generate-top-level-info top-level others)
  (if (null? others)
      top-level
      (generate-vector top-level cadr others)))

(define (generate-top-level-object top-level others)
  (if (null? others)
      top-level
      (scode/make-comment
       (list compiler-entries-tag
	     (generate-vector (compiled-code-address->block top-level)
			      caddr others))
       top-level)))