#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rgraph.scm,v 4.6 1989/10/26 07:38:21 cph Exp $

Copyright (c) 1987, 1988, 1989 Massachusetts Institute of Technology

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

;;;; Program Graph Abstraction

(declare (usual-integrations))

(define-structure (rgraph (type vector)
			  (copier false)
			  (constructor make-rgraph (n-registers)))
  n-registers
  (non-object-registers (reverse initial-non-object-registers))
  entry-edges
  bblocks
  register-bblock
  register-n-refs
  register-n-deaths
  register-live-length
  register-crosses-call?
  register-value-classes)

(define (add-rgraph-bblock! rgraph bblock)
  (set-rgraph-bblocks! rgraph (cons bblock (rgraph-bblocks rgraph))))

(define (delete-rgraph-bblock! rgraph bblock)
  (set-rgraph-bblocks! rgraph (delq! bblock (rgraph-bblocks rgraph))))

(define (add-rgraph-non-object-register! rgraph register)
  (set-rgraph-non-object-registers!
   rgraph
   (cons register (rgraph-non-object-registers rgraph))))

(define (add-rgraph-entry-edge! rgraph edge)
  (set-rgraph-entry-edges! rgraph (cons edge (rgraph-entry-edges rgraph))))

(define-integrable rgraph-register-renumber rgraph-register-bblock)
(define-integrable set-rgraph-register-renumber! set-rgraph-register-bblock!)

;;; Pseudo-register value classes are kept on an association list between value
;;; classes and lists of pseudo-registers in the class.  A register not found
;;; in any value class list is assumed to have class VALUE, the broadest and
;;; most common class.  This minimizes the space used to store register value
;;; classifiations at the expense of reduced speed.  It is illegal to change
;;; the value class of a pseudo-register unless its current class is VALUE
;;; (completely unspecified); this restriction is checked.

(define (rgraph-register-value-class rgraph register)
  (let loop ((classes (rgraph-register-value-classes rgraph)))
    (if (null? classes)
	'VALUE
	(let ((class-list (car classes)))
	  (if (memq register (cdr class-list))
	      (car class-list)
	      (loop (cdr classes)))))))

(define (set-rgraph-register-value-class! rgraph register value-class)
  (let ((old-value-class (rgraph-register-value-class rgraph register)))
    (if (eq? old-value-class 'VALUE)
	(if (not (eq? value-class 'VALUE))
	    (let loop ((classes (rgraph-register-value-classes rgraph)))
	      (if (null? classes)
		  (set-rgraph-register-value-classes!
		   rgraph
		   (cons (list value-class register)
			 (rgraph-register-value-classes rgraph)))
		  (let ((class-list (car classes)))
		    (if (eq? value-class (car class-list))
			(let ((register-list (cdr class-list)))
			  (if (not (memq register register-list))
			      (set-cdr! class-list (cons register register-list))))
			(loop (cdr classes)))))))
	(if (not (eq? old-value-class value-class))
	    (error "Illegal register value class change" register value-class)))))

(define *rgraphs*)
(define *current-rgraph*)

(define (rgraph-initial-edges rgraph)
  (list-transform-positive (rgraph-entry-edges rgraph)
    (lambda (edge)
      (node-previous=0? (edge-right-node edge)))))