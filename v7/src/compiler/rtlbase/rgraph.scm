#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rgraph.scm,v 4.4 1988/11/02 21:51:17 cph Rel $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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
  )
(define (add-rgraph-non-object-register! rgraph register)
  (set-rgraph-non-object-registers!
   rgraph
   (cons register (rgraph-non-object-registers rgraph))))

(define (add-rgraph-entry-edge! rgraph edge)
  (set-rgraph-entry-edges! rgraph (cons edge (rgraph-entry-edges rgraph))))

(define-integrable rgraph-register-renumber rgraph-register-bblock)
(define-integrable set-rgraph-register-renumber! set-rgraph-register-bblock!)
(define *rgraphs*)
(define *current-rgraph*)

(define (rgraph-initial-edges rgraph)
  (list-transform-positive (rgraph-entry-edges rgraph)
    (lambda (edge)
      (node-previous=0? (edge-right-node edge)))))