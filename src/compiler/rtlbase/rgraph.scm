#| -*-Scheme-*-

$Id: rgraph.scm,v 4.11 2004/12/06 02:34:04 cph Exp $

Copyright 1987,1988,1989,1990,2004 Massachusetts Institute of Technology

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

;;;; Program Graph Abstraction

(declare (usual-integrations))

(define-structure (rgraph (type vector)
			  (copier #f)
			  (constructor make-rgraph (n-registers)))
  n-registers
  (entry-edges '())
  (bblocks '())
  register-bblock
  register-n-refs
  register-n-deaths
  register-live-length
  register-crosses-call?
  register-value-classes
  register-known-values)

(define (add-rgraph-bblock! rgraph bblock)
  (set-rgraph-bblocks! rgraph (cons bblock (rgraph-bblocks rgraph))))

(define (delete-rgraph-bblock! rgraph bblock)
  (set-rgraph-bblocks! rgraph (delq! bblock (rgraph-bblocks rgraph))))

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