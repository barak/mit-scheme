#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Argument Simplicity Analysis

(declare (usual-integrations))

(define (simplicity-analysis parallels)
  (for-each (lambda (parallel)
	      (for-each (lambda (subproblem)
			  (set-subproblem-simple?! subproblem 'UNKNOWN))
			(parallel-subproblems parallel)))
	    parallels)
  (for-each (lambda (parallel)
	      (for-each walk/subproblem (parallel-subproblems parallel)))
	    parallels))

(define (walk/subproblem subproblem)
  (if (eq? (subproblem-simple? subproblem) 'UNKNOWN)
      (update-subproblem! subproblem))
  (subproblem-simple? subproblem))

(define (new-subproblem/compute-simplicity! subproblem)
  ;; This is currently used only when `subproblem' has no prefix; if
  ;; other kinds of subproblems are supplied here, we might need to
  ;; worry about changing the node walker to handle those types of
  ;; nodes that are introduced later in the optimization process.
  (update-subproblem! subproblem))

(define (update-subproblem! subproblem)
  (set-subproblem-simple?!
   subproblem
   (if (subproblem-canonical? subproblem)
       (walk/node (subproblem-entry-node subproblem)
		  (subproblem-continuation subproblem))
       (and (walk/rvalue (subproblem-rvalue subproblem))
	    (let ((prefix (subproblem-prefix subproblem)))
	      (if (cfg-null? prefix)
		  true
		  (walk/node (cfg-entry-node prefix) false))))))
  unspecific)

(define (walk/node node continuation)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (and (for-all? (parallel-subproblems node) walk/subproblem)
	  (walk/next (snode-next node) continuation)))
    ((APPLICATION)
     (case (application-type node)
       ((COMBINATION)
	(if (combination/simple-inline? node)
	    (walk/return-operator (combination/continuation node) continuation)
	    (let ((callee (rvalue-known-value (combination/operator node))))
	      (and callee
		   (rvalue/procedure? callee)
		   (procedure-inline-code? callee)
		   (walk/next (procedure-entry-node callee) continuation)))))
       ((RETURN)
	(walk/return-operator (return/operator node) continuation))
       (else
	(error "Unknown application type" node))))
    ((ASSIGNMENT)
     (and (walk/lvalue (assignment-lvalue node))
	  (walk/rvalue (assignment-rvalue node))
	  (walk/next (snode-next node) continuation)))
    ((DEFINITION)
     (and (walk/lvalue (definition-lvalue node))
	  (walk/rvalue (definition-rvalue node))
	  (walk/next (snode-next node) continuation)))
    ((TRUE-TEST)
     (and (walk/rvalue (true-test-rvalue node))
	  (walk/next (pnode-consequent node) continuation)
	  (walk/next (pnode-alternative node) continuation)))
    ((VIRTUAL-RETURN FG-NOOP)
     (walk/next (snode-next node) continuation))))

(define (walk/next node continuation)
  (if node
      (walk/node node continuation)
      (not continuation)))

(define (walk/return-operator operator continuation)
  (and (return-operator/subproblem? operator)
       (if (eq? operator continuation)
	   true
	   (walk/next (continuation/entry-node operator) continuation))))

(define (walk/rvalue rvalue)
  (if (rvalue/reference? rvalue)
      (let ((lvalue (reference-lvalue rvalue)))
	(if (or (variable/value-variable? lvalue)
		(lvalue-known-value lvalue))
	    true
	    (walk/lvalue lvalue)))
      true))

(define (walk/lvalue lvalue)
  (not (block-passed-out? (variable-block lvalue))))