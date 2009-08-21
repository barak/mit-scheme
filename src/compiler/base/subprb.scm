#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Subproblem Type

(declare (usual-integrations))

#|

Subproblems come in two forms, canonical and non-canonical.  In a
canonical subproblem, the `prefix' is always exited by a return
statement whose operator is the subproblem's `continuation'.  The
`rvalue' is always the parameter of the `continuation'.

In a non-canonical subproblem, there is no `continuation' -- the
`rvalue' is sufficiently simple that no complex computation is
required to compute its value.  Instead, the `prefix' is some setup
code that must be executed for effect, while the value of the
subproblem is just `rvalue'.

The non-canonical subproblem is used as an optimization by several
parts of the compiler, where better code can be generated if it is
known that the continuation need not be used.

|#

(define-structure (subproblem
		   (constructor make-subproblem
				(prefix continuation rvalue)))
  (prefix false read-only true)
  (continuation false read-only true)
  (rvalue false read-only true)
  (simple? 'UNKNOWN)
  (free-variables 'UNKNOWN))

(define-integrable (subproblem-entry-node subproblem)
  (cfg-entry-node (subproblem-prefix subproblem)))

(define-integrable (subproblem-canonical? subproblem)
  (procedure? (subproblem-continuation subproblem)))

(define-integrable (subproblem-type subproblem)
  (continuation*/type (subproblem-continuation subproblem)))

(define-integrable (set-subproblem-type! subproblem type)
  (set-continuation*/type! (subproblem-continuation subproblem) type))

(define-integrable (subproblem-register subproblem)
  (continuation*/register (subproblem-continuation subproblem)))

(define (subproblem-context subproblem)
  (continuation*/context (subproblem-continuation subproblem)))

(define (continuation*/type continuation)
  (if (procedure? continuation)
      (continuation/type continuation)
      (virtual-continuation/type continuation)))

(define (set-continuation*/type! continuation type)
  (if (procedure? continuation)
      (set-continuation/type! continuation type)
      (set-virtual-continuation/type! continuation type)))

(define (continuation*/register continuation)
  (if (procedure? continuation)
      (continuation/register continuation)
      (virtual-continuation/register continuation)))

(define (continuation*/context continuation)
  (let ((continuation/context
	 (lambda (continuation)
	   (make-reference-context
	    (block-parent (continuation/block continuation))))))
    (cond ((procedure? continuation)
	   (continuation/context continuation))
	  ((virtual-continuation/reified? continuation)
	   (continuation/context
	    (virtual-continuation/reification continuation)))
	  (else
	   (virtual-continuation/context continuation)))))

;;;; Virtual Continuations

;;; These are constructed in the FG generation phase for the purpose
;;; of delaying generation of real continuations until the last
;;; possible moment.  After the FG generation, non-reified virtual
;;; continuations are used to hold several values that normally would
;;; have resided in the real continuation.

(define-structure (virtual-continuation
		   (constructor virtual-continuation/%make)
		   (conc-name virtual-continuation/)
		   (print-procedure
		    (standard-unparser (symbol->string 'VIRTUAL-CONTINUATION)
		      (lambda (state continuation)
			(let ((type (virtual-continuation/type continuation)))
			  (if type
			      (unparse-object
			       state
			       (enumeration/index->name continuation-types
							type))))))))
  context
  parent
  type
  debugging)

(define-integrable (virtual-continuation/make block type)
  ;; Used exclusively after FG generation.
  (virtual-continuation/%make block false type false))

(define-integrable (virtual-continuation/reified? continuation)
  (not (virtual-continuation/type continuation)))

(define-integrable virtual-continuation/reification
  virtual-continuation/context)

(define (virtual-continuation/reify! continuation)
  ;; This is used only during FG generation when it is decided that we
  ;; need a real continuation to handle a subproblem.
  (if (virtual-continuation/type continuation)
      (let ((reification
	     (make-continuation
	      (virtual-continuation/context continuation)
	      (virtual-continuation/parent continuation)
	      (virtual-continuation/type continuation))))
	(set-continuation/debugging-info!
	 reification
	 (virtual-continuation/debugging continuation))
	(set-virtual-continuation/context! continuation reification)
	(set-virtual-continuation/parent! continuation false)
	(set-virtual-continuation/type! continuation false)
	reification)
      (virtual-continuation/context continuation)))

(define (virtual-continuation/register continuation)
  (or (virtual-continuation/parent continuation)
      (let ((register (rtl:make-pseudo-register)))
	(set-virtual-continuation/parent! continuation register)
	register)))