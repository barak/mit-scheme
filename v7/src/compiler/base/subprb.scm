#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/subprb.scm,v 4.7 1989/08/10 11:05:29 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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