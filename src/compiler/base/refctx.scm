#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Reference Contexts

(declare (usual-integrations))

;;; In general, generating code for variable (and block) references
;;; requires only two pieces of knowledge: the block in which the
;;; reference occurs, and the block being referenced (in the case of
;;; variables, the latter is the block in which the variable is
;;; bound).  Usually the location of the parent of a given block is
;;; precisely known, e.g. as a stack offset from that block, and in
;;; cases where different locations are possible, an explicit static
;;; link is used to provide that location.

;;; In the case where static links are normally used, it is sometimes
;;; possible to bypass a static link for a particular reference: this
;;; because the knowledge of the reference's position within the
;;; program's control structure implies that the parent block is in a
;;; known location.  In other words, even though that parent block can
;;; have several different locations relative to its child, from that
;;; particular place in the program only one of those locations is
;;; possible.

;;; Reference contexts are a mechanism to capture this kind of control
;;; structure dependent knowledge.  Basically, every point in the flow
;;; graph that does some kind of environment reference keeps a pointer
;;; to a reference context.  These reference contexts can be
;;; independently changed to annotate interesting facts.

(define reference-context-tag
  ;; This tag is used to prevent `define-structure' from redefining
  ;; the variable `reference-context'.
  "reference-context")

(define-structure (reference-context
		   (type vector)
		   (named reference-context-tag)
		   (constructor make-reference-context (block))
		   (conc-name reference-context/))
  (block false read-only true)
  (offset false)
  (adjacent-parents '()))

(define-integrable (reference-context/procedure context)
  (block-procedure (reference-context/block context)))

(define-integrable (reference-context/adjacent-parent? context block)
  (memq block (reference-context/adjacent-parents context)))

(define (add-reference-context/adjacent-parents! context blocks)
  (set-reference-context/adjacent-parents!
   context
   (eq-set-union blocks (reference-context/adjacent-parents context))))

#|
(define (node/reference-context node)
  (cfg-node-case (tagged-vector/tag node)
    ((APPLICATION) (application-context node))
    ((VIRTUAL-RETURN) (virtual-return-context node))
    ((ASSIGNMENT) (assignment-context node))
    ((DEFINITION) (definition-context node))
    ((STACK-OVERWRITE) (stack-overwrite-context node))
    ((TRUE-TEST) (true-test-context node))
    ((PARALLEL POP FG-NOOP) false)))
|#

;;; Once the FG graph has been constructed, this procedure will walk
;;; over it and install reference contexts in all the right places.
;;; It will also guarantee that all of the rvalues associated with a
;;; particular CFG node have the same context as the node.  This means
;;; that subsequently it is only necessary to walk over the CFG nodes
;;; and modify their contexts.

(define (initialize-reference-contexts! expression procedures)
  (with-new-node-marks
   (lambda ()
     (initialize-contexts/node (expression-entry-node expression))
     (for-each (lambda (procedure)
		 (initialize-contexts/next (procedure-entry-node procedure)))
	       procedures))))

(define (initialize-contexts/next node)
  (if (and node (not (node-marked? node)))
      (initialize-contexts/node node)))

(define (initialize-contexts/node node)
  (node-mark! node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (initialize-contexts/parallel node)
     (initialize-contexts/next (snode-next node)))
    ((APPLICATION)
     (initialize-contexts/application node)
     (initialize-contexts/next (snode-next node)))
    ((VIRTUAL-RETURN)
     (initialize-contexts/virtual-return node)
     (initialize-contexts/next (snode-next node)))
    ((ASSIGNMENT)
     (initialize-contexts/assignment node)
     (initialize-contexts/next (snode-next node)))
    ((DEFINITION)
     (initialize-contexts/definition node)
     (initialize-contexts/next (snode-next node)))
    ((STACK-OVERWRITE)
     (initialize-contexts/stack-overwrite node)
     (initialize-contexts/next (snode-next node)))
    ((POP FG-NOOP)
     (initialize-contexts/next (snode-next node)))
    ((TRUE-TEST)
     (initialize-contexts/true-test node)
     (initialize-contexts/next (pnode-consequent node))
     (initialize-contexts/next (pnode-alternative node)))))

(define (initialize-contexts/parallel parallel)
  (for-each
   (lambda (subproblem)
     (let ((prefix (subproblem-prefix subproblem)))
       (if (not (cfg-null? prefix))
	   (initialize-contexts/next (cfg-entry-node prefix))))
     (if (subproblem-canonical? subproblem)
	 (initialize-contexts/reference (subproblem-rvalue subproblem))
	 (let* ((continuation (subproblem-continuation subproblem))
		(old (virtual-continuation/context continuation))
		(new (guarantee-context old)))
	   (if new
	       (begin
		 (set-virtual-continuation/context! continuation new)
		 (initialize-contexts/rvalue
		  old new
		  (subproblem-rvalue subproblem)))))))
   (parallel-subproblems parallel)))

(define (initialize-contexts/application application)
  (let* ((old (application-context application))
	 (new (guarantee-context old)))
    (if new
	(begin
	  (set-application-context! application new)
	  (if (application/return? application)
	      (begin
		(initialize-contexts/rvalue old new
					    (application-operator application))
		(for-each (lambda (operand)
			    (initialize-contexts/rvalue old new operand))
			  (application-operands application)))))))
  unspecific)

(define (initialize-contexts/virtual-return return)
  (let* ((old (virtual-return-context return))
	 (new (guarantee-context old)))
    (if new
	(begin
	  (set-virtual-return-context! return new)
	  (initialize-contexts/rvalue old new (virtual-return-operand return))
	  (let ((continuation (virtual-return-operator return)))
	    (if (virtual-continuation/reified? continuation)
		(initialize-contexts/rvalue
		 old
		 new
		 (virtual-continuation/reification continuation))
		(guarantee-context! old new continuation
				    virtual-continuation/context
				    set-virtual-continuation/context!)))))))

(define (initialize-contexts/assignment assignment)
  (let* ((old (assignment-context assignment))
	 (new (guarantee-context old)))
    (if new
	(begin
	  (set-assignment-context! assignment new)
	  (initialize-contexts/rvalue old new
				      (assignment-rvalue assignment))))))

(define (initialize-contexts/definition assignment)
  (let* ((old (definition-context assignment))
	 (new (guarantee-context old)))
    (if new
	(begin
	  (set-definition-context! assignment new)
	  (initialize-contexts/rvalue old new
				      (definition-rvalue assignment))))))

(define (initialize-contexts/stack-overwrite assignment)
  (let* ((old (stack-overwrite-context assignment))
	 (new (guarantee-context old)))
    (if new
	(set-stack-overwrite-context! assignment new)))
  unspecific)

(define (initialize-contexts/true-test true-test)
  (let* ((old (true-test-context true-test))
	 (new (guarantee-context old)))
    (if new
	(begin
	  (set-true-test-context! true-test new)
	  (initialize-contexts/rvalue old new (true-test-rvalue true-test))))))

(define (initialize-contexts/rvalue old new rvalue)
  (enumeration-case rvalue-type (tagged-vector/index rvalue)
    ((REFERENCE)
     (if (variable/value-variable? (reference-lvalue rvalue))
	 (initialize-contexts/reference rvalue)
	 (guarantee-context! old new rvalue
			     reference-context set-reference-context!)))
    ((UNASSIGNED-TEST)
     (guarantee-context! old new rvalue
			 unassigned-test-context set-unassigned-test-context!))
    ((PROCEDURE)
     (let ((context (procedure-closure-context rvalue)))
       (cond ((reference? context)
	      (initialize-contexts/reference context))
#|
	     ;; Unnecessary because no procedures have closure
	     ;; contexts when initialize-contexts is run.
	     ((block? context)
	      (guarantee-context! old new rvalue
				  procedure-closure-context
				  set-procedure-closure-context!))
|#
	     )))))

(define (initialize-contexts/reference rvalue)
  (set-reference-context! rvalue
			  (make-reference-context (reference-context rvalue))))

(define-integrable (guarantee-context! old new object context set-context!)
  (guarantee-context!/check-old old (context object))
  (set-context! object new)
  unspecific)

(define (guarantee-context!/check-old old context)
  (if (not (eq? old context))
      (error "Reference context mismatch" old context)))

(define (guarantee-context old)
  (and (block? old)
       (make-reference-context old)))

(define (modify-reference-contexts! node limit modification)
  (with-new-node-marks
   (lambda ()
     (if limit (node-mark! limit))
     (modify-contexts/node modification node))))

(define (modify-contexts/node modification node)
  (node-mark! node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (for-each
      (lambda (subproblem)
	(let ((prefix (subproblem-prefix subproblem)))
	  (if (not (cfg-null? prefix))
	      (modify-contexts/next modification (cfg-entry-node prefix))))
	(if (not (subproblem-canonical? subproblem))
	    (modification
	     (virtual-continuation/context
	      (subproblem-continuation subproblem)))))
      (parallel-subproblems node))
     (modify-contexts/next modification (snode-next node)))
    ((APPLICATION)
     (modification (application-context node))
     (modify-contexts/operator modification (application-operator node))
     (modify-contexts/next modification (snode-next node)))
    ((VIRTUAL-RETURN)
     (modification (virtual-return-context node))
     (let ((continuation (virtual-return-operator node)))
       (if (virtual-continuation/reified? continuation)
	   (modify-contexts/operator
	    modification
	    (virtual-continuation/reification continuation))
	   (modification (virtual-continuation/context continuation))))
     (modify-contexts/next modification (snode-next node)))
    ((ASSIGNMENT)
     (modification (assignment-context node))
     (modify-contexts/next modification (snode-next node)))
    ((DEFINITION)
     (modification (definition-context node))
     (modify-contexts/next modification (snode-next node)))
    ((STACK-OVERWRITE)
     (modification (stack-overwrite-context node))
     (modify-contexts/next modification (snode-next node)))
    ((POP FG-NOOP)
     (modify-contexts/next modification (snode-next node)))
    ((TRUE-TEST)
     (modification (true-test-context node))
     (modify-contexts/next modification (pnode-consequent node))
     (modify-contexts/next modification (pnode-alternative node)))))

(define (modify-contexts/operator modification rvalue)
  (let ((value (rvalue-known-value rvalue)))
    (if (and value (rvalue/procedure? value))
	(modify-contexts/next modification (procedure-entry-node value)))))

(define (modify-contexts/next modification node)
  (if (and node (not (node-marked? node)))
      (modify-contexts/node modification node)))