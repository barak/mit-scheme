#| -*-Scheme-*-

$Id: blktyp.scm,v 4.18 2003/02/14 18:28:01 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990, 1999 Massachusetts Institute of Technology

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

;;;; Environment Type Assignment
;;; package: (compiler fg-optimizer setup-block-types)

(declare (usual-integrations))

(define (setup-block-types! root-block)
  (define (loop block)
    (enumeration-case block-type (block-type block)
      ((PROCEDURE)
       (if (block-passed-out? block)
	   (block-type! block block-type/ic)
	   (begin
	     (block-type! block block-type/stack)
	     (maybe-close-procedure! (block-procedure block)))))
      ((CONTINUATION)
       (for-each loop (block-children block)))
      ((EXPRESSION)
       (if (not (block-passed-out? block))
	   (error "Expression block not passed out" block))
       (block-type! block block-type/ic))
      (else
       (error "Illegal block type" block))))

  (define (block-type! block type)
    (set-block-type! block type)
    (for-each loop (block-children block)))
  
  (loop root-block)
  (if compiler:use-multiclosures?
      (merge-closure-blocks! root-block)))

(define (merge-closure-blocks! root-block)
  (define (loop block update?)
    (enumeration-case block-type (block-type block)
     ((STACK)
      (let ((procedure (block-procedure block)))
	(if (procedure/full-closure? procedure)
	    (let ((closure-block (block-parent block)))
	      (if (eq? closure-block (block-shared-block closure-block))
		  (or (attempt-child-graft block procedure update?)
		      (and update? (update-closure! procedure))))))
	(examine-children block
			  (or (attempt-children-merge block procedure update?)
			      update?))))
     ((IC CONTINUATION EXPRESSION)
      (examine-children block update?))
     (else
      (error "Illegal block type" block))))
  
  (define (examine-children block update?)
    (for-each (lambda (child)
		(loop child update?))
	      (original-block-children block)))

  (loop root-block false))

(define (original-block-children block)
  (append (block-disowned-children block)
	  (list-transform-positive
	      (block-children block)
	    (lambda (block*)
	      (eq? block (original-block-parent block*))))))

(define (maybe-close-procedure! procedure)
  (if (eq? true (procedure-closure-context procedure))
      (let ((block (procedure-block procedure))
	    (previously-trivial? (procedure/trivial-closure? procedure))
	    (original-parent (procedure-target-block procedure)))
	(let ((parent (block-parent block)))
	  (set-procedure-closure-context!
	   procedure
	   (make-reference-context original-parent))
	  (with-values
	      (lambda ()
		(let ((uninteresting-variable?
		       (lambda (variable)
			 (or (lvalue-integrated? variable)
			     (let ((value (lvalue-known-value variable)))
			       (and value
				    (or (eq? value procedure)
					(and (rvalue/procedure? value)
					     (procedure/trivial-or-virtual?
					      value)))))))))
		  (find-closure-bindings
		   original-parent
		   (list-transform-negative (block-free-variables block)
		     (lambda (lvalue)
		       (or (uninteresting-variable? lvalue)
			   (begin
			     (set-variable-closed-over?! lvalue true)
			     false))))
		   '()
		   (list-transform-negative
		       (block-variables-nontransitively-free block)
		     uninteresting-variable?))))
	    (lambda (closure-block closure-block?)
	      (transfer-block-child! block parent closure-block)
	      (set-procedure-closure-size!
	       procedure
	       (cond (closure-block?
		      (compute-closure-offsets! closure-block
						(closure-first-offset 1 0)))
		     (closure-block 1)
		     (else 0)))))
	  (set-procedure-closure-cons! procedure '(NORMAL))
	  (if previously-trivial?
	      (if (not (procedure/trivial-closure? procedure))
		  (error "trivial procedure becoming non-trivial" procedure))
	      (if (procedure/trivial-closure? procedure)
		  (warn "non-trivial procedure becoming trivial"
			procedure)))))))

(define (attempt-child-graft block procedure update?)
  (let ((block* (block-nearest-closure-ancestor
		 (procedure-target-block procedure))))
    (and block*
	 (let ((closure-block (block-parent block))
	       (ancestor-block (block-shared-block (block-parent block*))))
	   (and (for-all?
		 (refilter-variables (block-bound-variables closure-block)
				     update? procedure)
		 (let ((bvars (block-bound-variables ancestor-block)))
		   (lambda (var)
		     (or (memq var bvars)
			 (let ((val (lvalue-known-value var)))
			   (and val
				(if (rvalue/block? val)
				    (eq? val ancestor-block)
				    (and (rvalue/procedure? val)
					 (procedure/full-closure? val)
					 (eq? (block-shared-block
					       (procedure-closing-block val))
					      ancestor-block)))))))))
		(graft-child! procedure ancestor-block closure-block))))))

(define (graft-child! procedure ancestor-block closure-block)
  (for-each
   (lambda (var)
     (if (and (lvalue-known-value var)
	      (not (variable-closed-over? var))
	      (let* ((sblock (block-nearest-closure-ancestor
			      (variable-block var)))
		     (cblock (and sblock (block-parent sblock))))
		(and cblock
		     (eq? (block-shared-block cblock) ancestor-block))))
	 (lvalue-put! var 'INTEGRATED ancestor-block)))
   (procedure-variables procedure))
  (graft-block! '(DESCENDANT) ancestor-block closure-block procedure)
  true)

(define (update-closure! procedure)
  (let ((closure-block (procedure-closing-block procedure)))
    (if (not (eq? (block-shared-block closure-block) closure-block))
	(error "update-closure!: Updating shared closure" procedure))
    (let ((vars (refilter-variables (block-bound-variables closure-block)
				    true procedure)))
      (set-block-bound-variables! closure-block vars)
      (set-procedure-closure-size!
       procedure
       (compute-closure-offsets! closure-block
				 (closure-block-first-offset
				  closure-block))))))

(define (refilter-variables bvars filter? procedure)
  (if (not filter?)
      bvars
      (let loop ((vars (reverse bvars))
		 (real '())
		 (blocks '()))
	(cond ((not (null? vars))
	       (let* ((var (car vars))
		      (ind (variable-indirection var)))
		 (if ind
		     (loop (cdr vars)
			   (if (memq (car ind) real)
			       real
			       (cons (car ind) real))
			   blocks)
		     (let ((val (lvalue-known-value var)))
		       (cond ((not val)
			      (loop (cdr vars)
				    (cons var real)
				    blocks))
			     ((rvalue/block? val)
			      ;; This should not be found since this is
			      ;; only the result of this procedure itself,
			      ;; or link-children!, and either way, it
			      ;; should not be called after that point.
			      (error "refilter-variables: Block found"
				     procedure))
			     #|
			     ;; This doesn't work because these variables
			     ;; have not been indirected, so the eventual
			     ;; lookup will fail.
			     ;; We need to think about whether they can be
			     ;; indirected always.
			     ((and (rvalue/procedure? val)
				   (procedure/closure? val))
			      (let ((block
				     (block-shared-block
				      (procedure-closing-block val))))
				(if (memq block blocks)
				    (loop (cdr vars)
					  real
					  blocks)
				    (loop (cdr vars)
					  (cons var real)
					  (cons block blocks)))))
			     |#
			     (else
			      (loop (cdr vars)
				    (cons var real)
				    blocks)))))))
	      ((null? real)
	       ;; Only non-trivial closures passed here.
	       (error "refilter-variables: becoming trivial!" procedure))
	      (else real)))))

(define (attempt-children-merge block procedure update?)
  (let ((closure-children
	 (list-transform-positive
	     (original-block-children block)
	   (lambda (block*)
	     (let ((procedure* (block-procedure block*)))
	       (and procedure*
		    (procedure/full-closure? procedure*)))))))
    (and (not (null? closure-children))
	 (list-split
	  closure-children
	  (lambda (block*)
	    (procedure-get (block-procedure block*) 'UNCONDITIONAL))
	  (lambda (unconditional conditional)
	    (and (not (null? unconditional))
		 (or (not (null? conditional))
		     (not (null? (cdr unconditional))))
		 (merge-children! block procedure
				  unconditional conditional
				  update?)))))))

(define (merge-children! block procedure unconditional conditional update?)
  (let ((ic-parent
	 (let ((block
		(list-search-positive unconditional
		  (lambda (block*)
		    (block-parent (block-parent block*))))))
	   (and block
		(block-parent (block-parent block)))))
	(closed-over-variables
	 (refilter-variables
	  (reduce-right eq-set-union
			'()
			(map (lambda (block*)
			       (block-bound-variables (block-parent block*)))
			     unconditional))
	  update? (block-procedure (car unconditional)))))
    (let loop ((conditional conditional)
	       (block-closed (reverse unconditional)))
      (cond ((not (null? conditional))
	     (loop (cdr conditional)
		   (let* ((block* (car conditional))
			  (closure-block (block-parent block*)))
		     (if (and (or (not (block-parent closure-block))
				  ic-parent)
			      (for-all?
			       (refilter-variables
				(block-bound-variables closure-block)
				update? (block-procedure block*))
			       (lambda (var)
				 (or (lvalue-implicit? var unconditional)
				     (let ((ind (variable-indirection var)))
				       (memq (if ind
						 (car ind)
						 var)
					     closed-over-variables))))))
			 (cons (car conditional) block-closed)
			 block-closed))))
	    ((null? (cdr block-closed))
	     false)
	    (else
	     (link-children! block procedure (reverse block-closed)
			     ic-parent closed-over-variables))))))

(define closure-redirection-tag (intern "#[closure-redirection]"))

(define (link-children! block procedure block-closed ic-parent variables)
  ;; Possible improvement: the real free variables may be references
  ;; to closure ancestors.  At this point, all of them can be merged
  ;; with the ancestor parent!  This should be pretty rare, but...
  (list-split
   variables
   (lambda (var)
     (lvalue-implicit? var block-closed))
   (lambda (removable real)
     (if (and (null? real) (not ic-parent))
	 (error "link-children!: Trivial multiclosure" block-closed variables))
     (let ((letrec-names (procedure-names procedure))
	   (indirection-var (make-variable block closure-redirection-tag))
	   (shared-block
	    (make-closure-block
	     ic-parent
	     (reduce-right eq-set-union
			   '()
			   (map (lambda (block*)
				  (block-free-variables (block-parent block*)))
				block-closed))
	     real
	     '())))
       (set-variable-closed-over?! indirection-var true)
       (let ((cache (list shared-block)))
	 (set-lvalue-initial-values! indirection-var cache)
	 (set-lvalue-values-cache! indirection-var cache)
	 (set-lvalue-known-value! indirection-var shared-block))
       ;; what follows is a kludge to communicate with
       ;; rtlgen/rgproc.scm
       (set-procedure-names! procedure
			     (cons indirection-var letrec-names))
       (set-procedure-values! procedure
			      (cons shared-block (procedure-values procedure)))
       (set-block-bound-variables! block
				   (append (block-bound-variables block)
					   (list indirection-var)))
       (set-block-entry-number! shared-block 0)
       (for-each
	(let ((pair `(INDIRECTED . ,indirection-var)))
	  (lambda (block)
	    (graft-block! pair shared-block
			  (block-parent block) (block-procedure block))))
	block-closed)
       (let ((pair (cons indirection-var true)))
	 (for-each
	  (lambda (removable)
	    (if (not (memq removable letrec-names))
		(error "link-children!: non-letrec removable" removable))
	    (set-variable-indirection! removable pair))
	  removable)
	 (for-each
	  (lambda (name)
	    (if (not (variable-indirection name))
		(let ((proc (lvalue-known-closure name)))
		  (if (and proc
			   (eq? (block-shared-block
				 (procedure-closing-block proc))
				shared-block))
		      (set-variable-indirection! name pair)))))
	  letrec-names)
	 true)))))

(define (graft-block! how-consed block block* procedure*)
  (if (or (closure-procedure-needs-external-descriptor? procedure*)
	  ;; Known lexpr closures are invoked through apply.
	  (procedure-rest procedure*))
      (let ((entry (block-entry-number block)))
	(if (zero? entry)
	    (set-block-procedure! block procedure*))
	(set-block-entry-number! block (1+ entry))
	(set-block-entry-number! block* entry))
      (set-block-entry-number! block* 0))
  (let ((parent (block-parent block))
	(parent* (block-parent block*)))
    (cond ((not parent*)
	   (if parent
	       (set-block-parent! block* parent)))
	  ((not parent)
	   (set-block-parent! block parent*)
	   (for-each (lambda (block**)
		       (set-block-parent! block** parent*))
		     (block-grafted-blocks block)))
	  ((not (eq? parent parent*))
	   (error "graft-block!: Differing parents" block block*))))
  (set-procedure-closure-cons! procedure* how-consed)
  (set-block-shared-block! block* block)
  ;; Note that the list of grafted blocks are in decreasing entry
  ;; number order, except for those that have 0 as their entry number
  ;; (and thus don't need entries).  This is used to advantage in
  ;; make-non-trivial-closure-cons in rtlgen/rgrval.scm .
  (let ((new-grafts (cons block* (block-grafted-blocks block))))
    (set-block-grafted-blocks! block new-grafts)
    (for-each (let ((bvars (block-bound-variables block)))
		(lambda (block*)
		  (set-block-bound-variables! block* bvars)
		  (let ((size
			 (compute-closure-offsets!
			  block*
			  (closure-block-first-offset block*))))
		    (if (not (null? (block-children block*)))
			(set-procedure-closure-size!
			 (block-procedure (car (block-children block*)))
			 size)))))
	      (cons block new-grafts))))

;;; Utilities that should live elsewhere

(define (indirection-block-procedure block)
  (or (block-procedure block)
      (if (null? (block-grafted-blocks block))
	  (error "indirection-block-procedure: Bad indirection block" block)
	  (block-procedure
	   (car (block-children
		 (car (block-grafted-blocks block))))))))

(define (lvalue-implicit? var blocks)
  (let ((val (lvalue-known-value var)))
    (and val
	 (rvalue/procedure? val)
	 (memq (procedure-block val) blocks))))

(define (lvalue-known-closure var)
  (let ((val (lvalue-known-value var)))
    (and val
	 (rvalue/procedure? val)
	 (procedure/full-closure? val)
	 val)))

(define-integrable (procedure/full-closure? proc)
  (and (procedure/closure? proc)
       (not (procedure/trivial-closure? proc))))

(define (list-split list predicate recvr)
  (let split ((list list)
	      (recvr recvr))
    (if (not (pair? list))
	(recvr '() '())
	(let ((next (car list)))
	  (split (cdr list)
		 (if (predicate next)
		     (lambda (win lose)
		       (recvr (cons next win) lose))
		     (lambda (win lose)
		       (recvr win (cons next lose)))))))))

(define (find-closure-bindings block free-variables bound-variables
			       variables-nontransitively-free)
  (if (or (not block) (ic-block? block))
      (let ((grandparent (and (not (null? free-variables)) block)))
	(if (null? bound-variables)
	    (values grandparent false)
	    (values
	     (make-closure-block grandparent
				 free-variables
				 bound-variables
				 variables-nontransitively-free)
	     true)))
      (with-values
	  (lambda ()
	    (filter-bound-variables (block-bound-variables block)
				    free-variables
				    bound-variables))
	(lambda (free-variables bound-variables)
	  (find-closure-bindings (original-block-parent block)
				 free-variables
				 bound-variables
				 variables-nontransitively-free)))))

(define (filter-bound-variables bindings free-variables bound-variables)
  (cond ((null? bindings)
	 (values free-variables bound-variables))
	((memq (car bindings) free-variables)
	 (filter-bound-variables (cdr bindings)
				 (delq! (car bindings) free-variables)
				 (cons (car bindings) bound-variables)))
	(else
	 (filter-bound-variables (cdr bindings)
				 free-variables
				 bound-variables))))

(define (make-closure-block parent free-variables bound-variables
			    variables-nontransitively-free)
  (let ((block (make-block parent 'CLOSURE)))
    (set-block-free-variables! block free-variables)
    (set-block-bound-variables! block bound-variables)
    (set-block-variables-nontransitively-free!
     block
     variables-nontransitively-free)
    (set-block-shared-block! block block)
    (set-block-entry-number! block 1)
    (set-block-grafted-blocks! block '())
    block))

(define (compute-closure-offsets! block offset)
  (if block
      (let ((parent (block-parent block)))
	(do ((variables (block-bound-variables block) (cdr variables))
	     (size (if (and parent (ic-block/use-lookup? parent)) 1 0)
		   (1+ size))
	     (table '()
		    (cons (cons (car variables) (+ offset size))
			  table)))
	    ((null? variables)
	     (set-block-closure-offsets! block table)
	     size)
	  (if (lvalue-integrated? (car variables))
	      (error "compute-closure-offsets!: integrated lvalue"
		     (car variables)))))
      0))

;;;; Reference contexts in which procedures are closed.
;;; Needed to determine the access paths of free variables to close over.

(define (setup-closure-contexts! expression procedures)
  (with-new-node-marks
   (lambda ()
     (setup-closure-contexts/node (expression-entry-node expression))
     (for-each
      (lambda (procedure)
	(setup-closure-contexts/next (procedure-entry-node procedure)))
      procedures))))

(define (setup-closure-contexts/next node)
  (if (and node (not (node-marked? node)))
      (setup-closure-contexts/node node)))

(define (setup-closure-contexts/node node)
  (node-mark! node)
  (cfg-node-case (tagged-vector/tag node)
    ((PARALLEL)
     (for-each
      (lambda (subproblem)
	(let ((prefix (subproblem-prefix subproblem)))
	  (if (not (cfg-null? prefix))
	      (setup-closure-contexts/next (cfg-entry-node prefix))))
	(if (not (subproblem-canonical? subproblem))
	    (setup-closure-contexts/rvalue
	     (virtual-continuation/context
	      (subproblem-continuation subproblem))
	     (subproblem-rvalue subproblem))))
      (parallel-subproblems node))
     (setup-closure-contexts/next (snode-next node)))
    ((APPLICATION)
     (if (application/return? node)
	 (let ((context (application-context node)))
	   (setup-closure-contexts/rvalue context (application-operator node))
	   (for-each (lambda (operand)
		       (setup-closure-contexts/rvalue context operand))
		     (application-operands node))))
     (setup-closure-contexts/next (snode-next node)))
    ((VIRTUAL-RETURN)
     (let ((context (virtual-return-context node)))
       (setup-closure-contexts/rvalue context (virtual-return-operand node))
       (let ((continuation (virtual-return-operator node)))
	 (if (virtual-continuation/reified? continuation)
	     (setup-closure-contexts/rvalue
	      context
	      (virtual-continuation/reification continuation)))))
     (setup-closure-contexts/next (snode-next node)))
    ((ASSIGNMENT)
     (setup-closure-contexts/rvalue (assignment-context node)
				    (assignment-rvalue node))
     (setup-closure-contexts/next (snode-next node)))
    ((DEFINITION)
     (setup-closure-contexts/rvalue (definition-context node)
				    (definition-rvalue node))
     (setup-closure-contexts/next (snode-next node)))
    ((TRUE-TEST)
     (setup-closure-contexts/rvalue (true-test-context node)
				    (true-test-rvalue node))
     (setup-closure-contexts/next (pnode-consequent node))
     (setup-closure-contexts/next (pnode-alternative node)))
    ((STACK-OVERWRITE POP FG-NOOP)
     (setup-closure-contexts/next (snode-next node)))))

(define (setup-closure-contexts/rvalue context rvalue)
  (if (and (rvalue/procedure? rvalue)
	   (let ((context* (procedure-closure-context rvalue)))
	     (and (reference-context? context*)
		  (begin
		    (if (not (eq? (reference-context/block context)
				  (reference-context/block context*)))
			(error "mismatched reference contexts"
			       context context*))
		    (not (eq? context context*))))))
      (set-procedure-closure-context! rvalue context)))