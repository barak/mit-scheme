#| -*-Scheme-*-

$Id: rgrval.scm,v 4.25 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; RTL Generation: RValues
;;; package: (compiler rtl-generator generate/rvalue)

(declare (usual-integrations))

(define (generate/rvalue operand scfg*cfg->cfg! generator)
  (with-values (lambda () (generate/rvalue* operand))
    (lambda (prefix expression)
      (scfg*cfg->cfg! prefix (generator expression)))))

(define (generate/rvalue* operand)
  ((method-table-lookup rvalue-methods (tagged-vector/index operand)) operand))

(define rvalue-methods
  (make-method-table rvalue-types false))

(define-integrable (expression-value/simple expression)
  (values (make-null-cfg) expression))

(define (expression-value/temporary prefix result)
  (load-temporary-register
   (lambda (assignment reference)
     (values (scfg*scfg->scfg! prefix assignment) reference))
   result
   identity-procedure))

(define-method-table-entry 'CONSTANT rvalue-methods
  (lambda (constant)
    (expression-value/simple (rtl:make-constant (constant-value constant)))))

(define-method-table-entry 'BLOCK rvalue-methods
  (lambda (block)
    block ;; ignored
    (expression-value/simple (rtl:make-fetch register:environment))))

(define-method-table-entry 'REFERENCE rvalue-methods
  (lambda (reference)
    (let ((context (reference-context reference))
	  (lvalue (reference-lvalue reference))
	  (safe? (reference-safe? reference)))
      (let ((value (lvalue-known-value lvalue))
	    #| (indirection (variable-indirection lvalue)) |#
	    (perform-fetch
	     (lambda (#| lvalue |#)
	       (find-variable/value context lvalue
		 expression-value/simple
		 (lambda (environment name)
		   (expression-value/temporary
		    (load-temporary-register scfg*scfg->scfg! environment
		      (lambda (environment)
			(wrap-with-continuation-entry
			 context
			 (lambda (cont-label)
			   (rtl:make-interpreter-call:lookup
			    cont-label
			    environment
			    (intern-scode-variable!
			     (reference-context/block context)
			     name)
			    safe?)))))
		    (rtl:interpreter-call-result:lookup)))
		 (lambda (name)
		   (if (memq 'IGNORE-REFERENCE-TRAPS
			     (variable-declarations lvalue))
		       (load-temporary-register values
			   (rtl:make-variable-cache name)
			 rtl:make-fetch)
		       (generate/cached-reference context name safe?)))))))
	(cond ((not value)
	       #|
	       (if (and indirection (cdr indirection))
		   (error "reference: Unknown mapped indirection"
			  lvalue))
	       |#
	       (perform-fetch #| (if indirection (car indirection) lvalue) |#))
	      ((not (rvalue/procedure? value))
	       (generate/rvalue* value))
	      #|
	      ((procedure/trivial-or-virtual? value)
	       (expression-value/simple (make-trivial-closure-cons value)))
	      ((and indirection (cdr indirection))
	       (generate/indirected-closure indirection value context
					    reference))
	      |#
	      (else
	       (perform-fetch #| lvalue |#)))))))

(define (generate/cached-reference context name safe?)
  (let ((result (rtl:make-pseudo-register)))
    (values
     (load-temporary-register scfg*scfg->scfg! (rtl:make-variable-cache name)
       (lambda (cell)
	 (let ((reference (rtl:make-fetch cell)))
	   (let ((n2 (rtl:make-type-test (rtl:make-object->type reference)
					 (ucode-type reference-trap)))
		 (n3 (rtl:make-assignment result reference))
		 (n4
		  (wrap-with-continuation-entry
		   context
		   (lambda (cont-label)
		     (rtl:make-interpreter-call:cache-reference
		      cont-label cell safe?))))
		 (n5
		  (rtl:make-assignment
		   result
		   (rtl:interpreter-call-result:cache-reference))))
	     (pcfg-alternative-connect! n2 n3)
	     (scfg-next-connect! n4 n5)
	     (if safe?
		 (let ((n6 (rtl:make-unassigned-test reference))
		       ;; Make new copy of n3 to keep CSE happy.
		       ;; Otherwise control merge will confuse it.
		       (n7 (rtl:make-assignment result reference)))
		   (pcfg-consequent-connect! n2 n6)
		   (pcfg-consequent-connect! n6 n7)
		   (pcfg-alternative-connect! n6 n4)
		   (make-scfg (cfg-entry-node n2)
			      (hooks-union
			       (scfg-next-hooks n3)
			       (hooks-union (scfg-next-hooks n5)
					    (scfg-next-hooks n7)))))
		 (begin
		   (pcfg-consequent-connect! n2 n4)
		   (make-scfg (cfg-entry-node n2)
			      (hooks-union (scfg-next-hooks n3)
					   (scfg-next-hooks n5)))))))))
     (rtl:make-fetch result))))

(define-method-table-entry 'PROCEDURE rvalue-methods
  (lambda (procedure)
    (enqueue-procedure! procedure)
    (case (procedure/type procedure)
      ((TRIVIAL-CLOSURE)
       (expression-value/simple (make-trivial-closure-cons procedure)))
      ((CLOSURE)
       (case (car (procedure-closure-cons procedure))
	 ((NORMAL)
	  (load-temporary-register
	   (lambda (assignment reference)
	     (values
	      (scfg*scfg->scfg!
	       assignment
	       (load-closure-environment procedure reference false))
	      reference))
	   (make-non-trivial-closure-cons procedure false)
	   identity-procedure))
	 ((DESCENDANT)
	  (expression-value/simple
	   (make-cons-closure-redirection procedure)))
	 (else
	  (expression-value/simple
	   (make-cons-closure-indirection procedure)))))
      ((IC)
       (make-ic-cons procedure))
      ((OPEN-EXTERNAL OPEN-INTERNAL)
       (if (not (procedure-virtual-closure? procedure))
	   (error "Reference to open procedure" procedure))
       (expression-value/simple (make-trivial-closure-cons procedure)))
      (else
       (error "Unknown procedure type" procedure)))))

(define (make-ic-cons procedure)
  ;; IC procedures have their entry points linked into their headers
  ;; at load time by the linker.
  (let ((header
	  (scode/make-lambda (procedure-name procedure)
			     (map variable-name
				  (procedure-required-arguments procedure))
			     (map variable-name (procedure-optional procedure))
			     (let ((rest (procedure-rest procedure)))
			       (and rest (variable-name rest)))
			     (map variable-name (procedure-names procedure))
			     '()
			     false)))
    (let ((kernel
	   (lambda (scfg expression)
	     (values scfg
		     (rtl:make-typed-cons:pair
		      (rtl:make-machine-constant
		       (scode/procedure-type-code header))
		      (rtl:make-constant header)
		      expression)))))
      (set! *ic-procedure-headers*
	    (cons (cons header (procedure-label procedure))
		  *ic-procedure-headers*))
      (let ((context (procedure-closure-context procedure)))
	(if (reference? context)
	    (with-values (lambda () (generate/rvalue* context))
	      kernel)
	    ;; Is this right if the procedure is being closed
	    ;; inside another IC procedure?
	    (kernel (make-null-cfg)
		    (rtl:make-fetch register:environment)))))))

(define (make-trivial-closure-cons procedure)
  (enqueue-procedure! procedure)
  (rtl:make-typed-cons:procedure
   (rtl:make-entry:procedure (procedure-label procedure))))

(define (make-cons-closure-indirection procedure)
  (let* ((context (procedure-closure-context procedure))
	 (variable (cdr (procedure-closure-cons procedure))))
    (make-closure-redirection
     (find-variable/value/simple
      context variable
      "make-cons-closure-indirection: Unavailable indirection variable")
     (indirection-block-procedure
      (block-shared-block (procedure-closing-block procedure)))
     procedure)))

(define (make-cons-closure-redirection procedure)
  (let* ((context (procedure-closure-context procedure))
	 (block (stack-block/external-ancestor
		 (reference-context/block context))))
    (redirect-closure context
		      block
		      (block-procedure block)
		      procedure)))

(define (redirect-closure context block* procedure* procedure)
  (make-closure-redirection
   (rtl:make-fetch (block-ancestor-or-self->locative
		    context block* 0
		    (procedure-closure-offset procedure*)))
   procedure*
   procedure))

(define (make-closure-redirection expression procedure procedure*)
  (enqueue-procedure! procedure*)
  (let ((block (procedure-closing-block procedure))
	(block* (procedure-closing-block procedure*)))
    (let* ((block** (block-shared-block block)))
      (if (not (eq? (block-shared-block block*) block**))
	  (error "make-closure-redirection: non-shared redirection"
		 procedure procedure*))
      (let ((nentries (block-number-of-entries block**))
	    (entry (closure-block-entry-number block))
	    (entry* (closure-block-entry-number block*)))
	(let ((distance
	       (back-end:-
		(closure-entry-distance nentries entry entry*)
		(closure-environment-adjustment nentries entry))))
	  (if (back-end:= distance 0)
	      expression
	      (rtl:bump-closure expression distance)))))))

(define (make-non-trivial-closure-cons procedure block**)
  (let* ((block (procedure-closing-block procedure))
	 (block* (or block** block)))
    (cond ((not block)
	   (error "make-non-trivial-closure-cons: Consing trivial closure"
		  procedure))
	  ((not (eq? (block-shared-block block) block*))
	   (error "make-non-trivial-closure-cons: Non-canonical closure"
		  procedure))
	  ((= (block-entry-number block*) 1)
	   ;; Single entry point.  This could use the multiclosure case
	   ;; below, but this is simpler.
	   (with-values (lambda () (procedure-arity-encoding procedure))
	     (lambda (min max)
	       (rtl:make-typed-cons:procedure
		(rtl:make-cons-closure
		 (rtl:make-entry:procedure (procedure-label procedure))
		 min
		 max
		 (procedure-closure-size procedure))))))
	  ((= (block-entry-number block*) 0)
	   ;; No entry point (used for environment only)
	   (rtl:make-cons-pointer
	    (rtl:make-machine-constant (ucode-type vector))
	    (rtl:make-cons-multiclosure 0
					(procedure-closure-size procedure)
					'#())))
	  (else
	   ;; Multiple entry points
	   (let* ((procedures
		   (let ((children
			  ;; This depends on the order of entries established
			  ;; by graft-block! in fgopt/blktyp.scm .
			  (reverse
			   (map (lambda (block)
				  (block-procedure
				   (car (block-children block))))
				(list-transform-negative
				    (block-grafted-blocks block*)
				  (lambda (block)
				    (zero? (block-entry-number block))))))))
		     ;; Official entry point.
		     (cons procedure children)))
		  (entries
		   (map (lambda (proc)
			  (with-values
			      (lambda () (procedure-arity-encoding proc))
			    (lambda (min max)
			      (list (procedure-label proc) min max))))
			procedures)))
	     (if (not (= (length entries) (block-entry-number block*)))
		 (error "make-non-trivial-closure-cons: disappearing entries"
			procedure))
	     (rtl:make-typed-cons:procedure
	      (rtl:make-cons-multiclosure (block-entry-number block*)
					  (procedure-closure-size procedure)
					  (list->vector entries))))))))

(define (load-closure-environment procedure closure-locative context*)
  (let ((context (or context* (procedure-closure-context procedure))))
    (define (load-closure-parent block force?)
      (if (and (not force?)
	       (or (not block)
		   (not (ic-block/use-lookup? block))))
	  (make-null-cfg)
	  (rtl:make-assignment
	   (rtl:locative-offset closure-locative
				(closure-block-first-offset block))
	   (if (not (ic-block/use-lookup? block))
	       (rtl:make-constant false)
	       (begin
		 (if (not (reference-context? context))
		     (error "load-closure-environment: bad closure context"
			    procedure))
		 (if (ic-block? (reference-context/block context))
		     (rtl:make-fetch register:environment)
		     (closure-ic-locative context block)))))))

    (let ((block (procedure-closing-block procedure)))
      (cond ((not block)
	     (make-null-cfg))
	    ((ic-block? block)
	     (load-closure-parent block true))
	    ((closure-block? block)
	     (let loop
		 ((entries (block-closure-offsets block))
		  (code (load-closure-parent (block-parent block) false)))
	       (if (null? entries)
		   code
		   (loop
		    (cdr entries)
		    (scfg*scfg->scfg!
		     (rtl:make-assignment
		      (rtl:locative-offset closure-locative
					   (cdar entries))
		      (let* ((variable (caar entries))
			     (value (lvalue-known-value variable)))
			(cond
			 ;; Paranoia.
			 ((and value
			       (rvalue/procedure? value)
			       (procedure/trivial-or-virtual? value)
			       (error "known ignorable procedure"
				      value variable))
			  (make-trivial-closure-cons value))
			 ((and (eq? value
				    (reference-context/procedure context))
			       (bypass-closure-reference? value))
			  (rtl:make-fetch
			   (block-closure-locative context)))
			 (else
			  (find-closure-variable context variable)))))
		     code)))))
	    (else
	     (error "Unknown block type" block))))))

(define (bypass-closure-reference? procedure)
  ;; This checks whether the closure object at the top of the stack
  ;; is the same as the value of a variable bound to the closure.
  ;; It typically is, but is not on the 68k if the closure is not the
  ;; first entry of the shared closure because the closure-for-environment
  ;; is always the canonical entry point.
  (let* ((closure-block (procedure-closing-block procedure))
	 (shared-block (block-shared-block closure-block)))
    (back-end:= (closure-environment-adjustment
		 (block-number-of-entries shared-block)
		 (closure-block-entry-number closure-block))
		0)))