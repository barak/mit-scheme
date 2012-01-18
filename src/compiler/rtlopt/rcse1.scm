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

;;;; RTL Common Subexpression Elimination: Codewalker
;;;  Based on the GNU C Compiler
;;; package: (compiler rtl-cse)

(declare (usual-integrations))

(define *initial-queue*)
(define *branch-queue*)

(define (common-subexpression-elimination rgraphs)
  (with-new-node-marks (lambda () (for-each cse-rgraph rgraphs))))

(define (cse-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph)
	      (*next-quantity-number* 0)
	      (*initial-queue* (make-queue))
	      (*branch-queue* '()))
    (for-each (lambda (edge)
		(enqueue!/unsafe *initial-queue* (edge-right-node edge)))
	      (rgraph-initial-edges rgraph))
    (fluid-let ((*register-tables*
		 (register-tables/make (rgraph-n-registers rgraph)))
		(*hash-table*)
		(*stack-offset*)
		(*stack-reference-quantities*))
      (continue-walk))))

(define (continue-walk)
  (cond ((not (null? *branch-queue*))
	 (let ((entry (car *branch-queue*)))
	   (set! *branch-queue* (cdr *branch-queue*))
	   (let ((state (car entry)))
	     (set! *register-tables* (state/register-tables state))
	     (set! *hash-table* (state/hash-table state))
	     (set! *stack-offset* (state/stack-offset state))
	     (set! *stack-reference-quantities*
		   (state/stack-reference-quantities state)))
	   (walk-bblock (cdr entry))))
	((not (queue-empty? *initial-queue*))
	 (state/reset!)
	 (walk-bblock (dequeue!/unsafe *initial-queue*)))))

(define-structure (state (type vector) (conc-name state/))
  (register-tables false read-only true)
  (hash-table false read-only true)
  (stack-offset false read-only true)
  (stack-reference-quantities false read-only true))

(define (state/reset!)
  (register-tables/reset! *register-tables*)
  (set! *hash-table* (make-hash-table))
  (set! *stack-offset* 0)
  (set! *stack-reference-quantities* '())
  unspecific)

(define (state/get)
  (make-state (register-tables/copy *register-tables*)
	      (hash-table-copy *hash-table*)
	      *stack-offset*
	      (map (lambda (entry)
		     (cons (car entry) (quantity-copy (cdr entry))))
		   *stack-reference-quantities*)))

(define (walk-bblock bblock)
  (let loop ((rinst (bblock-instructions bblock)))
    (let ((rtl (rinst-rtl rinst)))
      ((if (eq? (rtl:expression-type rtl) 'ASSIGN)
	   cse/assign
	   (let ((method (hash-table/get cse-methods (rtl:expression-type rtl) #f)))
	     (if (not method)
		 (error "Missing CSE method" (rtl:expression-type rtl)))
	     method))
       rtl))
    (if (rinst-next rinst)
	(loop (rinst-next rinst))))
  (node-mark! bblock)
  (if (sblock? bblock)
      (let ((next (snode-next bblock)))
	(if (walk-next? next)
	    (walk-next next)
	    (continue-walk)))
      (let ((consequent (pnode-consequent bblock))
	    (alternative (pnode-alternative bblock)))
	(if (walk-next? consequent)
	    (if (walk-next? alternative)
		(if (node-previous>1? consequent)
		    (begin (enqueue!/unsafe *initial-queue* consequent)
			   (walk-next alternative))
		    (begin (if (node-previous>1? alternative)
			       (enqueue!/unsafe *initial-queue* alternative)
			       (set! *branch-queue*
				     (cons (cons (state/get) alternative)
					   *branch-queue*)))
			   (walk-bblock consequent)))
		(walk-next consequent))
	    (if (walk-next? alternative)
		(walk-next alternative)
		(continue-walk))))))

(define-integrable (walk-next? bblock)
  (and bblock (not (node-marked? bblock))))

(define-integrable (walk-next bblock)
  (if (node-previous>1? bblock) (state/reset!))
  (walk-bblock bblock))

(define (define-cse-method type method)
  (hash-table/put! cse-methods type method)
  type)

(define cse-methods (make-strong-eq-hash-table))


(define (cse/assign statement)
  (expression-replace! rtl:assign-expression rtl:set-assign-expression!
		       statement
    (lambda (volatile? insert-source!)
      ((let ((address (rtl:assign-address statement)))
	 (if volatile? (notice-pop! (rtl:assign-expression statement)))
	 (cond ((rtl:register? address) cse/assign/register)
	       ((stack-reference? address) cse/assign/stack-reference)
	       ((and (rtl:pre-increment? address)
		     (interpreter-stack-pointer?
		      (rtl:address-register address)))
		cse/assign/stack-push)
	       ((interpreter-register-reference? address)
		cse/assign/interpreter-register)
	       (else
		(let ((address (expression-canonicalize address)))
		  (rtl:set-assign-address! statement address)
		  cse/assign/general))))
       (rtl:assign-address statement)
       (rtl:assign-expression statement)
       volatile?
       insert-source!))))

(define (cse/assign/register address expression volatile? insert-source!)
  (if (interpreter-stack-pointer? address)
      (if (and (rtl:offset? expression)
	       (interpreter-stack-pointer? (rtl:offset-base expression))
	       (rtl:machine-constant? (rtl:offset-offset expression)))
	  (stack-pointer-adjust!
	   (rtl:machine-constant-value (rtl:offset-offset expression)))
	  (begin
	    (stack-invalidate!)
	    (stack-pointer-invalidate!)))
      (register-expression-invalidate! address))
  (if (and (not volatile?)
	   (pseudo-register? (rtl:register-number address)))
      (insert-register-destination! address (insert-source!))))

(define (cse/assign/stack-reference address expression volatile?
				    insert-source!)
  expression
  (stack-reference-invalidate! address)
  (if (not volatile?)
      (insert-stack-destination! address (insert-source!))))

(define (cse/assign/stack-push address expression volatile? insert-source!)
  expression
  (let ((adjust!
	 (lambda ()
	   (stack-pointer-adjust! (rtl:address-number address)))))
    (if (not volatile?)
	(let ((element (insert-source!)))
	  (adjust!)
	  (insert-stack-destination!
	   (rtl:make-offset (interpreter-stack-pointer)
			    (rtl:make-machine-constant 0))
	   element))
	(adjust!))))

(define (cse/assign/interpreter-register address expression volatile?
					 insert-source!)
  expression
  (let ((hash (expression-hash address)))
    (let ((memory-invalidate!
	   (lambda ()
	     (hash-table-delete! hash (hash-table-lookup hash address)))))
      (if volatile?
	  (memory-invalidate!)
	  (assignment-memory-insertion address
				       hash
				       insert-source!
				       memory-invalidate!)))))

(define (cse/assign/general address expression volatile? insert-source!)
  expression
  (full-expression-hash address
    (lambda (hash volatile?* in-memory?)
      in-memory?
      (let ((memory-invalidate!
	     (cond ((stack-pop? address)
		    (lambda () unspecific))
		   ((and (memq (rtl:expression-type address)
			       '(PRE-INCREMENT POST-INCREMENT))
			 (interpreter-free-pointer?
			  (rtl:address-register address)))
		    (lambda ()
		      (register-expression-invalidate!
		       (rtl:address-register address))))
		   ((expression-address-varies? address)
		    (lambda ()
		      (hash-table-delete-class! element-in-memory?)))
		   (else
		    (lambda ()
		      (hash-table-delete! hash
					  (hash-table-lookup hash address))
		      (varying-address-invalidate!))))))
	(if (or volatile? volatile?*)
	    (memory-invalidate!)
	    (assignment-memory-insertion address
					 hash
					 insert-source!
					 memory-invalidate!)))))
  (notice-pop! address))

(define (notice-pop! expression)
  ;; **** Kludge.  Works only because stack-pointer
  ;; gets used in very fixed way by code generator.
  (if (stack-pop? expression)
      (stack-pointer-adjust! (rtl:address-number expression))))

(define (assignment-memory-insertion address hash insert-source!
				     memory-invalidate!)
  #|
  ;; This does not cause bugs (false hash number passed to
  ;; insert-memory-destination! fixed one), but does not do anything
  ;; useful.  The idea of doing optimization on the address of a
  ;; memory assignment does not work since the RTL does not
  ;; distinguish addresses from references.  When the RTL is changed,
  ;; we can do CSE on the memory address.
  (let ((address (find-cheapest-expression address hash false)))
    (let ((element (insert-source!)))
      (memory-invalidate!)
      (insert-memory-destination! address element false)))
  |#
  hash
  (insert-source!)
  (memory-invalidate!)
  (mention-registers! address))

(define (trivial-action volatile? insert-source!)
  (if (not volatile?)
      (insert-source!)))

(define (define-trivial-one-arg-method type get set)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get set statement trivial-action))))

(define (define-trivial-two-arg-method type get-1 set-1 get-2 set-2)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-1 set-1 statement trivial-action)
      (expression-replace! get-2 set-2 statement trivial-action))))

(define-trivial-two-arg-method 'EQ-TEST
  rtl:eq-test-expression-1 rtl:set-eq-test-expression-1!
  rtl:eq-test-expression-2 rtl:set-eq-test-expression-2!)

(define-trivial-one-arg-method 'PRED-1-ARG
  rtl:pred-1-arg-operand rtl:set-pred-1-arg-operand!)

(define-trivial-two-arg-method 'PRED-2-ARGS
  rtl:pred-2-args-operand-1 rtl:set-pred-2-args-operand-1!
  rtl:pred-2-args-operand-2 rtl:set-pred-2-args-operand-2!)

(define-trivial-one-arg-method 'FIXNUM-PRED-1-ARG
  rtl:fixnum-pred-1-arg-operand rtl:set-fixnum-pred-1-arg-operand!)

(define-trivial-two-arg-method 'FIXNUM-PRED-2-ARGS
  rtl:fixnum-pred-2-args-operand-1 rtl:set-fixnum-pred-2-args-operand-1!
  rtl:fixnum-pred-2-args-operand-2 rtl:set-fixnum-pred-2-args-operand-2!)

(define-trivial-one-arg-method 'FLONUM-PRED-1-ARG
  rtl:flonum-pred-1-arg-operand rtl:set-flonum-pred-1-arg-operand!)

(define-trivial-two-arg-method 'FLONUM-PRED-2-ARGS
  rtl:flonum-pred-2-args-operand-1 rtl:set-flonum-pred-2-args-operand-1!
  rtl:flonum-pred-2-args-operand-2 rtl:set-flonum-pred-2-args-operand-2!)

(define-trivial-one-arg-method 'TYPE-TEST
  rtl:type-test-expression rtl:set-type-test-expression!)

(define (method/noop statement)
  statement
  unspecific)

(define-cse-method 'OVERFLOW-TEST method/noop)
(define-cse-method 'POP-RETURN method/noop)
(define-cse-method 'CONTINUATION-ENTRY method/noop)
(define-cse-method 'CONTINUATION-HEADER method/noop)
(define-cse-method 'IC-PROCEDURE-HEADER method/noop)
(define-cse-method 'OPEN-PROCEDURE-HEADER method/noop)
(define-cse-method 'PROCEDURE-HEADER method/noop)
(define-cse-method 'CLOSURE-HEADER method/noop)
(define-cse-method 'INVOCATION:JUMP method/noop)
(define-cse-method 'INVOCATION:LEXPR method/noop)

(define (method/unknown-invocation statement)
  (for-each-pseudo-register
   (lambda (register)
     (let ((expression (register-expression register)))
       (if expression
	   (register-expression-invalidate! expression)))))
  (stack-pointer-adjust!
   (stack->memory-offset (rtl:invocation-pushed statement)))
  (expression-invalidate! (interpreter-value-register))
  (expression-invalidate! (interpreter-free-pointer)))

(define-cse-method 'INVOCATION:APPLY method/unknown-invocation)
(define-cse-method 'INVOCATION:COMPUTED-JUMP method/unknown-invocation)
(define-cse-method 'INVOCATION:COMPUTED-LEXPR method/unknown-invocation)
(define-cse-method 'INVOCATION:UUO-LINK method/unknown-invocation)
(define-cse-method 'INVOCATION:GLOBAL-LINK method/unknown-invocation)
(define-cse-method 'INVOCATION:PRIMITIVE method/unknown-invocation)
(define-cse-method 'INVOCATION:SPECIAL-PRIMITIVE method/unknown-invocation)

(define-cse-method 'INVOCATION:CACHE-REFERENCE
  (lambda (statement)
    (expression-replace! rtl:invocation:cache-reference-name
			 rtl:set-invocation:cache-reference-name!
			 statement
			 trivial-action)
    (method/unknown-invocation statement)))

(define-cse-method 'INVOCATION:LOOKUP
  (lambda (statement)
    (expression-replace! rtl:invocation:lookup-environment
			 rtl:set-invocation:lookup-environment!
			 statement
			 trivial-action)
    (method/unknown-invocation statement)))

(define-cse-method 'INVOCATION-PREFIX:MOVE-FRAME-UP
  (lambda (statement)
    (expression-replace! rtl:invocation-prefix:move-frame-up-locative
			 rtl:set-invocation-prefix:move-frame-up-locative!
			 statement
			 trivial-action)
    (stack-invalidate!)
    (stack-pointer-invalidate!)))

(define-cse-method 'INVOCATION-PREFIX:DYNAMIC-LINK
  (lambda (statement)
    (expression-replace! rtl:invocation-prefix:dynamic-link-locative
			 rtl:set-invocation-prefix:dynamic-link-locative!
			 statement
			 trivial-action)
    (expression-replace! rtl:invocation-prefix:dynamic-link-register
			 rtl:set-invocation-prefix:dynamic-link-register!
			 statement
			 trivial-action)
    (stack-invalidate!)
    (stack-pointer-invalidate!)))

(define (define-lookup-method type get-environment set-environment! register)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-environment set-environment! statement
	(lambda (volatile? insert-source!)
	  (expression-invalidate! (register))
	  (non-object-invalidate!)
	  (if (not volatile?) (insert-source!)))))))

(define-lookup-method 'INTERPRETER-CALL:ACCESS
  rtl:interpreter-call:access-environment
  rtl:set-interpreter-call:access-environment!
  interpreter-register:access)

(define-lookup-method 'INTERPRETER-CALL:CACHE-REFERENCE
  rtl:interpreter-call:cache-reference-name
  rtl:set-interpreter-call:cache-reference-name!
  interpreter-register:cache-reference)

(define-lookup-method 'INTERPRETER-CALL:CACHE-UNASSIGNED?
  rtl:interpreter-call:cache-unassigned?-name
  rtl:set-interpreter-call:cache-unassigned?-name!
  interpreter-register:cache-unassigned?)

(define-lookup-method 'INTERPRETER-CALL:LOOKUP
  rtl:interpreter-call:lookup-environment
  rtl:set-interpreter-call:lookup-environment!
  interpreter-register:lookup)

(define-lookup-method 'INTERPRETER-CALL:UNASSIGNED?
  rtl:interpreter-call:unassigned?-environment
  rtl:set-interpreter-call:unassigned?-environment!
  interpreter-register:unassigned?)

(define-lookup-method 'INTERPRETER-CALL:UNBOUND?
  rtl:interpreter-call:unbound?-environment
  rtl:set-interpreter-call:unbound?-environment!
  interpreter-register:unbound?)

(define (define-assignment-method type
	  get-environment set-environment!
	  get-value set-value!)
  (define-cse-method type
    (lambda (statement)
      (expression-replace! get-value set-value! statement trivial-action)
      (expression-replace! get-environment set-environment! statement
	(lambda (volatile? insert-source!)
	  (varying-address-invalidate!)
	  (non-object-invalidate!)
	  (if (not volatile?) (insert-source!)))))))

(define-assignment-method 'INTERPRETER-CALL:CACHE-ASSIGNMENT
  rtl:interpreter-call:cache-assignment-name
  rtl:set-interpreter-call:cache-assignment-name!
  rtl:interpreter-call:cache-assignment-value
  rtl:set-interpreter-call:cache-assignment-value!)

(define-assignment-method 'INTERPRETER-CALL:DEFINE
  rtl:interpreter-call:define-environment
  rtl:set-interpreter-call:define-environment!
  rtl:interpreter-call:define-value
  rtl:set-interpreter-call:define-value!)

(define-assignment-method 'INTERPRETER-CALL:SET!
  rtl:interpreter-call:set!-environment
  rtl:set-interpreter-call:set!-environment!
  rtl:interpreter-call:set!-value
  rtl:set-interpreter-call:set!-value!)