#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcse1.scm,v 1.112 1987/08/04 06:56:11 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; RTL Common Subexpression Elimination
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define *initial-queue*)
(define *branch-queue*)

(define (common-subexpression-elimination rgraphs)
  (with-new-node-marks
   (lambda ()
     (for-each cse-rgraph rgraphs))))

(define (cse-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph)
	      (*next-quantity-number* 0)
	      (*initial-queue* (make-queue))
	      (*branch-queue* '()))
    (for-each (lambda (edge)
		(enqueue! *initial-queue* (edge-right-node edge)))
	      (rgraph-initial-edges rgraph))
    (state:initialize rgraph continue-walk)))

(define (continue-walk)
  (cond ((not (null? *branch-queue*))
	 (let ((entry (car *branch-queue*)))
	   (set! *branch-queue* (cdr *branch-queue*))
	   (state:set! *current-rgraph* (car entry))
	   (walk-rnode (cdr entry))))
	((not (queue-empty? *initial-queue*))
	 (state:reset! *current-rgraph*)
	 (walk-rnode (dequeue! *initial-queue*)))))

(define (walk-rnode rnode)
  (node-mark! rnode)
  ((vector-method rnode walk-rnode) rnode))

(define-vector-method rtl-snode-tag walk-rnode
  (lambda (rnode)
    (cse-statement (rnode-rtl rnode))
    (let ((next (snode-next rnode)))
      (if (walk-next? next)
	  (walk-next next)
	  (continue-walk)))))

(define-vector-method rtl-pnode-tag walk-rnode
  (lambda (rnode)
    (cse-statement (rnode-rtl rnode))
    (let ((consequent (pnode-consequent rnode))
	  (alternative (pnode-alternative rnode)))
      (if (walk-next? consequent)
	  (if (walk-next? alternative)
	      (if (node-previous>1? consequent)
		  (begin (enqueue! *initial-queue* consequent)
			 (walk-next alternative))
		  (begin (if (node-previous>1? alternative)
			     (enqueue! *initial-queue* alternative)
			     (set! *branch-queue*
				   (cons (cons (state:get *current-rgraph*)
					       alternative)
					 *branch-queue*)))
			 (walk-rnode consequent)))
	      (walk-next consequent))
	  (if (walk-next? alternative)
	      (walk-next alternative)
	      (continue-walk))))))

(define (walk-next? rnode)
  (and rnode (not (node-marked? rnode))))

(define (walk-next rnode)
  (if (node-previous>1? rnode) (state:reset! *current-rgraph*))
  (walk-rnode rnode))

(define (cse-statement statement)
  ((if (eq? (rtl:expression-type statement) 'ASSIGN)
       cse/assign
       (cdr (or (assq (rtl:expression-type statement) cse-methods)
		(error "Missing CSE method" (car statement)))))
   statement))

(define cse-methods '())

(define (define-cse-method type method)
  (let ((entry (assq type cse-methods)))
    (if entry
	(set-cdr! entry method)
	(set! cse-methods (cons (cons type method) cse-methods))))
  type)

(define (cse/assign statement)
  (expression-replace! rtl:assign-expression rtl:set-assign-expression!
		       statement
    (lambda (volatile? insert-source!)
      (let ((address (rtl:assign-address statement)))
	(if (rtl:register? address)
	    (begin 
	      (register-expression-invalidate! address)
	      (if (and (not volatile?)
		       (not (rtl:machine-register-expression?
			     (rtl:assign-expression statement)))
		       ;; This is a kludge.  If the address is the
		       ;; frame pointer, then the source is the stack
		       ;; pointer.  If this is not done then some of
		       ;; the references to stack locations use the
		       ;; stack pointer instead of the frame pointer.
		       ;; This is not a bug but I want the stack
		       ;; addressing to be uniform for now.  -- cph
		       (not (interpreter-frame-pointer? address)))
		  (insert-register-destination! address (insert-source!))))

	    (let ((address (expression-canonicalize address)))
	      (rtl:set-assign-address! statement address)
	      (full-expression-hash address
		(lambda (hash volatile?* in-memory?*)
		  (let ((memory-invalidate!
			 (cond ((and (memq (rtl:expression-type address)
					   '(PRE-INCREMENT POST-INCREMENT))
				     (or (interpreter-stack-pointer?
					  (rtl:address-register address))
					 (interpreter-free-pointer?
					  (rtl:address-register address))))
				(lambda ()
				  (register-expression-invalidate!
				   (rtl:address-register address))))
			       ((expression-address-varies? address)
				(lambda ()
				  (hash-table-delete-class!
				   element-in-memory?)))
			       (else
				(lambda ()
				  (hash-table-delete!
				   hash
				   (hash-table-lookup hash address))
				  (hash-table-delete-class!
				   element-address-varies?))))))
		    (cond (volatile?* (memory-invalidate!))
			  ((not volatile?)
			   (let ((address
				  (find-cheapest-expression address hash
							    false)))
			     (let ((element (insert-source!)))
			       (memory-invalidate!)
			       (insert-memory-destination!
				address
				element
				(modulo (+ (symbol-hash 'ASSIGN) hash)
					n-buckets)))))))))))))))

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

(define-trivial-one-arg-method 'TRUE-TEST
  rtl:true-test-expression rtl:set-true-test-expression!)

(define-trivial-one-arg-method 'TYPE-TEST
  rtl:type-test-expression rtl:set-type-test-expression!)

(define-trivial-one-arg-method 'UNASSIGNED-TEST
  rtl:type-test-expression rtl:set-unassigned-test-expression!)

(define (method/noop statement)
  'DONE)

(define-cse-method 'RETURN method/noop)
(define-cse-method 'PROCEDURE-HEAP-CHECK method/noop)
(define-cse-method 'CONTINUATION-HEAP-CHECK method/noop)
(define-cse-method 'INVOCATION:APPLY method/noop)
(define-cse-method 'INVOCATION:JUMP method/noop)
(define-cse-method 'INVOCATION:LEXPR method/noop)
(define-cse-method 'INVOCATION:PRIMITIVE method/noop)
(define-cse-method 'INVOCATION:UUO-LINK method/noop)

(define (method/invalidate-stack statement)
  (stack-pointer-invalidate!))

(define-cse-method 'SETUP-LEXPR method/invalidate-stack)
(define-cse-method 'MESSAGE-SENDER:VALUE method/invalidate-stack)
(define-cse-method 'MESSAGE-RECEIVER:CLOSURE method/invalidate-stack)
(define-cse-method 'MESSAGE-RECEIVER:STACK method/invalidate-stack)
(define-cse-method 'MESSAGE-RECEIVER:SUBPROBLEM method/invalidate-stack)

(define-cse-method 'INTERPRETER-CALL:ENCLOSE
  (lambda (statement)
    (stack-pointer-invalidate!)
    (expression-invalidate! (interpreter-register:enclose))))

(define-cse-method 'INVOCATION:CACHE-REFERENCE
  (lambda (statement)
    (expression-replace! rtl:invocation:cache-reference-name
			 rtl:set-invocation:cache-reference-name!
			 statement
			 trivial-action)))

(define-cse-method 'INVOCATION:LOOKUP
  (lambda (statement)
    (expression-replace! rtl:invocation:lookup-environment
			 rtl:set-invocation:lookup-environment!
			 statement
			 trivial-action)))

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
	  (hash-table-delete-class! element-address-varies?)
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