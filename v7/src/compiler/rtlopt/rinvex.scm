#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rinvex.scm,v 1.3 1990/01/18 22:48:02 cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

;;;; RTL Invertible Expression Elimination

(declare (usual-integrations))

(define *initial-queue*)
(define *branch-queue*)
(define *register-values*)

(define (invertible-expression-elimination rgraphs)
  (with-new-node-marks (lambda () (for-each walk-rgraph rgraphs))))

(define (walk-rgraph rgraph)
  (fluid-let ((*current-rgraph* rgraph)
	      (*initial-queue* (make-queue))
	      (*branch-queue* '())
	      (*register-values*
	       (make-vector (rgraph-n-registers rgraph) false)))
    (for-each (lambda (edge)
		(enqueue!/unsafe *initial-queue* (edge-right-node edge)))
	      (rgraph-initial-edges rgraph))
    (continue-walk)))

(define (continue-walk)
  (cond ((not (null? *branch-queue*))
	 (let ((entry (car *branch-queue*)))
	   (set! *branch-queue* (cdr *branch-queue*))
	   (set! *register-values* (car entry))
	   (walk-bblock (cdr entry))))
	((not (queue-empty? *initial-queue*))
	 (vector-fill! *register-values* false)
	 (walk-bblock (dequeue!/unsafe *initial-queue*)))))

(define (walk-bblock bblock)
  (let loop ((rinst (bblock-instructions bblock)))
    (let ((rtl (rinst-rtl rinst)))
      ((lookup-method (rtl:expression-type rtl)) rtl))
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
		    (begin
		      (enqueue!/unsafe *initial-queue* consequent)
		      (walk-next alternative))
		    (begin
		      (if (node-previous>1? alternative)
			  (enqueue!/unsafe *initial-queue* alternative)
			  (set! *branch-queue*
				(cons (cons (vector-copy *register-values*)
					    alternative)
				      *branch-queue*)))
		      (walk-bblock consequent)))
		(walk-next consequent))
	    (if (walk-next? alternative)
		(walk-next alternative)
		(continue-walk))))))

(define-integrable (walk-next? bblock)
  (and bblock (not (node-marked? bblock))))

(define-integrable (walk-next bblock)
  (if (node-previous>1? bblock) (vector-fill! *register-values* false))
  (walk-bblock bblock))

(define-integrable (register-value register)
  (vector-ref *register-values* register))

(define-integrable (set-register-value! register value)
  (vector-set! *register-values* register value)
  unspecific)

(define (expression-update! get-expression set-expression! object)
  (set-expression!
   object
   (let loop ((expression (get-expression object)))
     (if (rtl:register? expression)
	 expression
	 (optimize-expression (rtl:map-subexpressions expression loop))))))

(define (optimize-expression expression)
  (let ((type (rtl:expression-type expression))
	(try-unary-fold
	 (lambda (types)
	   (let loop ((types types)
		      (expression (cadr expression)))
	     (if (null? types)
		 expression
		 (let ((subexpression
			(canonicalize-subexpression expression)))
		   (and (eq? (car types) (rtl:expression-type subexpression))
			(loop (cdr types)
			      (cadr subexpression)))))))))
    (let next-inversion ((unary-inversions unary-inversions))
      (if (null? unary-inversions)
	  expression
	  (let ((first-inversion (car unary-inversions)))
	    (or (and (eq? type (caar first-inversion))
		     (try-unary-fold (append (cdar first-inversion)
					     (cdr first-inversion))))
		(and (eq? type (cadr first-inversion))
		     (try-unary-fold (append (cddr first-inversion)
					     (car first-inversion))))
		(next-inversion (cdr unary-inversions))))))))

(define unary-inversions
  '(((OBJECT->FIXNUM) . (FIXNUM->OBJECT))
    ((OBJECT->UNSIGNED-FIXNUM) . (FIXNUM->OBJECT))
    ((ADDRESS->FIXNUM) . (FIXNUM->ADDRESS))
    ((@ADDRESS->FLOAT OBJECT->ADDRESS) . (FLOAT->OBJECT))))

(define (canonicalize-subexpression expression)
  (or (and (rtl:pseudo-register-expression? expression)
	   (register-value (rtl:register-number expression)))
      expression))

(define (define-method type method)
  (let ((entry (assq type methods)))
    (if entry
	(set-cdr! entry method)
	(set! methods (cons (cons type method) methods))))
  type)

(define (lookup-method type)
  (if (eq? type 'ASSIGN)
      walk/assign
      (let ((entry (assq type methods)))
	(if (not entry)
	    (error "Missing method" type))
	(cdr entry))))

(define methods
  '())

(define (walk/assign statement)
  (expression-update! rtl:assign-expression
		      rtl:set-assign-expression!
		      statement)
  (let ((address (rtl:assign-address statement)))
    (if (rtl:pseudo-register-expression? address)
	(set-register-value! (rtl:register-number address)
			     (rtl:assign-expression statement)))))

(define-method 'INVOCATION:SPECIAL-PRIMITIVE
  (lambda (statement)
    statement
    (for-each-pseudo-register
     (lambda (register)
       (set-register-value! register false)))))

(for-each (lambda (type)
	    (define-method type (lambda (statement) statement unspecific)))
	  '(CLOSURE-HEADER
	    CONTINUATION-ENTRY
	    CONTINUATION-HEADER
	    IC-PROCEDURE-HEADER
	    INVOCATION:APPLY
	    INVOCATION:COMPUTED-JUMP
	    INVOCATION:COMPUTED-LEXPR
	    INVOCATION:JUMP
	    INVOCATION:LEXPR
	    INVOCATION:PRIMITIVE
	    INVOCATION:UUO-LINK
	    OPEN-PROCEDURE-HEADER
	    OVERFLOW-TEST
	    POP-RETURN
	    PROCEDURE-HEADER))

(define (define-one-arg-method type get set)
  (define-method type
    (lambda (statement)
      (expression-update! get set statement))))

(define-one-arg-method 'FIXNUM-PRED-1-ARG
  rtl:fixnum-pred-1-arg-operand
  rtl:set-fixnum-pred-1-arg-operand!)

(define-one-arg-method 'FLONUM-PRED-1-ARG
  rtl:flonum-pred-1-arg-operand
  rtl:set-flonum-pred-1-arg-operand!)

(define-one-arg-method 'TYPE-TEST
  rtl:type-test-expression
  rtl:set-type-test-expression!)

(define-one-arg-method 'INVOCATION:CACHE-REFERENCE
  rtl:invocation:cache-reference-name
  rtl:set-invocation:cache-reference-name!)

(define-one-arg-method 'INVOCATION:LOOKUP
  rtl:invocation:lookup-environment
  rtl:set-invocation:lookup-environment!)

(define-one-arg-method 'INVOCATION-PREFIX:MOVE-FRAME-UP
  rtl:invocation-prefix:move-frame-up-locative
  rtl:set-invocation-prefix:move-frame-up-locative!)

(define-one-arg-method 'INTERPRETER-CALL:ACCESS
  rtl:interpreter-call:access-environment
  rtl:set-interpreter-call:access-environment!)

(define-one-arg-method 'INTERPRETER-CALL:CACHE-REFERENCE
  rtl:interpreter-call:cache-reference-name
  rtl:set-interpreter-call:cache-reference-name!)

(define-one-arg-method 'INTERPRETER-CALL:CACHE-UNASSIGNED?
  rtl:interpreter-call:cache-unassigned?-name
  rtl:set-interpreter-call:cache-unassigned?-name!)

(define-one-arg-method 'INTERPRETER-CALL:LOOKUP
  rtl:interpreter-call:lookup-environment
  rtl:set-interpreter-call:lookup-environment!)

(define-one-arg-method 'INTERPRETER-CALL:UNASSIGNED?
  rtl:interpreter-call:unassigned?-environment
  rtl:set-interpreter-call:unassigned?-environment!)

(define-one-arg-method 'INTERPRETER-CALL:UNBOUND?
  rtl:interpreter-call:unbound?-environment
  rtl:set-interpreter-call:unbound?-environment!)

(define (define-two-arg-method type get-1 set-1 get-2 set-2)
  (define-method type
    (lambda (statement)
      (expression-update! get-1 set-1 statement)
      (expression-update! get-2 set-2 statement))))

(define-two-arg-method 'EQ-TEST
  rtl:eq-test-expression-1
  rtl:set-eq-test-expression-1!
  rtl:eq-test-expression-2
  rtl:set-eq-test-expression-2!)

(define-two-arg-method 'FIXNUM-PRED-2-ARGS
  rtl:fixnum-pred-2-args-operand-1
  rtl:set-fixnum-pred-2-args-operand-1!
  rtl:fixnum-pred-2-args-operand-2
  rtl:set-fixnum-pred-2-args-operand-2!)

(define-two-arg-method 'FLONUM-PRED-2-ARGS
  rtl:flonum-pred-2-args-operand-1
  rtl:set-flonum-pred-2-args-operand-1!
  rtl:flonum-pred-2-args-operand-2
  rtl:set-flonum-pred-2-args-operand-2!)

(define-two-arg-method 'INVOCATION-PREFIX:DYNAMIC-LINK
  rtl:invocation-prefix:dynamic-link-locative
  rtl:set-invocation-prefix:dynamic-link-locative!
  rtl:invocation-prefix:dynamic-link-register
  rtl:set-invocation-prefix:dynamic-link-register!)

(define-two-arg-method 'INTERPRETER-CALL:CACHE-ASSIGNMENT
  rtl:interpreter-call:cache-assignment-name
  rtl:set-interpreter-call:cache-assignment-name!
  rtl:interpreter-call:cache-assignment-value
  rtl:set-interpreter-call:cache-assignment-value!)

(define-two-arg-method 'INTERPRETER-CALL:DEFINE
  rtl:interpreter-call:define-environment
  rtl:set-interpreter-call:define-environment!
  rtl:interpreter-call:define-value
  rtl:set-interpreter-call:define-value!)

(define-two-arg-method 'INTERPRETER-CALL:SET!
  rtl:interpreter-call:set!-environment
  rtl:set-interpreter-call:set!-environment!
  rtl:interpreter-call:set!-value
  rtl:set-interpreter-call:set!-value!)