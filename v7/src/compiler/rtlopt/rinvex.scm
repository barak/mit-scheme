#| -*-Scheme-*-

$Id: rinvex.scm,v 1.11 2002/11/20 19:45:57 cph Exp $

Copyright (c) 1989-1999, 2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; RTL Invertible Expression Elimination
;;; package: (compiler rtl-optimizer invertible-expression-elimination)

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
  ;; Note: The following code may cause pseudo-register copies to be
  ;; generated since it would have to propagate some of the
  ;; simplifications, and then delete the now unused registers.  This
  ;; is not worthwhile since the previous register is likely to be
  ;; dead at this point, so the lap-level register allocator will
  ;; reuse the alias achieving the effect of the deletion.  Ultimately
  ;; the expression invertibility code should be integrated into the
  ;; CSE and this register deletion would happen there.
  (set-expression!
   object
   (let loop ((expression (get-expression object)))
     (if (rtl:register? expression)
	 expression
	 (optimize-expression (rtl:map-subexpressions expression loop))))))

(define (optimize-expression expression)
  (let loop
      ((identities
	(list-transform-positive identities
	  (let ((type (rtl:expression-type expression)))
	    (lambda (identity)
	      (eq? type (car (cadr identity))))))))
    (cond ((null? identities)
	   expression)
	  ((let ((identity (car identities)))
	     (let ((in-domain? (car identity))
		   (matching-operation (cadr identity)))
	       (let loop
		   ((operations (cddr identity))
		    (subexpression ((cadr matching-operation) expression)))
		 (if (null? operations)
		     (and (valid-subexpression? subexpression)
			  (in-domain?
			   (rtl:expression-value-class subexpression))
			  subexpression)
		     (let ((subexpression
			    (canonicalize-subexpression subexpression)))
		       (and (eq? (caar operations)
				 (rtl:expression-type subexpression))
			    (loop (cdr operations)
				  ((cadar operations) subexpression))))))))
	   => optimize-expression)
	  (else
	   (loop (cdr identities))))))

(define identities
  ;; Each entry is composed of a value class and a sequence of
  ;; operations whose composition is the identity for that value
  ;; class.  Each operation is described by the operator and the
  ;; selector for the relevant operand.
  `((,value-class=value? (OBJECT->FIXNUM ,rtl:object->fixnum-expression)
			 (FIXNUM->OBJECT ,rtl:fixnum->object-expression))
    (,value-class=value? (FIXNUM->OBJECT ,rtl:fixnum->object-expression)
			 (OBJECT->FIXNUM ,rtl:object->fixnum-expression))
    (,value-class=value? (OBJECT->UNSIGNED-FIXNUM
			  ,rtl:object->unsigned-fixnum-expression)
			 (FIXNUM->OBJECT ,rtl:fixnum->object-expression))
    (,value-class=value? (FIXNUM->OBJECT ,rtl:fixnum->object-expression)
			 (OBJECT->UNSIGNED-FIXNUM
			  ,rtl:object->unsigned-fixnum-expression))
    (,value-class=value? (FIXNUM->ADDRESS ,rtl:fixnum->address-expression)
			 (ADDRESS->FIXNUM ,rtl:address->fixnum-expression))
    (,value-class=value? (ADDRESS->FIXNUM ,rtl:address->fixnum-expression)
			 (FIXNUM->ADDRESS ,rtl:fixnum->address-expression))
    (,value-class=value? (OBJECT->FLOAT ,rtl:object->float-expression)
			 (FLOAT->OBJECT ,rtl:float->object-expression))
    (,value-class=value? (FLOAT->OBJECT ,rtl:float->object-expression)
			 (OBJECT->FLOAT ,rtl:object->float-expression))
    (,value-class=address? (OBJECT->ADDRESS ,rtl:object->address-expression)
			   (CONS-POINTER ,rtl:cons-pointer-datum))
    ;; The following are not value-class=datum? and value-class=type?
    ;; because they are slightly more general.
    (,value-class=immediate? (OBJECT->DATUM ,rtl:object->datum-expression)
			     (CONS-NON-POINTER ,rtl:cons-non-pointer-datum))
    (,value-class=immediate? (OBJECT->TYPE ,rtl:object->type-expression)
			     (CONS-POINTER ,rtl:cons-pointer-type))
    (,value-class=immediate? (OBJECT->TYPE ,rtl:object->type-expression)
			     (CONS-NON-POINTER ,rtl:cons-non-pointer-type))))

(define (valid-subexpression? expression)
  ;; Machine registers not allowed because they are volatile.
  ;; Ideally at this point we could introduce a copy to the
  ;; value of the machine register required, but it is too late
  ;; to do this.  Perhaps always copying machine registers out
  ;; before using them would make this win.
  (or (not (rtl:register? expression))
      (rtl:pseudo-register-expression? expression)))

(define (canonicalize-subexpression expression)
  (or (and (rtl:pseudo-register-expression? expression)
	   (register-value (rtl:register-number expression)))
      expression))

(define (define-general-method type method)
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

(define-general-method 'INVOCATION:SPECIAL-PRIMITIVE
  (lambda (statement)
    statement
    (for-each-pseudo-register
     (lambda (register)
       (set-register-value! register false)))))

(for-each (lambda (type)
	    (define-general-method type (lambda (statement) statement unspecific)))
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
	    INVOCATION:GLOBAL-LINK
	    OPEN-PROCEDURE-HEADER
	    OVERFLOW-TEST
	    POP-RETURN
	    PROCEDURE-HEADER))

(define (define-one-arg-method type get set)
  (define-general-method type
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

(define-one-arg-method 'PRED-1-ARG
  rtl:pred-1-arg-operand
  rtl:set-pred-1-arg-operand!)

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
  (define-general-method type
    (lambda (statement)
      (expression-update! get-1 set-1 statement)
      (expression-update! get-2 set-2 statement))))

(define-two-arg-method 'EQ-TEST
  rtl:eq-test-expression-1
  rtl:set-eq-test-expression-1!
  rtl:eq-test-expression-2
  rtl:set-eq-test-expression-2!)

(define-two-arg-method 'PRED-2-ARGS
  rtl:pred-2-args-operand-1
  rtl:set-pred-2-args-operand-1!
  rtl:pred-2-args-operand-2
  rtl:set-pred-2-args-operand-2!)

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