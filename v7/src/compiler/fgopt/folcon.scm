#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/folcon.scm,v 4.2 1987/12/30 06:44:31 cph Exp $

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

;;;; Constant Folding

(declare (usual-integrations))

(package (fold-constants)

(define-export (fold-constants lvalues applications)
  (let loop
      ((lvalues lvalues)
       (combinations
	(list-transform-positive applications application/combination?)))
    (let ((unknown-lvalues (eliminate-known-nodes lvalues)))
      (transmit-values (fold-combinations combinations)
	(lambda (any-folded? not-folded)
	  (if any-folded?
	      (loop unknown-lvalues not-folded)
	      not-folded))))))

(define (eliminate-known-nodes lvalues)
  (let ((knowable-nodes
	 (list-transform-positive lvalues
	   (lambda (lvalue)
	     (and (not (or (lvalue-passed-in? lvalue)
			   (and (variable? lvalue)
				(variable-assigned? lvalue)
				(not (memq 'CONSTANT
					   (variable-declarations lvalue))))))
		  (let ((values (lvalue-values lvalue)))
		    (and (not (null? values))
			 (null? (cdr values))
			 (or (rvalue/procedure? (car values))
			     (and (rvalue/constant? (car values))
				  (object-immutable?
				   (constant-value (car values))))))))))))
    (for-each (lambda (lvalue) (lvalue-mark-set! lvalue 'KNOWABLE))
	      knowable-nodes)
    (transitive-closure false delete-if-known! knowable-nodes)
    (for-each (lambda (lvalue) (lvalue-mark-clear! lvalue 'KNOWABLE))
	      knowable-nodes))
  (list-transform-negative lvalues lvalue-known-value))

(define (delete-if-known! lvalue)
  (if (and (not (lvalue-known-value lvalue))
	   (for-all? (lvalue-backward-links lvalue) lvalue-known-value))
      (let ((value (car (lvalue-values lvalue))))
	(for-each (lambda (lvalue*)
		    (if (lvalue-mark-set? lvalue* 'KNOWABLE)
			(enqueue-node! lvalue*)))
		  (lvalue-forward-links lvalue))
	(set-lvalue-known-value! lvalue value))))

(define (fold-combinations combinations)
  (if (null? combinations)
      (return-2 false '())
      (transmit-values (fold-combinations (cdr combinations))
	(lambda (any-folded? not-folded)
	  (if (fold-combination (car combinations))
	      (return-2 true not-folded)
	      (return-2 any-folded? (cons (car combinations) not-folded)))))))

(define (fold-combination combination)
  (let ((operator (combination/operator combination))
	(continuation (combination/continuation combination))
	(operands (combination/operands combination)))
    (and (rvalue-known-constant? operator)
	 (let ((operator (rvalue-constant-value operator)))
	   (and (operator-constant-foldable? operator)
		(primitive-arity-correct? operator (length operands))))
	 ;; (rvalue-known? continuation)
	 ;; (uni-continuation? (rvalue-known-value continuation))
	 (for-all? operands rvalue-known-constant?)
	 (begin
	   (let ((constant
		  (make-constant
		   (apply (rvalue-constant-value operator)
			  (map rvalue-constant-value operands)))))
	     (combination/constant! combination constant)
	     (map (lambda (value)
		    (if (uni-continuation? value)
			(lvalue-connect!:rvalue
			 (uni-continuation/parameter value)
			 constant)))
		  (rvalue-values continuation)))
	   true))))

)