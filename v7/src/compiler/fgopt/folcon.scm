#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/folcon.scm,v 4.5 1988/12/06 18:56:59 jinx Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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
  #|
  ;; This is needed only if we use the version of eliminate-known-nodes
  ;; commented out below.
  ;; 
  ;; Initialize
  ;; a. Remove circularities
  (for-each (lambda (lvalue)
	      (set-lvalue-source-links!
	       lvalue
	       (list-transform-negative
		   (lvalue-backward-links lvalue)
		 (lambda (lvalue*)
		   (memq lvalue (lvalue-backward-links lvalue*))))))
	    lvalues)
  ;; b. Remove nop nodes
  (transitive-closure false delete-if-nop! lvalues)
  |#
  ;; Do the actual work
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

#|
(define (delete-if-nop! lvalue)
  (if (and (not (lvalue-passed-in? lvalue))
	   (null? (lvalue-values lvalue))
	   (null? (lvalue-source-links lvalue)))
      (for-each
       (lambda (lvalue*)
	 (set-lvalue-source-links!
	  lvalue*
	  (delq! lvalue (lvalue-source-links lvalue*)))
	 (enqueue-node! lvalue*))
       (lvalue-forward-links lvalue))))
|#

#|
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
			     (rvalue/constant? (car values))))))))))
    (for-each (lambda (lvalue) (lvalue-mark-set! lvalue 'KNOWABLE))
	      knowable-nodes)
    (transitive-closure false delete-if-known! knowable-nodes)
    (for-each (lambda (lvalue) (lvalue-mark-clear! lvalue 'KNOWABLE))
	      knowable-nodes))
  (list-transform-negative lvalues lvalue-known-value))

(define (delete-if-known! lvalue)
  (if (and (not (lvalue-known-value lvalue))
	   (for-all? (lvalue-source-links lvalue) lvalue-known-value))
      (let ((value (car (lvalue-values lvalue))))
	(for-each (lambda (lvalue*)
		    (if (lvalue-mark-set? lvalue* 'KNOWABLE)
			(enqueue-node! lvalue*)))
		  (lvalue-forward-links lvalue))
	(set-lvalue-known-value! lvalue value))))
|#

(define (eliminate-known-nodes lvalues)
  (list-transform-negative lvalues
      (lambda (lvalue)
	(and (not (or (lvalue-passed-in? lvalue)
		      (and (variable? lvalue)
			   (variable-assigned? lvalue)
			   (not (memq 'CONSTANT
				      (variable-declarations lvalue))))))
		 
	     (let ((values (lvalue-values lvalue)))
	       (and (not (null? values))
		    (null? (cdr values))
		    (let ((value (car values)))
		      (and (or (rvalue/procedure? value)
			       (rvalue/constant? value))
			   (begin
			     (set-lvalue-known-value! lvalue value)
			     true)))))))))

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
    (and (constant-foldable-operator? operator)
	 ;; (rvalue-known? continuation)
	 ;; (uni-continuation? (rvalue-known-value continuation))
	 (for-all? operands
		   (lambda (val)
		     (and (rvalue-known-constant? val)
			  (object-immutable? (rvalue-constant-value val)))))
	 (let ((op (constant-foldable-operator-value operator)))
	   (and (or (arity-correct? op (length operands))
		    (begin
		      (error "fold-combination: Wrong number of arguments"
			     op (length operands))
		      false))
		(let ((constant
		       (make-constant
			(apply op (map rvalue-constant-value operands)))))
		  (combination/constant! combination constant)
		  (for-each (lambda (value)
			      (if (uni-continuation? value)
				  (maybe-fold-lvalue!
				   (uni-continuation/parameter value)
				   constant)))
			    (rvalue-values continuation))
		  true))))))

(define (maybe-fold-lvalue! lvalue constant)
  (lvalue-connect!:rvalue lvalue constant)
  (reset-lvalue-cache! lvalue)
  (let ((val (lvalue-passed-in? lvalue)))
    (if (or (false? val) (eq? val 'INHERITED)) 		; (not (number? val))
	(error "maybe-fold-lvalue!: Folding a non source!" lvalue)
	(let ((new (-1+ val)))
	  (cond ((not (zero? new))
		 (set-lvalue-passed-in?! lvalue new))
		((recompute-lvalue-passed-in! lvalue)
		 (for-each (lambda (lvalue)
			     ;; We don't recompute-lvalue-passed-in!
			     ;; recursively because the forward-link
			     ;; relationship is transitively closed.
			     (if (eq? (lvalue-passed-in? lvalue) 'INHERITED)
				 (recompute-lvalue-passed-in! lvalue)))
			   (lvalue-forward-links lvalue))))))))

;; This returns true if the lvalue went from passed-in to not
;; passed-in.  It initializes the value to false because it may
;; be in its own backward-link list.

(define (recompute-lvalue-passed-in! lvalue)
  (set-lvalue-passed-in?! lvalue false)
  (if (there-exists? (lvalue-backward-links lvalue) lvalue-passed-in?)
      (begin
	(set-lvalue-passed-in?! lvalue 'INHERITED)
	;; The assignment would return the right value, but this is clearer.
	false)
      true))

(define (constant-foldable-operator? rv)
  (or (and (rvalue-known-constant? rv)
	   (let ((val (rvalue-constant-value rv)))
	     (and (primitive-procedure? val)
		  (constant-foldable-primitive? val))))
      (and (rvalue/reference? rv)
	   ;; (not (reference-known-value rv))
	   (not (reference-to-known-location? rv))
	   (let ((var (reference-lvalue rv)))
	     (and (memq 'USUAL-DEFINITION (variable-declarations var))
		  (constant-foldable-variable? (variable-name var)))))))

(define (constant-foldable-operator-value rv)
  (if (rvalue/reference? rv)
      (variable-usual-definition (variable-name (reference-lvalue rv)))
      (rvalue-constant-value rv)))  

(define (arity-correct? proc n)
  (let ((arity (procedure-arity proc)))
    (and (>= n (car arity))
	 (or (null? (cdr arity))
	     (<= n (cdr arity))))))

)