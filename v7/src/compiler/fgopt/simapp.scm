#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/simapp.scm,v 4.3 1988/06/14 08:35:26 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Dataflow Analysis: Simulate Application

(declare (usual-integrations))

(package (simulate-application)

(define-export (simulate-application lvalues applications)
  (for-each initialize-lvalue-cache! lvalues)
  (for-each (lambda (application)
	      (set-application-operators! application '()))
	    applications)
  (transitive-closure false process-application applications)
  (for-each reset-lvalue-cache! lvalues))

(define (process-application application)
  (set-application-operators!
   application
   (let ((operator (application-operator application)))
     ((method-table-lookup process-application-methods
			   (tagged-vector/index operator))
      (application-operators application)
      operator
      (operator-applicator application)))))

(define process-application-methods
  (make-method-table rvalue-types
		     (lambda (old operator apply-operator)
		       old apply-operator
		       (warn "Unapplicable operator" operator)
		       operator)))

(let ((processor
       (lambda (old operator apply-operator)
	 (if (not (null? old))
	     (error "Encountered constant-operator application twice"
		    operator))
	 (apply-operator operator)
	 operator)))
  (define-method-table-entry 'PROCEDURE process-application-methods processor)
  (define-method-table-entry 'CONSTANT process-application-methods processor))

(define-method-table-entry 'REFERENCE process-application-methods
  (lambda (old operator apply-operator)
    (let ((new (lvalue-values-cache (reference-lvalue operator))))
      (let loop ((operators new))
	;; We can use `eq?' here because we assume that
	;; (eq? (list-tail (eq-set-union x y) n) y) for some n.
	;; This is also noted at the definition of `eq-set-union'.
	(if (eq? operators old)
	    new
	    (begin (apply-operator (car operators))
		   (loop (cdr operators))))))))

(define (operator-applicator application)
  (let ((operands (application-operands application)))
    (let ((number-supplied (length operands)))
      (lambda (operator)
	(cond ((rvalue/procedure? operator)
	       (set-procedure-applications!
		operator
		(cons application (procedure-applications operator)))
	       (if (not (procedure-arity-correct? operator number-supplied))
		   (warn "Wrong number of arguments" operator operands))
	       ;; We should have some kind of LIST rvalue type to handle
	       ;; the case of rest parameters, but for now we just
	       ;; define them to be passed-in.  This is handled
	       ;; specially in that part of the analysis.
	       (let loop
		   ((parameters
		     (append (procedure-required operator)
			     (procedure-optional operator)))
		    (operands operands))
		 (if (not (null? parameters))
		     (if (null? operands)
			 (for-each lvalue-unassigned! parameters)
			 (begin
			   (lvalue-connect! (car parameters) (car operands))
			   (loop (cdr parameters) (cdr operands)))))))
	      ((rvalue/constant? operator)
	       (let ((value (constant-value operator)))
		 (cond ((primitive-procedure? value)
			(if (not
			     (primitive-arity-correct? value
						       (-1+ number-supplied)))
			    (warn
			     "Primitive called with wrong number of arguments"
			     value
			     number-supplied)))
		       ((not (unassigned-reference-trap? value))
			(warn "Inapplicable operator" value)))))
	      (else
	       (warn "Inapplicable operator" operator)))))))

(define (initialize-lvalue-cache! lvalue)
  (set-lvalue-values-cache! lvalue (lvalue-values lvalue)))

(define (lvalue-values lvalue)
  ;; This is slow but works even with cycles in the DFG.
  (let ((lvalues '()))
    (let loop ((lvalue lvalue))
      (if (not (memq lvalue lvalues))
	  (begin (set! lvalues (cons lvalue lvalues))
		 (for-each loop (lvalue-backward-links lvalue)))))
    (eq-set-union* (lvalue-initial-values (car lvalues))
		   (map lvalue-initial-values (cdr lvalues)))))

(define (lvalue-unassigned! lvalue)
  (lvalue-connect! lvalue (make-constant (make-unassigned-reference-trap))))

(define-integrable (lvalue-connect! lvalue rvalue)
  (if (rvalue/reference? rvalue)
      (lvalue-connect!:lvalue lvalue (reference-lvalue rvalue))
      (lvalue-connect!:rvalue lvalue rvalue)))

(define (lvalue-connect!:rvalue lvalue rvalue)
  (if (not (memq rvalue (lvalue-initial-values lvalue)))
      (begin
	(set-lvalue-initial-values! lvalue
				    (cons rvalue
					  (lvalue-initial-values lvalue)))
	(if (not (memq rvalue (lvalue-values-cache lvalue)))
	    (begin
	      (update-lvalue-cache! lvalue rvalue)
	      (for-each (lambda (lvalue)
			  (if (not (memq rvalue (lvalue-values-cache lvalue)))
			      (update-lvalue-cache! lvalue rvalue)))
			(lvalue-forward-links lvalue)))))))

(define (update-lvalue-cache! lvalue rvalue)
  (enqueue-nodes! (lvalue-applications lvalue))
  (set-lvalue-values-cache! lvalue
			    (cons rvalue
				  (lvalue-values-cache lvalue))))

(define (lvalue-connect!:lvalue to from)
  (if (not (memq from (lvalue-backward-links to)))
      (begin
	(enqueue-nodes! (lvalue-applications to))
	(set-lvalue-backward-links! to (cons from (lvalue-backward-links to)))
	(set-lvalue-forward-links! from (cons to (lvalue-forward-links from)))
	(set-lvalue-values-cache! to
				  (eq-set-union (lvalue-values-cache from)
						(lvalue-values-cache to)))
	(for-each (lambda (from)
		    (lvalue-connect!:lvalue to from))
		  (lvalue-backward-links from))
	(for-each (lambda (to)
		    (lvalue-connect!:lvalue to from))
		  (lvalue-forward-links to)))))

)