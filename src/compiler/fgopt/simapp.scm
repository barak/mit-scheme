#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Dataflow Analysis: Simulate Application

(declare (usual-integrations))

(define (simulate-application lvalues applications)
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
		       #|(warn "Possible inapplicable operator" operator)|#
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
	       (let ((value (constant-value operator))
		     (argument-count (-1+ number-supplied)))
		 (if (not
		      (cond ((eq? value compiled-error-procedure)
			     (positive? argument-count))
			    ((scode/procedure? value)
			     (procedure-arity-valid? value argument-count))
			    (else
			     #|
			     (if (not (unassigned-reference-trap? value))
				 (warn "Possible inapplicable operator" value))
			     |#
			     true)))
		     (warn "Procedure called with wrong number of arguments"
			   value
			   argument-count))))
	      #|
	      (else
	       (warn "Possible inapplicable operator" operator))
	      |#)))))

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
  (if (not (memq from (lvalue-initial-backward-links to)))
      (begin
	(set-lvalue-initial-backward-links!
	 to
	 (cons from (lvalue-initial-backward-links to)))
	(set-lvalue-initial-forward-links!
	 from
	 (cons to (lvalue-initial-forward-links from)))))
  (letrec ((connect
	    (lambda (to from)
	      (if (not (memq from (lvalue-backward-links to)))
		  (begin
		    (enqueue-nodes! (lvalue-applications to))
		    (set-lvalue-backward-links!
		     to
		     (cons from (lvalue-backward-links to)))
		    (set-lvalue-forward-links!
		     from
		     (cons to (lvalue-forward-links from)))
		    (set-lvalue-values-cache!
		     to
		     (eq-set-union (lvalue-values-cache from)
				   (lvalue-values-cache to)))
		    (for-each (lambda (from) (connect to from))
			      (lvalue-backward-links from))
		    (for-each (lambda (to) (connect to from))
			      (lvalue-forward-links to)))))))
    (connect to from)))