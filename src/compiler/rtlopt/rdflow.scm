#| -*-Scheme-*-

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

;;;; RTL Dataflow Analysis
;;; package: (compiler rtl-optimizer rtl-dataflow-analysis)

(declare (usual-integrations))

(define (rtl-dataflow-analysis rgraphs)
  (for-each (lambda (rgraph)
	      (let ((rnodes (generate-dataflow-graph rgraph)))
		(set-rgraph-register-value-classes!
		 rgraph
		 (vector-map (lambda (rnode)
			       (and rnode
				    (rnode/value-class rnode)))
			     rnodes))
		(generate-known-values! rnodes)
		(set-rgraph-register-known-values!
		 rgraph
		 (vector-map (lambda (rnode)
			       (and rnode
				    (rnode/known-value rnode)))
			     rnodes))))
	    rgraphs))

(define (rnode/value-class rnode)
  (let ((union
	 (reduce value-class/nearest-common-ancestor
		 false
		 ;; Here we assume that no member of
		 ;; `rnode/values' is a register expression.
		 (map rtl:expression-value-class
		      (rnode/values rnode)))))
    ;; Really this test should look for non-leaf value
    ;; classes, except that the "immediate" class (which is
    ;; the only other non-leaf class) is generated by the
    ;; `machine-constant' expression.  The `machine-constant'
    ;; expression should be typed so that its class could be
    ;; more precisely determined.
    (if (and (pseudo-register? (rnode/register rnode))
	     (or (eq? union value-class=value)
		 (eq? union value-class=word)
		 (eq? union value-class=unboxed)))
	(error "mixed-class register" rnode union))
    union))

(define-structure (rnode
		   (conc-name rnode/)
		   (constructor make-rnode (register))
		   (print-procedure
		    (unparser/standard-method 'RNODE
		      (lambda (state rnode)
			(unparse-object state (rnode/register rnode))))))
  (register false read-only true)
  (forward-links '())
  (backward-links '())
  (initial-values '())
  (values '())
  (known-value false)
  (classified-values))

(define (generate-dataflow-graph rgraph)
  (let ((rnodes (make-vector (rgraph-n-registers rgraph) false)))
    (for-each (lambda (bblock)
		(bblock-walk-forward bblock
		  (lambda (rinst)
		    (walk-rtl rnodes (rinst-rtl rinst)))))
	      (rgraph-bblocks rgraph))
    (for-each-rnode rnodes
      (lambda (rnode)
	(set-rnode/values!
	 rnode
	 (rtx-set/union* (rnode/initial-values rnode)
			 (map rnode/initial-values
			      (rnode/backward-links rnode))))))
    rnodes))

(define (for-each-rnode rnodes procedure)
  (for-each-vector-element rnodes
    (lambda (rnode)
      (if rnode
	  (procedure rnode)))))

(define (walk-rtl rnodes rtl)
  (let ((get-rnode
	 (lambda (expression)
	   (let ((register (rtl:register-number expression)))
	     (or (vector-ref rnodes register)
		 (let ((rnode (make-rnode register)))
		   (vector-set! rnodes register rnode)
		   rnode))))))
    (if (rtl:assign? rtl)
	(let ((address (rtl:assign-address rtl))
	      (expression (rtl:assign-expression rtl)))
	  (if (rtl:pseudo-register-expression? address)
	      (let ((target (get-rnode address)))
		(if (rtl:pseudo-register-expression? expression)
		    (rnode/connect! target (get-rnode expression))
		    (add-rnode/initial-value! target expression))))))
    (let loop ((rtl rtl))
      (rtl:for-each-subexpression rtl
	(lambda (expression)
	  (if (rtl:volatile-expression? expression)
	      (if (or (rtl:post-increment? expression)
		      (rtl:pre-increment? expression))
		  (add-rnode/initial-value!
		   (get-rnode (rtl:address-register expression))
		   expression)
		  (error "Unknown volatile expression" expression))
	      (loop expression)))))))

(define (add-rnode/initial-value! target expression)
  (let ((values (rnode/initial-values target)))
    (if (not (there-exists? values
	       (lambda (value)
		 (rtl:expression=? expression value))))
	(set-rnode/initial-values! target
				   (cons expression values)))))

(define (rnode/connect! target source)
  (if (not (memq source (rnode/backward-links target)))
      (begin
	(set-rnode/backward-links! target
				   (cons source (rnode/backward-links target)))
	(set-rnode/forward-links! source
				  (cons target (rnode/forward-links source)))
	(for-each (lambda (source) (rnode/connect! target source))
		  (rnode/backward-links source))
	(for-each (lambda (target) (rnode/connect! target source))
		  (rnode/forward-links target)))))

(define (generate-known-values! rnodes)
  (for-each-rnode rnodes
    (lambda (rnode)
      (set-rnode/classified-values! rnode
				    (map expression->classified-value
					 (rnode/values rnode)))))
  (for-each-rnode rnodes
    (lambda (rnode)
      (let ((expression (initial-known-value (rnode/classified-values rnode))))
	(set-rnode/known-value! rnode expression)
	(if (not (memq expression '(UNDETERMINED #F)))
	    (set-rnode/classified-values! rnode '())))))
  (let loop ()
    (let ((new-constant? false))
      (for-each-rnode rnodes
	(lambda (rnode)
	  (if (eq? (rnode/known-value rnode) 'UNDETERMINED)
	      (let ((values
		     (values-substitution-step
		      rnodes
		      (rnode/classified-values rnode))))
		(if (there-exists? values
		      (lambda (value)
			(eq? (car value) 'SUBSTITUTABLE-REGISTERS)))
		    (set-rnode/classified-values! rnode values)
		    (let ((expression (values-unique-expression values)))
		      (if expression (set! new-constant? true))
		      (set-rnode/known-value! rnode expression)
		      (set-rnode/classified-values! rnode '())))))))
      (if new-constant? (loop))))
  (for-each-rnode rnodes
    (lambda (rnode)
      (if (eq? (rnode/known-value rnode) 'UNDETERMINED)
	  (begin
	    (set-rnode/known-value!
	     rnode
	     (values-unique-expression (rnode/classified-values rnode)))
	    (set-rnode/classified-values! rnode '()))))))

(define (expression->classified-value expression)
  (cons (cond ((rtl:constant-expression? expression)
	       'CONSTANT)
	      ((rtl:contains-no-substitutable-registers? expression)
	       'NO-SUBSTITUTABLE-REGISTERS)
	      (else
	       'SUBSTITUTABLE-REGISTERS))
	expression))

(define (initial-known-value values)
  (and (not (null? values))
       (not (there-exists? values
	      (lambda (value)
		(rtl:volatile-expression? (cdr value)))))
       (let loop ((value (car values)) (rest (cdr values)))
	 (cond ((eq? (car value) 'SUBSTITUTABLE-REGISTERS) 'UNDETERMINED)
	       ((null? rest) (values-unique-expression values))
	       (else (loop (car rest) (cdr rest)))))))

(define (values-unique-expression values)
  (let ((class (caar values))
	(expression (cdar values)))
    (and (for-all? (cdr values)
	   (lambda (value)
	     (and (eq? class (car value))
		  (rtl:expression=? expression (cdr value)))))
	 expression)))

(define (values-substitution-step rnodes values)
  (map (lambda (value)
	 (if (eq? (car value) 'SUBSTITUTABLE-REGISTERS)
	     (let ((substitution? false))
	       (let ((expression
		      (let loop ((expression (cdr value)))
			(if (rtl:register? expression)
			    (let ((value
				   (register-known-value rnodes expression)))
			      (if value
				  (begin (set! substitution? true) value)
				  expression))
			    (rtl:map-subexpressions expression loop)))))
		 (if substitution?
		     (expression->classified-value expression)
		     value)))
	     value))
       values))

(define (register-known-value rnodes expression)
  (let ((rnode (vector-ref rnodes (rtl:register-number expression))))
    (and rnode
	 (let ((value (rnode/known-value rnode)))
	   (and (not (eq? value 'UNDETERMINED))
		value)))))