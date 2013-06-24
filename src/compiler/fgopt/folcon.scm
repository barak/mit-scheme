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

;;;; Constant Folding
;; Package: (compiler fg-optimizer fold-constants)

(declare (usual-integrations))

(define (fold-constants lvalues applications)
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
    (with-new-lvalue-marks
     (lambda ()
       (for-each lvalue-mark! knowable-nodes)
       (transitive-closure false delete-if-known! knowable-nodes))))
  (list-transform-negative lvalues lvalue-known-value))

(define (delete-if-known! lvalue)
  (if (and (not (lvalue-known-value lvalue))
	   (for-all? (lvalue-source-links lvalue) lvalue-known-value))
      (let ((value (car (lvalue-values lvalue))))
	(for-each (lambda (lvalue*)
		    (if (lvalue-marked? lvalue*)
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

#|
(define (fold-combinations combinations)
  (if (null? combinations)
      (return-2 false '())
      (transmit-values (fold-combinations (cdr combinations))
	(lambda (any-folded? not-folded)
	  (if (fold-combination (car combinations))
	      (return-2 true not-folded)
	      (return-2 any-folded? (cons (car combinations) not-folded)))))))
|#

(define (fold-combinations combinations)
  ;; (return-2 any-folded? not-folded)
  (let ((left combinations)
	(any-folded? false)
	(not-folded '()))
    (let restart-loop ()
      (with-simple-restart 'CONTINUE
	"Skip this constant-folding operation"
	(lambda ()
	  (let fold-loop ()
	    (if (not (null? left))
		(begin
		  (if (fold-combination (car left))
		      (set! any-folded? true)
		      (set! not-folded (cons (car left) not-folded)))
		  (set! left (cdr left))
		  (fold-loop))))))
      (if (not (null? left))
	  (begin
	    ;; This means that folding the current combination caused an error,
	    ;; and the user decided to skip.
	    (set! not-folded (cons (car left) not-folded))
	    (set! left (cdr left))
	    (restart-loop))))
    (return-2 any-folded? (reverse! not-folded))))

(define (fold-combination combination)
  (let ((operator (combination/operator combination))
	(continuation (combination/continuation combination))
	(operands (combination/operands combination)))
    (and (constant-foldable-operator? operator)
	 ;; (rvalue-known? continuation)
	 ;; (uni-continuation? (rvalue-known-value continuation))
	 (for-all? operands rvalue-known-constant?)
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