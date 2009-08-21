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

;;;; Side effect analysis
;;; package: (compiler fg-optimizer)

(declare (usual-integrations))

;;;; Computing the call graphs

(package (compute-call-graph! clear-call-graph!)

(define-export (compute-call-graph! procedures)
  ;; This is only needed because the fields in the
  ;; procedure objects are reused.
  (clear-call-graph! procedures)
  (for-each find&memoize-callees! procedures))

(define (find&memoize-callees! procedure)
  (let loop ((apps (block-applications (procedure-block procedure)))
	     (constants '())
	     (procedures '()))
    (cond ((null? apps)
	   (memoize-callees! procedure constants procedures))
	  ((not (application/combination? (car apps)))
	   (loop (cdr apps) constants procedures))
	  (else
	   (let* ((operator (application-operator (car apps)))
		  (nconsts
		   (eq-set-union
		    (list-transform-positive
			(rvalue-values operator)
		      rvalue/constant?)
		    constants)))
	     (loop (cdr apps)
		   (if (or (not (rvalue-passed-in? operator))
			   ;; This is only possible if it was
			   ;; declared CONSTANT.
			   (rvalue-known-value operator))
		       nconsts
		       ;; It is a passed in reference.
		       (eq-set-adjoin
			(reference-lvalue operator)
			nconsts))
		   (eq-set-union
		    (list-transform-positive
			(rvalue-values operator)
		      #|
		      ;; This is unnecessary as long as we treat continuations
		      ;; specially and treat cwcc as an unknown procedure.
		      (lambda (val)
			(and (rvalue/procedure? val)
			     (not (procedure-continuation? val))))
		      |#
		      rvalue/procedure?)
		    procedures)))))))

(define-export (clear-call-graph! procedures)
  (for-each (lambda (procedure)
	      (set-procedure-initial-callees! procedure '())
	      (set-procedure-callees! procedure '())
	      (set-procedure-callers! procedure '()))
	    procedures))

(define (memoize-callees! procedure constants callees)
  (set-procedure-initial-callees! procedure (cons constants callees))
  (for-each (lambda (callee)
	      (add-caller&callee! procedure callee))
	    callees))

;; This transitively completes the call graph.  Two procedures are
;; related by a caller/callee relationship if there is a path by which
;; the caller calls the callee.

(define (add-caller&callee! caller callee)
  (let ((callees (procedure-callees caller)))
    (if (not (memq callee callees))
	(begin
	  (set-procedure-callees! caller
				  (cons callee callees))
	  (set-procedure-callers! callee
				  (cons caller
					(procedure-callers callee)))
	  (for-each
	   (lambda (callee^2)
	     (add-caller&callee! caller callee^2))
	   (procedure-callees callee))
	  (for-each
	   (lambda (caller^2)
	     (add-caller&callee! caller^2 callee))
	   (procedure-callers caller))))
    'DONE))

) ;; package

(package (side-effect-analysis)

;; IMPORTANT: This assumes that the call graph has been computed.

(define-export (side-effect-analysis procs&conts applications)
  (let ((procedures
	 (list-transform-negative procs&conts procedure-continuation?)))
    (if (not compiler:analyze-side-effects?)
	(for-each (lambda (proc)
		    (set-procedure-side-effects!
		     proc
		     (list '(ARBITRARY BYPASSED))))
		  procedures)
	(begin
	  (for-each setup-side-effects! procedures)
	  (for-each compute-side-effects! procedures)
	  (transitive-closure
	   false
	   (lambda (item)
	     (if (application? item)
		 (analyze-combination! item)
		 (analyze-procedure! item)))
	   (append procedures
		   (list-transform-positive
			applications
		      application/combination?)))))))

(define (setup-side-effects! procedure)
  (let ((assigned-vars
	 (let ((block (procedure-block procedure)))
	   (list-transform-positive
	       (block-free-variables block)
	     (lambda (variable)
	       (there-exists?
		(variable-assignments variable)
		(lambda (assignment)
		  (eq? (reference-context/block
			(assignment-context assignment))
		       block)))))))
	(arbitrary-callees
	 (list-transform-negative
	     (car (procedure-initial-callees procedure))
	   (lambda (object)
	     (if (lvalue/variable? object)
		 (variable/side-effect-free? object)
		 (constant/side-effect-free? object))))))
    (set-procedure-side-effects!
     procedure
     `(,@(if (null? assigned-vars)
	     '()
	     (list `(ASSIGNMENT ,@assigned-vars)))
       ,@(if (null? arbitrary-callees)
	     '()
	     (list `(ARBITRARY ,@arbitrary-callees)))))))

(define (variable/side-effect-free? variable)
  (let ((decls (variable-declarations variable)))
    (or (memq 'SIDE-EFFECT-FREE decls)
	(memq 'PURE-FUNCTION decls)
	(and (memq 'USUAL-DEFINITION decls)
	     (side-effect-free-variable?
	      (variable-name variable))))))

(define (constant/side-effect-free? constant)
  (and (rvalue/constant? constant)			; Paranoia
       (let ((val (constant-value constant)))
	 (and (not (eq? val compiled-error-procedure))	; Hmm.
	      (if (primitive-procedure? val)
		  (side-effect-free-primitive? val)
		  (not (procedure-object? val)))))))

(define (process-derived-assignments! procedure variables effects)
  (let* ((block (procedure-block procedure))
	 (modified-variables
	  (list-transform-negative
	      variables
	    (lambda (var)
	      ;; The theoretical closing limit of this variable would be give
	      ;; a more precise bound, but we don't have that information.
	      (and (not (variable-closed-over? var))
		   (block-ancestor-or-self? (variable-block var) block))))))
    (if (null? modified-variables)
	effects
	(let ((place (assq 'DERIVED-ASSIGNMENT effects)))
	  (if (false? place)
	      (cons (cons 'DERIVED-ASSIGNMENT modified-variables)
		    effects)
	      (begin (set-cdr! place
			       (append! modified-variables (cdr place)))
		     effects))))))

;;;; Procedure side effects

(define (compute-side-effects! procedure)
  ;; There is no point in computing further if this procedure has
  ;; arbitrary side effects.
  (let ((my-effects (procedure-side-effects procedure)))
    (if (not (assq 'ARBITRARY my-effects))
	(begin
	  (for-each
	   (lambda (callee)
	     (if (not (eq? callee procedure))
		 (let dispatch-loop ((effects (procedure-side-effects callee)))
		   (if (null? effects)
		       'DONE
		       (begin
			 (case (caar effects)
			   ((ARBITRARY DERIVED-ARBITRARY RANDOM)
			    (let ((place (assq 'DERIVED-ARBITRARY my-effects)))
			      (if (false? place)
				  (set! my-effects
					(cons `(DERIVED-ARBITRARY ,callee)
					      my-effects)))))
			   ((ASSIGNMENT DERIVED-ASSIGNMENT)
			    (set! my-effects
				  (process-derived-assignments!
				   procedure
				   (cdar effects)
				   my-effects)))
			   (else
			    (error
			     "compute-side-effects!: Unknown side-effect class"
			     (caar effects))
			    (let ((place (assq 'RANDOM my-effects)))
			      (if (false? place)
				  (set! my-effects
					(cons '(RANDOM) my-effects))))))
			 (dispatch-loop (cdr effects)))))))
	   (procedure-callees procedure))
	  (set-procedure-side-effects! procedure my-effects)))
    'DONE))

;;; Determine whether the procedure computes a simple value.

(define (analyze-procedure! procedure)
  (if (and (not (procedure-continuation? procedure)) ;; paranoia
	   (null? (procedure-side-effects procedure))
	   (not (procedure/simplified? procedure)))
      (let ((pcont (procedure-continuation-lvalue procedure)))
	(and (not (lvalue-passed-out? pcont))
	     (let ((r/lvalue (continuation-variable/returned-value pcont)))
	       (and r/lvalue
		    (value/available? r/lvalue (procedure-block procedure))
		    (begin
		      (simplify-procedure! procedure r/lvalue)
		      (and (value/independent? r/lvalue
					       (procedure-block procedure))
			   (procedure-always-known-operator? procedure)
			   (begin (procedure/trivial! procedure 'BETA)
				  (enqueue-nodes!
				   (procedure-applications procedure)))))))))))

(define (continuation-variable/returned-value lvalue)
  (define (test-return return)
    (if (not (application/return? return))
	(begin
	  (error "continuation variable invoked in non-return application"
		 return)
	  false)
	(let ((value (return/operand return)))
	  (or (and (or (rvalue/constant? value)
		       (rvalue/procedure? value))
		   value)
	      #|
	      ;; This is not sufficient.
	      (and (rvalue/reference? value)
		   (reference-lvalue value))
	      |#
	      ))))

  (define (compare r/lvalue returns lvalues)
    (cond ((not (null? returns))
	   (and (eq? r/lvalue (test-return (car returns)))
		(compare r/lvalue (cdr returns) lvalues)))
	  ((not (null? lvalues))
	   (compare r/lvalue
		    (lvalue-applications (car lvalues))
		    (cdr lvalues)))
	  (else
	   r/lvalue)))

  (let find ((returns '())
	     (lvalues (eq-set-adjoin lvalue (lvalue-forward-links lvalue))))
    (if (not (null? returns))
	(let ((result (test-return (car returns))))
	  (and result (compare result (cdr returns) lvalues)))
	(and (not (null? lvalues))
	     (find (lvalue-applications (car lvalues))
		   (cdr lvalues))))))

;;; Determine whether the call should be punted

(define (analyze-combination! app)
  (define (simplify-combination! value)
    (combination/constant! app
			   (r/lvalue->rvalue (combination/context app) value))
    (let ((procedure (block-procedure (application-block app))))
      (if (rvalue/procedure? procedure)
	  (enqueue-node! procedure))))

  (define (check value op-vals)
    (if (and value
	     (for-all? op-vals
		       (lambda (proc)
			 (and (rvalue/procedure? proc)
			      (eq? value
				   (procedure/simplified-value
				    proc
				    (application-block app)))))))
	(simplify-combination! value)))

  (define (check-operators operator)
    (let ((vals (rvalue-values operator)))
      (and (not (null? vals))
	   (let ((proc (car vals)))
	     (and (rvalue/procedure? proc)
		  (check (procedure/simplified-value proc
						     (application-block app))
			 (cdr vals)))))))

  (and (application/combination? app)
       (let ((operator (application-operator app))
	     (cont (combination/continuation app)))
	 (and (not (rvalue-passed-in? operator))
	      (for-all? (rvalue-values operator)
			(lambda (proc)
			  (and (rvalue/procedure? proc)
			       (null? (procedure-side-effects proc)))))
	      (cond ((rvalue/procedure? cont)
		     (if (eq? (continuation/type cont)
			      continuation-type/effect)
			 (simplify-combination! (make-constant false))
			 (let ((val (lvalue-known-value
				     (continuation/parameter cont))))
			   (if val
			       (and (value/available? val
						      (application-block app))
				    (simplify-combination! val))
			       (check-operators operator)))))
		    ((and (rvalue/reference? cont)
			  (eq? (continuation-variable/type
				(reference-lvalue cont))
			       continuation-type/effect))
		     (simplify-combination! (make-constant false)))
		    (else
		     (check-operators operator)))))))

(define (value/test-generator block-test)
  (lambda (r/lvalue block)
    (if (lvalue/variable? r/lvalue)
	(block-test block (variable-block r/lvalue))
	(or (rvalue/constant? r/lvalue)
	    (and (rvalue/procedure? r/lvalue)
		 (if (procedure/closure? r/lvalue)
		     (or (procedure/trivial-closure? r/lvalue)
			 #|
			 ;; We need to change the rtl generator to avoid
			 ;; closing the procedure within itself
			 (block-ancestor-or-self?
			  block
			  (procedure-block r/lvalue))
			 |#
			 )
		     (block-test block
				 (procedure-closing-block r/lvalue))))))))

(define value/independent?
  (value/test-generator
   (lambda (block definition-block)
     (declare (integrate block definition-block))
     (not (block-ancestor-or-self? definition-block block)))))

(define value/available?
  (value/test-generator 
   (lambda (block definition-block)
     (declare (integrate block definition-block))
     (block-ancestor-or-self? block definition-block))))

(define-integrable (r/lvalue->rvalue context r/lvalue)
  (if (lvalue/variable? r/lvalue)
      (make-reference context r/lvalue false)
      r/lvalue))

(define (procedure/trivial! procedure kind)
  (let ((kinds (procedure-get procedure 'TRIVIAL)))
    (cond ((or (not kinds) (null? kinds))
	   (procedure-put! procedure 'TRIVIAL (list kind)))
	  ((not (memq kind kinds))
	   (procedure-put! procedure 'TRIVIAL (cons kind kinds))))))

(define (simplify-procedure! procedure r/lvalue)
  ;; **** Kludge! `make-application' requires that a block be given,
  ;; rather than a context, because this is how "fggen" builds things.
  ;; So we must pass the block and then clobber it after.
  (if (procedure-get procedure 'SIMPLIFIED)
      (error "procedure/trivial!: Already simplified" procedure))
  (procedure-put! procedure 'SIMPLIFIED r/lvalue)
  (let ((block (procedure-block procedure)))
    (let ((context (make-reference-context block)))
      (let ((application
	     (cfg-entry-node
	      (make-return block
			   (make-reference
			    context
			    (procedure-continuation-lvalue procedure)
			    true)
			   (r/lvalue->rvalue context r/lvalue)))))
	(set-application-context! application context)
	(set-procedure-entry-node! procedure application)))))

(define (procedure/simplified-value procedure block)
  (let ((node (procedure-entry-node procedure)))
    (and (application? node)
	 (application/return? node)
	 (let ((value
		(let ((operand (return/operand node)))
		  (if (rvalue/reference? operand)
		      (reference-lvalue operand)
		      (rvalue-known-value operand)))))
	   (and value
		(value/available? value block)
		value)))))

) ;; package