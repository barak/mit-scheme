#| -*-Scheme-*-

$Id$

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (usual-integrations))

;;;; CLOSURE ANALYZERS

;;; A closure analyzer is just a phase that requires a dataflow graph to perform
;;; its function.  Maybe we should rename it some day.

(define (make-dataflow-analyzer remember transformer)
  (lambda (original-program)
    (let* ((new-text     (copier/top-level original-program remember))
	   (graph        (dataflow/top-level new-text)))
      ;; dataflow/top-level may decline to generate a graph, in which case the
      ;; dataflow transformation is merely an identity.
      (if graph
	  (transformer original-program graph)
	  new-text))))

;;;; SPLIT-AND-DRIFT

;;; Goal: the output code has (CALL (LOOKUP ...) ...) only when the
;;; target is a known lambda expression.  Otherwise it will be
;;; (CALL ',%INTERNAL-APPLY <continuation> <operator> <operand>...)

;;; This phase splits closures that have at least one "known call site" (i.e.  a
;;; call site where the closure is the only possible destination), moving the
;;; body to a top-level LETREC and replacing all of the known calls with direct
;;; references to the moved code, avoiding the indirect jump via the closure
;;; object.

;;; There is a screw case that requires us to deal with %make-trivial-closure
;;; with a LOOKUP rather than a LAMBDA expression as the code body.  These
;;; closures form a simple special case, since the body doesn't need to be
;;; moved, but the calls must still be rewritten.

;;; If we start with:
;;;   (LETREC ((foo (lambda (x) (foo 3))))
;;;     (list foo foo))
;;; We'd like to generate something like:
;;;   (LETREC ((foo (make-trivial-closure ...))) (list foo foo))
;;; but that isn't legal in KMP-Scheme because the right hand side of
;;; a LETREC binding must be a LAMBDA expression.

;;; After conversion, the code above becomes:
;;;   (LETREC ((foo (lambda (cont-43 x)
;;;                   (CALL (LOOKUP foo) (LOOKUP cont-43) '3))))
;;;     (CALL ',%list cont-85
;;;           (CALL ',%make-trivial-closure '#F (LOOKUP foo))
;;;           (CALL ',%make-trivial-closure '#F (LOOKUP foo)))

;;; Note: the assumption here is that code generation guarantees that
;;; both calls to %make-trivial-closure generate the same (EQ?)
;;; object.

;;; We can't replace the LOOKUPs inside of %make-trivial-closure with
;;; LAMBDAs because the resulting procedures wouldn't be EQ? as
;;; required by the original source code.

(define split-and-drift
  (make-dataflow-analyzer
   (lambda (new old) (split/remember new old))
   (lambda (original-program graph)
     (let* ((code     (graph/program graph))
	    (closures (graph/closures graph))
	    (output-code `(LET () ,code))
	    ;; LET inserted so we can create a LETREC frame inside, if
	    ;; needed, in find-lambda-drift-frame
	    (lambda-drift-point (find-lambda-drift-frame output-code)))
       (let ((movable-closures
	      ;; Movable iff there is a call that is always and only to an
	      ;; instance of this closure.
	      (list-transform-positive closures
		(lambda (closure)
		  (and (not (value/closure/escapes? closure))
		       (there-exists? (value/closure/call-sites closure)
			 (lambda (call-site)
			   (operator-is-unique? call-site))))))))
	 (for-every movable-closures
	   (lambda (closure)
	     (split-closure-and-drift closure lambda-drift-point)))
	 (split/remember output-code original-program))))))

;;; Split and drift operations

(define drift-lambda!
  ;; Extends the LETREC-expr with a binding for new-name to lambda-expr
  (let* ((bindings (->pattern-variable 'BINDINGS))
	 (body (->pattern-variable 'BODY))
	 (pattern `(LETREC ,bindings ,body)))
    (lambda (LETREC-expr new-name lambda-expr)
      (cond ((form/match pattern LETREC-expr)
	     => (lambda (match-result)
		  (let ((bindings (cadr (assq bindings match-result)))
			(body (cadr (assq body match-result))))
		    (form/rewrite! LETREC-expr
		      `(LETREC ((,new-name ,lambda-expr) ,@bindings)
			 ,body)))))
	    (else
	     (internal-error "No LETREC in DRIFT-LAMBDA!" LETREC-expr))))))

(define (make-closure->lambda-expression make-closure-expression)
  ;; (Values lambda-expr format)
  (cond ((CALL/%make-heap-closure? make-closure-expression)
	 (values
	  (CALL/%make-heap-closure/lambda-expression make-closure-expression)
	  'HEAP))
	((CALL/%make-trivial-closure? make-closure-expression)
	 (values
	  (CALL/%make-trivial-closure/procedure make-closure-expression)
	  'TRIVIAL))
	((CALL/%make-stack-closure? make-closure-expression)
	 (values
	  (CALL/%make-stack-closure/lambda-expression make-closure-expression)
	  'STACK))
	(else (internal-error
	       "Unexpected expression in make-closure->lambda-expression"
	       make-closure-expression))))

;;; Split and drift operations, continued

(define (split-closure-and-drift closure lambda-drift-point)
  (let ((mutable-call-sites		; Call only this closure
	 (list-transform-positive (value/closure/call-sites closure)
	   (lambda (call-site)
	     (operator-is-unique? call-site)))))
    (call-with-values
	(lambda ()
	  (make-closure->lambda-expression (value/text closure)))
      (lambda (lambda-expr format)
	;; LAMBDA-EXPR is the body of the closure: either a LAMBDA or
	;;             LOOKUP expression;
	;; FORMAT is 'TRIVIAL, 'STACK, or 'HEAP
	(cond ((eq? format 'STACK) 'not-yet-implemented)
	      ((LOOKUP/? lambda-expr)	; See screw case above
	       (for-every mutable-call-sites
		 (lambda (site)
		   (let ((form (application/text site)))
		     ;; FORM is (CALL ',%internal-apply[-unchecked]
		     ;;               <continuation> <nargs>
		     ;;               <operator> <operand>...)
		     ;; The debugging information previously associated
		     ;; with the whole call must now be associated with
		     ;; _both_ the BEGIN and the inner CALL.
		     ;; The BEGIN is automatically associated with the
		     ;; debugging information since it is a form/rewrite!
		     ;; of the call.
		     ;; The inner call must be done explicitly.
		     (form/rewrite! form
		       `(BEGIN
			  ,(fifth form)	; In case of side-effects!
			  ,(split/remember*
			    `(CALL ,lambda-expr ,(third form)
				   ,@(list-tail form 5))
			    form)))))))
	      ((LAMBDA/? lambda-expr)
	       ;; Clean up the lambda bindings to remove optionals and lexprs in
	       ;; the lifted version.
	       (let* ((lambda-list (cadr lambda-expr))
		      (names (lambda-list->names lambda-list))
		      (body (third lambda-expr))
		      (lifted-lambda `(LAMBDA ,names ,body))
		      (new-name (split/new-name 'CLOSURE-GUTS)))
		 (drift-lambda!		; Drift to top-level LETREC
		  lambda-drift-point new-name lifted-lambda)
		 ;; The calls to split/remember* are for the same
		 ;; reason as above in the trivial case.
		 (form/rewrite! lambda-expr
		   ;; Rewrite body of closing code to call new top-level LAMBDA
		   (if *after-cps-conversion?*
		       `(LAMBDA ,lambda-list
			  ,(split/remember*
			    `(CALL (LOOKUP ,new-name)
				   ,@(map (lambda (name) `(LOOKUP ,name))
					  names))
			    body))
		       `(LAMBDA ,lambda-list
			  ,(split/remember*
			    `(CALL (LOOKUP ,new-name)
				   (QUOTE #F) 	; Continuation
				   ,@(map (lambda (name) `(LOOKUP ,name))
					  (cdr names)))
			    body))))
		 (for-every mutable-call-sites
		   (lambda (site)
		     ;; Rewrite calls that are known to be to heap or trivial
		     ;; closures, bypassing the closure and going
		     ;; direct to the top-level LAMBDA
		     (let ((form (application/text site)))
		       ;; FORM is
		       ;;   (CALL ',%internal-apply[-unchecked] <continuation>
		       ;;         <nargs> <operator> <operand>...)
		       (form/rewrite! form
			 (case format
			   ((TRIVIAL)
			    `(BEGIN
			       ,(fifth form) ; In case of side-effects!
			       ;; Same reason as above.
			       ,(split/remember*
				 `(CALL (LOOKUP ,new-name)
					,(third form)
					,@(lambda-list/applicate form
					   (cdr lambda-list)
					   (list-tail form 5)))
				 form)))
			   ((HEAP)
			    `(CALL (LOOKUP ,new-name)
				   ,(third form)
				   ,@(lambda-list/applicate form
				      (cdr lambda-list)
				      (list-tail form 4))))
			   (else (internal-error "Unknown format"
						 format)))))))))
	      (else (internal-error "Unknown handler" lambda-expr)))))))

;;; Support operations for split-and-drift

(define (find-lambda-drift-frame code)
  (define (loop previous code)

    (define (insert-LETREC!)
      (cond ((LET/? previous)
	     (let ((old-body (let/body previous)))
	       (if (LETREC/?  old-body)
		   old-body
		   (let ((new-body `(LETREC () ,old-body)))
		     (split/remember* new-body previous)
		     (form/rewrite! previous
		       `(LET ,(let/bindings previous) ,new-body))
		     new-body))))
	    ((CALL/? previous)
	     (let* ((lambda-expr (call/operator previous))
		    (old-body    (lambda/body lambda-expr)))
	       (if (LETREC/?  old-body)
		   old-body
		   (let ((new-body `(LETREC () ,old-body)))		     
		     (split/remember* new-body previous)
		     (form/rewrite! previous
		       `(CALL (LAMBDA ,(lambda/formals lambda-expr)
				,new-body)
			      ,(call/continuation previous)
			      ,@(call/operands previous)))
		     new-body))))
	    ((LETREC/? previous)
	     previous)
	    (else (internal-error "Unexpected binding form for inserting LETREC"
				  previous))))

    ;; Unwrap all static (and pseudo-static) bindings, and force the
    ;; next level to be a LETREC.  Return a pointer to the LETREC.
    (cond ((LET/? code)
	   (let ((bindings (let/bindings code))
		 (body     (LET/body code)))
	     (if (for-all? bindings
		   (lambda (binding)
		     (let ((value (cadr binding)))
		       (form/static? value))))
		 (loop code body)
		 (insert-LETREC!))))
	  ((and (CALL/? code)
		(LAMBDA/? (call/operator code))
		(equal? (call/continuation code) '(QUOTE #F)))
	   (if (for-all? (call/operands code) form/static?)
	       (loop code (lambda/body (call/operator code)))
	       (insert-letrec!)))
	  ((and (LETREC/? code)
		(null? (letrec/bindings code)))
	   (loop code (letrec/body code)))
	  (else (insert-LETREC!))))

  (if (not (and (LET/? code) (null? (let/bindings code))))
      (internal-error "Incorrect outer form for FIND-LAMBDA-DRIFT-FRAME"
		      code))
  (loop code (let/body code)))

;;; General utility routines

(define (split/new-name prefix)
  (new-variable prefix))

(define (operator-is-unique? call-site)
  ;; Call-site is an application structure, or a symbol denoting an
  ;; external known call site.
  (if (symbol? call-site)
      #F
      (node/unique-value (application/operator-node call-site))))

(define (split/remember new old)
  (code-rewrite/remember new old))

(define (split/remember* new copy)
  (code-rewrite/remember* new
			  (code-rewrite/original-form copy)))