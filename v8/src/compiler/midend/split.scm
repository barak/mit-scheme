;;; -*- Scheme -*-

(declare (usual-integrations))

;;;; CLOSURE ANALYZERS

;;; A closure analyzer is just a phase that requires a dataflow graph to perform
;;; its function.  Maybe we should rename it some day.

(define (make-dataflow-analyzer transformer)
  (lambda (KMP-Program)
    (let* ((new-text     (copier/top-level KMP-Program dataflow/remember))
	   (graph        (dataflow/top-level new-text)))
      (transformer new-text graph (graph/closures graph)))))

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
   (lambda (code graph closures)
     graph				; Not needed
     (let* ((output-code `(LET () ,code))
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
	 output-code)))))

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
		     ;; FORM is (CALL ',%internal-apply <continuation>
		     ;;               <nargs> <operator> <operand>...)
		     (form/rewrite! form
		       `(BEGIN
			  ,(fifth form)	; In case of side-effects!
			  (CALL ,lambda-expr ,(third form)
				,@(list-tail form 5))))))))

	      ((LAMBDA/? lambda-expr)
	       ;; Clean up the lambda bindings to remove optionals and lexprs in
	       ;; the lifted version.
	       (let* ((lambda-list (cadr lambda-expr))
		      (names (lambda-list->names lambda-list))
		      (lifted-lambda
		       `(LAMBDA ,names ,(third lambda-expr)))
		      (new-name (closan/new-name 'CLOSURE-GUTS)))
		 (drift-lambda!		; Drift to top-level LETREC
		  lambda-drift-point new-name lifted-lambda)
		 (form/rewrite! lambda-expr
		   ;; Rewrite body of closing code to call new top-level LAMBDA
		   (if *after-cps-conversion?*
		       `(LAMBDA ,lambda-list
			  (CALL (LOOKUP ,new-name)
				,@(map (lambda (name) `(LOOKUP ,name)) names)))
		       `(LAMBDA ,lambda-list
			  (CALL (LOOKUP ,new-name) (QUOTE #F) ; Continuation
				,@(map (lambda (name) `(LOOKUP ,name))
				    (cdr names))))))
		 (for-every mutable-call-sites
		   (lambda (site)
		     ;; Rewrite calls that are known to be to heap or trivial
		     ;; closures, bypassing the closure and going
		     ;; direct to the top-level LAMBDA
		     (let ((form (application/text site)))
		       ;; FORM is
		       ;;   (CALL ',%internal-apply <continuation>
		       ;;         <nargs> <operator> <operand>...)
		       (form/rewrite! form
			 (case format
			   ((TRIVIAL)
			    `(BEGIN
			       ,(fifth form) ; In case of side-effects!
			       (CALL (LOOKUP ,new-name)
				     ,(third form)
				     ,@(lambda-list/applicate
					(cdr lambda-list)
					(list-tail form 5)))))
			   ((HEAP)
			    `(CALL (LOOKUP ,new-name)
				   ,(third form)
				   ,@(lambda-list/applicate
				      (cdr lambda-list)
				      (list-tail form 4))))
			   (else (internal-error "Unknown format"
						 format)))))))))
	      (else (internal-error "Unknown handler" lambda-expr)))))))

;;; Support operations for split-and-drift

(define (find-lambda-drift-frame code)
  (define (loop previous code)
    (define (insert-LETREC!)
      (let ((old-body (let/body previous)))
	(if (LETREC/?  old-body)
	    old-body
	    (let ((result `(LETREC () ,old-body)))
	      (form/rewrite! previous `(LET ,(let/bindings previous) ,result))
	      result))))
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
	  (else (insert-LETREC!))))

  (if (not (and (LET/? code) (null? (let/bindings code))))
      (internal-error "Incorrect outer form for FIND-LAMBDA-DRIFT-FRAME"
		      code))
  (loop code (let/body code)))

;;; General utility routines

(define (closan/new-name prefix)
  (new-variable prefix))

(define (for-every things proc)
  (for-each proc things))

(define (operator-is-unique? call-site)
  ;; Call-site is an application structure, or a symbol denoting an
  ;; external known call site.
  (if (symbol? call-site)
      #F
      (node/unique-value (application/operator-node call-site))))
