#| -*-Scheme-*-

$Id: uenvir.scm,v 14.58 2002/11/20 19:46:23 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; Microcode Environments
;;; package: (runtime environment)

(declare (usual-integrations))

(define (environment? object)
  (or (system-global-environment? object)
      (ic-environment? object)
      (stack-ccenv? object)
      (closure-ccenv? object)))

(define (guarantee-environment object name)
  (if (not (environment? object))
      (illegal-environment object name)))

(define (illegal-environment object name)
  (error:wrong-type-argument object "environment" name))

(define (environment-has-parent? environment)
  (cond ((system-global-environment? environment)
	 #f)
	((ic-environment? environment)
	 (ic-environment/has-parent? environment))
	((stack-ccenv? environment)
	 (stack-ccenv/has-parent? environment))
	((closure-ccenv? environment)
	 (closure-ccenv/has-parent? environment))
	(else
	 (illegal-environment environment 'ENVIRONMENT-HAS-PARENT?))))

(define (environment-parent environment)
  (cond ((system-global-environment? environment)
	 (error:bad-range-argument environment 'ENVIRONMENT-PARENT))
	((ic-environment? environment)
	 (ic-environment/parent environment))
	((stack-ccenv? environment)
	 (stack-ccenv/parent environment))
	((closure-ccenv? environment)
	 (closure-ccenv/parent environment))
	(else
	 (illegal-environment environment 'ENVIRONMENT-PARENT))))

(define (environment-bound-names environment)
  (cond ((system-global-environment? environment)
	 (system-global-environment/bound-names))
	((ic-environment? environment)
	 (ic-environment/bound-names environment))
	((stack-ccenv? environment)
	 (stack-ccenv/bound-names environment))
	((closure-ccenv? environment)
	 (closure-ccenv/bound-names environment))
	(else
	 (illegal-environment environment 'ENVIRONMENT-BOUND-NAMES))))

(define (environment-macro-names environment)
  (cond ((system-global-environment? environment)
	 (system-global-environment/macro-names))
	((ic-environment? environment)
	 (ic-environment/macro-names environment))
	((or (stack-ccenv? environment)
	     (closure-ccenv? environment))
	 '())
	(else
	 (illegal-environment environment 'ENVIRONMENT-MACRO-NAMES))))

(define (environment-bindings environment)
  (let ((items (environment-bound-names environment)))
    (do ((items items (cdr items)))
	((not (pair? items)))
      (let ((name (car items)))
	(set-car! items
		  (cons name
			(let ((value
			       (environment-safe-lookup environment name)))
			  (if (unassigned-reference-trap? value)
			      '()
			      (list value)))))))
    items))

(define (environment-arguments environment)
  (cond ((ic-environment? environment)
	 (ic-environment/arguments environment))
	((stack-ccenv? environment)
	 (stack-ccenv/arguments environment))
	((or (system-global-environment? environment)
	     (closure-ccenv? environment))
	 'UNKNOWN)
	(else
	 (illegal-environment environment 'ENVIRONMENT-ARGUMENTS))))

(define (environment-procedure-name environment)
  (let ((scode-lambda (environment-lambda environment)))
    (and scode-lambda
	 (lambda-name scode-lambda))))

(define (environment-lambda environment)
  (cond ((system-global-environment? environment)
	 #f)
	((ic-environment? environment)
	 (ic-environment/lambda environment))
	((stack-ccenv? environment)
	 (stack-ccenv/lambda environment))
	((closure-ccenv? environment)
	 (closure-ccenv/lambda environment))
	(else
	 (illegal-environment environment 'ENVIRONMENT-LAMBDA))))

(define (environment-bound? environment name)
  (not (eq? 'UNBOUND (environment-reference-type environment name))))

(define (environment-reference-type environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/reference-type environment name))
	((stack-ccenv? environment)
	 (stack-ccenv/reference-type environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/reference-type environment name))
	(else
	 (illegal-environment environment 'ENVIRONMENT-REFERENCE-TYPE))))

(define (environment-assigned? environment name)
  (case (environment-reference-type environment name)
    ((UNBOUND) (error:unbound-variable environment name))
    ((MACRO) (error:macro-binding environment name))
    ((UNASSIGNED) #f)
    (else #t)))

(define (environment-lookup environment name)
  (let ((value (environment-safe-lookup environment name)))
    (cond ((unassigned-reference-trap? value)
	   (error:unassigned-variable environment name))
	  ((macro-reference-trap? value)
	   (error:macro-binding environment name))
	  (else value))))

(define (environment-lookup-macro environment name)
  (and (eq? 'MACRO (environment-reference-type environment name))
       (let ((value (environment-safe-lookup environment name)))
	 (and (macro-reference-trap? value)
	      (macro-reference-trap-transformer value)))))

(define (environment-safe-lookup environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/safe-lookup environment name))
	((stack-ccenv? environment)
	 (stack-ccenv/safe-lookup environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/safe-lookup environment name))
	(else
	 (illegal-environment environment 'ENVIRONMENT-SAFE-LOOKUP))))

(define (environment-assignable? environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/assignable? environment name))
	((stack-ccenv? environment)
	 (stack-ccenv/assignable? environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/assignable? environment name))
	(else
	 (illegal-environment environment 'ENVIRONMENT-ASSIGNABLE?))))

(define (environment-assign! environment name value)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/assign! environment name value))
	((stack-ccenv? environment)
	 (stack-ccenv/assign! environment name value))
	((closure-ccenv? environment)
	 (closure-ccenv/assign! environment name value))
	(else
	 (illegal-environment environment 'ENVIRONMENT-ASSIGN!))))

(define (environment-definable? environment name)
  name
  (cond ((interpreter-environment? environment) #t)
	((or (stack-ccenv? environment) (closure-ccenv? environment)) #f)
	(else (illegal-environment environment 'ENVIRONMENT-DEFINABLE?))))

(define (environment-define environment name value)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/define environment name value))
	((or (stack-ccenv? environment)
	     (closure-ccenv? environment))
	 (error:bad-range-argument environment 'ENVIRONMENT-DEFINE))
	(else
	 (illegal-environment environment 'ENVIRONMENT-DEFINE))))

(define (environment-define-macro environment name value)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/define-macro environment name value))
	((or (stack-ccenv? environment)
	     (closure-ccenv? environment))
	 (error:bad-range-argument environment 'ENVIRONMENT-DEFINE-MACRO))
	(else
	 (illegal-environment environment 'ENVIRONMENT-DEFINE-MACRO))))

;;;; Global environment

(define-integrable (system-global-environment? object)
  (eq? system-global-environment object))

(define (system-global-environment/bound-names)
  (walk-global object? map-entry/name))

(define (system-global-environment/macro-names)
  (walk-global macro-reference-trap? map-entry/name))

(define (object? v) v #t)

(define (map-entry/name name value)
  value
  name)

(define (map-entry/value name value)
  name
  value)

(define (walk-global keep? map-entry)
  (let ((obarray (fixed-objects-item 'OBARRAY)))
    (let ((n-buckets (vector-length obarray)))
      (let per-bucket ((index 0) (result '()))
	(if (fix:< index n-buckets)
	    (let per-symbol
		((bucket (vector-ref obarray index))
		 (result result))
	      (if (pair? bucket)
		  (per-symbol (cdr bucket)
			      (let ((name (car bucket)))
				(if (special-unbound-name? name)
				    result
				    (let ((value
					   (map-reference-trap-value
					    (lambda ()
					      (system-pair-cdr name)))))
				      (if (or (unbound-reference-trap? value)
					      (not (keep? value)))
					  result
					  (cons (map-entry name value)
						result))))))
		  (per-bucket (fix:+ index 1) result)))
	    result)))))

(define (special-unbound-name? name)
  (eq? name package-name-tag))

;;;; Interpreter Environments

(define (interpreter-environment? object)
  (or (system-global-environment? object)
      (ic-environment? object)))

(define-integrable (ic-environment? object)
  (object-type? (ucode-type environment) object))

(define (guarantee-interpreter-environment object)
  (if (not (interpreter-environment? object))
      (error:wrong-type-datum object "interpreter environment"))
  object)

(define (interpreter-environment/reference-type environment name)
  (let ((i ((ucode-primitive lexical-reference-type 2) environment name))
	(v '#(UNBOUND UNASSIGNED NORMAL MACRO)))
    (if (not (fix:< i (vector-length v)))
	(error "Unknown reference type:" i 'ENVIRONMENT-REFERENCE-TYPE))
    (vector-ref v i)))

(define (interpreter-environment/safe-lookup environment name)
  (let ((cell (list #f)))
    (set-car! cell
	      ((ucode-primitive safe-lexical-reference 2) environment name))
    (map-reference-trap (lambda () (car cell)))))

(define (interpreter-environment/assignable? environment name)
  (case (interpreter-environment/reference-type environment name)
    ((UNBOUND) (error:unbound-variable environment name))
    ((MACRO) (error:macro-binding environment name))
    (else #t)))

(define (interpreter-environment/assign! environment name value)
  (lexical-assignment environment name value)
  unspecific)

(define (interpreter-environment/define environment name value)
  (local-assignment environment name value))

(define (interpreter-environment/define-macro environment name transformer)
  (local-assignment environment name
		    (make-unmapped-macro-reference-trap transformer)))

(define (ic-environment/bound-names environment)
  (map-ic-environment-bindings environment object? map-entry/name))

(define (ic-environment/macro-names environment)
  (map-ic-environment-bindings environment
			       macro-reference-trap?
			       map-entry/name))

(define (ic-environment/arguments environment)
  (let ((environment (ic-external-frame environment)))
    (walk-ic-procedure-args environment
			    (ic-frame-procedure* environment)
			    object?
			    map-entry/value)))

(define (map-ic-environment-bindings environment keep? map-entry)
  (let ((external (ic-external-frame environment))
	(do-frame
	 (lambda (frame)
	   (let ((procedure (ic-frame-procedure frame)))
	     (if (vector? procedure)
		 (append! (walk-ic-frame-extension procedure keep? map-entry)
			  (walk-ic-procedure-args frame
						  (vector-ref procedure 1)
						  keep?
						  map-entry))
		 (walk-ic-procedure-args frame procedure keep? map-entry))))))
    (if (eq? external environment)
	(do-frame environment)
	(append! (do-frame environment) (do-frame external)))))

(define (walk-ic-procedure-args frame procedure keep? map-entry)
  (let ((name-vector (lambda-names-vector (procedure-lambda procedure))))
    (let loop ((index (vector-length name-vector)) (result '()))
      (if (fix:> index 1)
	  (let ((index (fix:- index 1)))
	    (loop index
		  (let ((name (vector-ref name-vector index)))
		    (if (special-unbound-name? name)
			result
			(let ((value (ic-frame-arg frame index)))
			  (if (or (unbound-reference-trap? value)
				  (not (keep? value)))
			      result
			      (cons (map-entry name value) result)))))))
	  result))))

(define (walk-ic-frame-extension extension keep? map-entry)
  (let ((limit (fix:+ 3 (object-datum (vector-ref extension 2)))))
    (let loop ((index 3) (result '()))
      (if (fix:< index limit)
	  (loop (fix:+ index 1)
		(let ((p (vector-ref extension index)))
		  (let ((name (car p)))
		    (if (special-unbound-name? name)
			result
			(let ((value
			       (map-reference-trap-value (lambda () (cdr p)))))
			  (if (keep? value)
			      (cons (map-entry name value) result)
			      result))))))
	  result))))

(define (ic-environment/has-parent? environment)
  (interpreter-environment? (ic-frame-parent environment)))

(define (ic-environment/parent environment)
  (let ((parent (ic-frame-parent environment)))
    (if (not (interpreter-environment? parent))
	(error:bad-range-argument environment 'ENVIRONMENT-PARENT))
    parent))

(define (ic-frame-parent environment)
  (procedure-environment (ic-environment/procedure environment)))

(define (ic-environment/lambda environment)
  (procedure-lambda (ic-environment/procedure environment)))

(define (ic-environment/procedure environment)
  (let ((procedure (ic-frame-procedure* environment)))
    (if (internal-lambda? (procedure-lambda procedure))
	(ic-frame-procedure* (procedure-environment procedure))
	procedure)))

(define (ic-external-frame environment)
  (let ((procedure (ic-frame-procedure* environment)))
    (if (internal-lambda? (procedure-lambda procedure))
	(procedure-environment procedure)
	environment)))

(define (ic-frame-procedure* environment)
  (let ((procedure (ic-frame-procedure environment)))
    (if (vector? procedure)
	(vector-ref procedure 1)
	procedure)))

(define-integrable (ic-frame-procedure environment)
  (system-vector-ref environment 0))

(define-integrable (ic-frame-arg environment index)
  (map-reference-trap-value
   (lambda ()
     (system-vector-ref environment index))))

(define (extend-top-level-environment environment #!optional names values)
  (if (not (interpreter-environment? environment))
      (illegal-environment environment 'EXTEND-TOP-LEVEL-ENVIRONMENT))
  (%extend-top-level-environment
   environment
   (if (default-object? names) '() names)
   (if (default-object? values) 'DEFAULT values)
   'EXTEND-TOP-LEVEL-ENVIRONMENT))

(define (make-root-top-level-environment #!optional names values)
  (%extend-top-level-environment
   (object-new-type (object-type #f) (fix:xor (object-datum #f) 1))
   (if (default-object? names) '() names)
   (if (default-object? values) 'DEFAULT values)
   'MAKE-ROOT-TOP-LEVEL-ENVIRONMENT))

(define (%extend-top-level-environment environment names values procedure)
  (if (not (list-of-type? names symbol?))
      (error:wrong-type-argument names "list of symbols" procedure))
  (system-list->vector
   (ucode-type environment)
   (cons (system-pair-cons (ucode-type procedure)
			   (make-slambda lambda-tag:unnamed names unspecific)
			   environment)
	 (if (eq? values 'DEFAULT)
	     (let ((values (make-list (length names))))
	       (do ((values values (cdr values)))
		   ((not (pair? values)))
		 (set-car! values
			   (make-unmapped-unassigned-reference-trap)))
	       values)
	     (begin
	       (if (not (list? values))
		   (error:wrong-type-argument values "list" procedure))
	       (if (not (fix:= (length values) (length names)))
		   (error:bad-range-argument values procedure))
	       values)))))

;;;; Compiled Code Environments

(define-structure (stack-ccenv (type vector)
			       (named
				((ucode-primitive string->symbol)
				 "#[(runtime environment)stack-ccenv]"))
			       (conc-name stack-ccenv/))
  (block #f read-only #t)
  (frame #f read-only #t)
  (start-index #f read-only #t))

(define (stack-frame/environment frame default)
  (let* ((ret-add (stack-frame/return-address frame))
	 (object (compiled-entry/dbg-object ret-add)))
    (cond ((not object)
	   default)
	  ((dbg-continuation? object)
	   (let ((block (dbg-continuation/block object)))
	     (let ((parent (dbg-block/parent block)))
	       (case (dbg-block/type parent)
		 ((STACK)
		  (make-stack-ccenv parent
				    frame
				    (+ (dbg-continuation/offset object)
				       (dbg-block/length block))))
		 ((IC)
		  (let ((index (dbg-block/ic-parent-index block)))
		    (if index
			(guarantee-interpreter-environment
			 (stack-frame/ref frame index))
			default)))
		 (else
		  (error "Illegal continuation parent block" parent))))))
	  ((dbg-procedure? object)
	   (let ((block (dbg-procedure/block object)))
	     (case (dbg-block/type block)
	       ((STACK)
		(make-stack-ccenv block
				  frame
				  (if (compiled-closure? ret-add) 0 1)))
	       (else
		(error "Illegal procedure block" block)))))
	  #|
	  ((dbg-expression? object)
	   ;; for now
	   default)
	  |#
	  (else
	   default))))

(define (compiled-procedure/environment entry)
  (if (not (compiled-procedure? entry))
      (error "Not a compiled procedure" entry 'COMPILED-PROCEDURE/ENVIRONMENT))
  (let ((procedure (compiled-entry/dbg-object entry)))
    (if (not procedure)
	(error "Unable to obtain closing environment" entry))
    (let ((block (dbg-procedure/block procedure)))
      (if (not block)
	  (error "Unable to obtain closing environment (missing block info)"
		 entry))
      (let ((parent (dbg-block/parent block)))
	(define (use-compile-code-block-environment)
	  (guarantee-interpreter-environment
	   (compiled-code-block/environment
	    (compiled-code-address->block entry))))
	(if parent
	    (case (dbg-block/type parent)
	      ((CLOSURE)
	       (make-closure-ccenv (dbg-block/original-parent block)
				   parent
				   entry))
	      ((IC)
	       (use-compile-code-block-environment))
	      (else
	       (error "Illegal procedure parent block" parent)))
	    ;; This happens when the procedure has no free variables:
	    (use-compile-code-block-environment))))))

(define (stack-ccenv/has-parent? environment)
  (if (dbg-block/parent (stack-ccenv/block environment))
      #t
      'SIMULATED))

(define (stack-ccenv/parent environment)
  (let ((block (stack-ccenv/block environment)))
    (let ((parent (dbg-block/parent block)))
      (if parent
	  (case (dbg-block/type parent)
	    ((STACK)
	     (let loop
		 ((block block)
		  (frame (stack-ccenv/frame environment))
		  (index
		   (+ (stack-ccenv/start-index environment)
		      (dbg-block/length block))))
	       (let ((stack-link (dbg-block/stack-link block)))
		 (cond ((not stack-link)
			(with-values
			    (lambda ()
			      (stack-frame/resolve-stack-address
			       frame
			       (stack-ccenv/static-link environment)))
			  (lambda (frame index)
			    (let ((block (dbg-block/parent block)))
			      (if (eq? block parent)
				  (make-stack-ccenv parent frame index)
				  (loop block frame index))))))
		       ((eq? stack-link parent)
			(make-stack-ccenv parent frame index))
		       (else
			(loop stack-link
			      frame
			      (+ (vector-length
				  (dbg-block/layout-vector stack-link))
				 (case (dbg-block/type stack-link)
				   ((STACK)
				    0)
				   ((CONTINUATION)
				    (dbg-continuation/offset
				     (dbg-block/procedure stack-link)))
				   (else
				    (error "illegal stack-link type"
					   stack-link)))
				 index)))))))
	    ((CLOSURE)
	     (make-closure-ccenv (dbg-block/original-parent block)
				 parent
				 (stack-ccenv/normal-closure environment)))
	    ((IC)
	     (guarantee-interpreter-environment
	      (if (dbg-block/static-link-index block)
		  (stack-ccenv/static-link environment)
		  (compiled-code-block/environment
		   (compiled-code-address->block
		    (stack-frame/return-address
		     (stack-ccenv/frame environment)))))))
	    (else
	     (error "illegal parent block" parent)))
	  (let ((environment
		 (compiled-code-block/environment
		   (compiled-code-address->block
		    (stack-frame/return-address
		     (stack-ccenv/frame environment))))))
	    (if (ic-environment? environment)
		environment
		system-global-environment))))))

(define (stack-ccenv/lambda environment)
  (dbg-block/source-code (stack-ccenv/block environment)))

(define (stack-ccenv/arguments environment)
  (let ((procedure (dbg-block/procedure (stack-ccenv/block environment))))
    (if procedure
	(letrec ((lookup
		  (lambda (variable)
		    (case (dbg-variable/type variable)
		      ((INTEGRATED)
		       (dbg-variable/value variable))
		      ((INDIRECTED)
		       (lookup (dbg-variable/value variable)))
		      (else
		       (stack-ccenv/safe-lookup
			environment
			(dbg-variable/name variable)))))))
	  (map* (map* (let ((rest (dbg-procedure/rest procedure)))
			(if rest (lookup rest) '()))
		      lookup
		      (dbg-procedure/optional procedure))
		lookup
		(dbg-procedure/required procedure)))
	'UNKNOWN)))

(define (stack-ccenv/bound-names environment)
  (map dbg-variable/name
       (list-transform-positive
	   (vector->list
	    (dbg-block/layout-vector (stack-ccenv/block environment)))
	 dbg-variable?)))

(define (stack-ccenv/reference-type environment name)
  (dbg-variable-reference-type (stack-ccenv/block environment)
			       name
			       (lambda (index)
				 (stack-ccenv/get-value environment index))
    (lambda (name)
      (environment-reference-type (stack-ccenv/parent environment) name))))

(define (stack-ccenv/safe-lookup environment name)
  (lookup-dbg-variable (stack-ccenv/block environment)
		       name
		       (lambda (index)
			 (stack-ccenv/get-value environment index))
    (lambda (name)
      (environment-safe-lookup (stack-ccenv/parent environment) name))))

(define (stack-ccenv/assignable? environment name)
  (assignable-dbg-variable? (stack-ccenv/block environment) name
    (lambda (name)
      (environment-assignable? (stack-ccenv/parent environment) name))))

(define (stack-ccenv/assign! environment name value)
  (assign-dbg-variable! (stack-ccenv/block environment)
			name
			(lambda (index)
			  (stack-ccenv/get-value environment index))
			value
    (lambda (name)
      (environment-assign! (stack-ccenv/parent environment) name value))))

(define (stack-ccenv/get-value environment index)
  (let ((cell (list #f)))
    (set-car!
     cell
     (stack-frame/ref (stack-ccenv/frame environment)
		      (+ (stack-ccenv/start-index environment) index)))
    (map-reference-trap (lambda () (car cell)))))

(define (stack-ccenv/static-link environment)
  (let ((static-link
	 (find-stack-element environment
			     dbg-block/static-link-index
			     "static link")))
    (if (not (or (stack-address? static-link)
		 (interpreter-environment? static-link)))
	(error "Illegal static link in frame" static-link environment))
    static-link))

(define (stack-ccenv/normal-closure environment)
  (let ((closure
	 (find-stack-element environment
			     dbg-block/normal-closure-index
			     "closure")))
    (if (not (or (compiled-closure? closure) (vector? closure)))
	(error "Frame missing closure" closure environment))
#|
    ;; Temporarily disable this consistency check until the compiler
    ;; is modified to provide the correct information for
    ;; multi-closed procedures.
    (if (not (eq? (compiled-entry/dbg-object closure)
		  (dbg-block/procedure (stack-ccenv/block environment))))
	(error "Wrong closure in frame" closure environment))
|#
    closure))

(define (find-stack-element environment procedure name)
  (let ((frame (stack-ccenv/frame environment)))
    (stack-frame/ref
     frame
     (let ((index
	    (find-stack-index (stack-ccenv/block environment)
			      (stack-ccenv/start-index environment)
			      (stack-frame/length frame)
			      procedure)))
       (if (not index)
	   (error (string-append "Unable to find " name) environment))
       index))))

(define (find-stack-index block start end procedure)
  (let loop ((block block) (start start))
    (let ((index (procedure block)))
      (if index
	  (+ start index)
	  (let ((start (+ start (dbg-block/length block)))
		(link (dbg-block/stack-link block)))
	    (and link
		 (< start end)
		 (loop link start)))))))

(define-integrable (dbg-block/length block)
  (vector-length (dbg-block/layout-vector block)))

(define-structure (closure-ccenv
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime environment)closure-ccenv]"))
		   (conc-name closure-ccenv/))
  (stack-block #f read-only #t)
  (closure-block #f read-only #t)
  (closure #f read-only #t))

(define (closure-ccenv/bound-names environment)
  (map dbg-variable/name
       (list-transform-positive
	   (vector->list
	    (dbg-block/layout-vector (closure-ccenv/stack-block environment)))
	 (lambda (variable)
	   (and (dbg-variable? variable)
		(or (eq? (dbg-variable/type variable) 'INTEGRATED)
		    (vector-find-next-element
		     (dbg-block/layout-vector
		      (closure-ccenv/closure-block environment))
		     variable)))))))

(define (closure-ccenv/reference-type environment name)
  (dbg-variable-reference-type (closure-ccenv/closure-block environment)
			       name
			       (lambda (index)
				 (closure-ccenv/get-value environment index))
    (lambda (name)
      (environment-reference-type (closure-ccenv/parent environment) name))))

(define (closure-ccenv/safe-lookup environment name)
  (lookup-dbg-variable (closure-ccenv/closure-block environment)
		       name
		       (lambda (index)
			 (closure-ccenv/get-value environment index))
    (lambda (name)
      (environment-safe-lookup (closure-ccenv/parent environment) name))))

(define (closure-ccenv/assignable? environment name)
  (assignable-dbg-variable? (closure-ccenv/closure-block environment) name
    (lambda (name)
      (environment-assignable? (closure-ccenv/parent environment) name))))

(define (closure-ccenv/assign! environment name value)
  (assign-dbg-variable! (closure-ccenv/closure-block environment)
			name
			(lambda (index)
			  (closure-ccenv/get-value environment index))
			value
    (lambda (name)
      (environment-assign! (closure-ccenv/parent environment) name value))))

(define (closure-ccenv/get-value environment index)
  (closure/get-value (closure-ccenv/closure environment)
		     (closure-ccenv/closure-block environment)
		     index))

(define (closure/get-value closure closure-block index)
  (let ((cell (list #f)))
    (set-car!
     cell
     (compiled-closure/ref closure
			   index
			   (dbg-block/layout-first-offset closure-block)))
    (map-reference-trap (lambda () (car cell)))))

(define (closure-ccenv/has-parent? environment)
  (or (let ((stack-block (closure-ccenv/stack-block environment)))
	(let ((parent (dbg-block/parent stack-block)))
	  (and parent
	       (case (dbg-block/type parent)
		 ((CLOSURE) (and (dbg-block/original-parent stack-block) #t))
		 ((STACK IC) #t)
		 (else (error "Illegal parent block" parent))))))
      'SIMULATED))

(define (closure-ccenv/parent environment)
  (let ((stack-block (closure-ccenv/stack-block environment))
	(closure-block (closure-ccenv/closure-block environment))
	(closure (closure-ccenv/closure environment)))
    (let ((parent (dbg-block/parent stack-block))
	  (use-simulation
	   (lambda ()
	     (if (compiled-closure? closure)
		 (let ((environment
			(compiled-code-block/environment
			 (compiled-entry/block closure))))
		   (if (ic-environment? environment)
		       environment
		       system-global-environment))
		 system-global-environment))))
      (if parent
	  (case (dbg-block/type parent)
	    ((STACK)
	     (make-closure-ccenv parent closure-block closure))
	    ((CLOSURE)
	     (let ((parent (dbg-block/original-parent stack-block)))
	       (if parent
		   (make-closure-ccenv parent closure-block closure)
		   (use-simulation))))
	    ((IC)
	     (guarantee-interpreter-environment
	      (let ((index (dbg-block/ic-parent-index closure-block)))
		(if index
		    (closure/get-value closure closure-block index)
		    (use-simulation)))))
	    (else
	     (error "Illegal parent block" parent)))
	  (use-simulation)))))

(define (closure-ccenv/lambda environment)
  (dbg-block/source-code (closure-ccenv/stack-block environment)))

(define (lookup-dbg-variable block name get-value not-found)
  (let loop ((name name))
    (let ((index (dbg-block/find-name block name)))
      (if index
	  (let ((variable (vector-ref (dbg-block/layout-vector block) index)))
	    (case (dbg-variable/type variable)
	      ((NORMAL)
	       (get-value index))
	      ((CELL)
	       (let ((value (get-value index)))
		 (if (not (cell? value))
		     (error "Value of variable should be in cell:"
			    variable value))
		 (cell-contents value)))
	      ((INTEGRATED)
	       (dbg-variable/value variable))
	      ((INDIRECTED)
	       (loop (dbg-variable/name (dbg-variable/value variable))))
	      (else
	       (error "Unknown variable type:" variable))))
	  (not-found name)))))

(define (dbg-variable-reference-type block name get-value not-found)
  (let ((value->reference-type
	 (lambda (value)
	   (cond ((unassigned-reference-trap? value) 'UNASSIGNED)
		 ((macro-reference-trap? value) 'MACRO)
		 (else 'NORMAL)))))
    (let loop ((name name))
      (let ((index (dbg-block/find-name block name)))
	(if index
	    (let ((variable
		   (vector-ref (dbg-block/layout-vector block) index)))
	      (case (dbg-variable/type variable)
		((NORMAL)
		 (value->reference-type (get-value index)))
		((CELL)
		 (let ((value (get-value index)))
		   (if (not (cell? value))
		       (error "Value of variable should be in cell"
			      variable value))
		   (value->reference-type (cell-contents value))))
		((INTEGRATED)
		 (value->reference-type (dbg-variable/value variable)))
		((INDIRECTED)
		 (loop (dbg-variable/name (dbg-variable/value variable))))
		(else
		 (error "Unknown variable type:" variable))))
	    (not-found name))))))

(define (assignable-dbg-variable? block name not-found)
  (let ((index (dbg-block/find-name block name)))
    (if index
	(eq? 'CELL
	     (dbg-variable/type
	      (vector-ref (dbg-block/layout-vector block)
			  index)))
	(not-found name))))

(define (assign-dbg-variable! block name get-value value not-found)
  (let ((index (dbg-block/find-name block name)))
    (if index
	(let ((variable (vector-ref (dbg-block/layout-vector block) index)))
	  (case (dbg-variable/type variable)
	    ((CELL)
	     (let ((cell (get-value index)))
	       (if (not (cell? cell))
		   (error "Value of variable should be in cell:" name cell))
	       (set-cell-contents! cell value)
	       unspecific))
	    ((NORMAL INTEGRATED INDIRECTED)
	     (error "Variable cannot be modified:" variable))
	    (else
	     (error "Unknown variable type:" variable))))
	(not-found name))))

(define (dbg-block/name block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/name procedure))))

(define (dbg-block/source-code block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/source-code procedure))))