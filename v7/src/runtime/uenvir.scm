#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/uenvir.scm,v 14.27 1992/07/21 22:01:58 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

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

;;;; Microcode Environments
;;; package: (runtime environment)

(declare (usual-integrations))

(define (environment? object)
  (or (system-global-environment? object)
      (ic-environment? object)
      (stack-ccenv? object)
      (closure-ccenv? object)))

(define (environment-has-parent? environment)
  (cond ((system-global-environment? environment)
	 false)
	((ic-environment? environment)
	 (ic-environment/has-parent? environment))
	((stack-ccenv? environment)
	 (stack-ccenv/has-parent? environment))
	((closure-ccenv? environment)
	 (closure-ccenv/has-parent? environment))
	(else (error "Illegal environment" environment))))

(define (environment-parent environment)
  (cond ((system-global-environment? environment)
	 (error "Global environment has no parent" environment))
	((ic-environment? environment)
	 (ic-environment/parent environment))
	((stack-ccenv? environment)
	 (stack-ccenv/parent environment))
	((closure-ccenv? environment)
	 (closure-ccenv/parent environment))
	(else (error "Illegal environment" environment))))

(define (environment-bound-names environment)
  (cond ((system-global-environment? environment)
	 (system-global-environment/bound-names environment))
	((ic-environment? environment)
	 (ic-environment/bound-names environment))
	((stack-ccenv? environment)
	 (stack-ccenv/bound-names environment))
	((closure-ccenv? environment)
	 (closure-ccenv/bound-names environment))
	(else (error "Illegal environment" environment))))

(define (environment-bindings environment)
  (map (lambda (name)
	 (cons name
	       (let ((value (environment-lookup environment name)))
		 (if (unassigned-reference-trap? value)
		     '()
		     (list value)))))
       (environment-bound-names environment)))

(define (environment-arguments environment)
  (cond ((ic-environment? environment)
	 (ic-environment/arguments environment))
	((stack-ccenv? environment)
	 (stack-ccenv/arguments environment))
	((or (system-global-environment? environment)
	     (closure-ccenv? environment))
	 'UNKNOWN)
	(else (error "Illegal environment" environment))))

(define (environment-procedure-name environment)
  (let ((scode-lambda (environment-lambda environment)))
    (and scode-lambda
	 (lambda-name scode-lambda))))

(define (environment-lambda environment)
  (cond ((system-global-environment? environment)
	 false)
	((ic-environment? environment)
	 (ic-environment/lambda environment))
	((stack-ccenv? environment)
	 (stack-ccenv/lambda environment))
	((closure-ccenv? environment)
	 (closure-ccenv/lambda environment))
	(else (error "Illegal environment" environment))))

(define (environment-bound? environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/bound? environment name))
	((stack-ccenv? environment)
	 (stack-ccenv/bound? environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/bound? environment name))
	(else (error "Illegal environment" environment))))

(define (environment-lookup environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/lookup environment name))
	((stack-ccenv? environment)
	 (stack-ccenv/lookup environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/lookup environment name))
	(else (error "Illegal environment" environment))))

(define (environment-assignable? environment name)
  (cond ((interpreter-environment? environment)
	 true)
	((stack-ccenv? environment)
	 (stack-ccenv/assignable? environment name))
	((closure-ccenv? environment)
	 (closure-ccenv/assignable? environment name))
	(else (error "Illegal environment" environment))))

(define (environment-assign! environment name value)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/assign! environment name value))
	((stack-ccenv? environment)
	 (stack-ccenv/assign! environment name value))
	((closure-ccenv? environment)
	 (closure-ccenv/assign! environment name value))
	(else (error "Illegal environment" environment))))

;;;; Interpreter Environments

(define (interpreter-environment? object)
  (or (system-global-environment? object)
      (ic-environment? object)))

(define (guarantee-interpreter-environment object)
  (if (not (interpreter-environment? object))
      (error:wrong-type-datum object "interpreter environment"))
  object)

(define-integrable (system-global-environment? object)
  (eq? system-global-environment object))

(define (interpreter-environment/bound? environment name)
  (not (lexical-unbound? environment name)))

(define (interpreter-environment/lookup environment name)
  (if (lexical-unassigned? environment name)
      (make-unassigned-reference-trap)
      (lexical-reference environment name)))

(define (interpreter-environment/assign! environment name value)
  (lexical-assignment environment name value)
  unspecific)

(define (system-global-environment/bound-names environment)
  (list-transform-negative (obarray->list (fixed-objects-item 'OBARRAY))
    (lambda (symbol)
      (unbound-name? environment symbol))))

(define-integrable (ic-environment? object)
  (object-type? (ucode-type environment) object))

(define (ic-environment/has-parent? environment)
  (not (eq? (ic-environment/parent environment) null-environment)))

(define (ic-environment/parent environment)
  (select-parent (ic-environment->external environment)))

(define (ic-environment/bound-names environment)
  (list-transform-negative
      (map* (lambda-bound
	     (select-lambda (ic-environment->external environment)))
	    car
	    (let ((extension (ic-environment/extension environment)))
	      (if (environment-extension? extension)
		  (environment-extension-aux-list extension)
		  '())))
    (lambda (name)
      (unbound-name? environment name))))

(define (unbound-name? environment name)
  (if (eq? name package-name-tag)
      true
      (lexical-unbound? environment name)))

(define (ic-environment/arguments environment)
  (lambda-components* (select-lambda (ic-environment->external environment))
    (lambda (name required optional rest body)
      name body
      (let ((lookup
	     (lambda (name)
	       (interpreter-environment/lookup environment name))))
	(map* (map* (if rest (lookup rest) '())
		    lookup
		    optional)
	      lookup
	      required)))))

(define (ic-environment/lambda environment)
  (procedure-lambda (ic-environment/procedure environment)))

(define (ic-environment/procedure environment)
  (select-procedure (ic-environment->external environment)))

(define (ic-environment/set-parent! environment parent)
  (system-pair-set-cdr!
   (let ((extension (ic-environment/extension environment)))
     (if (environment-extension? extension)
	 (begin
	   (set-environment-extension-parent! extension parent)
	   (environment-extension-procedure extension))
	 extension))
   parent))

(define (ic-environment/remove-parent! environment)
  (ic-environment/set-parent! environment null-environment))

(define null-environment
  (object-new-type (ucode-type null) 1))

(define (make-null-interpreter-environment)
  (let ((environment (let () (the-environment))))
    (ic-environment/remove-parent! environment)
    environment))

(define (ic-environment->external environment)
  (let ((procedure (select-procedure environment)))
    (if (internal-lambda? (procedure-lambda procedure))
	(procedure-environment procedure)
	environment)))

(define-integrable (select-extension environment)
  (system-vector-ref environment 0))

(define (select-procedure environment)
  (let ((object (select-extension environment)))
    (if (environment-extension? object)
	(environment-extension-procedure object)
	object)))

(define (select-parent environment)
  (procedure-environment (select-procedure environment)))

(define (select-lambda environment)
  (procedure-lambda (select-procedure environment)))

(define (ic-environment/extension environment)
  (select-extension (ic-environment->external environment)))

;;;; Compiled Code Environments

(define-structure (stack-ccenv
		   (named
		    (string->symbol "#[(runtime environment)stack-ccenv]"))
		   (conc-name stack-ccenv/))
  (block false read-only true)
  (frame false read-only true)
  (start-index false read-only true))

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
		(make-stack-ccenv
		 block
		 frame
		 (if (compiled-closure? ret-add)
		     0
		     1)))
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
  (let ((procedure (compiled-entry/dbg-object entry)))
    (if (not procedure)
	(error "Unable to obtain closing environment" entry))
    (let ((block (dbg-procedure/block procedure)))
      (let ((parent (dbg-block/parent block)))
	(case (dbg-block/type parent)
	  ((CLOSURE)
	   (make-closure-ccenv (dbg-block/original-parent block)
			       parent
			       entry))
	  ((IC)
	   (guarantee-interpreter-environment
	    (compiled-code-block/environment
	     (compiled-code-address->block entry))))
	  (else
	   (error "Illegal procedure parent block" parent)))))))

(define (stack-ccenv/has-parent? environment)
  (if (dbg-block/parent (stack-ccenv/block environment))
      true
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
		       (stack-ccenv/lookup environment
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

(define (stack-ccenv/bound? environment name)
  (dbg-block/find-name (stack-ccenv/block environment) name))

(define (stack-ccenv/lookup environment name)
  (lookup-dbg-variable (stack-ccenv/block environment)
		       name
		       (stack-ccenv/get-value environment)))

(define (stack-ccenv/assignable? environment name)
  (assignable-dbg-variable? (stack-ccenv/block environment) name))

(define (stack-ccenv/assign! environment name value)
  (assign-dbg-variable! (stack-ccenv/block environment)
			name
			(stack-ccenv/get-value environment)
			value))

(define (stack-ccenv/get-value environment)
  (lambda (index)
    (stack-frame/ref (stack-ccenv/frame environment)
		     (+ (stack-ccenv/start-index environment) index))))

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
		   (named
		    (string->symbol "#[(runtime environment)closure-ccenv]"))
		   (conc-name closure-ccenv/))
  (stack-block false read-only true)
  (closure-block false read-only true)
  (closure false read-only true))

(define (closure-ccenv/bound-names environment)
  (map dbg-variable/name
       (list-transform-positive
	   (vector->list
	    (dbg-block/layout-vector (closure-ccenv/stack-block environment)))
	 (lambda (variable)
	   (and (dbg-variable? variable)
		(closure-ccenv/variable-bound? environment variable))))))

(define (closure-ccenv/bound? environment name)
  (let ((block (closure-ccenv/stack-block environment)))
    (let ((index (dbg-block/find-name block name)))
      (and index
	   (closure-ccenv/variable-bound?
	    environment
	    (vector-ref (dbg-block/layout-vector block) index))))))

(define (closure-ccenv/variable-bound? environment variable)
  (or (eq? (dbg-variable/type variable) 'INTEGRATED)
      (vector-find-next-element
       (dbg-block/layout-vector (closure-ccenv/closure-block environment))
       variable)))

(define (closure-ccenv/lookup environment name)
  (lookup-dbg-variable (closure-ccenv/closure-block environment)
		       name
		       (closure-ccenv/get-value environment)))

(define (closure-ccenv/assignable? environment name)
  (assignable-dbg-variable? (closure-ccenv/closure-block environment) name))

(define (closure-ccenv/assign! environment name value)
  (assign-dbg-variable! (closure-ccenv/closure-block environment)
			name
			(closure-ccenv/get-value environment)
			value))

(define-integrable (closure/get-value closure closure-block index)
  (compiled-closure/ref closure
			index
			(dbg-block/layout-first-offset closure-block)))

(define (closure-ccenv/get-value environment)
  (lambda (index)
    (closure/get-value (closure-ccenv/closure environment)
		       (closure-ccenv/closure-block environment)
		       index)))

(define (closure-ccenv/has-parent? environment)
  (or (let ((stack-block (closure-ccenv/stack-block environment)))
	(let ((parent (dbg-block/parent stack-block)))
	  (and parent
	       (case (dbg-block/type parent)
		 ((CLOSURE) (and (dbg-block/original-parent stack-block) true))
		 ((STACK IC) true)
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

(define (lookup-dbg-variable block name get-value)
  (let loop ((name name))
    (let* ((index (dbg-block/find-name block name))
	   (variable (vector-ref (dbg-block/layout-vector block) index)))
      (case (dbg-variable/type variable)
	((NORMAL)
	 (get-value index))
	((CELL)
	 (let ((value (get-value index)))
	   (if (not (cell? value))
	       (error "Value of variable should be in cell" variable value))
	   (cell-contents value)))
	((INTEGRATED)
	 (dbg-variable/value variable))
	((INDIRECTED)
	 (loop (dbg-variable/name (dbg-variable/value variable))))
	(else
	 (error "Unknown variable type" variable))))))

(define (assignable-dbg-variable? block name)
  (eq? 'CELL
       (dbg-variable/type
	(vector-ref (dbg-block/layout-vector block)
		    (dbg-block/find-name block name)))))

(define (assign-dbg-variable! block name get-value value)
  (let* ((index (dbg-block/find-name block name))
	 (variable (vector-ref (dbg-block/layout-vector block) index)))
    (case (dbg-variable/type variable)
      ((CELL)
       (let ((cell (get-value index)))
	 (if (not (cell? cell))
	     (error "Value of variable should be in cell" name cell))
	 (set-cell-contents! cell value)
	 unspecific))
      ((NORMAL INTEGRATED INDIRECTED)
       (error "Variable cannot be side-effected" variable))
      (else
       (error "Unknown variable type" variable)))))

(define (dbg-block/name block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/name procedure))))

(define (dbg-block/source-code block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/source-code procedure))))