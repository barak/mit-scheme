#| -*-Scheme-*-

$Id$

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Microcode Environments
;;; package: (runtime environment)

(declare (usual-integrations))

(define (environment? object)
  (or (system-global-environment? object)
      (ic-environment? object)
      (ccenv? object)))

(define (environment-has-parent? environment)
  (cond ((system-global-environment? environment)
	 #f)
	((ic-environment? environment)
	 (ic-environment/has-parent? environment))
	((ccenv? environment)
	 (ccenv/has-parent? environment))
	(else (illegal-environment environment 'ENVIRONMENT-HAS-PARENT?))))

(define (environment-parent environment)
  (cond ((system-global-environment? environment)
	 (error "Global environment has no parent" environment))
	((ic-environment? environment)
	 (ic-environment/parent environment))
	((ccenv? environment)
	 (ccenv/parent environment))
	(else (illegal-environment environment 'ENVIRONMENT-PARENT))))

(define (environment-bound-names environment)
  (cond ((system-global-environment? environment)
	 (system-global-environment/bound-names environment))
	((ic-environment? environment)
	 (ic-environment/bound-names environment))
	((ccenv? environment)
	 (ccenv/bound-names environment))
	(else (illegal-environment environment 'ENVIRONMENT-BOUND-NAMES))))

(define (environment-bindings environment)
  (map (lambda (name)
	 (cons name
	       (let ((value (environment-lookup environment name)))
		 (if (unassigned-reference-trap? value)
		     '()
		     (list value)))))
       (environment-bound-names environment)))

(define (environment-arguments environment)
  (cond ((system-global-environment? environment)
	 'UNKNOWN)
	((ic-environment? environment)
	 (ic-environment/arguments environment))
	((ccenv? environment)
	 (ccenv/arguments environment))
	(else (illegal-environment environment 'ENVIRONMENT-ARGUMENTS))))

(define (environment-procedure-name environment)
  (let ((scode-lambda (environment-lambda environment)))
    (and scode-lambda
	 (lambda-name scode-lambda))))

(define (environment-lambda environment)
  (cond ((system-global-environment? environment)
	 #f)
	((ic-environment? environment)
	 (ic-environment/lambda environment))
	((ccenv? environment)
	 (ccenv/lambda environment))
	(else (illegal-environment environment 'ENVIRONMENT-LAMBDA))))

(define (environment-bound? environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/bound? environment name))
	((ccenv? environment)
	 (ccenv/bound? environment name))
	(else (illegal-environment environment 'ENVIRONMENT-BOUND?))))

(define (environment-lookup environment name)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/lookup environment name))
	((ccenv? environment)
	 (ccenv/lookup environment name))
	(else (illegal-environment environment 'ENVIRONMENT-LOOKUP))))

(define (environment-assignable? environment name)
  (cond ((interpreter-environment? environment)
	 #t)
	((ccenv? environment)
	 (ccenv/assignable? environment name))
	(else (illegal-environment environment 'ENVIRONMENT-ASSIGNABLE?))))

(define (environment-assign! environment name value)
  (cond ((interpreter-environment? environment)
	 (interpreter-environment/assign! environment name value))
	((ccenv? environment)
	 (ccenv/assign! environment name value))
	(else (illegal-environment environment 'ENVIRONMENT-ASSIGN!))))

(define (illegal-environment object procedure)
  (error:wrong-type-argument object "environment" procedure))

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

(define (ic-environment/lambda environment)
  (select-lambda (ic-environment->external environment)))

(define (ic-environment/procedure environment)
  (select-procedure (ic-environment->external environment)))

(define (ic-environment/bound-names environment)
  (list-transform-negative
      (let ((external (ic-environment->external environment))
	    (parameters (lambda-bound (ic-environment/lambda environment)))
	    (extension-names
	     (lambda (environment tail)
	       (let ((extension (select-extension environment)))
		 (if (environment-extension? extension)
		     (map* tail car (environment-extension-aux-list extension))
		     tail)))))
	(extension-names environment
			 (if (eq? environment external)
			     parameters
			     (extension-names external parameters))))
    (lambda (name)
      (unbound-name? environment name))))

(define (unbound-name? environment name)
  (if (eq? name package-name-tag)
      #t
      (lexical-unbound? environment name)))

(define (ic-environment/arguments environment)
  (lambda-components* (ic-environment/lambda environment)
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

(define (ic-environment/set-parent! environment parent)
  (let ((extension (select-extension (ic-environment->external environment))))
    (if (environment-extension? extension)
	(begin
	  (set-environment-extension-parent! extension parent)
	  (system-pair-set-cdr! (environment-extension-procedure extension)
				parent))
	(system-pair-set-cdr! extension parent))))

(define (ic-environment/remove-parent! environment)
  (ic-environment/set-parent! environment null-environment))

;;  This corresponds to the `#define END_OF_CHAIN ...' in sdata.h

(define null-environment
  (object-new-type (object-type #F)
		   (fix:xor (object-datum #F) 1)))

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

(define (extend-ic-environment environment)
  (if (not (or (system-global-environment? environment)
	       (ic-environment? environment)))
      (illegal-environment environment 'EXTEND-IC-ENVIRONMENT))
  (let ((environment (eval '(let () (the-environment)) environment)))
    (set-environment-syntax-table!
     environment
     (make-syntax-table (environment-syntax-table environment)))
    environment))

;;;; Compiled Code Environments

(define-structure (ccenv
		   (type vector)
		   (named
		    ((ucode-primitive string->symbol)
		     "#[(runtime environment)ccenv]"))
		   (conc-name ccenv/))
  ;; BLOCK is a block structure description (a DBG-BLOCK).
  (block #f read-only #t)
  ;; ROOT is the object from which to de-reference access paths, usually a
  ;; STACK-FRAME or a compiled closure.
  (root #f read-only #t))

(define (ccenv/has-parent? env)
  (let ((block  (ccenv/block env)))
    (and (dbg-block/parent block)
	 #T)))

(define (ccenv/parent env)
  (let ((block (ccenv/block env))
	(root  (ccenv/root env)))
    (let ((parent  (dbg-block/parent block))
	  (p-path  (dbg-block/parent-path-prefix block)))
      (let ((root*  (if p-path
			(lookup-path p-path root #F)
			root)))
	(cond ((eq? parent 'IC)
	       (guarantee-interpreter-environment root*))
	      (else
	       (make-ccenv parent root*)))))))
    
(define (ccenv/bound-names environment)
  (map dbg-variable/name
       (list-transform-positive
	   (vector->list
	    (dbg-block/variables (ccenv/block environment)))
	 (lambda (thing)
	   (and (dbg-variable? thing)
		(ccenv/path-bound? environment (dbg-variable/path thing)))))))

(define (ccenv/bound? environment name)
  (let* ((block    (ccenv/block environment))
	 (variable (dbg-block/find-variable block name)))
    (and variable
	 (ccenv/path-bound? environment (dbg-variable/path variable)))))

(define (ccenv/path-bound? environment path)
  ;; Some paths are only valid from an interrupt frame.  The same block is
  ;; used for the interrupt frame of a continuation and the
  ;; (pre-invocation) frame.
  (or (let ((root (ccenv/root environment)))
	(and (stack-frame? root)
	     (stack-frame/compiled-interrupt? root)))
      (not (interrupt-frame-path? path))))

(define (ccenv/lookup environment name)
  (lookup-path (ccenv/find-bound-path environment name)
	       (ccenv/root environment)
	       #F))

(define (ccenv/assignable? environment name)
  (let* ((block (ccenv/block environment))
	 (var   (dbg-block/find-variable block name)))
    (and var
	 (assignable-path? (dbg-variable/path var)))))

(define (ccenv/assign! environment name value)
  (assign-path! (ccenv/find-bound-path environment name)
		(ccenv/root environment)
		name
		value))

(define (ccenv/arguments environment)
  ;; Try to piece together the original arguments, taking into account
  ;; unassigned optionals and unavailable values.
  (let* ((block  (ccenv/block environment))
	 (source  (dbg-block/source-code block)))
    (if (lambda? source)
	(let ((lookup
	       (lambda (name)
		 (if (ccenv/bound? environment name)
		     (ccenv/lookup environment name)
		     unavailable-object))))
	  (lambda-components source
	    (lambda (name required optional rest auxiliary decl body)
	      name auxiliary decl body
	      (let ((required* (map lookup required))
		    (optional* (map lookup optional))
		    (rest*     (if rest (lookup rest) '())))
		(define (known)
		  (append required* optional* rest*))
		(cond ((and (not *allow-unavailable-environment-arguments*)
			    (or (there-exists? required* unavailable?)
				(there-exists? optional* unavailable?)
				(unavailable? rest*)))
		       'UNKNOWN)
		      ((pair? rest*) (known))
		      ((null? optional) (known))
		      (else
		       (let loop ((opts (reverse optional*)) (next #F))
			 (cond ((null? opts)
				(if (unavailable? next)
				    'UNKNOWN
				    required*))
			       ((unassigned-reference-trap? (car opts))
				(loop (cdr opts) (car opts)))
			       ((unavailable? (car opts))
				(loop (cdr opts) (car opts)))
			       ((unavailable? next)
				'UNKNOWN)
			       (else
				(append required* (reverse opts)))))))))))
	'UNKNOWN)))

(define *allow-unavailable-environment-arguments* #T)

(define unavailable-object (string->symbol "??"))

(define (unavailable? thing)
  (eq? thing unavailable-object))

(define (ccenv/lambda environment)
  (dbg-block/source-code (ccenv/block environment)))

(define (ccenv/find-bound-path environment name)
  (let* ((block  (ccenv/block environment))
	 (var    (dbg-block/find-variable block name)))
    (if var
	(dbg-variable/path var)
	((condition-signaller condition-type:unbound-variable
			      '(ENVIRONMENT LOCATION)
			      standard-error-handler)
	 environment name))))

(define (stack-frame/environment frame entry default)
  (let* ((object (compiled-entry/dbg-object entry)))
    (cond ((not object)
	   default)
	  ((dbg-continuation? object)
	   (let ((block (dbg-continuation/block object)))
	     (if block
		 (make-ccenv  block frame)
		 default)))
	  ((dbg-procedure? object)
	   (let ((invocation-block (dbg-procedure/block object)))
	     (cond ((not invocation-block)
		    default)
		   ((stack-frame/compiled-interrupt? frame)
		    (make-ccenv invocation-block frame))
		   (else
		    (error "Non-interrupt procedure frame" entry frame)))))
	  #|				;
	  ((dbg-expression? object)
	   ;; for now
	   default)
	  |#
	  (else
	   default))))

(define (compiled-procedure/environment entry)
  (if (not (compiled-procedure? entry))
      (error:wrong-type-argument entry "compiled procedure"
				 'COMPILED-PROCEDURE/ENVIRONMENT))
  (let ((procedure (compiled-entry/dbg-object entry)))
    (if (not procedure)
	(error "Unable to obtain closing environment" entry))
    (let ((invocation-block (dbg-procedure/block procedure)))
      (if (not invocation-block)
	  (error "Unable to obtain closing environment (missing block info)"
		 entry))
      (let ((parent (dbg-block/parent invocation-block)))
	(cond ((and (eq? parent 'IC)
		    (eq? (dbg-block/parent-path-prefix invocation-block)
			 'TOP-LEVEL-ENVIRONMENT))
	       (guarantee-interpreter-environment
		(compiled-code-block/environment
		 (compiled-code-address->block entry))))
	      ((compiled-closure? entry)
	       (make-ccenv parent entry))
	      (else
	       (make-ccenv parent entry)
	       ;;(error "Illegal procedure parent block" parent)
	       ))))))

(define (lookup-path initial-path root leave-last-instruction?)

  (let ((stack (vector root #f #f #f #f #f))
	(sp    0))
    (define (dispatch instruction)
      (define (path-error message)
	(error message instruction initial-path root sp stack))
      (define (push item)
	(set! sp (+ sp 1))
	(vector-set! stack sp item))
      (define (unary-operation procedure)
	(vector-set! stack sp (procedure (vector-ref stack sp))))
      (define (binary-operation procedure)
	(let* ((sp1   (- sp 1)))
	  (vector-set! stack sp1
		       (procedure (vector-ref stack sp1)
				  (vector-ref stack sp)))
	  (vector-set! stack sp #F)
	  (set! sp sp1)))
      (define (->compiled-code-block place)
	(let ((entry  (or (and (compiled-entry? place) place)
			  (and (stack-frame? place)
			       (stack-frame/return-address place)))))
	  (or (and entry
		   (compiled-entry/block entry))
	      (path-error "Cant find a compiled-code block"))))
      (define (compiled-entry? object)
	(object-type? (ucode-type compiled-entry) object))

      (define (cell-ref cell index)
	(cond ((and (cell? cell) (zero? index))
	       (cell-contents cell))
	      ((vector? cell)
	       (vector-ref cell index))
	      (else (path-error "Not a cell"))))
      (define (flonum-cell-ref cell index)
	(flo:vector-ref cell index))
      (define (constant-block-ref place index)
	(let ((block (->compiled-code-block place)))
	  (if (and (<= (compiled-code-block/constants-start block) index)
		   (<  index (compiled-code-block/constants-end block)))
	      (system-vector-ref (->compiled-code-block place) index)
	      (path-error "Illegal constants block offset"))))
      (define (closure-ref closure index)
	(if (not (compiled-closure? closure))
	    (path-error "Not a compiled closure"))
	((ucode-primitive primitive-object-ref) closure index))
      (define (stack-frame-ref frame index)
	(if (not (stack-frame? frame))
	    (path-error "Not a stack frame"))
	(let ((elements (stack-frame/elements frame)))
	  (vector-ref elements (- (vector-length elements) index))))
      (define (interrupt-frame-ref frame index)
	(if (not (and (stack-frame? frame)
		      (stack-frame/compiled-interrupt? frame)))
	    (path-error "Not a compiled interrupt stack frame"))
	(let ((elements (stack-frame/elements frame)))
	  (vector-ref elements index)))
      (define (cc-block-entry place offset)
	((ucode-primitive primitive-object-new-type)
	 (ucode-type compiled-entry)
	 (fix:+ (object-datum (->compiled-code-block place)) offset)))
      (define (uncoerce-procedure procedure)
	;; just use the coerced procedure for now
      	procedure)
      (define (top-level-environment place)
	(compiled-code-block/environment (->compiled-code-block place)))

      (cond ((pair? instruction)
	     (push (cdr instruction))
	     (dispatch (car instruction)))
	    ((primitive-procedure? instruction)
	     (case (primitive-procedure-arity instruction)
	       ((1)  (unary-operation instruction))
	       ((2)  (binary-operation instruction))
	       (else (path-error "Unknown primitive arity"))))
	    (else
	     (case instruction
	       ((INTEGRATED)
		;; we have the root and the constant on the stack!
		(vector-set! stack (- sp 1) (vector-ref stack sp))
		(set! sp (- sp 1)))
	       ((UNASSIGNED)
		;; replace root:
		(vector-set! stack sp (make-unassigned-reference-trap)))
	       ((CELL)           (binary-operation cell-ref))
	       ((FLONUM-CELL)    (binary-operation flonum-cell-ref))
	       ((CONSTANT-BLOCK) (binary-operation constant-block-ref))
	       ((TOP-LEVEL-ENVIRONMENT)
		(unary-operation top-level-environment))
	       ((CLOSURE)	  (binary-operation closure-ref))
	       ((STACK)           (binary-operation stack-frame-ref))
	       ((INTERRUPT-FRAME) (binary-operation interrupt-frame-ref))
	       ((CC-ENTRY)  	  (binary-operation cc-block-entry))
	       ((UNCOERCE)        (unary-operation uncoerce-procedure))

	       ((ROOT)            (push root))
	       (else (path-error "Unknown path expression"))))))

    (define (loop path i end)
      (if (< i end)
	  (begin
	    (dispatch (vector-ref path i))
	    (loop path (+ i 1) end))))

    (if initial-path
	(begin
	  (if (vector? initial-path)
	      (loop initial-path 0 (- (vector-length initial-path)
				      (if leave-last-instruction? 1 0)))
	      (if leave-last-instruction?
		  'done
		  (dispatch initial-path)))
	  (if (not (= sp 0))
	      (error "Path did not evaluate to a single result!"
		     initial-path sp stack))
	  (map-reference-trap
	   (lambda ()
	     (vector-ref stack 0))))
	unavailable-object)))

(define (path/last-element path)
  (cond ((pair? path) path)
	((vector? path) (vector-ref path (- (vector-length path) 1)))
	(else #F)))

(define (assignable-path? path)
  (define (cell-op? thing)
    (and (pair? thing) (memq (car thing) '(CELL FLONUM-CELL))))
  (cell-op? (path/last-element path)))

(define (interrupt-frame-path? path)
  ;; Does the path start from an interrupt frame?
  (define (frame-op? thing)
    (and (pair? thing) (eq? (car thing) 'INTERRUPT-FRAME)))
  (cond ((vector? path)
	 (and (not (zero? (vector-length path)))
	      (frame-op? (vector-ref path 0))))
	((pair? path)
	 (frame-op? path))
	(else  #F)))

(define (assign-path! path root name value)
  (let* ((place   (lookup-path path root #T))
	 (element (path/last-element path)))
    (cond ((and (pair? element) (eq? (car element) 'CELL))
	   (let ((index  (cdr element)))
	     (cond ((and (cell? place) (zero? index))
		    (set-cell-contents! place value))
		   ((vector? place)
		    (vector-set! place index value))
		   (else (error "Value of variable should be in cell/vector"
				name place path))))
	   unspecific)
	  ((and (pair? element) (eq? (car element) 'FLONUM-CELL))
	   (let ((index  (cdr element)))
	     (cond ((not (flo:flonum? value))
		    (error "Cant assign" name
			   (error-irritant/noise ". Value must be a flonum ")
			   value))
		   (else
		    (flo:vector-set! place index value))))
	   unspecific)
	  (else
	   (error "Unassignable variable:" name)))))


(define (dbg-block/name block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/name procedure))))

(define (dbg-block/source-code block)
  (let ((procedure (dbg-block/procedure block)))
    (and procedure
	 (dbg-procedure/source-code procedure))))

#|
Path expressions.

A path is either (1) #F, indicating that the value is not available,
(2) a single path item, or (3) a vector of path items.

The evaluation model is that the path items are reverse-polish
operations.  The stack initially contains the ROOT value (typically a
stack frame or compiled-closure).  The operations are processed in
order to produce a single value (returning ofther than 1 value is an
evaluation error).

Path items are simple or `compound'.  A compound item is a pair
comprising a simple item and a literal.  The literal (any scheme
object) is pushed on the stack before evaluating the simple item.
Simple items are primitive procedures, which are called with the right
number of items from the top of the stack (The top two elements are
called TOS & 2ND below), and special operations, which are encoded as
symbols.

The special items, in their usual syntax (simple or compound) are
described briefly:

(INTEGRATED . object)
  Replace TOS with OBJECT

UNASSIGNED
  Replace TOS with an unassigned reference trap.

(CELL . offset)
  TOS is a cell (a cell or a vector).  The value is within the cell at
  OFFSET.  This path describes a location which is used for reading or
  assignment.

(CONSTANT-BLOCK . offset)
  Find the compiled code block for TOS and index into it.  This is used
  instead of INTEGRATED for constants that are available from the
  constants block (rather than a non-EQ? version).

TOP-LEVEL-ENVIRONMENT
  Find the compiled code block for TOS and retrun its environment.

(CLOSURE . offset)
  TOS is a compiled closure.  Replace with its component.

(STACK . offset)
  TOS must be a stack frame.  Replace the element indexed from the base.

(INTERUPT-FRAME . offset)
  TOS must be an interrupt stack frame.  Replace with index from the
  start.

(CC-ENTRY . byte-offset)
  Find the compiled code block for TOS and replace with the compiled
  entry at that offset from the compiled code block.

UNCOERCE
  Undo effect of COERCE-TO-COMPILED-PROCEURE

ROOT
  Push the original ROOT to start a new subexpression.

Example 1

   UNASSIGNED    - the variable is unassigned

Example 2

   #((STACK . 3) (CLOSURE . 3) VECTOR-LENGTH
     ROOT (INTEGRATED . 1)
     MINUS-FIXNUM)

 The expression `(fix:- (vector-length foo) 1)' where FOO is a closed
 variable and the closure is available from the stack-frame.
 Note: this could have been optimized to

   #((STACK . 3) (CLOSURE . 3) VECTOR-LENGTH (MINUS-FIXNUM . 1))

 but this kind of expression is sufficiently rare that the space
 savings are not worth the effort in writing the code.

|#
