#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/uenvir.scm,v 14.10 1989/08/03 23:01:31 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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
      (lexical-unbound? environment symbol))))

(define-integrable (ic-environment? object)
  (object-type? (ucode-type environment) object))

(define (guarantee-ic-environment object)
  (if (not (ic-environment? object))
      (error "Bad IC environment" object))
  object)

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
      (lexical-unbound? environment name))))

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
	 (begin (set-environment-extension-parent! extension parent)
		(environment-extension-procedure extension))
	 extension))
   parent))

(define (ic-environment/remove-parent! environment)
  (ic-environment/set-parent! environment null-environment))

(define null-environment
  (object-new-type (ucode-type null) 1))

(define (make-null-interpreter-environment)
  (let ((environment (the-environment)))
    (ic-environment/remove-parent! environment)
    environment))

(define (ic-environment->external environment)
  (let ((procedure (select-procedure environment)))
    (if (internal-lambda? (compound-procedure-lambda procedure))
	(compound-procedure-environment procedure)
	environment)))

(define-integrable (select-extension environment)
  (system-vector-ref environment 0))

(define (select-procedure environment)
  (let ((object (select-extension environment)))
    (if (environment-extension? object)
	(environment-extension-procedure object)
	object)))

(define (select-parent environment)
  (compound-procedure-environment (select-procedure environment)))

(define (select-lambda environment)
  (compound-procedure-lambda (select-procedure environment)))

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
  (let ((continuation
	 (compiled-entry/dbg-object (stack-frame/return-address frame))))
    (if continuation
	(let ((block (dbg-continuation/block continuation)))
	  (let ((parent (dbg-block/parent block)))
	    (case (dbg-block/type parent)
	      ((STACK)
	       (make-stack-ccenv parent
				 frame
				 (+ (dbg-continuation/offset continuation)
				    (vector-length (dbg-block/layout block)))))
	      ((IC)
	       (let ((index (dbg-block/ic-parent-index block)))
		 (if index
		     (guarantee-ic-environment (stack-frame/ref frame index))
		     default)))
	      (else
	       (error "Illegal continuation parent" parent)))))
	default)))