#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Procedure datatype
;;; package: (compiler)

(declare (usual-integrations))

(define-rvalue procedure
  type			;either PROCEDURE or a continuation type
  block			;model of invocation environment [block]
  name			;name of procedure [symbol]
  required		;list of required parameters [variables]
  optional		;list of optional parameters [variables]
  rest			;"rest" parameter, if any [variable or #f]
  names			;list of internal letrec names [variables]
  values		;list of internal letrec values [rvalues]
  entry-edge		;body of procedure [cfg edge]
  original-required	;like `required' but never changed
  original-optional	;like `optional' but never changed
  original-rest		;like `rest' but never changed
  label			;label to identify procedure entry point [symbol]
  applications		;list of applications for which this is an operator
  always-known-operator? ;always known operator of application? [boolean]
  closure-cons		;for closure, how it is to be consed.
  closure-context	;for closure, where procedure is closed [block]
  closure-offset	;for closure, offset of procedure in stack frame
  register		;for continuation, argument register
  closure-size		;for closure, virtual size of frame [integer or #f]
  target-block		;where procedure is "really" closed [block]
  initial-callees	;procs. invoked by me directly
  (free-callees		;procs. invoked by means of free variables (1)
   callees)		;procs. invoked by me (transitively)
  (free-callers		;procs. that invoke me by means of free variables (1)
   callers)		;procs. that invoke me (transitively)
  virtual-closure?	;need entry point but no environment? [boolean]
  closure-reasons	;reasons why a procedure is closed.
  (variables		;variables which may be bound to this procedure (1)
   side-effects)	;classes of side-effects performed by this procedure
  alist			;random bits of information [assq list]
  debugging-info	;[dbg-procedure or dbg-continuation]
  )

;; (1) The first meaning is used during closure analysis.
;;     The second meaning is used during side-effect analysis.

(define *procedures*)

(define (make-procedure type block name required optional rest names values
			scfg)
  (map lvalue-connect! names values)
  (let ((procedure
	 (make-rvalue procedure-tag
		      type block name required optional rest names values
		      (node->edge (cfg-entry-node scfg))
		      (list-copy required)
		      (list-copy optional)
		      (if (eq? type continuation-type/procedure)
			  rest
			  '())		;initial continuation/combinations
		      (generate-label name)
		      '()		;applications
		      #f		;always-known-operator?
		      #f		;closure-cons
		      #f		;closure-context
		      #f		;closure-offset
		      #f		;register
		      #f		;closure-size
		      #f		;target-block
		      '()               ;initial-callees
		      '()		;[free-]callees
		      '()		;[free-]callers
		      #f		;virtual-closure?
		      '()		;closure-reasons
		      '()		;variables or side-effects
		      '()		;alist
		      #f		;debugging-info
		      )))
    (set! *procedures* (cons procedure *procedures*))
    (set-block-procedure! block procedure)
    procedure))

(define-vector-tag-unparser procedure-tag
  (lambda (state procedure)
    ((let ((type
	    (enumeration/index->name continuation-types
				     (procedure-type procedure))))
       (if (eq? type 'PROCEDURE)
	   (standard-unparser (symbol->string 'PROCEDURE)
	     (lambda (state procedure)
	       (unparse-label state (procedure-label procedure))))
	   (standard-unparser (symbol->string (procedure-label procedure))
	     (lambda (state procedure)
	       procedure
	       (unparse-object state type)))))
     state procedure)))

(define-integrable (unparse-label state label)
  (unparse-string state (symbol->string label)))

(define-integrable (rvalue/procedure? rvalue)
  (eq? (tagged-vector/tag rvalue) procedure-tag))

(define (rvalue/true-procedure? rvalue)
  (and (rvalue/procedure? rvalue)
       (not (procedure-continuation? rvalue))))

(define (procedure-arity-correct? procedure argument-count)
  (let ((number-required (length (procedure-required procedure))))
    (and (>= argument-count number-required)
	 (if (procedure-rest procedure)
	     #t
	     (<= argument-count
		 (+ number-required
		    (length (procedure-optional procedure))))))))

(define (procedure-arity-encoding procedure)
  (let* ((min (1+ (length (procedure-required-arguments procedure))))
	 (max (+ min (length (procedure-optional procedure)))))
    (values min (if (procedure-rest procedure) (- (1+ max)) max))))

(define-integrable (procedure-closing-block procedure)
  (block-parent (procedure-block procedure)))

(define-integrable (procedure-continuation-lvalue procedure)
  ;; Valid only if (not (procedure-continuation? procedure))
  (car (procedure-required procedure)))

(define-integrable (procedure-required-arguments procedure)
  ;; Valid only if (not (procedure-continuation? procedure))
  (cdr (procedure-required procedure)))

(define-integrable (procedure-entry-node procedure)
  (edge-next-node (procedure-entry-edge procedure)))

(define (set-procedure-entry-node! procedure node)
  (let ((edge (procedure-entry-edge procedure)))
    (edge-disconnect-right! edge)
    (edge-connect-right! edge node)))

(define-integrable procedure-passed-out?
  rvalue-%passed-out?)

(define-integrable set-procedure-passed-out?!
  set-rvalue-%passed-out?!)

(define-integrable (closure-procedure-needs-operator? procedure)
  ;; This must be true if the closure needs its parent frame since the
  ;; parent frame is found from the operator.  Currently only avoided
  ;; for trivial closures.
  (not (procedure/trivial-closure? procedure)))

(define-integrable (procedure-application-unique? procedure)
  (null? (cdr (procedure-applications procedure))))

(define (delete-procedure-application! procedure application)
  (let ((applications (delq! application (procedure-applications procedure))))
    (set-procedure-applications! procedure applications)
    (if (null? applications)
	(set-procedure-always-known-operator?! procedure #f))))

(define (procedure-get procedure key)
  (let ((entry (assq key (procedure-alist procedure))))
    (and entry
	 (cdr entry))))

(define (procedure-put! procedure key item)
  (let ((entry (assq key (procedure-alist procedure))))
    (if entry
	(set-cdr! entry item)
	(set-procedure-alist! procedure
			      (cons (cons key item)
				    (procedure-alist procedure))))))

(define (procedure-remove! procedure key)
  (set-procedure-alist! procedure (del-assq! key (procedure-alist procedure))))

(define-integrable (procedure/simplified? procedure)
  (procedure-get procedure 'SIMPLIFIED))

(define-integrable (procedure/trivial? procedure)
  (procedure-get procedure 'TRIVIAL))

(define (procedure-inline-code? procedure)
  (and (not (procedure-rest procedure))
       (or (procedure/trivial? procedure)
	   (and (procedure-always-known-operator? procedure)
		(procedure-application-unique? procedure)
		(procedure/virtually-open? procedure)))))

(define-integrable (open-procedure-needs-static-link? procedure)
  (stack-block/static-link? (procedure-block procedure)))

(define-integrable (open-procedure-needs-dynamic-link? procedure)
  (stack-block/dynamic-link? (procedure-block procedure)))

;;;; Procedure Types

;;; IC ("interpreter compatible") procedures are closed procedures
;;; whose environment frames are compatible with those generated by
;;; the interpreter.  Both the procedure's frame and all of its
;;; ancestors are interpreter compatible.

;;; CLOSURE procedures are closed procedures whose frame is a stack
;;; frame.  The parent frame of such a procedure may be null, an IC
;;; frame, or a CLOSURE frame (which is a compiler generated, heap
;;; allocated frame).

;;; OPEN-EXTERNAL procedures are open procedures whose frame is a
;;; stack frame, and whose parent frame is either null, or an IC
;;; frame.  These are treated similarly to CLOSURE procedures except
;;; that the stack frame is laid out differently.

;;; OPEN-INTERNAL procedures are open procedures whose frame and
;;; parent are both stack frames.  The parent frame of such a
;;; procedure is created by either a closure or open-external
;;; procedure.

(define (procedure/type procedure)
  (let ((block (procedure-block procedure)))
    (enumeration-case block-type (block-type block)
      ((STACK)
       (if (procedure-closure-context procedure)
	   (if (procedure/trivial-closure? procedure)
	       'TRIVIAL-CLOSURE
	       'CLOSURE)
	   (if (stack-parent? block)
	       'OPEN-INTERNAL
	       'OPEN-EXTERNAL)))
      ((IC) 'IC)
      ((CLOSURE) (error "Illegal occurrence of CLOSURE block" procedure))
      (else (error "Unknown block type" block)))))

(define-integrable (procedure/ic? procedure)
  (ic-block? (procedure-block procedure)))

(define (procedure/closure? procedure)
  (and (procedure/closed? procedure)
       (not (procedure/ic? procedure))))

(define (procedure/trivial-closure? procedure)
  (let ((enclosing (procedure-closing-block procedure)))
    (or (not enclosing)
	(and (ic-block? enclosing)
	     (not (ic-block/use-lookup? enclosing))))))

(define-integrable procedure/closed?
  procedure-closure-context)

(define-integrable (procedure/open? procedure)
  (not (procedure/closed? procedure)))

(define-integrable (procedure/external? procedure)
  (block/external? (procedure-block procedure)))

(define-integrable (procedure/internal? procedure)
  (block/internal? (procedure-block procedure)))

(define (procedure/open-external? procedure)
  (and (procedure/open? procedure)
       (procedure/external? procedure)))

(define (procedure/open-internal? procedure)
  (and (procedure/open? procedure)
       (procedure/internal? procedure)))

(define (procedure/virtually-open? procedure)
  (or (procedure/open? procedure)
      (and (procedure/closure? procedure)
	   (procedure/trivial-closure? procedure))))

(define (procedure/trivial-or-virtual? procedure)
  (or (procedure-virtual-closure? procedure)
      (and (procedure/closure? procedure)
	   (procedure/trivial-closure? procedure))))

;;;; Closure reasons

;; The possible reasons are
;;
;; PASSED-OUT: Procedure is available from outside block (usually an
;; upwards funarg).
;;
;; ARGUMENT: Procedure is given as an argument to a procedure does not
;; share its lexical chain.  Some of these cases of downward funargs
;; could be stack allocated.
;;
;; ASSIGNMENT: Procedure is assigned to some variable outside its
;; closing block.
;;
;; CONTAGION: Procedure is called by some other closure.
;;
;; COMPATIBILITY: Procedure is called from a location which may have
;; more than one operator, but the complete set of possibilities is
;; known and they are compatible closures.
;;
;; APPLY-COMPATIBILITY: Procedure is called from a location which may
;; have more than one operator, but the complete set of possibilities
;; is now known or they are incompatible, so (internal) apply has to
;; be used.

(define (add-closure-reason! procedure keyword argument)
  (let ((entries (procedure-closure-reasons procedure)))
    (let ((entry (assq keyword entries)))
      (if entry
	  (if (and argument (not (memq argument (cdr entry))))
	      (set-cdr! entry (cons argument (cdr entry))))
	  (set-procedure-closure-reasons! procedure
					  (cons (cons keyword
						      (if argument
							  (list argument)
							  '()))
						entries))))))

(define (closure-procedure-needs-external-descriptor? procedure)
  (let loop ((reasons (procedure-closure-reasons procedure)))
    (and (pair? reasons)
	 (or (memq (caar reasons)
		   '(PASSED-OUT ARGUMENT ASSIGNMENT
				COMPATIBILITY APPLY-COMPATIBILITY))
	     (loop (cdr reasons))))))

(define (procedure-maybe-registerizable? procedure)
  ;; Yields true if the procedure might be able to have some of its
  ;; parameters in registers.  Note: This does not mean that the
  ;; procedure WILL have its parameters in registers, or that ALL its
  ;; parameters will be in registers.  Which parameters will actually
  ;; be in registers depends on the procedure's argument subproblems,
  ;; as well as the parameter lvalues themselves.
  (and (procedure-always-known-operator? procedure)
       (procedure-application-unique? procedure)
       (procedure/virtually-open? procedure)
       (not (block-layout-frozen? (procedure-block procedure)))))
