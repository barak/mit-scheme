#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/proced.scm,v 4.6 1988/11/01 04:48:30 jinx Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Procedure datatype

(declare (usual-integrations))

(define-rvalue procedure
  type			;either PROCEDURE or a continuation type
  block			;model of invocation environment [block]
  name			;name of procedure [symbol]
  required		;list of required parameters [variables]
  optional		;list of optional parameters [variables]
  rest			;"rest" parameter, if any [variable or false]
  names			;list of internal letrec names [variables]
  values		;list of internal letrec values [rvalues]
  entry-edge		;body of procedure [cfg edge]
  original-required	;like `required' but never changed
  original-optional	;like `optional' but never changed
  original-rest		;like `rest' but never changed
  label			;label to identify procedure entry point [symbol]
  applications		;list of applications for which this is an operator
  always-known-operator? ;always known operator of application? [boolean]
  closing-limit		;closing limit (see code)
  closure-block		;for closure, where procedure is closed [block]
  closure-offset	;for closure, offset of procedure in stack frame
  register		;for continuation, argument register
  closure-size		;for closure, virtual size of frame [integer or false]
  target-block		;where procedure is "really" closed [block]
  free-callees		;procedures invoked by means of free variables
  free-callers		;procedures that invoke me by means of free variables
  virtual-closure?	;need entry point but no environment? [boolean]
  closure-reasons	;reasons why a procedure is closed.
  side-effects		;classes of side-effects performed by this procedure
  trivial?		;true if body is trivial and should open code [boolean]
  )

(define *procedures*)

(define (make-procedure type block name required optional rest names values
			scfg)
  (map lvalue-connect! names values)
  (let ((procedure
	 (make-rvalue procedure-tag
		      type block name required optional rest names values
		      (node->edge (cfg-entry-node scfg))
		      (list-copy required) (list-copy optional) rest
		      (generate-label name) false false false false false
		      false false false false false false '() '() false)))
    (set! *procedures* (cons procedure *procedures*))
    (set-block-procedure! block procedure)
    procedure))

(define-vector-tag-unparser procedure-tag
  (lambda (state procedure)
    ((let ((type
	    (enumeration/index->name continuation-types
				     (procedure-type procedure))))
       (if (eq? type 'PROCEDURE)
	   (standard-unparser "PROCEDURE"
	     (lambda (state procedure)
	       (unparse-object state (procedure-label procedure))))
	   (standard-unparser (symbol->string (procedure-label procedure))
	     (lambda (state procedure)
	       procedure
	       (unparse-object state type)))))
     state procedure)))

(define-integrable (rvalue/procedure? rvalue)
  (eq? (tagged-vector/tag rvalue) procedure-tag))

(define (procedure-arity-correct? procedure argument-count)
  (let ((number-required (length (procedure-required procedure))))
    (and (>= argument-count number-required)
	 (if (procedure-rest procedure)
	     true
	     (<= argument-count
		 (+ number-required
		    (length (procedure-optional procedure))))))))

(define-integrable (procedure-closing-block procedure)
  (block-parent (procedure-block procedure)))

(define (set-procedure-closing-block! procedure block)
  (set-block-parent! (procedure-block procedure) block))

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

(define (close-procedure? procedure)
  (not (eq? (procedure-closing-limit procedure)
	    (procedure-closing-block procedure))))

(define-integrable (closure-procedure-needs-operator? procedure)
  ;; This must be true if the closure needs its parent frame since the
  ;; parent frame is found from the operator.  Currently only avoided
  ;; for trivial closures.
  (not (procedure/trivial-closure? procedure)))

(define (procedure-interface-optimizible? procedure)
  (and (stack-block? (procedure-block procedure))
       (procedure-always-known-operator? procedure)))

(define-integrable (procedure-application-unique? procedure)
  (null? (cdr (procedure-applications procedure))))

(define (procedure-inline-code? procedure)
  (or (procedure-trivial? procedure)
      (and (procedure-always-known-operator? procedure)
	   (procedure-application-unique? procedure)
	   (procedure/virtually-open? procedure))))

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
       (cond ((procedure-closure-block procedure) 'CLOSURE)
	     ((stack-parent? block) 'OPEN-INTERNAL)
	     (else 'OPEN-EXTERNAL)))
      ((IC) 'IC)
      ((CLOSURE) (error "Illegal occurrence of CLOSURE block" procedure))
      (else (error "Unknown block type" block)))))

(define-integrable (procedure/ic? procedure)
  (ic-block? (procedure-block procedure)))

(define-integrable (procedure/closure? procedure)
  (and (procedure-closure-block procedure)
       (not (procedure/ic? procedure))))

(define-integrable (procedure/trivial-closure? procedure)
  (let ((enclosing (procedure-closing-block procedure)))
    (or (null? enclosing)
	(and (ic-block? enclosing)
	     (not (ic-block/use-lookup? enclosing))))))

(define (procedure/closed? procedure)
  (or (procedure/ic? procedure)
      (procedure/closure? procedure)))

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

(define (add-closure-reason! procedure reason1 reason2)
  (let ((reasons (procedure-closure-reasons procedure)))
    (let ((slot (assq reason1 reasons)))
      (cond ((null? slot)
	     (set-procedure-closure-reasons!
	      procedure
	      (cons (cons reason1
			  (if (false? reason2)
			      '()
			      (list reason2)))
		    reasons)))
	    ((and (not (false? reason2))
		  (not (memq reason2 (cdr slot))))
	     (set-cdr! slot (cons reason2 (cdr slot))))))))

;; The possible reasons are
;;
;; - passed-out : procedure is available from outside block
;;   (usually an upwards funarg).
;;
;; - argument : procedure is given as an argument to a procedure does not
;;   share its lexical chain.  Some of these cases of downward funargs
;;   could be stack allocated.
;;
;; - assignment: procedure is assigned to some variable outside its closing
;;   block. 
;;
;; - contagion: procedure is called by some other closure.
;;
;; - compatibility: procedure is called from a location which may have more
;;   than one operator, but the complete set of possibilities is known and
;;   they are compatible closures.
;;
;; - apply-compatibility: procedure is called from a location which may have
;;   move than one operator, but the complete set of possibilities is now known
;;   or they are incompatible, so (internal) apply has to be used.

(define (closure-procedure-needs-external-descriptor? procedure)
  (let loop ((reasons (procedure-closure-reasons procedure)))
    (and (not (null? reasons))
	 (or (memq (caar reasons)
		   '(PASSED-OUT ARGUMENT ASSIGNMENT APPLY-COMPATIBILITY))
	     (loop (cdr reasons))))))