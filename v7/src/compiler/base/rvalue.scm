#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/rvalue.scm,v 4.3 1988/06/14 08:33:23 cph Exp $

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

;;;; Right (Hand Side) Values

(declare (usual-integrations))

(define-root-type rvalue
  %passed-out?)

;;; converted to a macro.
;;; (define (make-rvalue tag . extra)
;;;   (list->vector (cons* tag false extra)))

(define-enumeration rvalue-type
  (block
   constant
   expression
   procedure
   reference
   unassigned-test))

(define (rvalue-values rvalue)
  (if (rvalue/reference? rvalue)
      (reference-values rvalue)
      (list rvalue)))

(define (rvalue-passed-in? rvalue)
  (and (rvalue/reference? rvalue)
       (reference-passed-in? rvalue)))

(define (rvalue-passed-out? rvalue)
  (if (rvalue/reference? rvalue)
      (reference-passed-out? rvalue)
      (rvalue-%passed-out? rvalue)))

(define (rvalue-known-value rvalue)
  (if (rvalue/reference? rvalue)
      (reference-known-value rvalue)
      rvalue))

(define (rvalue-known-constant? rvalue)
  (let ((value (rvalue-known-value rvalue)))
    (and value
	 (rvalue/constant? value))))

(define (rvalue-constant-value rvalue)
  (constant-value (rvalue-known-value rvalue)))

(define (rvalue=? rvalue rvalue*)
  (if (rvalue/reference? rvalue)
      (if (rvalue/reference? rvalue*)
	  (lvalue=? (reference-lvalue rvalue) (reference-lvalue rvalue*))
	  (eq? (lvalue-known-value (reference-lvalue rvalue)) rvalue*))
      (if (rvalue/reference? rvalue*)
	  (eq? rvalue (lvalue-known-value (reference-lvalue rvalue*)))
	  (eq? rvalue rvalue*))))

;;;; Constant

(define-rvalue constant
  value)

(define *constants*)

(define (make-constant value)
  (let ((entry (assv value *constants*)))
    (if entry
	(cdr entry)
	(let ((constant (make-rvalue constant-tag value)))
	  (set! *constants* (cons (cons value constant) *constants*))
	  constant))))

(define-vector-tag-unparser constant-tag
  (standard-unparser "CONSTANT"
    (lambda (state constant)
      (unparse-object state (constant-value constant)))))

(define-integrable (rvalue/constant? rvalue)
  (eq? (tagged-vector/tag rvalue) constant-tag))

;;;; Reference

(define-rvalue reference
  block
  lvalue
  safe?)

(define (make-reference block lvalue safe?)
  (make-rvalue reference-tag block lvalue safe?))

(define-vector-tag-unparser reference-tag
  (standard-unparser "REFERENCE"
    (lambda (state reference)
      (unparse-object state (variable-name (reference-lvalue reference))))))

(define-integrable (rvalue/reference? rvalue)
  (eq? (tagged-vector/tag rvalue) reference-tag))

(define-integrable (reference-values reference)
  (lvalue-values (reference-lvalue reference)))

(define-integrable (reference-passed-in? reference)
  (lvalue-passed-in? (reference-lvalue reference)))

(define-integrable (reference-passed-out? reference)
  (lvalue-passed-out? (reference-lvalue reference)))

(define-integrable (reference-known-value reference)
  (lvalue-known-value (reference-lvalue reference)))

(define (reference-to-known-location? reference)
  (variable-in-known-location? (reference-block reference)
			       (reference-lvalue reference)))

;;; This type is only important while we use the `unassigned?' special
;;; form to perform optional argument defaulting.  When we switch over
;;; to the new optional argument proposal we can flush this since the
;;; efficiency of this construct won't matter anymore.

(define-rvalue unassigned-test
  block
  lvalue)

(define (make-unassigned-test block lvalue)
  (make-rvalue unassigned-test-tag block lvalue))

(define-vector-tag-unparser unassigned-test-tag
  (standard-unparser "UNASSIGNED-TEST"    (lambda (state unassigned-test)
      (unparse-object state (unassigned-test-lvalue unassigned-test)))))

(define-integrable (rvalue/unassigned-test? rvalue)
  (eq? (tagged-vector/tag rvalue) unassigned-test-tag))

;;;; Expression

(define-rvalue expression
  block
  continuation
  entry-edge
  label)

(define *expressions*)

(define (make-expression block continuation scfg)
  (let ((expression
	 (make-rvalue expression-tag block continuation
		      (node->edge (cfg-entry-node scfg))
		      (generate-label 'EXPRESSION))))
    (set! *expressions* (cons expression *expressions*))
    (set-block-procedure! block expression)
    expression))

(define-integrable (rvalue/expression? rvalue)
  (eq? (tagged-vector/tag rvalue) expression-tag))

(define-integrable (expression-entry-node expression)
  (edge-next-node (expression-entry-edge expression)))