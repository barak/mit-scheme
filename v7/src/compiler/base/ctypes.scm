;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Compiler CFG Datatypes

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/ctypes.scm,v 1.37 1986/12/20 22:51:33 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define-snode assignment block lvalue rvalue)

(define (make-assignment block lvalue rvalue)
  (vnode-connect! lvalue rvalue)
  (snode->scfg (make-snode assignment-tag block lvalue rvalue)))

(define-snode definition block lvalue rvalue)

(define (make-definition block lvalue rvalue)
  (vnode-connect! lvalue rvalue)
  (snode->scfg (make-snode definition-tag block lvalue rvalue)))

(define-pnode true-test rvalue)

(define-integrable (make-true-test rvalue)
  (pnode->pcfg (make-pnode true-test-tag rvalue)))

(define-pnode unassigned-test block variable)

(define-integrable (make-unassigned-test block variable)
  (pnode->pcfg (make-pnode unassigned-test-tag block variable)))

(define-pnode unbound-test block variable)

(define-integrable (make-unbound-test block variable)
  (pnode->pcfg (make-pnode unbound-test-tag block variable)))

(define-snode combination block compilation-type value operator operands
  procedures known-operator)
(define *combinations*)

(define (make-combination block compilation-type value operator operands)
  (let ((combination
	 (make-snode combination-tag block compilation-type value operator
		     operands '() false)))
    (set! *combinations* (cons combination *combinations*))
    (set-block-combinations! block
			     (cons combination (block-combinations block)))
    (set-vnode-combinations! value
			     (cons combination (vnode-combinations value)))
    (snode->scfg combination)))

(define-snode continuation rtl delta label)
(define *continuations*)

(define-integrable (make-continuation rtl delta)
  (let ((continuation
	 (make-snode continuation-tag rtl delta
		     (generate-label 'CONTINUATION))))
    (set! *continuations* (cons continuation *continuations*))
    continuation))

(define-unparser continuation-tag
  (lambda (continuation)
    (write (continuation-label continuation))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  (symbol-hash-table/lookup *label->object* label))