#| -*-Scheme-*-

$Id: rvalue.scm,v 4.12 2007/01/05 21:19:20 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Right (Hand Side) Values
;;; package: (compiler)

(declare (usual-integrations))

(define-root-type rvalue
  %passed-out?)

;;; converted to a macro.
;;; (define (make-rvalue tag . extra)
;;;   (list->vector (cons* tag #f extra)))

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
  (standard-unparser (symbol->string 'CONSTANT)
    (lambda (state constant)
      (unparse-object state (constant-value constant)))))

(define-integrable (rvalue/constant? rvalue)
  (eq? (tagged-vector/tag rvalue) constant-tag))

;;;; Reference

(define-rvalue reference
  context
  lvalue
  safe?)

(define (make-reference block lvalue safe?)
  (make-rvalue reference-tag block lvalue safe?))

(define-vector-tag-unparser reference-tag
  (standard-unparser (symbol->string 'REFERENCE)
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
  (variable-in-known-location? (reference-context reference)
			       (reference-lvalue reference)))

;;; This type is only important while we use the `unassigned?' special
;;; form to perform optional argument defaulting.  When we switch over
;;; to the new optional argument proposal we can flush this since the
;;; efficiency of this construct won't matter anymore.

(define-rvalue unassigned-test
  context
  lvalue)

(define (make-unassigned-test block lvalue)
  (make-rvalue unassigned-test-tag block lvalue))

(define-vector-tag-unparser unassigned-test-tag
  (standard-unparser (symbol->string 'UNASSIGNED-TEST)
    (lambda (state unassigned-test)
      (unparse-object state (unassigned-test-lvalue unassigned-test)))))

(define-integrable (rvalue/unassigned-test? rvalue)
  (eq? (tagged-vector/tag rvalue) unassigned-test-tag))

;;;; Expression

(define-rvalue expression
  block
  continuation
  entry-edge
  label
  debugging-info)

(define *expressions*)

(define (make-expression block continuation scfg)
  (let ((expression
	 (make-rvalue expression-tag block continuation
		      (node->edge (cfg-entry-node scfg))
		      (generate-label 'EXPRESSION) #f)))
    (set! *expressions* (cons expression *expressions*))
    (set-block-procedure! block expression)
    expression))

(define-integrable (rvalue/expression? rvalue)
  (eq? (tagged-vector/tag rvalue) expression-tag))

(define-integrable (expression-entry-node expression)
  (edge-next-node (expression-entry-edge expression)))