#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/lvalue.scm,v 1.1 1987/06/17 02:16:09 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Compiler DFG Datatypes: Variable Nodes

(declare (usual-integrations))

(define-vnode variable block name assigned? in-cell? normal-offset)

(define (make-variable block name)
  (make-vnode variable-tag block name false false false))

(define variable-assoc
  (association-procedure eq? variable-name))

(define (variable-offset block variable)
  (if (closure-block? block)
      (cdr (assq variable (block-closure-offsets block)))
      (variable-normal-offset variable)))

(define-unparser variable-tag
  (lambda (variable)
    (write-string "VARIABLE ")
    (write (variable-name variable))))

(define-vnode access environment name)

(define (make-access environment name)
  (make-vnode access-tag environment name))

(define-vnode temporary type conflicts allocation)

(define (make-temporary)
  (make-vnode temporary-tag false '() false))

(define-vnode value-register)

(define (make-value-register)
  (make-vnode value-register-tag))

(define-vnode value-ignore)

(define (make-value-ignore)
  (make-vnode value-ignore-tag))