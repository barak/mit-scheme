#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlcfg.scm,v 1.2 1987/05/07 00:10:04 cph Exp $

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

;;;; RTL CFG Nodes

(declare (usual-integrations))

;;; Hack to make RNODE-RTL, etc, work on both types of node.

(define-snode rtl-snode)
(define-pnode rtl-pnode)
(define-vector-slots rnode 7 rtl dead-registers logical-link register-map lap
  frame-pointer-offset)
(define-vector-slots rtl-pnode 13 consequent-lap-generator
  alternative-lap-generator)

(define (statement->snode statement)
  (make-pnode rtl-snode-tag statement '() false false false false))

(define-integrable (statement->scfg statement)
  (snode->scfg (statement->snode statement)))

(define (predicate->pnode predicate)
  (make-pnode rtl-pnode-tag predicate '() false false false false false false))

(define-integrable (predicate->pcfg predicate)
  (pnode->pcfg (predicate->pnode predicate)))

(define-integrable (rnode-dead-register? rnode register)
  (memv register (rnode-dead-registers rnode)))

(let ((rnode-describe
       (lambda (rnode)
	 `((RNODE-RTL ,(rnode-rtl rnode))
	   (RNODE-DEAD-REGISTERS ,(rnode-dead-registers rnode))
	   (RNODE-LOGICAL-LINK ,(rnode-logical-link rnode))
	   (RNODE-REGISTER-MAP ,(rnode-register-map rnode))
	   (RNODE-LAP ,(rnode-lap rnode))
	   (RNODE-FRAME-POINTER-OFFSET ,(rnode-frame-pointer-offset rnode))))))

  (define-vector-method rtl-snode-tag ':DESCRIBE
    (lambda (snode)
      (append! ((vector-tag-method snode-tag ':DESCRIBE) snode)
	       (rnode-describe snode))))

  (define-vector-method rtl-pnode-tag ':DESCRIBE
    (lambda (pnode)
      (append! ((vector-tag-method pnode-tag ':DESCRIBE) pnode)
	       (rnode-describe pnode)
	       `((RTL-PNODE-CONSEQUENT-LAP-GENERATOR
		  ,(rtl-pnode-consequent-lap-generator pnode))
		 (RTL-PNODE-ALTERNATIVE-LAP-GENERATOR
		  ,(rtl-pnode-alternative-lap-generator pnode)))))))