#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/ctypes.scm,v 4.1 1987/12/04 20:03:40 cph Exp $

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

;;;; Compiler CFG Datatypes

(declare (usual-integrations))

;;;; Application

(define-snode application
  type
  block
  operator
  operands
  (parallel-node owner)
  (operators		;used in simulate-application
   arguments)		;used in outer-analysis
  operand-values	;set by outer-analysis, used by identify-closure-limits
  )

(define *applications*)

(define (make-application type block operator operands)
  (let ((application
	 (make-snode application-tag
		     type block operator operands false '() '())))
    (set! *applications* (cons application *applications*))
    (add-block-application! block application)
    (if (rvalue/reference? operator)
	(add-lvalue-application! (reference-lvalue operator) application))
    (make-scfg application '())))

(define-vector-tag-unparser application-tag
  (lambda (application)
    (let ((type (application-type application)))
      (cond ((eq? type 'COMBINATION)
	     (write-string "COMBINATION"))
	    ((eq? type 'RETURN)
	     (write-string "RETURN ")
	     (write (return/operand application)))
	    (else
	     (write-string "APPLICATION ")
	     (write type))))))

(define-snode parallel
  application-node
  subproblems)

(define *parallels*)

(define (make-parallel application subproblems)
  (let ((parallel (make-snode parallel-tag false subproblems)))
    (set-parallel-application-node! parallel application)
    (set-application-parallel-node! application parallel)
    (set! *parallels* (cons parallel *parallels*))
    (snode->scfg parallel)))

(define (make-combination block continuation operator operands)
  (let ((application
	 (make-application 'COMBINATION
			   block
			   (subproblem-rvalue operator)
			   (cons continuation
				 (map subproblem-rvalue operands)))))
    (scfg*scfg->scfg!
     (make-parallel (cfg-entry-node application) (cons operator operands))
     application)))

(define-integrable (application/combination? application)
  (eq? (application-type application) 'COMBINATION))

(define-integrable combination/block application-block)
(define-integrable combination/operator application-operator)
(define-integrable combination/inliner application-arguments)
(define-integrable set-combination/inliner! set-application-arguments!)
(define-integrable combination/frame-size application-operand-values)
(define-integrable set-combination/frame-size! set-application-operand-values!)
(define-integrable combination/inline? combination/inliner)

(define-integrable (combination/continuation combination)
  (car (application-operands combination)))

(define-integrable (combination/operands combination)
  (cdr (application-operands combination)))

(define-structure (inliner (type vector) (conc-name inliner/))
  (handler false read-only true)
  (generator false read-only true)
  operands)

;;; This method of handling constant combinations has the feature that
;;; such combinations are handled exactly like RETURNs by the
;;; procedure classification phase, which occurs after all constant
;;; combinations have been identified.

(define (combination/constant! combination rvalue)
  (let ((continuation (combination/continuation combination)))
    (set-application-type! combination 'RETURN)
    (set-application-operator! combination continuation)
    (set-application-operands! combination (list rvalue))))

(define-integrable (make-return block continuation rvalue)
  (make-application 'RETURN block continuation (list rvalue)))

(define-integrable (application/return? application)
  (eq? (application-type application) 'RETURN))

(define-integrable return/block
  application-block)

(define-integrable return/operator
  application-operator)

(define-integrable (return/operand return)
  (car (application-operands return)))

;;;; Miscellaneous Node Types

(define-snode assignment
  block
  lvalue
  rvalue)

(define *assignments*)

(define (make-assignment block lvalue rvalue)
  (lvalue-connect! lvalue rvalue)
  (let ((assignment (make-snode assignment-tag block lvalue rvalue)))
    (set! *assignments* (cons assignment *assignments*))
    (snode->scfg assignment)))

(define-snode definition
  block
  lvalue
  rvalue)

(define (make-definition block lvalue rvalue)
  (lvalue-connect! lvalue rvalue)
  (snode->scfg (make-snode definition-tag block lvalue rvalue)))

(define-pnode true-test
  rvalue)

(define (make-true-test rvalue)
  (pnode->pcfg (make-pnode true-test-tag rvalue)))

(define-snode fg-noop)

(define (make-fg-noop)
  (snode->scfg (make-snode fg-noop-tag)))

(define-snode virtual-return
  operator
  operand)

(define (make-virtual-return operator operand)
  (snode->scfg (make-snode virtual-return-tag operator operand)))

(define (make-push block rvalue)
  (make-virtual-return (virtual-continuation/make block continuation-type/push)
		       rvalue))

(define-snode pop
  continuation)

(define (make-pop continuation)
  (snode->scfg (make-snode pop-tag continuation)))