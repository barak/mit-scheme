#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/rvalue.scm,v 1.4 1987/08/04 06:54:16 cph Exp $

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

;;;; Compiler DFG Datatypes: Right (Hand Side) Values

(declare (usual-integrations))

(define-rvalue constant value)
(define *constants*)

(define (make-constant value)
  (let ((entry (assv value *constants*)))
    (if entry
	(cdr entry)
	(let ((constant (make-rvalue constant-tag value)))
	  (set! *constants* (cons (cons value constant) *constants*))
	  constant))))

(define-unparser constant-tag
  (lambda (constant)
    (write-string "CONSTANT ")
    (write (constant-value constant))))

(define-rvalue block parent children bound-variables free-variables procedure
  declarations type closures combinations interned-variables closure-offsets frame)
(define *blocks*)

(define (make-block parent)
  (let ((block
	 (make-rvalue block-tag parent '() '() '() false
		      '() 'STACK '() '() '() '() false)))
    (if parent
	(set-block-children! parent (cons block (block-children parent))))
    (set! *blocks* (cons block *blocks*))
    block))

(define-unparser block-tag
  (lambda (block)
    (write-string "BLOCK")
    (let ((procedure (block-procedure block)))
      (if procedure
	  (begin (write-string " ")
		 (write (procedure-label procedure)))))))

(define-rvalue reference block variable safe?)

(define (make-reference block variable)
  (make-rvalue reference-tag block variable false))

(define (make-safe-reference block variable)
  (make-rvalue reference-tag block variable true))

(define-unparser reference-tag
  (lambda (reference)
    (write-string "REFERENCE ")
    (write (variable-name (reference-variable reference)))))

(define-rvalue procedure block value fg-edge rgraph externally-visible?
  closure-block label external-label name required optional rest
  names values auxiliary original-parameters)
(define *procedures*)

(define (make-procedure block subproblem name required optional rest
			names values auxiliary)
  (let ((procedure
	 (make-rvalue procedure-tag block (subproblem-value subproblem)
		      (cfg-entry-edge (subproblem-cfg subproblem))
		      (rgraph-allocate) false false
		      (generate-label (variable-name name))
		      (generate-label) name required optional rest
		      names values auxiliary (vector required optional rest))))
    (set-block-procedure! block procedure)
    (vnode-connect! name procedure)
    (set! *procedures* (cons procedure *procedures*))
    (symbol-hash-table/insert! *label->object*
			       (procedure-label procedure)
			       procedure)
    procedure))

(define-integrable (procedure-fg-entry procedure)
  (edge-right-node (procedure-fg-edge procedure)))

(define-integrable (unset-procedure-fg-entry! procedure)
  (set-procedure-fg-edge! procedure false))

(define-integrable (procedure-original-required procedure)
  (vector-ref (procedure-original-parameters procedure) 0))

(define-integrable (procedure-original-optional procedure)
  (vector-ref (procedure-original-parameters procedure) 1))

(define-integrable (procedure-original-rest procedure)
  (vector-ref (procedure-original-parameters procedure) 2))

(define-unparser procedure-tag
  (lambda (procedure)
    (write-string "PROCEDURE ")
    (write (procedure-label procedure))))

(define-integrable (label->procedure label)
  (symbol-hash-table/lookup *label->object* label))

(define-rvalue quotation block value fg-edge rgraph label)
(define *quotations*)

(define (make-quotation block subproblem)
  (let ((quotation
	 (make-rvalue quotation-tag block (subproblem-value subproblem)
		      (cfg-entry-edge (subproblem-cfg subproblem))
		      (rgraph-allocate)
		      (generate-label 'QUOTATION))))
    (set! *quotations* (cons quotation *quotations*))
    quotation))

(define-integrable (quotation-fg-entry quotation)
  (edge-right-node (quotation-fg-edge quotation)))

(define-integrable (unset-quotation-fg-entry! quotation)
  (set-quotation-fg-edge! quotation false))

(define-vector-slots rgraph 0
  edge
  n-registers
  continuations
  bblocks
  register-bblock
  register-next-use
  register-n-refs
  register-n-deaths
  register-live-length
  register-crosses-call?
  )

(define-integrable rgraph-register-renumber rgraph-register-bblock)
(define-integrable set-rgraph-register-renumber! set-rgraph-register-bblock!)

(define *rgraphs*)
(define *current-rgraph*)

(define (rgraph-allocate)
  (make-vector 10 false))

(define (rgraph-entry-edges rgraph)
  (cons (rgraph-edge rgraph)
	(map continuation-rtl-edge (rgraph-continuations rgraph))))

(define (rgraph-initial-edges rgraph)
  (cons (rgraph-edge rgraph)
	(let loop ((continuations (rgraph-continuations rgraph)))
	  (if (null? continuations)
	      '()
	      (let ((edge (continuation-rtl-edge (car continuations))))
		(if (node-previous=0? (edge-right-node edge))
		    (cons edge (loop (cdr continuations)))
		    (loop (cdr continuations))))))))