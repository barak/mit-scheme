#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/fgopt/outer.scm,v 4.1 1987/12/04 19:06:50 cph Exp $

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

;;;; Dataflow analysis: track values into or out of the graph

(declare (usual-integrations))

(package (outer-analysis)

(define-export (outer-analysis root-expression procedures applications)
  (transitive-closure
   (lambda ()
     ;; Sort of a kludge, we assume that the root expression is
     ;; evaluated in an IC block.  Maybe this isn't so.
     (block-passed-out! (expression-block root-expression))
     (lvalue-passed-in! (expression-continuation root-expression))
     (for-each (lambda (procedure)
		 ;; This is a kludge to handle the lack of a model for
		 ;; what really happens with rest parameters.
		 (if (procedure-rest procedure)
		     (lvalue-passed-in! (procedure-rest procedure))))
	       procedures)
     (for-each prepare-application applications))
   check-application
   applications))

(define (prepare-application application)
  (let ((values
	 (let ((operands (application-operands application)))
	   (if (null? operands)
	       '()
	       (eq-set-union* (rvalue-values (car operands))
			      (map rvalue-values (cdr operands)))))))
    (set-application-operand-values! application values)
    (set-application-arguments! application values))
  ;; Need more sophisticated test here so that particular primitive
  ;; operators only pass out specific operands.  A good test case is
  ;; `lexical-unassigned?' with a known block for its first argument
  ;; and a known symbol for its second.  Unfortunately, doing this
  ;; optimally introduces feedback in this analysis.
  (if (there-exists? (rvalue-values (application-operator application))
		     (lambda (value) (not (rvalue/procedure? value))))
      (application-arguments-passed-out! application)))

(define (check-application application)
  (if (rvalue-passed-in? (application-operator application))
      (application-arguments-passed-out! application))
#|
  ;; This looks like it isn't necessary, but I seem to recall that it
  ;; was needed to fix some bug.  If so, then there is a serious
  ;; problem, since we could "throw" into some operand other than
  ;; the continuation. -- CPH.
  (if (and (application/combination? application)
	   (there-exists? (combination/operands application)
			  rvalue-passed-in?))
      (for-each (lambda (value)
		  (if (uni-continuation? value)
		      (lvalue-passed-in! (uni-continuation/parameter value))))
		(rvalue-values (combination/continuation application))))
|#
  )

(define (application-arguments-passed-out! application)
  (let ((arguments (application-arguments application)))
    (set-application-arguments! application '())
    (for-each rvalue-passed-out! arguments)))

(define (rvalue-passed-out! rvalue)
  ((method-table-lookup passed-out-methods (tagged-vector/index rvalue))
   rvalue))

(define-integrable (%rvalue-passed-out! rvalue)
  (set-rvalue-%passed-out?! rvalue true))

(define passed-out-methods
  (make-method-table rvalue-types %rvalue-passed-out!))

(define-method-table-entry 'REFERENCE passed-out-methods
  (lambda (reference)
    (lvalue-passed-out! (reference-lvalue reference))))

(define-method-table-entry 'PROCEDURE passed-out-methods
  (lambda (procedure)
    (if (not (rvalue-%passed-out? procedure))
	(begin
	  (%rvalue-passed-out! procedure)
	  ;; The rest parameter was marked in the initialization.
	  (for-each lvalue-passed-in! (procedure-required procedure))
	  (for-each lvalue-passed-in! (procedure-optional procedure))))))

(define (block-passed-out! block)
  (if (not (rvalue-%passed-out? block))
      (begin
	(%rvalue-passed-out! block)
	(for-each (let ((procedure (block-procedure block)))
		    (if (and (rvalue/procedure? procedure)
			     (not (procedure-continuation? procedure)))
			(let ((continuation
			       (procedure-continuation-lvalue procedure)))
			  (lambda (lvalue)
			    (if (not (eq? lvalue continuation))
				(lvalue-externally-visible! lvalue))))
			lvalue-externally-visible!))
		  (block-bound-variables block))
	(let ((parent (block-parent block)))
	  (if parent
	      (block-passed-out! parent)
	      (for-each lvalue-externally-visible!
			(block-free-variables block)))))))

(define-method-table-entry 'BLOCK passed-out-methods
  block-passed-out!)

(define (lvalue-externally-visible! lvalue)
  (lvalue-passed-in! lvalue)
  (lvalue-passed-out! lvalue))

(define (lvalue-passed-in! lvalue)
  (if (lvalue-passed-in? lvalue)
      (set-lvalue-passed-in?! lvalue 'SOURCE)
      (begin
	(%lvalue-passed-in! lvalue 'SOURCE)
	(for-each (lambda (lvalue)
		    (if (not (lvalue-passed-in? lvalue))
			(%lvalue-passed-in! lvalue 'INHERITED)))
		  (lvalue-forward-links lvalue)))))

(define (%lvalue-passed-in! lvalue value)
  (set-lvalue-passed-in?! lvalue value)
  (for-each (lambda (application)
	      (if (not (null? (application-arguments application)))
		  (enqueue-node! application)))
	    (lvalue-applications lvalue)))

(define (lvalue-passed-out! lvalue)
  (if (not (lvalue-passed-out? lvalue))
      (begin (%lvalue-passed-out! lvalue)
	     (for-each %lvalue-passed-out! (lvalue-backward-links lvalue))
	     (for-each rvalue-passed-out! (lvalue-values lvalue)))))

(define-integrable (%lvalue-passed-out! lvalue)
  (set-lvalue-passed-out?! lvalue true))

)