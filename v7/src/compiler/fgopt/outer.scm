#| -*-Scheme-*-

$Id: outer.scm,v 4.7 2002/11/20 19:45:49 cph Exp $

Copyright (c) 1987, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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
  (set-application-args-passed-out?! application false)
  ;; Need more sophisticated test here so that particular primitive
  ;; operators only pass out specific operands.  A good test case is
  ;; `lexical-unassigned?' with a known block for its first argument
  ;; and a known symbol for its second.  Unfortunately, doing this
  ;; optimally introduces feedback in this analysis.
  (if (there-exists? (rvalue-values (application-operator application))
		     (lambda (value) (not (rvalue/procedure? value))))
      (application-arguments-passed-out! application)))

(define (check-application application)
  (if (and (rvalue-passed-in? (application-operator application))
	   (not (application-args-passed-out? application)))
      (application-arguments-passed-out! application)))

(define-integrable (application-arguments-passed-out! application)
  (set-application-args-passed-out?! application true)
  (for-each rvalue-passed-out!
	    (application-operands application)))

(define (rvalue-passed-out! rvalue)
  ((method-table-lookup passed-out-methods (tagged-vector/index rvalue))
   rvalue))

(define-integrable (%rvalue-passed-out! rvalue)
  (set-rvalue-%passed-out?! rvalue
			    (let ((old (rvalue-%passed-out? rvalue)))
			      (if old
				  (1+ old)
				  1))))

(define passed-out-methods
  (make-method-table rvalue-types %rvalue-passed-out!))

(define-method-table-entry 'REFERENCE passed-out-methods
  (lambda (reference)
    (lvalue-passed-out! (reference-lvalue reference))))

(define-method-table-entry 'PROCEDURE passed-out-methods
  (lambda (procedure)
    (%rvalue-passed-out! procedure)
    ;; The rest parameter was marked in the initialization.
    (for-each lvalue-passed-in! (procedure-required procedure))
    (for-each lvalue-passed-in! (procedure-optional procedure))))

(define (block-passed-out! block)
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
		  (block-free-variables block)))))

(define-method-table-entry 'BLOCK passed-out-methods
  block-passed-out!)

(define (lvalue-externally-visible! lvalue)
  (if (not (and (lvalue/variable? lvalue)
		(memq 'CONSTANT (variable-declarations lvalue))))
      (lvalue-passed-in! lvalue))
  (lvalue-passed-out! lvalue))

(define (lvalue-passed-in! lvalue)
  (let ((prev (lvalue-passed-in? lvalue)))
    (cond ((false? prev)
	   (%lvalue-passed-in! lvalue 1)
	   (for-each (lambda (lvalue)
		       (if (not (lvalue-passed-in? lvalue))
			   (%lvalue-passed-in! lvalue 'INHERITED)))
		     (lvalue-forward-links lvalue)))
	  ((not (eq? prev 'INHERITED))		; (number? prev)
	   (set-lvalue-passed-in?! lvalue (1+ prev)))
	  (else
	   (set-lvalue-passed-in?! lvalue 1)))))

(define (%lvalue-passed-in! lvalue value)
  (set-lvalue-passed-in?! lvalue value)
  (for-each (lambda (application)
	      (if (not (application-args-passed-out? application))
		  (enqueue-node! application)))
	    (lvalue-applications lvalue)))

(define (lvalue-passed-out! lvalue)
  (%lvalue-passed-out! lvalue)
  (for-each %lvalue-passed-out! (lvalue-backward-links lvalue))
  (for-each rvalue-passed-out! (lvalue-values lvalue)))

(define-integrable (%lvalue-passed-out! lvalue)
  (set-lvalue-passed-out?! lvalue
			   (let ((old (lvalue-passed-out? lvalue)))
			     (if old
				 (1+ old)
				 1))))

)