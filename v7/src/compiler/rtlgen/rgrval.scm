d3 1
a4 1
$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgrval.scm,v 4.12 1988/12/12 21:52:46 cph Exp $
#| -*-Scheme-*-
Copyright (c) 1988 Massachusetts Institute of Technology
$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgrval.scm,v 4.12 1988/12/12 21:52:46 cph Exp $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; RTL Generation: RValues
;;; package: (compiler rtl-generator generate/rvalue)

(declare (usual-integrations))

(define (generate/rvalue operand scfg*cfg->cfg! generator)
  (with-values (lambda () (generate/rvalue* operand))
    (lambda (prefix expression)
      (scfg*cfg->cfg! prefix (generator expression)))))

(define (generate/rvalue* operand)
  ((method-table-lookup rvalue-methods (tagged-vector/index operand)) operand))

(define rvalue-methods
  (make-method-table rvalue-types false))

(define-integrable (expression-value/simple expression)
  (values (make-null-cfg) expression))

(define (expression-value/temporary prefix result)
  (load-temporary-register
   (lambda (assignment reference)
     (values (scfg*scfg->scfg! prefix assignment) reference))
   result
   identity-procedure))

(define-method-table-entry 'CONSTANT rvalue-methods
  (lambda (constant)
    (expression-value/simple (rtl:make-constant (constant-value constant)))))

(define-method-table-entry 'BLOCK rvalue-methods
  (lambda (block)
    block ;; ignored
    (expression-value/simple (rtl:make-fetch register:environment))))

(define-method-table-entry 'REFERENCE rvalue-methods
  (lambda (reference)
    (let ((context (reference-context reference))
	  (safe? (reference-safe? reference)))
	     (lambda ()
	       (find-variable context lvalue
		(lambda (locative)
		  (expression-value/simple (rtl:make-fetch locative)))
		(lambda (environment name)
		  (expression-value/temporary
		   (rtl:make-interpreter-call:lookup
		    environment
		    (intern-scode-variable! (reference-context/block context)
					    name)
		    safe?)
		   (rtl:interpreter-call-result:lookup)))
		(lambda (name)
		  (if (memq 'IGNORE-REFERENCE-TRAPS
			    (variable-declarations lvalue))
		      (load-temporary-register values
					       (rtl:make-variable-cache name)
					       rtl:make-fetch)
		      (generate/cached-reference name safe?)))))))
	(cond ((not value) (perform-fetch))
			  lvalue))
	       |#
	      ((not (rvalue/procedure? value))
	       (generate/rvalue* value))
	      (else (perform-fetch)))))))
