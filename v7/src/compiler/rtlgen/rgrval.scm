d3 1
a4 1
$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgrval.scm,v 1.13 1987/07/26 22:06:03 cph Exp $
#| -*-Scheme-*-
Copyright (c) 1987 Massachusetts Institute of Technology
$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rgrval.scm,v 1.13 1987/07/26 22:06:03 cph Exp $

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
(define (generate/rvalue rvalue)
  ((vector-method rvalue generate/rvalue) rvalue))

(define (define-rvalue-generator tag generator)
  (define-vector-method tag generate/rvalue generator))
  (with-values (lambda () (generate/rvalue* operand))
(define rvalue-methods
  (return-2 (make-null-cfg) expression))

(define-integrable (expression-value/transform expression-value transform)
  (transmit-values expression-value
    (lambda (prefix expression)
      (return-2 prefix (transform expression)))))

(define (expression-value/temporary prefix result)
  (let ((temporary (make-temporary)))
    (return-2 (scfg*scfg->scfg! prefix (rtl:make-assignment temporary result))
	      (rtl:make-fetch temporary))))
(define-method-table-entry 'CONSTANT rvalue-methods
(define (generate/constant constant)
  (expression-value/simple (rtl:make-constant (constant-value constant))))

(define-rvalue-generator constant-tag
  generate/constant)

(define-rvalue-generator block-tag
  (lambda (block)
(define-method-table-entry 'BLOCK rvalue-methods

(define-rvalue-generator reference-tag
  (lambda (reference)
    (if (vnode-known-constant? (reference-variable reference))
	(generate/constant (vnode-known-value (reference-variable reference)))
	(find-variable (reference-block reference)
		       (reference-variable reference)
	  (lambda (locative)
	    (expression-value/simple (rtl:make-fetch locative)))
	  (lambda (environment name)
	    (expression-value/temporary
	     (rtl:make-interpreter-call:lookup
	      environment
	      (intern-scode-variable! (reference-block reference) name)
	      (reference-safe? reference))
	     (rtl:interpreter-call-result:lookup)))
	  (lambda (name)
	    (generate/cached-reference name (reference-safe? reference)))))))

(define (generate/cached-reference name safe?)
  (let ((temp (make-temporary))
	(result (make-temporary)))
    (return-2
     (let ((cell (rtl:make-fetch temp)))
       (let ((reference (rtl:make-fetch cell)))
	 (let ((n1 (rtl:make-assignment temp (rtl:make-variable-cache name)))
	       (n2 (rtl:make-type-test (rtl:make-object->type reference)
				       (ucode-type reference-trap)))
	       (n3 (rtl:make-assignment result reference))
	       (n4 (rtl:make-interpreter-call:cache-reference cell safe?))
	       (n5
		(rtl:make-assignment
		 result
		 (rtl:interpreter-call-result:cache-reference))))
	   (scfg-next-connect! n1 n2)
	   (pcfg-alternative-connect! n2 n3)
	   (scfg-next-connect! n4 n5)
	   (if safe?
	       (let ((n6 (rtl:make-unassigned-test reference))
		     ;; Make new copy of n3 to keep CSE happy.
		     ;; Otherwise control merge will confuse it.
		     (n7 (rtl:make-assignment result reference)))
		 (pcfg-consequent-connect! n2 n6)
		 (pcfg-consequent-connect! n6 n7)
		 (pcfg-alternative-connect! n6 n4)
		 (make-scfg (cfg-entry-node n1)
			    (hooks-union (scfg-next-hooks n3)
					 (hooks-union (scfg-next-hooks n5)
						      (scfg-next-hooks n7)))))
	       (begin
		 (pcfg-consequent-connect! n2 n4)
		 (make-scfg (cfg-entry-node n1)
			    (hooks-union (scfg-next-hooks n3)
					 (scfg-next-hooks n5))))))))
		   (make-scfg (cfg-entry-node n2)
			      (hooks-union (scfg-next-hooks n3)
(define-rvalue-generator temporary-tag
  (lambda (temporary)
    (if (vnode-known-constant? temporary)
	(generate/constant (vnode-known-value temporary))
	(expression-value/simple (rtl:make-fetch temporary)))))

(define-rvalue-generator access-tag
  (lambda (*access)
    (transmit-values (generate/rvalue (access-environment *access))
      (lambda (prefix expression)
	(expression-value/temporary
	 (scfg*scfg->scfg!
	  prefix
	  (rtl:make-interpreter-call:access expression (access-name *access)))
	 (rtl:interpreter-call-result:access))))))

(define-rvalue-generator procedure-tag
  (lambda (procedure)
(define-method-table-entry 'PROCEDURE rvalue-methods
    (case (procedure/type procedure)
       (expression-value/transform (make-closure-environment procedure)
	 (lambda (environment)
	   (make-closure-cons procedure environment))))
	 (else
       (expression-value/simple (make-ic-cons procedure)))
	   (make-cons-closure-indirection procedure)))))
       (error "Reference to open procedure" procedure))
       (if (not (procedure-virtual-closure? procedure))
	   (error "Reference to open procedure" procedure))

(define (make-ic-cons procedure)
  ;; IC procedures have their entry points linked into their headers
  ;; at load time by the linker.
  (let ((header
	 (scode/make-lambda (variable-name (procedure-name procedure))
			    (map variable-name (procedure-required procedure))
			    (map variable-name (procedure-optional procedure))
			    (let ((rest (procedure-rest procedure)))
			      (and rest (variable-name rest)))
			    (map variable-name
				 (append (procedure-auxiliary procedure)
					 (procedure-names procedure)))
			    '()
			    false)))
    (set! *ic-procedure-headers*
	  (cons (cons header (procedure-external-label procedure))
		*ic-procedure-headers*))
    (rtl:make-typed-cons:pair
     (rtl:make-constant (scode/procedure-type-code header))
     (rtl:make-constant header)
     ;; Is this right if the procedure is being closed
     ;; inside another IC procedure?
     (rtl:make-fetch register:environment))))
	    ;; inside another IC procedure?
(define (make-closure-environment procedure)
  (let ((block (block-parent (procedure-block procedure))))
(define (make-non-trivial-closure-cons procedure block**)
	   (expression-value/simple (rtl:make-constant false)))
	  ((ic-block? block)
	   (expression-value/simple
	    (if (ic-block/use-lookup? block)
		(let ((closure-block (procedure-closure-block procedure)))
		  (if (ic-block? closure-block)
		      (rtl:make-fetch register:environment)
		      (closure-ic-locative closure-block block)))
		(rtl:make-constant false))))
	  ((closure-block? block)
	   (let ((closure-block (procedure-closure-block procedure)))
	     (define (loop variables)
	       (cond ((null? variables) '())
		     ((integrated-vnode? (car variables))
		      (loop (cdr variables)))
		     (else
		      (cons (rtl:make-push
			     (rtl:make-fetch
			      (find-closure-variable closure-block
						     (car variables))))
			    (loop (cdr variables))))))

	     (let ((pushes
		    (let ((parent (block-parent block))
			  (pushes (loop (block-bound-variables block))))
		      (if (and parent (ic-block/use-lookup? parent))
			  (cons (rtl:make-push
				 (closure-ic-locative closure-block
						      parent))
				pushes)
			  pushes))))
	       (expression-value/temporary
		(scfg*->scfg!
		 (reverse!
		  (cons (rtl:make-interpreter-call:enclose (length pushes))
			pushes)))
		(rtl:interpreter-call-result:enclose)))))
	  (else
	   (error "Unknown block type" block)))))

(define (make-closure-cons procedure environment)
  (rtl:make-typed-cons:pair (rtl:make-constant type-code:compiled-procedure)
			    (rtl:make-entry:procedure procedure)
			    environment))			       (find-closure-variable context variable)))))
			  code)))))
	     (error "Unknown block type" block))))))
	     (error "Unknown block type" block))))))
