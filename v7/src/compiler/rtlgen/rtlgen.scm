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

;;;; RTL Generation

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/rtlgen.scm,v 1.2 1986/12/21 14:52:34 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

(define *nodes*)

(define (generate-rtl quotations procedures)
  (with-new-node-marks
   (lambda ()
     (fluid-let ((*nodes* '()))
       (for-each generate:quotation quotations)
       (for-each generate:procedure procedures)
       (for-each generate:remove-memo *nodes*)))))

(define (generate:cfg cfg offset)
  (generate:node (cfg-entry-node cfg) offset))

(define (generate:next node offset)
  (cond ((not node) (make-null-cfg))
	((node-marked? node)
	 (let ((memo (node-property-get node generate:node)))
	   (if (not (= (car memo) offset))
	       (error "Node entered at different offsets" node))
	   (cdr memo)))
	(else (generate:node node offset))))

(define (generate:node node offset)
  (node-mark! node)
  (let ((cfg ((vector-method node generate:node) node offset)))
    (node-property-put! node generate:node (cons offset cfg))
    (set! *nodes* (cons node *nodes*))
    cfg))

(define (generate:remove-memo rnode)
  (node-property-remove! rnode generate:node))

(define (define-generator tag generator)
  (define-vector-method tag generate:node generator))

(define (generate:quotation quotation)
  (set-quotation-rtl! quotation
		      (generate:cfg (quotation-fg-entry quotation) 0)))

(define (generate:procedure procedure)
  (set-procedure-rtl!
   procedure
   ((cond ((ic-procedure? procedure) identity-procedure)
	  ((closure-procedure? procedure) generate:closure-procedure)
	  ((stack-procedure? procedure) generate:stack-procedure)
	  (else (error "Unknown procedure type" procedure)))
    procedure
    (generate:cfg (procedure-fg-entry procedure) 0))))

(define (generate:closure-procedure procedure cfg)
  (scfg-append! (if (or (not (null? (procedure-optional procedure)))
			(procedure-rest procedure))
		    ((if (closure-procedure-needs-operator? procedure)
			 rtl:make-setup-closure-lexpr
			 rtl:make-setup-stack-lexpr)
		     procedure)
		    (rtl:make-procedure-heap-check procedure))
		(setup-stack-frame procedure)
		cfg))

(define (generate:stack-procedure procedure cfg)
  (scfg-append! (if (procedure-rest procedure)
		    (rtl:make-setup-stack-lexpr procedure)
		    (rtl:make-procedure-heap-check procedure))
		(setup-stack-frame procedure)
		cfg))

(define (setup-stack-frame procedure)
  (define (loop variables pushes)
    (if (null? variables)
	(scfg*->scfg! pushes)
	(loop (cdr variables)
	      (cons (rtl:make-push
		     (if (variable-assigned? (car variables))
			 (rtl:make-cell-cons (rtl:make-unassigned))
			 (rtl:make-unassigned)))
		    pushes))))

  (define (cellify-variables variables)
    (scfg*->scfg! (map cellify-variable variables)))

  (define (cellify-variable variable)
    (if (variable-assigned? variable)
	(let ((locative
	       (stack-locative-offset
		register:stack-pointer
		(variable-offset (procedure-block procedure) variable))))
	  (rtl:make-assignment locative
			       (rtl:make-cell-cons (rtl:make-fetch locative))))
	(make-null-cfg)))

  (scfg-append! (loop (procedure-auxiliary procedure) '())
		(cellify-variables (procedure-required procedure))
		(cellify-variables (procedure-optional procedure))
		(let ((rest (procedure-rest procedure)))
		  (if rest
		      (cellify-variable rest)
		      (make-null-cfg)))))

;;;; Statements

(define-generator definition-tag
  (lambda (definition offset)
    (scfg-append! (rvalue->sexpression (definition-rvalue definition) offset
		    (lambda (expression)
		      (find-variable (definition-block definition)
				     (definition-lvalue definition)
				     offset
			(lambda (locative)
			  (error "Definition of compiled variable"))
			(lambda (environment name)
			  (rtl:make-interpreter-call:define environment
							    name
							    expression)))))
		  (generate:next (snode-next definition) offset))))

(define-generator assignment-tag
  (lambda (assignment offset)
    (generate-assignment (assignment-block assignment)
			 (assignment-lvalue assignment)
			 (assignment-rvalue assignment)
			 (snode-next assignment)
			 offset)))

(define (generate-assignment block lvalue rvalue next offset)
  ((vector-method lvalue generate-assignment) block lvalue rvalue next offset))

(define (define-assignment tag generator)
  (define-vector-method tag generate-assignment generator))

(define-assignment variable-tag
  (lambda (block variable rvalue next offset)
    (scfg-append! (if (integrated-vnode? variable)
		      (make-null-cfg)
		      (rvalue->sexpression rvalue offset
			(lambda (expression)
			  (find-variable block variable offset
			    (lambda (locative)
			      (rtl:make-assignment locative expression))
			    (lambda (environment name)
			      (rtl:make-interpreter-call:set!
			       environment
			       (intern-scode-variable! block name)
			       expression))))))
		  (generate:next next offset))))

(define (assignment:value-register block value-register rvalue next offset)
  (if next (error "Return node has next"))
  (scfg-append! (if (or (value-register? rvalue)
			(value-temporary? rvalue))
		    (make-null-cfg)
		    (rvalue->sexpression rvalue offset
		      (lambda (expression)
			(rtl:make-assignment register:value expression))))
		(if (stack-procedure-block? block)
		    (rtl:make-message-sender:value
		     (+ offset (block-frame-size block)))
		    (scfg-append!
		     (if (closure-procedure-block? block)
			 (rtl:make-pop-frame (block-frame-size block))
			 (make-null-cfg))
		     (rtl:make-return)))))

(define-assignment value-register-tag
  assignment:value-register)

(define-assignment value-push-tag
  (lambda (block value-push rvalue next offset)
    (rvalue->sexpression rvalue offset
      (lambda (expression)
	(scfg-append! (rtl:make-push expression)
		      (generate:next next (1+ offset)))))))

(define-assignment value-ignore-tag
  (lambda (block value-ignore rvalue next offset)
    (if next (error "Return node has next"))
    (make-null-cfg)))

(define-assignment temporary-tag
  (lambda (block temporary rvalue next offset)
    (let ((type (temporary-type temporary)))
      (case type
	((#F)
	 (scfg-append!
	  (if (integrated-vnode? temporary)
	      (make-null-cfg)
	      (rvalue->sexpression rvalue offset
	       (lambda (expression)
		 (rtl:make-assignment temporary expression))))
	  (generate:next next offset)))
	((VALUE)
	 (assignment:value-register block temporary rvalue next offset))
	(else
	 (error "Unknown temporary type" type))))))

;;;; Predicates

(define-generator true-test-tag
  (lambda (test offset)
    (pcfg*scfg->pcfg!
     (let ((rvalue (true-test-rvalue test)))
       (if (rvalue-known-constant? rvalue)
	   (constant->pcfg (rvalue-constant-value rvalue))
	   (rvalue->pexpression rvalue offset rtl:make-true-test)))
     (generate:next (pnode-consequent test) offset)
     (generate:next (pnode-alternative test) offset))))

(define-generator unassigned-test-tag
  (lambda (test offset)
    (pcfg*scfg->pcfg!
     (find-variable (unassigned-test-block test)
		    (unassigned-test-variable test)
		    offset
       (lambda (locative)
	 (rtl:make-unassigned-test (rtl:make-fetch locative)))
       (lambda (environment name)
	 (scfg*pcfg->pcfg!
	  (rtl:make-interpreter-call:unassigned? environment name)
	  (rtl:make-true-test (rtl:interpreter-call-result:unassigned?)))))
     (generate:next (pnode-consequent test) offset)
     (generate:next (pnode-alternative test) offset))))

(define-generator unbound-test-tag
  (lambda (test offset)
    (pcfg*scfg->pcfg!
     (let ((variable (unbound-test-variable test)))
       (if (ic-block? (variable-block variable))
	   (scfg*pcfg->pcfg!
	    (rtl:make-interpreter-call:unbound?
	     (nearest-ic-block-expression (unbound-test-block test) offset)
	     (variable-name variable))
	    (rtl:make-true-test (rtl:interpreter-call-result:unbound?)))
	   (make-false-pcfg)))
     (generate:next (pnode-consequent test) offset)
     (generate:next (pnode-alternative test) offset))))

;;;; Expressions

(define (rvalue->sexpression rvalue offset receiver)
  (rvalue->expression rvalue offset (prepend-to-scfg receiver)))

(define ((prepend-to-scfg receiver) expression prefix)
  (scfg-append! prefix (receiver expression)))

(define (rvalue->pexpression rvalue offset receiver)
  (rvalue->expression rvalue offset (prepend-to-pcfg receiver)))

(define ((prepend-to-pcfg receiver) expression prefix)
  (scfg*pcfg->pcfg! prefix (receiver expression)))

(define (rvalue->expression rvalue offset receiver)
  ((vector-method rvalue rvalue->expression) rvalue offset receiver))

(define (define-rvalue->expression tag generator)
  (define-vector-method tag rvalue->expression generator))

(define (constant->expression constant offset receiver)
  (receiver (rtl:make-constant (constant-value constant))
	    (make-null-cfg)))

(define-rvalue->expression constant-tag
  constant->expression)

(define-rvalue->expression block-tag
  (lambda (block offset receiver)
    (receiver (rtl:make-fetch register:environment) (make-null-cfg))))

(define-rvalue->expression value-register-tag
  (lambda (value-register offset receiver)
    (receiver (rtl:make-fetch register:value) (make-null-cfg))))

(define-rvalue->expression reference-tag
  (lambda (reference offset receiver)
    (reference->expression (reference-block reference)
			   (reference-variable reference)
			   offset
			   receiver)))

(define (reference->expression block variable offset receiver)
  (if (vnode-known-constant? variable)
      (constant->expression (vnode-known-value variable) offset receiver)
      (find-variable block variable offset
	(lambda (locative)
	  (receiver (rtl:make-fetch locative) (make-null-cfg)))
	(lambda (environment name)
	  (receiver (rtl:interpreter-call-result:lookup)
		    (rtl:make-interpreter-call:lookup
		     environment
		     (intern-scode-variable! block name)))))))

(define-rvalue->expression temporary-tag
  (lambda (temporary offset receiver)
    (if (vnode-known-constant? temporary)
	(constant->expression (vnode-known-value temporary) offset receiver)
	(let ((type (temporary-type temporary)))
	  (cond ((not type)
		 (receiver (rtl:make-fetch temporary)
			   (make-null-cfg)))
		((eq? type 'VALUE)
		 (receiver (rtl:make-fetch register:value)
			   (make-null-cfg)))
		(else (error "Illegal temporary reference" type)))))))

(define-rvalue->expression access-tag
  (lambda (*access offset receiver)
    (receiver (rtl:interpreter-call-result:access)
	      (rtl:make-interpreter-call:access (access-environment *access)
						(access-name *access)))))

(define-rvalue->expression procedure-tag
  (lambda (procedure offset receiver)
    ((cond ((ic-procedure? procedure) rvalue->expression:ic-procedure)
	   ((closure-procedure? procedure)
	    rvalue->expression:closure-procedure)
	   ((stack-procedure? procedure)
	    (error "RVALUE->EXPRESSION: Stack procedure reference" procedure))
	   (else (error "Unknown procedure type" procedure)))
     procedure offset receiver)))

(define (rvalue->expression:ic-procedure procedure offset receiver)
  ;; IC procedures have their entry points linked into their headers
  ;; at load time by the linker.
  (let ((header
	 (scode:make-lambda (variable-name (procedure-name procedure))
			    (map variable-name (procedure-required procedure))
			    (map variable-name (procedure-optional procedure))
			    (let ((rest (procedure-rest procedure)))
			      (and rest (variable-name rest)))
			    (map variable-name (procedure-auxiliary procedure))
			    '()
			    false)))
    (set! *ic-procedure-headers*
	  (cons (cons procedure header)
		*ic-procedure-headers*))
    (receiver (rtl:make-typed-cons:pair
	       (rtl:make-constant (scode:procedure-type-code header))
	       (rtl:make-constant header)
	       (rtl:make-fetch register:environment))
	      (make-null-cfg))))

(define (rvalue->expression:closure-procedure procedure offset receiver)
  (let ((block (block-parent (procedure-block procedure))))
    (define (finish environment prefix)
      (receiver (rtl:make-typed-cons:pair
		 (rtl:make-constant type-code:compiled-procedure)
		 (rtl:make-entry:procedure procedure)
		 environment)
		prefix))
    (cond ((not block)
	   (finish (rtl:make-constant false) (make-null-cfg)))
	  ((ic-block? block)
	   (finish (rtl:make-fetch register:environment) (make-null-cfg)))
	  ((closure-block? block)
	   (let ((closure-block (procedure-closure-block procedure)))
	     (define (loop variables n receiver)
	       (if (null? variables)
		   (receiver offset n '())
		   (loop (cdr variables) (1+ n)
		     (lambda (offset n pushes)
		       (receiver (1+ offset) n
				 (cons (rtl:make-push
					(rtl:make-fetch
					 (find-closure-variable closure-block
								(car variables)
								offset)))
				       pushes))))))

	     (define (make-frame n pushes)
	       (finish (rtl:interpreter-call-result:enclose)
		       (scfg*->scfg!
			(reverse!
			 (cons (rtl:make-interpreter-call:enclose n)
			       pushes)))))

	     (define (loser locative)
	       (error "Closure parent not IC block"))

	     (loop (block-bound-variables block) 0
	       (lambda (offset n pushes)
		 (let ((parent (block-parent block)))
		   (if parent
		       (find-block closure-block parent offset
			 loser
			 loser
			 (lambda (locative nearest-ic-locative)
			   (make-frame (1+ n)
				       (cons (rtl:make-push locative)
					     pushes))))
		       (make-frame n pushes)))))))
	  (else (error "Unknown block type" block)))))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: (access rtl-generator-package compiler-package)
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
  "node rtl arguments")