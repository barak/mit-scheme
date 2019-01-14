#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Flow Graph Generation
;;; package: (compiler fg-generator)

(declare (usual-integrations))

;; Note: The C back end cannot dump objects, and instead generates
;; code to construct the objects.
;; Thus the unmapping of reference traps must be done late
;; when generating such code, and not early, since the code
;; that destructures object will otherwise run into actual
;; reference traps.

(define compiler:fggen-unmap-reference-traps-early? true)

(define-structure (context (conc-name context/)
			   (constructor context/make))
  (unconditional? #f read-only #t type boolean)
  (static? #f read-only #t type boolean))

(define-integrable (context/make-initial)
  (context/make #t #t))

(define-integrable (context/make-internal)
  (context/make #t #f))

(define-integrable (context/conditional context)
  (context/make #f (context/static? context)))

(define-integrable (context/unconditional context)
  (context/make #t (context/static? context)))

(define (construct-graph scode)
  (fluid-let ((*virtual-continuations* '())
	      (*global-variables* '()))
    (let ((block (make-block #f 'EXPRESSION)))
      (let ((continuation (make-continuation-variable block)))
	(let ((expression
	       (make-expression
		block
		continuation
		(with-values
		    (lambda ()
		      (let ((collect
			     (lambda (names declarations body)
			       (values (make-variables block names)
				       declarations
				       (unscan-defines names '() body)))))
			(if (scode/open-block? scode)
			    (collect (scode/open-block-names scode)
				     (scode/open-block-declarations scode)
				     (scode/open-block-actions scode))
			    (scan-defines scode collect))))
		  (lambda (variables declarations scode)
		    (set-block-bound-variables! block variables)
		    (generate/body block continuation (context/make-initial)
				   declarations scode))))))
	  ;; Delete as many noop nodes as possible.
	  (for-each
	   (lambda (procedure)
	     (if (procedure-continuation? procedure)
		 (let ((next (snode-next (procedure-entry-node procedure))))
		   (if next
		       (set-procedure-entry-node! procedure next)))))
	   *procedures*)
	  (for-each (lambda (continuation)
		      (set-virtual-continuation/parent! continuation #f))
		    *virtual-continuations*)
	  (initialize-reference-contexts! expression *procedures*)
	  expression)))))

(define (make-variables block names)
  (map (lambda (name) (make-variable block name)) names))

(define (generate/body block continuation context declarations expression)
  (process-pre-declarations! block declarations)
  (let ((scfg (generate/expression block continuation context expression)))
    (process-post-declarations! block declarations)
    scfg))

;;;; Continuations

(define (continuation/case continuation unknown effect predicate value)
  (cond ((variable? continuation)
	 (let ((type (continuation-variable/type continuation)))
	   (cond ((not type) unknown)
		 ((eq? type continuation-type/effect) effect)
		 ((eq? type continuation-type/predicate) unknown)
		 ((eq? type continuation-type/value) unknown)
		 (else (error "Illegal continuation type" type)))))
	((virtual-continuation? continuation)
	 (if (virtual-continuation/reified? continuation)
	     (continuation/case (virtual-continuation/reification continuation)
				unknown
				effect
				predicate
				value)
	     (let ((type (virtual-continuation/type continuation)))
	       (cond ((eq? type continuation-type/effect) effect)
		     ((eq? type continuation-type/predicate) predicate)
		     ((eq? type continuation-type/value) value)
		     (else
		      (error "Illegal virtual continuation type" type))))))
	((procedure? continuation)
	 (let ((type (continuation/type continuation)))
	   (cond ((eq? type continuation-type/effect) effect)
		 ((eq? type continuation-type/predicate) predicate)
		 ((eq? type continuation-type/value) value)
		 (else (error "Illegal continuation type" type)))))
	(else (error "Illegal continuation" continuation))))

(define (continuation/known-type continuation)
  (cond ((variable? continuation)
	 (continuation-variable/type continuation))
	((virtual-continuation? continuation)
	 (virtual-continuation/type continuation))
	((procedure? continuation)
	 (continuation/type continuation))
	(else (error "Illegal continuation" continuation))))

(define (continuation/type? continuation type)
  (cond ((variable? continuation)
	 (eq? (continuation-variable/type continuation) type))
	((virtual-continuation? continuation)
	 (eq? (virtual-continuation/type continuation) type))
	((procedure? continuation)
	 (eq? (continuation/type continuation) type))
	(else (error "Illegal continuation" continuation))))

(define-integrable (continuation/effect? continuation)
  (continuation/type? continuation continuation-type/effect))

(define-integrable (continuation/predicate? continuation)
  (continuation/type? continuation continuation-type/predicate))

(define (continuation/rvalue continuation)
  (make-reference (continuation/block continuation)
		  (continuation/parameter continuation)
		  #t))

(define-integrable (continuation/next-hooks continuation)
  (list (make-hook (continuation/entry-node continuation)
		   set-snode-next-edge!)))

(define-integrable (continuation-reference block continuation)
  (cond ((variable? continuation) (make-reference block continuation #t))
	((procedure? continuation) continuation)
	(else (error "Illegal continuation" continuation))))

(define (scfg*ctype->ctype! continuation)
  (continuation/case continuation
		     scfg*scfg->scfg!
		     scfg*scfg->scfg!
		     scfg*pcfg->pcfg!
		     scfg*subproblem->subproblem!))

;;;; Subproblems

(define (with-reified-continuation block
				   continuation
				   scfg*value->value!
				   generator)
  (if (virtual-continuation? continuation)
      (let ((continuation (virtual-continuation/reify! continuation)))
	(let ((push (make-push block continuation)))
	  (scfg*value->value! push
			      (generator (cfg-entry-node push)
					 continuation))))
      (generator #f continuation)))

(define (make-subproblem/canonical prefix continuation)
  (make-subproblem prefix
		   continuation
		   (continuation/rvalue continuation)))

(define (scfg*subproblem->subproblem! scfg subproblem)
  (make-subproblem (scfg*scfg->scfg! scfg (subproblem-prefix subproblem))
		   (subproblem-continuation subproblem)
		   (subproblem-rvalue subproblem)))

(define (pcfg*subproblem->subproblem! pcfg consequent alternative)
  (make-subproblem (pcfg*scfg->scfg! pcfg
				     (subproblem-prefix consequent)
				     (subproblem-prefix alternative))
		   (subproblem-continuation consequent)
		   (subproblem-rvalue alternative)))

(define *virtual-continuations*)

(define (virtual-continuation/make block parent type debugging)
  (let ((continuation
	 (virtual-continuation/%make block parent type debugging)))
    (set! *virtual-continuations* (cons continuation *virtual-continuations*))
    continuation))

(define (wrapper/subproblem type)
  (lambda (block continuation debugging generator)
    (generator (virtual-continuation/make block continuation type debugging))))

(define wrapper/subproblem/effect
  (wrapper/subproblem continuation-type/effect))

(define wrapper/subproblem/predicate
  (wrapper/subproblem continuation-type/predicate))

(define wrapper/subproblem/value
  (wrapper/subproblem continuation-type/value))

(define (make-continuation-debugging-info type expression . rest)
  (list->vector (cons* type (scode/original-expression expression) rest)))

(define (generator/subproblem wrapper)
  (lambda (block continuation context expression debugging-type . rest)
    (wrapper block
	     continuation
	     (and debugging-type
		  (apply make-continuation-debugging-info debugging-type rest))
      (lambda (continuation)
	(generate/expression block continuation context expression)))))

(define generate/subproblem/effect
  (generator/subproblem wrapper/subproblem/effect))

(define generate/subproblem/predicate
  (generator/subproblem wrapper/subproblem/predicate))

(define generate/subproblem/value
  (generator/subproblem wrapper/subproblem/value))

;;;; Values

(define (generate/constant block continuation context expression)
  context				; ignored
  (continue/rvalue-constant block continuation (make-constant expression)))

(define (generate/the-environment block continuation context expression)
  context expression			; ignored
  (continue/rvalue-constant block continuation block))

(define (continue/rvalue-constant block continuation rvalue)
  ((continuation/case continuation
		      continue/unknown
		      continue/effect
		      continue/predicate-constant
		      continue/value)
   block
   continuation
   rvalue))

(define (continue/predicate-constant block continuation rvalue)
  block continuation ;; ignored
  (if (and (rvalue/constant? rvalue)
	   (not (constant-value rvalue)))
      (snode->pcfg-false (make-fg-noop))
      (snode->pcfg-true (make-fg-noop))))

(define (continue/rvalue block continuation rvalue)
  ((continuation/case continuation
		      continue/unknown
		      continue/effect
		      continue/predicate
		      continue/value)
   block
   continuation
   rvalue))

(define (continue/unknown block continuation rvalue)
  (make-return block (make-reference block continuation #t) rvalue))

(define (continue/effect block continuation rvalue)
  rvalue ;; ignored
  (if (variable? continuation)
      (continue/unknown block continuation (make-constant #f))
      (make-null-cfg)))

(define-integrable (continue/predicate block continuation rvalue)
  continuation ;; ignored
  (make-true-test block rvalue))

(define (continue/value block continuation rvalue)
  (if (virtual-continuation? continuation)
      (make-subproblem (make-null-cfg) continuation rvalue)
      (make-subproblem/canonical (make-return block continuation rvalue)
				 continuation)))

(define-integrable (scode/make-safe-variable name)
  (cons safe-variable-tag name))

(define-integrable (scode/safe-variable-name reference)
  (cdr reference))

(define safe-variable-tag
  "safe-variable")

;; This is a kludge.

(define *global-variables*)

(define (scode/global-variable-name absolute-reference)
  (let ((name (scode/absolute-reference-name absolute-reference)))
    (or (assq name *global-variables*)
	(let ((pair (cons name '*GLOBAL*)))
	  (set! *global-variables* (cons pair *global-variables*))
	  pair))))

(define (generate/unassigned? block continuation context expression)
  (if (continuation/predicate? continuation)
      (continue/rvalue block
		       continuation
		       (make-unassigned-test
			block
			(find-name block (scode/unassigned?-name expression))))
      (generate/conditional block continuation context
			    (scode/make-conditional expression #T #F))))

(define (find-name block name)
  (define (search block if-non-local)
    (or (variable-assoc name (block-bound-variables block))
	(variable-assoc name (block-free-variables block))
	(let ((variable
	       (if (block-parent block)
		   (search (block-parent block)
			   (lambda (bl var) bl var))
		   (make-variable block name))))
	  (set-block-free-variables! block
				     (cons variable
					   (block-free-variables block)))
	  (if-non-local block variable)
	  variable)))
  (search block
	  (lambda (block variable)
	    (set-block-variables-nontransitively-free!
	     block
	     (cons variable
		   (block-variables-nontransitively-free block))))))

(define-integrable (make-variable-generator extract-name safe?)
  (lambda (block continuation context expression)
    context				; ignored
    (continue/rvalue block
		     continuation
		     (make-reference block
				     (find-name block
						(extract-name expression))
				     safe?))))

(define generate/variable
  (make-variable-generator scode/variable-name #f))

(define generate/safe-variable
  (make-variable-generator scode/safe-variable-name #t))

(define generate/global-variable
  (make-variable-generator scode/global-variable-name #f))


(define (generate/lambda block continuation context expression)
  (generate/lambda* block continuation
		    context (context/make-internal)
		    expression #f #f))

;; context is the context of the lambda expression.
;; context* is the context of its subexpressions.

(define (generate/lambda* block continuation context context* expression
			  continuation-type closure-block)
  (continue/rvalue-constant
   block
   continuation
   (scode/lambda-components expression
     (lambda (name required optional rest auxiliary declarations body)
       (transmit-values (parse-procedure-body auxiliary body)
	 (lambda (names values body*)
	   (let ((block (make-block block 'PROCEDURE)))
	     (let ((continuation (make-continuation-variable block))
		   (required* (make-variables block required))
		   (optional* (make-variables block optional))
		   (rest* (and rest (make-variable block rest)))
		   (names (make-variables block names)))
	       (let ((vars `(,@required*
			     ,@optional*
			     ,@(if rest* (list rest*) '())
			     ,@names)))
		 (set-continuation-variable/type! continuation
						  continuation-type)
		 (set-block-bound-variables! block `(,continuation ,@vars))
		 (if (context/static? context*)
		     (for-each (lambda (var)
				 (lvalue-put! var 'STATIC #t))
			       vars)))
	       (let ((procedure
		      (make-procedure
		       continuation-type/procedure
		       block name (cons continuation required*) optional* rest*
		       names
		       (map
			(lambda (value)
			  ;; The other parts of this subproblem are not
			  ;; interesting since `value' is guaranteed to
			  ;; be either a constant or a procedure.
			  (subproblem-rvalue
			   (generate/subproblem/value block continuation
						      context* value #f)))
			values)
		       (generate/body block continuation
				      context* declarations body*))))
		 (if closure-block
		     (set-procedure-closure-context! procedure closure-block))
		 (if (context/unconditional? context)
		     (procedure-put! procedure 'UNCONDITIONAL #t))
		 (set-procedure-debugging-info!
		  procedure
		  (if (and
		       (scode/comment? body)
		       (scode/comment-directive? (scode/comment-text body)))
		      (scode/make-lambda name required optional rest auxiliary
					 declarations
					 (caddr (scode/comment-text body)))
		      expression))
		 procedure)))))))))

(define (parse-procedure-body auxiliary body)
  (transmit-values
      (parse-procedure-body* auxiliary (scode/sequence-actions body))
    (lambda (names values auxiliary actions)
      (if (null? auxiliary)
	  (return-3 names values (scode/make-sequence actions))
	  (return-3 '() '()
		    (scode/make-combination
		     (scode/make-lambda
		      scode-lambda-name:let auxiliary '() #f names '()
		      (scode/make-sequence
		       (map* actions scode/make-assignment names values)))
		     (map (lambda (name)
			    name ;; ignored
			    (make-unassigned-reference-trap))
			  auxiliary)))))))

(define (parse-procedure-body* names actions)
  ;; Extract any definitions that do not depend on the order of
  ;; events.
  (cond ((null? names)
	 (return-4 '() '() '() actions))
	((null? actions)
	 (error "Extraneous auxiliaries" names))

	;; Because `scan-defines' returns the auxiliary names in a
	;; particular order, we can expect to encounter them in that
	;; same order when looking through the body's actions.

	((and (scode/assignment? (car actions))
	      (eq? (scode/assignment-name (car actions)) (car names)))
	 (transmit-values (parse-procedure-body* (cdr names) (cdr actions))
	   (let ((value (scode/assignment-value (car actions))))
	     (if (or (scode/lambda? value)
		     (scode/delay? value)
		     (scode/constant? value))
		 (lambda (names* values auxiliary actions*)
		   (return-4 (cons (car names) names*)
			     (cons value values)
			     auxiliary
			     (if (null? actions*)
				 (list undefined-scode-conditional-branch)
				 actions*)))
		 (lambda (names* values auxiliary actions*)
		   (return-4 names*
			     values
			     (cons (car names) auxiliary)
			     (cons (car actions) actions*)))))))
	(else
	 (transmit-values (parse-procedure-body* names (cdr actions))
	   (lambda (names* values auxiliary actions*)
	     (return-4 names*
		       values
		       auxiliary
		       (cons (car actions) actions*)))))))

;;;; Combinators

(define (generate/sequence block continuation context expression)
  (if (scode/sequence? expression)
      ;; This is done in a funny way to enforce processing in sequence order.
      ;; In this way, compile-by-procedures compiles in a predictable order.
      (let ((first-action
	     (generate/subproblem/effect
	      block continuation context
	      (car (scode/sequence-actions expression))
              'SEQUENCE-CONTINUE
	      expression)))
	((scfg*ctype->ctype! continuation)
	 first-action
	 (generate/expression
	  block continuation context
	  (scode/make-sequence (cdr (scode/sequence-actions expression))))))
      (error "Not a sequence" expression)))

(define (generate/conditional block continuation context expression)
  (let ((predicate (scode/conditional-predicate expression))
	(consequent (scode/conditional-consequent expression))
	(alternative (scode/conditional-alternative expression)))
    (let ((predicate
	   (generate/subproblem/predicate
	    block continuation context
	    predicate 'CONDITIONAL-DECIDE expression)))
      (let ((simple
	     (lambda (hooks branch)
	       ((scfg*ctype->ctype! continuation)
		(make-scfg (cfg-entry-node predicate) hooks)
		(generate/expression block continuation context branch)))))
	(cond ((hooks-null? (pcfg-consequent-hooks predicate))
	       (simple (pcfg-alternative-hooks predicate) alternative))
	      ((hooks-null? (pcfg-alternative-hooks predicate))
	       (simple (pcfg-consequent-hooks predicate) consequent))
	      (else
	       (let ((finish
		      (lambda (continuation combiner)
			(combiner
			 predicate
			 (generate/expression block continuation
					      (context/conditional context)
					      consequent)
			 (generate/expression block continuation
					      (context/conditional context)
					      alternative)))))
		 ((continuation/case continuation
		    (lambda () (finish continuation pcfg*scfg->scfg!))
		    (lambda () (finish continuation pcfg*scfg->scfg!))
		    (lambda () (finish continuation pcfg*pcfg->pcfg!))
		    (lambda ()
		      (with-reified-continuation block
						 continuation
						 scfg*subproblem->subproblem!
			(lambda (push continuation)
			  push	;ignore
			  (finish continuation
			    (lambda (predicate consequent alternative)
			      (make-subproblem/canonical
			       (pcfg*scfg->scfg!
				predicate
				(subproblem-prefix consequent)
				(subproblem-prefix alternative))
			       continuation)))))))))))))))

(define (generate/combination block continuation context expression)
  (let ((operator (scode/combination-operator expression))
	(operands (scode/combination-operands expression)))
    (cond ((eq? (ucode-primitive not) operator)
	   (generate/conditional block continuation context
				 (scode/make-conditional (car operands)
							 #F #T)))
	  ((and (eq? (ucode-primitive general-car-cdr) operator)
		(let ((n (cadr operands)))
		  (and (exact-integer? n)
		       (positive? n))))
	   (generate/expression
	    block continuation context
	    (let loop ((expression (car operands)) (n (cadr operands)))
	      (if (= n 1)
		  expression
		  (loop (scode/make-combination
			 (if (= (remainder n 2) 1)
			     (ucode-primitive car)
			     (ucode-primitive cdr))
			 (list expression))
			(quotient n 2))))))
	  (else
	   (generate/operator
	    block continuation context expression operator
	    (generate/operands expression operands block continuation context
			       1))))))

(define (generate/operands expression operands block continuation context index)
  (let walk ((operands operands) (index index))
    (if (pair? operands)
	;; This forces the order of evaluation
	(let ((next
	       (generate/subproblem/value block continuation context
					  (car operands) 'COMBINATION-OPERAND
					  expression index)))
	  (cons next
		(walk (cdr operands) (1+ index))))
	'())))

(define (generate/operator block continuation context expression operator
			   operands*)
  (let ((make-combination
	 (lambda (push continuation)
	   (make-combination
	    block
	    (continuation-reference block continuation)
	    (wrapper/subproblem/value
	     block
	     continuation
	     (make-continuation-debugging-info 'COMBINATION-OPERAND
					       expression
					       0)
	     (lambda (continuation*)
	       (cond ((scode/lambda? operator)
		      (generate/lambda*
		       block continuation*
		       context (context/unconditional context)
		       operator
		       (continuation/known-type continuation)
		       #f))
		     ((scode/absolute-reference? operator)
		      (generate/global-variable block continuation*
						context operator))
		     (else
		      (generate/expression block continuation*
					   context operator)))))
	    operands*
	    push))))
    ((continuation/case continuation
      (lambda () (make-combination #f continuation))
      (lambda ()
	(if (variable? continuation)
	    (make-combination #f continuation)
	    (with-reified-continuation block
	      continuation
	      scfg*scfg->scfg!
	      (lambda (push continuation)
		(make-scfg
		 (cfg-entry-node
		  (make-combination push continuation))
		 (continuation/next-hooks continuation))))))
      (lambda ()
	(with-reified-continuation block
	  continuation
	  scfg*pcfg->pcfg!
	  (lambda (push continuation)
	    (scfg*pcfg->pcfg!
	     (make-scfg
	      (cfg-entry-node (make-combination push continuation))
	      (continuation/next-hooks continuation))
	     (make-true-test
	      block
	      (continuation/rvalue continuation))))))
      (lambda ()
	(with-reified-continuation block
	  continuation
	  scfg*subproblem->subproblem!
	  (lambda (push continuation)
	    (make-subproblem/canonical
	     (make-combination push continuation)
	     continuation))))))))

;;;; Assignments

(define (generate/assignment* maker find-name continuation-type
			      block continuation context expression name value)
  (let ((subproblem
	 (generate/subproblem/value block continuation context
				    value continuation-type expression)))
    (scfg-append!
     (if (subproblem-canonical? subproblem)
	 (make-scfg
	  (cfg-entry-node (subproblem-prefix subproblem))
	  (continuation/next-hooks (subproblem-continuation subproblem)))
	 (subproblem-prefix subproblem))
     (maker block (find-name block name) (subproblem-rvalue subproblem))
     (continue/effect block continuation #f))))

(define (generate/assignment block continuation context expression)
  (let ((name (scode/assignment-name expression))
	(value (scode/assignment-value expression)))
    (if (continuation/effect? continuation)
	(generate/assignment* make-assignment find-name 'ASSIGNMENT-CONTINUE
			      block continuation context
			      expression name value)
	(generate/combination
	 block continuation context
	 (let ((old-value (generate-uninterned-symbol "set-old-"))
	       (new-value (generate-uninterned-symbol "set-new-")))
	   (scode/make-let (list new-value)
			   (list value)
	     (scode/make-let (list old-value)
			     (list (scode/make-safe-variable name))
	       (scode/make-assignment name (scode/make-variable new-value))
	       (scode/make-variable old-value))))))))

(define (generate/definition block continuation context expression)
  (let ((name (scode/definition-name expression))
	(value (scode/definition-value expression)))
    (if (continuation/effect? continuation)
	(generate/assignment* make-definition make-definition-variable
			      'DEFINITION-CONTINUE block continuation
			      context expression name
			      (insert-letrec name value))
	(generate/expression
	 block continuation context
	 (scode/make-sequence (list expression name))))))

(define (make-definition-variable block name)
  (let ((bound (block-bound-variables block)))
    (or (variable-assoc name bound)
	(let ((variable (make-variable block name)))
	  (set-block-bound-variables! block (cons variable bound))
	  variable))))

(define (insert-letrec name value)
  (if (and compiler:implicit-self-static?
	   (scode/lambda? value))
      (scode/make-let '() '()
		      (scode/make-definition name value)
		      (scode/make-variable name))
      value))

;;;; Rewrites

(define (generate/disjunction block continuation context expression)
  ((continuation/case continuation
		      generate/disjunction/value
		      generate/disjunction/control
		      generate/disjunction/control
		      generate/disjunction/value)
   block continuation context expression))

(define (generate/disjunction/control block continuation context expression)
  (let ((predicate (scode/disjunction-predicate expression))
	(alternative (scode/disjunction-alternative expression)))
    (generate/conditional
     block continuation context
     (scode/make-conditional predicate #t alternative))))

(define (generate/disjunction/value block continuation context expression)
  (let ((predicate (scode/disjunction-predicate expression))
	(alternative (scode/disjunction-alternative expression)))
    (if (and (scode/combination? predicate)
	     (boolean-valued-operator?
	      (scode/combination-operator predicate)))
	(generate/conditional
	 block continuation context
	 (scode/make-conditional predicate #t alternative))
	(generate/combination
	 block continuation context
	 (let ((temp (generate-uninterned-symbol "or-predicate-")))
	   (scode/make-let (list temp)
			   (list predicate)
			   (let ((predicate (scode/make-variable temp)))
			     (scode/make-conditional predicate
						     predicate
						     alternative))))))))

(define (boolean-valued-operator? operator)
  (cond ((scode/primitive-procedure? operator)
	 (boolean-valued-function-primitive? operator))
	((scode/absolute-reference? operator)
	 (boolean-valued-function-variable?
	  (scode/absolute-reference-name operator)))
	(else
	 #f)))

(define (generate/access block continuation context expression)
  (generate/combination
   block continuation context
   (scode/make-combination (ucode-primitive lexical-reference)
			   (list (scode/access-environment expression)
				 (scode/access-name expression)))))

;; Handle directives inserted by the canonicalizer

(define (generate/comment block continuation context comment)
  (let ((text (scode/comment-text comment))
	(expression (scode/comment-expression comment)))
    (if (not (scode/comment-directive? text))
	(generate/expression block continuation context expression)
	(case (caadr text)
	  ((PROCESSED)
	   (generate/expression block continuation context expression))
	  ((COMPILE)
	   (if (not (scode/quotation? expression))
	       (error "Bad COMPILE directive" comment))
	   (continue/rvalue-constant
	    block continuation
	    (make-constant
	     (compile-recursively
	      (scode/quotation-expression expression)
	      #f
	      #f))))
	  ((COMPILE-PROCEDURE)
	   (let ((process
		  (lambda (name)
		    (if compiler:compile-by-procedures?
			(continue/rvalue-constant
			 block continuation
			 (make-constant
			  (compile-recursively expression #t name)))
			(generate/expression block continuation
					     context expression))))
		 (fail
		  (lambda ()
		    (error "Bad COMPILE-PROCEDURE directive" comment))))
	     (cond ((scode/lambda? expression)
		    (process (scode/lambda-name expression)))
		   ((scode/open-block? expression)
		    (let ((body (scode/open-block-actions expression)))
		      (if (and (null? (scode/open-block-names expression))
			       (scode/lambda? body))
			  (process (scode/lambda-name body))
			  (fail))))
		   (else
		    (fail)))))
	  ((ENCLOSE)
	   (generate/enclose block continuation context expression))
	  ((CONSTANTIFY)
	   (generate/constantify block continuation context comment expression))
	  (else
	   (warn "generate/comment: Unknown directive" (cadr text) comment)
	   (generate/expression block continuation context expression))))))

;; CONSTANTIFY directives are generated when an expression is introduced by
;; the canonicalizer.  It instructs fggen that the expression may be constant
;; folded once its operands have been, if they are all constants.

(define (generate/constantify block continuation context comment expression)
  (if (or (not (scode/combination? expression))
	  (not (eq? (ucode-primitive vector)
		    (scode/combination-operator expression))))
      (error "Bad CONSTANTIFY directive" comment))
  (let ((operands (generate/operands expression
				     (scode/combination-operands expression)
				     block continuation context 1)))
    (if (every (lambda (subpr)
		 (rvalue/constant? (subproblem-rvalue subpr)))
	       operands)
	(generate/constant
	 block continuation context
	 (list->vector
	  (map (lambda (subpr)
		 (let ((temp (constant-value (subproblem-rvalue subpr))))
		   (if compiler:fggen-unmap-reference-traps-early?
		       (unmap-reference-trap temp)
		       temp)))
	       operands)))
	(generate/operator block continuation context expression
			   (ucode-primitive vector)
			   operands))))

;; ENCLOSE directives are generated only for lambda expressions
;; evaluated in environments whose manipulation has been made
;; explicit.  The code should include a syntactic check.  The
;; expression must be a call to scode-eval with a quotation of a
;; lambda and a variable as arguments.
;; NOTE: This code depends on lvalue-integrated? never integrating
;; the hidden reference within the procedure object.  See base/lvalue
;; for some more information.

(define (generate/enclose block continuation context expression)
  (let ((operands (scode/combination-operands expression)))
    (generate/lambda*
     (block-parent block) continuation
     context (context/make-internal)
     (scode/quotation-expression (car operands))
     #f
     (make-reference block
		     (find-name block
				(scode/variable-name (cadr operands)))
		     #f))))

(define (generate/delay block continuation context expression)
  (generate/combination
   block continuation context
   (scode/make-combination
    (ucode-primitive system-pair-cons)
    (list (ucode-type delayed)
	  0
	  (scode/make-lambda scode-lambda-name:unnamed '() '() #f '() '()
			     (scode/delay-expression expression))))))

(define (generate/error-combination block continuation context expression)
  (scode/error-combination-components expression
    (lambda (message irritants)
      (generate/combination
       block continuation context
       (scode/make-combination compiled-error-procedure
			       (cons message irritants))))))

(define (generate/quotation block continuation context expression)
  (generate/combination
   block continuation context
   (scode/make-combination
    (ucode-primitive system-pair-car)
    (list (cons constant-quotation-tag expression)))))

(define (generate/constant-quotation block continuation context expression)
  context				; ignored
  (continue/rvalue-constant block
			    continuation
			    (make-constant (cdr expression))))

(define constant-quotation-tag
  "constant-quotation")

;;;; Dispatcher

(define generate/expression
  (let ((dispatch-vector
	 (make-vector (microcode-type/code-limit) generate/constant))
	(generate/combination
	 (lambda (block continuation context expression)
	   (let ((operator (scode/combination-operator expression))
		 (operands (scode/combination-operands expression)))
	     (cond ((and (eq? operator (ucode-primitive lexical-unassigned?))
			 (scode/the-environment? (car operands))
			 (scode/symbol? (cadr operands)))
		    (generate/unassigned? block continuation
					  context expression))
		   ((and (or (eq? operator (ucode-primitive error-procedure))
			     (and (scode/absolute-reference? operator)
				  (eq? (scode/absolute-reference-name operator)
				       'ERROR-PROCEDURE)))
			 (let ((irritants (cadr operands)))
			   (or (null? irritants)
			       (and (scode/absolute-combination? irritants)
				    (eq? (scode/absolute-combination-name
					  irritants)
					 'LIST))
			       (and (scode/combination? irritants)
				    (eq? (scode/combination-operator irritants)
					 (ucode-primitive cons))))))
		    (generate/error-combination block continuation
						context expression))
		   (else
		    (generate/combination block continuation
					  context expression))))))
	(generate/pair
	 (lambda (block continuation context expression)
	   (cond ((eq? (car expression) safe-variable-tag)
		  (generate/safe-variable block continuation
					  context expression))
		 ((eq? (car expression) constant-quotation-tag)
		  (generate/constant-quotation block continuation
					       context expression))
		 (else
		  (generate/constant block continuation
				     context expression))))))
    (letrec-syntax
	((dispatch-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     `(VECTOR-SET! DISPATCH-VECTOR
			   (MICROCODE-TYPE ',(cadr form))
			   ,(close-syntax (caddr form) environment)))))
	 (dispatch-entries
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((handler (close-syntax (caddr form) environment)))
	       `(BEGIN
		  ,@(map (lambda (type)
			   `(DISPATCH-ENTRY ,type ,handler))
			 (cadr form)))))))
	 (standard-entry
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((name (cadr form)))
	       `(DISPATCH-ENTRY ,name
				,(close-syntax (symbol 'GENERATE/ name)
					       environment)))))))
      (standard-entry access)
      (standard-entry assignment)
      (standard-entry conditional)
      (standard-entry definition)
      (standard-entry delay)
      (standard-entry disjunction)
      (standard-entry pair)
      (standard-entry quotation)
      (standard-entry the-environment)
      (standard-entry variable)
      (dispatch-entries (lambda lexpr extended-lambda) generate/lambda)
      (dispatch-entry sequence generate/sequence)
      (dispatch-entry combination generate/combination)
      (dispatch-entry comment generate/comment))
    (named-lambda (generate/expression block continuation context expression)
      ((vector-ref dispatch-vector (object-type expression))
       block continuation context expression))))