#| -*-Scheme-*-

$Id: 6ad8b010e651f0ceedab380289c8cc4ca45861b9 $

Copyright (c) 1994-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; ??
;;; package: (compiler midend)
;;
;;  Dataflow Interface
;;
;;  Client programs may access some of fields of a NODE:
;;    NODE/UNIQUE-VALUE
;;    NODE/USES/OPERATOR
;;    NODE/USES/OPERAND
;;    NODE/FORMAL-PARAMETER?
;;    NODE/THE-PROCEDURE-VALUE
;;  Temporarily, until we build better abstractions for the various kinds
;;  of node:
;;    NODE/TEXT
;;    NODE/NAME
;;
;;  Interface for graphs:
;;
;;    GRAPH/PROGRAM
;;    GRAPH/CLOSURES
;;    GRAPH/TEXT->NODE
;;

(declare (usual-integrations))

(define *dataflow-report-applied-non-procedures?* #T)
(define *node-count*)

;; If set to a number, dataflow/top-level declines to do the dataflow on
;; a graph containing more than *maximum-node-count*, and returns #F
;; instead of the graph.
(define *maximum-node-count* #F)
;;(define *maximum-node-count* 10000)
(define *warning-node-count* 10000)

(define (dataflow/top-level program)

  (define (do-dataflow)
    (let* ((env          (dataflow/make-env))
	   (graph        (make-graph program))
	   (result-node  (dataflow/expr env graph program)))
      (fluid-let ((*node-count* (graph/node-count graph)))
	(sample/1 '(dataflow/graph-size histogram) *node-count*)
	(if (and *maximum-node-count*
		 (> *node-count* *maximum-node-count*))
	    (begin
	      (internal-warning
	       "Graph too big" graph 'has *node-count* 'nodes
	       `(*maximum-node-count* is ,*maximum-node-count*))
	      #F)
	    (begin
	      (if result-node
		  (initial-link-nodes! result-node (graph/escape-node graph)))
	      (dataflow/make-globals-escape! env graph)
	      (if (> (graph/node-count graph) *warning-node-count*)
		  (user-warning
		   "Big dataflow graph"
		   (graph/node-count graph) 'nodes
		   `(*maximum-node-count* is ,*maximum-node-count*)))

	      (graph/initialize-links! graph)
	      (graph/dataflow! graph)
	      (graph/cleanup! graph)
	      
	      ;;(graph/substitite-simple-constants
	      ;; graph graph/read-eqv?-preserving-constant?)
	      (graph/compiled-procedure-reductions graph)

	      graph)))))

  (let ((graph  (with-abort-restart  do-dataflow)))
    (or graph
	(begin
	  (user-warning "Dataflow graph too large to fit in heap")
	  #F))))

(define (with-abort-restart thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart
      'ABORT
      "Abort Dataflow"
      (lambda (#!optional message)
	(continuation #F))
      values
      thunk))))

(define-macro (define-dataflow-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'DATAFLOW/ keyword)))
    (call-with-values
	(lambda ()
	  (%matchup (cdddr bindings) '(handler env graph form) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons* (car bindings) (cadr bindings) 'FORM names)
			    ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV GRAPH FORM)
	       (LET ((RESULT ,code))
		 (GRAPH/ASSOCIATE! GRAPH FORM RESULT)
		 RESULT))))))))

;; handler: env x graph! x fields -> node


(define-dataflow-handler LOOKUP (env graph form name)
  (let* ((reference-node  (dataflow/name->node env graph name))
         (result-node     (graph/add-expression-node! graph form name)))
    (if (not reference-node)
        (internal-error "LOOKUP: Cant find:" name))
    (initial-link-nodes! reference-node result-node)
    result-node))


(define-dataflow-handler SET! (env graph form name expr)
  ;; This version models the MIT scheme SET! form which returns the
  ;; previous value of the binding.
  (let ((expr-node    (dataflow/expr env graph expr))
        (name-node    (dataflow/name->node env graph name))
        (result-node  (graph/add-expression-node! graph form "#[set!-result]")))
    (initial-link-nodes! expr-node name-node)
    (initial-link-nodes! name-node result-node)
    result-node))


(define-dataflow-handler DEFINE (env graph form name expr)
  ;; DEFINE is like SET!, except that the value is unspecified previous
  ;; value of the binding.  The node for the name is in the
  ;; environment because it is put there by scanning for defines in
  ;; BEGIN.
  form					; ignore
  (let ((expr-node    (dataflow/expr env graph expr))
        (name-node    (dataflow/name->node env graph name)))
    (initial-link-nodes! expr-node name-node)
    #F))

(define (dataflow/name->node env graph name)
  ;; Lookup name, possibly creating a global node for the name if it is
  ;; global and we do not yet know about the name.  In this case we
  ;; ensure that the value escapes (some other program may copy the
  ;; variable) and the values comming from that variable are unknown
  ;; (some other program may set the variable).
  (let* ((binding     (dataflow/env/lookup env name))
         (ref-node    (or
                       (and binding (dataflow/binding/value binding))
                       (let ((value (graph/add-location-node! graph
							      'global-variable
							      name)))
                         (dataflow/env/define-global! env name value)
                         value))))
    ref-node))


;; A distinction is made between before and after CPS conversion.  After
;; CPS conversion procedures do not `return' results, so we need not
;; create nodes for the procedure results.  It is important not to
;; create these nodes for performance reasons because they all form a
;; huge equivalence class.

(define-dataflow-handler LAMBDA (env graph form lambda-list body)
  (let* ((input-names    (lambda-list->names lambda-list))
         (input-nodes    (map (lambda (name) (graph/add-location-node! graph form name))
                              input-names))
         (body-node      (dataflow/expr (dataflow/env/push-frame env
                                                                 input-names
                                                                 input-nodes)
                                        graph body))
         (result-node    (and body-node
			      (graph/add-expression-node!
			       graph form "#[procedure-result]")))
         (procedure-node (graph/add-location-node! graph form
						   "#[procedure-value]"))
         (value          (graph/add-procedure!
                          graph form input-nodes result-node)))
    (if (eq? body-node #F)
	(if *after-cps-conversion?*
	    'ok
	    (error "CPS procedure returns result " form))
	(if *after-cps-conversion?*
	    (error "Pre-CPS procedure returns not result " form)
	    (initial-link-nodes! body-node result-node)))
    (add! procedure-node value node/initial-values set-node/initial-values!)
    procedure-node))



(define-dataflow-handler LET (env graph form bindings body)
  (dataflow/let-like-handler "#[let-result]" #F
                             env graph form bindings body))

(define-dataflow-handler LETREC (env graph form bindings body)
  (dataflow/let-like-handler "#[letrec-result]" #T
                             env graph form bindings body))

(define (dataflow/let-like-handler result-name recursive?
                                   env graph form bindings body)
  (let* ((binding-names   (map (lambda (x) (car x)) bindings))
         (binding-exprs   (map (lambda (x) (second x)) bindings))
         (binding-nodes   (map (lambda (name) (graph/add-location-node! graph form name))
                               binding-names))
         (inner-env       (dataflow/env/push-frame env binding-names binding-nodes))
         (expr-nodes      (dataflow/expr* (if recursive? inner-env env)
                                          graph binding-exprs))
         (body-node       (dataflow/expr inner-env graph body))
         (result-node     (and body-node
			       (graph/add-expression-node! graph form result-name))))

    (map initial-link-nodes! expr-nodes binding-nodes)
    (if result-node
	(initial-link-nodes! body-node result-node))
    result-node))


(define-dataflow-handler QUOTE (env graph form object)
  env object				; ignore
  (graph/add-constant-node! graph form))


(define-dataflow-handler DECLARE (env graph form #!rest anything)
  env graph                             ; ignored
  form anything
  'declaration-does-not-have-a-node)


(define-dataflow-handler BEGIN (env graph form #!rest actions)
  ;; Only top-level BEGINs contain DEFINEs but this code show work for
  ;; internal defines too, had they not been converted to #!AUXes and
  ;; then (a little later) LET[REC]s
  (dataflow/scan-defines! actions env graph)
  (let* ((nodes        (dataflow/expr* env graph actions))
         (last-node    (car (last-pair nodes)))
         (result-node  (and last-node
			    (graph/add-expression-node! graph form
							"#[begin-result]"))))
    (if (node? result-node)
	(initial-link-nodes! last-node result-node))
    result-node))


(define (dataflow/scan-defines! forms env graph)
  (define names '())
  (define defines '())
  (define (scan forms)
    (cond ((null? forms)
           unspecific)
          ((not (and (pair? forms) (pair? (car forms))))
           (user-error "scan-defines - not legal KMP scheme: " forms))
          ((eq? (caar forms) 'BEGIN)
           (dataflow/scan-defines! (cdr (car forms)) env graph)
           (scan (cdr forms)))
          ((eq? (caar forms) 'DEFINE)
           (set! names (cons (second (car forms)) names))
           (set! defines (cons (car forms) defines))
           (scan (cdr forms)))
          (else
           (scan (cdr forms)))))
  (scan forms)
  (dataflow/env/extend-frame!
   env
   names
   (map (lambda (name defn) (graph/add-location-node! graph defn name))
	names defines)))


(define-dataflow-handler IF (env graph form pred conseq alt)
  (let ((predicate-node    (dataflow/expr env graph pred))
        (consequent-node   (dataflow/expr env graph conseq))
        (alternative-node  (dataflow/expr env graph alt)))
    (let ((result-node       (and consequent-node
				  alternative-node
				  (graph/add-expression-node! graph form
							      "#[if-result]"))))
      predicate-node			; unused
      (cond ((node? result-node)
	     (initial-link-nodes! consequent-node result-node)
	     (initial-link-nodes! alternative-node result-node))
	    ((or (node? consequent-node)
		 (node? alternative-node))
	     (internal-error "Mismatch between CPS states of branches"
			     consequent-node alternative-node form)))
      result-node)))


(define-dataflow-handler OR (env graph form pred alt)
  (let ((predicate-node    (dataflow/expr env graph pred))
        (alternative-node  (dataflow/expr env graph alt))
        (result-node       (graph/add-expression-node! graph form
						       "#[or-result]")))
    ;; No worry about CPS style as OR is removed before CPS conversion
    (initial-link-nodes! predicate-node result-node)
    (initial-link-nodes! alternative-node result-node)
    result-node))


(define-dataflow-handler ACCESS (env graph form name env-expr)
  (let* ((env-node       (dataflow/expr env graph env-expr))
         (result-node    (graph/add-expression-node! graph form
						     "#[access-result]")))
    ;; IF env is system-global-environment and the name is standard (like
    ;; cons, car, apply, append etc) we should do something better.
    env-node name
    (initial-link-nodes! (graph/unknown-input-node graph) result-node)
    result-node))


(define-dataflow-handler CALL (env graph form rator cont #!rest rands)
  (let* ((special-result
	  (dataflow/handler/special-call env graph form rator cont rands))
	 (result
	  (if (eq? special-result 'ORDINARY)
	      (dataflow/handler/ordinary-call env graph form rator cont rands)
	      special-result)))

    (if (and ;;(not (LAMBDA/? rator))
	     (or (and (node? result)	   (not (equal? cont '(QUOTE #F))))
		 (and (not (node? result)) (equal? cont '(QUOTE #F)))))
	(internal-error "result/CPS mismatch" result form))

   result))

(define (dataflow/handler/ordinary-call  env graph form rator cont rands)
  (let* ((operator-node  (dataflow/expr env graph rator))
         (operand-nodes  (dataflow/expr* env graph rands))
	 (direct-style?  (equal? cont '(QUOTE #F)))
	 (cont-node      (if direct-style? #F (dataflow/expr env graph cont)))
	 (has-value?     (and direct-style?)
			      ;;(not (and (LAMBDA/? rator)
			      ;;	*after-cps-conversion?*))
			 )
         (result-node    (if has-value?
			     (graph/add-expression-node! graph form
							 "#[call-result]")
			     (graph/add-location-node!  graph form
							"#[call-result]"))))
    (graph/add-application! graph form
			    operator-node
			    (cons cont-node operand-nodes)
			    result-node)
    (and has-value? result-node)))


(define (dataflow/handler/special-call  env graph form rator cont rands)
  (if (QUOTE/? rator)
      (let ((operator (quote/text rator)))
        (define (use method) (method env graph form rator cont rands))
        (cond ((eq? operator %make-heap-closure)
               (use dataflow/handler/%make-heap-closure))
              ((eq? operator %make-stack-closure)
               (use dataflow/handler/%make-stack-closure))
              ((eq? operator %make-trivial-closure)
               (use dataflow/handler/%make-trivial-closure))
              ((eq? operator %heap-closure-ref)
               (use dataflow/handler/%heap-closure-ref))
              ((eq? operator %stack-closure-ref)
               (use dataflow/handler/%stack-closure-ref))
	      ((eq? operator %heap-closure-set!)
	       (use dataflow/handler/%heap-closure-set!))
              ((or (eq? operator %internal-apply)
		   (eq? operator %internal-apply-unchecked))
               (use dataflow/handler/%internal-apply))
	      ((eq? operator %fetch-stack-closure)
               (use dataflow/handler/%fetch-stack-closure))
	      ((eq? operator %fetch-continuation)
               (use dataflow/handler/%fetch-continuation))
              ((eq? operator %invoke-continuation)
               (use dataflow/handler/%invoke-continuation))
	      ;;((eq? operator %invoke-operator-cache)
	      ;; (use dataflow/handler/%invoke-operator-cache))
              (else 
               'ORDINARY)))
      'ORDINARY))


(define (dataflow/handler/%make-heap-closure env graph form rator cont rands)
  ;; (CALL ',%make-heap-closure '#F <lambda-expression> '#(name*) <value>*)
  ;;       -------rator-------- cont ---------------rands---------------
  ;;
  rator cont				; ignore
  (let* ((lambda-expr   (first rands))
         (value-exprs   (cddr rands))
         (closure-name  (second (second lambda-expr))) ; (LAMBDA (k <this> ...))
         (names-vector  (second (second rands)))

         (lambda-node   (dataflow/expr env graph lambda-expr))
         (expr-nodes    (dataflow/expr* env graph value-exprs))
         (closed-names  (map (lambda (name) (cons closure-name name))
			     (vector->list names-vector)))
         (closed-nodes  (map (lambda (name) (graph/add-location-node!
					     graph form name))
			     closed-names))
	 (procedure     (node/the-procedure-value lambda-node))
         (closure-node  (graph/add-expression-node! graph form
						    "#[heap-closure-value]"))
         (value         (graph/add-closure! graph
                                            form
                                            'HEAP
                                            procedure
                                            names-vector
                                            (list->vector closed-nodes)
                                            closure-node)))
    
    (map initial-link-nodes! expr-nodes closed-nodes)
    (add! closure-node value node/initial-values set-node/initial-values!)
    (graph/add-special-application!  graph form
				     %make-heap-closure
				     expr-nodes
				     '() ; no triggers
				     closure-node)
    closure-node))



(define dataflow/?closure-elts (->pattern-variable 'CLOSURE-ELTS))
(define dataflow/?closure-vector (->pattern-variable 'CLOSURE-VECTOR))
(define dataflow/?cont (->pattern-variable 'CONT))
(define dataflow/?expr1 (->pattern-variable 'EXPR1))
(define dataflow/?expr2 (->pattern-variable 'EXPR2))
(define dataflow/?args (->pattern-variable 'ARGS))
(define dataflow/?frame-var (->pattern-variable 'FRAME-VAR))
(define dataflow/?nrands (->pattern-variable 'NRANDS))
(define dataflow/?rands (->pattern-variable 'RANDS))
(define dataflow/?lam-expr (->pattern-variable 'LAM-EXPR))

(define dataflow/stack-closure-pattern
  `(CALL (QUOTE ,%make-stack-closure)
	 (QUOTE #F)
	 (LAMBDA (,dataflow/?cont ,@dataflow/?args)
	   (LET ((,dataflow/?frame-var ,dataflow/?expr1))
	     ,dataflow/?expr2))
	 (QUOTE ,dataflow/?closure-vector)
	 . ,dataflow/?closure-elts))

(define dataflow/implicit-stack-frame 
  (generate-uninterned-symbol "*STACK-FRAME*"))

(define (dataflow/handler/%make-stack-closure env graph form rator cont rands)
  ;; (CALL ',%make-stack-closure '#F <lambda-expression> '#(name*) <value>*)
  ;;       -------rator-------- cont ---------------rands---------------
  ;; push a frame on the environment for the closed-over variables.  The
  ;; variables are named as pairs (closure . variable)
  rator cont				; ignore
  (let* ((lambda-expr   (first  rands))
	 (parts         (form/match dataflow/stack-closure-pattern form))
         (closure-name  (cadr (assq dataflow/?frame-var parts)))
         (names-vector  (cadr (assq dataflow/?closure-vector parts)))
         (value-exprs   (cadr (assq dataflow/?closure-elts parts)))
         (closed-names  (map (lambda (name) (cons closure-name name))
                             (vector->list names-vector)))
         (closed-nodes  (map (lambda (name) (graph/add-location-node!
					     graph form name))
                             closed-names))
         (closure-node  (graph/add-expression-node! graph form
						    "#[stack-closure-value]"))
         (inner-env     (dataflow/env/push-frame env
						 (list dataflow/implicit-stack-frame)
						 (list closure-node)))
         (lambda-node   (dataflow/expr inner-env graph lambda-expr))
	 (procedure     (node/the-procedure-value lambda-node))
         (expr-nodes    (dataflow/expr* env graph value-exprs))
         (value         (graph/add-closure! graph
                                            form
                                            'STACK
                                            procedure
                                            names-vector
                                            (list->vector closed-nodes)
                                            closure-node)))
    
    (map initial-link-nodes! expr-nodes closed-nodes)
    (add! closure-node value node/initial-values set-node/initial-values!)
    (graph/add-special-application!  graph form
				     %make-stack-closure
				     expr-nodes
				     '() ; no triggers
				     closure-node)
    closure-node))

(define (dataflow/handler/%fetch-stack-closure env graph form rator cont rands)
  ;; (CALL ',%fetch-stack-closure '#F '<names-vector>)
  ;;       --------rator--------- cont ----rands-----
  ;;
  rator cont rands			; ignore
  (let* ((closure-node   
	  (dataflow/name->node env graph dataflow/implicit-stack-frame))
         (result-node
	  (graph/add-expression-node! graph form
				      "#[fetch-stack-closure-result]")))
    (initial-link-nodes! closure-node result-node)
    result-node))


(define (dataflow/handler/%make-trivial-closure env graph form rator cont rands)
  ;; (CALL ',%make-trivial-closure '#F <lambda-expression or LOOKUP>)
  ;;       --------rator---------- cont -----------rands------------
  ;; Initially we add the closure with a NODE for the procedure part.
  ;; After initial value propagation replace this with the procedure value.
  rator cont				; ignore
  (define (finish lambda-node)
    (let* ((closure-node (graph/add-expression-node! graph form
						     "#[trivial-closure]"))
           (value        (graph/add-closure! graph
                                             form
                                             'TRIVIAL
                                             lambda-node ;procedure
                                             #()
                                             #()
                                             closure-node)))
      (add! closure-node value node/initial-values set-node/initial-values!)
      closure-node))

  (let* ((procedure-expr   (first  rands)))
    (cond ((LOOKUP/? procedure-expr)
           ;; This occurs in a really wierd screw-case, documented elsewhere.
	   ;; The  <name> in (LOOKUP <name>) is bound to the lambda, so we
           ;; can find the lambda node by searching the initial links
           ;; backwards
	   (finish
	    (dataflow/name->node env graph (lookup/name procedure-expr))))
	  ((LAMBDA/? procedure-expr)
	   (finish (dataflow/expr env graph procedure-expr)))
	  (else
	   (internal-error "Procedure is neither LAMBDA nor LOOKUP" form)))))


(define (graph/initialize-closure-procedures! graph)
  (define (fix-closure-procedure closure)
    (if (eq? 'TRIVIAL (value/closure/kind closure))
	(let ((proc-node (value/closure/procedure closure)))
	  (set-value/closure/procedure! closure
					(node/the-procedure-value proc-node)))))
  (for-each-item fix-closure-procedure (graph/closures graph)))


(define (dataflow/handler/%heap-closure-ref env graph form rator cont rands)
  ;; (CALL ',%heap-closure-ref '#F  <closure> <offset> 'NAME)
  ;;       -------rator------- cont ---------------rands---------------
  ;; <closure> is always (LOOKUP closure-name)
  rator cont				; ignore
  (let* ((closure-node  (dataflow/expr env graph (first rands)))
         (result-node   (graph/add-expression-node! graph form
						    (second (third rands)))))

    (graph/add-special-application!  graph form
				     %heap-closure-ref
				     (list closure-node)
				     (list closure-node)
				     result-node)
    result-node))


(define (dataflow/handler/%heap-closure-set! env graph form rator cont rands)
  ;; (CALL ',%heap-closure-set! '#F <closure> <offset> <value> 'NAME)
  ;;       -------rator------- cont ---------------rands---------------
  ;; <closure> is always (LOOKUP closure-name)
  rator cont rands			; ignore
  (let* ((closure-node
	  (dataflow/expr env graph (call/%heap-closure-set!/closure form)))
	 (value-node
	  (dataflow/expr env graph (call/%heap-closure-set!/value form)))
         (result-node
	  (graph/add-expression-node! graph form
				      (quote/text
				       (call/%heap-closure-set!/name form)))))

    (graph/add-special-application!  graph form
				     %heap-closure-set!
				     (list closure-node value-node)
				     (list closure-node)
				     result-node)
    result-node))


(define (dataflow/handler/%stack-closure-ref env graph form rator cont rands)
  ;; (CALL ',%stack-closure-ref '#F  <closure> <offset> 'NAME)
  ;;       -------rator------- cont ---------------rands---------------
  ;; <closure> is always (LOOKUP closure-name)
  rator cont				; ignore
  (let* ((closure-node  (dataflow/expr env graph (first rands)))
         (result-node   (graph/add-expression-node! graph form
						    (second (third rands)))))

    (graph/add-special-application!  graph form
				     %stack-closure-ref
				     (list closure-node)
				     (list closure-node)
				     result-node)
    result-node))


(define (dataflow/handler/%internal-apply env graph form rator cont rands)
  ;; (CALL ',%internal-apply           <cont> 'NARGS <procedure> <value>*)
  ;; (CALL ',%internal-apply-unchecked <cont> 'NARGS <procedure> <value>*)
  ;;       ------rator------           -cont- ----------rands------------
  ;;
  ;; Treated like a normal call
  rator					; ignore
  (let* ((operator-node  (dataflow/expr env graph (second rands)))
         (operand-nodes  (dataflow/expr* env graph (cddr rands)))
	 (direct-style?  (equal? cont '(QUOTE #F)))
	 (cont-node      (if direct-style? #F (dataflow/expr env graph cont)))
         (result-node    (if direct-style?
			     (graph/add-expression-node!
			      graph form "#[internal-apply-result]")
			     (graph/add-location-node!
			      graph form "#[internal-apply-result]"))))

    (graph/add-application! graph form operator-node (cons cont-node operand-nodes) result-node)
    (and direct-style? result-node)))

(define (dataflow/handler/%fetch-continuation env graph form rator cont rands)
  ;; (CALL ',%fetch-continuation '#F)
  ;;       --------rator--------  cont --no rands--
  ;;
  env rator cont rands			; ignore
  (let* ((result-node 
	  (graph/add-expression-node! graph form
				      "#[fetch-continuation-result]"))
         (value        (value/make-unknown 'top-level-continuation)))
    (add! result-node value node/initial-values set-node/initial-values!)
    result-node))

(define (dataflow/handler/%invoke-continuation env graph form rator cont rands)
  ;; (CALL ',%invoke-continuation <continuation> <value>*)
  ;;       --------rator--------  -----cont----- --rands--
  ;; The continuation could be anything and invoking it is like an
  ;; application, but it will ignore its own continuation parameter

  rator					; ignore
  (let* ((operator-node      (dataflow/expr env graph cont))
         (operand-nodes      (dataflow/expr* env graph rands))
	 (bogus-continuation #F) ;(graph/add-constant-node! graph `(QUOTE #F))
         (result-node    #F))
    
    (graph/add-application! graph form operator-node 
			    (cons bogus-continuation operand-nodes)
			    result-node)
    result-node))


;;(define (dataflow/handler/%invoke-operator-cache env graph form rator cont rands)
;;  ;; (CALL ',%invoke-operator-cache <continuation>
;;  ;;       '(NAME NARGS) <operator-cache> <value>*)
;;  ;;       ---------------rands--------------------
;;  rator
;;  (let* ((cont-node   (dataflow/expr env graph cont))
;;	 (expr-nodes  (dataflow/expr* env graph (cddr rands)))
;;	 (result-node (graph/add-node! graph form
;;				       "#[invoke-operator-cache-result]"))
;;	 (escape-node (graph/escape-node graph)))
;;    (for-each (lambda (node)
;;		(initial-link-nodes! node escape-node))
;;      expr-nodes)
;;    (initial-link-nodes! cont-node escape-node)
;;    (initial-link-nodes! (graph/unknown-input-node graph) result-node)
;;    result-node))



(define (dataflow/expr env graph expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (dataflow/quote env graph expr))
    ((LOOKUP)
     (dataflow/lookup env graph expr))
    ((LAMBDA)
     (dataflow/lambda env graph expr))
    ((LET)
     (dataflow/let env graph expr))
    ((DECLARE)
     (dataflow/declare env graph expr))
    ((CALL)
     (dataflow/call env graph expr))
    ((BEGIN)
     (dataflow/begin env graph expr))
    ((IF)
     (dataflow/if env graph expr))
    ((LETREC)
     (dataflow/letrec env graph expr))
    ((OR)
     (dataflow/or env graph expr))
    ((SET!)
     (dataflow/set! env graph expr))
    ((DEFINE)
     (dataflow/define env graph expr))
    ((ACCESS)
     (dataflow/access env graph expr))
    ((UNASSIGNED? DELAY
     IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (dataflow/expr* env graph exprs)
  (map (lambda (expr)
	 (dataflow/expr env graph expr))
       exprs))

(define (dataflow/remember new old)
  old                                   ; ignored for now
  new)

(define (dataflow/new-name prefix)
  (new-variable prefix))


(define-structure (dataflow/binding
                   (conc-name dataflow/binding/)
		   (print-procedure
		    (standard-unparser-method 'DATAFLOW/BINDING
		      (lambda (binding port)
			(write-char #\Space port)
			(write (dataflow/binding/name binding) port)))))
  (name  false read-only true)
  (value false read-only false))

(define (dataflow/make-env) (cons '() '()))

(define (dataflow/env/lookup env name)
  (let spine-loop ((env env))
    (and (not (null? env))
         (let rib-loop ((rib (car env)))
           (cond ((null? rib)
                  (spine-loop (cdr env)))
                 ((name-eq? name (dataflow/binding/name (car rib)))
                  (car rib))
                 (else
                  (rib-loop (cdr rib))))))))

(define-integrable (name-eq? name1 name2)
  (let ((name1 name1)
	(name2 name2))
    (or (eq? name1 name2)
	(and (pair? name1)
	     (pair? name2)
	     (eq? (car name1) (car name2))
	     (eq? (cdr name1) (cdr name2))))))

(define-integrable dataflow/binding/make make-dataflow/binding)

(define (dataflow/env/push-frame env names values)
  (cons (map make-dataflow/binding names values)
        env))

(define (dataflow/env/extend-frame! env names values)
  (set-car! env (append! (car env)
                         (map make-dataflow/binding names values)))
  env)

(define (dataflow/env/global-environment env)
  (let spine-loop ((env env))
    (if (null? (cdr env))
        env
        (spine-loop (cdr env)))))

(define (dataflow/env/define-global! env name value)
  (let ((env  (dataflow/env/global-environment env)))
    (set-car! env (cons (dataflow/binding/make name value) (car env)))))

(define (dataflow/env/for-each-global-binding procedure env)
  (map procedure (car (dataflow/env/global-environment env))))

;;; Data flow graph
;;
;;  There are two prinicipal kinds of things: NODEs which represent a
;;  place in te program, and VALUE-SETs which represent the set of
;;  values that may be the value of a particular expression identified
;;  by the node.
;;
;;  Nodes are either of class LOCATION, being an abstract storage location
;;  (e.g. a formal parameter or closure `slot', or of class
;;  EXPRESSION, for nodes that correspond directly to the source.)  It
;;  might be possible to store this value implicitly in terms of the
;;  text an name fields.
;;
(define-structure
  (node
   (conc-name node/)
   (constructor %make-node))
  number				; each node is numbered
  text                                  ; source code
  name                                  ; name with source code
  initial-values                        ; list of initial values
  initial-links-in
  initial-links-out
  values                                ; value-set intermediate & final values
  links-in                              ; nodes which sink values to here
  links-out                             ; nodes which source values from here
  connectivity				; data structure for efficient
					; predicate for membership in links-in
  uses/operator                         ; applications with this node as operator
  uses/operand                          ; applications with this node as operand
                                        ; or continuation
  uses/trigger				; graph computations that should be
					; reconsidered when the values change
  class					; LOCATION or EXPRESSION
  )

;; Note: node/name is either a string or a symbol.  Strings are used to
;; name otherwise unnamed places, like the result of an IF.  Symbols
;; are used for names which occur in the program.  LAMBDA-parameters
;; and LET-bindings have the parameter/binding name as node/name and
;; the binding form (i.e. the LAMBDA expression or LET expression) as
;; node/text.  Thus we can distinguish the nodes representing:
;;  . The LAMBDA parameter X (name=x, text=(LAMBDA (... X ...) ...))
;;  . The LAMBDA expression  (name="#[procedure-expression]", text=(LAMBDA...))
;;  . The value returned by the procedure 
;;                           (name="#[procedure-result]", text=(LAMBDA...))
;;  . The LET binding X (name=x, text=(LET (... (X ...) ...) ...))
;;  . The LET expresion result (name="...", text=(LET (... (X ...) ...) ...))

(define (%graph/make-node graph text name class)
  graph
  (let ((node  (%make-node (graph/node-count graph)
			   text
                           name         ; name
                           '()          ; initial-values
                           '()          ; initial-links-in
                           '()          ; initial-links-out
                           'NOT-CACHED  ; values
                           (make-empty-node-set) ; links-in
                           (make-empty-node-set) ; links-out
			   #F		; connectivity
                           '()          ; uses/operator
                           '()          ; uses/operand
                           '()          ; uses/trigger
			   class
                           )))
    (set-graph/node-count! graph (+ (graph/node-count graph) 1))
    node))


(define (initial-link-nodes! from to)
  (add! to   from node/initial-links-in  set-node/initial-links-in!)
  (add! from to   node/initial-links-out set-node/initial-links-out!))

(define (node/the-constant-value node)
  (if (not (and (pair? (node/initial-values node))
                (null? (cdr (node/initial-values node)))
                (value/constant? (car (node/initial-values node)))))
      (internal-error "Constant node does not have unique value" node)
      (value/constant/value (car (node/initial-values node)))))

(define (node/the-procedure-value node)
  (define (bad)
    (internal-error "Node does not have an initial known procedure value" node))
  (if (null? (node/initial-values node))
      (let ((values (value-set/new-singletons (node/values node))))
	(if (and (pair? values)
		 (null? (cdr values))
		 (value/procedure? (car values)))
	    (car values)
	    (bad)))
      (if (and (pair? (node/initial-values node))
	       (null? (cdr (node/initial-values node)))
	       (value/procedure? (car (node/initial-values node))))
	  (car (node/initial-values node))
	  (bad))))

(define (node/unique-value node)
  (value-set/unique-value (node/values node)))

(define (node/formal-parameter? node)
  (and (pair? (node/text node))
       (eq? (car (node/text node)) 'LAMBDA)
       (symbol? (node/name node))))


(define (expression-node? node)
  (eq? (node/class node) 'EXPRESSION))

(define (location-node? node)
  (eq? (node/class node) 'LOCATION))


;;
;; Values
;;

;;(define-structure (value
;;                   named
;;                   (type vector)
;;                   (conc-name value/)
;;                   ;(constructor %make-value)
;;                   (predicate %value?))
;;  text                                  ; source code resulting in this value
;;  nodes					; nodes which get this value
;;  )

(define value/subtypes '())

(define-macro (define-value structure-description . slots)
  (let ((name               (car structure-description))
	(structure-options  (cdr structure-description)))

    `(BEGIN
       (DEFINE-STRUCTURE
	 (,name
	  NAMED (TYPE VECTOR)
	  . ,structure-options)
	 TEXT				; source code resulting in this value
	 NODES				; nodes which get this value
	 . ,slots)
       (SET! VALUE/SUBTYPES (CONS ,name VALUE/SUBTYPES)))))


;;(define (value? structure)
;;  (and (vector? structure)
;;       (memq (vector-ref structure 0) value/subtypes)))

(define-integrable (value/text structure)   (vector-ref structure 1))
(define-integrable (value/nodes structure)  (vector-ref structure 2))

;;(define-integrable (set-value/text! structure x)  (vector-set! structure 1 x))
(define-integrable (set-value/nodes! structure x) (vector-set! structure 2 x))

;;(define-integrable (value/initialize! value text)
;;  (set-value/text! value text)
;;  (set-value/nodes! value '())
;;  value)


(define-value (value/constant
	       (conc-name value/constant/)
	       (constructor %value/make-constant))
  ;; No extra fields
  )

(define (value/make-constant text)
  (%value/make-constant text '() ))

(define (value/constant/value constant)
  ;; get the quoted thing
  (second (value/text constant)))


(define-value (value/procedure
	       (conc-name value/procedure/)
	       (constructor %value/make-procedure))
  ;; Nodes for arguments and auxes.  We distinguish them by looking at the
  ;; lambda list of the text slot:
  input-nodes
  result-node                           ; node for result value of procedure
  )

(define (value/make-procedure text input-nodes result-node)
  (%value/make-procedure text '()
			 input-nodes result-node))

(define (value/procedure/lambda-list procedure-value)
  (second (value/text procedure-value)))


(define-value
  (value/closure
   (conc-name value/closure/)
   (constructor %value/make-closure)
   (print-procedure
    (standard-unparser-method 'VALUE/CLOSURE
      (lambda (value port)
	(write-char #\space port)
	(write (value/closure/kind value) port)))))

  kind                                  ; 'HEAP or 'STACK or 'TRIVIAL
  procedure                             ; a procedure value (lambda (k self ..))
  location-names                        ; vector of symbols
  location-nodes                        ; nodes for closed-over values
  ;; the SELF-NODE has this closure as its initial (and only) value
  self-node
  ;; CALL-SITES is a list of applications and symbols.  Symbols denote
  ;; external known call sites, for example, the continuation
  ;; invocation implicit in &<.
  call-sites
  ;; ESCAPES? is #T or #F.  To cause the closure to escape, link the node
  ;; to the escape node (or add the value to the escape node's value
  ;; set).  This will cause the closure to be applied to unknown
  ;; values.  Setting this bit marks the closure as escaped, which
  ;; might be useful if the closure partially escapes, for example, as
  ;; a continuation of a known but not inlined primitive.
  escapes?				; #T or #F
  )

(define (value/closure/trivial? closure)
  (eq? (value/closure/kind closure) 'TRIVIAL))


(define (value/make-closure text        ; e.g. (CALL '#[make-heap-closure] ...)
                            kind
                            procedure
                            location-names ; vector
                            location-nodes ; vector
                            self-node
                            )
  (%value/make-closure text '()
		       kind procedure location-names
		       location-nodes self-node
		       '()
		       #F))

(define (value/closure/lookup-location-node closure name)
  (let*  ((names  (value/closure/location-names closure))
          (n      (vector-length names)))
    (let loop ((i 0))
      (cond ((>= i n)  (internal-error "Non-closed name" name closure))
            ((eq? (vector-ref names i) name)
             (vector-ref (value/closure/location-nodes closure) i))
            (else (loop (1+ i)))))))


(define-value (value/unknown
	       (conc-name value/unknown/)
	       (constructor %value/make-unknown))
  )

(define (value/make-unknown text)
  (%value/make-unknown text '()))

;;  Sets of values
;;
;;  A value set must collect all the known procedures and closures that
;;  arrive at a node.  It may also collect other values in some form.
;;

(define value-set/print-procedure
  (standard-unparser-method
   'VALUE-SET
   (lambda (object port)
     (cond ((value-set/unknown? object)
	    => (lambda (unk)
		 (write-string " " port)
		 (write unk port)))
	   ((value-set/unique-value object)
	    => (lambda (value)
		 (write-string " = " port)
		 (write value port)))
	   ((and (null? (value-set/singletons object))
		 (null? (value-set/new-singletons object)))
	    (write-string " EMPTY" port))
	   (else
	    (write-string " *" port))))))

(define-structure (value-set
                   (conc-name value-set/)
                   (constructor %make-value-set)
		   (print-procedure value-set/print-procedure))
  unknown?              ;; either #F or the offending value/unknown
  singletons            ;; value/procedure & value/constant & value/unknown
  new-singletons
  other-values          ;; whatever - perhaps some lattice element
  )


(define-integrable (make-value-set)
  (%make-value-set #F '() '() '()))

(define (value-set/unique-value set)
  ;; returns the unique value or #F if there is no unique value.
  ;; This procedure is valid only after dataflow.
  (if (or (value-set/unknown? set)
	  (null? (value-set/singletons set))
	  (not (null? (cdr (value-set/singletons set)))))
      #F
      (car (value-set/singletons set))))

      
(define (value-set/age-value! set)
  ;; Moves a singleton value from the new values to the old, and returns it.
  ;; Updates unknown? slot
  (if (eq-set/empty? (value-set/new-singletons set))
      #f
      (let ((elt  (car (value-set/new-singletons set))))
        (begin
          (set-value-set/singletons! set (cons elt (value-set/singletons set)))
          (set-value-set/new-singletons! set (cdr (value-set/new-singletons set)))
          (if (and (value/unknown? elt)
                   (not (value-set/unknown? set)))
              (set-value-set/unknown?! set elt))
          elt))))

(define (value-set/union!? set additions)
  ;; Returns #t if the union operation added new elements, false if the
  ;; operation turned out to be idempotent
  (let* ((old-singletons     (value-set/singletons set))
         (new-singletons     (value-set/new-singletons set))
         (updated-singletons
          (eq-set/union3-difference new-singletons
                                    old-singletons
                                    (value-set/new-singletons additions)
                                    (value-set/singletons additions)))
         (changed?           (not (eq? new-singletons updated-singletons))))
    (set-value-set/new-singletons! set updated-singletons)
    ;; 1. An unknown frob, if present, will find its way via the above proc.
    ;; 2. Do something with other-values
    changed?))

(define (value-set/union!*? set sets)
  (let loop ((sets sets) (changed? #F))
    (if (null? sets)
        changed?
        (loop (cdr sets) (or (value-set/union!? set (car sets)) changed?)))))

(define (value-set/add-singleton!? set value)
  ;; Returns #t if the value was added, false it was already an element.
  (let* ((old-singletons     (value-set/singletons set))
         (new-singletons     (value-set/new-singletons set)))
    (cond ((memq value old-singletons)  #F)
          ((memq value new-singletons)  #F)
          (else (set-value-set/new-singletons! set (cons value new-singletons))
                #T))))

;;  eq-sets
;;

(define (eq-set/empty) '())
(define (eq-set/empty? set) (null? set))
(define (eq-set/union s1 s2)
  (cond ((null? s1)  s2)
        ((null? s2)  s1)
        ((memq (car s1) s2) (eq-set/union (cdr s1) s2))
        (else               (cons (car s1) (eq-set/union (cdr s1) s2)))))

(define (eq-set/union-difference initial exclude additions)
  (cond ((null? additions)
         initial)
        ((memq (car additions) initial)
         (eq-set/union-difference initial exclude (cdr additions)))
        ((memq (car additions) exclude)
         (eq-set/union-difference initial exclude (cdr additions)))
        (else
         (cons (car additions)
               (eq-set/union-difference initial exclude (cdr additions))))))

(define (eq-set/union3-difference new old new2 old2)
  ;; This result has the property of being EQ? with new if no elements are
  ;; added to the set.
  (eq-set/union-difference (eq-set/union-difference new old old2)
                           old new2))

(define-structure (graph
                   (conc-name graph/)
                   (constructor %make-graph))
  program
  escape-node                           ; values that escape collect here
  unknown-input-node                    ; values that arrive from unknown
                                        ; places (calls in, global vars)
  nodes                                 ; all nodes in graph
  procedures                            ; all procedure values in graph
  closures				; all closures
  applications                          ; all call sites
  ;;references                          ; all variable references
  text->node-table 
  constant->node-table                  ; cache of constants
  node-count
  )


(define (make-graph program)
  (let* ((graph
          (%make-graph program
                       #f               ; escape-node
                       #f               ; unknown-input-node
                       '()              ; nodes
                       '()              ; procedures
		       '()		; closures
                       '()              ; applications
                       ;;'()            ; references
                       (make-eq-hash-table) ; text->node-table
                       (make-eqv-hash-table) ; constant->node-table
		       0                ; node-count
                       ))
         (escape-node
	  (graph/add-location-node! graph 'escape-node #f))
         (unknown-input-node
	  (graph/add-location-node! graph 'unknown-input-node #f)))
    (set-graph/escape-node! graph escape-node)
    (add! escape-node 'ESCAPE-APPLICATION
          node/uses/trigger set-node/uses/trigger!)
    (set-graph/unknown-input-node! graph unknown-input-node)
    ;; I am not sure that this is either necessary or advisable, but it
    ;; ensures that the escaping nodes are fed back as possible inputs:
    ;; (initial-link-nodes! escape-node unknown-input-node)
    (add! unknown-input-node (value/make-unknown 'unknown-input)
          node/initial-values set-node/initial-values!)
    graph))
               

(define (graph/associate! graph text node)
  (hash-table/put! (graph/text->node-table graph) text node))

(define (graph/text->node graph text)
  (hash-table/get (graph/text->node-table graph) text #F))


(define (graph/add-expression-node! graph text name)
  ;; Nodes corresponding to expressions in the source
  (let ((node  (%graph/make-node graph text name 'EXPRESSION)))
    (add! graph node graph/nodes set-graph/nodes!)
    node))

(define (graph/add-location-node! graph text name)
  ;; Nodes corresponding to hidden locations (formals, bindings, cells,...)
  (let ((node  (%graph/make-node graph text name 'LOCATION)))
    (add! graph node graph/nodes set-graph/nodes!)
    node))

(define (graph/for-each-node graph procedure)
  (for-each procedure (graph/nodes graph)))

;; THIS USED TO BE TRUE BUT NOW WE DONT CACHE THE NODES THEMSELVES
;; Both constants and nodes are cached in the constant->node-table.
;; If the node exists the constant is the node's initial value.

(define (graph/add-constant! graph text)
  ;; Text = (QUOTE constant)
  (let* ((table         (graph/constant->node-table graph))
         (constant      (second text))
         (cached-node   (hash-table/get table constant #F)))
    (cond ((value/constant? cached-node)
           cached-node)
          ((node? cached-node)
           (car (node/initial-values cached-node)))
          (else
           (let* ((value  (value/make-constant text)))
             (hash-table/put! table constant value)
             value)))))

;;(define (graph/add-constant-node! graph text)
;;  ;; Text = (QUOTE constant)
;;  (let* ((table         (graph/constant->node-table graph))
;;         (constant      (second text))
;;         (cached-node   (hash-table/get table constant #F)))
;;    (if (node? cached-node)
;;        cached-node
;;        (let* ((value  (or cached-node (value/make-constant text)))
;;               (node   (graph/add-node! graph text "#[constant]")))
;;          (add! node value node/initial-values set-node/initial-values!)
;;          (hash-table/put! table constant node)
;;          node))))

;;(define (graph/add-constant-node! graph text)
;;  ;; Text = (QUOTE constant)
;;  (let* ((value  (value/make-constant text))
;;	 (node   (graph/add-expression-node! graph text "#[constant]")))
;;    (add! node value node/initial-values set-node/initial-values!)
;;    ;;(hash-table/put! table (quote/text text) node)
;;    node))

(define (graph/add-constant-node! graph text)
  ;; Text = (QUOTE constant)
  (let* ((value  (graph/add-constant! graph text))
	 (node   (graph/add-expression-node! graph text "#[constant]")))
    (add! node value node/initial-values set-node/initial-values!)
    ;;(hash-table/put! table (quote/text text) node)
    node))

;;(define (graph/add-reference! graph text variable-node)
;;  (let ((reference (value/make-reference text variable-node)))
;;    (add! graph reference graph/references set-graph/references!)
;;    (add! variable-node reference node/references set-node/references!)
;;    reference))

(define (graph/add-procedure! graph text input-nodes result-node)
  (let ((procedure (value/make-procedure text input-nodes result-node)))
    (add! graph procedure graph/procedures set-graph/procedures!)
    procedure))


(define (graph/add-closure! graph text kind procedure location-names location-nodes self-node)
  (let ((closure (value/make-closure
                  text kind procedure
                  location-names
                  location-nodes
                  self-node)))
    (add! graph closure graph/closures set-graph/closures!)
    closure))

(define (graph/initialize-links! graph)

  (define (connect! from to)
    ;; link nodes transitively
    (if (not (nodes-linked? from to))
	(begin
	  (link-nodes! from to)
	  (node-set/for-each (node/links-in from)
	    (lambda (from) (connect! from to)))
	  (node-set/for-each (node/links-out to)
	    (lambda (to) (connect! from to))))))

  (graph/for-each-node graph
    (lambda (node)
      (for-each-item (lambda (to)
		       (connect! node to))
		     (node/initial-links-out node))
      (for-each-item (lambda (from)
		       (connect! from node))
		     (node/initial-links-in node)))))


(define-structure
  (application
   (conc-name application/)
   (print-procedure
    (standard-unparser-method 'APPLICATION
      (lambda (application port)
	(if (CALL/? (application/text application))
	    (let ((operator (call/operator (application/text application))))
	      (write-char #\Space port)
	      (cond ((QUOTE/? operator)
		     (write operator port))
		    ((LOOKUP/? operator)
		     (write (lookup/name operator) port))
		    ((LAMBDA/? operator)
		     (write-string "(lambda)" port))
		    (else
		     (write-string "<operator>" port)))))))))
  text
  operator-node
  operand-nodes
  ;; The result node is the expected thing in Direct style.  In CPS it
  ;; holds the value that should be passed to the continuation, which
  ;; is only really useful when modelling calls to external known
  ;; operators.  For these we create special-applications on the fly
  ;; that feed the result back to the continuation.
  result-node
  )
                   
(define (graph/add-application! graph text
                                operator-node operand-nodes result-node)
  (let ((application (make-application
                      text operator-node operand-nodes result-node)))

    (add! operator-node application node/uses/trigger set-node/uses/trigger!)

    (add! graph application graph/applications set-graph/applications!)
    (add! operator-node application node/uses/operator set-node/uses/operator!)
    (for-each
     (lambda (node)
       (if node
	   (add! node application node/uses/operand set-node/uses/operand!)))
     operand-nodes)
    application))



(define-structure
  (special-application
   (conc-name special-application/)
   (print-procedure
    (standard-unparser-method 'SPECIAL-APPLICATION
      (lambda (application port)
	(write-char #\Space port)
	(write (special-application/operator application) port)))))
  text
  operator				; cookie
  operand-nodes
  result-node)

(define (graph/add-special-application!
	 graph text
	 operator operand-nodes
	 trigger-nodes
	 result-node)
  (let ((application (make-special-application
                      text operator operand-nodes result-node)))

    (add! graph application graph/applications set-graph/applications!)
    (for-each
     (lambda (node)
       (add! node application node/uses/operand set-node/uses/operand!))
     operand-nodes)
    (for-each
     (lambda (node)
       (add! node application node/uses/trigger set-node/uses/trigger!))
     trigger-nodes)
    application))



;;
;;  The abstraction that we use for lists of things

(define-integrable (add! structure item accessor setter!)
  (let ((structure structure))
    (setter! structure (cons item (accessor structure)))))

(define (empty? frob)
  (null? frob))

(define-integrable (in? item collection)
  (memq item collection))

(define-integrable (for-each-item proc things)
  (for-each proc things))



(define (graph/pp graph)
  (define (ppp x) (pp x (current-output-port) #T))

  (define (section heading selector pp)
    (newline) (newline) (display heading)
    (for-each (lambda (proc)
		(newline)
		(pp proc))
	      (selector graph)))

  (pp graph)

  (section "NODES" graph/nodes pp)
  
  (newline) (newline) (display "TEXT->NODE map") (newline)
  (for-each ppp (hash-table->alist (graph/text->node-table graph)))
  
  (section "APPLICATIONS" graph/applications ppp)
  (section "PROCEDURES"   graph/procedures   pp)
  (section "CLOSURES"     graph/closures     pp)
)


;;
;;  Simulated application
;;

;; Normal procedures: connect up the arguments to the parameters.  This
;; may invalidate other application nodes if an operator flow out
;;
;; Primitives: output must be monotonic on each inputs.  Need to rerun
;; any application which has a new value for any of the arguments.
;;
;; Values which escape end up in the escape-node.

(define (graph/dataflow! graph)
  (graph/for-each-node graph
    (lambda (node) (set-node/values! node 'NOT-CACHED)))
  (graph/for-each-node graph node/initialize-cache!)
  ;; Trivial closures need to
  (graph/initialize-closure-procedures! graph)
  (let ((queue  (queue/make)))
    (queue/enqueue!* queue (graph/applications graph))
    (queue/enqueue!  queue 'ESCAPE-APPLICATION)
    (queue/drain! queue  (simulate-combination graph queue)))
  ;; This ensures that unknown values get into the flag position in nodes 
  ;; that are not used in an application:
  (graph/for-each-node graph
    (lambda (node)
      (let loop ()
	(if (value-set/age-value! (node/values node)) (loop)))))
  ;; Mark all closures that escape.  Must be done after the above step.
  (for-each-item (lambda (value)
		   (if (value/closure? value)
		       (set-value/closure/escapes?! value #T)))
		 (value-set/singletons (node/values (graph/escape-node graph))))

  ;; Invert graph to obtain values->nodes
  (graph/for-each-node  graph
    (lambda (node)
      (for-each-item
       (lambda (value)
	 (add! value node value/nodes set-value/nodes!))
       (value-set/singletons (node/values node)))))
						     
  )


(define ((simulate-combination graph queue) application)
  (cond ((eq? 'ESCAPE-APPLICATION application)
	 (dataflow/apply-escapees! graph queue))
	((application? application)
	 (simulate-application application graph queue))
	((special-application? application)
	 (simulate-special-application application graph queue))
	(else
	 (internal-error "Illegal graph application" application))))

(define (simulate-application application graph queue)

  (define (connect! from to) (connect-nodes! graph queue from to))

  (let* ((operator-node  (application/operator-node application))
	 (operand-nodes  (application/operand-nodes application))
	 (result-node    (application/result-node application)))

    (define (apply-next-operator)
      (let ((operator (value-set/age-value! (node/values operator-node))))
	(cond ((false? operator)
	       'done)

	      ((value/unknown? operator)
	       (for-each
		(lambda (operand-node)
		  (if operand-node
		      (connect! operand-node (graph/escape-node graph))))
		operand-nodes)
	       (if result-node
		   (connect! (graph/unknown-input-node graph) result-node))
	       (apply-next-operator))

	      ((value/constant? operator)
	       ;; all the magic cookies
	       (dataflow/applicate-constant!
		graph queue application operator)
	       (apply-next-operator))

	      ((value/procedure? operator)
	       (dataflow/applicate! graph
				    queue
				    (value/procedure/lambda-list operator)
				    (value/procedure/input-nodes operator)
				    operand-nodes)
	       (cond ((and result-node (value/procedure/result-node operator))
		      ;; i.e. direct style
		      (connect! (value/procedure/result-node operator)
				result-node))
		     ;;((or (value/procedure/result-node operator)
		     ;;     (first operand-nodes))
		     ;;(internal-error "Direct/CPS mismatch"
		     ;;		      operator application))
		     )
	       (apply-next-operator))

	      ((value/closure? operator)
	       ;; This is slightly more involved as we have to extract the
	       ;; procedure and arrange for the closure to be passed for
	       ;; non-trivial closures
	       (let* ((procedure  (value/closure/procedure operator)))

		 (add! operator application value/closure/call-sites
		       set-value/closure/call-sites!)
		 (dataflow/applicate!
		  graph
		  queue
		  (value/procedure/lambda-list procedure)
		  (value/procedure/input-nodes procedure)
		  (if (memq (value/closure/kind operator) '(TRIVIAL STACK))
		      operand-nodes
		      (cons* (first operand-nodes)
			     (value/closure/self-node operator)
			     (cdr operand-nodes))))
		 (cond ((and result-node (value/procedure/result-node procedure))
			;; i.e. direct style
			(connect! (value/procedure/result-node procedure)
				  result-node))
		       ;;((or result-node (value/procedure/result-node procedure))
		       ;;(internal-error "Direct/CPS mismatch"
		       ;;		operator application))
		       )
		 (apply-next-operator)))

	      (else
	       (internal-error "Dont know how to apply"
			       operator application)))))

    (apply-next-operator)))


(define (simulate-special-application application graph queue)

  (define (connect! from to)  (connect-nodes! graph queue from to))

  graph

  (let ((operator       (special-application/operator application))
	(operand-nodes  (special-application/operand-nodes application))
	(result-node    (special-application/result-node application)))

    (cond ((eq? operator %make-heap-closure)
	   ;;no action required - closure value precomputed
	   unspecific)
	  ((eq? operator %make-stack-closure)
	   ;;no action required - closure value precomputed
	   unspecific)

	  ((or (eq? operator %heap-closure-ref)
	       (eq? operator %stack-closure-ref))
	   ;; (CALL ',%heap-closure-ref '#F  <closure> <offset> 'NAME)
	   ;; (CALL ',%stack-closure-ref '#F  <closure> <offset> 'NAME)
	   (let* ((text         (special-application/text application))
		  (name         (second (sixth text)))
		  (ref-kind     (second (second text)))
		  (closure-node (first operand-nodes))
		  (closure      (value-set/age-value!
				 (node/values closure-node))))
	     (if closure
		 (let ((location-node
			(value/closure/lookup-location-node closure name)))
		   (if (not (node-set/empty? (node/links-in result-node)))
		       (internal-error "Multiple linkings at " application))
		   ;;(pp `(,ref-kind ,closure ,name))
		   (connect! location-node result-node)
		   (let ((bad  (value-set/age-value!
				(node/values closure-node))))
		     (if bad
			 (internal-error "Multiple closures at" ref-kind
					 application)))))))

	  ((eq? operator %heap-closure-set!)
	   (let* ((text         (special-application/text application))
		  (name         (quote/text
				 (call/%heap-closure-set!/name text)))
		  (closure-node (first operand-nodes))
		  (value-node   (second operand-nodes))
		  (closure      (value-set/age-value!
				 (node/values closure-node))))
	     (if closure
		 (let ((location-node
			(value/closure/lookup-location-node closure name)))
		   (connect! value-node location-node)
		   (let ((bad  (value-set/age-value!
				(node/values closure-node))))
		     (if bad
			 (internal-error "Multiple closures at"
					 application)))))))

	  (else 
	   (internal-error
	    "Unknown special-application operator" operator application)))))


(define (dataflow/apply-escapees! graph queue)
  ;; Ensure any procedure that escapes is called with unknown arguments and
  ;; its result escapes too.

  (define (connect! from to)  (connect-nodes! graph queue from to))
  
  (let* ((unknown-input-node   (graph/unknown-input-node graph))
         (escape-node          (graph/escape-node graph))
         (values               (node/values escape-node)))
    (let escape-next-object ()
      (let ((object  (value-set/age-value! values)))
        (cond ((false? object)
               unspecific)
              ((value/procedure? object)
	       ;;(pp (list 'escaped: object))
	       (for-each (lambda (input-node)
			   (connect! unknown-input-node input-node))
		 (value/procedure/input-nodes object))
	       (if (value/procedure/result-node object)
		   (connect! (value/procedure/result-node object)
			     escape-node))
               (escape-next-object))

              ((value/closure? object)
               ;; This is slightly more involved as we have to link up a heap
	       ;; closure's procedure with the correct value (node) for the
               ;; closure argument.

	       ;; NOTE:  Perhaps this code should also escape the closure
	       ;; locations?
	       ;;(pp (list 'escaped: object))
               (let* ((procedure   (value/closure/procedure object))
		      (input-nodes (value/procedure/input-nodes procedure)))
		 (if (value/procedure/result-node procedure)
		     (connect! (value/procedure/result-node procedure)
			       escape-node))
                 (connect! unknown-input-node (first input-nodes))
		 (if (eq? 'HEAP (value/closure/kind object))
		     (begin
		       (connect! (value/closure/self-node object)
				 (second input-nodes))
		       (for-each
			(lambda (input-node)
			  (connect! unknown-input-node input-node))
			(cddr input-nodes)))
		     (begin ; TRIVIAL or STACK
		       (for-each
			(lambda (input-node)
			  (connect! unknown-input-node input-node))
			(cdr input-nodes)))))
	       
               (escape-next-object))

	      (#F
	       ;; Anything containing locations that are accessible by accessor
	       ;; procedures needs to escape the locations.
	       )
              (else;; unknown, constants, primitives
               (escape-next-object)))))))


(define (dataflow/make-globals-escape! env graph)
  (dataflow/env/for-each-global-binding
   (lambda (binding)
     (let ((value  (dataflow/binding/value binding)))
       (initial-link-nodes! value (graph/escape-node graph))
       (initial-link-nodes! (graph/unknown-input-node graph) value)))
   env))

(define (dataflow/applicate-constant! graph
                                      queue
                                      application
                                      operator)

  (let ((operator  (value/constant/value operator)))
    (if (and (not (primitive-procedure? operator))
	     (not (compiled-procedure? operator))
	     (not (known-operator? operator))
	     *dataflow-report-applied-non-procedures?*)
	(warn "Possibly applied non-procedure object: " operator)))

  ((dataflow/get-method (value/constant/value operator))
   graph
   queue
   application
   operator))


(define dataflow/cookie-methods  (make-eq-hash-table))

(define (define-dataflow-method cookie method)
  (hash-table/put! dataflow/cookie-methods cookie method))

(define (dataflow/get-method cookie)
  (hash-table/get dataflow/cookie-methods cookie
                  dataflow/method/default-method))

(define (dataflow/method/default-method graph queue application operator)
  ;; The default method assumes the very worst about the operator: all the
  ;; arguments escape and the result, if any, is completely unknown
  (define (connect! from to)  (connect-nodes! graph queue from to))
  operator
  (if (application/result-node application)
      (connect! (graph/unknown-input-node graph)
		(application/result-node application)))
  (for-each (lambda (node)
	      (if node
		  (connect! node (graph/escape-node graph))))
	    (application/operand-nodes application)))

(define (dataflow/method/simple graph queue application operator)
  ;; The simple method assumes that none of the arguments escape and the
  ;; result is completely unknown
  operator
  (define (connect! from to)  (connect-nodes! graph queue from to))
  (define result-node (application/result-node application))
  (connect! (graph/unknown-input-node graph) result-node))
  
(define (dataflow/method/simple-predicate graph queue application operator)
  ;; The simple method assumes that none of the arguments escape and the
  ;; result is either #F or #T
  operator
  (define result-node (application/result-node application))
  (node/add-value! result-node (graph/add-constant! graph `(QUOTE #F)) queue)
  (node/add-value! result-node (graph/add-constant! graph `(QUOTE #T)) queue))


(define (dataflow/external-return graph queue application operator)
  (let ((result-node  (application/result-node application))
	(cont-node    (first (application/operand-nodes application))))
    (if cont-node
	(let ((application
	       (graph/add-application! graph
				       `(EXTERNAL-RETURN ,operator)
				       cont-node
				       (list #F result-node)
				       #F)))
	  ;; enqueue it in case the result is already available
	  (queue/enqueue!  queue application))
	;; In direct style the result is already in place.
	'ok)))
				  
  
(define (dataflow/method/external-predicate graph queue application operator)
  ;; The simple method assumes that none of the arguments escape and the
  ;; result is either #F or #T
  operator
  (define result-node (application/result-node application))
  (node/add-value! result-node (graph/add-constant! graph `(QUOTE #F)) queue)
  (node/add-value! result-node (graph/add-constant! graph `(QUOTE #T)) queue)
  (dataflow/external-return graph queue application operator))



(define-dataflow-method fix:+ dataflow/method/simple)
(define-dataflow-method fix:- dataflow/method/simple)
(define-dataflow-method fix:* dataflow/method/simple)

(define-dataflow-method fix:< dataflow/method/simple-predicate)

(for-each
 (lambda (name)
   (define-dataflow-method (make-primitive-procedure name)
     dataflow/method/external-predicate))
 '(&< &= &>))



;;(define (dataflow/method/%make-heap-closure graph queue application operator)
;;  ;; (CALL ,%make-heap-closure '#F (lambda (k closure ..) ..) '#(x y) x y)    
;;  (define (connect! from to)  (connect-nodes! graph queue from to))
;;    
;;  (let* ((arg-nodes       (application/operand-nodes application))
;;         (text            (application/text application))
;;         (location-names  (second (fifth text)))
;;         (cont-node       (first arg-nodes))
;;         (lambda-node     (second arg-nodes))
;;         (vector-node     (third arg-nodes))
;;         (value-nodes     (cdddr arg-nodes))
;;         (procedure       (node/the-procedure-value lambda-node))
;;         (self-node       (application/result-node application))
;;         (closure (graph/add-closure!
;;                   graph
;;                   text
;;                   'HEAP
;;                   procedure
;;                   location-names
;;                   self-node)))
;;    (let loop ((i 0)  (value-nodes value-nodes))
;;      (if (< i (vector-length location-names))
;;          (let ((node (vector-ref (value/closure/location-nodes closure) i)))
;;            (node/initialize-cache! node)
;;            (connect! (car value-nodes) node)
;;            (loop (+ i 1) (cdr value-nodes)))))
;;    (node/add-value! self-node closure queue)))
;;
;;(define-dataflow-method %make-heap-closure dataflow/method/%make-heap-closure)
;;
;;
;;(define (dataflow/method/%heap-closure-ref graph queue application operator)
;;  ;; (CALL ,%heap-closure-ref '#F <closure> <offset> 'NAME)
;;    
;;  (define (connect! from to)  (connect-nodes! graph queue from to))
;;    
;;  (let* ((text            (application/text application))
;;         (arg-nodes       (application/operand-nodes application))
;;         (closure-node    (second arg-nodes))
;;         (closure-values  (node/values closure-node))
;;         (name            (second (fifth text)))
;;         (result-node     (application/result-node application)))
;;    ;; This procedure should only ever be called with a single known closure
;;    ;; value.  We connect the named slot
;;      
;;    (let loop ((closure  (value-set/age-value! closure-values)))
;;      (cond ((false? closure)
;;             unspecific)
;;            ((value/closure? closure)
;;             (if (null? (node/links-in result-node))
;;                 (connect! (value/closure/lookup-location-node closure name)
;;                           result-node)
;;                 (internal-error "%heap-closure-ref again!"
;;                                 closure application))
;;             (loop (value/set/age-value! closure-values)))
;;            (else
;;             (internal-error "%heap-closure-ref of non-closure"
;;                             closure application))))))
;;
;;(define-dataflow-method %heap-closure-ref dataflow/method/%heap-closure-ref)
;;

      

(define (dataflow/applicate! graph
                             queue
                             whole-lambda-list
                             whole-formals      ; abstract names in lambda list
                             whole-args)

  (define (connect! from to)
    (connect-nodes! graph queue from to))

  (define (do-normal! name formal arg)
    name
    (if arg
	(connect! arg formal)))

  (define (do-optional! name formal arg)
    name
    (if arg
        (connect! arg formal)
        (node/add-value!
         formal
         (graph/add-constant! graph `(QUOTE ,(make-unassigned-reference-trap)))
         queue)))

  (define (do-rest! name formal args)
    name
    (if (null? args)
        ;;(connect! (graph/add-constant-node! graph '(QUOTE ())) formal)
        (node/add-value! formal
                         (graph/add-constant! graph `(QUOTE ()))
                         queue)
        (connect! (graph/unknown-input-node graph) formal)))

  (define (normal-loop lambda-list formals args)
    (cond ((null? lambda-list)
           (if (not (null? args))
               (warn "Too many args" whole-lambda-list whole-args)))
          ((eq? (car lambda-list) #!optional)
           (optional-loop (cdr lambda-list) formals args))
          ((eq? (car lambda-list) #!rest)
           (rest-loop (cdr lambda-list) formals args))
          ((null? args)
           (warn "Too few arguments" whole-lambda-list whole-args))
          (else
           (do-normal! (car lambda-list) (car formals) (car args))
           (normal-loop (cdr lambda-list) (cdr formals) (cdr args)))))
  
  (define (optional-loop lambda-list formals args)
    (cond ((null? lambda-list)
           (if (not (null? args))
               (warn "Too many args"  whole-lambda-list whole-args)))
          ((eq? (car lambda-list) #!rest)
           (rest-loop (cdr lambda-list) formals args))
          ((null? args)
           (do-optional! (car lambda-list) (car formals) #f)
           (optional-loop (cdr lambda-list) (cdr formals) '()))
          (else
           (do-optional! (car lambda-list) (car formals) (car args))
           (optional-loop (cdr lambda-list) (cdr formals) (cdr args)))))
           
  (define (rest-loop lambda-list formals args)
    (do-rest! (car lambda-list) (car formals) args))

  (normal-loop whole-lambda-list whole-formals whole-args))



;;;;
;;;; Node sets allow insertion while the set is being traversed.  This is
;;;; node by keeping the items added during the traversal and inserting
;;;; them later.
;;
;;(load-option 'rb-tree)
;;
;;(define-integrable (node-set/lock s) (car s))
;;(define-integrable (node-set/elements s) (cdr s))
;;(define-integrable (node-set/set-lock! s v) (set-car! s v))
;;(define-integrable (node-set/set-elements! s v) (set-cdr! s v))
;;
;;(define-integrable (node-set/locked? s) (not (symbol? (node-set/lock s))))
;;
;;(define (node-set/add-unlocked! set elt)
;;  (node-set/set-elements! set (cons elt (node-set/elements set))))
;;
;;(define (link-nodes! from to)
;;  (define-integrable (add! set elt)
;;    (if (node-set/locked? set)
;;	(node-set/set-lock! set (cons elt (node-set/lock set)))
;;	(node-set/add-unlocked! set elt)))
;;  (add! (node/links-in to) from)
;;  (add! (node/links-out from) to))
;;  
;;(define (nodes-linked? from to)
;;  (or (eq? from to)
;;      (node-set/member? from (node/links-in to))))
;;
;;(define (make-empty-node-set)
;;  (cons 'unlocked '()))
;;
;;(define (node-set/empty? set)
;;  (null? (node-set/elements set)))
;;
;;(define (node-set/member? node set)
;;  (or (memq node (node-set/elements set))
;;      (and (node-set/locked? set)
;;	   (memq node (node-set/lock set)))))
;;
;;(define (node-set/for-each set proc)
;;  (let ((old-lock  (node-set/lock set)))
;;    (node-set/set-lock! set '())
;;    (for-each proc (node-set/elements set))
;;    (if (not (null? (node-set/lock set)))
;;	(pp `(deferred . ,(node-set/lock set))))
;;    (for-each (lambda (addend)
;;		(node-set/add-unlocked! set addend))
;;      (node-set/lock set))
;;    (node-set/set-lock! set old-lock)
;;    unspecific))	  
;;
;;(define (node-set/size set)
;;  (length (node-set/elements set)))


;;______________________________________________________________________________
;;
;; A note about the structure of the graph.  About 80% of the nodes have
;; 3 or fewer in-edges.  The most popular in-degree is 0 (~35%), then
;; 3 and 1 (at 15%) and then 2 (at 8%) [after cps conversion, one
;; large sample].
;;
;; The node(s) which collect escaped values have a huge number of edges.
;;
;; The overhead of deciding to use the bit-strings is not worth it for
;; graphs with < 3k nodes.
;;
;;(define (link-nodes! from to)
;;  (define-integrable (add! structure item accessor setter!)
;;    (let ((structure structure))
;;      (setter! structure (cons item (accessor structure)))))
;;  (add! to from node/links-in set-node/links-in!)
;;  (add! from to node/links-out set-node/links-out!)
;;  (cond ((node/connectivity to)
;;	 (bit-string-set! (node/connectivity to) (node/number from)))
;;	((>= (length (node/links-in to)) 10)
;;	 (let ((bs (make-bit-string *node-count* #F)))
;;	   (for-each (lambda (node)
;;		       (bit-string-set! bs (node/number node)))
;;		     (node/links-in to))
;;	   (set-node/connectivity! to bs))))
;;  unspecific)
;;  
;;(define (nodes-linked? from to)
;;  (cond ((eq? from to)
;;	 #T)
;;	((node/connectivity to)
;;	 (bit-string-ref (node/connectivity to) (node/number from)))
;;	((memq from (node/links-in to))
;;	 #T)
;;	(else #F)))
;;
;;(define (make-empty-node-set)
;;  '())
;;
;;(define (node-set/empty? set)
;;  (null? set))
;;
;;(define (node-set/for-each set proc)
;;  (for-each proc set))
;;
;;(define (node-set/size set)
;;  (length set))
;;______________________________________________________________________________



;;______________________________________________________________________________
;;
;; Simple lists are slow and take 2n words per entry
;;
;;(define (link-nodes! from to)
;;  (define-integrable (add! structure item accessor setter!)
;;    (let ((structure structure))
;;      (setter! structure (cons item (accessor structure)))))
;;  (add! to from node/links-in set-node/links-in!)
;;  (add! from to node/links-out set-node/links-out!))
;;  
;;(define (nodes-linked? from to)
;;  (or (eq? from to)
;;      (memq from (node/links-in to))))
;;
;;(define (make-empty-node-set)
;;  '())
;;
;;(define (node-set/empty? set)
;;  (null? set))
;;
;;(define (node-set/for-each set proc)
;;  (for-each proc set))
;;
;;(define (node-set/size set)
;;  (length set))
;;______________________________________________________________________________

;; The growing vector approach.  Uses at most kN+2 works where k in 4/3.
;; as k -> 1 the overhead reduced by the vector has to be grown more
;; often (see GROW) below.  The vector contains a count C in the first
;; (0 index) slot and set elements in slots 1..C.

(define (link-nodes! from to)

  (define (initial-vector elt)  (vector 1 elt))

  (define (grow v)
    ;; Fast open-coded grow operations for common cases.  All vectors start
    ;; out small and so benefit from this (I hope).
    (case (vector-length v)
      ((2) (vector (vector-ref v 0) (vector-ref v 1) #F))
      ((3) (vector (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) #F))
      (else
       (vector-grow v (fix:quotient (fix:* (vector-length v) 4) 3)))))
  
  (define-integrable (add! structure item accessor setter!)
    (let ((set  (accessor structure)))
      (if set
	  (let ((index  (fix:+ (vector-ref set 0) 1))
		(vlen   (vector-length set)))
	    (if (fix:>= index vlen)
		(let ((set* (grow set)))
		  (vector-set! set* index item)
		  (vector-set! set* 0 index)
		  (setter! structure set*))
		(begin
		  (vector-set! set index item)
		  (vector-set! set 0 index))))
	  (setter! structure (initial-vector item)))))

  (add! to from node/links-in set-node/links-in!)
  (add! from to node/links-out set-node/links-out!)

  (if (fix:>= (vector-length (node/links-in to)) 75)
      (if (node/connectivity to)
	  (bit-vector-set! (node/connectivity to) (node/number from))
	  (let ((bs       (make-bit-vector *node-count* #F))
		(links-in (node/links-in to)))
	    (let loop ((i (vector-ref links-in 0)))
	      (cond ((fix:> i 0)
		     (bit-vector-set! bs (node/number (vector-ref links-in i)))
		     (loop (fix:- i 1)))))
	    (set-node/connectivity! to bs))))
)

;;(define (nodes-linked? from to)
;;  (or (eq? from to)
;;      (let ((set  (node/links-in to)))
;;	(and set
;;	     (let loop ((i  (vector-ref set 0)))
;;	       (and (fix:> i 0)
;;		    ;; Loop unrolled 1 time is safe because the zero slot
;;		    ;; contains a fixnum that will never match a node
;;		    (or (eq? from (vector-ref set i))
;;			(eq? from (vector-ref set (fix:- i 1)))
;;			(loop (fix:- i 2)))))))))

(define (nodes-linked? from to)
  (or (eq? from to)
      (if (node/connectivity to)
	  (bit-vector-ref (node/connectivity to) (node/number from))
	  (let ((set  (node/links-in to)))
	    (and set
		 (let unrolled-loop ((i  (vector-ref set 0)))
		   (if (fix:>= i 8)
		       (or (eq? from (vector-ref set i))
			   (eq? from (vector-ref set (fix:- i 1)))
			   (eq? from (vector-ref set (fix:- i 2)))
			   (eq? from (vector-ref set (fix:- i 3)))
			   (eq? from (vector-ref set (fix:- i 4)))
			   (eq? from (vector-ref set (fix:- i 5)))
			   (eq? from (vector-ref set (fix:- i 6)))
			   (eq? from (vector-ref set (fix:- i 7)))
			   (unrolled-loop (fix:- i 8)))
		       (let end-loop ((i i))
			 (and (fix:> i 0)
			      ;; Loop unrolled 1 time is safe because the zero slot
			      ;; contains a fixnum that will never match a node
			      (or (eq? from (vector-ref set i))
				  (eq? from (vector-ref set (fix:- i 1)))
				  (end-loop (fix:- i 2))))))))))))


;; BIT vectors as strings:

(define-integrable (bv-index->byte-index n)
  (fix:lsh n -3))

(define-integrable (bv-index->mask n)
  (vector-8b-ref "\001\002\004\010\020\040\100\200" (fix:and n 7)))

(define (make-bit-vector n init)
  (define-integrable (bv-bits->bytes n)
    (fix:lsh (fix:+ n 7) -3))
  (make-string (bv-bits->bytes n)
	       (if init
		   (integer->char 255)
		   (integer->char 0))))

(define-integrable (bit-vector-ref bv n)
  (fix:= (fix:and (vector-8b-ref bv (bv-index->byte-index n))
		  (bv-index->mask n))
	 (bv-index->mask n)))

(define (bit-vector-set! bv n)
  (vector-8b-set! bv
		  (bv-index->byte-index n)
		  (fix:or (vector-8b-ref bv (bv-index->byte-index n))
			  (bv-index->mask n))))




(define (make-empty-node-set)
  '#F)

(define-integrable (node-set/empty? set)
  (eq? set '#F))

(define-integrable (node-set/for-each set proc)
  (let ((set set))
    (if set
	(let loop ((i  (vector-ref set 0)))
	  (if (fix:> i 0)
	      (begin
		(proc (vector-ref set i))
		(loop (fix:- i 1))))))))

(define (node-set/size set)
  (if set
      (vector-ref set 0)
      0))

(define (connect-nodes! graph queue from to)
  graph

  (define (link! from to)
    (if (not (nodes-linked? from to))
	(link-nodes! from to)))
    
  (link! from to)
  (node-set/for-each (node/links-in from)
    (lambda (from*)
      (link! from* to)))
  (node-set/for-each (node/links-out to)
    (lambda (to*)
      (link! from to*)))
  (node-set/for-each (node/links-in from)
    (lambda (from*)
      (node-set/for-each (node/links-out to)
	(lambda (to*)
	  (link! from* to*)))))

  ;; Now any node newly reachable from any predecessor of FROM is in FROM's
  ;; successors, so we can just do a one-level propagation of values
  ;;(node/propagate! from queue)
  ;;
  ;; This is better as it does not needlessly propagate preexisting
  ;; successors of from:
  (if (value-set/union!? (node/values to) (node/values from))
      (begin
        (node/enqueue-applications! to queue)
        (node/propagate! to queue))))



(define (node/add-value! node value queue)
  (if (value-set/add-singleton!? (node/values node) value)
      (begin
        (node/enqueue-applications! node queue)
        (node/propagate! node queue))))

(define (node/enqueue-applications! node queue)
  (queue/enqueue!* queue (node/uses/trigger node)))
  
(define (node/propagate! node queue)
  ;; This is a node who's value has changed, so propagate to successors
  (let ((values  (node/values node)))
    (node-set/for-each (node/links-out node)
      (lambda (dest)
	(if (value-set/union!? (node/values dest) values)
	    (node/enqueue-applications! dest queue))))))

(define (node/initialize-cache! node)
  (node/compute-initial-values! node))

#|
(define (node/compute-initial-values node)
  ;; This is slow but works even with cycles in the DFG.
  ;; Only works if there are no links in from a node with a reference value
  (let ((nodes '()))
    (let walk ((node node))
      (if (not (memq node nodes))
          (begin (set! nodes (cons node nodes))
                 (for-each walk (node/links-in node)))))
    (value-set/union* (node/initial-values (car nodes))
                      (map node/initial-values (cdr nodes)))))
|#


(define (node/compute-initial-values! target-node)
  ;; This is slow but works even with cycles in the DFG.
  (let ((nodes '()))
    (define (eval-node! node)
      (cond ((memq node nodes)
             unspecific)
            ((eq? (node/values node) 'NOT-CACHED)
             (set! nodes (cons node nodes))
             (node-set/for-each (node/links-in node) eval-node!)
             (let ((vs (make-value-set)))
               (set-node/values! node vs)
               (set-value-set/new-singletons! vs (node/initial-values node))
               (node-set/for-each
                (node/links-in node)
                (lambda (input-node)
                  (if (value-set? (node/values input-node))
                      (value-set/union!? vs (node/values input-node)))))))
            (else
             unspecific)))
    (eval-node! target-node)))

(define (graph/substitite-simple-constants graph simple-constant?)
  ;; Rewrite any node with a unique constant value K satisfying
  ;; SIMPLE-CONSTANT? as (QUOTE K)
  (for-each 
      (lambda (node)
	(if (expression-node? node)
	    (let ((value (node/unique-value node)))
	      (cond ((QUOTE/? (node/text node))
		     unspecific)
		    ((and (value/constant? value)
			  (simple-constant? (value/constant/value value))
			  (form/simple&side-effect-free? (node/text node)))
		     (if compiler:guru?
			 (begin
			   (display "\n; Constant propagation:")
			   (kmp/ppp
			    `(,node ,(node/text node) =>
				    (QUOTE ,(value/constant/value value))))))
		     (form/rewrite! (node/text node)
		       `(QUOTE ,(value/constant/value value))))
		    (else unspecific)))))
    (graph/nodes graph)))

(define (graph/read-eq?-preserving-constant? value)
  (or (fixnum? value)
      (char? value)
      (symbol? value)
      (memq value '(#F () #T))))

(define (graph/read-eqv?-preserving-constant? value)
  (or (graph/read-eq?-preserving-constant? value)
      (number? value)))

(define (graph/look-for-interesting-nodes graph)
  (define (parse lambda-expr remove-closure?)
    (call-with-values
	(lambda () (lambda-list/parse (lambda/formals lambda-expr)))
      (lambda (required optional rest aux)
	aux
	(let ((req  (if remove-closure? (cdr required) required)))
	  (cons (length req)
		(if rest #F (+ (length req) (length optional))))))))
  (define (value/arity value)
    (cond ((value/procedure? value)
	   (parse (value/text value) #F))
	  ((value/closure? value)
	   (parse (value/text (value/closure/procedure value))
		  (eq? (value/closure/kind value) 'HEAP)))
	  (else (internal-warning "graph/look-for-interesting-nodes unexpected"
				  value)
		#F)))
  (for-each 
      (lambda (node)
	(if (expression-node? node)
	    (let ((values (node/values node)))
	      (cond ((value-set/unknown? values))
		    ((null? (node/uses/operator node)))
		    ((value-set/unique-value values))
		    ((for-all? (value-set/singletons values)
		       (lambda (value)
			 (or (value/procedure? value)
			     (value/closure? value))))
		     ;;(for-each (lambda (ap)
		     ;;		 (fluid-let ((*unparser-list-depth-limit* 3))
		     ;;		   (pp ap)))
		     ;;  (node/uses/operator node))
		     (display "\n;; Multiple procedures ") (display node)
		     (display " ")
		     (for-each (lambda (p)
				 (display (value/arity p)))
		       (value-set/singletons values))
		     (display (map (lambda (p) (or (value/procedure? p)
						   (value/closure/kind p)))
				   (value-set/singletons values))))
		    (else unspecific)))))
    (graph/nodes graph)))

(define (graph/compiled-procedure-reductions graph)
  ;; What can we optimize certain known operators if we know that all of
  ;; the operands are compiled procedures (and all of the same arity).
  (define (match? lambda-expr remove-closure? arity)
    (call-with-values
	(lambda () (lambda-list/parse (lambda/formals lambda-expr)))
      (lambda (required optional rest aux)
	aux
	(let ((req  (if remove-closure? (cddr required) (cdr required))))
	  (and (not rest)
	       (= arity (+ (length req) (length optional))))))))
  (define (value/arity-matches? value arity)
    (cond ((value/procedure? value)
	   (match? (value/text value) #F arity))
	  ((value/closure? value)
	   (match? (value/text (value/closure/procedure value))
		   (eq? (value/closure/kind value) 'HEAP)
		   arity))
	  (else #F)))
  (define (inspect application)
    (let ((text (application/text application)))
      (cond ((call/%internal-apply? text)
	     (let* ((node    (application/operator-node application))
		    (values  (node/values node))
		    (call-arity		; sans continuation
		     (length (cdr (application/operand-nodes application)))))
	       (cond ((value-set/unknown? values))
		     ((value-set/unique-value values))
		     ((for-all? (value-set/singletons values)
			(lambda (value)
			  (value/arity-matches? value call-arity)))
		      (if compiler:guru?
			  (begin
			    (display "\n;; Call site ") (display node)
			    (display " multiple procedures arity ")
			    (display call-arity) (display ": ")
			    (display (map (lambda (p)
					    (or (value/procedure? p)
						(value/closure/kind p)))
					  (value-set/singletons values)))))
		      (form/rewrite! (second (application/text application))
			`(QUOTE ,%internal-apply-unchecked)))
		     (else unspecific))))
	    ((call/%compiled-entry?? text)
	     (let* ((node    (second (application/operand-nodes application)))
		    (values  (node/values node)))
	       (cond ((value-set/unknown? values))
		     ((for-all? (value-set/singletons values)
			(lambda (value)
			  (or (value/closure? value)
			      (value/procedure? value))))
		      (if compiler:guru?
			  (pp `(rewrite: ,text => (QUOTE #T))))
		      (form/rewrite! text `(QUOTE ,#T))))))
	    ((call/%compiled-entry-maximum-arity?? text)
	     (let* ((node    (third (application/operand-nodes application)))
		    (values  (node/values node))
		    (arity+1 (first (call/operands text))))
	       (cond ((not (QUOTE/? arity+1)))
		     ((value-set/unknown? values))
		     ((for-all? (value-set/singletons values)
			(lambda (value)
			  (value/arity-matches? value
						(- (quote/text arity+1) 1))))
		      (if compiler:guru?
			  (pp `(rewrite: ,text => (QUOTE #T))))
		      (form/rewrite! text `(QUOTE ,#T))))))
	  
	    (else unspecific))))
  (for-each inspect (graph/applications graph)))

(define (graph/cleanup! graph)
  ;; After dataflow has comuted the values at each node, we no longer need
  ;; the interconnections.
  
  (define (node/cleanup! node)
    (set-node/links-in! node #F)
    (set-node/links-out! node #F))

  (for-each node/cleanup!  (graph/nodes graph)))

(define (graph/display-statistics! graph)
  (define (say . things) (for-each display things))
  (define (histogram aspect measure)
    (let ((data (map measure  (aspect graph)))
	  (hist (make-eq-hash-table)))
      (let loop ((data data))
	(if (not (null? data))
	    (let ((datum (car data)))
	      (hash-table/put! hist datum (+ 1 (hash-table/get hist datum 0)))
	      (loop (cdr data)))))
      (sort (hash-table->alist hist) (lambda (u v) (< (car u) (car v))))))

  (define ((edge-count aspect) node)
    (node-set/size (aspect node)))

  (define (count-pairs object)
    (define (count it n)
      (if (pair? it)
	  (count (car it) (count (cdr it) (+ n 1)))
	  n))
    (count object 0))

  (say "\n; "  graph
       "  "  (length (graph/nodes graph))
       " nodes  " (graph/node-count graph)
       "  (" (reduce + 0 (map (lambda (node) (if (node/connectivity node) 1 0))
			     (graph/nodes graph)))
       " with bit strings)")
  (say "\n; Source has "  (count-pairs (graph/program graph))  " pairs.")
  (say "\n; "
       (reduce + 0 (map (edge-count node/links-in)
			(graph/nodes graph)))
       " in-edges, "
       (reduce + 0 (map (edge-count node/links-out)
			(graph/nodes graph)))
       " out-edges.")
  ;;(say "\n; Histogram ((out-edges . node-count) ...)")
  ;;(pp (histogram graph/nodes (lambda (node) (length (node/links-out node))))
  ;;    (current-output-port) #F)
  (say "\n; Histogram ((in-edges . node-count) ...)")
  (pp (histogram graph/nodes (edge-count node/links-in))
      (current-output-port) #F))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-all pattern program)

  (define (match? pattern text)
    (or (eq? pattern '?)
	(eq? pattern text)
	(and (symbol? pattern) (symbol? text)
	     (string-ci=? (symbol-name pattern) (symbol-name text)))
	(and (pair? pattern) (pair? text)
	     (match? (car pattern) (car text))
	     (match? (cdr pattern) (cdr text)))))

  (define (search text)
    (if (match? pattern text)
	(list text)
	(let loop ((text* text)
		   (frobs '()))
	  (cond ((not (pair? text*))
		 frobs)
		((search (car text*))
		 => (lambda (x) (loop (cdr text*) (append! frobs x))))
		(else (loop (cdr text*) frobs))))))

  (set! *finds* (search program))
  *finds*)

(define *finds*)

(define (find pattern #!optional program)
  (find-all pattern (if (default-object? program)
		    *current-phase-input*
		    program))
  (if (null? *finds*)
      #F
      (car *finds*)))

(define (find* pattern #!optional program)
  (find-all pattern (if (default-object? program)
		    *current-phase-input*
		    program))
  *finds*)

(define (refind)
  (if (or (not *finds*)
	  (null? (cdr *finds*)))
      #F
      (begin (set! *finds* (cdr *finds*))
	     (car *finds*))))
	     
	      
(define (parents expr #!optional program)
  ;; EQ? parents of an expression
  (define (find text)
    (if (pair? text)
	(apply
	 append
	 (if (there-exists? text (lambda (x) (eq? x expr)))
	     (list text)
	     '())
	 (map find text))
	'()))
  (find (if (default-object? program)
	    *current-phase-input*
	    program)))

;;;
;;; Local Variables:
;;; eval: (put 'graph/for-each-node 'scheme-indent-function 1)
;;; eval: (put 'node-set/for-each 'scheme-indent-function 1)
;;; End:
;;;
;;; Edwin variables:
;;; End:
;;;
