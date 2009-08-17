#| -*-Scheme-*-

$Id: 0e0c94352274cd2cef3f667ed08cc6c5ffd275c9 $

Copyright (c) 1994, 1999 Massachusetts Institute of Technology

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

(declare (usual-integrations))

;;; Widen parameter lists where a known closure is being passed around, so that
;;; the component parts can be passed rather than the closure object itself.  We
;;; do this only when the closure can be eliminated entirely; hence, the
;;; requirement that the closure not escape.

(define (reject-reason closure)
  ;; Returns the reason a closure can't be considered for widening.
  ;; This is referred to later as "undeniably-dirty?".  Current
  ;; reasons are:
  ;;   1. The value ESCAPES.
  ;;   2. There is some use of the value where other values might also
  ;;      occur.  (Could be weakened to sites where other values occur
  ;;      that don't widen the same way.)
  ;;   3. There is some use of the value that we don't know how to
  ;;      widen.  We can widen expressions that create closures,
  ;;      references to closed over variables, operands of
  ;;      applications, bindings of LET or LETREC variables, formal
  ;;      parameters of LAMBDA, and the expressions which fetches a
  ;;      stack closure.
  (cond ((value/closure/escapes? closure) 'escapes)
	#| ((eq? 'STACK (value/closure/kind closure)) 'stack-closure) |#
	(else
	 (let ((reasons '()))
	   (define (new-reason! reason)
	     (set! reasons (cons reason reasons))
	     unspecific)
	   (do ((nodes (value/nodes closure) (cdr nodes)))
	       ((null? nodes)
		(if (null? reasons)
		    (if (eq? 'STACK (value/closure/kind closure))
			(begin
			  (internal-warning "I want to widen a stack closure"
					    closure)
			  #F)
			#F)
		    reasons))
	     (let ((node (car nodes)))
	       (cond ((not (node/unique-value node))
		      (new-reason! (list 'not-unique node)))
		     #| ((continuation-invocation-operand? node)
			 (new-reason! (list 'continuation-invocation node)))
		     |#
		     ((not (or #| (not (null? (node/uses/operator node))) |#
				  (closure-constructor-node? node)
				  (closure-slot-node? node)
				  (not (null? (node/uses/operand node)))
				  (let-binding-node? node)
				  (node/formal-parameter? node)
				  (fetch-stack-closure-node? node)))
		      (new-reason! (list 'unusual-use node)))
		     (else 'OK))))))))

(define widen-parameter-lists
  ;; Generate the data flow graph, separate out the closures that
  ;; appear to be widenable, then do a more careful analysis to
  ;; actually choose the ones which will be widened (i.e. converted
  ;; from single objects into a set of the closed-over values).
  (make-dataflow-analyzer
   (lambda (new old) (widen/remember new old))
   (lambda (original-code graph)
     original-code			; ignore
     ;;(write-line graph)
     (rewrite-as-widened graph
			 (graph/program graph)
			 (analyze-widenable-closures
			  (list-transform-negative (graph/closures graph)
			    reject-reason))))))

(define closure/name
  (let* ((name (->pattern-variable 'NAME))
	 (pattern
	 `(CALL ',%make-heap-closure '#F
		(LAMBDA (,(->pattern-variable 'CONTINUE)
			 ,name 
			 . ,(->pattern-variable 'FORMALS))
		  ,(->pattern-variable 'BODY))
		. ,(->pattern-variable 'CRAP))))
    (lambda (closure)
      (symbol->string
       (case (value/closure/kind closure)
	 ((STACK)   'STACK-CLOSURE)
	 ((TRIVIAL) 'TRIVIAL-CLOSURE)
	 ((HEAP)    (let ((match (form/match pattern (value/text closure))))
		      (if match
			  (cadr (assq name match))
			  (internal-error "Heap closure naming error"))))
	 (else (internal-error "Unknown closure type")))))))

;; Functions to retrieve the representations (list of variable names
;; to replace) and name maps (maps from old closed variable name to
;; list of new variable names) for each of the widenable closures.
(define value/closure.representation 'LATER)
(define set-value/closure.representation! 'LATER)
(define value/closure.name-map 'LATER)
(define set-value/closure.name-map! 'LATER)

;; Now initialize those functions
(let ((representations (make-attribute))
      (name-maps       (make-attribute)))
  ;; For each closure that is widenable, we store the representation
  ;; we choose for the closure as a list of closed over variables.
  (set! value/closure.representation
	(lambda (value/closure) (get-attribute value/closure representations)))
  (set! set-value/closure.representation!
	(lambda (value/closure rep)
	  (set-attribute! value/closure representations rep)))
  (set! value/closure.name-map
	(lambda (value/closure) (get-attribute value/closure name-maps)))
  (set! set-value/closure.name-map!
	(lambda (value/closure rep)
	  (set-attribute! value/closure name-maps rep))))

(define (analyze-widenable-closures widenable-closures)
  ;; The WIDENABLE-CLOSURES all have the property that whenever they appear as a
  ;; value somewhere, they are the only possible value, and they appear only in
  ;; restricted contexts, as defined by REJECT-REASON.

  ;; Returns the list of closures that will actually be widened.  As a
  ;; side-effect, it computes and stores the representations and name maps for
  ;; these closures.

  (define (transitively-dirty? undeniably-dirty? components adj)
    ;; Given a set of nodes (COMPONENTS) and an ADJacency function
    ;; (from nodes to a list of adjacent nodes), return a function on
    ;; the nodes which is true IFF the node is UNDENIABLY-DIRTY? or is
    ;; adjacent to a node that is transitively-dirty.  The algorithm
    ;; is simply depth first search.
    (define dirty? (make-attribute))
    (define seen? (make-attribute))
    (define (visit u)
      (if (not (get-attribute u seen?))
	  (begin
	    (set-attribute! u seen? #T)
	    (if (undeniably-dirty? u)
		(set-attribute! u dirty? #T)
		(for-every (adj u)
		  (lambda (v)
		    (if (visit v) (set-attribute! u dirty? #T)))))))
      (get-attribute u dirty?))
    (for-each visit components)
    (lambda (u) (get-attribute u dirty?)))

  (let ((closure.adjacent-closures (make-attribute))
	(closure.closed-over-non-closures? (make-attribute)))

    ;; A closure C (in WIDENABLE-CLOSURES) is adjacent to other
    ;; widenable-closures over which it is closed.
    (define (adj c) (or (get-attribute c closure.adjacent-closures) '()))
    (define (adj! c1 c2)
      (set-attribute! c1 closure.adjacent-closures
		      (cons c2 (adj c1))))

    ;; True IFF a closure C (in WIDENABLE-CLOSURES) is closed over
    ;; anything other than another one of the widenable-closures.
    (define (external? c)
      (get-attribute c closure.closed-over-non-closures?))
    (define (external! c)
      (set-attribute! c closure.closed-over-non-closures? #T))

    ;; Initialize the ADJ and EXTERNAL? functions
    (for-every widenable-closures
      (lambda (c)
	(let ((values-closed-over
	       (vector->list (value/closure/location-nodes c))))
	  (for-every (map node/unique-value values-closed-over)
	    (lambda (value)
	      (if (memq value widenable-closures)
		  (adj! c value)
		  (external! c)))))))

    (let* ((components (strongly-connected-components widenable-closures adj))
	   (scc-graph (s-c-c->adj components adj)))
      ;; Identify the strongly connected components of the graph of
      ;; widenable closures closed over one another.  All of the
      ;; closures in a given component either widen or don't widen.
      ;; When they widen, they widen into an odd kind of union of
      ;; their closed over components.

      (define (cyclic? component)
	;; By their nature, strongly-connected-components that have
	;; more than one element are cyclic.
	(or (not (null? (cdr component)))
	    (let ((closure (car component)))
	      (there-exists? (adj closure)
		(lambda (adjacent) (eq? closure adjacent))))))

      (define (primordially-dirty? component)
	;; A strongly connected component can't be widened if it is
	;; cyclic and any component is closed over something outside
	;; itself, since this would lead to an infinite number of
	;; items in its widened representation.
	(and (cyclic? component)
	     (there-exists? component external?)))

      (define (generate-reps-and-name-maps! closures)
	(define seen? (make-attribute))
	(define (visit u)
	  ;; Returns the representation of this closure and calculates
	  ;; the name map.
	  (if (get-attribute u seen?)
	      (value/closure.representation u)
	      (begin
		(set-attribute! u seen? #T)
		(set-value/closure.representation! u '())
		(let ((values-closed-over
		       (vector->list (value/closure/location-nodes u)))
		      (names-closed-over
		       (vector->list (value/closure/location-names u)))
		      (closure-name (closure/name u))
		      (the-map '()))
		  (define (new! old-name new-names)
		    (define (new-name name)
		      (dataflow/new-name (string-append
					  closure-name "."
					  (symbol-name old-name) "/"
					  (symbol-name name) "+")))
		    (set! the-map
			  `((,old-name . ,(map new-name new-names))
			    . ,the-map))
		    'OK)
		  (for-each
		   (lambda (value-node name)
		     (let ((neighbor (node/unique-value value-node)))
		       (new! name 
			     (if (memq neighbor closures)
				 (visit neighbor)
				 (list name)))))
		    values-closed-over names-closed-over)
		  (set-value/closure.name-map! u the-map)
		  (let ((rep (apply append (reverse (map cdr the-map)))))
		    ;; The choice of representation is not freely made
		    ;; here.  The actual order must match the order of
		    ;; the value-computing expressions that appear where
		    ;; the closure is created, and we don't want to have
		    ;; to permute those expressions.
		    (set-value/closure.representation! u rep)
		    rep)))))
	(for-each visit closures))

      (let* ((is-dirty-because-of-kids?
	      (transitively-dirty? primordially-dirty? components scc-graph))
	     (finally-widenable-closures
	      (apply append
		     (list-transform-negative components
		       (lambda (component)
			 (and (cyclic? component)
			      (is-dirty-because-of-kids? component)))))))
	(if (not (null? finally-widenable-closures))
	    (if compiler:guru?
		(pp `(finally ,(length finally-widenable-closures) widened))))
	(generate-reps-and-name-maps! finally-widenable-closures)
	finally-widenable-closures))))

(define-macro (define-widen-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'WIDEN/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdddr bindings)
			  '(handler graph name-map form)
			  '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
          (let ((handler
		 (lambda ,(cons* (first bindings) (second bindings)
				 (third bindings) names)
		   ,@body)))
            (named-lambda (,proc-name graph name-map form)
	      ;; These handlers return a list of forms, to account for the fact
	      ;; that widening turns single expressions into multiple ones
	      ,code)))))))

(define (widen/expr graph name-map expr)
  ;; Maps a single expression to a list of (zero or more) expressions
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((ACCESS)  (widen/access graph name-map expr))
    ((BEGIN)   (widen/begin graph name-map expr))
    ((CALL)    (widen/call graph name-map expr))
    ((DECLARE) (widen/declare graph name-map expr))
    ((DEFINE)  (widen/define graph name-map expr))
    ((DELAY)   (widen/delay graph name-map expr))
    ((IF)      (widen/if graph name-map expr))
    ((IN-PACKAGE)  (widen/in-package graph name-map expr))
    ((LAMBDA)  (widen/lambda graph name-map expr))
    ((LET)     (widen/let graph name-map expr))
    ((LETREC)  (widen/letrec graph name-map expr))
    ((LOOKUP)  (widen/lookup graph name-map expr))
    ((OR)      (widen/or graph name-map expr))
    ((QUOTE)   (widen/quote graph name-map expr))
    ((SET!)    (widen/set! graph name-map expr))
    ((THE-ENVIRONMENT) (widen/the-environment graph name-map expr))
    ((UNASSIGNED?)  (widen/unassigned? graph name-map expr))
    (else     (illegal expr))))


(define (widen->expr graph name-map expr)
  ;; Requires that the widened version be exactly one expression, and
  ;; returns that expression
  (let ((result (widen/expr graph name-map expr)))
    (if (not (singleton-list? result))
	(internal-error "Did not widen to ONE expression" expr result))
    (car result)))

(define (widen/expr* graph name-map exprs)
  ;; Returns a list of lists of expressions
  (map (lambda (exp) (widen/expr graph name-map exp)) exprs))

(define (widen/flatten-expr* graph name-map exprs)
  ;; Maps a list of expressions to a list of expressions (not
  ;; necessarily length preserving, of course)
  (apply append (widen/expr* graph name-map exprs)))

(define-widen-handler LOOKUP (graph name-map LOOKUP-form name)
  ;; If the name being looked up is one to widen, return lookups of
  ;; the names to which it expands; otherwise just return the original
  ;; lookup
  graph					; Not used
  (cond ((assq name name-map)
	 => (lambda (entry)
	      (map (lambda (name) `(LOOKUP ,name)) (cdr entry))))
	(else (list LOOKUP-form))))

(define (widen/rewrite-bindings name-map names value-nodes continue)
  ;; Calls CONTINUE with a (possibly) new name-map and names.
  (define (rename formal closure)
    ;; Return a list of new names to reference the widened form of a
    ;; given FORMAL whose value will be the value represented by CLOSURE
    (map (lambda (closed-over)
	   (dataflow/new-name
	    (string-append (symbol->string formal) "."
			   (symbol->string closed-over)
			   "-")))
      (value/closure.representation closure)))
  (let loop ((name-map name-map)
	     (new-names '())
	     (names names)
	     (nodes value-nodes))
    (cond ((null? nodes)
	   (continue name-map (reverse new-names)))
	  ((lambda-list-keyword? (car names))
	   (loop name-map (cons (car names) new-names) (cdr names) nodes))
	  ((widen/rewrite? (car nodes))
	   (let* ((this (car nodes))
		  (formal (car names))
		  (closure (node/unique-value this))
		  (rep (rename formal closure)))
	     (loop `((,formal . ,rep) . ,name-map)
		   `(,@(reverse rep) . ,new-names)
		   (cdr names)
		   (cdr nodes))))
	  (else (loop name-map
		      `(,(car names) . ,new-names)
		      (cdr names)
		      (cdr nodes))))))

(define-widen-handler LAMBDA (graph name-map LAMBDA-form lambda-list body)
  ;; The body needs to be rewritten.  If the parameter list needs widening it
  ;; will require that the body be rewritten with additional local variables
  ;; alpha-renamed.  Widening happens after CPS conversion, so the body
  ;; shouldn't need widening.

  (define (graph->parameter-nodes graph lambda-expr)
    (value/procedure/input-nodes
     (node/the-procedure-value 
      (graph/text->node graph lambda-expr))))

  (no-widening-allowed graph LAMBDA-form)
  (widen/rewrite-bindings
   name-map
   lambda-list
   (graph->parameter-nodes graph LAMBDA-form)
   (lambda (name-map lambda-list)
     (widen/simple-rewrite
      `(LAMBDA ,lambda-list ,(widen->expr graph name-map body))
      LAMBDA-form))))

(define (widen/let-like graph name-map let-or-letrec form bindings body)
  (let ((bound-names (map car bindings))
	(binding-exprs (map cadr bindings)))
    (widen/rewrite-bindings
     name-map
     bound-names
     (map (lambda (expr) (graph/text->node graph expr)) binding-exprs)
     (lambda (new-name-map names)
       (let* ((which-map (if (eq? let-or-letrec 'LET) name-map new-name-map))
	      (value-exprs
	       (widen/flatten-expr* graph which-map binding-exprs)))
	 (if (not (= (length value-exprs) (length names)))
	     (internal-error "LET expansion error" (list names value-exprs)))
	 (widen/simple-rewrite
	  `(,let-or-letrec
	    ,(map list names value-exprs)
	    ,(widen->expr graph new-name-map body))
	  form))))))

(define-widen-handler LET (graph name-map LET-form bindings body)
  (no-widening-allowed graph LET-form)
  (widen/let-like graph name-map 'LET LET-form bindings body))

(define-widen-handler LETREC (graph name-map LETREC-form bindings body)
  (no-widening-allowed graph LETREC-form)
  (widen/let-like graph name-map 'LETREC LETREC-form bindings body))

;;; CONTAINERS: When a non-widenable closure is closed over a
;;; widenable closure, we choose to pack and unpack the elements of
;;; the widened closure in the single slot provided by the unwidened
;;; one.  An alternate (preferable?) choice would be to alter the
;;; representation of the non-widenable closure to have extra slots,
;;; but that would require transitively rewriting all references to
;;; those closures.

(define (widen/create-container exprs)
  ;; We choose to use #F ('deleted-container just for now so we can see it)
  ;; where it expands to 0 values, the value itself where it expands
  ;; to one value, a pair for 2 values, and a vector for any other
  ;; case.
  (if compiler:guru?
      (pp `("Creating container" ,(length exprs))))
  (case (length exprs)
    ;;((0) `'#F)
    ((0) `'deleted-container)
    ((1) (car exprs))
    ((2) `(CALL ',%cons '#F ,(car exprs) ,(cadr exprs)))
    (else `(CALL ',%vector '#F . ,exprs))))

(define (widen/unwrap-container n expr)
  ;; N is the number of items in the container, and EXPR is the
  ;; expression that generates the container's value.
  ;; NOTE: We expect RTL CSE to remove the redundant evaluations of EXPR.
  ;;(pp `("Unwrapping containter (form below)" ,n))
  ;;(kmp/pp expr)
  (case n
    ((0) '())
    ((1) (list expr))
    ((2) `((CALL ',%car '#F ,expr)
	   (CALL ',%cdr '#F ,expr)))
    (else (let loop ((m (- n 1))
		     (result '()))
	    (if (negative? m)
		result
		(loop (- m 1)
		      `((CALL ',%vector-ref '#F ,expr ',m) . ,result)))))))

(define (no-CONT-allowed cont)
  (if (not (equal? CONT ''#F))
      (internal-error "No continuation allowed" cont)))

(define (no-widening-allowed graph form)
  (if (widen/rewrite? (graph/text->node graph form))
      (internal-error "Widening non-widenable form" form)))

(define (widen/handler/make-closure graph name-map form rator cont rands)
  ;; (CALL ',%make-????-closure '#F  <lambda-expr> 'VECTOR <value>*)
  ;;       -------- rator ----- cont ------------ rands ------------

  (define (containerize exprs node)
    ;; EXPRS are the expressions corresponding to the value for this NODE.
    (if (widen/rewrite? node)
	(if (not (= (length exprs)
		    (length (value/closure.representation node))))
	    (internal-error
	     "Representation mismatch of widened closed value" exprs node)
	    (widen/create-container exprs))
	(if (not (singleton-list? exprs))
	    (internal-error
	     "Representation mismatch of non-widened closed value"
	     exprs node)
	    (car exprs))))

  ;; If a closure is being widened, it is converted to just
  ;; the rewritten <value>* expressions.  Otherwise, any closed-over
  ;; widenable closures must be converted to containers (see
  ;; WIDEN/CREATE-CONTAINER, above).

  (no-CONT-allowed cont)
  (let ((value-exprs (cddr rands))
	(the-closure-node (graph/text->node graph form)))
    (let ((closure (node/unique-value the-closure-node))
	  (exprs (widen/expr* graph name-map value-exprs)))
      (if (widen/rewrite? the-closure-node)
	  (let ((values (apply append exprs)))
	    (if (not (= (length values)
			(length (value/closure.representation closure))))
		(internal-error
		 "Representation mismatch of make-heap-closure"
		 rator rands values)
		values))
	  (widen/simple-rewrite
	   `(CALL ,rator ,cont
		  ,(widen->expr graph name-map (car rands))
		  ,(cadr rands)
		  . ,(map containerize
			  exprs
			  (map node/unique-value
			       (vector->list
				(value/closure/location-nodes closure)))))
	   form)))))

(define (widen/handler/%make-heap-closure graph name-map form rator cont rands)
  ;; (CALL ',%make-heap-closure '#F  <lambda-expr> 'VECTOR <value>*)
  ;;       -------- rator ----- cont ------------ rands ------------
  (no-CONT-allowed cont)
  (widen/handler/make-closure graph name-map form rator cont rands))

(define (widen/handler/%make-stack-closure
	 graph name-map form rator cont rands)
  ;; (CALL ',%make-stack-closure '#F <lambda-expr or '#F> 'VECTOR <value>*)
  ;;       -------- rator ------ cont --------------- rands --------------
  (no-CONT-allowed cont)
  (widen/handler/make-closure graph name-map form rator cont rands))

(define (widen/handler/%make-trivial-closure
	 graph name-map form rator cont rands)
  ;; (CALL ',%make-trivial-closure '#F <lambda-expression or LOOKUP>)
  ;;       --------- rator ------- cont ----------- rands ----------
  (no-CONT-allowed cont)
  (let ((the-closure-node (graph/text->node graph form)))
    (if (widen/rewrite? the-closure-node)
	'()				; Vanishes entirely!
	(widen/simple-rewrite
	 `(CALL ,rator ,cont ,(widen->expr graph name-map (car rands)))
	 form))))
 
(define (widen/closure-ref graph name-map form rator cont rands)
  ;; (CALL ',%????-closure-ref '#F <closure> <offset> 'NAME)
  ;;       ------ rator ------ cont ------- rands ---------
  ;; NOTE: <offset> is assumed not to require examination (i.e. it
  ;; doesn't contain names that are remapped by the NAME-MAP)
  (define (widen-closure-ref closure closure-exprs name)
    (let ((rep-vector (list->vector (value/closure.representation closure)))
	  (name-map   (value/closure.name-map closure)))
      (if (not (= (vector-length rep-vector) (length closure-exprs)))
	  (internal-error "Closure didn't widen as expected"
			  closure closure-exprs rep-vector))
      (let ((entry (assq name name-map)))
	(if (not entry)
	    (internal-error "Closure doesn't have desired slot"
			    closure rep-vector name))
	(map (lambda (name)
	       (list-ref closure-exprs (vector-index rep-vector name)))
	     (cdr entry)))))
  (let ((my-value      (graph/text->node graph form))
	(closure-node  (graph/text->node graph (car rands)))
	(closure-exprs (widen/expr graph name-map (car rands))))
    (if (widen/rewrite? closure-node)
	(widen-closure-ref
	 (node/unique-value closure-node) closure-exprs (cadr (third rands)))
	(if (not (singleton-list? closure-exprs))
	    (internal-error
	     "Unexpected widening of closure being dereferenced"
	     my-value closure-exprs)
	    (let ((slot-extractor `(CALL ,rator ,cont ,(car closure-exprs)
					 ,(second rands) ,(third rands))))
	      (if (widen/rewrite? my-value)
		  (let* ((result-closure (node/unique-value my-value))
			 (rep (value/closure.representation result-closure)))
		    (widen/unwrap-container (length rep) slot-extractor))
		  (list slot-extractor)))))))

(define (widen/handler/%heap-closure-ref graph name-map form rator cont rands)
  ;; (CALL ',%heap-closure-ref '#F <closure> <offset> 'NAME)
  ;;       ------ rator ------ cont ------- rands ---------
  (no-CONT-allowed cont)
  (widen/closure-ref graph name-map form rator cont rands))

(define (widen/handler/%stack-closure-ref graph name-map form rator cont rands)
  ;; (CALL ',%stack-closure-ref '#F <closure> <offset> 'NAME)
  (no-CONT-allowed cont)
  (widen/closure-ref graph name-map form rator cont rands))

(define (widen/handler/%internal-apply
	 graph name-map form rator cont rands)
  ;; (CALL ',%internal-apply           <cont> 'NARGS <procedure> <value>*)
  ;; (CALL ',%internal-apply-unchecked <cont> 'NARGS <procedure> <value>*)
  ;;       ------ rator ----           -cont- --------- rands -----------
  form					; Not used
  (let ((widened-operands
	 (widen/flatten-expr* graph name-map (cddr rands))))
    (widen/simple-rewrite
     `(CALL ,rator
	    ,(widen->expr graph name-map cont)
	    ',(length widened-operands)
	    ,(widen->expr graph name-map (second rands))
	    . ,widened-operands)
     form)))

(define (widen/handler/%fetch-stack-closure
	 graph name-map form rator cont rands)
  ;; (CALL ',%fetch-stack-closure '#F 'VECTOR)
  name-map rator rands			; Not used
  (no-widening-allowed graph form)
  (no-CONT-allowed cont)
  (list form))

(define (widen/handler/%fetch-continuation
	 graph name-map form rator cont rands)
  ;; (CALL ',%fetch-continuation '#F)
  name-map rator			; Not used
  (no-CONT-allowed cont)
  (no-widening-allowed graph form)
  (if (not (null? rands))
      (internal-error "FETCH-CONTINUATION with operands" form rands))
  (list form))

(define (widen/handler/%invoke-continuation
	 graph name-map form rator cont rands)
  ;; (CALL ',%invoke-continuation <continuation> <value>*)
  form 					; Not used
  (widen/simple-rewrite
   `(CALL ,rator ,(widen->expr graph name-map cont)
	  . ,(widen/flatten-expr* graph name-map rands))
   form))

(define (widen/handler/default graph name-map form rator cont rands)
  form					; Not used
  (widen/simple-rewrite
   `(CALL ,(widen->expr graph name-map rator)
	  ,(widen->expr graph name-map cont)
	  . ,(widen/flatten-expr* graph name-map rands))
   form))

(define-widen-handler CALL (graph name-map CALL-form rator cont #!rest rands)
  (define (use method)
    (method graph name-map CALL-form rator cont rands))
  (if (QUOTE/? rator)
      (let ((operator (QUOTE/text rator)))
	(cond ((eq? operator %make-heap-closure)
               (use widen/handler/%make-heap-closure))
              ((eq? operator %make-stack-closure)
               (use widen/handler/%make-stack-closure))
              ((eq? operator %make-trivial-closure)
               (use widen/handler/%make-trivial-closure))
              ((eq? operator %heap-closure-ref)
               (use widen/handler/%heap-closure-ref))
              ((eq? operator %stack-closure-ref)
               (use widen/handler/%stack-closure-ref))
              ((or (eq? operator %internal-apply)
		   (eq? operator %internal-apply-unchecked))
               (use widen/handler/%internal-apply))
	      ((eq? operator %fetch-stack-closure)
               (use widen/handler/%fetch-stack-closure))
	      ((eq? operator %fetch-continuation)
               (use widen/handler/%fetch-continuation))
              ((eq? operator %invoke-continuation)
               (use widen/handler/%invoke-continuation))
	      (else (use widen/handler/default))))
      (use widen/handler/default)))

(define-widen-handler QUOTE (graph name-map QUOTE-form object)
  graph name-map			; ignored
  (no-widening-allowed graph QUOTE-form)
  (widen/simple-rewrite `(QUOTE ,object)
			QUOTE-form))

(define-widen-handler DECLARE (graph name-map DECLARE-form #!rest anything)
  graph name-map
  (no-widening-allowed graph DECLARE-form)
  (widen/simple-rewrite `(DECLARE ,@anything)
			DECLARE-form))

(define-widen-handler BEGIN (graph name-map BEGIN-form #!rest actions)
  (define (separate l cont)
    (if (null? l)
	(cont '() '())
	(let loop ((before '())
		   (after l))
	  (if (null? (cdr after))
	      (cont (reverse before) after)
	      (loop (cons (car after) before) (cdr after))))))
  BEGIN-form				; Unused
  (separate
   actions
   (lambda (for-effect value)
     (let ((for-effect-exprs (widen/flatten-expr* graph name-map for-effect))
	   (value-exprs (widen/flatten-expr* graph name-map value)))
       (cond ((null? value-exprs)
	      (if (null? for-effect-exprs)
		  '()			; Vanishes entirely
		  (internal-error "BEGIN with effects and vanishing value")))
	     ((not (null? (cdr value-exprs)))
	      (internal-error "BEGIN with multiple values" BEGIN-form))
	     (else
	      (widen/simple-rewrite
	       `(BEGIN
		  ,@for-effect-exprs
		  ,(car value-exprs))
	       BEGIN-form)))))))

(define-widen-handler IF (graph name-map IF-form pred conseq alt)
  (no-widening-allowed graph IF-form)
  (widen/simple-rewrite `(IF ,(widen->expr graph name-map pred)
			     ,(widen->expr graph name-map conseq)
			     ,(widen->expr graph name-map alt))
			IF-form))

(define-widen-handler SET! (graph name-map SET!-form name value)
  (no-widening-allowed graph SET!-form)
  (if (assq name name-map)
      (internal-error "Widening SET! variable" name))
  (widen/simple-rewrite `(SET! ,name ,(widen->expr graph name-map value))
			SET!-form))

(define-widen-handler ACCESS (graph name-map ACCESS-form name env-expr)
  (no-widening-allowed graph ACCESS-form)
  (if (assq name name-map)
      (internal-error "Widening ACCESS variable" name))
  (widen/simple-rewrite
   `(ACCESS ,name ,(widen->expr graph name-map env-expr))
   ACCESS-form))

(define-widen-handler UNASSIGNED? (graph name-map UNASSIGNED?-form name)
  graph name-map			; ignored
  (no-widening-allowed graph UNASSIGNED?-form)
  (if (assq name name-map)
      (internal-error "Widening UNASSIGNED? variable" name)
      (widen/simple-rewrite
       `(UNASSIGNED? ,name)
       UNASSIGNED?-form)))

(define-widen-handler OR (graph name-map OR-form pred alt)
  (no-widening-allowed graph OR-form)
  (widen/simple-rewrite
   `(OR ,(widen->expr graph name-map pred)
	,(widen->expr graph name-map alt))
   OR-form))

(define-widen-handler DELAY (graph name-map DELAY-form expr)
  (no-widening-allowed graph DELAY-form)
  (widen/simple-rewrite
   `(DELAY ,(widen->expr graph name-map expr))
   DELAY-form))

(define-widen-handler DEFINE (graph name-map DEFINE-form name value)
  (no-widening-allowed graph DEFINE-form)
  (widen/simple-rewrite
   `(DEFINE ,name ,(widen->expr graph name-map value))
   DEFINE-form))

(define-widen-handler IN-PACKAGE
  (graph name-map IN-PACKAGE-form envexpr bodyexpr)
  (no-widening-allowed graph IN-PACKAGE-form)
  (widen/simple-rewrite
   `(IN-PACKAGE ,(widen->expr graph name-map envexpr)
      ,(widen->expr graph name-map bodyexpr))
   IN-PACKAGE-form))

(define-widen-handler THE-ENVIRONMENT (graph name-map THE-ENVIRONMENT-form)
  graph name-map			; Ignored
  (no-widening-allowed graph THE-ENVIRONMENT-form)
  (widen/simple-rewrite
   `(THE-ENVIRONMENT)
   THE-ENVIRONMENT-form))

(define widen/rewrite! 'LATER)
(define widen/rewrite? 'LATER)
(let ((*nodes-to-rewrite* (make-attribute)))
  (set! widen/rewrite!
	(lambda (node) (set-attribute! node *nodes-to-rewrite* #T)))
  (set! widen/rewrite?
	(lambda (node) (get-attribute node *nodes-to-rewrite*)))
  unspecific)

(define (rewrite-as-widened graph code widenable)
  ;; Rewrite CODE after widening all references to the WIDENABLE closures.  The
  ;; widening is done by side-effecting CODE, and the rewritten CODE is
  ;; returned.
  (sample/1 '(widen/widened-closures count) (length widenable))
  (for-every widenable
    (lambda (closure)
      ;; Mark the closures and all nodes at which the value arrives as
      ;; rewritable.
      (widen/rewrite! closure)
      (for-every (value/nodes closure) widen/rewrite!)))
  (form/rewrite! code (widen->expr graph '() code))
  code)

(define (closure/closed-over-names closure)
  (vector->list (value/closure/location-names closure)))

(define (closure-constructor-text? text)
  (or (CALL/%make-heap-closure? text)
      (CALL/%make-trivial-closure? text)
      (CALL/%make-stack-closure? text)))

(define (closure-constructor-node? node)
  (and (closure-constructor-text? (node/text node))
       (string? (node/name node))))

(define (closure-constructor-node/closed-expressions node)
  (if (eq? 'TRIVIAL (value/closure/kind (node/unique-value node)))
      '()
      (cdr (cddddr (node/text node)))))

(define (fetch-stack-closure-node? node)
  (CALL/%fetch-stack-closure? (node/text node)))

(define let-binding-node?
  (let ((pattern `(LET ,(->pattern-variable 'BINDINGS)
		    ,(->pattern-variable 'BODY))))
    (lambda (node)
      (and
       (form/match pattern (node/text node))
       #T))))

(define (closure-slot-node? node)
  (and (closure-constructor-text? (node/text node))
       (pair? (node/name node))))

(define-integrable (singleton-list? x)
  (and (pair? x)
       (null? (cdr x))))

(define (widen/remember new old)
  (code-rewrite/remember new old))

(define (widen/simple-rewrite new old)
  (list (widen/remember* new old)))

(define (widen/remember* new copy)
  (code-rewrite/remember* new
			  (code-rewrite/original-form copy)))