#| -*-Scheme-*-

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

;;;; Reduce debugging expressions to canonical form
;;; package: (compiler midend)

(declare (usual-integrations))

#|

This phase works by constructing an expression graph containing all
the possible ways of finding the value of an object.  Variables (user
variables and intermediate variables) are nodes in the graph.  Edges
connect nodes to equivlent expressions.

The graph is constructed from rewriting information provided by other
phases as they make representation decisions.  The basic rule for this
to work is that if any phase changes the prepresentaion of the object
(e.g. a cellified value) it must ensure that it generates a new name
for the new representation (i.e. the cell).  This, togenther with
alpha-renaming, ensures that different representations are never
confused.

The graph is traversed to generate path expressions that are used for
retrieving the values at dbg time.  This happens in two phases.  Only
a certain amount of information is available (in registers, on the
stack etc).  First, all the nodes that can reach a node which is
directly available are marked.  At graph construction time a certain
portion of the graph is similarly marked as available from static
sources (constants).  Then the graph is traversed from the target
expressions (the user level variables) to search for a path to
available information, and failing that, to static information.

An alternative would be to just dump the graph with the debugging
info, and search it at debugging time.  The graph is quite large, but
a lot of that is KMP expressions, and nodes which will never be
reachable.

|#

(define *dbg-graph*)

(define (dbg-reduce/top-level program)
  (fluid-let ((*dbg-graph* (dbg-rewrites->graph *dbg-rewrites*)))
    (dbg-reduce/expr
     (dbg-reduce/initial-env)
     ;; Only generate the DBG infor fo the top-level form if it has expression
     ;; dbg info.
     (if (and (LAMBDA/? program)	; which should be the case
	      (not (new-dbg-expression?
		    (code-rewrite/original-form/previous program))))
	 (lambda/body program)
	 program))
    )
	
  program)


(define-macro (define-dbg-reducer keyword bindings . body)
  (let ((proc-name (symbol-append 'DBG-REDUCE/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     ;; All handlers inherit ENV and FORM from the surrounding scope.
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
	       ,code)))))))

(define-dbg-reducer LOOKUP (name)
  name env				; unused
  unspecific)

(define-dbg-reducer QUOTE (object)
  object env				; unused
  unspecific)

(define-dbg-reducer LAMBDA (lambda-list body)
  (dbg-reduce/lambda* env form lambda-list body))

(define (dbg-reduce/lambda* env form lambda-list body)
  ;; Is there stuff on the stack (i.e. a stack model)?
  ;;
  ;;   (match body
  ;;     ((LET ((?frame-name
  ;;              (CALL ',%fetch-stack-closure _ '?frame-vector)) . _)
  ;;        _) =>
  ;;      YES)
  ;;     (else NO))
  (let* ((frame-vector
	  (and (LET/? body)
	       (pair? (let/bindings body))
	       (CALL/%fetch-stack-closure?
		(second (first (let/bindings body))))
	       (QUOTE/text 
		(CALL/%fetch-stack-closure/vector
		 (second (first (let/bindings body)))))))
	 (frame-name (and frame-vector (first (first (let/bindings body))))))

   ;; If this is a heap closure then (1) ensure the parent frames are
   ;; generated with access paths rooted at the closure object and (2)
   ;; remove the closure variable from the normal arguments.
   ;; (To determine this we really need to dispatch of %make-heap-closure
   ;; because the forst arg might incidentally be a closure variable).

    (let* ((arg-names  (cdr (lambda-list->names lambda-list))) ;ignore cont.

	   (closure-arg (and (pair? arg-names)
			     (closure-variable? (car arg-names))
			     (car arg-names)))
	   (parent-env (and closure-arg
			    (dbg-reduce/env/new-frame
			     env
			     `((,closure-arg . ,dbg-red/start-from-closure))
			     #F)))
	   (parent-path (and closure-arg
			     dbg-red/closure-invocation-parent-path))
	   (arg-names  (if closure-arg (cdr arg-names) arg-names)))

      ;; Calculate offsets into the interrupt stack-frame as parsed by the
      ;; continuation parser (which is slightly different to how it is
      ;; stored on the stack during execution.  This depends highly on
      ;; the assembly interface and conpar.

      (let* ((arg-slots  (length arg-names))
	     (arg-regs   (vector-length *rtlgen/argument-registers*))
	     (arg0-offset
	      (+ (min arg-regs arg-slots)
		 4))
	     (arg0-offset/stack
	      (+ arg0-offset arg-slots))
	     (alist  (map (lambda (name i)
			    (cons name 
				  (if (< i arg-regs)
				      (- arg0-offset i)
				      (- arg0-offset/stack i))))
			  arg-names
			  (iota arg-slots))))
	(let ((env*  (dbg-reduce/env/new-frame env alist frame-name)))
	  (dbg-reduce/reduce form env* parent-path parent-env)
	  (dbg-reduce/expr env* body))))))

(define-dbg-reducer LET (bindings body)
  (for-each (lambda (binding)
	      (dbg-reduce/expr env (cadr binding)))
    bindings)
  (dbg-reduce/bindings bindings #F)
  (dbg-reduce/expr env body))

(define-dbg-reducer LETREC (bindings body)
  ;; add static bindings
  (dbg-reduce/bindings bindings #T)
  (for-each (lambda (binding)
	      (dbg-reduce/expr env (cadr binding)))
    bindings)
  (dbg-reduce/expr env body))

(define (dbg-reduce/bindings bindings assume-static?)
  (for-each 
      (lambda (binding)
	(if (or assume-static? (form/static? (cadr binding)))
	    (let* ((name (car binding))
		   (node (dbg-red/find-node name)))
	      (if node
		  (let ((edge
			 (dbg-red/node/add-edge! node `(CC-ENTRY . ,name))))
		    (dbg-red/edge/statically-available! edge))
		  ;; If no node is found then this binding is likely a
		  ;; procedure which never corresponded to a user's procedure.
		  (if compiler:guru?
		      (internal-warning "Node absent" name))))))
    bindings))

(define-dbg-reducer IF (pred conseq alt)
  (dbg-reduce/expr env pred)
  (dbg-reduce/expr env conseq)
  (dbg-reduce/expr env alt))

(define-dbg-reducer DECLARE (#!rest anything)
  env anything				; unused
  unspecific)

(define-dbg-reducer BEGIN (#!rest actions)
  (dbg-reduce/expr* env actions))

(define-dbg-reducer CALL (rator cont #!rest rands)
  ;; For now just copy dbg expressions for CALLs.  Either they will be
  ;; dropped or used to create DBG-CONTINUATIONS for preservation type
  ;; calls.
  (code-rewrite/remember*! form (code-rewrite/original-form/previous form))
  (dbg-reduce/expr env rator)
  (dbg-reduce/expr env cont)
  (dbg-reduce/expr* env rands))

(define (dbg-reduce/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (dbg-reduce/quote env expr))
    ((LOOKUP)   (dbg-reduce/lookup env expr))
    ((CALL)     (dbg-reduce/call env expr))
    ((LAMBDA)   (dbg-reduce/lambda env expr))
    ((LET)      (dbg-reduce/let env expr))
    ((BEGIN)    (dbg-reduce/begin env expr))
    ((IF)       (dbg-reduce/if env expr))
    ((LETREC)   (dbg-reduce/letrec env expr))
    ((DECLARE)  (dbg-reduce/declare env expr))
    (else
     (illegal expr))))

(define (dbg-reduce/expr* env exprs)
  (map (lambda (expr)
	 (dbg-reduce/expr env expr))
       exprs))

(define-structure
    (dbg-reduce/env
     (conc-name dbg-reduce/env/)
     (constructor dbg-reduce/env/%make))
  ;; Static objects: a list of `labels'
  static				
  ;; Dynamic objects (in current procedure parameters).  A list of pairs
  ;; (name . interrupt-frame-offset).
  parameters
  frame-name)				; #F or a symbol

(define (dbg-reduce/initial-env)
  (dbg-reduce/env/%make '() '() #F))

(define (dbg-reduce/env/new-frame env parameters frame-name)
  (dbg-reduce/env/%make (dbg-reduce/env/static env)
			parameters
			frame-name))

(define (dbg-reduce/env/extend-static env static*)
  (dbg-reduce/env/%make (append static* (dbg-reduce/env/static env))
			(dbg-reduce/env/parameters env)
			(dbg-reduce/env/frame-name env)))

(define (dbg-reduce/env/lookup env name)
  ;; -> #F, stack offset, or ??
  (cond ((assq name (dbg-reduce/env/parameters env))	 => cdr)
	((memq name (dbg-reduce/env/static env))    name)
	(else #F)))


(define (dbg-reduce/reduce form env parent-path parent-env)
  (define (find-block dbg-info)
    (define (expression-block e)
      (and (new-dbg-expression? e)
	   (new-dbg-expression/block e)))
    (or (new-dbg-form/block dbg-info)
	(and (new-dbg-continuation? dbg-info)
	     (or (expression-block (new-dbg-continuation/inner dbg-info))
		 (expression-block (new-dbg-continuation/outer dbg-info))))))
  (cond ((code-rewrite/original-form/previous form)
         => (lambda (dbg-info)
              (let* ((block     (find-block dbg-info))
                     (block*
		      (dbg-red/reconstruct-block block env
						 parent-path parent-env))
                     (dbg-info* (new-dbg-form/new-block dbg-info block*)))
		(code-rewrite/remember*! form dbg-info*)
		)))))

(define (dbg-red/reconstruct-block block env closure-parent-path parent-env)
  ;; Copy entire environment model BLOCK structure whilst reconstructing
  ;; variable expressions from actual environment ENV.
  (define (->path item)
    (let ((path   (dbg-red/reconstruct-path item *dbg-graph* env)))
      ;;(pp `(,item ,path))
      #|(if path
	  (begin
	    (if (equal? path 'unassigned)
		(sample/1 '(dbg-red/paths-unassigned count) 1))
	    (sample/1 '(dbg-red/paths-reconstructed count) 1))
	  (sample/1 '(dbg-red/paths-failed count) 1))|#
      path))

  (define (reconstruct-block block)
    (and block
	 (let* ((parent    (new-dbg-block/parent block))
		(ic-parent-path
		 (and parent
		      (new-dbg-block/parent-path-prefix parent)
		      (->path (new-dbg-block/parent-path-prefix parent))))
		(variables*
		 (vector-map (new-dbg-block/variables block)
		   (lambda (variable)
		     (new-dbg-variable/new-path variable (->path variable))))))
	   ;; Note. It is important that the above ->PATH calls happen before
	   ;; the call to DBG-RED/RECONSTRUCT-BLOCK below.
	   (define (make parent* parent-path*)
	     (new-dbg-block/%make
	      (new-dbg-block/type block)
	      parent*
	      parent-path*
	      variables*
	      (new-dbg-block/procedure block)))
	   (cond (ic-parent-path
		  (make 'IC ic-parent-path))
		 ((and parent closure-parent-path)
		  (make (dbg-red/reconstruct-block parent parent-env #F #F)
			closure-parent-path))
		 ((and parent (eq? (new-dbg-block/type parent) 'FIRST-CLASS))
		  (make 'IC 'TOP-LEVEL-ENVIRONMENT))
		 (else
		  (make (reconstruct-block parent) #F))))))
  
  (and block
       (begin
	 (dbg-red/env/mark-available-subgraph! env)
	 (reconstruct-block block))))

(define-structure
    (dbg-red/node
     (type vector)
     (named)
     (conc-name dbg-red/node/)
     (constructor dbg-red/node/make (name))
     (print-procedure
      (standard-unparser-method 'DBG-RED/NODE
	(lambda (node port)
	  (write-char #\Space port)
	  (display (dbg-red/node/name node) port)))))
  (name #F read-only true)		; e.g. dbg-variable, symbol, or scode
  (mark #F)
  (cache #F)
  (available-mark #F)
  (available-count 0)
  (definitions '#(0) read-only false)	; n-15 -> n-15-43
					; n-15 -> cell-ref(n-15-cell)
  (static-definitions '())
  ;;(indirect-definitions '() read-only false)
  (references  '#(0) read-only false)	; accessor(accessor(n-15))
  )
     
(define-structure
    (dbg-red/edge
     (type vector)
     (named)
     (constructor dbg-red/edge/make (expr from index))
     (conc-name dbg-red/edge/)
     (print-procedure
      (standard-unparser-method 'DBG-RED/EDGE
	(lambda (edge port)
	  (write-char #\Space port)
	  (display (dbg-red/edge/index edge) port)
	  (write-char #\Space port)
	  (display (dbg-red/node/name (dbg-red/edge/from edge)) port)
	  (write-string " " port)
	  (display (dbg-red/edge/expr edge) port)))))
  expr					;
  from					; a node
  index					; position in FROM's definitions
  )

(define-structure
    (dbg-red/graph
     (conc-name dbg-red/graph/)
     (constructor dbg-red/graph/make))
  table					; maps names to nodes
  ;; a list of nodes which have scode expressions as names:
  expressions
  )

(define (dbg-red/vector-add! v item)
  (let ((count (vector-ref v 0))
	(len   (vector-length v)))
    (let ((v* (if (= count (- len 1))
		  (vector-grow v (fix:+ (fix:quotient (fix:* len 4) 3) 1))
		  v))
	  (count* (fix:+ count 1)))
      (vector-set! v* count* item)
      (vector-set! v* 0 count*)
      v*)))

(define (dbg-red/node/add-edge! node expr)
  (let* ((defs (dbg-red/node/definitions node))
	 (edge (dbg-red/edge/make expr node (+ (vector-ref defs 0) 1))))
    (set-dbg-red/node/definitions! node (dbg-red/vector-add! defs edge))
    edge))

(define (dbg-rewrites->graph infos)
  (let* ((table  (make-monotonic-strong-eq-hash-table))
	 (expressions '())
	 (static-edges '()))

    (define (find-node key)
      (or (monotonic-strong-eq-hash-table/get table key #F)
	  (let ((node (dbg-red/node/make key)))
	    (monotonic-strong-eq-hash-table/put! table key node)
	    node)))

    (define (add-references! expr edge)
      (define (add-reference! key)
	(let* ((node* (find-node key))
	       (refs  (dbg-red/node/references node*)))
	  (set-dbg-red/node/references! node* (dbg-red/vector-add! refs edge))))

      (let walk ((expr expr))
	(cond ((symbol? expr) (add-reference! expr))
	      ((LOOKUP/? expr) (add-reference! (lookup/name expr)))
	      ((QUOTE/? expr)  unspecific)
	      ((dbg/stack-closure-ref? expr)
	       (walk (vector-ref expr 1)))
	      ((dbg/heap-closure-ref? expr)
	       (walk (vector-ref expr 1)))
	      ((CALL/? expr)
	       (for-each walk (call/operands expr)))
	      ((not (pair? expr)) unspecific)
	      (else ;;(pp expr)
		    unspecific))))

    (for-each
	(lambda (info)
	  (let ((key  (vector-ref info 0))
		(expr (vector-ref info 1)))
	    (let* ((node  (find-node key))
		   (edge  (dbg-red/node/add-edge! node expr)))
	      (if (QUOTE/? expr)
		  (set! static-edges (cons edge static-edges)))
	      (add-references! expr edge)
	      (if (and (not (scode-constant? key))
		       (not (%record? key)))
		  (set! expressions (cons node expressions))))))
      (cdr infos))
    (for-each dbg-red/edge/statically-available! static-edges)
    #|(if compiler:enable-statistics?
	(hash-table/for-each table
	  (lambda (key entry)
	    key
	    (sample/1 '(DBG-RED/OUT-DEGREE HISTOGRAM) 
		      (vector-ref (dbg-red/node/definitions entry) 0))
	    (sample/1 '(DBG-RED/IN-DEGREE HISTOGRAM)
		      (vector-ref (dbg-red/node/references entry) 0))
	    (sample/1 '(DBG-RED/STATIC-OUT-DEGREE HISTOGRAM)
		      (length (dbg-red/node/static-definitions entry))))))|#
    (dbg-red/graph/make table expressions)))

(define (dbg-red/edge/statically-available! edge)
  ;; Mark node as statically available and propogate that information
  (let* ((node  (dbg-red/edge/from  edge))
	 (defs  (dbg-red/node/static-definitions node)))
    (if (not (memq edge defs))
	(let ((refs (dbg-red/node/references node)))
	  (set-dbg-red/node/static-definitions! node (cons edge defs))
	  (do ((i 1 (+ i 1)))
	      ((> i (vector-ref refs 0)))
	    ;; We should really do this when all subexpressions are static,
	    ;; not just any subexpression.
	    (dbg-red/edge/statically-available! (vector-ref refs i)))))))

(define dbg-red/current-available-mark #F)

(define (dbg-red/edge/available! edge)
  ;; Move available edge closer to front
  (let ((node  (dbg-red/edge/from  edge)))
    (dbg-red/node/available! node)
    (let ((available-count (dbg-red/node/available-count node))
	  (index (dbg-red/edge/index edge))
	  (defs  (dbg-red/node/definitions node)))
      (if (not (eq? (vector-ref defs index) edge))
	  (internal-error "Edge not at it's index" edge))
      (if (> index available-count)	;not already available
	  ;; exchange with non-available edge
	  (let* ((available-count* (+ available-count 1))
		 (other-edge (vector-ref defs available-count*)))
	    (set-dbg-red/edge/index! other-edge index)
	    (set-dbg-red/edge/index! edge available-count*)
	    (vector-set! defs index other-edge)
	    (vector-set! defs available-count* edge)
	    (set-dbg-red/node/available-count! node available-count*))))))

(define (dbg-red/node/available! node)
  (if (not (eq? (dbg-red/node/available-mark node)
		dbg-red/current-available-mark))
      (let* ((uses  (dbg-red/node/references node))
	     (count (vector-ref uses 0)))
	(set-dbg-red/node/available-mark! node dbg-red/current-available-mark)
	(set-dbg-red/node/available-count! node 0)
	(let loop ((i 1))
	  (if (<= i count)
	      (let ((edge (vector-ref uses i)))
		(dbg-red/edge/available! edge)
		(loop (+ i 1))))))))


(define (dbg-red/find-node name)
  (monotonic-strong-eq-hash-table/get
   (dbg-red/graph/table *dbg-graph*) name #F))

(define (dbg-red/env/mark-available-subgraph! env)
  (define (available! name)
    (let ((node (dbg-red/find-node name)))
      (if node
	  (dbg-red/node/available! node))))

  (set! dbg-red/current-available-mark (list 'available))
  
  (available! (dbg-reduce/env/frame-name env))
  (for-each (lambda (name.path)
	      (available! (car name.path)))
    (dbg-reduce/env/parameters env)))

(define dbg-red/reconstruct-path
  (lambda (item graph env)

    (define (reconstruct-name item)
      (cond ((dbg-reduce/env/lookup env item)
	     => (lambda (offset-or-name)
		  (cond ((number? offset-or-name)
			 (list (dbgred/INTERRUPT-FRAME offset-or-name)))
			((eq? offset-or-name dbg-red/start-from-closure)
			 '())
			(else
			 (internal-error "CC-entries done statically")
			 `((CC-ENTRY . ,offset-or-name))))))
	    ((monotonic-strong-eq-hash-table/get (dbg-red/graph/table graph)
						 item #F)
	     => reconstruct-node)
	    (else #F)))

    (define (reconstruct-node node)
      (let ((edges (dbg-red/node/definitions node))
	    (limit (dbg-red/node/available-count node)))
	(define (dynamic-path)
	  (let loop ((i 1))
	    (and (<= i limit)
		 (or (reconstruct-edge (vector-ref edges i))
		     (loop (+ i 1))))))
	(define (static-path)
	  (let loop ((edges (dbg-red/node/static-definitions node)))
	    (if (null? edges)
		#F
		(or (reconstruct-edge (car edges))
		    (loop (cdr edges))))))
	(if (eq? (dbg-red/node/mark node) env)
	    (if (eq? (dbg-red/node/cache node) 'PENDING)
		#F
		(dbg-red/node/cache node))
	    (begin
	      (set-dbg-red/node/mark!  node env)
	      (set-dbg-red/node/cache! node 'PENDING)
	      (let ((path
		     (if (eq? (dbg-red/node/available-mark node)
			      dbg-red/current-available-mark)
			 (or (dynamic-path) (static-path))
			 (static-path))))
		(set-dbg-red/node/cache! node path)
		path)))))

    (define (reconstruct-edge edge)
      (reconstruct-expression (dbg-red/edge/expr edge)))

    (define (reconstruct-expression expr)
      (cond ((QUOTE/? expr)
	     (if (eq? (quote/text expr) %unassigned)
		 '(UNASSIGNED)
		 `((INTEGRATED . ,(quote/text expr)))))
	    ((LOOKUP/? expr) (reconstruct-name (lookup/name expr)))
	    ((symbol? expr)  (reconstruct-name expr))
	    ((dbg/stack-closure-ref? expr)
	     (let ((frame   (vector-ref expr 1))
		   (offset
		    (vector-index (vector-ref expr 2) (vector-ref expr 3))))
	       (and (eq? frame (dbg-reduce/env/frame-name env))
		    (list (dbgred/STACK offset)))))
	    ((dbg/heap-closure-ref? expr)
	     (let ((closure-path (reconstruct-expression (vector-ref expr 1)))
		   (offset
		    (vector-index (vector-ref expr 2) (vector-ref expr 3))))
	       (and closure-path
		    (cons (dbgred/CLOSURE (+ offset
					     (rtlgen/closure-first-offset)))
			  closure-path))))
	    ((CALL/%multicell-ref? expr)
	     (let ((cell-path
		    (reconstruct-expression (call/%multicell-ref/cell expr)))
		   (layout  (call/%multicell-ref/layout expr))
		   (name    (call/%multicell-ref/name expr)))
	       (and cell-path
		    (QUOTE/? layout)
		    (QUOTE/? name)
		    (cons (dbgred/CELL 
			   (vector-index (quote/text layout)
					 (quote/text name)))
			  cell-path))))
	    ((CALL/%flo:multicell-ref? expr)
	     (let ((cell-path
		    (reconstruct-expression (call/%flo:multicell-ref/cell expr)))
		   (layout  (call/%flo:multicell-ref/layout expr))
		   (name    (call/%flo:multicell-ref/name expr)))
	       (and cell-path
		    (QUOTE/? layout)
		    (QUOTE/? name)
		    (cons (dbgred/FLONUM-CELL 
			   (vector-index (quote/text layout)
					 (quote/text name)))
			  cell-path))))
	    ((or (CALL/%stack-closure-ref? expr)
		 (CALL/%heap-closure-ref? expr))
	     (internal-error "DBG expression should have been compressed" expr))
	    ((and (pair? expr)
		  (eq? (car expr) 'CC-ENTRY))
	     (list expr))
	    ((and (CALL/? expr) (QUOTE/? (call/operator expr))
		  (dbg-reduce/equivalent-primitive
		   (quote/text (call/operator expr))))
	     => (lambda (primitive)
		  (let ((operands (call/operands expr)))
		    (define (mention primitive)
		      (sample/1 '(dbgred/primitives histogram) (list primitive))
		      #T)
		    (case (primitive-procedure-arity primitive)
		      ((1) 
		       (let ((path1 (reconstruct-expression (first operands))))
			 (and path1
			      (mention primitive)
			      (cons primitive path1))))
		      ((2)
		       (let ((path1 (reconstruct-expression (first operands)))
			     (path2 (reconstruct-expression (cadr operands))))
			 (and path1
			      path2
			      (mention primitive)
			      `(,primitive ,@path2 ROOT ,@path1))))
		      (else #F)))))
	    ((and (CALL/? expr)
		  (equal? (call/operator expr) '(QUOTE UNCOERCE)))
	     (let ((procedure-path
		    (reconstruct-expression (first (call/operands expr)))))
	       (and procedure-path
		    `(UNCOERCE . ,procedure-path))))
	    (else #F)))
    
    (let ((reversed-path (reconstruct-name item)))
      (cond ((null? reversed-path)
	     ;; A null path means that the root IS the object.  This happens
	     ;; for a self-reference in a letrec bound closure.
	     '#())
	    ((not (pair? reversed-path))
	     '#F)
	    ((null? (cdr reversed-path)) ;just one action?
	     (car reversed-path))
	    (else
	     (list->vector (reverse reversed-path)))))))

(define dbg-red/start-from-closure "dbg-red/start-from-closure")

(define dbg-red/closure-invocation-parent-path
  '(INTERRUPT-FRAME . 4))

(define (dbg-reduce/indexed-path tag)
  (let ((vec '#()))
    (lambda (n)
      (let ((len (vector-length vec)))
	(if (< n len)
	    (vector-ref vec n)
	    (begin
	      (set! vec (vector-append vec (make-initialized-vector (+ n 5)
					     (lambda (i)
					       (cons tag (+ i len))))))
	      (vector-ref vec n)))))))

(define dbgred/INTERRUPT-FRAME (dbg-reduce/indexed-path 'INTERRUPT-FRAME))
(define dbgred/STACK           (dbg-reduce/indexed-path 'STACK))
(define dbgred/CLOSURE         (dbg-reduce/indexed-path 'CLOSURE))
(define dbgred/CELL            (dbg-reduce/indexed-path 'CELL))
(define dbgred/FLONUM-CELL     (dbg-reduce/indexed-path 'FLONUM-CELL))

(define dbg-reduce/equivalent-operators (make-monotonic-strong-eq-hash-table))

(define (dbg-reduce/equivalent-primitive operator)
  (monotonic-strong-eq-hash-table/get
   dbg-reduce/equivalent-operators operator #F))

(let ()
  (define (->prim op)
    (if (symbol? op) (make-primitive-procedure op) op))
  (define (allow . ops)
    (for-each (lambda (op)
		(let ((op (->prim op)))
		  (monotonic-strong-eq-hash-table/put!
		   dbg-reduce/equivalent-operators op op)))
      ops))
  (define (replace op op2)
    (monotonic-strong-eq-hash-table/put!
     dbg-reduce/equivalent-operators op (->prim op2)))
  (replace %vector-length vector-length)

  (allow '%record-length 'ascii->char 'bit-string->unsigned-integer
         'bit-string-length 'bit-string?  'cell?  'char->ascii 'char->integer
         'char-ascii?  'char-bits 'char-code 'char-downcase 'char-upcase
         'compiled-code-address->block 'compiled-code-address->offset 'eq?
         'integer?  'not 'multiply-fixnum 'plus-fixnum 'minus-fixnum
         'minus-one-plus-fixnum 'one-plus-fixnum 'less-than-fixnum?
         'equal-fixnum?  'greater-than-fixnum?  'fixnum-and 'fixnum-andc
         'divide-fixnum 'fixnum?  'gcd-fixnum 'fixnum-lsh 'negative-fixnum?
         'fixnum-not 'fixnum-or 'positive-fixnum?  'fixnum-quotient
         'fixnum-remainder 'fixnum-xor 'zero-fixnum?  'fixnum?  'flonum-multiply
         'flonum-add 'flonum-subtract 'flonum-divide 'flonum-less?
         'flonum-equal?  'flonum-greater?  'flonum-abs 'flonum-acos 'flonum-asin
         'flonum-atan 'flonum-atan2 'flonum-ceiling 'flonum-ceiling->exact
         'flonum-cos 'flonum-exp 'flonum-expt 'flonum?  'flonum-floor
         'flonum-floor->exact 'flonum-log 'flonum-negate 'flonum-negative?
         'flonum-positive?  'flonum-round 'flonum-round->exact 'flonum-sin
         'flonum-sqrt 'flonum-tan 'flonum-truncate 'flonum-truncate->exact
         'floating-vector-length 'flonum-zero?  'index-fixnum?
         'integer-multiply 'integer-add 'integer-subtract 'integer-subtract-1
         'integer-add-1 'integer-less?  'integer-equal?  'integer-greater?
         'integer-divide 'integer?  'integer-negate 'integer-negative?
         'integer-positive?  'integer-quotient 'integer-remainder 'integer-zero?
         'integer->char 'make-non-pointer-object 'not 'constant?  'object-datum
         'object-gc-type 'object-type 'object-type?  'pair?
         'primitive-procedure-arity 'null?  'string-hash 'string-hash-mod
         'string-maximum-length 'string?  'substring-ci=?  'substring-downcase!
         'system-pair?  'system-vector-size 'system-vector?  'vector-length ))

;; tracking of representation and naming changes for generating debugging
;; info.
	 
;; Compact representation of closure reference expressions

(define (dbg/make-closure-ref op closure elements-vector name)
  (vector op closure elements-vector name))

(define (dbg/stack-closure-ref? thing)
  (and (vector? thing)
       (eq? (vector-ref thing 0) %stack-closure-ref)))

(define (dbg/heap-closure-ref? thing)
  (and (vector? thing)
       (eq? (vector-ref thing 0) %heap-closure-ref)))

(define (dbg-red/compress-expression form)
  (define (compress-closure-ref op)
    ;; (CALL '%*-closure-ref '#F <closure> <index> 'name)
    (let* ((closure (dbg-red/compress-expression (fourth form)))
	   (layout  (quote/text (fifth form)))
	   (name    (quote/text (sixth form))))
      (vector op closure layout name)))

  (define (compress-ordinary-call form)
    (let ((exprs* (map dbg-red/compress-expression (call/operands form))))
      (if (there-exists? exprs* false?)
	  #F
	  `(CALL ,(call/operator form) ,(call/continuation form) ,@exprs*))))

  (cond ((QUOTE/? form) form)
	((symbol? form) form)
	((LOOKUP/? form) (lookup/name form))
	((and (CALL/? form)
	      (QUOTE/? (call/operator form)))
	 (let ((op (quote/text (call/operator form))))
	   (cond ((or (eq? op %stack-closure-ref)
		      (eq? op %heap-closure-ref))
		  (compress-closure-ref op))
		 ((monotonic-strong-eq-hash-table/get *dbg-forbidden-operators* op #F) #F)
		 (else
		  (compress-ordinary-call form)))))
	(else #F)))

(define *dbg-rewrites*)

(define (dbg-info/make-rewrites)
  (cons 'HEAD '()))

(define (dbg-info/remember from to*)
  (define (good to)
    (set-cdr! *dbg-rewrites*
	      (cons (vector from to) (cdr *dbg-rewrites*))))
  (cond ((continuation-variable? from))
	((dbg/stack-closure-ref? to*) (good to*))
	((dbg/heap-closure-ref? to*) (good to*))
	(else
	 (let ((to (dbg-red/compress-expression to*)))
	   (cond ((eq? from to))
		 ((false? to)
		  #|(fluid-let ((*unparser-list-breadth-limit* 7)
			      (*unparser-list-depth-limit* 6))
		    (pp `(reject ,from ,to*)))|#)
		 (else (good to)))))))


(define *dbg-forbidden-operators* (make-monotonic-strong-eq-hash-table))

(define (dbg-info/for-all-dbg-expressions! procedure)
  (for-each (lambda (from+to)
	      (procedure (vector-ref from+to 1)))
    (cdr *dbg-rewrites*)))

(let ((forbid
       (lambda (operator)
	 (monotonic-strong-eq-hash-table/put! *dbg-forbidden-operators*
					      operator #T))))
  (forbid %make-heap-closure)
  (forbid CONS)
  (forbid %cons)
  (forbid %vector))

#|
Path expressions
(INTEGRATED . value)			;compile time constant
(UNASSIGNED)				;integrated unassigned reference trap
(CONSTANT-BLOCK . offset)		;integrated sharded pointer if possible
(INTERRUPT-FRAME . value)		;index into inerrupt frame
(CLOSURE . offset)			;index into compiled closure
(STACK . offset-from-base)		;index into continuation frame
(CELL . value)				;index into cell or multi-cell
(CC-ENTRY . offset)			;entry in current cc-block

(UNCOERCE)				;a result of coerce-to-compiled-procedure

(<primitive>)				;apply primitive to current place
(<primitive> . arg2)			;apply primitive to current place &arg2

;;These are used in parent environment path expressions
(TOP-LEVEL-ENVIRONMENT)			;compiled code block's environment
|#

#|
95/08/03:
((27.305 77. "/sw/adams/hack/dbgred2.inf")
 (21.277 60. (primitive "GARBAGE-COLLECT"))
 (19.149 54. "/scheme/8.0/700/lib/options/hashtb.inf")
 (9.574 27. other)
 (7.801 22. "/scheme/8.0/700/runtime/list.inf")
 (3.901 11. "/scheme/8.0/700/compiler/midend/fakeprim.inf")
|#