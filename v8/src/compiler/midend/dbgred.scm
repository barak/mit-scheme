#| -*-Scheme-*-

$Id: dbgred.scm,v 1.7 1995/07/04 17:40:53 adams Exp $

Copyright (c) 1994-1995 Massachusetts Institute of Technology

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

;;;; Reduce debugging expressions to canonical form
;;; package: (compiler midend)

(declare (usual-integrations))

(define *dbgt*)
(define *dbg-graph*)

(define (dbg-reduce/top-level program)
  (set! *dbgt* (make-eq-hash-table))
  (fluid-let ((*dbg-graph* (dbg-rewrites->graph *dbg-rewrites*)))
    (dbg-reduce/expr (dbg-reduce/initial-env)
		     (if (LAMBDA/? program) ; should be the case
			 (lambda/body program)
			 program)))
  (sample/1 '(dbg-red/cache-gets histogram) dbg-red/cache-gets)
  (sample/1 '(dbg-red/cache-sets histogram) dbg-red/cache-sets)
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
  ;;(dbg-reduce/reduce form env)
  unspecific)

(define-dbg-reducer QUOTE (object)
  object env				; unused
  ;;(dbg-reduce/reduce form env)
  unspecific)

(define-dbg-reducer LAMBDA (lambda-list body)
  ;; redefine dynamic frame
  ;;   Several issues need to be addressed: (1) when we look at the
  ;;   parameters (both register and stack) they have been rearranged
  ;;   to look like an interpreter call (2) continuations will not
  ;;   have rearranged their stack-saved values but will have
  ;;   rearranged their multiple arguments (if any).
  ;; . We must generate stack references for stack arguments as closure
  ;;   conversion did not post stack-ref rewrites for them.
  ;; . 
  ;;(match body
  ;;  ((LET ((?frame-name
  ;;           (CALL ',%fetch-stack-closure _ '?frame-vector)) . _)
  ;;     _) =>
  ;;   deal)
  ;;  (else no-deal))
  (let* ((frame-vector
	  (and (LET/? body)
	       (pair? (let/bindings body))
	       (CALL/%fetch-stack-closure?
		(second (first (let/bindings body))))
	       (QUOTE/text 
		(CALL/%fetch-stack-closure/vector
		 (second (first (let/bindings body)))))))
	 (frame-name (and frame-vector (first (first (let/bindings body))))))
    (let* ((arg-names  (cdr (lambda-list->names lambda-list)))
	   (arg-slots  (length arg-names))
	   (arg-regs   (vector-length *rtlgen/argument-registers*))
	   (interrupt-stack-frame-length
	    (+ (min arg-regs arg-slots)
	       (if frame-vector (vector-length frame-vector) 0)
	       6))
	   (arg0-offset
	    (+ (min arg-regs arg-slots)
	       4))
	   (arg0-offset/stack
	    (+ arg0-offset arg-slots 1))
	   (alist  (map (lambda (name i)
			  (cons name 
				(if (< i arg-regs)
				    (- arg0-offset i)
				    (- arg0-offset/stack i))))
			arg-names
			(iota arg-slots)))
	   (slot-map -1+))
      (let ((env*  (dbg-reduce/env/new-frame env alist frame-name slot-map)))
	(dbg-reduce/reduce form env*)
	(dbg-reduce/expr env* body)))))


(define-dbg-reducer LET (bindings body)
  (for-each (lambda (binding)
	      (dbg-reduce/expr env (cadr binding)))
	    bindings)
  (let* ((static-names
	  (map first
	       (list-transform-positive bindings
		 (lambda (binding)
		   (form/static? (cadr binding))))))
	 (env*
	  (dbg-reduce/env/extend-static env static-names)))
    (dbg-reduce/reduce form env)
    (dbg-reduce/expr env* body)))

(define-dbg-reducer LETREC (bindings body)
  ;; add static bindings
  (let ((env* (dbg-reduce/env/extend-static env (map car bindings))))
    (for-each (lambda (binding)
		(dbg-reduce/expr env* (cadr binding)))
	      bindings)
    ;;(dbg-reduce/reduce form env*)
    (dbg-reduce/expr env* body)))

(define-dbg-reducer IF (pred conseq alt)
  ;;(dbg-reduce/reduce form env)
  (dbg-reduce/expr env pred)
  (dbg-reduce/expr env conseq)
  (dbg-reduce/expr env alt))

(define-dbg-reducer DECLARE (#!rest anything)
  env anything				; unused
  ;;(dbg-reduce/reduce form env)
  unspecific)

(define-dbg-reducer BEGIN (#!rest actions)
  ;;(dbg-reduce/reduce form env)
  (dbg-reduce/expr* env actions))

(define-dbg-reducer CALL (rator cont #!rest rands)
  ;;(dbg-reduce/reduce form env)
  (dbg-reduce/expr env rator)
  (dbg-reduce/expr env cont)
  (dbg-reduce/expr* env rands))

(define (dbg-reduce/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (dbg-reduce/quote env expr))
    ((LOOKUP)   (dbg-reduce/lookup env expr))
    ((LAMBDA)   (dbg-reduce/lambda env expr))
    ((LET)      (dbg-reduce/let env expr))
    ((DECLARE)  (dbg-reduce/declare env expr))
    ((CALL)     (dbg-reduce/call env expr))
    ((BEGIN)    (dbg-reduce/begin env expr))
    ((IF)       (dbg-reduce/if env expr))
    ((LETREC)   (dbg-reduce/letrec env expr))
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
  ;; Dynamic objects (in current procedure parameters).  A list of (name
  ;; . stack-offset) pairs
  parameters
  frame-name				; #F or a symbol
  ;; procedure mapping %stack-closure-ref offsets to actual offsets
  frame-offset-map)

(define (dbg-reduce/initial-env)
  (dbg-reduce/env/%make '() '() #F #F))

(define (dbg-reduce/env/new-frame env parameters frame-name frame-offset-map)
  (dbg-reduce/env/%make (dbg-reduce/env/static env)
			parameters
			frame-name
			frame-offset-map))

(define (dbg-reduce/env/extend-static env static*)
  (dbg-reduce/env/%make (append static* (dbg-reduce/env/static env))
			(dbg-reduce/env/parameters env)
			(dbg-reduce/env/frame-name env)
			(dbg-reduce/env/frame-offset-map env)))

(define (dbg-reduce/env/lookup env name)
  ;; -> #F, stack offset, or ??
  (cond ((assq name (dbg-reduce/env/parameters env))	 => cdr)
	((memq name (dbg-reduce/env/static env))    name)
	(else #F)))

(define (dbg-reduce/reduce form env)
  (cond ((code-rewrite/original-form/previous form)
         => (lambda (dbg-info)
              (let* ((block     (new-dbg-form/block dbg-info))
                     (block*    (dbg-red/reconstruct-block block env))
                     (dbg-info* (new-dbg-form/new-block dbg-info block*)))
		(hash-table/put! *dbgt* form (vector env dbg-info*))))))
  unspecific)

(define (dbg-red/reconstruct-block block env)
  ;; Copy entire environment model BLOCK structure whilst reconstructing
  ;; variable expressions from actual environment ENV.
  (define (->path item)
    (let ((path   (dbg-red/reconstruct-path item *dbg-graph* env)))
      (pp `(,item ,path))
      path))

  (define (reconstruct-block block)
    (and block
	 (let* ((parent    (new-dbg-block/parent block))
		(parent-path
		 (and parent
		      (new-dbg-block/parent-path-prefix parent)
		      (->path (new-dbg-block/parent-path-prefix parent)))))
	   (define (make parent* parent-path*)
	     (new-dbg-block/%make
	      (new-dbg-block/type block)
	      parent*
	      parent-path*
	      (vector-map (new-dbg-block/variables block)
		(lambda (var)
		  (new-dbg-variable/new-expression var (->path var))))))
	   (cond (parent-path
		  (make 'IC parent-path))
		 ((and parent (eq? (new-dbg-block/type parent) 'FIRST-CLASS))
		  (make 'IC '((TOP-LEVEL-ENVIRONMENT))))
		 (else
		  (make (reconstruct-block parent) #F))))))
  
  (and block
       (begin
	 (pp `(reconstruct-block ,block ,env ,*dbg-graph*))
	 (let ((block* (reconstruct-block block)))
	   (pp `(reconstruct-block ,block => ,block*))
	   block*))))



(define-structure
    (dbg-red/edge
     (type vector)
     (constructor dbg-red/edge/make (expr))
     (conc-name dbg-red/edge/))
  (mark #F)
  (cache #F)
  expr)

(define-structure
    (dbg-red/graph
     (conc-name dbg-red/graph/)
     (constructor dbg-red/graph/make))
  table					; maps names to edge `list' vectors
  expressions				; a list of scode expressions in names
  )

(define (dbg-rewrites->graph infos)
  (let* ((table  (make-eq-hash-table))
	 (expressions '()))
    (for-each
	(lambda (info)
	  (let ((key  (vector-ref info 0))
		(expr (vector-ref info 1)))
	    (let ((entry (hash-table/get table key #F)))
	      (hash-table/put!
	       table key
	       (cond ((not entry)
		      (vector (dbg-red/edge/make expr)))
		     (else
		      (make-initialized-vector (1+ (vector-length entry))
			(lambda (i)
			  (if (< i (vector-length entry))
			      (vector-ref entry i)
			      (dbg-red/edge/make expr)))))))
	      (if (and (not (scode-constant? key))
		       (not (%record? key))
		       (not entry))
		  (set! expressions (cons key expressions))))))
      (cdr infos))
    (dbg-red/graph/make table expressions)))

(define dbg-red/cache-sets 0)
(define dbg-red/cache-gets 0)

(define (dbg-red/reconstruct-path item graph env)
  (define (reconstruct-name item)
    (cond ((dbg-reduce/env/lookup env item)
	   => (lambda (offset-or-name)
		(list
		 (cons (if (number? offset-or-name)
			   'INTERRUPT-FRAME
			   'CC-ENTRY)
		       offset-or-name))))
	  ((hash-table/get (dbg-red/graph/table graph) item #F)
	   => (lambda (edges)
		(let loop ((i  (- (vector-length edges) 1)))
		  (and (>= i 0)
		       (or (reconstruct-edge (vector-ref edges i))
			   (loop (- i 1)))))))
	  (else #F)))

  (define (reconstruct-edge edge)
    (if (eq? (dbg-red/edge/mark edge) env)
	(if (eq? (dbg-red/edge/cache edge) 'PENDING)
	    #F
	    (begin
	      (set! dbg-red/cache-gets (+ 1 dbg-red/cache-gets))
	      (dbg-red/edge/cache edge)))
	(begin
	  (set-dbg-red/edge/mark! edge env)
	  (set-dbg-red/edge/cache! edge 'PENDING)
	  (let ((path (reconstruct-expression (dbg-red/edge/expr edge))))
	    (set-dbg-red/edge/cache! edge path)
	    (set! dbg-red/cache-sets (+ 1 dbg-red/cache-sets))
	    path))))

  (define (reconstruct-expression expr)
    (cond ((QUOTE/? expr) `((INTEGRATED . (quote/text expr))))
	  ((LOOKUP/? expr) (reconstruct-name (lookup/name expr)))
	  ((symbol? expr)  (reconstruct-name expr))
	  ((CALL/%stack-closure-ref? expr)
	   (let ((frame   (call/%stack-closure-ref/closure expr))
		 (offset  (call/%stack-closure-ref/offset expr)))
	     (and (LOOKUP/? frame)
		  (QUOTE/? offset)
		  (eq? (lookup/name frame) (dbg-reduce/env/frame-name env))
		  `((STACK . ,((dbg-reduce/env/frame-offset-map env)
			     (quote/text offset)))))))
	  ((CALL/%heap-closure-ref? expr)
	   (let ((closure (call/%heap-closure-ref/closure expr))
		 (offset  (call/%heap-closure-ref/offset expr)))
	     (let ((closure-path (reconstruct-expression closure)))
	       (and closure-path
		    (QUOTE/? offset)
		    `((CLOSURE . ,(+ (quote/text offset)
				     (rtlgen/closure-first-offset)))
		      . ,closure-path)))))
	  ((CALL/%multicell-ref? expr)
	   (let ((cell-path
		  (reconstruct-expression (call/%multicell-ref/cell expr)))
		 (layout  (call/%multicell-ref/layout expr))
		 (name    (call/%multicell-ref/name expr)))
	     (and cell-path
		  (QUOTE/? layout)
		  (QUOTE/? name)
		  `((CELL
		     . ,(vector-index (quote/text layout) (quote/text name)))
		    . ,cell-path))))
	  (else #F)))
    
  (let ((reversed-path (reconstruct-name item)))
    (and reversed-path
	 (reverse reversed-path))))

#|
Path expressions
(INTEGRATED . value)			;compile time constant
(CONSTANT-BLOCK . offset)		;integrated sharded pointer if possible
(INTERRUPT-FRAME . value)		;index into inerrupt frame
(CLOSURE . offset)			;index into compiled closure
(STACK . offset-from-base)		;index into continuation frame
(CELL . value)				;index into cell or multi-cell
(CC-ENTRY . offset)			;entry in current cc-block
;;These are used in parent environment path expressions
(TOP-LEVEL-ENVIRONMENT)			;compiled code block's environment
|#