#| -*-Scheme-*-

$Id: dbgred.scm,v 1.8 1995/07/21 14:28:53 adams Exp $

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
  (let* ((static-names
	  (map first
	       (list-transform-positive bindings
		 (lambda (binding)
		   (form/static? (cadr binding))))))
	 (env*
	  (dbg-reduce/env/extend-static env static-names)))
    ;;(dbg-reduce/reduce form env* #F #F)
    (dbg-reduce/expr env* body)))

(define-dbg-reducer LETREC (bindings body)
  ;; add static bindings
  (let ((env* (dbg-reduce/env/extend-static env (map car bindings))))
    (for-each (lambda (binding)
		(dbg-reduce/expr env* (cadr binding)))
	      bindings)
    (dbg-reduce/expr env* body)))

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
      path))

  (define (reconstruct-block block)
    (and block
	 (let* ((parent    (new-dbg-block/parent block))
		(ic-parent-path
		 (and parent
		      (new-dbg-block/parent-path-prefix parent)
		      (->path (new-dbg-block/parent-path-prefix parent)))))
	   (define (make parent* parent-path*)
	     (new-dbg-block/%make
	      (new-dbg-block/type block)
	      parent*
	      parent-path*
	      (vector-map (new-dbg-block/variables block)
		(lambda (variable)
		  (new-dbg-variable/new-path variable (->path variable))))
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
       (reconstruct-block block)))



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
			 `((CC-ENTRY . ,offset-or-name))))))
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
      (cond ((QUOTE/? expr)
	     (if (eq? (quote/text expr) %unassigned)
		 '(UNASSIGNED)
		 `((INTEGRATED . ,(quote/text expr)))))
	    ((LOOKUP/? expr) (reconstruct-name (lookup/name expr)))
	    ((symbol? expr)  (reconstruct-name expr))
	    ((CALL/%stack-closure-ref? expr)
	     (let ((frame   (call/%stack-closure-ref/closure expr))
		   (offset  (call/%stack-closure-ref/offset expr)))
	       (and (LOOKUP/? frame)
		    (QUOTE/? offset)
		    (eq? (lookup/name frame) (dbg-reduce/env/frame-name env))
		    (list (dbgred/STACK (quote/text offset))))))
	    ((CALL/%heap-closure-ref? expr)
	     (let ((closure (call/%heap-closure-ref/closure expr))
		   (offset  (call/%heap-closure-ref/offset expr)))
	       (let ((closure-path (reconstruct-expression closure)))
		 (and closure-path
		      (QUOTE/? offset)
		      (cons (dbgred/CLOSURE (+ (quote/text offset)
					       (rtlgen/closure-first-offset)))
			    closure-path)))))
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

(define dbg-reduce/equivalent-operators (make-eq-hash-table))

(define (dbg-reduce/equivalent-primitive operator)
  (hash-table/get dbg-reduce/equivalent-operators operator #F))

(let ()
  (define (->prim op)
    (if (symbol? op) (make-primitive-procedure op) op))
  (define (allow . ops)
    (for-each (lambda (op)
		(let ((op (->prim op)))
		  (hash-table/put! dbg-reduce/equivalent-operators op op)))
      ops))
  (define (replace op op2)
    (hash-table/put! dbg-reduce/equivalent-operators op (->prim op2)))
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
	 
(define-structure
    (dbg-use
     (conc-name dbg-use/)
     (constructor dbg-use/make (name))
     (print-procedure
      (standard-unparser-method 'DBG-USE
	(lambda (u port)
	  (write-char #\Space port)
	  (display (dbg-use/name u) port)))))
  (name #F read-only true)		; e.g. n-15
  (definitions '() read-only false)	; n-15 -> n-15-43
					; n-15 -> cell-ref(n-15-cell)
  ;;(indirect-definitions '() read-only false)
  (expressions '() read-only false)	; accessor(accessor(n-15))
  )
	 
(define *dbg-rewrites*)

(define (dbg-info/make-rewrites)
  (cons 'HEAD '()))

(define (dbg-info/remember from to)
  (define (unconstructable? form)
    (and (CALL/? form)
	 (QUOTE/? (call/operator form))
	 (hash-table/get *dbg-unconstructable-operators*
			 (quote/text (call/operator form)) #F)))
  (let ((to (if (LOOKUP/? to) (lookup/name to) to)))
    (if (and (not (unconstructable? to))
	     (not (continuation-variable? from))
	     (not (eq? from to)))
	(set-cdr! *dbg-rewrites*
		  (cons (vector from to) (cdr *dbg-rewrites*))))))

(define *dbg-unconstructable-operators* (make-eq-hash-table))

(define (dbg-info/for-all-dbg-expressions! procedure)
  (for-each (lambda (from+to)
	      (procedure (vector-ref from+to 1)))
    (cdr *dbg-rewrites*)))

(let ((forbid
       (lambda (operator)
	 (hash-table/put! *dbg-unconstructable-operators* operator #T))))
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