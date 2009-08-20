#| -*-Scheme-*-

$Id$

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

;;;; Substitute simple and used-only-once parameters
;;; package: (compiler midend)

(declare (usual-integrations))


(define *simplify/open-code-expression-limit* 'DONT)
;; Maximun `size' of open coded expression

(define (simplify/top-level program)
  (simplify/expr #F program))

(define-macro (define-simplifier keyword bindings . body)
  (let ((proc-name (symbol-append 'SIMPLIFY/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (SIMPLIFY/REMEMBER ,code FORM))))))))

(define-simplifier LOOKUP (env name)
  (let ((ref `(LOOKUP ,name)))
    (simplify/lookup*! env name ref 'ORDINARY)))

(define-simplifier LAMBDA (env lambda-list body)
  `(LAMBDA ,lambda-list
     ,(simplify/expr
       (simplify/env/make env
        (map simplify/binding/make (lambda-list->names lambda-list)))
       body)))

(define-simplifier QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-simplifier DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-simplifier BEGIN (env #!rest actions)
  `(BEGIN ,@(simplify/expr* env actions)))

(define-simplifier IF (env pred conseq alt)
  `(IF ,(simplify/expr env pred)
       ,(simplify/expr env conseq)
       ,(simplify/expr env alt)))

(define (do-simplification env mutually-recursive? bindings body continue)
  ;; BINDINGS is a list of triples: (environment name expression)
  ;; where ENVIRONMENT is either #F or the environment for the lambda
  ;; expression bound to this name
  (define unsafe-cyclic-reference?
    ;; Maps a LAMBDA form to a boolean: was this LAMBDA chosen to break
    ;; cycles?  Things that we do not take into account but we should:
    ;; (1) If for some reason we would not try to substitute the lambda,
    ;; then it already breaks a cycle.  (2) we should put lambdas
    ;; with simple inline-able bodies last so they don't break a cycle
    ;; by accident. (3) the DFS should be rooted in the LETREC's body.
    (if mutually-recursive?
	(let ((table (make-monotonic-strong-eq-hash-table)))
	  (define (insert! triple)
	    (monotonic-strong-eq-hash-table/put!
	     table
	     (second triple)		;name
	     (if (first triple)
		 (simplify/env/free-calls (first triple))
		 '())))
	  (for-each insert! bindings)
	  (make-breaks-cycle?
	   (map second bindings)
	   (lambda (name) (monotonic-strong-eq-hash-table/get table name #F))))
	(lambda (lambda-expr) lambda-expr #F)))

  (simplify/bindings env unsafe-cyclic-reference?
		     (simplify/delete-parameters env bindings
						 unsafe-cyclic-reference?)
		     body continue))

(define-simplifier CALL (env rator cont #!rest rands)
  (define (do-ops rator*)
    `(CALL ,rator*
	   ,(simplify/expr env cont)
	   ,@(simplify/expr* env rands)))

  (cond ((LOOKUP/? rator)
	 (let* ((name   (lookup/name rator))
		(rator* (simplify/remember `(LOOKUP ,name) rator))
		(result (do-ops rator*)))
	   (simplify/lookup*! env name result 'OPERATOR)))
	((LAMBDA/? rator)
	 (guarantee-simple-lambda-list (lambda/formals rator)) ;Miller & Adams
	 (let* ((lambda-list (lambda/formals rator))
		(env0  (simplify/env/make env
			 (map simplify/binding/make lambda-list)))
		(body* (simplify/expr env0 (caddr rator)))
		(bindings* (map (lambda (name value)
				  (simplify/binding&value env name value))
				lambda-list
				(cons cont rands))))
	   (do-simplification env0 #F bindings* body*
	     (lambda (bindings* body*)
	       (simplify/pseudo-letify rator bindings* body*)))))
	(else
	 (do-ops (simplify/expr env rator)))))

(define-simplifier LET (env bindings body)
  (let* ((env0 (simplify/env/make env
		(map (lambda (binding) (simplify/binding/make (car binding)))
		     bindings)))
	 (body* (simplify/expr env0 body))
	 (bindings*
	  (map (lambda (binding)
		  (simplify/binding&value env (car binding) (cadr binding)))
	       bindings)))
    (do-simplification env0 #F bindings* body* simplify/letify)))

(define-simplifier LETREC (env bindings body)
  (let* ((env0 (simplify/env/make env
		(map (lambda (binding) (simplify/binding/make (car binding)))
		     bindings)))
	 (body* (simplify/expr env0 body))
	 (bindings*
	  (map (lambda (binding)
		  (simplify/binding&value env0 (car binding) (cadr binding)))
	       bindings)))
    (do-simplification env0 #T bindings* body* simplify/letrecify)))

(define-simplifier LETREC (env bindings body)
  (let* ((n-bindings (length bindings))
	 (frame (map (lambda (binding) (simplify/binding/make (car binding)))
		     bindings))
	 (env0 (simplify/env/make env frame))
	 (body* (simplify/expr env0 body)))

    (let ((bindings* (make-vector n-bindings))
	  (initial-queue (map cons* frame (iota n-bindings) bindings)))

      (define (finish unused)
	(define (insert! elt)
	  (vector-set! bindings*
		       (second elt)
		       (list false (third elt) (fourth elt))))
	(for-each insert! unused)
	(do-simplification env0 #T (vector->list bindings*)
			   body* simplify/letrecify))

      ;; We scan a queue of bindings to check.  If a binding is referenced, add
      ;; it to the set.  If it is unreferenced, put it in a retry
      ;; list.  The note below: we don't reverse the list as this
      ;; gives us a back-and-forth pattern of scanning which guards
      ;; against pathological (O(n^2)) cases.

      (let loop ((queue initial-queue)	; list (binding,name,expression)
		 (retry '())
		 (found-one? #F))

	(if (null? queue)
	    (if found-one?
		(loop retry '() #F)	; Note: not reversed!
		(finish retry))
	    (let ((head (car queue))
		  (rest (cdr queue)))
	      (if (and (null? (simplify/binding/operator-refs (car head)))
		       (null? (simplify/binding/ordinary-refs (car head))))
		  (loop rest (cons head retry) found-one?)
		  (begin
		    (vector-set! bindings*
				 (second head)
				 (simplify/binding&value env0 (third head) (fourth head)))
		    (loop (cdr queue) retry #T)))))))))


(define (simplify/binding&value env name value)
  (if (not (LAMBDA/? value))
      (list false name (simplify/expr env value))
      (let* ((lambda-list (lambda/formals value))
	     (env1 (simplify/env/make env
		    (map simplify/binding/make
			 (lambda-list->names lambda-list)))))
	(let ((value*
	       `(LAMBDA ,lambda-list
		  ,(simplify/expr env1 (lambda/body value)))))
	  (list env1 name (simplify/remember value* value))))))

(define (simplify/delete-parameters env0 bindings unsafe-cyclic-reference?)
  ;; ENV0 is the current environment frame
  ;; BINDINGS is parallel to that, but is a list of
  ;;   (frame* name expression) triplet lists as returned by
  ;;   simplify/binding&value, where frame* is either #F or the frame
  ;;   for the LAMBDA expression that is bound to this name
  (for-each
      (lambda (bnode triplet)
	(let ((env1  (first triplet))
	      (name  (second triplet))
	      (value (third triplet)))
	  (and env1
	       (null? (simplify/binding/ordinary-refs bnode))
	       (not (null? (simplify/binding/operator-refs bnode)))
	       ;; Don't bother if it will be open coded
	       (not (null? (cdr (simplify/binding/operator-refs bnode))))
	       (not (simplify/open-code? name value unsafe-cyclic-reference?))
	       ;; At this point, env1 and triplet represent a LAMBDA
	       ;; expression to which there are no regular references and
	       ;; which will not be open coded.  We consider altering its
	       ;; formal parameter list.
	       (let ((unrefd
		      (list-transform-positive (simplify/env/bindings env1)
			(lambda (bnode*)
			  (and (null? (simplify/binding/ordinary-refs bnode*))
			       (null? (simplify/binding/operator-refs bnode*))
			       (not (continuation-variable?
				     (simplify/binding/name bnode*))))))))
		 (and (not (null? unrefd))
		      (for-each (lambda (unrefd)
				  (simplify/maybe-delete unrefd
							 bnode
							 (caddr triplet)))
			unrefd))))))
    (simplify/env/bindings env0)
    bindings)
  (map cdr bindings))

(define (simplify/maybe-delete unrefd bnode form)
  (let ((position (simplify/operand/position unrefd form))
	(operator-refs (simplify/binding/operator-refs bnode)))
    (and (positive? position)		; continuation/ignore must remain
	 (if (for-all? operator-refs
	       (lambda (call)
		 (simplify/deletable-operand? call position)))
	     (begin
	       (for-each
		(lambda (call)
		  (simplify/delete-operand! call position))
		operator-refs)
	       (simplify/delete-parameter! form position))))))

(define (simplify/operand/position bnode* form)
  (let ((name (simplify/binding/name bnode*)))
    (let loop ((ll (cadr form))
	       (index 0))
      (cond ((null? ll)
	     (internal-error "Missing operand" name form))
	    ((eq? name (car ll)) index)
	    ((or (eq? (car ll) #!optional)
		 (eq? (car ll) #!rest))
	     -1)
	    (else
	     (loop (cdr ll) (+ index 1)))))))

(define (simplify/deletable-operand? call position)
  (let loop ((rands    (call/cont-and-operands call))
	     (position position))
    (and (not (null? rands))
	 (if (zero? position)
	     (form/simple&side-effect-free? (car rands))
	     (loop (cdr rands) (- position 1))))))

(define (simplify/delete-operand! call position)
  (form/rewrite!
   call
   `(CALL ,(call/operator call)
	  ,@(list-delete/index (call/cont-and-operands call) position))))

(define (simplify/delete-parameter! form position)
  (set-car! (cdr form)
	    (list-delete/index (cadr form) position)))

(define (list-delete/index l index)
  (let loop ((l l)
	     (index index)
	     (accum '()))
    (if (zero? index)
	(append (reverse accum) (cdr l))
	(loop (cdr l)
	      (- index 1)
	      (cons (car l) accum)))))

(define (simplify/bindings env0 unsafe-cyclic-reference? bindings body letify)
  ;; ENV0 is the current environment frame
  ;; BINDINGS is parallel to that, but is a list of
  ;;   (name expression) two-lists as returned by
  ;;   simplify/delete-parameters
  (let* ((frame-bindings (simplify/env/bindings env0))
	 (unused
	  (list-transform-positive frame-bindings
	    (lambda (binding)
	      (and (null? (simplify/binding/ordinary-refs binding))
		   (null? (simplify/binding/operator-refs binding)))))))
    (call-with-values
     (lambda ()
       (list-split unused
		   (lambda (binding)
		     (let* ((place (assq (simplify/binding/name binding)
					 bindings)))
		       (form/simple&side-effect-free? (cadr place))))))
     (lambda (simple-unused hairy-unused)
       ;; simple-unused can be flushed, since they have no side effects
       (let ((bindings* (delq* (map (lambda (simple)
				      (assq (simplify/binding/name simple)
					    bindings))
				    simple-unused)
			       bindings))
	     (not-simple-unused (delq* simple-unused frame-bindings)))
	 (if (or (not (eq? *order-of-argument-evaluation* 'ANY))
		 (null? hairy-unused))
	     (let ((new-env
		    (simplify/env/modified-copy env0 not-simple-unused)))
	       (simplify/bindings* new-env
				   bindings*
				   unsafe-cyclic-reference?
				   body
				   letify))
	     (let ((hairy-bindings
		    (map (lambda (hairy)
			   (assq (simplify/binding/name hairy)
				 bindings*))
			 hairy-unused))
		   (used-bindings (delq* hairy-unused not-simple-unused)))
	       (beginnify
		(append
		 (map cadr hairy-bindings)
		 (list
		  (let ((new-env
			 (simplify/env/modified-copy env0 used-bindings)))
		    (simplify/bindings* new-env
					(delq* hairy-bindings bindings*)
					unsafe-cyclic-reference?
					body
					letify))))))))))))

(define (simplify/bindings* env0 bindings unsafe-cyclic-reference? body letify)
  ;; ENV0 is the current environment frame, as simplified by simplify/bindings
  ;; BINDINGS is parallel to that, but is a list of
  ;;   (name expression) two-lists as returned by
  ;;   simplify/delete-parameters
  (let* ((frame-bindings (simplify/env/bindings env0))
	 (to-substitute
	  (list-transform-positive frame-bindings
	   (lambda (node)
	     (let* ((name  (simplify/binding/name node))
		    (value (second (assq name bindings))))
	       (and (pair? value)
		    (let ((ordinary (simplify/binding/ordinary-refs node))
			  (operator (simplify/binding/operator-refs node)))
		      (if (LAMBDA/? value)
			  (or (and (null? ordinary)
				   (or (null? (cdr operator))
				       (simplify/open-code?
					name value unsafe-cyclic-reference?)))
			      (and (null? operator)
				   (null? (cdr ordinary))))
			  (and (= (+ (length ordinary) (length operator)) 1)
			       (simplify/substitute? value body))))))))))
    (for-each
     (lambda (node)
       (simplify/substitute! env0
			     node
			     (cadr (assq (simplify/binding/name node)
					 bindings))))
     to-substitute)
    ;; This works only as long as all references are replaced.
    (letify (delq* (map (lambda (node)
			  (assq (simplify/binding/name node)
				bindings))
			to-substitute)
		   bindings)
	    body)))

(define (simplify/substitute? value body)
  (or (form/simple&side-effect-insensitive? value)
      (and *after-cps-conversion?*
	   (CALL/? body)
	   (form/simple&side-effect-free? value)
	   (not (form/static? value)))))

;; Note: this only works if no variable free in value is captured at any
;; reference in node.
;; This is true because the program was alpha-converted and when we
;; substitue expressions, we copy the form renaming the bound
;; variables.

(define (simplify/substitute! env node value)
  env					; ignored
  (let ((ordinary-refs  (simplify/binding/ordinary-refs node))
	(operator-refs  (simplify/binding/operator-refs node)))
    (define copy-value
      ;; We copy the value only when we are substituting in several places, and
      ;; then we copy only for the 2nd substitution onwards.  This
      ;; saves work because we tend to copy one huge expression or
      ;; many tiny ones.
      (let* ((all-refs (append ordinary-refs operator-refs)))
	(lambda (ref)
	  (if (eq? ref (car all-refs))
	      value
	      (simplify/copy-form/renaming env value)))))

    ;; In the case where the value is just another variable (i.e an
    ;; indirection), we must add the rewritten source to the
    ;; references of the value's variable.  In other cases the
    ;; references are in-tact because they are substructures of the
    ;; the value.
    (for-each (lambda (ref)
		(let ((value*  (copy-value ref)))
		  (simplify/remember*! ref value)
		  (form/rewrite! ref value*)
		  (if (LOOKUP/? value*)
		      (simplify/lookup*! env (lookup/name value*) ref 
					 'ORDINARY))))
      ordinary-refs)
    
    (for-each (lambda (ref)
		(form/rewrite! ref `(CALL ,(copy-value ref) ,@(cddr ref))))
      operator-refs)

    ;; For DBG info
    (dbg-info/remember (simplify/binding/name node) value)))

(define (simplify/copy-form/renaming env form)
  ;;  Copy FORM, renaming local bindings and keeping references to free
  ;;  variables in ENV.  Currently it does not update the debugging
  ;;  info, but it should.
  (define (rename name)
    (if (lambda-list-keyword? name)
	name
	(let ((new-name (variable/rename name)))
	  (dbg-info/remember name `(LOOKUP ,new-name))
	  new-name)))
  (define (walk renames form)
    (define (extend old new) (map* renames cons old new))
    (define (reference name wrap kind)
      (cond ((assq name renames)
	     => (lambda (place) (wrap (cdr place))))
	    (else
	     (simplify/lookup*! env name (wrap name) kind))))
    (define (let/letrec keyword)
      (let* ((old      (map first (second form)))
	     (new      (map rename old))
	     (renames* (extend  old new))
	     (renames** (if (eq? keyword 'LET) renames renames*)))
	`(,keyword ,(map (lambda (name binding)
			   (list name (walk renames** (second binding))))
			 new
			 (second form))
		   ,(walk renames* (third form)))))
    (define (walk* forms)
      (map (lambda (form*) (walk renames form*)) forms))
    (cond ((QUOTE/? form)  form)
	  ((LOOKUP/? form)
	   (let ((name  (lookup/name form)))
	     (define (lookup x) `(LOOKUP ,x))
	     (reference name lookup 'ORDINARY)))
	  ((LAMBDA/? form)
	   (let* ((old  (lambda/formals form))
		  (new  (map rename old)))
	     `(LAMBDA ,new
		,(walk (extend old new) (lambda/body form)))))
	  ((CALL/? form)
	   (if (LOOKUP/? (call/operator form))
	       (let ((name (lookup/name (call/operator form))))
		 (define (call name)
		   `(CALL (LOOKUP ,name)
			  ,@(walk* (call/cont-and-operands form))))
		 (reference name call 'OPERATOR))
	       `(CALL ,@(walk* (cdr form)))))
	  ((LET/? form)
	   (let/letrec 'LET))
	  ((LETREC/? form)
	   (let/letrec 'LETREC))
	  ((IF/? form)
	   `(IF ,@(walk* (cdr form))))
	  ((BEGIN/? form)
	   `(BEGIN ,@(walk* (cdr form))))
	  ((DECLARE/? form) `(DECLARE ,@(cdr form)))
	  (else
	   (internal-error "Unexpected syntax" form))))

  (walk '() form))

(define (simplify/pseudo-letify rator bindings body)
  (pseudo-letify rator bindings body simplify/remember))

(define (simplify/letify bindings body)
  `(LET ,bindings ,body))

(define (simplify/letrecify bindings body)
  `(LETREC ,bindings ,body))

(define (simplify/open-code? name value unsafe-cyclic-reference?)
  ;; VALUE must be a lambda expression
  (let ((body (lambda/body value)))
    (or (QUOTE/? body)
	(LOOKUP/? body)
	;;(and (CALL/? body)
	;;     (QUOTE/? (call/operator body))
	;;     (known-operator? (quote/text (call/operator body)))
	;;     (for-all? (call/cont-and-operands body)
	;;       (lambda (element)
	;;	 (or (QUOTE/? element)
	;;	     (LOOKUP/? element)))))
	(and (number? *simplify/open-code-expression-limit*)
	     (form/simple&side-effect-free? body)
	     (let* ((quota 
		     (+ *simplify/open-code-expression-limit*
			(length (lambda/formals value))))
		    (small?
		     (simplify/expression-small? body quota)))
	       (and small?
		    (begin
		      (if compiler:guru?
			  (pp `(Small-procedure-inlined:
				,value
				(cost:
				 ,(- quota small?)
				 initial-quota:
				 ,*simplify/open-code-expression-limit*
				 + ,(length (lambda/formals value))
				 remaining-quota:
				 ,small?))))
		      small?))))
	(and *after-cps-conversion?*
	     (CALL/? body)
	     (<= (call/count-dynamic-operands body)
		 (length (lambda/formals value)))
	     (not (unsafe-cyclic-reference? name))
	     (for-all? (cdr body)
	       (lambda (element)
		 (or (QUOTE/? element)
		     (LOOKUP/? element)
		     (form/static? element))))))))

(define *simplify/operator-open-coding-costs* (make-eq-hash-table))

(let ()
  (define (cost operator value)
    (hash-table/put! *simplify/operator-open-coding-costs* operator value))
  (cost not 0)
  ;;(cost %vector-index -2)
  (cost %heap-closure-ref -2)
  (cost %stack-closure-ref -2))

(define (simplify/expression-small? expr quota)
  (define (sub a b)
    (and a b
	 (let ((q (- a b)))
	   (and (> q 0) q))))
  (define (small?* exprs quota)
    (cond ((not quota) #F)
	  ((null? exprs) quota)
	  (else (small?* (cdr exprs) (small? (car exprs) 'SUBPROBLEM quota)))))
  (define (small? expr context quota)
    (cond ((not quota) #F)
	  ((QUOTE/? expr)
	   (if (eq? context 'PREDICATE) quota (sub quota 1)))
	  ((LOOKUP/? expr) (sub quota 1))
	  ((LAMBDA/? expr) (sub quota 1))
	  ((form/static? expr) quota)
	  ((LETREC/? expr)
	   (small? (letrec/body expr) context 'quota))
	  ((LET/? expr)
	   (small?* (map second (let/bindings expr))
		    (small? (let/body expr) 'SUBPROBLEM quota)))
	  ((and (CALL/? expr)
		(equal? (call/continuation expr) '(QUOTE #F)))
	   (let ((rator (call/operator expr)))
	     (cond ((QUOTE/? rator)
		    (small?* (call/operands expr)
			     (sub quota
				  (- (hash-table/get
				      *simplify/operator-open-coding-costs*
				      (quote/text rator)
				      1)
				     (call/count-static-operands expr)))))
		   (else #F))))
	  ((IF/? expr)
	   (small?* (cddr expr) (small? (cadr expr) 'PREDICATE quota)))
	  (else #F)))
  
  (small? expr 'SUBPROBLEM quota))

(define (call/count-dynamic-operands call)
  (- (length (call/operands call))
     (call/count-static-operands call)))

(define (call/count-static-operands call)
  (if (QUOTE/? (call/operator call))
      (let ((rator  (quote/text (call/operator call))))
	(cond ((eq? rator %invoke-remote-cache)      2)
	      ((eq? rator %invoke-operator-cache)    2)
	      ((eq? rator %internal-apply)           1)
	      ((eq? rator %internal-apply-unchecked) 1)
	      ((eq? rator %primitive-apply)          2)
	      ((eq? rator %cell-ref)                 1)
	      ((eq? rator %cell-set!)                1)
	      ((eq? rator %multicell-ref)            2)
	      ((eq? rator %multicell-set!)           2)
	      (else                                  0)))
      0))

(define (simplify/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  ;;(sample/1 '(simplify/dispatch histogram) (car expr))
  ;; Distrubution: quote: 57%, call: 20%, lookup: 17%, let: 3%, lambda: 2%
  (case (car expr)
    ((QUOTE)   (simplify/quote env expr))
    ((CALL)    (simplify/call env expr))
    ((LOOKUP)  (simplify/lookup env expr))
    ((LET)     (simplify/let env expr))
    ((LAMBDA)  (simplify/lambda env expr))
    ((IF)      (simplify/if env expr))
    ((LETREC)  (simplify/letrec env expr))
    ((BEGIN)   (simplify/begin env expr))
    ((DECLARE) (simplify/declare env expr))
    (else      (illegal expr))))

(define (simplify/expr* env exprs)
  (map (lambda (expr)
	 (simplify/expr env expr))
       exprs))

(define (simplify/remember new old)
  (code-rewrite/remember new old))

(define (simplify/remember*! new old)
  (code-rewrite/remember*! new (code-rewrite/original-form old)))

(define (simplify/new-name prefix)
  (new-variable prefix))

(define-structure
    (simplify/binding
     (conc-name simplify/binding/)
     (constructor simplify/binding/make (name))
     (print-procedure
      (standard-unparser-method 'SIMPLIFY/BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write-string (symbol-name (simplify/binding/name binding)) port)))))

  (name false read-only true)
  (ordinary-refs '() read-only false)
  (operator-refs '() read-only false))

(define-structure
    (simplify/env
     (conc-name simplify/env/)
     (constructor simplify/env/make (parent bindings))
     (print-procedure
      (standard-unparser-method 'SIMPLIFY/ENV
	(lambda (env port)
	  (write-char #\Space port)
	  (write (map simplify/binding/name (simplify/env/bindings env))
		 port)))))

  (bindings '() read-only true)
  (parent #F read-only true)
  ;; FREE-CALLS is used to mark calls to names free in this frame but bound
  ;; in the parent frame.  Used to detect mutual recursion in LETREC.
  (free-calls '() read-only false))

(define (simplify/env/modified-copy old-env new-bindings)
  (let ((result (simplify/env/make (simplify/env/parent old-env)
				   new-bindings)))
    (set-simplify/env/free-calls! result
     (simplify/env/free-calls old-env))
    result))


;; The profiler says a lot of time is being spent here in large programs:
;;(define simplify/env/frame-lookup
;;    (association-procedure (lambda (x y) (eq? x y)) simplify/binding/name))

(define (simplify/env/frame-lookup name bindings)
  (let loop ((bindings bindings))
    (if (pair? bindings)
	(if (eq? name (simplify/binding/name (car bindings)))
	    (car bindings)
	    (loop (cdr bindings)))
	#F)))

(define (simplify/lookup*! env name reference kind)
  ;; kind = 'OPERATOR, 'ORDINARY
  (let frame-loop ((prev #F)
		   (env env))
    (cond ((not env)
	   (free-var-error name)
	   reference)
	  ((simplify/env/frame-lookup name (simplify/env/bindings env))
	   => (lambda (binding)
		(case kind
		  ((OPERATOR)
		   (set-simplify/binding/operator-refs!
		    binding
		    (cons reference (simplify/binding/operator-refs binding)))
		   (if prev
		       (set-simplify/env/free-calls!
			prev
			(cons name (simplify/env/free-calls prev)))))
		  ((ORDINARY)
		   (set-simplify/binding/ordinary-refs!
		    binding
		    (cons reference (simplify/binding/ordinary-refs binding))))
		  (else
		   (internal-error "simplify/lookup*! bad KIND" kind)))
		reference))
	  (else (frame-loop env (simplify/env/parent env))))))
