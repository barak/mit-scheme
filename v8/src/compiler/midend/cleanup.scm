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

;;; package: (compiler midend)
;;
;; . Second half of beta substitution: substitute parameters at calls to
;;   known procedures.
;; . Constant folding (and rewrites on commutivity / associativity).

(declare (usual-integrations))

(define (cleanup/top-level program)
  (cleanup/expr (cleanup/env/initial) program))

(define-macro (define-cleanup-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'CLEANUP/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE (,proc-name ENV FORM)
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (CLEANUP/REMEMBER ,code FORM)))))))

(define-cleanup-handler LOOKUP (env name)
  (let ((value (cleanup/env/lookup name env)))
    (if (not value)
	(free-var-error name)
	(form/copy value))))

(define-cleanup-handler LAMBDA (env lambda-list body)
  (define (exit! name) (cleanup/env/exit! env name))
  (let ((lambda-list*
	 (map (lambda (name)
		(if (lambda-list-keyword? name)
		    name
		    (cleanup/binding/name (cleanup/env/enter! env name))))
	      lambda-list)))
    (let ((body* (cleanup/expr env body)))
      (for-each exit! (lambda-list->names lambda-list))
      `(LAMBDA ,lambda-list* ,body*))))

(define-cleanup-handler LETREC (env bindings body)
  (define (enter! binding) (cleanup/env/enter! env (car binding)))
  (define (exit! binding) (cleanup/env/exit! env (car binding)))
  (let ((bindings* (map enter! bindings)))
    (let ((body*   (cleanup/expr env body)))
      (let ((result
	     (if (null? bindings)
		 body*
		 `(LETREC ,(map (lambda (binding binding*)
				  (list (cleanup/binding/name binding*)
					(cleanup/expr env (second binding))))
				bindings
				bindings*)
		    ,body*))))
	(for-each exit! bindings)
	result))))

(define-cleanup-handler QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-cleanup-handler DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-cleanup-handler BEGIN (env #!rest actions)
  (beginnify (cleanup/expr* env actions) #T))

(define-cleanup-handler LET (env bindings body)
  (cleanup/let* cleanup/letify env bindings body))

(define-cleanup-handler IF (env pred conseq alt)
  (cleanup/if/un-not env pred conseq alt #T))

(define (cleanup/if/un-not env pred conseq alt source-pred?)
  ;; repeatedly transform (IF (not p) c a) => (if p a c)
  (cond ((and (CALL/? pred)
	      (QUOTE/? (call/operator pred))
	      (eq? (quote/text (call/operator pred)) not)
	      (equal? (call/continuation pred) `(QUOTE #F)))
	 (cleanup/if/un-not env (first (call/operands pred))
			    alt conseq
			    source-pred?))
	(source-pred?			; try again with cleaned-up pred
	 (cleanup/if/un-not env (cleanup/expr env pred) conseq alt #F))
	(else
	 (cleanup/if/try-2 env pred conseq alt))))

(define (cleanup/if/try-2 env pred* conseq alt)
  (let ((conseq*  (cleanup/expr env conseq))
	(alt*     (cleanup/expr env alt)))
    (define (default) `(IF ,pred* ,conseq* ,alt*))
    (cond ((QUOTE/? pred*)
	   (case (boolean/discriminate (quote/text pred*))
	     ((FALSE)    alt*)
	     ((TRUE)     conseq*)
	     (else       (default))))
	  (;; (if p p #F) => p    (Some generic arith diamonds)
	   (and (equal? alt* '(QUOTE #F))
		(equal? pred* conseq*)
		(form/simple&side-effect-free? pred*))
	   pred*)
	  (else  (default)))))

(define-cleanup-handler CALL (env rator cont #!rest rands)
  (define (default)
    (let ((rator*  (cleanup/expr env rator))
	  (cont*   (cleanup/expr env cont))
	  (rands*  (cleanup/expr* env rands)))
      ;; (CALL (LETREC (...) foo) a b c) =>  (LETREC (...) (CALL foo a b c))
      ;;  [assumption: program is alpha-converted to avoid name capture]
      (if (and (LETREC/? rator*)
	       (LOOKUP/? (letrec/body rator*)))
	  `(LETREC ,(letrec/bindings rator*)
	     (CALL ,(letrec/body rator*) ,cont* ,@rands*))
	  `(CALL ,rator* ,cont* ,@rands*))))
  (cond ((QUOTE/? rator)
	 (let ((rator-name  (quote/text rator))
	       (cont*   (cleanup/expr env cont))
	       (rands*  (cleanup/expr* env rands)))
	   (define (default)
	     `(CALL (QUOTE ,rator-name) ,cont* ,@rands*))
	   (define (use-result result)
	     (if (equal? cont* '(QUOTE #F))
		 result
		 `(CALL (QUOTE ,%invoke-continuation) ,cont* ,result)))
	   (define (try-op operator arity rands**)
	     (cond ((cleanup/rewrite? operator arity)
		    => (lambda (handler)
			 (cond ((apply handler rands**)
				=> use-result)
			       (else (default)))))
		   (else (default))))
	   (if (eq? rator-name %invoke-remote-cache)
	       (let ((descriptor (quote/text (car rands*))))
		 (try-op (first descriptor) (second descriptor) (cddr rands*)))
	       (try-op rator-name (length rands*) rands*))))
	((LAMBDA/? rator)
         (let ((lambda-list  (lambda/formals rator))
               (lambda-body  (lambda/body rator)))
           (define (generate env let-names let-values)
	     ;;(pp ` (generate ,env ,let-names ,let-values))
             (cleanup/let*
              (lambda (bindings* body*)
                (cleanup/pseudo-letify rator bindings* body*))
              env
              (cleanup/lambda-list->bindings form let-names let-values)
              lambda-body))

	   (if (call/%make-stack-closure? cont)
	       ;; Cannot substitute a make-stack-closure because both pushing
	       ;; and poping have to be kept in the right order.  Deal with
	       ;; this by splitting off the continuation binding and
               ;; treating the rest of the bindings normally.
	       (let ((old-cont-var (car lambda-list)))
		 (let ((cont* (cleanup/expr env cont)))
		   (let ((cont-binding (cleanup/env/enter! env old-cont-var)))
		     (let ((result
			    (cleanup/bind-stack-closure
			     rator
			     (cleanup/binding/name cont-binding)
			     (generate env (cdr lambda-list) rands)
			     cont*)))
		       (cleanup/env/exit! env old-cont-var)
		       result))))
	       (generate env lambda-list (cons cont rands)))))
	((not *flush-closure-calls?*)
	 (default))
	(else
	 (let ((call* (default)))
	   (cond ((form/match cleanup/call-closure-pattern call*)
		  => (lambda (result)
		       (cleanup/call/maybe-flush-closure call* env result)))
		 ((form/match cleanup/call-trivial-pattern call*)
		  => (lambda (result)
		       (let ((lam-expr (cadr (assq cleanup/?lam-expr result)))
			     (rands    (cadr (assq cleanup/?rands result)))
			     (cont     (cadr (assq cleanup/?cont result))))
			 (cleanup/expr env
				       `(CALL ,lam-expr ,cont ,@rands)))))
		 (else  call*))))))

(define (cleanup/bind-stack-closure rator new-cont-var body closure)
  ;; Construct an expression of the form
  ;;   (CALL (LAMBDA (new-cont-var) body) closure)
  ;;
  ;; We handle two special cases, which are equivalent to substituting for
  ;; NEW-CONT-VAR.  This would not be necessary if simplify was
  ;; better.  As simplify is one-pass, it occasionally leaves redexes
  ;; which only get discovered after stack closures are introduced.
  ;; In fact, simplify might be a better place for this rewrite.  The
  ;; rewrites look clearer in standard CPS scheme (K is NEW-CONT-VAR):
  ;;
  ;;    ((lambda (k) (k e1 ...)) <closure>)   => (<closure> e1 ...)
  ;;    ((lambda (k) (f k e1 ...)) <closure>) => (f <closure> e1 ...)
  ;;
  ;; Note that we take care to check that the make-stack-closure is a real
  ;; continuation and not, for example, pushing extra arguments.

  (define (ordinary-case)
    (let ((new-lambda  `(LAMBDA (,new-cont-var) ,body)))
      (cleanup/remember new-lambda rator)
      `(CALL ,new-lambda ,closure)))
  (cond ((and (CALL/%invoke-continuation? body)
	      (LOOKUP/? (call/%invoke-continuation/cont body))
	      (eq? new-cont-var
		   (lookup/name (call/%invoke-continuation/cont body)))
	      (CALL/%make-stack-closure? closure)
	      (LAMBDA/?
	       (CALL/%make-stack-closure/lambda-expression closure)))
	 `(CALL (QUOTE ,%invoke-continuation)
		,closure
		,@(CALL/%invoke-continuation/values body)))
	((and (CALL/? body)
	      (LOOKUP/? (call/operator body))
	      (LOOKUP/? (call/continuation body))
	      (eq? new-cont-var (lookup/name (call/continuation body)))
	      (CALL/%make-stack-closure? closure)
	      (LAMBDA/?
	       (CALL/%make-stack-closure/lambda-expression closure)))
	 `(CALL ,(call/operator body)
		,closure
		,@(call/operands body)))
	(else (ordinary-case))))

(define *cleanup/rewriters* (make-monotonic-strong-eq-hash-table))

(define (cleanup/rewrite? name arity)
  (cond ((monotonic-strong-eq-hash-table/get *cleanup/rewriters* name #F)
	 => (lambda (alist)
	      (cond ((assq arity alist) => cdr)
		    (else  #F))))
	(else  #F)))

(define (define-cleanup-rewrite name arity handler)
  (let ((slot
	 (monotonic-strong-eq-hash-table/get *cleanup/rewriters* name '())))
    (monotonic-strong-eq-hash-table/put! *cleanup/rewriters*
					 name
					 (cons (cons arity handler) slot)))
  name)

(let ()
  ;; Arithmetic constant folding
  (define (quote-unmapped v)
    `(QUOTE ,(unmap-careful v)))

  (define (unary name op)
    (define-cleanup-rewrite name 1
      (lambda (expr)
	(let  ((value (form/number? expr)))
	  (and value
	       (let ((result  (op value)))
		 (and result
		      (quote-unmapped result))))))))

  (define (careful-binary name op)
    (define-cleanup-rewrite name 2
      (lambda (expr1 expr2)
	(let ((value1  (form/number? expr1)))
	  (and value1
	       (let ((value2  (form/number? expr2)))
		 (and value2
		      (let ((result  (op value1 value2)))
			(and result
			     (quote-unmapped result))))))))))

  (define (binary name op)
    (define-cleanup-rewrite name 2
      (lambda (expr1 expr2)
	(let ((value1  (form/number? expr1)))
	  (and value1
	       (let ((value2  (form/number? expr2)))
		 (and value2
		      `(QUOTE ,(op value1 value2)))))))))

  (unary 'SQRT sqrt)
  (unary 'EXP  exp)
  (unary 'LOG  log)
  (unary 'SIN  sin)
  (unary 'COS  cos)
  (unary 'TAN  tan)
  (unary 'ASIN asin)
  (unary 'ACOS acos)

  (unary  'ATAN atan)
  (binary 'ATAN atan)

  (binary 'EXPT expt)
  (binary (make-primitive-procedure '&+) +)
  (binary (make-primitive-procedure '&-) -)
  (binary (make-primitive-procedure '&*) *)
  (binary (make-primitive-procedure '&<) <)
  (binary (make-primitive-procedure '&=) =)
  (binary (make-primitive-procedure '&>) >)

  (careful-binary (make-primitive-procedure '&/) careful//)
  (careful-binary (make-primitive-procedure 'QUOTIENT) careful/quotient)
  (careful-binary (make-primitive-procedure 'REMAINDER) careful/remainder)
)

;; Fixnum algebraic rewrites
;;   (+ (+ x a) b)  =>  (+ x (+ a b))
;;   (+ a x) => (+ x a)
;;   (- x a) => (+ x -a)
;;   (+ (+ x a) (+ y b)) => (+ (+ x y) (+ a b))

(let ()
  (define (constant-case op value1 value2) ; OP should be overflow-save
    (and (fixnum? value1) (fixnum? value2)
	 (let ((result (op value1 value2)))
	   (and (fixnum? result)
		`(QUOTE ,result)))))

  (define (call-of? op form)
    (and (CALL/? form)
	 (QUOTE/? (call/operator form))
	 (eq? (quote/text (call/operator form)) op)))

  (define (define-commutative-cleanup-rewrite fix-op #!optional test-op)
    (let ((test-op (if (default-object? test-op) fix-op test-op)))
      (define-cleanup-rewrite fix-op 2
	(lambda (expr1 expr2)
	  (let ((value1 (form/number? expr1))
		(value2 (form/number? expr2)))
	    (cond ((constant-case test-op value1 value2))
		  ((and (call-of? fix-op expr1)
			(constant-case test-op value2
				       (form/number? (call/operand2 expr1))))
		   ;; (op (op x c) d) => (op x (op c d))
		   => (lambda (result)
			`(CALL ',fix-op '#F ,(call/operand1 expr1) ,result)))
		  ((and (call-of? fix-op expr1)
			(call-of? fix-op expr2)
			(constant-case test-op
				       (form/number? (call/operand2 expr1))
				       (form/number? (call/operand2 expr2))))
		   ;; (op (op x a) (op y b)) => (op (op x y) (op a b))
		   => (lambda (result)
			`(CALL ',fix-op
			       '#F
			       (CALL ',fix-op
				     '#F
				     ,(call/operand1 expr1)
				     ,(call/operand1 expr2))
			       ,result)))
		  ((and value1 (not value2)) ; (op c x) => (op x c)
		   `(CALL ',fix-op '#F ,expr2 ,expr1))
		  (else #F)))))))

  (define-cleanup-rewrite fix:- 2
    (lambda (expr1 expr2)
      (let ((value1 (form/number? expr1))
	    (value2 (form/number? expr2)))
	(cond ((constant-case - value1 value2))
	      ((and (fixnum? value2) (fixnum? (- value2)))
	       `(CALL ',fix:+ '#F ,expr1 ',(- value2)))
	      (else #F)))))

  (define-commutative-cleanup-rewrite fix:and)
  (define-commutative-cleanup-rewrite fix:or)
  (define-commutative-cleanup-rewrite fix:xor)
  (define-commutative-cleanup-rewrite fix:+ +)
  (define-commutative-cleanup-rewrite fix:* *))

(define-cleanup-rewrite 'STRING->SYMBOL 1
  (lambda (expr)
    (and (QUOTE/? expr)
	 (string? (quote/text expr))
	 `(QUOTE ,(string->symbol (quote/text expr))))))

(define-cleanup-rewrite 'INTERN 1
  (lambda (expr)
    (and (QUOTE/? expr)
	 (string? (quote/text expr))
	 `(QUOTE ,(intern (quote/text expr))))))

(define-cleanup-rewrite (make-primitive-procedure 'EQ?) 2
  (lambda (e1 e2)
    (and (QUOTE/? e1)
	 (QUOTE/? e2)
	 `(QUOTE ,(eq? (quote/text e1) (quote/text e2))))))

(define-cleanup-rewrite %fixnum->flonum 1
  (lambda (expr)
    (and (QUOTE/? expr)
	 (fixnum? (quote/text expr))
	 `(QUOTE ,(exact->inexact (quote/text expr))))))

(let ((NOT-primitive  (make-primitive-procedure 'NOT)))
  (define (form-absorbs-not? form)
    ;; Assumption: open-coded (non out-of-line) predicates can be compiled
    ;; with negated tests.
    (or (QUOTE/? form)
	(LOOKUP/? form)			; only true if in a predicate context
	(and (CALL/? form)
	     (QUOTE/? (call/operator form))
	     (let ((rator  (quote/text (call/operator form))))
	       (and (operator/satisfies? rator '(PROPER-PREDICATE))
		    (not (operator/satisfies? rator '(OUT-OF-LINE-HOOK))))))))
  (define (apply-NOT expr)
    (cond ((QUOTE/? expr) `(QUOTE ,(not (quote/text expr))))
	  ((and (IF/? expr)
		(or (form-absorbs-not? (if/consequent expr))
		    (form-absorbs-not? (if/alternate expr))))
	   ;; (NOT (IF p c a)) =>  (IF p (NOT c) (NOT a))
	   `(IF ,(if/predicate expr)
		,(apply-NOT (if/consequent expr))
		,(apply-NOT (if/alternate expr))))
	  (else
	   `(CALL (QUOTE ,NOT-primitive) '#F ,expr))))
  (define-cleanup-rewrite NOT-primitive 1 apply-NOT))

(define (cleanup/call/maybe-flush-closure call* env match-result)
  (let ((lambda-expr    (cadr (assq cleanup/?lam-expr match-result)))
	(cont           (cadr (assq cleanup/?cont match-result)))
	(closure-elts   (cadr (assq cleanup/?closure-elts match-result)))
	(closure-vector (cadr (assq cleanup/?closure-vector match-result)))
	(rands          (cadr (assq cleanup/?rands match-result))))
    (let* ((lambda-list (cadr lambda-expr))
	   (lambda-body (caddr lambda-expr))
	   (closure-name (cadr lambda-list)))
      (call-with-values
       (lambda () (cleanup/closure-refs lambda-body closure-name))
       (lambda (self-refs ordinary-refs)
	 (if (not (null? self-refs))
	     call*
	     (let ((bindings (map list
				  (vector->list closure-vector)
				  closure-elts)))
	       (for-each (lambda (ref)
			   (let ((name (cadr (sixth ref))))
			     (form/rewrite! ref `(LOOKUP ,name))))
			 ordinary-refs)
	       (let ((cont-name (car lambda-list)))
		 (cleanup/expr
		  env
		  (bind* (cons cont-name (map car bindings))
			 (cons cont (map cadr bindings))
			 `(CALL (LAMBDA ,(cons (car lambda-list)
					       (cddr lambda-list))
				  ,lambda-body)
				,(if (equal? cont `(QUOTE #F))
				     `(QUOTE #F)
				     `(LOOKUP ,cont-name))
				,@rands)))))))))))

(define cleanup/?closure-elts (->pattern-variable 'CLOSURE-ELTS))
(define cleanup/?closure-vector (->pattern-variable 'CLOSURE-VECTOR))
(define cleanup/?cont (->pattern-variable 'CONT))
(define cleanup/?nrands (->pattern-variable 'NRANDS))
(define cleanup/?rands (->pattern-variable 'RANDS))
(define cleanup/?lam-expr (->pattern-variable 'LAM-EXPR))
(define cleanup/?rest (->pattern-variable 'REST))

(define cleanup/call-closure-pattern
  `(CALL (QUOTE ,%internal-apply)
	 ,cleanup/?cont
	 (QUOTE ,cleanup/?nrands)
	 (CALL (QUOTE ,%make-heap-closure)
	       (QUOTE #F)
	       ,cleanup/?lam-expr
	       (QUOTE ,cleanup/?closure-vector)
	       ,@cleanup/?closure-elts)
	 ,@cleanup/?rands))

(define cleanup/call-trivial-pattern
  `(CALL (QUOTE ,%internal-apply)
	 ,cleanup/?cont
	 (QUOTE ,cleanup/?nrands)
	 (CALL (QUOTE ,%make-trivial-closure)
	       (QUOTE #F)
	       ,cleanup/?lam-expr)
	 ,@cleanup/?rands))

(define (cleanup/closure-refs form var-name)
  ;; (values self-refs ordinary-refs)
  ;; var-name is assumed to be unique, so there is
  ;; no need to worry about shadowing.
  (list-split
   (let walk ((form form))
     (and (pair? form)
	  (case (car form)
	    ((QUOTE DECLARE) '())
	    ((LOOKUP)
	     (if (eq? (lookup/name form) var-name)
		 (list form)
		 '()))
	    ((LAMBDA)
	     (walk (lambda/body form)))
	    ((LET LETREC)
	     (append-map* (walk (caddr form))
			  (lambda (binding)
			    (walk (cadr binding)))
			  (cadr form)))
	    ((BEGIN IF)
	     (append-map walk (cdr form)))
	    ((CALL)
	     (if (call/%heap-closure-ref? form)
		 (if (eq? (lookup/name (call/%heap-closure-ref/closure form))
			  var-name)
		     (list form)
		     '())
		 (append-map walk (cdr form))))
	    (else
	     (no-longer-legal form)))))
   LOOKUP/?))

(define (cleanup/let* letify env bindings body)
  ;; Some bindings bind names to trivial expressions (e.g. constant) and
  ;; easy expression (e.g. closure references).  We substitute the
  ;; expressions for these names in BODY.  The rest remain bound.
  (define (binding-value binding) (cleanup/expr env (second binding)))
  (define (exit! binding)  (cleanup/env/exit!  env (car binding)))
  (define (loop bindings* values* remainding-bindings)
    (cond ((null? bindings*) (reverse! remainding-bindings))
	  ((cleanup/always-substitute? (car values*))
	   (cleanup/env/enter!/value env (caar bindings*) (car values*))
	   (loop (cdr bindings*) (cdr values*) remainding-bindings))
	  (else
	   (let ((new-binding (cleanup/env/enter! env (caar bindings*))))
	     (loop (cdr bindings*)
		   (cdr values*)
		   (cons (list (cleanup/binding/name new-binding)
			       (car values*))
			 remainding-bindings))))))
  (let ((values (map binding-value bindings)))
    (let ((remainding-bindings (loop bindings values '())))
      (let ((body* (cleanup/expr env body)))
	(for-each exit! bindings)
	(if (null? remainding-bindings)
	    body*
	    (letify remainding-bindings body*))))))

(define (cleanup/always-substitute? form)
  (or (LOOKUP/? form)
      (QUOTE/? form)
      (call/%stack-closure-ref? form)
      (call/%heap-closure-ref? form)))	; OK: no mutators for heap closures

(define (cleanup/letify bindings body)
  `(LET ,bindings ,body))

(define (cleanup/lambda-list->bindings form lambda-list operands)
  ;; returns LET-like bindings
  (map (lambda (name operand) (list name operand))
       (lambda-list->names lambda-list)
       (lambda-list/applicate form lambda-list operands)))

(define (cleanup/pseudo-letify rator bindings body)
  ;; If the body is a lookup
  (define (default)
    (pseudo-letify rator bindings body cleanup/remember))
  (define (trivial last bindings)
    (beginnify (map* (list last) cadr bindings)))
  (cond ((memq *order-of-argument-evaluation* '(ANY LEFT-TO-RIGHT))
	 (default))
	((LOOKUP/? body)  ; ([]LET (... (x e) ...) x) => (begin ... e)
	 (let ((place (assq (lookup/name body) bindings)))
	   (if (not place)
	       (trivial body bindings)
	       (trivial	(second place) (delq place bindings)))))
	((QUOTE/? body)
	 (trivial body bindings))
	(else
	 (default))))

;; Environment is a map from names to bindings.  Because the flow of
;; control is a DFS of the scopes, we can maintain the map by adding
;; bindings on entry to a scope, and removing it on exit.

(define (cleanup/env/find name env)
  (monotonic-strong-eq-hash-table/get env name #F))

(define (cleanup/env/lookup name env)
  (let ((binding  (cleanup/env/find name env)))
    (and binding
	 (cleanup/binding/value binding))))

(define (cleanup/env/initial)
  (make-monotonic-strong-eq-hash-table))

(define (cleanup/env/enter! env name)	; ->binding
  (let* ((shadowed  (monotonic-strong-eq-hash-table/get env name #F))
	 (name*     (if shadowed
			(let ((new-name (variable/rename name)))
			  (dbg-info/remember name new-name)
			  new-name)
			name))
	 (binding   (cleanup/binding/make name* `(LOOKUP ,name*) shadowed)))
    (monotonic-strong-eq-hash-table/put! env name binding)
    binding))

(define (cleanup/env/enter!/value env name value) ; ->binding
  ;; enter the scope of a variable which will be substituted
  (let* ((shadowed  (monotonic-strong-eq-hash-table/get env name #F))
	 (binding   (cleanup/binding/make #F value shadowed)))
    (dbg-info/remember name value)
    (monotonic-strong-eq-hash-table/put! env name binding)
    binding))

(define (cleanup/env/exit! env name)
  (let ((binding (monotonic-strong-eq-hash-table/get env name #F)))
    (monotonic-strong-eq-hash-table/put! env name
					 (cleanup/binding/shadowed binding))))

(define-structure
    (cleanup/binding
     (conc-name cleanup/binding/)
     (constructor cleanup/binding/make (name value shadowed)))
  (name     #F  read-only true)
  (value    #F  read-only true)
  (shadowed #F  read-only true))

(define (cleanup/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  ;;(sample/1 '(cleanup/dispatch histogram) (car expr))
  ;; Dynamic Freqency: quote: 48%, call: 24%, lookup: 20%, let: 4%, ...
  (case (car expr)
    ((QUOTE)    (cleanup/quote env expr))
    ((CALL)     (cleanup/call env expr))
    ((LOOKUP)   (cleanup/lookup env expr))
    ((LET)      (cleanup/let env expr))
    ((LAMBDA)   (cleanup/lambda env expr))
    ((IF)       (cleanup/if env expr))
    ((LETREC)   (cleanup/letrec env expr))
    ((BEGIN)    (cleanup/begin env expr))
    ((DECLARE)  (cleanup/declare env expr))
    (else
     (illegal expr))))

(define (cleanup/expr* env exprs)
  (map (lambda (expr)
	 (cleanup/expr env expr))
       exprs))

(define (cleanup/remember new old)
  (code-rewrite/remember new old))