#| -*-Scheme-*-

$Id: cleanup.scm,v 1.27 1995/11/28 17:43:59 adams Exp $

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

;;;; Rename to avoid conflict, substitute parameters, etc.
;;; package: (compiler midend)

(declare (usual-integrations))

(define (cleanup/top-level program)
  (cleanup/expr (cleanup/env/initial) program))

(define-macro (define-cleanup-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'CLEANUP/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (CLEANUP/REMEMBER ,code FORM))))))))

(define-cleanup-handler LOOKUP (env name)
  (let ((value (cleanup/env/lookup name env)))
    (if (not value)
	(free-var-error name)
	(form/copy value))))

(define-cleanup-handler LAMBDA (env lambda-list body)
  (let ((renames (cleanup/renamings env (lambda-list->names lambda-list))))
    `(LAMBDA ,(map (lambda (token)
		     (cleanup/rename renames token))
		   lambda-list)
       ,(cleanup/expr (cleanup/env/extend env renames) body))))

(define-cleanup-handler LETREC (env bindings body)
  (do-letrec-cleanup env bindings body))

(define (do-letrec-cleanup env bindings body)
  (let* ((renames (cleanup/renamings env (map car bindings)))
	 (env*    (cleanup/env/extend env renames))
	 (body*   (cleanup/expr env* body)))
    (if (null? bindings)
	body*
	`(LETREC ,(map (lambda (binding)
			 (list (cleanup/rename renames (car binding))
			       (cleanup/expr env* (cadr binding))))
		       bindings)
	   ,body*))))

(define-cleanup-handler QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-cleanup-handler DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

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

(define-cleanup-handler BEGIN (env #!rest actions)
  (beginnify (cleanup/expr* env actions) #T))

(define-cleanup-handler LET (env bindings body)
  (cleanup/let* cleanup/letify env bindings body))

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
	   (call-with-values
	       (lambda ()
		 (cond ((eq? rator-name %invoke-remote-cache)
			(let ((descriptor (quote/text (car rands*))))
			  (values (first descriptor)
				  (second descriptor)
				  (cddr rands*))))
		       (else
			(values rator-name (length rands*) rands*))))
	     (lambda (operator arity rands**)
	       (cond ((cleanup/rewrite? operator arity)
		      => (lambda (handler)
			   (cond ((apply handler rands**)
				  => use-result)
				 (else (default)))))
		     (else (default)))))))
	((LAMBDA/? rator)
         (let ((lambda-list  (lambda/formals rator))
               (lambda-body  (lambda/body rator)))
           (define (generate env let-names let-values)
             (cleanup/let*
              (lambda (bindings* body*)
                (cleanup/pseudo-letify rator bindings* body*))
              env
              (cleanup/bindify let-names let-values)
              lambda-body))
	   #|				;
	   (define (build-call-lambda/try1 new-cont-var body closure) ;
	     `(CALL (LAMBDA (,new-cont-var) ,body) ,closure))
	   |#
	   (define (build-call-lambda/try2 new-cont-var body closure)
	     ;; We can further reduce one special case: when the body is an
	     ;; invoke-continuation and the stack closure is a real
	     ;; continuation (not just a push)
	     (if (and (CALL/%invoke-continuation? body)
		      (LOOKUP/? (CALL/%invoke-continuation/cont body))
		      (eq? new-cont-var
			   (LOOKUP/name (CALL/%invoke-continuation/cont body)))
		      (CALL/%make-stack-closure? closure)
		      (LAMBDA/?
		       (CALL/%make-stack-closure/lambda-expression closure)))
		 `(CALL (QUOTE ,%invoke-continuation)
			,closure
			,@(CALL/%invoke-continuation/values body))
		 (let ((new-lambda  `(LAMBDA (,new-cont-var) ,body)))
		   (cleanup/remember new-lambda rator)
		   `(CALL ,new-lambda ,closure))))
	   (if (call/%make-stack-closure? cont)
	       ;; Cannot substitute a make-stack-closure because both pushing
	       ;; and poping have to be kept in the right order.
	       (let* ((old-cont-var (car lambda-list))
		      (new-cont-var (variable/rename old-cont-var))
		      (new-env
		       (cleanup/env/extend
			env
			(list (cleanup/binding/make old-cont-var 
						    `(LOOKUP ,new-cont-var)))))
		      )
		 (build-call-lambda/try3
		  rator
		  new-cont-var
		  (generate new-env (cdr lambda-list) rands)
		  (cleanup/expr env cont)))
	       (generate env lambda-list (cons cont rands)))))
	((not *flush-closure-calls?*)
	 (default))
	(else
	 (let ((call* (default)))
	   (cond ((form/match cleanup/call-closure-pattern call*)
		  => (lambda (result)
		       (cleanup/call/maybe-flush-closure call*
							 env
							 result)))
		 ((form/match cleanup/call-trivial-pattern call*)
		  => (lambda (result)
		       (let ((lam-expr (cadr (assq cleanup/?lam-expr result)))
			     (rands    (cadr (assq cleanup/?rands result)))
			     (cont     (cadr (assq cleanup/?cont result))))
			 (cleanup/expr env
				       `(CALL ,lam-expr ,cont ,@rands)))))
		 (else
		  call*))))))


(define (build-call-lambda/try3 rator new-cont-var body closure)
  ;; We can further reduce one special case: when the body is an
  ;; invoke-continuation and the stack closure is a real
  ;; continuation (not just a push)
  (cond ((and (CALL/%invoke-continuation? body)
	      (LOOKUP/? (call/%invoke-continuation/cont body))
	      (eq? new-cont-var
		   (lookup/name
		    (call/%invoke-continuation/cont body)))
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
	(else
	 (let ((new-lambda  `(LAMBDA (,new-cont-var) ,body)))
	   (cleanup/remember new-lambda rator)
	   `(CALL ,new-lambda ,closure)))))


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
;;
;; (+ (+ x a) b)  =>  (+ x (+ a b))
;; (+ a x) => (+ x a)
;; (- x a) => (+ x -a)
;; (+ (+ x a) (+ y b)) => (+ (+ x y) (+ a b))

(let ()
  (define (constant-case op value1 value2) ; OP should be overflow-save
    (and (fixnum? value1)
	 (fixnum? value2)
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
    (let  ((value (form/number? expr)))
      (and (QUOTE/? expr)
	   (string? (quote/text expr))
	   `(QUOTE ,(string->symbol (quote/text expr)))))))

(define-cleanup-rewrite (make-primitive-procedure 'EQ?) 2
  (lambda (e1 e2)
    (and (QUOTE/? e1)
	 (QUOTE/? e2)
	 `(QUOTE ,(eq? (quote/text e1) (quote/text e2))))))

;;
(let ((NOT-primitive  (make-primitive-procedure 'NOT)))
  (define (form-absorbs-not? form)
    ;; Assumption: non out-of-line predicates can be compiled with negated
    ;; tests.
    (or (and (CALL/? form)
	     (QUOTE/? (call/operator form))
	     (let ((rator  (quote/text (call/operator form))))
	       (and (operator/satisfies? rator '(PROPER-PREDICATE))
		    (not (operator/satisfies? rator '(OUT-OF-LINE-HOOK))))))
	(QUOTE/? form)
	(LOOKUP/? form)))
  (define-cleanup-rewrite NOT-primitive 1
    (lambda (expr)
      ;; (NOT (IF p c a)) =>  (IF p (NOT c) (NOT a))
      (if (and (IF/? expr)
	       (or (form-absorbs-not? (if/consequent expr))
		   (form-absorbs-not? (if/alternate expr))))
	  `(IF ,(if/predicate expr)
	       (CALL (QUOTE ,NOT-primitive) '#F ,(if/consequent expr))
	       (CALL (QUOTE ,NOT-primitive) '#F ,(if/alternate expr)))
	  `(CALL (QUOTE ,NOT-primitive) '#F ,expr)))))

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
  ;; expressions for these names in BODY, but first we look at the
  ;; names in these expressions and rename to avoid name capture.
  (let ((bindings* (map (lambda (binding)
			  (cleanup/binding/make
			   (car binding)
			   (cleanup/expr env (cadr binding))))
			bindings)))
    (define (dbg-track! bindings)
      (for-each (lambda (binding)
		  (dbg-info/remember (cleanup/binding/name binding)
				     (form/copy (cleanup/binding/value binding))))
	bindings))
    (call-with-values
     (lambda ()
       (list-split bindings*
		   (lambda (binding*)
		     (QUOTE/? (cleanup/binding/value binding*)))))
     (lambda (trivial non-trivial)
       (call-with-values
	(lambda ()
	  (list-split non-trivial
		      (lambda (binding*)
			(cleanup/easy? (cleanup/binding/value binding*)))))
	(lambda (easy non-easy)
	  (let* ((possibly-captured
		  (map (lambda (binding)
			 (cleanup/easy/name (cleanup/binding/value binding)))
		       easy))
		 (complex-triplets
		  ;; (original-name renamed-version value-expression)
		  (map (lambda (binding)
			 (let ((name (cleanup/binding/name binding)))
			   (list name
				 (if (memq name possibly-captured)
				     (variable/rename name)
				     name)
				 (cleanup/binding/value binding))))
			non-easy))
		 (env*
		  (cleanup/env/extend
		   env
		   (map* (append trivial easy)
			 (lambda (triplet)
			   (cleanup/binding/make
			    (car triplet)
			    `(LOOKUP ,(cadr triplet))))
			 complex-triplets))))
	    (dbg-track! trivial)
	    (dbg-track! easy)
	    (let ((body* (cleanup/expr env* body)))
	      (if (null? complex-triplets)
		  body*
		  (letify (map cdr complex-triplets)
			  body*))))))))))

(define (cleanup/easy? form)
  (cond ((LOOKUP/? form) true)
	((CALL/? form)
	 (let ((rator (call/operator form)))
	   (and (QUOTE/? rator)
		(memq (quote/text rator) cleanup/easy/ops)
		(let ((cont&rands (call/cont-and-operands form)))
		  (and (for-all? cont&rands cleanup/trivial?)
		       (let ((all-lookups
			      (list-transform-positive cont&rands LOOKUP/?)))
			 (or (null? all-lookups)
			     (null? (cdr all-lookups)))))))))
	(else
	 false)))

(define (cleanup/trivial? form)
  (or (QUOTE/? form)
      (LOOKUP/? form)
      (and (CALL/? form)
	   (QUOTE (call/operator form))
	   (memq (quote/text (call/operator form)) cleanup/trivial/ops)
	   (for-all? (call/cont-and-operands form)
	     QUOTE/?))))

(define (cleanup/easy/name form)
  ;; form must satisfy cleanup/easy?
  (cond ((LOOKUP/? form) (lookup/name form))
	((CALL/? form)
	 (let ((lookup-rand
		(list-search-positive (call/cont-and-operands form) LOOKUP/?)))
	   (and lookup-rand
		(lookup/name lookup-rand))))
	(else
	 (internal-error "Unrecognized easy form" form))))

(define cleanup/trivial/ops '())
;  (list %vector-index)

(define cleanup/easy/ops
  (append cleanup/trivial/ops
	  (list %stack-closure-ref %heap-closure-ref)))

(define (cleanup/letify bindings body)
  `(LET ,bindings ,body))

(define (cleanup/bindify lambda-list operands)
  (map (lambda (name operand) (list name operand))
       (lambda-list->names lambda-list)
       (lambda-list/applicate lambda-list operands)))

(define (cleanup/pseudo-letify rator bindings body)
  (define (default)
    (pseudo-letify rator bindings body cleanup/remember))
  (define (trivial last bindings)
    (beginnify (map* (list last) cadr bindings)))
  (cond ((memq *order-of-argument-evaluation* '(ANY LEFT-TO-RIGHT))
	 (default))
	((LOOKUP/? body)
	 (let* ((name  (lookup/name body))
		(place (assq name bindings)))
	   (if (not place)
	       (trivial body bindings)
	       (trivial
		(cadr place)
		(delq place bindings)))))
	((QUOTE/? body)
	 (trivial body bindings))
	(else
	 (default))))

(define (cleanup/rename renames token)
  (let loop ((bindings renames))
    (cond ((not (pair? bindings))
	   token)
	  ((eq? token (cleanup/binding/name (car bindings)))
	   (lookup/name (cleanup/binding/value (car bindings))))
	  (else
	   (loop (cdr bindings))))))

(define (cleanup/renamings env names)
  (map (lambda (name)
	 (let ((value (cleanup/env/lookup name env)))
	   ;; Do not rename if the shadowed binding is disappearing
	   (cond ((or (not value)
		      (QUOTE/? value))
		  (cleanup/binding/make name `(LOOKUP ,name)))
		 (else
		  (let ((renamed-form
			 `(LOOKUP ,(variable/rename name))))
		    (dbg-info/remember name renamed-form)
		    (cleanup/binding/make name renamed-form))))))
       names))

;; Environment is a list of frames.  Frames are a list of bindings.

(define (cleanup/env/find name env)
  (let frame-loop ((env env))
    (and (pair? env)
	 (let loop ((bindings (car env)))
	   (cond ((not (pair? bindings))
		  (frame-loop (cdr env)))
		 ((eq? name (cleanup/binding/name (car bindings)))
		  (car bindings))
		 (else
		  (loop (cdr bindings))))))))

(define (cleanup/env/lookup name env)
  (let ((binding  (cleanup/env/find name env)))
    (and binding
	 (cleanup/binding/value binding))))

(define (cleanup/env/initial)
  '())

(define (cleanup/env/extend env new-frame)
  (cons new-frame env))

;;(define-integrable (cleanup/binding/make name value) (cons name value))
;;(define-integrable (cleanup/binding/name binding)  (car binding))
;;(define-integrable (cleanup/binding/value binding) (cdr binding))

(define-integrable (cleanup/binding/make name value) (vector name value))
(define-integrable (cleanup/binding/name binding)  (vector-ref binding 0))
(define-integrable (cleanup/binding/value binding) (vector-ref binding 1))

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