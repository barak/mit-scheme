#| -*-Scheme-*-

$Id: cleanup.scm,v 1.8 1995/02/21 06:33:13 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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
  (cleanup/expr '() program))

(define-macro (define-cleanup-handler keyword bindings . body)
  (let ((proc-name (symbol-append 'CLEANUP/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	     (NAMED-LAMBDA (,proc-name ENV FORM)
	       (LET ((TRANSFORM-CODE (LAMBDA () ,code)))
		 (LET ((INFO (CLEANUP/GET-DBG-INFO ENV FORM)))
		   (LET ((CODE (TRANSFORM-CODE)))
		     (IF INFO
			 (CODE-REWRITE/REMEMBER* CODE INFO))
		     CODE))))))))))

(define-cleanup-handler LOOKUP (env name)
  (let ((place (assq name env)))
    (if (not place)
	(free-var-error name)
	(form/copy (cadr place)))))

(define-cleanup-handler LAMBDA (env lambda-list body)
  (let ((renames (cleanup/renamings env (lambda-list->names lambda-list))))
    `(LAMBDA ,(lmap (lambda (token)
		      (cleanup/rename renames token))
		    lambda-list)
       ,(cleanup/expr (append renames env) body))))

(define-cleanup-handler LETREC (env bindings body)
  (do-letrec-cleanup env bindings body))

(define (do-letrec-cleanup env bindings body)
  (let* ((renames (cleanup/renamings env (lmap car bindings)))
	 (env*  (append renames env))
	 (body* (cleanup/expr env* body)))
    (if (null? bindings)
	body*
	`(LETREC ,(lmap (lambda (binding)
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
  (let ((pred* (cleanup/expr env pred)))
    (define (default)
      `(IF ,pred* 
	   ,(cleanup/expr env conseq)
	   ,(cleanup/expr env alt)))
    (cond ((QUOTE/? pred*)
	   (case (boolean/discriminate (quote/text pred*))
	     ((FALSE)
	      (cleanup/expr env alt))
	     ((TRUE)
	      (cleanup/expr env conseq))
	     (else
	      (default))))
	  ((CALL/? pred*)
	   ;; (if (not p) c a) => (if p a c)
	   (let ((pred-rator (call/operator pred*)))
	     (if (and (QUOTE/? pred-rator)
		      (eq? (quote/text pred-rator) not)
		      (equal? (call/continuation pred*) `(QUOTE #F)))
		 `(IF ,(first (call/operands pred*))
		      ,(cleanup/expr env alt)
		      ,(cleanup/expr env conseq))
		 (default))))
	  (else
	   (default)))))

(define-cleanup-handler BEGIN (env #!rest actions)
  (beginnify (cleanup/expr* env actions)))

(define-cleanup-handler LET (env bindings body)
  (cleanup/let* cleanup/letify env bindings body))

(define-cleanup-handler CALL (env rator cont #!rest rands)
  (define (default)
    `(CALL ,(cleanup/expr env rator)
	   ,(cleanup/expr env cont)
	   ,@(cleanup/expr* env rands)))
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
	   (with-values
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
         (let ((lambda-list (lambda/formals rator))
               (lambda-body (lambda/body rator)))
           (define (generate env let-names let-values)
             (cleanup/let*
              (lambda (bindings* body*)
                (cleanup/pseudo-letify rator bindings* body*))
              env
              (cleanup/bindify let-names let-values)
              lambda-body))
	   #|(define (build-call-lambda/try1 new-cont-var body closure)	;
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
		    (new-env `((,old-cont-var (LOOKUP ,new-cont-var))
			       ,@env)))
	       (build-call-lambda/try2
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
		 (let ((lam-expr
			(cadr (assq cleanup/?lam-expr result)))
		       (rands
			(cadr (assq cleanup/?rands result)))
		       (cont
			(cadr (assq cleanup/?cont result))))
		   (cleanup/expr env
				 `(CALL ,lam-expr ,cont ,@rands)))))
	   (else
	    call*))))))


(define *cleanup/rewriters* (make-eq-hash-table))

(define (cleanup/rewrite? name arity)
  (cond ((hash-table/get *cleanup/rewriters* name #F)
	 => (lambda (alist)
	      (cond ((assq arity alist) => cdr)
		    (else  #F))))
	(else  #F)))

(define (define-cleanup-rewrite name arity handler)
  (let ((slot  (hash-table/get *cleanup/rewriters* name '())))
    (hash-table/put! *cleanup/rewriters*
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
		  (bind* (cons cont-name (lmap car bindings))
			 (cons cont (lmap cadr bindings))
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
  (let ((bindings* (lmap (lambda (binding)
			   (list (car binding)
				 (cleanup/expr env (cadr binding))))
			 bindings)))
    (call-with-values
     (lambda ()
       (list-split bindings*
		   (lambda (binding*)
		     (QUOTE/? (cadr binding*)))))
     (lambda (trivial non-trivial)
       (call-with-values
	(lambda ()
	  (list-split non-trivial
		      (lambda (binding*)
			(cleanup/easy? (cadr binding*)))))
	(lambda (easy non-easy)
	  (let* ((possibly-captured
		  (lmap (lambda (binding)
			  (cleanup/easy/name (cadr binding)))
			easy))
		 (complex-triplets
		  ;; (original-name renamed-version value-expression)
		  (lmap (lambda (binding)
			  (let ((name (car binding)))
			    (list name
				  (if (memq name possibly-captured)
				      (variable/rename name)
				      name)
				  (cadr binding))))
			non-easy))
		 (body*
		  (cleanup/expr
		   (append trivial
			   easy
			   (lmap (lambda (triplet)
				   (list (car triplet)
					 `(LOOKUP ,(cadr triplet))))
				 complex-triplets)
			   env)
		   body)))
	    (if (null? complex-triplets)
		body*
		(letify (lmap cdr complex-triplets)
			body*)))))))))

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

(define cleanup/trivial/ops
  (list %vector-index))

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
  (let ((place (assq token renames)))
    (if (not place)
	token
	(cadr (cadr place)))))

(define (cleanup/renamings env names)
  (lmap (lambda (name)
	  (let ((place (assq name env)))
	    ;; Do not rename if the shadowed binding is disappearing
	    (if (or (not place)
		    (QUOTE/? (cadr place)))
		`(,name (LOOKUP ,name))
		`(,name (LOOKUP ,(variable/rename name))))))
	names))

(define (cleanup/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (cleanup/quote env expr))
    ((LOOKUP)   (cleanup/lookup env expr))
    ((LAMBDA)   (cleanup/lambda env expr))
    ((LET)      (cleanup/let env expr))
    ((DECLARE)  (cleanup/declare env expr))
    ((CALL)     (cleanup/call env expr))
    ((BEGIN)    (cleanup/begin env expr))
    ((IF)       (cleanup/if env expr))
    ((LETREC)   (cleanup/letrec env expr))
    (else
     (illegal expr))))

(define (cleanup/expr* env exprs)
  (lmap (lambda (expr)
	  (cleanup/expr env expr))
	exprs))

(define (cleanup/remember new old)
  (code-rewrite/remember new old))

(define (cleanup/get-dbg-info env expr)
  (cond ((code-rewrite/original-form/previous expr)
         => (lambda (dbg-info)
              ;; Copy the dbg info, rewriting the expressions
              (let* ((block     (new-dbg-form/block dbg-info))
                     (block*    (new-dbg-block/copy-transforming
                                 (lambda (expr)
                                   (cleanup/copy-dbg-kmp expr env))
                                 block))
                     (dbg-info* (new-dbg-form/new-block dbg-info block*)))
                dbg-info*)))
        (else #F)))


(define (cleanup/copy-dbg-kmp expr env)
  (form/copy-transforming
   (lambda (form copy uninteresting)
     copy
     (cond ((and (LOOKUP/? form)
		 (assq (lookup/name form) env))
	    => (lambda (place)
		 (form/copy (cadr place))))
	   (else
	    (uninteresting form))))
   expr))
