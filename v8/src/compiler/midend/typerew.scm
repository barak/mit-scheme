#| -*-Scheme-*-

$Id: typerew.scm,v 1.3 1995/09/03 17:15:04 adams Exp $

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

;;;; Type analysis and rewriting
;;; package: (compiler midend)

(declare (usual-integrations))

(define *typerew-type-map*)		; form->type

;; Sometime it is convienient ot decide an operator rewrite at type
;; analysis time:
(define *typerew-suggestions-map*)	; form->rewrite

(define *typerew-dbg-map*)

(define (typerew/top-level program)
  (let  ((program* (copier/top-level program code-rewrite/remember)))
    (kmp/ppp program*)
    (fluid-let ((*typerew-type-map*
		 (make-monotonic-strong-eq-hash-table))
		(*typerew-suggestions-map*
		 (make-monotonic-strong-eq-hash-table))
		(*typerew-dbg-map*
		 (make-monotonic-strong-eq-hash-table)))
      (typerew/expr program* q-env:top
		    (lambda (q t e)
		      (bkpt "PROGRAM* has been analysed")
		      (typerew/rewrite! program*)
		      program*)))))

(define-macro (define-type-rewriter keyword bindings . body)
  (let ((proc-name (symbol-append 'TYPEREW/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name FORM ENV RECEIVER)
	     ;; FORM, ENV and RECEIVER are in scope in handler
	     FORM
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
	       ,code)))))))

(define (typerew/associate-type form type)
  (monotonic-strong-eq-hash-table/put! *typerew-type-map* form type))

(define (typerew/type form)
  (or (monotonic-strong-eq-hash-table/get *typerew-type-map* form #F)
      (internal-error "No type associated with form" form)))

(define (typerew/suggest-rewrite form rewrite)
  (monotonic-strong-eq-hash-table/put! *typerew-suggestions-map* form rewrite))

;; This is incorrect in the following conservative way: QUANTITY may
;; already be bound in ENV to a type that would restrict TYPE.
(define-integrable (typerew/send receiver quantity type env)
  (receiver quantity type (q-env:glb/1 env quantity type)))

;; Do we really have to do an O(n) lookup?
(define (typerew/send receiver quantity type env)
  (let ((env* (q-env:glb/1 env quantity type)))
    (receiver quantity (q-env:lookup env* quantity) env*)))

(define-type-rewriter LOOKUP (name)
  (let ((quantity (quantity:variable name)))
    (receiver quantity (q-env:lookup env quantity) env)))

(define-type-rewriter LAMBDA (lambda-list body)
  ;; . Simple analysis: we assume that this procedure escapes and is called
  ;;   after someone has played with every mutable structure in the universe.
  ;; . The names in the lambda-list are unknown so we dont have
  ;;   to add them to the quantity environment.
  ;; . It is a shame to waste the returned information: it tells us the
  ;;   return type and constraints imposed on the arguments, and even if the
  ;;   procedure returns at all.
  (typerew/expr
   body
   (q-env:restrict env effect:unknown)
   (lambda (quantity type env*)
     quantity type env*			; a shame
     ;; Creating the closure itself is no big deal since we dont have
     ;; reasonable type information for procedures.
     (typerew/send receiver
		   (quantity:other-expression form effect:none)
		   type:compiled-entry
		   env))))

(define-type-rewriter CALL (rator cont #!rest rands)
  (define (default)
    (typerew/expr*/unordered
     (cdr form) env
     (lambda (quantities types envs env*)
       quantities types envs		; we could use these for something
       ;; Assume that the procedure wrecks everything
       (receiver (quantity:other-expression form effect:unknown)
		 type:any		; uninteresting => no SEND
		 (q-env:restrict env* effect:unknown)))))

  (define (apply-method method rands*)
    (typerew/expr*/unordered
     rands* env
     (lambda (quantities types envs env*)
       envs				; ignored
       (method quantities types env* form receiver))))
  
  (cond ((LAMBDA/? rator)
	 (let ((formals (lambda/formals rator)))
	   (if (or (hairy-lambda-list? formals)
		   (not (= (length (cdr formals)) (length rands))))
	       (default)
	       (typerew/bind (cdr formals) rands env receiver
			     (lambda/body rator)))))
	((not (QUOTE/? rator))
	 (default))
	((typerew/type-method? (quote/text rator) (length rands))
	 => (lambda (method)
	      (apply-method method rands)))
	((and (eq? (quote/text rator) %invoke-remote-cache)
	      (typerew/type-method? (first (quote/text (first rands)))
				    (second (quote/text (first rands)))))
	 => (lambda (method)
	      (apply-method method (cddr rands))))
	(else (default))))

(define-type-rewriter LET (bindings body)
  (typerew/bind (map first bindings) (map second bindings) env receiver body))

(define (typerew/bind names exprs env receiver body)
  (cond ((null? names)  (typerew/expr body env receiver))
	((null? (cdr exprs))
	 (typerew/expr
	  (first exprs) env
	  (lambda (quantity type env*)
	    (typerew/expr
	     body
	     (q-env:glb/1 env* (quantity:variable (car names)) type)
	     receiver))))
	(else				; lots of arguments in some order
	 (typerew/expr*/unordered
	  exprs env
	  (lambda (quantities types envs env*)
	    envs			; ignored
	    (typerew/expr body
			  (q-env:bind* env* names quantities types)
			  receiver))))))


(define-type-rewriter LETREC (bindings body)
  ;; This is lame. We need more complex procedure types to summarize what
  ;; we found out about the procedures, and an intelligent traversal
  ;; order to maximize the info (or some kind of iterative solution).
  (let ((env*
	 (q-env:glb env
		    (map (lambda (binding)
			   (cons (quantity:variable (first binding))
				 type:compiled-entry))
			 bindings))))
    (let loop ((bindings bindings)
	       (env** env*))
      (if (null? bindings)
	  (typerew/expr body env** receiver)
	  (typerew/expr (second (car bindings))
			env**
			(lambda (quantity type env***)
			  (loop (cdr bindings)
				(q-env:glb/1 env*** quantity type))))))))

(define-type-rewriter QUOTE (object)
  (receiver (quantity:constant form)  (type:of-object object)  env))

(define-type-rewriter DECLARE (#!rest anything)
  (receiver (quantity:other-expression form effect:none)  type:any  env))

(define-type-rewriter BEGIN (#!rest actions)
  (if (null? actions)
      (receiver (quantity:other-expression form effect:none) type:any env)
      (let loop ((actions actions) (env env))
	(if (null? (cdr actions))
	    (typerew/expr (car actions) env receiver)
	    (typerew/expr
	     (car actions) env
	     (lambda (quantity type env*)
	       quantity type ; ignored
	       (loop (cdr actions) env*)))))))
  
(define-type-rewriter IF (pred conseq alt)
  (typerew/pred
   pred env
   (lambda (env_t env_f)
     ;;(pp `(env_t: ,env_t env_f: ,env_f))
     (typerew/expr
      conseq env_t
      (lambda (quantity_t type_t env_t*)
	(typerew/expr
	 alt env_f
	 (lambda (quantity_f type_f env_f*)
	   ;;(pp `(type_t: ,type_t  type_f: ,type_f))
	   ;;(pp `(env_t*: ,env_t*  env_f*: ,env_f*))
	   (typerew/send receiver
			 (quantity:combination/2/assoc 'IF-MERGE
						       quantity_t quantity_f)
			 (type:or
			  (if (q-env:bottom? env_t*) type:empty type_t)
			  (if (q-env:bottom? env_f*) type:empty type_f))
			 (q-env:lub env_t* env_f*)))))))))

(define (typerew/expr*/left-to-right exprs env receiver)
  ;; receiver = (lambda (quantities types env) ...)
  (typerew/expr*/%ordered exprs env
			  (lambda (Qs Ts env*)
			    (receiver (reverse! Qs) (reverse! Ts) env*))))

(define (typerew/expr*/right-to-left exprs env receiver)
  ;; receiver = (lambda (quantities types env) ...)
  (typerew/expr*/%ordered (reverse exprs) env receiver))

(define (typerew/expr*/%ordered exprs env receiver)
  ;; receiver = (lambda (quantities types env) ...)
  ;; Note: Yields quantities and types in reversed order
  (let loop ((Qs '()) (Ts '()) (env env) (exprs exprs))
    (if (not (pair? exprs))
	(receiver Qs Ts env)
	(typerew/expr (car exprs)
		      env
		      (lambda (Q T env*)
			(loop (cons Q Qs) (cons T Ts) env* (cdr exprs)))))))

(define (typerew/expr*/unordered exprs env receiver)
  ;; receiver = (lambda (quantities types envs env) ...)
  ;; . ENVS are returned because they can give hints on how subexpressions
  ;;   should be ordered.
  ;; . Try every permutation! you must be joking.
  ;; . An approximation is to evaluate each expression in an environment
  ;;   containing all the deleterious and none of the beneficial effects of
  ;;   the other expressions.  This is the worst that the other
  ;;   expressions could do if they were ordered before this
  ;;   expression.  The output environment must then have all the
  ;;   deleterious effects of the other expressions applied (thus
  ;;   modelling their evaluation after the current expression).  The
  ;;   result is then the GLB of the expression results.
  ;; . An approximation to the approximation is punt if any expression has
  ;;   side-effects.

  (let ((split-env
	 (if (for-all? exprs form/simple&side-effect-free?) ;exponential!
	     env
	     (q-env:restrict env effect:unknown))))
    (define (glb* envs)
      ;; (reduce q-env:glb q-env:top envs)
      ;; Hopefully most envs are the same as passed in (lookups & quotes)
      (call-with-values
	  (lambda ()
	    (list-split envs (lambda (env) (eq? env split-env))))
	(lambda (splits others)
	  (if (and (null? splits) (pair? others))
	      (fold-left q-env:glb (car others) (cdr others))
	      (fold-left q-env:glb split-env others)))))
    (let loop ((Qs '()) (Ts '()) (Es '()) (exprs exprs))
      (if (not (pair? exprs))
	  (receiver (reverse! Qs) (reverse! Ts) (reverse! Es) (glb* Es))
	  (typerew/expr (car exprs)
			split-env
			(lambda (Q T env*)
			  (loop (cons Q Qs) (cons T Ts) (cons env* Es)
				(cdr exprs))))))))

(define (typerew/remember new old)
  (code-rewrite/remember new old))

(define (typerew/remember* new old)
  (code-rewrite/remember new old))

(define (typerew/new-name prefix)
  (new-variable prefix))

(define (typerew/type-checks? class)
  (and compiler:generate-type-checks?
       (if (pair? compiler:generate-type-checks?)
	   (memq class compiler:generate-type-checks?)
	   #T)))

(define (typerew/range-checks? class)
  (and compiler:generate-range-checks?
       (if (pair? compiler:generate-range-checks?)
	   (memq class compiler:generate-range-checks?)
	   #T)))

;; Quantities
;;
;; Quantities are represented as vectors:
;;   #(<hash> <effects> <variable>)
;;   #(<hash> <effects> <quoted-form>)
;;   #(<hash> <effects> <operator> . <operand-quantities>)
;; <effects> is the effects to which this quantity is sensitive

(define-integrable (quantity:hash Q)
  (vector-ref Q 0))

(define-integrable (quantity:effects Q)
  (vector-ref Q 1))

(define-integrable (quantity:operator Q)
  (vector-ref Q 2))

(define-integrable (quantity:operand1 Q)
  (vector-ref Q 3))

(define (quantity:constant quoted-form)
  (vector (quantity:hash-constant (quote/text quoted-form))
	  effect:none
	  quoted-form))

(define (quantity:variable name)
  (vector (quantity:hash-symbol name) effect:none name))

(define (quantity:combination/1 operator operand)
  (vector (quantity:hash+ (quantity:hash-operator operator)
			  (quantity:hash operand))
	  (effect:union (operator-sensitive-effects operator)
			(quantity:effects operand))
	  operator
	  operand))

(define (quantity:combination/2 operator operand1 operand2)
  (vector (quantity:hash+ (quantity:hash-operator operator)
			  (quantity:hash+ (quantity:hash operand1)
					  (quantity:hash operand2)))
	  (effect:union (operator-sensitive-effects operator)
			(effect:union (quantity:effects operand1)
				      (quantity:effects operand2)))
	  operator
	  operand1
	  operand2))

(define (quantity:combination/2/assoc operator operand1 operand2)
  (if (fix:<= (quantity:hash operand1) (quantity:hash operand2))
      (quantity:combination/2 operator operand1 operand2)
      (quantity:combination/2 operator operand2 operand1)))

(define (quantity:combination operator operands)
  (define (default)
    (list->vector
     (cons*
      (fold-left (lambda (hash q) (quantity:hash+ hash (quantity:hash q)))
		 (quantity:hash-operator operator)
		 operands)
      (fold-left (lambda (eff q) (effect:union eff (quantity:effects q)))
		 (operator-sensitive-effects operator)
		 operands)
      operator
      operands)))
  (cond ((not (pair? operands)) (default))
	((not (pair? (cdr operands)))
	 (quantity:combination/1 operator (first operands)))
	((not (pair? (cddr operands)))
	 (quantity:combination/2 operator (first operands) (second operands)))
	(else (default))))

(define (quantity:other-expression source effects)
  (vector 0 effects source))

(define (quantity:same? q1 q2)
  (let same? ((q1 q1) (q2 q2))
    (or (eq? q1 q2)
	(and (vector? q1)
	     (vector? q2)
	     (fix:= (quantity:hash q1) (quantity:hash q2))
	     (= (vector-length q1) (vector-length q2))
	     (let loop ((i (- (vector-length q1) 1)))
	       (or (fix:< i 2)
		   (and (same? (vector-ref q1 i) (vector-ref q2 i))
			(loop (fix:- i 1)))))))))

(define (quantity:hash-symbol sym)
  (let* ((s  (symbol-name sym))
	 (c1 (vector-8b-ref s 0))
	 (c2 (vector-8b-ref s (- (string-length s) 1))))
    (+ c1 (* 17 c2))))

(define (quantity:hash-constant value)
  (cond ((= 0 (object-gc-type value))
	 (fix:and #xFFF (object-datum value)))
	((flo:flonum? value) 1)
	(else            (object-type value))))
    
(define-integrable (quantity:hash+ q1 q2)
  (let ((q1* (fix:* q1 7))
	(q2* (fix:* q2 13)))
    (fix:and #xFFFF (fix:+ (fix:+ q1* (fix:lsh -13 q1))
			   (fix:+ q2* (fix:lsh -12 q2))))))
  
(define quantity:hash-operator
  (let ((table (make-monotonic-strong-eq-hash-table))
	(last  0))
    (lambda (operator)
      (or (monotonic-strong-eq-hash-table/get table operator #F)
	  (let ((value  (quantity:hash+ last 10000)))
	    (monotonic-strong-eq-hash-table/put! table operator value)
	    value)))))

;; Quantity environments map quantities to types
;;
;; Quantity type lattice
;;
;; . bottom: everything is known to be of type:empty (this means that
;;   i.e. the program never gets here)
;; . (): Top. nothing is known, i.e. every quantity may be of any type
;; . alist(quantity*type): listed quantities of of knwon type, others 

(define (q-env:lookup env quantity)	; -> a type
  (cond ((q-env:bottom? env)
	 type:empty)
	((%q-env:lookup env quantity (quantity:hash quantity))
	 => cdr)
	(else type:any)))

(define (%q-env:lookup env Q H) ; -> #F or the association
  (let loop ((env env))
    (cond ((not (pair? env))
	   #F)
	  ((fix:> (quantity:hash (caar env)) H)
	   #F)
	  ((quantity:same? Q (caar env))
	   (car env))
	  (else (loop (cdr env))))))

(define (%q-env:delete env Q H)
  (let loop ((env env))
    (cond ((not (pair? env))
	   '())
	  ((fix:> (quantity:hash (caar env)) H)
	   env)
	  ((quantity:same? Q (caar env))
	   (cdr env))
	  (else (cons (car env) (loop (cdr env)))))))

(define (q-env:restrict env effects)
  ;; Remove quantities depending on EFFECTS.
  ;;  Computes the LUB of ENV and the environment containing all possible
  ;;  quantities dependent on EFFECTS mapped to type:any and all other
  ;;  possible quantities mapped to type:none.
  (cond ((q-env:bottom? env)
	 env)  ;; justified only because it implies dead code
	((effect:none? effects)
	 env)
	(else
	 (list-transform-positive env
	   (lambda (quantity.type)
	     (effect:disjoint? (quantity:effects (car quantity.type))
			       effects))))))

(define q-env:top    '())
(define q-env:bottom 'bottom)

(define (q-env:bottom? env)
  (eq? q-env:bottom env))

(define (q-env:top? env)
  (null? env))

(define (q-env:lub env1 env2)
  (define (merge env1 env2)
    (define (skip1) (merge (cdr env1) env2))
    (if (and (pair? env1) (pair? env2))
	(let ((q1 (caar env1))
	      (q2 (caar env2)))
	  (let ((h1 (quantity:hash q1))
		(h2 (quantity:hash q2)))
	    (cond ((fix:< h2 h1)	 (merge env1 (cdr env2)))
		  ((fix:< h1 h2)	 (skip1))
		  ((%q-env:lookup env2 q1 h1)
		   => (lambda (q2.type2)
			(let ((type* (type:or (cdar env1) (cdr q2.type2))))
			  (if (type:subset? type:any type*) ; useless
			      (skip1)
			      (cons (cons q1 type*) (skip1))))))
		  (else (skip1)))))
	'()))
  (cond ((q-env:bottom? env1) env2)
	((q-env:bottom? env2) env1)
	(else (merge env1 env2))))

(define (q-env:glb/1 env quantity type)
  (let ((op (quantity:operator quantity)))
    (if (quote/? op)
	(if (type:disjoint? (type:of-object (quote/text op)) type)
	    q-env:bottom;; we have just concluded a constant an absurd  value
	    env)
	(q-env:glb env (list (cons quantity type))))))


#| env2 must be sorted
(define (q-env:glb* env quantities types asserted-types)
  (q-env:glb env
	     (map (lambda (q type a-type)
		    (cons q (type:and a-type type)))
		  quantities
		  types
		  asserted-types)))
|#

(define (q-env:glb* env quantities types asserted-types)
  (let loop ((env2 q-env:top) (Qs quantities) (Ts types) (As asserted-types))
    (if (null? Qs)
	(q-env:glb env env2)
	(loop (q-env:glb/1 env2 (car Qs) (type:and (car Ts) (car As)))
	      (cdr Qs)
	      (cdr Ts)
	      (cdr As)))))

(define (q-env:glb env1 env2)
  (define (merge env1 env2 accepted)
    (define (accept1) (merge (cdr env1) env2 (cons (car env1) accepted)))
    (define (accept2) (merge env1 (cdr env2) (cons (car env2) accepted)))
    (cond ((null? env1) (append! (reverse! accepted) env2))
	  ((null? env2) (append! (reverse! accepted) env1))
	  (else				;(and (pair? env1) (pair? env2))
	   (let ((q1 (caar env1))
		 (q2 (caar env2)))
	     (let ((h1 (quantity:hash q1))
		   (h2 (quantity:hash q2)))
	       (cond ((fix:< h1 h2)         (accept1))
		     ((fix:< h2 h1)	 (accept2))
		     ((%q-env:lookup env2 q1 h1)
		      => (lambda (q2.type2)
			   (let ((type* (type:and (cdar env1) (cdr q2.type2))))
			     (if (type:subset? type* type:empty)
				 q-env:bottom
				 (merge (cdr env1) 
					(%q-env:delete env2 q1 h1)
					(cons (cons q1 type*) accepted))))))
		     (else (accept1))))))))
  (cond ((q-env:bottom? env1) env1)
	((q-env:bottom? env2) env2)
	(else (merge env1 env2 '()))))

(define (q-env:bind* env names quantities types)
  ;; introduce new names into the environment
  (if (q-env:bottom? env)
      env
      (q-env:glb env
		 (map (lambda (name quantity type)
			quantity	; we dont know how to chain names yet
			(cons (quantity:variable name) type))
		      names
		      quantities
		      types))))

(define (typerew/pred form env receiver)
  ;; receiver = (lambda (env_t env_f) ...)
  (define (->expr)
    (typerew/expr
     form env
     (lambda (quantity type env*)
       (receiver (q-env:glb/1 env* quantity type:not-false)
		 (q-env:glb/1 env* quantity type:false)))))
  (cond ((and (CALL/? form)
	      (QUOTE (call/operator form))
	      (operator-predicate-test-type (quote/text (call/operator form))))
	 => (lambda (test-types)
	      (typerew/expr
	       form env
	       (lambda (quantity type env*)
		 ;;(pp `(predicate-q ,quantity))
		 (let ((arg-quantity (quantity:operand1 quantity))
		       (env*_t (q-env:glb/1 env* quantity type:not-false))
		       (env*_f (q-env:glb/1 env* quantity type:false)))
		   ;;(pp `(env*_t: ,env*_t env*_f: ,env*_f))
		   (receiver 
		    (q-env:glb/1 env*_t arg-quantity (car test-types))
		    (q-env:glb/1 env*_f arg-quantity (cdr test-types))))))))
	((IF/? form)
	 (typerew/pred
	  (if/predicate form) env
	  (lambda (env_t env_f)
	    (typerew/pred
	     (if/consequent form) env_t
	     (lambda (env_tt env_tf)
	       (typerew/pred
		(if/alternate form) env_f
		(lambda (env_ft env_ff)
		  (receiver (q-env:lub env_tt env_ft)
			    (q-env:lub env_ff env_tf)))))))))
	(else (->expr))))


(define (typerew/expr form env receiver)
  ;; receiver = (lambda (quantity type env*) ...)
  (if (not (pair? form))
      (illegal form))
  (define (receiver* quantity type env*)
     (typerew/associate-type form type)
     (monotonic-strong-eq-hash-table/put! *typerew-dbg-map* form
					  (list quantity type env*))
     (receiver quantity type env*))
  (case (car form)
    ((QUOTE)    (typerew/quote  form env receiver*))
    ((LOOKUP)   (typerew/lookup form env receiver*))
    ((LAMBDA)   (typerew/lambda form env receiver*))
    ((LET)      (typerew/let form env receiver*))
    ((DECLARE)  (typerew/declare form env receiver*))
    ((CALL)     (typerew/call form env receiver*))
    ((BEGIN)    (typerew/begin form env receiver*))
    ((IF)       (typerew/if form env receiver*))
    ((LETREC)   (typerew/letrec form env receiver*))
    (else
     (illegal form))))


(define (typerew/rewrite! form)

  (define (rewrite-bindings! bindings)
    (for-each (lambda (binding) (rewrite! (second binding)))
      bindings))

  (define (rewrite!* forms)
    (for-each rewrite! forms))

  (define (rewrite-call! form rator cont rands)
    (define (apply-method method rands*)
      (cond ((null? rands*)         (method form))
	    ((null? (cdr rands*))   (method form (car rands*)))
	    ((null? (cddr rands*))  (method form (car rands*) (cadr rands*)))
	    ((null? (cdddr rands*))
	     (method form (car rands*) (cadr rands*) (caddr rands*)))
	    (else (apply method form rands*))))
    (define (apply-suggestion suggestion)
      (suggestion form))
    (rewrite!* rands)
    (rewrite! cont)
    (cond ((not (QUOTE/? rator))
	   (rewrite! rator))
	  ((monotonic-strong-eq-hash-table/get *typerew-suggestions-map*
					       form #F)
	   => apply-suggestion)
	  ((typerew/rewrite-method? (quote/text rator) (length rands))
	   => (lambda (method)
		(apply-method method rands)))
	  ((and (eq? (quote/text rator) %invoke-remote-cache)
		(typerew/type-method? (first (quote/text (first rands)))
				      (second (quote/text (first rands)))))
	   => (lambda (method)
		(apply-method method (cddr rands))))
	  (else (rewrite! rator))))

  (define (rewrite! form)
    (cond ((QUOTE/? form))
	  ((LOOKUP/? form))
	  ((CALL/? form)
	   (rewrite-call! form
			  (call/operator form)
			  (call/continuation form)
			  (call/operands form)))
	  ((IF/? form)
	   (rewrite! (if/predicate form))
	   (rewrite! (if/consequent form))
	   (rewrite! (if/alternative form)))
	  ((BEGIN/? form)
	   (rewrite!* (begin/exprs form)))
	  ((LET/? form)
	   (rewrite-bindings! (let/bindings form))
	   (rewrite! (let/body form)))
	  ((LETREC/? form)
	   (rewrite-bindings! (letrec/bindings form))
	   (rewrite! (letrec/body form)))
	  ((LAMBDA/? form)
	   (rewrite! (lambda/body form)))
	  ((DECLARE/? form))
	  (else (illegal form))))

  (rewrite! form))

(define *typerew/type-methods*    (make-monotonic-strong-eq-hash-table))
(define *typerew/rewrite-methods* (make-monotonic-strong-eq-hash-table))

(define (typerew/type-method? op arity)
  (let ((arity.method
	 (monotonic-strong-eq-hash-table/get *typerew/type-methods* op #F)))
    (and arity.method
	 (if (car arity.method)		; specific arity only
	     (and (= (car arity.method) arity)
		  (cdr arity.method))
	     (cdr arity.method)))))	; #F => any arity

(define (define-typerew-type-method op arity method)
  ;; ARITY = #F means method for any arity
  (monotonic-strong-eq-hash-table/put! *typerew/type-methods* op
				       (cons arity method)))

(define (typerew/rewrite-method? op arity)
  (let ((arity.method
	 (monotonic-strong-eq-hash-table/get *typerew/rewrite-methods* op #F)))
    (and arity.method
	 (if (car arity.method)		; specific arity only
	     (and (= (car arity.method) arity)
		  (cdr arity.method))
	     (cdr arity.method)))))	; #F => any arity

(define (define-typerew-rewrite-method op arity method)
  ;; ARITY = #F means method for any arity
  (monotonic-strong-eq-hash-table/put! *typerew/rewrite-methods* op
				       (cons arity method)))

;; Operator replacement strategies

(define (typerew-simple-operator-replacement new-op)
  ;; Coerces operator to a replacement procedure
  (if (and (procedure? new-op) (not (primitive-procedure? new-op)))
      new-op
      (lambda (form)
	(pp `(operator-replacement ,new-op ,form))
	(form/rewrite! form
	  `(CALL (QUOTE ,new-op) ,@(cddr form))))))

(define (typerew-object-type-test typecode)
  (let ((OBJECT-TYPE?  (make-primitive-procedure 'OBJECT-TYPE?)))
    (lambda (expr)
      `(CALL ',OBJECT-TYPE? '#F  (QUOTE ,typecode)  ,expr))))

(define (typerew/->unary-expression make-expression)
  (if (and (procedure? make-expression)
	   (not (primitive-procedure? make-expression)))
      make-expression
      (lambda (arg1)
	`(CALL (QUOTE ,make-expression) '#F ,arg1))))

(define (typerew-operator-replacement/diamond-1-1-1 test good-op bad-op)
  (let ((test (typerew/->unary-expression test)))
    (lambda (form)
      (pp `(operator-replacement/check (,test ,good-op ,bad-op) ,form))
      (form/rewrite! form
	(let ((name (typerew/new-name 'OBJECT)))
	  (bind name (call/operand1 form)
		`(IF ,(test `(LOOKUP ,name))
		     (CALL ',good-op '#F (LOOKUP ,name))
		     (CALL ',bad-op  '#F (LOOKUP ,name)))))))))

(define (typerew-operator-replacement/diamond-2-1-1 test arg good-op bad-op)
  (lambda (form)
    (pp `(operator-replacement/check (,test ,good-op ,bad-op) ,form))
    (form/rewrite! form
      (let ((name (typerew/new-name 'OBJECT)))
	(bind name (call/operand1 form)
	      `(IF (CALL ',test    '#F ,arg (LOOKUP ,name))
		   (CALL ',good-op '#F (LOOKUP ,name))
		   (CALL ',bad-op  '#F (LOOKUP ,name))))))))

(define (typerew-operator-replacement/diamond-1-2-2 test good-op bad-op)
  (lambda (form)
    (pp `(operator-replacement (,test ,good-op ,bad-op) ,form))
    (form/rewrite! form
      (let ((object (typerew/new-name 'OBJECT))
	    (value  (typerew/new-name 'VALUE)))
	(bind* (list object value)
	       (list (call/operand1 form) (call/operand2 form))
	       `(IF (CALL ',test    '#F (LOOKUP ,object))
		    (CALL ',good-op '#F (LOOKUP ,object) (LOOKUP ,value))
		    (CALL ',bad-op  '#F (LOOKUP ,object) (LOOKUP ,value))))))))

(define (typerew-operator-replacement/diamond-2-2-2 test good-op bad-op)
  (lambda (form)
    (pp `(operator-replacement (,test ,good-op ,bad-op) ,form))
    (form/rewrite! form
      (let ((object (typerew/new-name 'OBJECT))
	    (index  (typerew/new-name 'INDEX)))
	(bind* (list object index)
	       (list (call/operand1 form) (call/operand2 form))
	       `(IF (CALL ',test    '#F (LOOKUP ,object) (LOOKUP ,index))
		    (CALL ',good-op '#F (LOOKUP ,object) (LOOKUP ,index))
		    (CALL ',bad-op  '#F (LOOKUP ,object) (LOOKUP ,index))))))))

(define (typerew-operator-replacement/diamond-2-3-3 test good-op bad-op)
  (define (rewrite)
    (let ((obj (typerew/new-name 'OBJECT))
	  (idx (typerew/new-name 'INDEX))
	  (elt (typerew/new-name 'ELEMENT)))
      (bind*
       (list obj idx elt)
       (list (call/operand1 form) (call/operand2 form) (call/operand3 form))
       `(IF (CALL ',test    '#F (LOOKUP ,obj) (LOOKUP ,idx))
	    (CALL ',good-op '#F (LOOKUP ,obj) (LOOKUP ,idx) (LOOKUP ,elt))
	    (CALL ',bad-op  '#F (LOOKUP ,obj) (LOOKUP ,idx) (LOOKUP ,elt))))))
  (lambda (form)
    (pp `(operator-replacement (,test ,good-op ,bad-op) ,form))
    (form/rewrite! form (rewrite))))

(define (typerew/general-type-method rator
				     asserted-argument-types
				     result-type
				     effects-performed)
  (lambda (quantities types env form receiver)
    form				; No operator replacement
    (let ((env* (q-env:restrict
		 (q-env:glb* env quantities types asserted-argument-types)
		 effects-performed)))
      (typerew/send receiver
		    (quantity:combination rator quantities)
		    result-type
		    env*))))

;; Example: substring?

(define-typerew-type-method 'SUBSTRING? 2
  (typerew/general-type-method 'SUBSTRING?
			       (list type:string type:string)
			       type:boolean
			       effect:none))

(define (typerew-binary-variants-type-method rator effect . spec)
  ;; spec: repeated (input-type1 input-type2 output-type rewrite)
  ;;  Final spec is the asserted-type1 asserted-type2 default-output-type
  (define (result receiver result-type q1 q2 env)
    (typerew/send receiver
		  (quantity:combination/2 rator q1 q2)
		  result-type
		  env))
  (define (compile-spec spec)
    (let* ((a1 (first spec))
	   (a2 (second spec)) 
	   (result-type (third spec))
	   (rewrite     (fourth spec))
	   (rewrite (and rewrite
			 (typerew-simple-operator-replacement rewrite))))

      (if (null? (cddddr spec)) ; final row of table
	  (lambda (t1 t2 q1 q2 env form receiver)
	    (if rewrite (typerew/suggest-rewrite form rewrite))
	    (result receiver result-type q1 q2
		    (q-env:restrict
		     (q-env:glb/1 (q-env:glb/1 env q1 (type:and t1 a1))
				  q2 (type:and t2 a2))
		     effect)))
	  (let ((after-tests (compile-spec (cddddr spec))))
	    (lambda (t1 t2 q1 q2 env form receiver)
	      (if (and (type:subset? t1 a1) (type:subset? t2 a2))
		  (begin
		    (if rewrite (typerew/suggest-rewrite form rewrite))
		    (result receiver result-type q1 q2 env))
		  (after-tests t1 t2 q1 q2 env form receiver)))))))
  (let ((compiled-spec  (compile-spec spec)))
    (lambda (quantities types env form receiver)
      (compiled-spec (first types)      (second types)
		     (first quantities) (second quantities)
		     env form receiver))))

(define (typerew-unary-variants-type-method rator effect . spec)
  ;; spec: repeated (input-type output-type rewriter)
  ;;       followed by asserted-type default-output-type
  (lambda (quantities types env form receiver)
    (let ((quantity  (car quantities))
	  (type      (car types)))
      
      (define (result env result-type)
	(typerew/send receiver
		      (quantity:combination/1 rator quantity)
		      result-type
		      env))

      (let loop ((spec spec))
	(cond ((null? (cddr spec))
	       (result
		(q-env:restrict
		 (q-env:glb/1 env quantity (type:and type (first spec)))
		 effect)
		(second spec)))
	      ((type:subset? type (car spec))
	       (if (caddr spec) ((caddr spec) form type))
	       (result env (cadr spec)))
	      (else (loop (cdddr spec))))))))

(define (define-typerew-unary-variants-type-method name . spec)
  (define-typerew-type-method name 1
    (apply typerew-unary-variants-type-method name spec)))

(define (define-typerew-binary-variants-type-method name . spec)
  (define-typerew-type-method name 2
    (apply typerew-binary-variants-type-method name spec)))

(define-typerew-unary-variants-type-method 'EXACT->INEXACT  effect:none
  type:real    type:inexact-real    #F
  type:recnum  type:inexact-recnum  #F
  type:number  type:number)

(define-typerew-unary-variants-type-method 'COS    effect:none
  type:exact-zero type:exact-one     #F
  type:real       type:flonum        #F
  type:number     type:number)
				 
(define-typerew-unary-variants-type-method 'SIN    effect:none
  type:exact-zero type:exact-zero    #F
  type:real       type:flonum        #F
  type:number     type:number)
				 
(define-typerew-unary-variants-type-method 'TAN    effect:none
  type:exact-zero type:exact-zero    #F
  type:real       type:flonum        #F
  type:number     type:number)
				 
(define-typerew-unary-variants-type-method 'ACOS   effect:none
  type:exact-one  type:exact-zero    #F
  type:number     type:inexact-number)
				 
(define-typerew-unary-variants-type-method 'ASIN   effect:none
  type:exact-zero type:exact-zero    #F
  type:number     type:inexact-number)
				 
(define-typerew-unary-variants-type-method 'EXP    effect:none
  type:recnum     type:inexact-recnum #F
  type:exact-zero type:exact-one      #F
  type:real       type:inexact-real   #F
  type:number     type:inexact-number)
				 
(define-typerew-unary-variants-type-method 'LOG    effect:none
  type:exact-one  type:exact-zero     #F
  type:number     type:inexact-number)


(define-typerew-unary-variants-type-method 'SYMBOL-NAME  effect:none
  type:symbol    type:string          system-pair-car
  type:symbol    type:string)

(for-each
    (lambda (name)
      (define-typerew-unary-variants-type-method (make-primitive-procedure name)
	effect:none
	type:any type:boolean))
  '(BIT-STRING? CELL? FIXNUM? FLONUM? INDEX-FIXNUM? NOT NULL?
		PAIR? STRING? INTEGER?))

(define-typerew-unary-variants-type-method %compiled-entry? effect:none
  type:any type:boolean)
				 

(define-typerew-binary-variants-type-method (make-primitive-procedure '&+)
  effect:none
  type:unsigned-byte    type:unsigned-byte    type:small-fixnum>=0  fix:+
  type:small-fixnum>=0  type:small-fixnum>=0  type:fixnum>=0        fix:+
  type:small-fixnum     type:small-fixnum     type:fixnum           fix:+
  type:flonum           type:flonum           type:flonum           flo:+
  type:exact-integer    type:exact-integer    type:exact-integer    #F
  type:exact-number     type:exact-number     type:exact-number     #F
  type:inexact-number   type:number           type:inexact-number   %+
  type:number           type:inexact-number   type:inexact-number   %+
  type:number           type:number           type:number           #F)


(define-typerew-binary-variants-type-method fix:+
  effect:none
  type:fixnum  type:fixnum type:fixnum  #F)

(define-typerew-binary-variants-type-method (make-primitive-procedure '&-)
  effect:none
  type:small-fixnum     type:small-fixnum     type:fixnum           fix:-
  type:flonum           type:flonum           type:flonum           flo:-
  type:exact-integer    type:exact-integer    type:exact-integer    #F
  type:exact-number     type:exact-number     type:exact-number     #F
  type:inexact-number   type:number           type:inexact-number   %-
  type:number           type:inexact-number   type:inexact-number   %-
  type:number           type:number           type:number           #F)

(let ((type:inexact+0    (type:or type:inexact-number type:exact-zero)))
  (define-typerew-binary-variants-type-method (make-primitive-procedure '&*)
    effect:none
    type:unsigned-byte    type:unsigned-byte    type:small-fixnum>=0  fix:*
    type:flonum           type:flonum           type:flonum           flo:*
    type:exact-integer    type:exact-integer    type:exact-integer    #F
    type:exact-number     type:exact-number     type:exact-number     #F
    ;; Note that (* <inexact> 0) = 0
    type:inexact-number   type:inexact-number   type:inexact-number   %*
    type:inexact-number   type:number           type:inexact+0        %*
    type:number           type:inexact-number   type:inexact+0        %*
    type:number           type:number           type:number           #F))

(define-typerew-binary-variants-type-method (make-primitive-procedure '&/)
  effect:none
  type:flonum           type:flonum           type:flonum           flo:/
  type:inexact-number   type:number           type:inexact-number   #F
  type:number           type:inexact-number   type:inexact-number   #F
  type:number           type:number           type:number           #F)

(let* ((type:fixnum-not-0 (type:except type:fixnum type:exact-zero))
       (type:fixnum-not-0/-1
	(type:except type:fixnum-not-0 type:exact-minus-one)))
  (define-typerew-binary-variants-type-method
    (make-primitive-procedure 'QUOTIENT)    effect:none
    ;; quotient on fixnums can overflow only when dividing by 0 or -1.  When
    ;; dividing by -1 it can only overflow when the value is the most
    ;; negative fixnum (-2^(word-size-1)). The quotient has the same
    ;; sign as the product.
    type:unsigned-byte   type:fixnum+ve       type:unsigned-byte   fix:quotient
    type:small-fixnum    type:fixnum-not-0/-1 type:small-fixnum    fix:quotient
    type:small-fixnum    type:fixnum-not-0    type:fixnum          fix:quotient
    type:fixnum          type:fixnum-not-0/-1 type:fixnum          fix:quotient
    type:flonum          type:flonum          type:flonum          %quotient
    type:exact-integer   type:exact-integer   type:exact-integer   %quotient
    ;; The only inexact integer representation is flonum
    type:inexact-number  type:number          type:flonum          %quotient
    type:number          type:inexact-number  type:flonum          %quotient
    type:number          type:number          type:number          #F)

  (define-typerew-binary-variants-type-method
    (make-primitive-procedure 'REMAINDER)    effect:none
    ;; quotient on fixnums can overflow only when dividing by 0 or -1.  When
    ;; dividing by -1 it can only overflow when the value is the most
    ;; negative fixnum (-2^(word-size-1)). The remainder has the same
    ;; sign as the dividend.
    type:unsigned-byte   type:fixnum-not-0    type:unsigned-byte  fix:remainder
    type:small-fixnum>=0 type:fixnum-not-0   type:small-fixnum>=0 fix:remainder
    type:fixnum>=0       type:fixnum-not-0    type:fixnum>=0      fix:remainder
    type:small-fixnum    type:fixnum-not-0    type:small-fixnum   fix:remainder
    type:fixnum          type:fixnum-not-0    type:fixnum         fix:remainder
    type:flonum          type:flonum          type:flonum         %remainder
    type:exact-integer   type:exact-integer   type:exact-integer  %remainder
    ;; The only inexact integer representation is flonum
    type:inexact-number  type:number          type:flonum         %remainder
    type:number          type:inexact-number  type:flonum         %remainder
    type:number          type:number          type:number         #F)

  ;; MODULO is not integrated.
  )


(let ((type:fix:+1/-1 (type:or type:exact-one type:exact-minus-one)))
  (define-typerew-binary-variants-type-method 'EXPT
    effect:none
    type:fix:+1/-1     type:fixnum          type:fix:+1/-1      #F
    ;; luckily (EXPT <flonum> 0) => <flonum>
    type:flonum        type:exact-integer   type:flonum         #F
    type:number        type:number          type:number         #F))

(define-typerew-rewrite-method 'EXPT 2
  (lambda (form base exponent)
    (let* ((t-base     (typerew/type base))
	   (t-exponent (typerew/type exponent)))
      (cond ((and (type:subset? t-exponent type:fixnum)
		  (or (equal? base '(QUOTE -1))
		      (equal? base '(QUOTE -1.0))))
	     (let ((negative-one (quote/text e-base)))
	       (form/rewrite! form	;
		 `(IF (CALL ',eq? '#F
			    (CALL ',fix:and '#F ,exponent '1)
			    '0)
		      ',(- negative-one)
		      ',negative-one))))))))

(let ()
  (define (define-relational-method name fix:op flo:op out:op)
    (define-typerew-binary-variants-type-method (make-primitive-procedure name)
      effect:none
      type:fixnum          type:fixnum            type:boolean   fix:op
      type:flonum          type:flonum            type:boolean   flo:op
      type:exact-number    type:exact-number      type:boolean   #F
      type:inexact-number  type:number            type:boolean   out:op
      type:number          type:inexact-number    type:boolean   out:op
      type:number          type:number            type:boolean   #F))

  (define-relational-method  '&<  fix:<  flo:<  %<)
  (define-relational-method  '&=  fix:=  flo:=  %=)
  (define-relational-method  '&>  fix:>  flo:>  %>))

(let ((type:eqv?-is-eq? (type:or (type:not type:number) type:fixnum))
      (eq?              (make-primitive-procedure 'EQ?)))
  (define-typerew-binary-variants-type-method 'EQV?
    effect:none
    type:eqv?-is-eq?   type:any            type:boolean      EQ?
    type:any           type:eqv?-is-eq?    type:boolean      EQ?
    type:any           type:any            type:boolean      #F))


(let ()
  (define (def-unary-selector name asserted-type  result-type type-check-class
	    %test %operation)
    ;; No effects.
    (let* ((rator  (make-primitive-procedure name))
	   (safe-replacement
	    (typerew-operator-replacement/diamond-1-1-1 %test %operation rator))
	   (unsafe-replacement (typerew-operator-replacement %operation)))

      (define-typerew-type-method rator 1
	(typerew/general-type-method
	 rator (list asserted-type) result-type effect:none))

      (define-typerew-rewrite-method rator 1
	(lambda (form arg1)
	  (if (and (typerew/type-checks? type-check-class)
		   (not (type:subset? (typerew/type arg1) asserted-type)))
	      (safe-replacement form)
	      (unsafe-replacement form))))))
  
  (def-unary-selector 'CAR type:pair type:any 'PAIR  PAIR?  %car)
  (def-unary-selector 'CDR type:pair type:any 'PAIR  PAIR?  %cdr)
  (def-unary-selector 'VECTOR-LENGTH type:vector type:vector-length 'VECTOR
    (typerew-object-type-test (machine-tag 'VECTOR))
    %vector-length)
    
  (define (def-unary-mutator name location-type type-check-class
	    effect %test %operation)
    (let* ((rator  (make-primitive-procedure name))
	   (unsafe-replacement (typerew-operator-replacement %operation))
	   (safe-replacement
	    (typerew-operator-replacement/diamond-1-2-2 %test %operation rator))
	   (asserted-types (list location-type type:any)))

      (define-typerew-type-method rator 2
	(typerew/general-type-method rator asserted-types type:any effect))

      (define-typerew-rewrite-method rator 2
	(lambda (form arg1 arg2)
	  arg2				;
	  (if (or (not (typerew/type-checks? type-check-class))
		  (type:subset? (typerew/type arg1) asserted-type))
	      (safe-replacement form)
	      (unsafe-replacement form))))))
  
  (def-unary-mutator 'SET-CAR! type:pair 'PAIR effect:set-car! PAIR? %set-car!)
  (def-unary-mutator 'SET-CDR! type:pair 'PAIR effect:set-cdr! PAIR? %set-cdr!)
  )


(let ()
  ;; For the indexed selectors or mutators we do not even try to figure out
  ;; if the index is in range.

  (define (def-indexed-operations selector-name mutator-name type-check-class
	    element-type asserted-v-type asserted-i-type mutator-effect
	    %selector %mutator v-typecode v-length)
    ;; No effects.
    (let* ((selector           (make-primitive-procedure selector-name))
	   (unsafe-selection   (typerew-operator-replacement %selector)))

      (define (safe-selection checks)
	(lambda (form)
	  (define (equivalent form*)
	    (typerew/remember* form* form))
	  (let ((collection  (typerew/new-name 'COLLECTION))
		(index       (typerew/new-name 'INDEX)))
	    (form/rewrite! form
	      (bind* (list collection index)
		     (list (call/operand1 form) (call/operand2 form))
		     `(IF (CALL ',%generic-index-check/ref '#F
				(LOOKUP ,collection) (LOOKUP ,index)
				(QUOTE ,checks))
			  ,(equivalent
			    `(CALL ',%selector '#F
				   (LOOKUP ,collection) (LOOKUP ,index)))
			  ,(equivalent
			    `(CALL ',selector '#F
				   (LOOKUP ,collection) (LOOKUP ,index)))))))))

      (define-typerew-type-method selector 2
	(typerew/general-type-method
	 selector (list asserted-v-type asserted-i-type) element-type
	 effect:none))

      (define-typerew-rewrite-method selector 2
	(lambda (form collection index)
	  (let ((v-type         (typerew/type collection))
		(type-checks?   (typerew/type-checks? type-check-class))
		(range-checks?  (typerew/range-checks? type-check-class)))
	    (let ((check/1? (and type-checks?
				 (not (type:subset? v-type asserted-v-type))
				 v-typecode))
		  (check/2? (and (or type-checks? range-checks?)
				 v-length)))
	      (if (or check/1? check/2?)
		  ((safe-selection (vector check/1? check/2?)) form)
		  (unsafe-selection form)))))))


    (let* ((mutator         (make-primitive-procedure mutator-name))
	   (unsafe-mutation (typerew-operator-replacement %mutator)))

      (define (safe-mutation checks)
	(lambda (form)
	  (define (equivalent form*)
	    (typerew/remember* form* form))
	  (let ((collection  (typerew/new-name 'COLLECTION))
		(index       (typerew/new-name 'INDEX))
		(element     (typerew/new-name 'ELEMENT)))
	    (form/rewrite! form
	      (bind* (list collection index element)
		     (list (call/operand1 form) (call/operand2 form)
			   (call/operand3 form))
		     `(IF (CALL ',%generic-index-check/set! '#F
				(LOOKUP ,collection) (LOOKUP ,index)
				(LOOKUP ,element) (QUOTE ,checks))
			  ,(equivalent
			    `(CALL ',%mutator '#F
				   (LOOKUP ,collection) (LOOKUP ,index)
				   (LOOKUP ,element)))
			  ,(equivalent
			    `(CALL ',mutator '#F
				   (LOOKUP ,collection) (LOOKUP ,index)
				   (LOOKUP ,element)))))))))

      (define-typerew-type-method mutator 3
	(typerew/general-type-method
	 mutator (list asserted-v-type asserted-i-type element-type) type:any
	 effect:none))

      (define-typerew-rewrite-method mutator 3
	(lambda (form collection index element)
	  (let ((v-type      (typerew/type collection))
		(e-type      (typerew/type element))
		(type-checks?   (typerew/type-checks? type-check-class))
		(range-checks?  (typerew/range-checks? type-check-class)))
	    (let ((check/1? (and type-checks?
				 (not (type:subset? v-type asserted-v-type))
				 v-typecode))
		  (check/2? (and (or type-checks? range-checks?)
				 v-length))
		  (check/3? (and type-checks? element-type
				 (not (type:subset? e-type element-type))
				 element-typecode)))
	      (if (or check/1? check/2? check/3?)
		  ((safe-mutation (vector check/1? check/2? check/3?)) form)
		  (unsafe-mutation form))))))))  

  (def-indexed-operations 'VECTOR-REF  'VECTOR-SET!   'VECTOR
    type:any type:vector type:vector-length  effect:vector-set!
    %vector-ref %vector-set! (machine-tag 'VECTOR) %vector-length)

  (def-indexed-operations '%RECORD-REF '%RECORD-SET!  'RECORD
    type:any type:%record type:vector-length effect:%record-set!
    %%record-ref %%record-set! (machine-tag 'RECORD) %%record-length)

  (def-indexed-operations 'STRING-REF  'STRING-SET!   'STRING
    type:character type:string type:string-length  effect:string-set!
    %string-ref %string-set! (machine-tag 'VECTOR-8B) %string-length)

  (def-indexed-operations 'VECTOR-8B-REF  'VECTOR-8B-SET!  'STRING
    type:unsigned-byte type:string type:string-length  effect:string-set!
    %vector-8b-ref %vector-8b-set! (machine-tag 'VECTOR-8B) %string-length)

  (def-indexed-operations
    'FLOATING-VECTOR-REF 'FLOATING-VECTOR-SET!  'FLO:VECTOR
    type:flonum type:flonum-vector type:vector-length effect:flo:vector-set!
    %flo:vector-ref %flo:vector-set! (machine-tag 'FLONUM)  %flo:vector-length)
)

(define (pp/ann/ty program)
  (let ((type-map *typerew-type-map*)
	(sugg-map *typerew-suggestions-map*)
	(dbg-map  *typerew-dbg-map*)
	(cache    (make-monotonic-strong-eq-hash-table))) ; prevents GC
    (define (annotate e)
      (or (monotonic-strong-eq-hash-table/get cache e #F)
	  (let ((type  (monotonic-strong-eq-hash-table/get type-map e #F))
		(new   (monotonic-strong-eq-hash-table/get sugg-map e #F)))
	    (let ((annotation
		   (cond ((and (not type) (not new))  #F)
			  ((not type)
			   `(suggested-operator-replacement: ,new))
			  ((not new)  type)
			  (else
			   `(type: ,type
				   suggested-operator-replacement: ,new)))))
	      (monotonic-strong-eq-hash-table/put! cache e annotation)
	      annotation))))
    (pp/ann program annotate)))
