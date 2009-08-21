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

;;;; Type analysis and rewriting
;;; package: (compiler midend)

(declare (usual-integrations))

(define *typerew-type-map*)		; form->type

(define *typerew-dbg-map*)

(define (typerew/top-level program)
  (let  ((program* (copier/top-level program code-rewrite/remember)))
    ;;(kmp/ppp program*)
    (fluid-let ((*typerew-type-map*        (make-form-map))
		(*typerew-dbg-map*	   (make-form-map)))
      (typerew/expr program* q-env:top
		    (lambda (q t e) q t e
		      ;;(bkpt "PROGRAM* has been analysed")
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
  (form-map/put! *typerew-type-map* form type))

(define (typerew/type form)
  (or (form-map/get *typerew-type-map* form #F)
      (internal-error "No type associated with form" form)))

(define (typerew/type/no-error form)
  (form-map/get *typerew-type-map* form #F))

;; This is incorrect in the following conservative way: QUANTITY may
;; already be bound in ENV to a type that would restrict TYPE.
;;(define-integrable (typerew/send receiver quantity type env)
;;  (receiver quantity type (q-env:glb/1 env quantity type)))

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
  (call-with-values
      (lambda () (lambda-list/parse lambda-list))
    (lambda (required optional rest aux)
      required optional aux		; ignored
      rest

      (typerew/expr
       body
       (let ((env-at-call-time (q-env:restrict env effect:unknown)))
	 (if rest
	     (q-env:glb/1 env-at-call-time (quantity:variable rest) type:list)
	     env-at-call-time))       
       (lambda (quantity type env*)
	 quantity type env*		; a shame
	 ;; Creating the procedure or closure itself is no big deal since
	 ;; we dont have reasonable type information for procedures:
	 (typerew/send receiver
		       (quantity:other-expression form effect:none)
		       type:compiled-entry
		       env))))))

(define-type-rewriter CALL (rator cont #!rest rands)
  cont					; ignored - pre-CPS
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
	    quantity			; ignored
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

#|
This version is WASTEFUL since letrec bindings are always procedures
and we dont do much with that.

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
				(q-env:glb/1 env*** quantity type)))))))
|#

(define-type-rewriter LETREC (bindings body)
  ;; This is lame. We need more complex procedure types to summarize what
  ;; we found out about the procedures, and an intelligent traversal
  ;; order to maximize the info (or some kind of iterative solution).
  (let ((env* env))
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
  anything				; ignored
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

(define (typerew/expr*/unordered/old-version exprs env receiver)
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

  (let* ((all-effects
	  (if (for-all? exprs form/simple&side-effect-free?) ;exponential!
	      effect:none
	      effect:unknown))
	 (split-env (q-env:restrict env all-effects)))
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
			  (loop (cons Q Qs) (cons T Ts)
				(cons (q-env:restrict env* all-effects) Es)
				(cdr exprs))))))))

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
  ;; . An optimization: LOOKUPs and QUOTES cant do any damage, so (1) we
  ;;   collect them together and process them at the end and (2) if
  ;;   there is only one hard expression then that can be done
  ;;   directly.

  (define (do-easy easy Qs Ts Es env*)
    ;; now EASY, and Qs, Ts and Es are reversed wrt EXPRS.
    (let loop ((easy easy)
	       (Qs Qs) (Ts Ts) (Es Es)
	       (Qs* '()) (Ts* '()) (Es* '()) (env* env*))
      (define (take-hard easy)
	(loop easy
	      (cdr Qs) (cdr Ts) (cdr Es)
	      (cons (car Qs) Qs*) (cons (car Ts) Ts*) (cons (car Es) Es*) env*))
      (cond ((null? easy)
	     (if (null? Qs)
		 (receiver Qs* Ts* Es* env*)
		 (take-hard easy)))
	    ((car easy)
	     (typerew/expr
	      (car easy)
	      env*
	      (lambda (Q T env**)
		(loop (cdr easy)
		      Qs Ts Es 
		      (cons Q Qs*) (cons T Ts*) (cons env** Es*)
		      (q-env:glb/1 env** Q T)))))
	    (else
	     (take-hard (cdr easy))))))

  (let loop ((exprs exprs) (easy '()) (hard '()))
    ;; HARD and EASY are reversed wrt EXPRS.  EASY ends up the same length as
    ;; EXPRS, with a #f to mark the slots that are occupied by the
    ;; hard expression - so we can reassemble them later.
    (if (pair? exprs)
	(if (or (LOOKUP/? (car exprs))
		(QUOTE/? (car exprs)))
	    (loop (cdr exprs) (cons (car exprs) easy) hard)
	    (loop (cdr exprs) (cons #F easy) (cons (car exprs) hard)))
	(cond ((null? hard) (do-easy easy '() '() '() env))
	      ((null? (cdr hard))
	       (typerew/expr
		(car hard)
		env
		(lambda (Q T env*)
		  (do-easy easy (list Q) (list T) (list env*) env*))))
	      (else
	       (typerew/expr*/unordered/hard
		hard env
		(lambda (Qs Ts Es env*)
		  (do-easy easy Qs Ts Es env*))))))))

(define (typerew/expr*/unordered/hard exprs env receiver)
  (let* ((all-effects
	  (if (for-all? exprs form/simple&side-effect-free?) ;exponential!
	      effect:none
	      effect:unknown))
	 (split-env (q-env:restrict env all-effects)))
    (define (glb* envs)
      (reduce q-env:glb q-env:top envs))
    (let loop ((Qs '()) (Ts '()) (Es '()) (exprs exprs))
      (if (not (pair? exprs))
	  (let ((env* (glb* Es)))	; do before reverse:
	    (receiver (reverse! Qs) (reverse! Ts) (reverse! Es) env*))
	  (typerew/expr (car exprs)
			split-env
			(lambda (Q T env*)
			  (loop (cons Q Qs) (cons T Ts)
				(cons (q-env:restrict env* all-effects) Es)
				(cdr exprs))))))))

(define (typerew/pred form env receiver)
  ;; receiver = (lambda (env_t env_f) ...)
  (define (->expr)
    (typerew/expr
     form env
     (lambda (quantity type env*)
       (receiver (q-env:glb/1 env* quantity (type:and type type:not-false))
		 (q-env:glb/1 env* quantity (type:and type type:false))))))
  (cond ((and (CALL/? form)
	      (QUOTE/? (call/operator form))
	      (operator-predicate-test-type (quote/text (call/operator form))))
	 => (lambda (test-types)
	      (typerew/expr
	       form env
	       (lambda (quantity type env*)
		 type
		 ;;(pp `((predicate-q ,quantity) (pred-type ,type) (env* ,env*)))
		 (let ((arg-quantity (quantity:operand1 quantity))
		       (env*_t (q-env:glb/1 env* quantity type:not-false))
		       (env*_f (q-env:glb/1 env* quantity type:false)))
		   ;;(pp `((arg-quantity ,arg-quantity)(env*_t: ,env*_t) (env*_f: ,env*_f)))
		   (let ((glb-t (q-env:glb/1 env*_t arg-quantity (car test-types)))
			 (glb-f (q-env:glb/1 env*_f arg-quantity (cdr test-types))))
		     ;;(pp `((glb-t: ,glb-t) (glb-f: ,glb-f)))
		     (receiver glb-t glb-f)))))))
		    
		    
	((and (CALL/? form)
	      (QUOTE/? (call/operator form))
	      (eq? OBJECT-TYPE? (quote/text (call/operator form)))
	      (form/number? (call/operand1 form)))
	 => (lambda (tag)
	      (typerew/expr
	       form env
	       (lambda (quantity type env*)
		 type
		 (let ((arg-quantity (quantity:operand2 quantity))
		       (env*_t (q-env:glb/1 env* quantity type:not-false))
		       (env*_f (q-env:glb/1 env* quantity type:false))
		       (test-types (and (exact-integer? tag)
					(type:tag->test-types tag))))
		   ;;(pp `(env*_t: ,env*_t env*_f: ,env*_f))
		   ;;(pp `(test-types ,test-types))
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
     (form-map/put! *typerew-dbg-map* form
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

(define (typerew/remember new old)
  (code-rewrite/remember new old))

(define (typerew/remember* new-form old)
  (let ((info (code-rewrite/original-form old)))
    (if info
	(code-rewrite/remember* new-form info)
	new-form)))

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
;; Quantities are naming scheme for expressions in the program.  We do
;; not use the expressions themselves because we want to tell when two
;; different expressions are really the same thing.
;;
;; Note: currently `different' expressions have to be syntactically the
;; same to be the same quantity, i.e. we do not track what variables
;; are bound to.
;;
;; Quantities are represented as vectors:
;;   #(<hash> <effects> <variable>)
;;   #(<hash> <effects> <quoted-form>)
;;   #(<hash> <effects> <operator> . <operand-quantities>)
;; <effects> is the effects to which this quantity is sensitive.

(define-integrable (quantity:hash Q)
  (vector-ref Q 0))

(define-integrable (quantity:effects Q)
  (vector-ref Q 1))

(define-integrable (quantity:operator Q)
  (vector-ref Q 2))

(define-integrable (quantity:operand1 Q)
  (vector-ref Q 3))

(define-integrable (quantity:operand2 Q)
  (vector-ref Q 4))

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
	(if (type:subset? type type:empty)
	    q-env:bottom
	    (q-env:glb env (list (cons quantity type)))))))

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

;;;; TYPE METHODS
;;
;; Operators have type methods.  Type methods are procedures of the form
;;   (lambda (quantities types env form receiver) ...)
;; They invoke the reciever on
;;   a) a new quantity for the combination
;;   b) the return type of the combination
;;   c) an updated environment reflecting inferences that can be made from the
;;      execution of the combination's operator.
;; TYPEREW/GENERAL-TYPE-METHOD is a generator of type methods from an
;; enforced signature and a set of effects.

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

(define (typerew/general-type-method rator
				     asserted-argument-types
				     result-type
				     effects-performed)
  (let ((adjusted-asserted-argument-types ; handles #!rest args
	 (if (list? asserted-argument-types)
	     (lambda (Ts) Ts asserted-argument-types)
	     (lambda (Ts)
	       ;; Note: we do not detect any arity errors for procedures with
	       ;; #!rest and !#optional arguments, but it is harmless in the
	       ;; sense that we infer what would happen if the program did not
	       ;; terminate with an error.
	       (let loop ((As asserted-argument-types) (Ts Ts) (As* '()))
		 (cond ((null? Ts) (reverse! As*))
		       ((pair? As)
			(loop (cdr As) (cdr Ts) (cons (car As) As*)))
		       (else
			(loop As (cdr Ts) (cons As As*)))))))))
    (lambda (quantities types env form receiver)
      form				; No operator replacement
      (let ((env* (q-env:restrict
		   (q-env:glb* env quantities types
			       (adjusted-asserted-argument-types types))
		   effects-performed)))
	(typerew/send receiver
		      (quantity:combination rator quantities)
		      result-type
		      env*)))))

(let ((OBJECT-TYPE? (make-primitive-procedure 'OBJECT-TYPE?)))
  (define-typerew-type-method OBJECT-TYPE? 2
    (typerew/general-type-method OBJECT-TYPE?
				 (list type:unsigned-byte type:any)
				 type:boolean
				 effect:none)))

;; Example: SUBSTRING?
;;  SUBSTRING? checks that the two arguments are strings and signals an
;;  error if they are not.  If it returns, the result is either #T or
;;  #F, (THIS IS INACCURATE) and it makes no effects (e.g. it doesnt
;;  change the strings).
;;(define-typerew-type-method 'SUBSTRING? 2
;;  (typerew/general-type-method 'SUBSTRING?
;;			       (list type:string type:string)
;;			       type:boolean
;;			       effect:none))

;; 
(define (typerew/rewrite! program)

  (define (rewrite-bindings! bindings)
    (for-each (lambda (binding) (rewrite! (second binding)))
      bindings))

  (define (rewrite!* forms)
    (for-each rewrite! forms))

  (define (rewrite-call! form rator cont rands)

    (define (install-replacement! replacement-generator)
      (sample/1 '(typerew/replaced-operators histogram)
		(let ((op (quote/text (call/operator form))))
		  (if (eq? op %invoke-remote-cache)
		      (first (quote/text (call/operand1 form)))
		      op)))
      (form/rewrite! form (replacement-generator form)))

    (define (apply-method method rands*)
      (install-replacement!
       (cond ((null? rands*)         (method form))
	     ((null? (cdr rands*))   (method form (car rands*)))
	     ((null? (cddr rands*))  (method form (car rands*) (cadr rands*)))
	     ((null? (cdddr rands*))
	      (method form (car rands*) (cadr rands*) (caddr rands*)))
	     (else (apply method form rands*)))))

    (rewrite!* rands)
    (rewrite! cont)
    (if (QUOTE/? rator)
	(let ((rator* (quote/text rator)))
	  (if compiler:type-error-warnings?
	      (typerew/type-check form rator* rands))
	  (cond ((typerew/replacement-method? rator* (length rands))
		 => (lambda (method)
		      (apply-method method rands)))
		((and (eq? rator* %invoke-remote-cache)
		      (typerew/replacement-method?
		       (first (quote/text (first rands)))
		       (second (quote/text (first rands)))))
		 => (lambda (method)
		      (apply-method method (cddr rands))))
		(else (rewrite! rator))))
	(rewrite! rator)))

  (define (check-constant form simple?)
    (let ((type (typerew/type/no-error form)))
      (if type
	  (let ((cst (type:->constant? type)))
	    (if cst
		(form/rewrite! form
		  (if simple?
		      cst
		      `(BEGIN ,(code-rewrite/remember (form/preserve form)
						      form)
			      ,cst))))))))

  (define (rewrite! form)
    (cond ((QUOTE/? form))
	  ((LOOKUP/? form)
	   (check-constant form #T))
	  ((CALL/? form)
	   (rewrite-call! form
			  (call/operator form)
			  (call/continuation form)
			  (call/operands form))
	   (check-constant form #F))
	  ((IF/? form)
	   (rewrite! (if/predicate form))
	   (rewrite! (if/consequent form))
	   (rewrite! (if/alternative form))
	   (check-constant form #F))
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
  
  (rewrite! program))

(define (typerew/type-check form rator* rands)
  ;; Inspect the argument types of FORM and report any errors.
  ;;   FORM = `(call (quote ,RATOR*) '#f ,RANDS)
  
  (define (report errors)
    (user-warning
     (with-output-to-string
       (lambda ()
	 (display "This form has ")
	 (display (if (null? (cdr errors)) "a type error." "type errors."))
	 (for-each display errors)
	 errors))
     (form->source-irritant form)))

  (define (format position required-type actual-type)
    (with-output-to-string
      (lambda ()
	(display "\n; Argument ")
	(display position)
	(display " is ")
	(display (type:user-description actual-type #F))
	(display ", should be ")
	(display (type:user-description required-type #T))
	(display "."))))

  (define (check proc-type all-rands)
    (let ((argument-types (procedure-type/argument-types proc-type))
	  (asserted-types (procedure-type/argument-assertions proc-type)))
      (let loop ((rands  all-rands)
		 (position 1)
		 (argument-types argument-types)
		 (asserted-types asserted-types)
		 (errors '()))		; list (string)
	(define (next errors*)
	  (loop (cdr rands) (+ position 1) 
		(if (pair? argument-types) (cdr argument-types) argument-types)
		(if (pair? asserted-types) (cdr asserted-types) asserted-types)
		errors*))
	(define (test argument-type asserted-type)
	  (let ((rand-type (typerew/type (car rands)))
		(req-type  (type:and asserted-type argument-type)))
	    (if (and (type:disjoint? rand-type req-type)
		     (not (type:subset? rand-type type:empty)))
		(next (cons (format position req-type rand-type) errors))
		(next errors))))
	(cond ((null? rands)
	       (if (pair? errors)
		   (report (reverse! errors))))
	      ((null? argument-types)
	       (internal-warning "Extra arguments in: " proc-type all-rands)
	       (if (pair? errors) (report (reverse! errors))))
	      ((pair? argument-types)
	       (test (car argument-types) (car asserted-types)))
	      (else
	       (test argument-types asserted-types))))))

  (cond ((and (eq? rator* %invoke-remote-cache)
	      (operator-type (first (quote/text (first rands)))))
	 => (lambda (operator-type)
	      (check operator-type (cddr rands))))
	((operator-type rator*)
	 => (lambda (operator-type)
	      (check operator-type rands)))
	(else unspecific)))		; we know nothing

;; REPLACEMENT METHODS
;;
;; Operators have replacement methods.  Replacement methods are produres
;; of the form
;;  (lambda (form arg1 arg2 ... argN) ...)
;; where FORM is the combination with the operator for which this is a
;; rewrite method, and ARG1 .. ARGN are the argument forms.  FORM is
;; passed as an easy way of copying the original expression (via
;; form/and is necessary for accessing the remote-execute-cache for
;; those operators which are global procedures.
;;
;; Replacement methods returns a replacement generator.  The replacement
;; generator is a procedure that when applied to the original FORM,
;; yields new form.  It does not modify the program text.

(define (typerew/replacement-method? op arity)
  (let ((arity.method
	 (monotonic-strong-eq-hash-table/get *typerew/rewrite-methods* op #F)))
    (and arity.method
	 (if (car arity.method)		; specific arity only
	     (and (= (car arity.method) arity)
		  (cdr arity.method))
	     (cdr arity.method)))))	; #F => any arity

(define (define-typerew-replacement-method op arity method)
  ;; ARITY = #F means method for any arity
  (monotonic-strong-eq-hash-table/put! *typerew/rewrite-methods* op
				       (cons arity method)))

;; Operator replacement strategies

(define (typerew-no-replacement form)
  form)

(define (typerew-simple-operator-replacement new-op)
  ;; Coerces operator to a replacement procedure
  (if (and (procedure? new-op) (not (primitive-procedure? new-op)))
      new-op
      (lambda (form)
	(sample/1 '(typerew/simple-replacements histogram) new-op)
	(let ((rator (quote/text (call/operator form))))
	  (if (eq? rator %invoke-remote-cache)
	      (begin
		;;(pp `(,(fourth form) => ,new-op))
		`(CALL (QUOTE ,new-op) '#F ,@(cdr (cddddr form))))
	      (begin
		;;(pp `(,(quote/text (call/operator form)) => ,new-op))
		`(CALL (QUOTE ,new-op) ,@(cddr form))))))))

(define (typerew-object-type-test type-name)
  (let ((OBJECT-TYPE?  (make-primitive-procedure 'OBJECT-TYPE?))
	(type-code     (machine-tag type-name)))
    (lambda (object)
      `(CALL ',OBJECT-TYPE? '#F  (QUOTE ,type-code) ,object))))

(define (typerew/->unary-combination make-combination/operator)
  (if (and (procedure? make-combination/operator)
	   (not (primitive-procedure? make-combination/operator)))
      make-combination/operator
      (lambda (arg1)
	`(CALL (QUOTE ,make-combination/operator) '#F ,arg1))))

(define (typerew/->nary-combination make-combination/operator)
  (if (and (procedure? make-combination/operator)
	   (not (primitive-procedure? make-combination/operator)))
      make-combination/operator
      (lambda args
	`(CALL (QUOTE ,make-combination/operator) '#F ,@args))))

(define typerew/->binary-combination typerew/->nary-combination)
(define typerew/->ternary-combination typerew/->nary-combination)

(define (typerew/->primitive-error-combination primitive)
  (if (not (primitive-procedure? primitive))
      (internal-error "Expected a primitive procedure" primitive))
  (lambda args
    `(CALL (QUOTE ,%invoke-remote-cache)
	   '#F
	   '(%COMPILED-CODE-SUPPORT:SIGNAL-ERROR-IN-PRIMITIVE
	     ,(+ (length args) 1))
	   'bogus-cache-reference	; naughty, should insert a global cache
	   ',primitive
	   ,@args)))

(define (typerew/diamond original-form test-form form*1 form*2)
  (define (equivalent form*)
    (typerew/remember* form* original-form))
  (sample/1 '(typerew/diamond-replacements histogram)
	    (quote/text (call/operator original-form)))
  (equivalent `(IF ,test-form
		   ,(equivalent form*1)
		   ,(equivalent form*2))))
  
(define (typerew-operator-replacement/diamond-1-1-1 test good-op bad-op)
  (let ((test    (typerew/->unary-combination test))
	(good-op (typerew/->unary-combination good-op))
	(bad-op  (typerew/->unary-combination bad-op)))
    (lambda (form)
      (let ((name (typerew/new-name 'OBJECT)))
	(bind name (call/operand1 form)
	      (typerew/diamond form
			       (test    `(LOOKUP ,name))
			       (good-op `(LOOKUP ,name))
			       (bad-op  `(LOOKUP ,name))))))))

(define (typerew-operator-replacement/diamond-1-2-2 test good-op bad-op)
  (let ((test    (typerew/->unary-combination test))
	(good-op (typerew/->binary-combination good-op))
	(bad-op  (typerew/->binary-combination bad-op)))
    (lambda (form)
      (let ((object (typerew/new-name 'OBJECT))
	    (value  (typerew/new-name 'VALUE)))
	(bind* 
	 (list object value)
	 (list (call/operand1 form) (call/operand2 form))
	 (typerew/diamond form
			  (test    `(LOOKUP ,object))
			  (good-op `(LOOKUP ,object) `(LOOKUP ,value))
			  (bad-op  `(LOOKUP ,object) `(LOOKUP ,value))))))))


(define (typerew-operator-replacement/diamond-2-2-2 test good-op bad-op)
  (let ((test    (typerew/->binary-combination test))
	(good-op (typerew/->binary-combination good-op))
	(bad-op  (typerew/->binary-combination bad-op)))
    (lambda (form)
      (let ((object (typerew/new-name 'OBJECT))
	    (index  (typerew/new-name 'INDEX)))
	(bind*
	 (list object index)
	 (list (call/operand1 form) (call/operand2 form))
	 (typerew/diamond form 
			  (test    `(LOOKUP ,object) `(LOOKUP ,index))
			  (good-op `(LOOKUP ,object) `(LOOKUP ,index))
			  (bad-op  `(LOOKUP ,object) `(LOOKUP ,index))))))))

(define (typerew-operator-replacement/diamond-2-3-3 test good-op bad-op)
  (let ((test    (typerew/->binary-combination test))
	(good-op (typerew/->ternary-combination good-op))
	(bad-op  (typerew/->ternary-combination bad-op)))
    (lambda (form)
      (let ((obj (typerew/new-name 'OBJECT))
	    (idx (typerew/new-name 'INDEX))
	    (elt (typerew/new-name 'ELEMENT)))
	(bind*
	 (list obj idx elt)
	 (list (call/operand1 form) (call/operand2 form) (call/operand3 form))
	 (typerew/diamond
	  form
	  (test    `(LOOKUP ,obj) `(LOOKUP ,idx))
	  (good-op `(LOOKUP ,obj) `(LOOKUP ,idx) `(LOOKUP ,elt))
	  (bad-op  `(LOOKUP ,obj) `(LOOKUP ,idx) `(LOOKUP ,elt))))))))

(define (typerew-operator-replacement/diamond-3-3-3 test good-op bad-op)
  (let ((test    (typerew/->binary-combination test))
	(good-op (typerew/->ternary-combination good-op))
	(bad-op  (typerew/->ternary-combination bad-op)))
    (lambda (form)
      (let ((obj (typerew/new-name 'OBJECT))
	    (idx (typerew/new-name 'INDEX))
	    (elt (typerew/new-name 'ELEMENT)))
	(bind*
	 (list obj idx elt)
	 (list (call/operand1 form) (call/operand2 form) (call/operand3 form))
	 (typerew/diamond
	  form
	  (test    `(LOOKUP ,obj) `(LOOKUP ,idx) `(LOOKUP ,elt))
	  (good-op `(LOOKUP ,obj) `(LOOKUP ,idx) `(LOOKUP ,elt))
	  (bad-op  `(LOOKUP ,obj) `(LOOKUP ,idx) `(LOOKUP ,elt))))))))

(define (typerew/%1 op)			; (mumble x y z) => (op x)
  (lambda (form)
    (define (make args)
      (sample/1 '(typerew/left-constant-replacements histogram) op)
      `(CALL (QUOTE ,op)
	     '#F
	     ,(first args)))
    (if (eq? (quote/text (call/operator form)) %invoke-remote-cache)
	(make (cddr (cddddr form)))
	(make (cdddr form)))))

(define (typerew-binary-variants-type-method
	 rator
	 domain1 domain2 range effect . spec)
  ;; spec: repeated (input-type1 input-type2 output-type)
  ;;  Compute result type for an operator that verifies its arguments are in
  ;;  DOMAIN1 and DOMAIN2.  Test triples in order.

  (define (result receiver result-type q1 q2 env)
    (typerew/send receiver
		  (quantity:combination/2 rator q1 q2)
		  result-type
		  env))

  (define universal-domain?
    (and (type:subset? type:any domain1)
	 (type:subset? type:any domain2)))

  (define (compile-spec spec)
    ;; COMPILE-SPEC converts SPEC into a procedure to eliminate the
    ;; interpretive overhead of analysing SPEC every time.
    (if (null? spec)
	;; Select a DEFAULT-METHOD optimized to reduce useless work
	(cond ((and (effect:none? effect) universal-domain?)
	       (lambda (t1 t2 q1 q2 env form receiver)
		 t1 t2 form		; ignored
		 (result receiver range q1 q2 env)))
	      ((effect:none? effect)
	       (lambda (t1 t2 q1 q2 env form receiver)
		 form			; ignored
		 (result receiver range q1 q2
			 (q-env:glb/1 (q-env:glb/1 env q1 t1) q2 t2))))
	      (else
	       (lambda (t1 t2 q1 q2 env form receiver)
		 form			; ignored
		 (result receiver range q1 q2
			 (q-env:restrict (q-env:glb/1 (q-env:glb/1 env q1 t1)
						      q2 t2)
					 effect)))))
	(let* ((a1  (first spec))
	       (a2  (second spec)) 
	       (result-type (third spec)))
	  (let ((more-tests (compile-spec (cdddr spec))))
	    (lambda (t1 t2 q1 q2 env form receiver)
	      (if (and (type:subset? t1 a1) (type:subset? t2 a2))
		  (result receiver result-type q1 q2 env)
		  (more-tests t1 t2 q1 q2 env form receiver)))))))

  (let ((compiled-spec  (compile-spec spec)))
    (lambda (quantities types env form receiver)
      (compiled-spec (type:and (first types)  domain1)
		     (type:and (second types) domain2)
		     (first quantities) (second quantities)
		     env form receiver))))

(define (typerew-binary-variants-replacement-method . spec)
  ;; spec: repeated (input-type1 input-type2 output-type replacement)
  ;;  Select a replacement according to signature
  (define (make-search spec)
    ;; MAKE-SEARCH converts SPEC into a procedure to eliminate the
    ;; interpretive overhead of analysing SPEC every time.
    (if (null? spec)
	(lambda (t1 t2 t-result)
	  t1 t2 t-result		; ignore
	  typerew-no-replacement)
	(let* ((a1 (first spec))
	       (a2 (second spec)) 
	       (result-type (third spec))
	       (replacement (fourth spec)))
	  (let ((try-others (make-search (cddddr spec)))
		(replacement*
		 (if replacement
		     (typerew-simple-operator-replacement replacement)
		     typerew-no-replacement)))
	    (lambda (t1 t2 t-result)
	      (if (and (type:subset? t1 a1) (type:subset? t2 a2)
		       (type:subset? t-result result-type))
		  (begin
		    replacement*)
		  (try-others t1 t2 t-result)))))))

  (let ((search  (make-search spec)))
    (lambda (form arg1 arg2)
      (search (typerew/type arg1) (typerew/type arg2)
	      (typerew/type form)))))

(define (typerew-unary-variants-type-method
	 rator
	 domain range effect . spec)
  ;; spec: repeated (input-type output-type)
  ;;  Compute result type for an operator that verifies its arguments are in
  ;;  DOMAIN.  Test in order.

  (define (result receiver result-type quantity env)
    (typerew/send receiver
		  (quantity:combination/1 rator quantity)
		  result-type
		  env))

  (define universal-domain?
    (type:subset? type:any domain))

  (define (compile-spec spec)
    ;; COMPILE-SPEC converts SPEC into a procedure to eliminate the
    ;; interpretive overhead of analysing SPEC every time.
    (if (null? spec)
	;; Select a DEFAULT-METHOD optimized to reduce useless work
	(cond ((and (effect:none? effect) universal-domain?)
	       (lambda (t q env form receiver)
		 t form			; ignored
		 (result receiver range q env)))
	      ((effect:none? effect)
	       (lambda (t q env form receiver)
		 form			; ignored
		 (result receiver range q
			 (q-env:glb/1 env q t))))
	      (else
	       (lambda (t q env form receiver)
		 form			; ignored
		 (result receiver range q
			 (q-env:restrict (q-env:glb/1 env q t)
					 effect)))))
	(let* ((arg-type     (first spec))
	       (result-type  (second spec)))
	  (let ((more-tests   (compile-spec (cddr spec))))
	    (lambda (t q env form receiver)
	      (if (type:subset? t arg-type)
		  (result receiver result-type q env)
		  (more-tests t q env form receiver)))))))

  (let ((compiled-spec  (compile-spec spec)))
    (lambda (quantities types env form receiver)
      (compiled-spec (type:and (first types)  domain)
		     (first quantities)
		     env form receiver))))

(define (typerew-unary-variants-replacement-method . spec)
  ;; spec: repeated (input-type output-type replacement)
  ;;  Select a replacement according to signature
  (define (make-search spec)
    ;; MAKE-SEARCH converts SPEC into a procedure to eliminate the
    ;; interpretive overhead of analysing SPEC every time.
    (if (null? spec)
	(lambda (t-input t-result)
	  t-input t-result		; ignore
	  typerew-no-replacement)
	(let* ((arg-type   (first spec))
	       (result-type (second spec))
	       (replacement (third spec)))
	  (let ((try-others (make-search (cdddr spec)))
		(replacement*
		 (if replacement
		     (typerew-simple-operator-replacement replacement)
		     typerew-no-replacement)))
	    (lambda (t-input t-result)
	      (if (and (type:subset? t-input arg-type)
		       (type:subset? t-result result-type))
		  replacement*
		  (try-others t-input t-result)))))))

  (let ((search  (make-search spec)))
    (lambda (form arg1)
      (search (typerew/type arg1) (typerew/type form)))))

(define (define-typerew-unary-predicate-type-method operator method)
  (define-typerew-type-method operator 1
    (lambda (quantities types env form receiver)
      form				; No operator replacement
      (let ((env* (q-env:glb* env quantities types (list type:any))))
	(typerew/send receiver
		      (quantity:combination operator quantities)
		      (method form (first types))
		      env*)))))

(define (define-typerew-binary-predicate-type-method operator method)
  (define-typerew-type-method operator 2
    (lambda (quantities types env form receiver)
      form				; No operator replacement
      (let ((env* (q-env:glb* env quantities types (list type:any type:any))))
	(typerew/send receiver
		      (quantity:combination operator quantities)
		      (method form (first types) (second types))
		      env*)))))

(define (define-typerew-unary-variants-type-method name . spec)
  (define-typerew-type-method name 1
    (apply typerew-unary-variants-type-method name spec)))

(define (define-typerew-unary-variants-replacement-method name . spec)
  (define-typerew-replacement-method name 1
    (apply typerew-unary-variants-replacement-method spec)))

(define (define-typerew-binary-variants-type-method name . spec)
  (define-typerew-type-method name 2
    (apply typerew-binary-variants-type-method name spec)))

(define (define-typerew-binary-variants-replacement-method name . spec)
  (define-typerew-replacement-method name 2
    (apply typerew-binary-variants-replacement-method spec)))

(define-typerew-unary-variants-type-method 'EXACT->INEXACT
  type:number  type:inexact-number   effect:none
  type:real    type:inexact-real	;i.e. flonum
  type:recnum  type:inexact-recnum)

(define-typerew-unary-variants-replacement-method 'EXACT->INEXACT
  type:fixnum  type:flonum      %fixnum->flonum)

(define-typerew-unary-variants-type-method 'INEXACT->EXACT
  type:number  type:exact-number  effect:none
  type:real    type:exact-real
  type:recnum  type:exact-recnum)


(let ()
  (define (def op flo:op)
    (define-typerew-unary-variants-type-method op
      type:number    type:exact-integer  effect:none)
    (define-typerew-unary-variants-replacement-method op
      type:flonum    type:exact-integer  FLO:op))

  (def  'CEILING->EXACT   FLO:CEILING->EXACT)
  (def  'FLOOR->EXACT     FLO:FLOOR->EXACT)
  (def  'ROUND->EXACT     FLO:ROUND->EXACT)
  (def  'TRUNCATE->EXACT  FLO:TRUNCATE->EXACT))

(let ()
  (define (def op flo:op)
    (define-typerew-unary-variants-type-method op
      type:number        type:real          effect:none
      type:flonum        type:flonum
      type:exact-number  type:exact-real)
    (define-typerew-unary-variants-replacement-method op
      type:flonum        type:flonum     FLO:op))
  (def  'CEILING   FLO:CEILING)
  (def  'FLOOR     FLO:FLOOR)
  (def  'ROUND     FLO:ROUND)
  (def  'TRUNCATE  FLO:TRUNCATE))

(let ((INTEGER->FLONUM (ucode-primitive INTEGER->FLONUM 2))
      (FIXNUM->FLONUM  (ucode-primitive FIXNUM->FLONUM 1))
      (type:false/flonum (type:or type:false type:flonum))
      (type:0/1          (type:or type:exact-zero type:exact-one)))
  (define-typerew-binary-variants-type-method INTEGER->FLONUM
    type:exact-integer  type:unsigned-byte        type:false/flonum
    effect:none
    type:fixnum         type:exact-zero           type:flonum
    type:fixnum         type:exact-one            type:flonum         ; [1]
    type:fixnum         type:exact-one            type:false/flonum   ; [2]
    type:fixnum         type:small-fixnum:2..255  type:flonum
    type:exact-integer  type:0/1                  type:false/flonum
    type:exact-integer  type:small-fixnum:2..255  type:flonum)
  
  ;; [1] if fixnums guaranteed to fit in a flonum (e.g. 32 bit machine)
  ;; [2] if fixnums may not fix in a flonum (e.g. 64 bit machine).

  (define-typerew-binary-variants-replacement-method INTEGER->FLONUM
    type:fixnum        type:any      type:flonum (typerew/%1 %fixnum->flonum))

  (define-typerew-unary-variants-replacement-method FIXNUM->FLONUM
    type:fixnum        type:flonum      %fixnum->flonum))




(define-typerew-unary-variants-type-method 'COS
  type:number     type:number       effect:none
  type:exact-zero type:exact-one
  type:real       type:flonum)
				 
(define-typerew-unary-variants-type-method 'SIN
  type:number     type:number       effect:none
  type:exact-zero type:exact-zero
  type:real       type:flonum)
				 
(define-typerew-unary-variants-type-method 'TAN
  type:number     type:number
  effect:none
  type:exact-zero type:exact-zero
  type:real       type:flonum)
				 
(define-typerew-unary-variants-type-method 'ACOS
  type:number     type:number        effect:none
  type:exact-one  type:exact-zero
  type:number     type:inexact-number)

				 
(define-typerew-unary-variants-type-method 'ASIN
  type:number     type:number        effect:none
  type:exact-zero type:exact-zero
  type:number     type:inexact-number)
				 
(define-typerew-unary-variants-type-method 'EXP
  type:number     type:number              effect:none
  type:recnum     type:inexact-recnum
  type:exact-zero type:exact-one
  type:real       type:inexact-real
  type:number     type:inexact-number)
				 
(define-typerew-unary-variants-type-method 'LOG
  type:number     type:number          effect:none
  type:exact-one  type:exact-zero
  type:number     type:inexact-number)

(let ()
  (define (def name flo:op)
    (define-typerew-unary-variants-replacement-method name
      type:flonum  type:flonum  flo:op))
  (def  'COS   flo:cos)
  (def  'SIN   flo:sin)
  (def  'TAN   flo:tan)
  (def  'EXP   flo:exp))

(define-typerew-unary-variants-type-method 'ABS
  type:number          type:real            effect:none
  type:exact-one       type:exact-zero
  (type:or type:small-fixnum type:big-fixnum+ve)    type:fixnum
  type:fixnum          (type:or type:fixnum type:bignum>0)
  type:exact-integer   type:exact-integer
  type:flonum          type:flonum)

(define-typerew-unary-variants-replacement-method 'ABS
  type:flonum     type:flonum    flo:abs)

(define-typerew-unary-variants-replacement-method 'SQRT
  type:number          type:number          effect:none
  type:fixnum+ve       (type:or type:small-fixnum+ve type:flonum)
  type:fixnum+ve       (type:or type:small-fixnum+ve type:flonum)
  type:flonum          (type:or type:flonum type:inexact-recnum))


(define-typerew-unary-variants-type-method 'SYMBOL-NAME
  type:symbol    type:string    effect:none)

(define-typerew-unary-variants-replacement-method 'SYMBOL-NAME
  type:symbol    type:string    system-pair-car)

(define (typerew/rewrite/coerced-arguments op coerce-left coerce-right)
  (lambda (form)
    (define (make args)
      (sample/1 '(typerew/coerced->flonum-replacements histogram) op)
      `(CALL (QUOTE ,op)
	     '#F
	     ,(coerce-left  (first args))
	     ,(coerce-right (second args))))
    (if (eq? (quote/text (call/operator form)) %invoke-remote-cache)
	(make (cddr (cddddr form)))
	(make (cdddr form)))))

(define (typerew/coerce/fixnum->flonum expr)
  (if (QUOTE/? expr)
      `(QUOTE ,(exact->inexact (quote/text expr)))
      `(CALL (QUOTE ,%fixnum->flonum) '#F ,expr)))

(define (typerew/%l flo:op)
  (typerew/rewrite/coerced-arguments flo:op typerew/coerce/fixnum->flonum
				     identity-procedure))

(define (typerew/%r flo:op)
  (typerew/rewrite/coerced-arguments flo:op identity-procedure
				     typerew/coerce/fixnum->flonum))

(define (typerew/%lc op left-constant)	; (mumble x y z) => (op x y z 'c)
  (lambda (form)
    (define (make args)
      (sample/1 '(typerew/left-constant-replacements histogram) op)
      `(CALL (QUOTE ,op)
	     '#F
	     ,@args
	     (QUOTE ,left-constant)))
    (if (eq? (quote/text (call/operator form)) %invoke-remote-cache)
	(make (cddr (cddddr form)))
	(make (cdddr form)))))

(let ((&+ (make-primitive-procedure '&+))
      (type:not-fixnum (type:not type:fixnum)))

  (define (generic-addition-inference op)
    (define-typerew-binary-variants-type-method op
      type:number           type:number           type:number
      effect:none
      type:unsigned-byte    type:unsigned-byte     type:small-fixnum>=0
      type:small-fixnum>=0  type:small-fixnum>=0   type:fixnum>=0
      type:small-fixnum-ve  type:small-fixnum-ve   type:fixnum-ve
      type:small-fixnum>=0  type:small-fixnum-ve   type:small-fixnum
      type:small-fixnum-ve  type:small-fixnum>=0   type:small-fixnum
      type:small-fixnum     type:small-fixnum      type:fixnum
      type:fixnum>=0        type:fixnum-ve         type:fixnum
      type:fixnum-ve        type:fixnum>=0         type:fixnum
      type:fixnum           type:flonum            type:flonum
      type:flonum           type:fixnum            type:flonum
      type:flonum           type:flonum            type:flonum
      type:exact-integer    type:exact-integer     type:exact-integer
      type:exact-number     type:exact-number      type:exact-number
      type:inexact-number   type:number            type:inexact-number
      type:number           type:inexact-number    type:inexact-number))

  (generic-addition-inference &+)
  (generic-addition-inference %+)

  (define-typerew-binary-variants-replacement-method &+
    type:fixnum         type:fixnum         type:fixnum     fix:+
    type:flonum         type:flonum         type:flonum     flo:+
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:+)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:+)
    type:not-fixnum     type:any            type:any        %+
    type:any            type:not-fixnum     type:any        %+)

  (define-typerew-binary-variants-replacement-method %+
    type:fixnum         type:fixnum         type:fixnum     fix:+
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:+)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:+)
    type:flonum         type:flonum         type:flonum     flo:+))


(define-typerew-binary-variants-type-method fix:+
  type:any           type:any            type:fixnum
  effect:none
  type:unsigned-byte    type:unsigned-byte     type:small-fixnum>=0
  type:small-fixnum>=0  type:small-fixnum>=0   type:fixnum>=0
  type:small-fixnum-ve  type:small-fixnum-ve   type:fixnum-ve
  type:small-fixnum>=0  type:small-fixnum-ve   type:small-fixnum
  type:small-fixnum-ve  type:small-fixnum>=0   type:small-fixnum)

(let ((&- (make-primitive-procedure '&-))
      (type:not-fixnum (type:not type:fixnum)))

  (define (generic-subtraction-inference op)
    (define-typerew-binary-variants-type-method op
      type:number           type:number           type:number
      effect:none
      type:small-fixnum     type:small-fixnum     type:fixnum
      type:fixnum>=0        type:fixnum>=0        type:fixnum
      type:fixnum           type:flonum           type:flonum
      type:flonum           type:fixnum           type:flonum
      type:flonum           type:flonum           type:flonum
      type:exact-integer    type:exact-integer    type:exact-integer
      type:exact-number     type:exact-number     type:exact-number
      type:inexact-number   type:number           type:inexact-number
      type:number           type:inexact-number   type:inexact-number))

  (generic-subtraction-inference &-)
  (generic-subtraction-inference %-)

  (define-typerew-binary-variants-replacement-method &-
    type:fixnum         type:fixnum         type:fixnum     fix:-
    type:flonum         type:flonum         type:flonum     flo:-
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:-)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:-)
    type:not-fixnum     type:any            type:any        %-
    type:any            type:not-fixnum     type:any        %-)

  (define-typerew-binary-variants-replacement-method %-
    type:fixnum         type:fixnum         type:fixnum     fix:-
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:-)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:-)
    type:flonum         type:flonum         type:flonum     flo:-))


(let ((&*                 (make-primitive-procedure '&*))
      (&/                 (make-primitive-procedure '&/))
      (type:inexact+0     (type:or type:inexact-number type:exact-zero))
      (type:exact-int-not-0  (type:except type:exact-integer type:exact-zero))
      (type:flonum+0      (type:or type:flonum type:exact-zero))
      (type:not-fixnum    (type:not type:fixnum)))

  (define (generic-multiply-inference op)
    (define-typerew-binary-variants-type-method op
      type:number           type:number           type:number
      effect:none
      type:unsigned-byte    type:unsigned-byte    type:small-fixnum>=0
      type:exact-int-not-0  type:flonum           type:flonum
      type:flonum           type:exact-int-not-0  type:flonum
      type:exact-integer    type:flonum           type:flonum+0
      type:flonum           type:exact-integer    type:flonum+0
      type:flonum           type:flonum           type:flonum
      type:exact-integer    type:exact-integer    type:exact-integer
      type:exact-number     type:exact-number     type:exact-number
      ;; Note that (* <inexact> 0) = 0
      type:inexact-number   type:inexact-number   type:inexact-number
      type:inexact-number   type:number           type:inexact+0
      type:number           type:inexact-number   type:inexact+0))

  (generic-multiply-inference &*)
  (generic-multiply-inference %*)

  (define-typerew-binary-variants-replacement-method &*
    type:fixnum         type:fixnum         type:fixnum     fix:*
    type:flonum         type:flonum         type:flonum     flo:*
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:*)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:*)
    type:not-fixnum     type:any            type:any        %*
    type:any            type:not-fixnum     type:any        %*)

  (define-typerew-binary-variants-replacement-method %*
    type:fixnum         type:fixnum         type:fixnum     fix:*
    type:fixnum         type:flonum         type:flonum     (typerew/%l flo:*)
    type:flonum         type:fixnum         type:flonum     (typerew/%r flo:*)
    type:flonum         type:flonum         type:flonum     flo:*)
  

  (define (generic-divide-inference op)
    (define-typerew-binary-variants-type-method op
      type:number           type:number           type:number
      effect:none
      type:flonum           type:flonum           type:flonum
      type:flonum           type:fixnum           type:flonum
      type:exact-int-not-0  type:flonum           type:flonum
      type:exact-integer    type:flonum           type:flonum+0
      type:inexact-number   type:number           type:inexact-number
      type:number           type:inexact-number   type:inexact-number))

  (generic-divide-inference &/)
  (generic-divide-inference %/)

  (define-typerew-binary-variants-replacement-method &/
    type:fixnum          type:flonum          type:flonum   (typerew/%l flo:/)
    type:flonum          type:fixnum          type:flonum   (typerew/%r flo:/)
    type:flonum          type:flonum          type:flonum   flo:/)

  (define-typerew-binary-variants-replacement-method %/
    type:fixnum          type:flonum          type:flonum   (typerew/%l flo:/)
    type:flonum          type:fixnum          type:flonum   (typerew/%r flo:/)
    type:flonum          type:flonum          type:flonum   flo:/))


(let* ((type:fixnum-not-0 (type:except type:fixnum type:exact-zero))
       (type:fixnum-not-0/-1
	(type:except type:fixnum-not-0 type:exact-minus-one))
       (type:integer-result (type:or type:exact-integer type:flonum))
       (QUOTIENT   (make-primitive-procedure 'QUOTIENT))
       (REMAINDER  (make-primitive-procedure 'REMAINDER))
       (INTEGER-QUOTIENT   (ucode-primitive INTEGER-QUOTIENT))
       (INTEGER-REMAINDER  (ucode-primitive INTEGER-REMAINDER)))

  ;; QUOTIENT and REMAINDER on fixnums can overflow only when dividing by 0
  ;; or -1.  When dividing by -1 it can only overflow when the value
  ;; is the most negative fixnum (-2^(word-size-1)). The quotient has
  ;; the same sign as the product.  The remainder has the same sign as
  ;; the dividend.  Both return integers (exact or inexact).  Note
  ;; that inexact inputs might be recnums and might yield exact
  ;; results:
  ;;   (quotient 10+0.i 3)  =>  3
  ;; The flonum cases correspond to a subset of the inexact cases with a
  ;; known (i.e. flonum) representation.

  (define-typerew-binary-variants-type-method  QUOTIENT
    type:number          type:number          type:integer-result
    effect:none
    type:unsigned-byte   type:fixnum+ve       type:unsigned-byte
    type:small-fixnum    type:fixnum-not-0/-1 type:small-fixnum
    type:small-fixnum    type:fixnum-not-0    type:fixnum
    type:fixnum          type:fixnum-not-0/-1 type:fixnum
    type:exact-integer   type:exact-integer   type:exact-integer
    type:flonum          type:flonum          type:flonum
    type:inexact-number  type:number          type:integer-result
    type:number          type:inexact-number  type:integer-result)

  (define-typerew-binary-variants-type-method  REMAINDER 
    type:number          type:number          type:integer-result
    effect:none
    type:unsigned-byte   type:exact-integer   type:unsigned-byte
    type:fixnum>=0       type:unsigned-byte   type:unsigned-byte
    type:small-fixnum>=0 type:exact-integer   type:small-fixnum>=0
    type:small-fixnum    type:exact-integer   type:small-fixnum
    type:exact-integer   type:unsigned-byte   type:small-fixnum
    type:fixnum>=0       type:exact-integer   type:fixnum>=0
    type:exact-integer   type:small-fixnum    type:fixnum
    type:fixnum          type:exact-integer   type:fixnum
    type:exact-integer   type:exact-integer   type:exact-integer
    type:flonum          type:flonum          type:flonum
    type:inexact-number  type:number          type:integer-result
    type:number          type:inexact-number  type:integer-result)

  (define-typerew-binary-variants-type-method INTEGER-QUOTIENT
    type:exact-integer  type:exact-integer    type:exact-integer  effect:none)

  (define-typerew-binary-variants-type-method INTEGER-REMAINDER
    type:exact-integer   type:exact-integer   type:exact-integer
    effect:none
    type:unsigned-byte   type:exact-integer   type:unsigned-byte
    type:fixnum>=0       type:unsigned-byte   type:unsigned-byte
    type:small-fixnum>=0 type:exact-integer   type:small-fixnum>=0
    type:fixnum>=0       type:exact-integer   type:fixnum>=0
    type:small-fixnum    type:exact-integer   type:small-fixnum
    type:exact-integer   type:unsigned-byte   type:small-fixnum
    type:exact-integer   type:small-fixnum    type:fixnum
    type:fixnum          type:exact-integer   type:fixnum)

  (define-typerew-binary-variants-replacement-method  QUOTIENT
    type:small-fixnum    type:fixnum-not-0    type:fixnum        fix:quotient
    type:fixnum          type:fixnum-not-0/-1 type:fixnum        fix:quotient
    type:any             type:any             type:any           %quotient)

  (define-typerew-binary-variants-replacement-method  REMAINDER
    type:fixnum          type:fixnum-not-0    type:fixnum        fix:remainder
    type:any             type:any             type:any           %remainder)

  (define-typerew-binary-variants-replacement-method  INTEGER-REMAINDER
    type:fixnum          type:fixnum-not-0    type:fixnum        fix:remainder)

  ;; MODULO is not integrated.
  )

(let ((INTEGER-ADD-1      (ucode-primitive INTEGER-ADD-1))
      (INTEGER-SUBTRACT-1 (ucode-primitive INTEGER-SUBTRACT-1))
      (INTEGER-ADD        (ucode-primitive INTEGER-ADD))
      (INTEGER-SUBTRACT   (ucode-primitive INTEGER-SUBTRACT))
      (INTEGER-MULTIPLY   (ucode-primitive INTEGER-MULTIPLY)))

  (define-typerew-unary-variants-type-method INTEGER-ADD-1
    type:exact-integer    type:exact-integer     effect:none
    type:unsigned-byte    type:small-fixnum>=0
    type:small-fixnum+ve  type:fixnum+ve
    type:small-fixnum>=0  type:fixnum+ve
    type:small-fixnum-ve  type:small-fixnum
    type:small-fixnum     type:fixnum
    type:fixnum-ve        type:fixnum)

  (define-typerew-unary-variants-type-method INTEGER-SUBTRACT-1
    type:exact-integer    type:exact-integer     effect:none
    type:small-fixnum-ve  type:fixnum-ve
    type:small-fixnum+ve  type:small-fixnum>=0
    type:small-fixnum     type:fixnum
    type:fixnum+ve        type:fixnum>=0
    type:small-fixnum>=0  type:small-fixnum
    type:fixnum>=0        type:fixnum)

  (define-typerew-binary-variants-type-method INTEGER-ADD
    type:exact-integer    type:exact-integer     type:exact-integer
    effect:none
    type:unsigned-byte    type:unsigned-byte     type:small-fixnum>=0
    type:small-fixnum>=0  type:small-fixnum>=0   type:fixnum>=0
    type:small-fixnum-ve  type:small-fixnum-ve   type:fixnum-ve
    type:small-fixnum>=0  type:small-fixnum-ve   type:small-fixnum
    type:small-fixnum-ve  type:small-fixnum>=0   type:small-fixnum
    type:small-fixnum     type:small-fixnum      type:fixnum
    type:fixnum>=0        type:fixnum-ve         type:fixnum
    type:fixnum-ve        type:fixnum>=0         type:fixnum
    type:exact-integer    type:exact-integer     type:exact-integer)

  (define-typerew-binary-variants-type-method INTEGER-SUBTRACT
    type:exact-integer  type:exact-integer type:exact-integer effect:none
    type:small-fixnum   type:small-fixnum  type:fixnum)

  (define-typerew-binary-variants-type-method INTEGER-MULTIPLY
    type:exact-integer  type:exact-integer  type:exact-integer  effect:none
    type:unsigned-byte  type:unsigned-byte  type:small-fixnum>=0)

  (define-typerew-binary-variants-replacement-method INTEGER-ADD
    type:fixnum         type:fixnum         type:fixnum     fix:+)
  (define-typerew-binary-variants-replacement-method INTEGER-SUBTRACT
    type:fixnum         type:fixnum         type:fixnum     fix:-)
  (define-typerew-binary-variants-replacement-method INTEGER-MULTIPLY
    type:fixnum         type:fixnum         type:fixnum     fix:*)
  )
#|
(let ()
  ;; Binary MIN and MAX.  We can replace
  ;;   (MIN e1 e2)
  ;; by
  ;;   (if (< e1 e2) e1 e2)
  ;; only if e1 and e2 always have the same exactness
  (define (def min/max)
    (define-typerew-binary-variants-type-method min/max
      type:number         type:number         type:real
      effect:none
      type:fixnum         type:fixnum         type:fixnum
      type:exact-integer  type:exact-integer  type:exact-integer
      type:flonum         type:flonum         type:flonum)

    (define-typerew-binary-variants-replacement-method min/max
      type:fixnum         type:fixnum        type:any (pick fix:op)
      type:exact-integer  type:exact-integer type:any (pick gen:op)
      type:flonum         type:flonum        type:any (pick flo:op)))

  (define (pick compare)
    (lambda (form)
      (let ((arg1   (sixth  form))
	    (arg2   (seventh form))
	    (name1  (typerew/new-name 'ARG1))
	    (name2  (typerew/new-name 'ARG2)))
	(bind* (list name1 name2)
	       (list arg1 arg2)
	       `(IF (CALL ',compare '#F (LOOKUP ,name1) (LOOKUP ,name2))
		    (LOOKUP ,name1)
		    (LOOKUP ,name2))))))

  (def 'MIN  fix:<   (make-primitive-procedure '&<)   flo:<)
  (def 'MAX  fix:>   (make-primitive-procedure '&>)   flo:>))
|#



(let ((type:fix:+1/-1 (type:or type:exact-one type:exact-minus-one)))

  (define-typerew-binary-variants-type-method 'EXPT
    type:number           type:number          type:number
    effect:none
    type:exact-minus-one  type:exact-integer   type:fix:+1/-1
    type:exact-one        type:exact-integer   type:exact-one
    ;; luckily (EXPT <flonum> 0) => <flonum>
    type:flonum           type:exact-integer   type:flonum)

  (define-typerew-replacement-method 'EXPT 2
    (lambda (form base exponent)
      form				; ignored
      (let* ((t-exponent (typerew/type exponent)))
	(cond ((and (type:subset? t-exponent type:fixnum)
		    (or (equal? base '(QUOTE -1))
			(equal? base '(QUOTE -1.0))))
	       (let ((negative-one (quote/text base)))
		 (lambda (form)
		   form			; ignored
		   `(IF (CALL ',eq? '#F
			      (CALL ',fix:and '#F ,exponent '1)
			      '0)
			',(- negative-one)
			',negative-one))))
	      (else typerew-no-replacement))))))

(let ((type:not-fixnum  (type:not type:fixnum)))
  (define (define-relational-method name fix:op flo:op %op)
    (let ((primitive  (make-primitive-procedure name)))
      (define-typerew-binary-variants-type-method  primitive
	type:number             type:number             type:boolean
	effect:none)

      (define-typerew-binary-variants-replacement-method primitive
	type:fixnum       type:fixnum       type:any      fix:op
	type:fixnum       type:flonum       type:any      (typerew/%l flo:op)
	type:flonum       type:fixnum       type:any      (typerew/%r flo:op)
	type:flonum       type:flonum       type:any      flo:op
	type:not-fixnum   type:any          type:any      %op
	type:any          type:not-fixnum   type:any      %op)

      (define-typerew-binary-variants-type-method  %op
	type:number             type:number             type:boolean
	effect:none)

      (define-typerew-binary-variants-replacement-method %op
	type:fixnum       type:fixnum       type:any      fix:op
	type:fixnum       type:flonum       type:any      (typerew/%l flo:op)
	type:flonum       type:fixnum       type:any      (typerew/%r flo:op)
	type:flonum       type:flonum       type:any      flo:op)))

  (define-relational-method  '&<  fix:<  flo:<  %<)
  (define-relational-method  '&>  fix:>  flo:>  %>))

(let ((&=  (make-primitive-procedure '&=))
      (EQ? (make-primitive-procedure 'EQ?))
      (INTEGER-EQUAL?   (make-primitive-procedure 'INTEGER-EQUAL?))
      (INTEGER-LESS?    (make-primitive-procedure 'INTEGER-LESS?))
      (INTEGER-GREATER? (make-primitive-procedure 'INTEGER-GREATER?))
      (INTEGER-ZERO?  (make-primitive-procedure 'INTEGER-ZERO?))
      (INTEGER-NEGATIVE?  (make-primitive-procedure 'INTEGER-NEGATIVE?))
      (INTEGER-POSITIVE?  (make-primitive-procedure 'INTEGER-POSITIVE?))
      (type:not-fixnum  (type:not type:fixnum)))
  (define-typerew-binary-variants-type-method  &=
    type:number                 type:number             type:boolean
    effect:none)
  (define-typerew-binary-variants-type-method  %=
    type:number                 type:number             type:boolean
    effect:none)
  (define-typerew-binary-variants-type-method  INTEGER-EQUAL?
    type:exact-integer          type:exact-integer      type:boolean
    effect:none)
  (define-typerew-binary-variants-replacement-method  &=
    ;; Representation note: EQ? works for comparing any exact number to a
    ;; fixnum because the generic arithmetic canonicalizes values to
    ;; fixnums wherever possible.
    type:fixnum          type:exact-number   type:any      EQ?
    type:exact-number    type:fixnum         type:any      EQ?
    type:flonum          type:flonum         type:any      flo:=
    type:fixnum          type:flonum         type:any      (typerew/%l flo:=)
    type:flonum          type:fixnum         type:any      (typerew/%r flo:=)
    type:not-fixnum      type:any            type:any      %=
    type:any             type:not-fixnum     type:any      %=)
  (define-typerew-binary-variants-replacement-method  %=
    type:fixnum          type:exact-number   type:any      EQ?
    type:exact-number    type:fixnum         type:any      EQ?
    type:flonum          type:flonum         type:any      flo:=
    type:fixnum          type:flonum         type:any      (typerew/%l flo:=)
    type:flonum          type:fixnum         type:any      (typerew/%r flo:=))

  (define-typerew-binary-variants-replacement-method  INTEGER-EQUAL?
    type:fixnum             type:exact-integer      type:any    EQ?
    type:exact-integer      type:fixnum             type:any    EQ?)

  (define-typerew-binary-variants-replacement-method  INTEGER-LESS?
    type:fixnum             type:fixnum             type:any    fix:<)

  (define-typerew-binary-variants-replacement-method  INTEGER-GREATER?
    type:fixnum             type:fixnum             type:any    fix:>)

  (define-typerew-unary-variants-replacement-method INTEGER-ZERO?
    type:exact-integer     type:any                 (typerew/%lc EQ? 0))

  (define-typerew-unary-variants-replacement-method INTEGER-NEGATIVE?
    type:fixnum            type:any                 (typerew/%lc fix:< 0))

  (define-typerew-unary-variants-replacement-method INTEGER-POSITIVE?
    type:fixnum            type:any                 (typerew/%lc fix:> 0))
)


;; We have no objects which could be EQ? (EQV? EQUAL?) without being the
;; same type. (Numbers are only EQV? or EQUAL? if they have the same
;; exactness.)
(let ((define-equality-disjointness
	(lambda (equality-test)
	  (define-typerew-binary-predicate-type-method equality-test
	    (lambda (form type1 type2)
	      form				; unused
	      (if (type:disjoint? type1 type2)
		  type:false
		  type:boolean))))))
  (define-equality-disjointness EQ?)
  (define-equality-disjointness 'EQV?)
  (define-equality-disjointness 'EQUAL?))

(let ((type:eqv?-is-eq?
       (type:or (type:not type:number) type:fixnum))
      (type:equal?-is-eq?
       (type:or* type:fixnum type:character type:tc-constant type:symbol))
      (EQ?              (make-primitive-procedure 'EQ?)))

  (define-typerew-binary-variants-type-method 'EQV?
    type:any           type:any           type:boolean      effect:none)

  (define-typerew-binary-variants-type-method 'EQUAL?
    type:any           type:any           type:boolean      effect:none)

  (define-typerew-binary-variants-replacement-method 'EQV?
    type:eqv?-is-eq?   type:any           type:any      EQ?
    type:any           type:eqv?-is-eq?   type:any      EQ?)

  (define-typerew-binary-variants-replacement-method 'EQUAL?
    type:equal?-is-eq?   type:any             type:any      EQ?
    type:any             type:equal?-is-eq?   type:any      EQ?))



(define-typerew-binary-predicate-type-method %small-fixnum?
  (let ((type:not-small-fixnum (type:not type:small-fixnum))
	(type:not-fixnum       (type:not type:fixnum)))
    (lambda (form argtype1 argtype2)
      argtype2 ; ignored
      (define (discern type1 type2)
	(cond ((type:disjoint? argtype1 type1)  type:false)
	      ((type:disjoint? argtype1 type2)  type:true)
	      (else                           type:boolean)))
      (let ((n-bits (form/exact-integer? (call/operand2 form))))
	(cond ((= n-bits 1) (discern type:small-fixnum type:not-small-fixnum))
	      ((= n-bits 0) (discern type:fixnum       type:not-fixnum))
	      (else         (discern type:small-fixnum type:any)))))))

(let ()
  (define (def-unary-selector name asserted-type  type-check-class
	    %test %operation)
    ;; No effects.
    (let* ((rator  (make-primitive-procedure name))
	   (checking-replacement
	    (typerew-operator-replacement/diamond-1-1-1
	     %test %operation
	     (typerew/->primitive-error-combination rator)))
	   (unchecked-replacement
	    (typerew-simple-operator-replacement %operation)))

      (define-typerew-replacement-method rator 1
	(lambda (form arg1)
	  form
	  (if (and (typerew/type-checks? type-check-class)
		   (not (type:subset? (typerew/type arg1) asserted-type)))
	      checking-replacement
	      unchecked-replacement)))))

  (def-unary-selector 'CAR type:pair 'PAIR  PAIR?  %car)
  (def-unary-selector 'CDR type:pair 'PAIR  PAIR?  %cdr)
  (def-unary-selector 'VECTOR-LENGTH type:vector 'VECTOR
    (typerew-object-type-test 'VECTOR)
    %vector-length)
  (def-unary-selector '%RECORD-LENGTH type:%record 'RECORD
    (typerew-object-type-test 'RECORD)
    %%record-length)
  (def-unary-selector 'STRING-LENGTH type:string 'STRING
    (typerew-object-type-test 'VECTOR-8B)
    %string-length)
  (def-unary-selector 'FLOATING-VECTOR-LENGTH type:flonum-vector
    'FLOATING-VECTOR
    (typerew-object-type-test 'FLONUM) ;
    %floating-vector-length)
    
  (define (def-unary-mutator name location-type type-check-class
	    %test %operation)
    (let* ((rator  (make-primitive-procedure name))
	   (checking-replacement
	    (typerew-operator-replacement/diamond-1-2-2
	     %test %operation
	     (typerew/->primitive-error-combination rator)))
	   (unchecked-replacement
	    (typerew-simple-operator-replacement %operation)))

      (define-typerew-replacement-method rator 2
	(lambda (form arg1 arg2)
	  form arg2				;
	  (if (and (typerew/type-checks? type-check-class)
		   (not (type:subset? (typerew/type arg1) location-type)))
	      checking-replacement
	      unchecked-replacement)))))
  
  (def-unary-mutator 'SET-CAR! type:pair 'PAIR PAIR? %set-car!)
  (def-unary-mutator 'SET-CDR! type:pair 'PAIR PAIR? %set-cdr!)
  )


(let ()
  ;; For the indexed selectors or mutators we do not even try to figure out
  ;; if the index is in range.  Range checking also performs
  ;; type-checking of the index (via an unsigned comarison).  Note
  ;; that %RECORDs are always created with at least a descriptor slot,
  ;; so an index known to be exact zero does not need a range (or
  ;; type) check.  This is what RANGE-TYPE-OK is for.  If not #F, then
  ;; it is a type describing those index values which never need a
  ;; check.

  (define (def-indexed-operations selector-name mutator-name type-check-class
	    element-type collection-type
	    %selector %mutator v-typecode v-length element-typecode
	    range-ok-type)
    ;; No effects.
    (let ((selector            (make-primitive-procedure selector-name))
	  (unchecked-selection (typerew-simple-operator-replacement %selector)))
      
      (define (make-checked-selection checks)
	(typerew-operator-replacement/diamond-2-2-2
	 (lambda (collection index)
	   `(CALL ',%generic-index-check/ref '#F
		  ,collection ,index (QUOTE ,checks)))
	 (typerew/->binary-combination %selector)
	 (typerew/->primitive-error-combination selector)))
      
      (define-typerew-replacement-method selector 2
	(lambda (form collection index)
	  form index
	  (let ((v-type         (typerew/type collection))
		(i-type         (typerew/type index))
		(type-checks?   (typerew/type-checks? type-check-class))
		(range-checks?  (typerew/range-checks? type-check-class)))
	    (let ((check/1? (and type-checks?
				 (not (type:subset? v-type collection-type))
				 v-typecode))
		  (check/2?		; length check incorporates type check
		   (and (or (and range-checks?
				 (not (and range-ok-type
					   (type:subset? i-type range-ok-type))))
			    (and type-checks?
				 (not (type:subset? i-type type:fixnum))))
			v-length)))
	      (if (or check/1? check/2?)
		  (make-checked-selection (vector check/1? check/2?))
		  unchecked-selection))))))

    (let ((mutator         (make-primitive-procedure mutator-name))
	  (unsafe-mutation (typerew-simple-operator-replacement %mutator)))

      (define (make-checked-mutation checks)
	(typerew-operator-replacement/diamond-3-3-3
	 (lambda (collection index element)
	   `(CALL ',%generic-index-check/set! '#F
		  ,collection ,index ,element (QUOTE ,checks)))
	 %mutator
	 (typerew/->primitive-error-combination mutator)))

      (define-typerew-replacement-method mutator 3
	(lambda (form collection index element)
	  form index
	  (let ((v-type      (typerew/type collection))
		(i-type      (typerew/type index))
		(e-type      (typerew/type element))
		(type-checks?   (typerew/type-checks? type-check-class))
		(range-checks?  (typerew/range-checks? type-check-class)))
	    (let ((check/1? (and type-checks?
				 (not (type:subset? v-type collection-type))
				 v-typecode))
		  (check/2?
		   (and (or (and range-checks?
				 (not (and range-ok-type
					   (type:subset? i-type range-ok-type))))
			    (and type-checks?
				 (not (type:subset? i-type type:fixnum))))
			v-length))
		  (check/3? (and type-checks? element-typecode
				 (not (type:subset? e-type element-type))
				 element-typecode)))
	      (if (or check/1? check/2? check/3?)
		  (make-checked-mutation (vector check/1? check/2? check/3?))
		  unsafe-mutation)))))))

  (def-indexed-operations 'VECTOR-REF  'VECTOR-SET!   'VECTOR
    type:any type:vector
    %vector-ref %vector-set! (machine-tag 'VECTOR) %vector-length #F #F)

  (def-indexed-operations '%RECORD-REF '%RECORD-SET!  'RECORD
    type:any type:%record
    %%record-ref %%record-set! (machine-tag 'RECORD) %%record-length #F
    type:exact-zero)

  (def-indexed-operations 'STRING-REF  'STRING-SET!   'STRING
    type:character type:string
    %string-ref %string-set! (machine-tag 'VECTOR-8B) %string-length
    (machine-tag 'CHARACTER) #F)

  (def-indexed-operations 'VECTOR-8B-REF  'VECTOR-8B-SET!  'STRING
    type:unsigned-byte type:string
    %vector-8b-ref %vector-8b-set! (machine-tag 'VECTOR-8B) %string-length
    (machine-tag 'POSITIVE-FIXNUM) #F)

  (def-indexed-operations
    'FLOATING-VECTOR-REF 'FLOATING-VECTOR-SET!  'FLOATING-VECTOR
    type:flonum type:flonum-vector
    %floating-vector-ref %floating-vector-set! (machine-tag 'FLONUM)
    %floating-vector-length (machine-tag 'FLONUM) #F)
)


(define (typerew/initialize-known-operators!)

  ;; Augment our special knowledge.

  ;; (1) Predicates defined in terms of the types they distinguish:

  (for-every (monotonic-strong-eq-hash-table->alist
	      *operator-predicate-test-types*)
    (lambda (operator.t1.t2)
      (let ((operator (car operator.t1.t2))
	    (types-possibly-true   (cadr operator.t1.t2))
	    (types-possibly-false  (cddr operator.t1.t2)))
	(if (not (monotonic-strong-eq-hash-table/get *typerew/type-methods*
						     operator #F))
	    (define-typerew-unary-predicate-type-method operator
	      (lambda (form argtype)
		form ; unused
		(cond ((type:disjoint? argtype types-possibly-false)
		       type:true)
		      ((type:disjoint? argtype types-possibly-true)
		       type:false)
		      (else type:boolean))))
	    (warn "Already defined:" operator)))))

  ;; (2) Any operations defined in typedb.scm:

  (for-every (monotonic-strong-eq-hash-table->alist *operator-types*)
    (lambda (operator.procedure-type)
      (let ((operator   (car operator.procedure-type))
	    (proc-type  (cdr operator.procedure-type)))
	(if (not (monotonic-strong-eq-hash-table/get *typerew/type-methods*
						     operator #F))
	    (let ((argtypes (procedure-type/argument-assertions proc-type)))
	      (if (list? argtypes)
		  (define-typerew-type-method operator (length argtypes)
		    (typerew/general-type-method
		     operator
		     argtypes
		     (procedure-type/result-type proc-type)
		     (procedure-type/effects-performed proc-type)))
		  (define-typerew-type-method operator #F
		    (typerew/general-type-method
		     operator
		     argtypes
		     (procedure-type/result-type proc-type)
		     (procedure-type/effects-performed proc-type))))))))))
		 

(typerew/initialize-known-operators!)

(define (pp/ann/ty program)
  (let ((type-map *typerew-type-map*)
	(dbg-map  *typerew-dbg-map*)
	(cache    (make-form-map)))	; prevents GC
    dbg-map
    (define (annotate e)
      (or (form-map/get cache e #F)
	  (let ((type  (form-map/get type-map e #F)))
	    (let ((annotation type))
	      (form-map/put! cache e annotation)
	      annotation))))
    (pp/ann program annotate)))