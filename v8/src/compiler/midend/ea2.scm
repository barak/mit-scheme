#| -*-Scheme-*-

$Id: c66ee08e429c83cac5253b8b4dca5496169fb2d1 $

Copyright (c) 1995, 1999 Massachusetts Institute of Technology

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

;;;; Early generic arithmetic rewrite
;;; package: (compiler midend)

(declare (usual-integrations))


;; Affects how careful we are to maintain exactness:
(define *earlyrew/maximize-exactness?* #T)


(define (earlyrew/top-level program)
  ;; 1. Copy the program, doing some expansions and constant folding
  ;; 2. Figure out some types and do some rewrites based on that
  (let ((copy (earlyrew/expr program)))
    (earlyrew/typeinf copy)
    copy))

(define-macro (define-early-rewriter keyword bindings . body)
  (let ((proc-name (symbol-append 'EARLYREW/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (LET ((HANDLER (LAMBDA ,names ,@body)))
	     (NAMED-LAMBDA (,proc-name FORM)
	       (EARLYREW/REMEMBER ,code FORM))))))))

(define-early-rewriter LOOKUP (name)
  `(LOOKUP ,name))

(define-early-rewriter LAMBDA (lambda-list body)
  `(LAMBDA ,lambda-list
     ,(earlyrew/expr body)))

(define-early-rewriter CALL (rator cont #!rest rands)
  (define (default)
    `(CALL ,(earlyrew/expr rator)
	   ,(earlyrew/expr cont)
	   ,@(earlyrew/expr* rands)))
  (cond ((and (QUOTE/? rator)
	      (rewrite-operator/early? (quote/text rator)))
	 => (lambda (handler)
	      (if (not (equal? cont '(QUOTE #F)))
		  (internal-error "Early rewrite done after CPS conversion?"
				  cont))
	      (apply handler (earlyrew/expr* rands))))
	(else
	 (default))))

(define-early-rewriter LET (bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (earlyrew/expr (cadr binding))))
	      bindings)
     ,(earlyrew/expr body)))

(define-early-rewriter LETREC (bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (earlyrew/expr (cadr binding))))
		 bindings)
     ,(earlyrew/expr body)))

(define-early-rewriter QUOTE (object)
  `(QUOTE ,object))

(define-early-rewriter DECLARE (#!rest anything)
  `(DECLARE ,@anything))

(define-early-rewriter BEGIN (#!rest actions)
  `(BEGIN ,@(earlyrew/expr* actions)))

(define-early-rewriter IF (pred conseq alt)
  `(IF ,(earlyrew/expr pred)
       ,(earlyrew/expr conseq)
       ,(earlyrew/expr alt)))

(define (earlyrew/expr expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (earlyrew/quote expr))
    ((LOOKUP)   (earlyrew/lookup expr))
    ((LAMBDA)   (earlyrew/lambda expr))
    ((LET)      (earlyrew/let expr))
    ((DECLARE)  (earlyrew/declare expr))
    ((CALL)     (earlyrew/call expr))
    ((BEGIN)    (earlyrew/begin expr))
    ((IF)       (earlyrew/if expr))
    ((LETREC)   (earlyrew/letrec expr))
    (else       (illegal expr))))

(define (earlyrew/expr* exprs)
  (map earlyrew/expr exprs))

(define (earlyrew/remember new old)
  (code-rewrite/remember new old))

(define (earlyrew/new-name prefix)
  (new-variable prefix))

(define *early-rewritten-operators*
  (make-eq-hash-table))

(define-integrable (rewrite-operator/early? rator)
  (hash-table/get *early-rewritten-operators* rator false))

(define (define-rewrite/early operator-name-or-object handler)
  (hash-table/put! *early-rewritten-operators*
		   (if (know-operator? operator-name-or-object)
		       operator-name-or-object
		       (make-primitive-procedure operator-name-or-object))
		   handler))

;;;; Rewrites of unary operations in terms of binary operations

(let ((unary-rewrite
       (lambda (binary-name rand2)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (rand1)
	     `(CALL (QUOTE ,binary-operation)
		    (QUOTE #F)
		    ,rand1
		    (QUOTE ,rand2))))))
      (special-rewrite
       (lambda (binary-name rand2)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (rand1)
	     `(CALL (QUOTE ,binary-operation)
		    (QUOTE #F)
		    ,rand1
		    (QUOTE ,rand2))))))
      (special-rewrite/left
       (lambda (binary-name rand1)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (rand2)
	     `(CALL (QUOTE ,binary-operation)
		    (QUOTE #F)
		    (QUOTE ,rand1)
		    ,rand2))))))

  (define-rewrite/early 'ZERO?     (unary-rewrite '&= 0))
  (define-rewrite/early 'POSITIVE? (unary-rewrite '&> 0))
  (define-rewrite/early 'NEGATIVE? (unary-rewrite '&< 0))
  (define-rewrite/early '1+        (unary-rewrite '&+ 1))
  (define-rewrite/early '-1+       (unary-rewrite '&- 1))

  (define-rewrite/early 'ZERO-FIXNUM?
    (special-rewrite 'EQUAL-FIXNUM? 0))
  (define-rewrite/early 'NEGATIVE-FIXNUM?
    (special-rewrite 'LESS-THAN-FIXNUM? 0))
  (define-rewrite/early 'POSITIVE-FIXNUM?
    (special-rewrite 'GREATER-THAN-FIXNUM? 0))
  (define-rewrite/early 'ONE-PLUS-FIXNUM
    (special-rewrite 'PLUS-FIXNUM 1))
  (define-rewrite/early 'MINUS-ONE-PLUS-FIXNUM
    (special-rewrite 'MINUS-FIXNUM 1))

  (define-rewrite/early 'FLONUM-ZERO?     (special-rewrite 'FLONUM-EQUAL? 0.))
  (define-rewrite/early 'FLONUM-NEGATIVE? (special-rewrite 'FLONUM-LESS? 0.))
  (define-rewrite/early 'FLONUM-POSITIVE? (special-rewrite 'FLONUM-GREATER? 0.))

  (define-rewrite/early 'FLONUM-NEGATE
    (special-rewrite/left 'FLONUM-SUBTRACT 0.)))

#|
;; Some machines have an ABS instruction.
;; This should be enabled according to the back end.

(define-rewrite/early 'FLONUM-ABS
  (let ((flo:> (make-primitive-procedure 'FLONUM-GREATER?))
	(flo:- (make-primitive-procedure 'FLONUM-SUBTRACT)))
    (lambda (x)
      (let ((x-name (earlyrew/new-name 'X)))
	(bind x-name x
	      `(IF (CALL (QUOTE ,flo:>) (QUOTE #F) (QUOTE 0.) (LOOKUP ,x-name))
		   (CALL (QUOTE ,flo:-) (QUOTE #F) (QUOTE 0.) (LOOKUP ,x-name))
		   (LOOKUP ,x-name)))))))
|#

;;;; *** Special, for now ***
;; This is done this way because of current rtl generator 

(let ((allocation-rewriter
       (lambda (name out-of-line limit)
	 (let ((primitive (make-primitive-procedure name)))
	   (lambda (size)
	     (define (default)
	       `(CALL (QUOTE ,out-of-line) (QUOTE #F) ,size))
	     (cond ((form/number? size)
		    => (lambda (nbytes)
			 (if (not (and (exact-nonnegative-integer? nbytes)
				       (<= nbytes limit)))
			     (default)
			     `(CALL (QUOTE ,primitive) (QUOTE #F) ,size))))
		   (else
		    (default))))))))
  (define-rewrite/early 'STRING-ALLOCATE
    (allocation-rewriter 'STRING-ALLOCATE %string-allocate
			 *string-allocate-max-open-coded-length*))
  (define-rewrite/early 'FLOATING-VECTOR-CONS
    (allocation-rewriter 'FLOATING-VECTOR-CONS %floating-vector-cons 
			 *floating-vector-cons-max-open-coded-length*)))

;; *** This can be improved by using %vector-allocate,
;; and a non-marked header moved through the vector as it is filled. ***

(define-rewrite/early 'VECTOR-CONS
  (let ((primitive (make-primitive-procedure 'VECTOR-CONS)))
    (lambda (size fill)
      (define (default)
	`(CALL (QUOTE ,%vector-cons) (QUOTE #F) ,size ,fill))
      (cond ((form/number? size)
	     => (lambda (nbytes)
		  (if (or (not (exact-nonnegative-integer? nbytes))
			  (> nbytes *vector-cons-max-open-coded-length*))
		      (default)
		      `(CALL (QUOTE ,primitive) (QUOTE #F) ,size ,fill))))
	    (else
	     (default))))))


(define-rewrite/early 'GENERAL-CAR-CDR
  (let ((prim-general-car-cdr (make-primitive-procedure 'GENERAL-CAR-CDR))
        (prim-car             (make-primitive-procedure 'CAR))
        (prim-cdr             (make-primitive-procedure 'CDR)))
    (lambda (term pattern)
      (define (default)
	`(CALL (QUOTE ,prim-general-car-cdr) (QUOTE #f) ,term ,pattern))
      (cond ((form/number? pattern)
	     => (lambda (pattern)
		  (if (and (integer? pattern) (> pattern 0))
		      (let walk-bits ((num  pattern)
				      (text term))
			(if (= num 1)
			    text
			    (walk-bits (quotient num 2)
				       `(CALL (QUOTE ,(if (odd? num)
							  prim-car
							  prim-cdr))
					      (QUOTE #f)
					      ,text))))
		      (default))))
	    (else (default))))))


#|
(define (define-rewrite/early/global name arity handler)
  (let ((slot (hash-table/get *early-rewritten-operators* name '())))
    (hash-table/put! *early-rewritten-operators*
		     name
		     (cons (cons arity handler) slot))))

(define-rewrite/early %invoke-remote-cache 
  (lambda (descriptor operator-cache . values)
    (define (default values)
      `(CALL (QUOTE ,%invoke-remote-cache)
	     (QUOTE #f)
	     ,descriptor
	     ,operator-cache
	     ,@values))
    (let* ((descriptor* (quote/text descriptor))
	   (name  (first descriptor*))
	   (arity (second descriptor*)))
      (cond ((rewrite-operator/early? name)
	     => (lambda (alist)
		  (cond ((assq arity alist)
			 => (lambda (arity.handler)
			      (apply (cdr arity.handler) default values)))
			(else (default values)))))
	    (else
	     (default values))))))
|#


;;______________________________________________________________________
;;
;; Type-aware rewriting for generic arithmetic,
;;  . Traverse the program and compute type information.  At this time a
;;    rewrite may be decided and `posted'.
;;  . Traverse the program again, applying posted and other rewrites in some
;;    depth first ordering.


(define *earlyrew/typemap*)
(define *earlyrew/posted-rewrites*)

(define (earlyrew/typeinf program)
  (fluid-let ((*earlyrew/typemap* (make-eq-hash-table))
	      (*earlyrew/posted-rewrites* (make-eq-hash-table)))
    (earlyrew/typeinf/expr *earlyrew/typemap* program)
    (earlyrew/rewrite!/top-level program)
    unspecific))

(define-macro (define-early-type-inferencer keyword bindings . body)
  (let ((proc-name (symbol-append 'EARLYREW/TYPEINF/ keyword)))
    (call-with-values
	(lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name ENV FORM)
	     ;; FORM is in scope in HANDLER
	     (LET ((HANDLER (LAMBDA ,(cons (car bindings) names) ,@body)))
	       ,code)))))))


(define-early-type-inferencer LOOKUP (env name)
  (earlyrew/typeinf/env/lookup env name))

(define-early-type-inferencer LAMBDA (env lambda-list body)
  (let* ((names  (lambda-list->names lambda-list))
	 (types  (make-list (length names) earlyrew/type/*unknown))
	 (env*   (earlyrew/typeinf/env/extend env names types)))
    (earlyrew/typeinf/expr env* body))
  earlyrew/type/*procedure)

(define-early-type-inferencer CALL (env rator cont #!rest rands)
  (let ((rand-types  (earlyrew/typeinf/expr* env rands))
	(rand-count  (length rands)))
    (define (default) earlyrew/type/*unknown)
    (define (apply-handler handler rand-types)
      (if (not (equal? cont '(QUOTE #F)))
	  (internal-error "Early rewrite done after CPS conversion?"
			  cont))
      (apply handler form rand-types))
    (cond ((QUOTE/? rator)
	   (cond ((earlyrew/type-method? (quote/text rator) rand-count)
		  => (lambda (handler)
		       (apply-handler handler rand-types)))
		 ((eq? (quote/text rator) %invoke-remote-cache)
		  (let ((descriptor  (quote/text (first rands))))
		    (cond ((earlyrew/type-method? (first descriptor)
						  (second descriptor))
			   => (lambda (handler)
				(apply-handler handler (cddr rand-types))))
			  (else (default)))))
		 (else (default))))
	  ((LAMBDA/? rator)
	   (hash-table/put! *earlyrew/typemap* rator earlyrew/type/*procedure)
	   (let* ((names  (cdr (lambda/formals rator)))
		  (env*   (earlyrew/typeinf/env/extend env names rand-types)))
	     (earlyrew/typeinf/expr env* (lambda/body rator))))
	  (else
	   (earlyrew/typeinf/expr env rator)
	   (default)))))

(define-early-type-inferencer LET (env bindings body)
  (let* ((names  (map first bindings))
	 (types  (map (lambda (binding)
			(earlyrew/typeinf/expr env (cadr binding)))
		      bindings))
	 (env*   (earlyrew/typeinf/env/extend env names types)))
    (earlyrew/typeinf/expr env* body)))

(define-early-type-inferencer LETREC (env bindings body)
  (let* ((names (map first bindings))
	 (types (map (lambda (ignored) ignored earlyrew/type/*procedure)
		     bindings))
	 (env*  (earlyrew/typeinf/env/extend env names types)))
    (earlyrew/typeinf/expr* env* (map second bindings))
    (earlyrew/typeinf/expr env* body)))

(define-early-type-inferencer QUOTE (env object)
  env ; ignored
  (earlyrew/typeinf/type-of-constant object))

(define-early-type-inferencer DECLARE (env #!rest anything)
  env anything				; ignored
  earlyrew/type/*illegal-type)

(define-early-type-inferencer BEGIN (env #!rest actions)
  (let  ((types (earlyrew/typeinf/expr* env actions)))
    (if (equal? (first actions) '(DECLARE (RESULT-TYPE FLONUM)))
	earlyrew/type/*flonum
	(car (last-pair types)))))

(define-early-type-inferencer IF (env pred conseq alt)
  (earlyrew/typeinf/expr env pred)
  (earlyrew/type/lub  (earlyrew/typeinf/expr env conseq)
		      (earlyrew/typeinf/expr env alt)))

(define (earlyrew/typeinf/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (let ((type
	 (case (car expr)
	   ((QUOTE)     (earlyrew/typeinf/quote   env expr))
	   ((LOOKUP)    (earlyrew/typeinf/lookup  env expr))
	   ((LAMBDA)    (earlyrew/typeinf/lambda  env expr))
	   ((LET)       (earlyrew/typeinf/let     env expr))
	   ((DECLARE)   (earlyrew/typeinf/declare env expr))
	   ((CALL)      (earlyrew/typeinf/call    env expr))
	   ((BEGIN)     (earlyrew/typeinf/begin   env expr))
	   ((IF)        (earlyrew/typeinf/if      env expr))
	   ((LETREC)    (earlyrew/typeinf/letrec  env expr))
	   (else        (illegal expr)))))
    ;;(if (not (fixnum? type))
    ;;	(internal-error "Not a type" type expr))
    ;;Remove complex numbers:
    ;;(set! type (fix:and type (fix:not (fix:or earlyrew/type/*exact-recnum earlyrew/type/*inexact-recnum))))
    (hash-table/put! *earlyrew/typemap* expr type)
    type))

(define (earlyrew/typeinf/expr* env exprs)
  (map (lambda (expr)
	 (earlyrew/typeinf/expr env expr))
       exprs))

(define (earlyrew/typeinf/env/lookup env name)
  (cond ((hash-table/get env name #F))
	(else (free-var-error name))))

(define (earlyrew/typeinf/env/extend env names0 types0)
  (define (extend! name type)
    (cond ((hash-table/get env name #F)
	   (internal-error "Not alpha-converted? Name already defined:" name))
	  (else
	   (hash-table/put! env name type))))
  (let loop ((names names0) (types types0) (optionals? #F))
    (cond ((and (null? names) (null? types))
	   env)
	  ((and optionals? (null? types))
	   env)
	  ((or (null? names) (null? types))
	   (internal-error "Mismatch" names0 types0))
	  ((eq? (car names) #!optional)
	   (loop (cdr names) types #T))
	  ((eq? (car names) #!aux)
	   (loop (cdr names) types #T))
	  ((eq? (car names) #!rest)
	   (extend! (second names) earlyrew/type/*unknown)
	   env)
	  (else
	   (extend! (car names) (car types))
	   (loop (cdr names) (cdr types) optionals?)))))

(define (earlyrew/form/type form)
  (let ((type (hash-table/get *earlyrew/typemap* form #F)))
    (or type
	(internal-warning "Form not annotated with type:" form))))

(let-syntax ((primitive-types
	      (macro names0
		(define (definer name value)
		  `(DEFINE ,(symbol-append 'EARLYREW/TYPE/ name) ',value))
		(let loop ((names names0) (value 1) (defs '()))
		  (if (null? names)
		      `(BEGIN ,(definer '*UNKNOWN (- value 1))
			      (DEFINE EARLYREW/TYPE/TYPE-NAMES
				',(list->vector names0))
			      ,@defs)
		      (loop (cdr names)
			    (* value 2)
			    (cons (definer (car names) value) defs)))))))
  (primitive-types  *exact-zero		; special numbers...
		    *exact-one
		    *exact-minus-one
		    *small-fixnum>1	; numbers which won't overflow
		    *small-fixnum<-1	;  if added or subtracted
		    *big-fixnum+ve	; other fixnums
		    *big-fixnum-ve	; other fixnums
		    *bignum
		    *ratnum
		    *flonum
		    *exact-recnum
		    *inexact-recnum
		    *other		; anything else
		    ))

(define (earlyrew/type/union . ts)
  (reduce fix:or 0 ts))

(let-syntax ((alias
	      (macro (name . parts)
		(define (->name name)
		  (symbol-append 'EARLYREW/TYPE/ name))
		`(DEFINE ,(->name name)
		   (EARLYREW/TYPE/UNION ,@(map ->name parts))))))
  (alias *empty)
  (alias *small-fixnum+ve  *exact-one *small-fixnum>1)
  (alias *small-fixnum-ve  *exact-minus-one *small-fixnum<-1)
  (alias *small-fixnum 	   *exact-zero *small-fixnum-ve *small-fixnum+ve)
  (alias *big-fixnum       *big-fixnum-ve *big-fixnum+ve)
  (alias *fixnum           *small-fixnum *big-fixnum)
  (alias *exact-integer    *fixnum *bignum)
  (alias *exact-real       *fixnum *bignum *ratnum)
  (alias *inexact-real     *flonum)
  (alias *real             *exact-real *inexact-real)
  (alias *recnum           *exact-recnum *inexact-recnum)
  (alias *exact-number     *exact-real *exact-recnum)
  (alias *inexact-number   *inexact-real *inexact-recnum)
  (alias *number           *real *recnum)

  (alias *small-non-negative-fixnum  *exact-zero *small-fixnum+ve)
  (alias *non-negative-fixnum  *exact-zero *small-fixnum+ve *big-fixnum+ve)
  (alias *unsigned-byte        *exact-zero *exact-one *small-fixnum>1)
  (alias *procedure *other)
  (alias *boolean *other)
  (alias *vector-length  *exact-zero *small-fixnum+ve)
  (alias *string-length  *non-negative-fixnum))

(define earlyrew/type/*illegal-type 'ILLEGAL-TYPE)

(define (earlyrew/typeinf/type->description t)
  (cond ((eq? t earlyrew/type/*unknown)    '(*unknown))
	((eq? t earlyrew/type/*number)     '(*number))
	((eq? t earlyrew/type/*fixnum)     '(*fixnum))
	(else
	 (let loop ((bit 1) (index 0))
	   (if (< bit earlyrew/type/*unknown)
	       (if (zero? (fix:and t bit))
		   (loop (* bit 2) (+ index 1))
		   (cons (vector-ref earlyrew/type/type-names index)
			 (loop (* bit 2) (+ index 1))))
	       '())))))


(define-structure ea/pp/annotation
  text
  type)

(define (pp/ann/ty program)
  (let ((old-browser:print browser:print))
    (let ((ht (make-eq-hash-table)))
      (define (ppt form)
	(let  ((ann (hash-table/get ht form #F)))
	  (pp form)
	  (if (ea/pp/annotation? ann)
	      (let  ((type  (ea/pp/annotation-type ann)))
		(newline)
		(display (unsigned-integer->bit-string 16 type))
		(pp (earlyrew/typeinf/type->description type)))
	      (pp type))))
      (hash-table/for-each *earlyrew/typemap*
	(lambda (node type)
	  (if (pair? node)
	      (hash-table/put! ht node (make-ea/pp/annotation node type)))))
      (fluid-let
	  ((browser:print
	    (lambda (object)
	      (if (ea/pp/annotation? object)
		  (let ((form (ea/pp/annotation-text object)))
		    (fluid-let ((*unparser-list-depth-limit* 4))
		      (pp form))
		    (newline)
		    (fluid-let ((*unparser-list-depth-limit* 1))
		      (if (call/? form)
			  (begin
			    (display "\n<H2>Argument types</H2>\n")
			    (for-each (lambda (part)
					(ppt part)
					(display "\n"))
			      (if (call/%invoke-remote-cache? form)
				  (cddr (cdddr form))
				  (cdddr form)))))
		      (display "\n\n<H2>Result type</H2>\n")
		      (ppt form)))
		  (old-browser:print object)))))
	(pp/ann program ht)))))
	  
		   
(define earlyrew/typeinf/type-of-constant
  (let* ((max-fixnum        (object-new-type 0 -1))
	 (max-small-fixnum  (quotient max-fixnum 2))
	 (min-small-fixnum  (- -1 max-small-fixnum)))
    (lambda (value)
      (cond ((fixnum? value)
	     (cond ((eqv? value 0)   earlyrew/type/*exact-zero)
		   ((eqv? value 1)   earlyrew/type/*exact-one)
		   ((eqv? value -1)  earlyrew/type/*exact-minus-one)
		   ((<= 2 value max-small-fixnum)
		    earlyrew/type/*small-fixnum>1)
		   ((<= min-small-fixnum value -2)
		    earlyrew/type/*small-fixnum<-1)
		   ((< value 0)
		    earlyrew/type/*big-fixnum-ve)
		   (else earlyrew/type/*big-fixnum+ve)))
	    ((exact-integer? value)
	     earlyrew/type/*bignum)
	    ((exact-rational? value)
	     earlyrew/type/*ratnum)
	    ((flo:flonum? value)
	     earlyrew/type/*flonum)
	    ((complex? value)
	     (if (exact? value)
		 earlyrew/type/*exact-recnum
		 earlyrew/type/*inexact-recnum))
	    (else
	     earlyrew/type/*other)))))

(define (earlyrew/type/lub t1 t2)
  (if (and (fixnum? t1)
	   (fixnum? t2))
      (fix:or t1 t2)
      (internal-error "LUB:" t1 t2)))

(define (earlyrew/type/intersection t1 t2)
  (if (and (fixnum? t1)
	   (fixnum? t2))
      (fix:and t1 t2)
      (internal-error "INTERSECTION:" t1 t2)))

(define (earlyrew/subtype? sub super)
  (if (and (fixnum? sub)
	   (fixnum? super))
      (fix:= super (fix:or sub super))
      (internal-error "SUBTYPE:" sub super)))

(define (earlyrew/could-be? type what)
  (not (fix:= (fix:and type what) 0)))

(define (earlyrew/type=? t1 t2) (fix:= t1 t2))

(define (earlyrew/type/not t)
  (if (fixnum? t)
      (fix:andc earlyrew/type/*unknown t)
      (internal-error "SUBTYPE:" sub super)))
  
(define (earlyrew/closed-on closed-type tu tv subject-type)
  (if (and (earlyrew/subtype? tu closed-type)
	   (earlyrew/subtype? tv closed-type))
      (earlyrew/type/intersection subject-type closed-type)
      subject-type))

(define (earlyrew/binary-exactness-contagion tu tv subject-type)
  (cond
   ((and (earlyrew/subtype? tu earlyrew/type/*exact-number)
	 (earlyrew/subtype? tv earlyrew/type/*exact-number))
    (earlyrew/type/intersection subject-type earlyrew/type/*exact-number))
   ((or (earlyrew/subtype? tu earlyrew/type/*inexact-number)
	(earlyrew/subtype? tv earlyrew/type/*inexact-number))
    (earlyrew/type/intersection subject-type earlyrew/type/*inexact-number))
   (else
    subject-type)))
  
(define *earlyrew-typeinf-operators* (make-eq-hash-table))

(define (earlyrew/type-method? operator arity)
  (cond ((hash-table/get *earlyrew-typeinf-operators* operator #F)
	 => (lambda (alist)
	      (cond ((assq arity alist) => cdr)
		    (else #F))))
	(else #F)))

(define (define-early-type-method name arity handler)
  (let ((slot  (hash-table/get *earlyrew-typeinf-operators* name '())))
    (hash-table/put! *earlyrew-typeinf-operators*
		     name
		     (cons (cons arity handler) slot)))
  name)


(let* ((unary-result
	(lambda (type #!optional arg-type)
	  (default-object? arg-type)	; ignored
	  (lambda (op)
	    (define-early-type-method op 1
	      (lambda (form u) form u  type)))))
       (binary-result
	(lambda (type #!optional arg1-type arg2-type)
	  (default-object? arg1-type)	; ignored
	  (default-object? arg2-type)	; ignored
	  (lambda (op)
	    (define-early-type-method op 2
	      (lambda (form u v) form u v  type)))))
       (do-each
	(lambda (op . args) (for-each op args))))

  (do-each (unary-result earlyrew/type/*fixnum)
	   fix:-1+ fix:1+ fix:not)
  (do-each (binary-result earlyrew/type/*fixnum)
	   fix:+ fix:- fix:* fix:quotient fix:remainder
	   fix:andc fix:and fix:or fix:xor fix:lsh)
  (do-each (unary-result earlyrew/type/*flonum)
	   flo:negate flo:abs flo:sqrt
	   flo:floor flo:ceiling flo:truncate flo:round
	   flo:exp flo:log flo:sin flo:cos flo:tan flo:asin
	   flo:acos flo:atan)
  (do-each (binary-result earlyrew/type/*flonum)
	   flo:+ flo:- flo:* flo:/ flo:atan2 flo:expt)

  (do-each (unary-result earlyrew/type/*boolean)
	   not eq? null? false?
	   boolean? cell? pair? vector? %record? string?
	   fixnum? index-fixnum? flo:flonum?)

  (do-each (binary-result earlyrew/type/*boolean)
	   (make-primitive-procedure '&=)
	   (make-primitive-procedure '&<)
	   (make-primitive-procedure '&>)
	   fix:= fix:> fix:< fix:<= fix:>=
	   flo:= flo:> flo:<
	   object-type?)

  (do-each (unary-result earlyrew/type/*unsigned-byte)
	   char-code char->ascii)

  (do-each (binary-result earlyrew/type/*unsigned-byte)
	   vector-8b-ref)

  (do-each (binary-result earlyrew/type/*flonum)
	   flo:vector-ref)

  (do-each (unary-result earlyrew/type/*small-fixnum+ve)
	   char->integer)

  (do-each (unary-result earlyrew/type/*vector-length)
	   vector-length  flo:vector-length  length)

  ;;((unary-result earlyrew/type/*small-non-negative-fixnum) string-length)
  (do-each (unary-result earlyrew/type/*string-length)
	   string-length bit-string-length)

  ((unary-result earlyrew/type/*small-fixnum) object-type)
  )


(define (earlyrew/rewrite-operator! replacement-op)
  (lambda (form)
    (form/rewrite! (call/operator form) `(QUOTE ,replacement-op))))

(define (earlyrew/rewrite-diamond  gen-test-x gen-x-type
				   gen-test-y gen-y-type
				   cheap-op costly-op)
  (define (test-will-fail? type type-test-checks-for)
    (earlyrew/subtype? type (earlyrew/type/not type-test-checks-for)))
  (define (test-will-succeed? type type-test-checks-for)
    (earlyrew/subtype? type type-test-checks-for))

  (define (generate-test var t gen-test gen-type)
    (cond ((test-will-fail? t gen-type)  `(QUOTE ,#F))
	  ((test-will-succeed? t gen-type) `(QUOTE ,#T))
	  (else (gen-test `(LOOKUP ,var)))))
  (let ((rewrite-costly (earlyrew/rewrite-operator! costly-op))
	(rewrite-cheap  (earlyrew/rewrite-operator! cheap-op)))
    (lambda (form tx ty)
      form				; ignored
      (cond ((or (test-will-fail? tx gen-x-type)
		 (test-will-fail? ty gen-y-type))
	     (rewrite-costly form))
	    ((and (test-will-succeed? tx gen-x-type)
		  (test-will-succeed? ty gen-y-type))
	     (rewrite-cheap form))
	    (else
	     (let* ((x-name  (earlyrew/new-name 'X))
		    (y-name  (earlyrew/new-name 'Y))
		    (x-test  (generate-test x-name tx gen-test-x gen-x-type))
		    (y-test  (generate-test y-name ty gen-test-y gen-y-type)))
	       (form/rewrite! form
		 (bind x-name (first (call/operands form))
		       (bind y-name (second (call/operands form))
			     `(IF ,(andify x-test y-test)
				  (CALL (QUOTE ,cheap-op)
					(QUOTE #F)
					(LOOKUP ,x-name)
					(LOOKUP ,y-name))
				  (CALL (QUOTE ,costly-op)
					(QUOTE #F)
					(LOOKUP ,x-name)
					(LOOKUP ,y-name))))))))))))


(let ()
  (define (plus/minus-pre form tu tv)
    form				; ignored
    (cond ((and (earlyrew/subtype? tu earlyrew/type/*small-fixnum)
		(earlyrew/subtype? tv earlyrew/type/*small-fixnum))
	   earlyrew/type/*fixnum)
	  (else
	   earlyrew/type/*number)))

  (define (standard-binary-method name pre post)
    (define-early-type-method name 2
      (lambda (form tu tv)
	(let* ((result  (pre form tu tv))
	       (result*
		(earlyrew/binary-exactness-contagion
		 tu tv
		 (earlyrew/closed-on earlyrew/type/*real tu tv result)))
	       (result** 
		(or (and post (post form result* tu tv)) result*)))
	  result**))))

  (define (number-pre form tu tv) form tu tv  earlyrew/type/*number)
  (define (*-post form result tu tv)
    form ; ignored
    (if (or (earlyrew/subtype? earlyrew/type/*exact-zero tu)
	    (earlyrew/subtype? earlyrew/type/*exact-zero tv))
	(fix:or earlyrew/type/*exact-zero result)
	result))

  (standard-binary-method (make-primitive-procedure '&+)  plus/minus-pre #F)
  (standard-binary-method (make-primitive-procedure '&-)  plus/minus-pre #F)
  (standard-binary-method (make-primitive-procedure '&*)  number-pre *-post)
  (standard-binary-method (make-primitive-procedure '&/)  number-pre #F)
  )


(define-early-type-method 'EXACT->INEXACT 1
  (lambda (form arg-type)
    form				; ignored
    (cond ((earlyrew/subtype? arg-type earlyrew/type/*real)
	   earlyrew/type/*inexact-real)
	  ((earlyrew/subtype? arg-type earlyrew/type/*recnum)
	   earlyrew/type/*inexact-recnum)
	  (else
	   earlyrew/type/*inexact-number))))

(define-early-type-method 'SQRT 1
  (lambda (form arg-type)
    form				; ignored
    (cond ((earlyrew/subtype? arg-type earlyrew/type/*non-negative-fixnum)
	   earlyrew/type/*real)
	  ((earlyrew/subtype? arg-type earlyrew/type/*inexact-number)
	   earlyrew/type/*inexact-number)
	  (else
	   earlyrew/type/*number))))

(define-early-type-method 'EXPT 2
  (let ((type:+1/-1 (earlyrew/type/union earlyrew/type/*exact-one
					 earlyrew/type/*exact-minus-one)))
    (lambda (form t-base t-exponent)
      form				; ignored
      (cond ((and (earlyrew/subtype? t-base earlyrew/type/*exact-minus-one)
		  (earlyrew/subtype? t-exponent earlyrew/type/*exact-integer))
	     type:+1/-1)
	    ((earlyrew/subtype? t-exponent earlyrew/type/*exact-integer)
	     (fix:or earlyrew/type/*exact-one t-base))
	    (else
	     earlyrew/type/*number)))))

(let ()
  (define (unary/2 name input1 output1 input2 output2)
    (define-early-type-method name 1
      (lambda (form arg-type)
	form				; ignored
	(cond ((earlyrew/subtype arg-type input1)    output1)
	      ((earlyrew/subtype arg-type input2)    output2)
	      (else                     	     earlyrew/type/*number)))))
  (define (unary/3 name input1 output1 input2 output2 input3 output3)
    (define-early-type-method name 1
      (lambda (form arg-type)
	form				; ignored
	(cond ((earlyrew/subtype arg-type input1)    output1)
	      ((earlyrew/subtype arg-type input2)    output2)
	      ((earlyrew/subtype arg-type input3)    output3)
	      (else                       	     earlyrew/type/*number)))))
  (unary/2 'SIN
	   earlyrew/type/*exact-zero earlyrew/type/*exact-zero
	   earlyrew/type/*real       earlyrew/type/*flonum)
  (unary/2 'COS
	   earlyrew/type/*exact-zero earlyrew/type/*exact-one
	   earlyrew/type/*real       earlyrew/type/*flonum)
  (unary/2 'TAN
	   earlyrew/type/*exact-zero earlyrew/type/*exact-zero
	   earlyrew/type/*real       earlyrew/type/*flonum)
  (unary/2 'ACOS
	   earlyrew/type/*exact-one  earlyrew/type/*exact-zero
	   earlyrew/type/*unknown    earlyrew/type/*inexact-number)
  (unary/2 'ASIN
	   earlyrew/type/*exact-zero earlyrew/type/*exact-zero
	   earlyrew/type/*unknown    earlyrew/type/*inexact-number)
  (unary/3 'EXP
	   earlyrew/type/*recnum     earlyrew/type/*inexact-recnum
	   earlyrew/type/*exact-zero earlyrew/type/*exact-one
	   earlyrew/type/*real       earlyrew/type/*inexact-real)
  (unary/2 'LOG
	   earlyrew/type/*exact-one  earlyrew/type/*exact-zero
	   earlyrew/type/*number     earlyrew/type/*inexact-number)  )

      
#|
(define-early-type-method 'EXPT 2
  (let ((&* (make-primitive-procedure '&*))
	(max-multiplies 3))
    (lambda (form base exponent)
      (define (make-product x y)
	`(CALL (QUOTE ,&*)
	       (QUOTE #F)
	       ,x ,y))
      (define (count-multiplies n)
	(cond ((= n 1) 0)
	      ((= n 2) 1)
	      ((even? n) (+ (count-multiplies (/ n 2)) 1))
	      ((odd? n)  (+ (count-multiplies (- n 1)) 1))))
      (define (power variable n)
	(cond ((= n 1) variable)
	      ((= n 2) (make-product variable variable))
	      ((even? n)
	       (let ((square (earlyrew/new-name 'X)))
		 (bind square (make-product variable variable)
		       (power `(LOOKUP ,square) (/ n 2)))))
	      ((odd? n)
	       (make-product variable (power variable (- n 1))))))	       
		       
      (cond ((form/number? exponent)
	     => (lambda (exponent)
		  (cond ((form/number? base)
			 => (lambda (base)
			      `(QUOTE ,(expt base exponent))))
			((eqv? exponent 0)
			 `(QUOTE 1))
			((eqv? exponent 1)
			 base)
			((and (exact-integer? exponent)
			      (>= exponent 2)
			      (<= (count-multiplies exponent) max-multiplies))
			 (let* ((base-name  (earlyrew/new-name 'X))
				(expression (power `(LOOKUP ,base-name) exponent)))
			   (bind base-name base
				 expressions)))
			(else (default)))))
	    (else
	     (default))))))
|#

(define (earlyrew/post-rewrite form rewrite)
  (hash-table/put! *earlyrew/posted-rewrites* form rewrite))

(define (earlyrew/posted-rewrite? form)
  (hash-table/get *earlyrew/posted-rewrites* form #F))

(define (earlyrew/rewrite!/top-level form)
  (earlyrew/rewrite! form))

(define (earlyrew/rewrite! form)
  (define (rewrite* forms)
    (for-each earlyrew/rewrite! forms))
  (define (let&rec bindings body)
    (for-each (lambda (bindings) (earlyrew/rewrite! (second bindings)))
      bindings)
    (earlyrew/rewrite! body))
  (cond ((QUOTE/? form))
	((LOOKUP/? form))
	((CALL/? form)
	 (let ((operator  (call/operator form))
	       (operands  (call/operands form)))
	   (define (try name arity rands)
	     (let ((handler  (earlyrew/type-rewrite? name arity)))
	       (if handler
		   (apply handler form rands))))
	   (earlyrew/rewrite! operator)
	   (rewrite* (call/cont-and-operands form))
	   (cond ((earlyrew/posted-rewrite? form)
		  => (lambda (rewrite!) (rewrite! form)))
		 ((not (QUOTE/? operator)) #F)
		 ((eq? (quote/text operator) %invoke-remote-cache)
		  (try (first  (quote/text (first operands)))
		       (second (quote/text (first operands)))
		       (cddr operands)))
		 (else
		  (try (quote/text operator) (length operands) operands)))))
	((LAMBDA/? form)  (earlyrew/rewrite! (lambda/body form)))
	((IF/? form)      (rewrite* (cdr form)))
	((LET/? form)     (let&rec (let/bindings form) (let/body form)))
	((LETREC/? form)  (let&rec (letrec/bindings form) (letrec/body form)))
	((BEGIN/? form)   (rewrite* (begin/exprs form)))
	((DECLARE/? form))
	(else (illegal form))))


(define *earlyrew/type-rewrites* (make-eq-hash-table))

(define (define-type-rewrite name arity handler)
  (let ((alist (hash-table/get *earlyrew/type-rewrites* name '())))
    (hash-table/put! *earlyrew/type-rewrites*
		     name
		     (cons (cons arity handler) alist)))
  name)

(define (earlyrew/type-rewrite? name arity)
  (let ((alist (hash-table/get *earlyrew/type-rewrites* name '())))
    (and alist
	 (let ((pair (or (assq arity alist)  (assq 'any alist))))
	   (and pair
		(cdr pair))))))

(define earlyrew/flonum-test
  (let ((flonum?  (make-primitive-procedure 'FLONUM?)))
    (lambda (subject)
      `(CALL (QUOTE ,flonum?) '#F ,subject))))

(let ()
  (define (small-fixnum-test/1 subject)
    `(CALL (QUOTE ,%small-fixnum?) '#F ,subject '1))
  (define (additive fix:op flo:op out-of-line:op)
    (let ((fix:diamond (earlyrew/rewrite-diamond
			small-fixnum-test/1 earlyrew/type/*small-fixnum
			small-fixnum-test/1 earlyrew/type/*small-fixnum
			fix:op out-of-line:op))
	  (flo:diamond (earlyrew/rewrite-diamond
			earlyrew/flonum-test earlyrew/type/*flonum
			earlyrew/flonum-test earlyrew/type/*flonum
			flo:op out-of-line:op)))
      (lambda (form x y)
	(let  ((tx  (earlyrew/form/type x))
	       (ty  (earlyrew/form/type y)))
	  (cond ((or (earlyrew/subtype? tx earlyrew/type/*flonum)
		     (earlyrew/subtype? ty earlyrew/type/*flonum))
		 (flo:diamond form tx ty))
		(else
		 (fix:diamond form tx ty)))))))

  (define (define-additive name fix:op flo:op out:op)
    (define-type-rewrite (make-primitive-procedure name) 2
      (additive fix:op flo:op out:op)))

  (define-additive '&+ fix:+ flo:+ %+)
  (define-additive '&- fix:- flo:- %-)

  (define-additive '&< fix:< flo:< %<)
  (define-additive '&= fix:= flo:= %=)
  (define-additive '&> fix:> flo:> %>))

(define-type-rewrite (make-primitive-procedure '&*) 2
  (let ((rewrite-out-of-line (earlyrew/rewrite-operator! %*))
	(multiply-fixnum  (make-primitive-procedure 'MULTIPLY-FIXNUM))
	(flo:diamond (earlyrew/rewrite-diamond
		      earlyrew/flonum-test earlyrew/type/*flonum
		      earlyrew/flonum-test earlyrew/type/*flonum
		      flo:* %*)))
    (define (small-fixnum-multiply? cst multiplicand-type)
      (and (QUOTE/? cst)
	   (good-factor? (quote/text cst))
	   (earlyrew/could-be? multiplicand-type earlyrew/type/*fixnum)))
    (lambda (form x y)
      (define (small-fixnum-multiply name cst multiplicand multiplicand-type)
	(let* ((name     (earlyrew/new-name name))
	       (constant (quote/text cst)))
	  (form/rewrite! form
	    (bind name multiplicand
		  `(IF (CALL (QUOTE ,%small-fixnum?)
			     (QUOTE #F)
			     (LOOKUP ,name)
			     (QUOTE ,(good-factor->nbits constant)))
		       (CALL (QUOTE ,multiply-fixnum)
			     (QUOTE #F)
			     (LOOKUP ,name)
			     (QUOTE ,constant))
		       (CALL (QUOTE ,%*)
			     (QUOTE #F)
			     (LOOKUP ,name)
			     (QUOTE ,constant)))))))
      (let  ((tx  (earlyrew/form/type x))
	     (ty  (earlyrew/form/type y)))
	(cond ((or (earlyrew/subtype? tx earlyrew/type/*flonum)
		   (earlyrew/subtype? ty earlyrew/type/*flonum))
	       (flo:diamond form tx ty))
	      ((small-fixnum-multiply? x ty)
	       (small-fixnum-multiply 'X x y ty))
	      ((small-fixnum-multiply? y tx)
	       (small-fixnum-multiply 'Y y x tx))
	      (else
	       (rewrite-out-of-line form)))))))

(define-type-rewrite 'EXPT 2
  (lambda (form base exponent)
    (let ((t-b  (earlyrew/form/type base))
	  (t-e  (earlyrew/form/type exponent)))
      (cond ((not (QUOTE/? base)) unspecific)
	    ((and (or (eqv? (quote/text base) -1.0)
		      (eqv? (quote/text base) -1))
		  (earlyrew/subtype? t-e earlyrew/type/*fixnum))
	     (let ((exponent-name (earlyrew/new-name 'EXPONENT))
		   (negative-one  (quote/text base)))
	       (form/rewrite! form
		 (bind exponent-name exponent
		       `(IF (CALL ',eq? '#F
				  (CALL ',fix:and '#F
					(LOOKUP ,exponent-name) '1)
				  '0)
			    ',(- negative-one)
			    ',negative-one)))))
	    ((and (eqv? (quote/text base) 2)
		  (earlyrew/subtype? t-e earlyrew/type/*fixnum))
	     (let ((exponent-name (earlyrew/new-name 'EXPONENT)))
	       (form/rewrite! form
		 (bind exponent-name exponent
		       `(IF (IF (CALL ',fix:< '#F
				      (QUOTE 0)
				      (LOOKUP ,exponent-name))
				(CALL ',fix:< '#F
				      (LOOKUP ,exponent-name)
				      '24)
				'#F)
			    (CALL ',fix:lsh '#F '1 (LOOKUP ,exponent-name))
			    (CALL ,(second form) ; invoke-remote-operator-c
				  ,(third form)	; '#F
				  ,(fourth form) ; '(expt 2)
				  ,(fifth form)	; (lookup cacahe-variable)
				  ,(sixth form)	; '2
				  (LOOKUP ,exponent-name)))))))
	    (else unspecific)))))