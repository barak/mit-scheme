#| -*-Scheme-*-

$Id: earlyrew.scm,v 1.14 1995/08/31 15:23:51 adams Exp $

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

;;;; Early generic arithmetic rewrite
;;; package: (compiler midend)

(declare (usual-integrations))


;; Affects how careful we are to maintain exactness:
(define *earlyrew/maximize-exactness?* #T)


(define (earlyrew/top-level program)
  (earlyrew/expr program))

(define-macro (define-early-rewriter keyword bindings . body)
  (let ((proc-name (symbol-append 'EARLYREW/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name FORM)
	     ;; FORM is in scope in handler
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
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
	      (apply handler form (earlyrew/expr* rands))))
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
    (else
     (illegal expr))))

(define (earlyrew/expr* exprs)
  (map (lambda (expr)
	 (earlyrew/expr expr))
       exprs))

(define (earlyrew/remember new old)
  (code-rewrite/remember new old))

(define (earlyrew/remember* new old)
  (code-rewrite/remember new old))

(define (earlyrew/new-name prefix)
  (new-variable prefix))

(define *early-rewritten-operators*
  (make-eq-hash-table))

(define-integrable (rewrite-operator/early? rator)
  (hash-table/get *early-rewritten-operators* rator false))

(define (define-rewrite/early operator-name-or-object handler)
  (hash-table/put! *early-rewritten-operators*
		   (if (known-operator? operator-name-or-object)
		       operator-name-or-object
		       (make-primitive-procedure operator-name-or-object))
		   handler))

(define (earlyrew/nothing-special form x y)
  form x y				; ignored
  false)

(define (earlyrew/binaryop op &op-name %fixop %genop n-bits
			   #!optional opt-x opt-y right-sided?)
  (let ((&op (make-primitive-procedure &op-name))
	(optimize-x (if (default-object? opt-x)
			earlyrew/nothing-special
			opt-x))
	(optimize-y (if (default-object? opt-y)
			earlyrew/nothing-special
			opt-y))
	(right-sided? (if (default-object? right-sided?)
			  false
			  right-sided?))
	(%test (lambda (name)
		 `(CALL (QUOTE ,%small-fixnum?)
			(QUOTE #F)
			(LOOKUP ,name)
			(QUOTE ,n-bits))))
	(test (lambda (value)
		(small-fixnum? value n-bits))))
    (lambda (form x y)
      (cond ((form/number? x)
	     => (lambda (x-value)
		  (cond ((form/number? y)
			 => (lambda (y-value)
			      `(CALL (QUOTE ,%genop)
				     (QUOTE #F)
				     (QUOTE ,x-value)
				     (QUOTE ,y-value))))
			((optimize-x form x-value y))
			((not (test x-value))
			 `(CALL (QUOTE ,%genop)
				(QUOTE #F)
				(QUOTE ,x-value)
				,y))
			((not *earlyrew-expand-genarith?*)
			 `(CALL (QUOTE ,&op)
				(QUOTE #F)
				(QUOTE ,x-value)
				,y))
			(right-sided?
			 `(CALL (QUOTE ,%genop)
				(QUOTE #F)
				(QUOTE ,x-value)
				,y))
			(else
			 (let ((y-name (earlyrew/new-name 'Y)))
			   `(CALL (LAMBDA (,y-name)
				    (IF ,(%test y-name)
					(CALL (QUOTE ,%fixop)
					      (QUOTE #F)
					      (QUOTE ,x-value)
					      (LOOKUP ,y-name))
					(CALL (QUOTE ,%genop)
					      (QUOTE #F)
					      (QUOTE ,x-value)
					      (LOOKUP ,y-name))))
				  ,y))))))

	    ((form/number? y)
	     => (lambda (y-value)
		  (cond ((optimize-y form x y-value))
			((not (test y-value))
			 `(CALL (QUOTE ,%genop)
				(QUOTE #F)
				,x
				(QUOTE ,y-value)))
			((not *earlyrew-expand-genarith?*)
			 `(CALL (QUOTE ,&op)
				(QUOTE #F)
				,x
				(QUOTE ,y-value)))			 
			(else
			 (let ((x-name (earlyrew/new-name 'X)))
			   `(CALL (LAMBDA (,x-name)
				    (IF ,(%test x-name)
					(CALL (QUOTE ,%fixop)
					      (QUOTE #F)
					      (LOOKUP ,x-name)
					      (QUOTE ,y-value))
					(CALL (QUOTE ,%genop)
					      (QUOTE #F)
					      (LOOKUP ,x-name)
					      (QUOTE ,y-value))))
				  ,x))))))
	    ((not *earlyrew-expand-genarith?*)
	     `(CALL (QUOTE ,&op) (QUOTE #F) ,x ,y))
	    (right-sided?
	     `(CALL (QUOTE ,%genop) (QUOTE #F) ,x ,y))
	    (else
	     (let ((x-name (earlyrew/new-name 'X))
		   (y-name (earlyrew/new-name 'Y)))
	       (bind* (list x-name y-name)
		      (list x y)
		      `(IF ,(andify (%test x-name) (%test y-name))
			   (CALL (QUOTE ,%fixop)
				 (QUOTE #F)
				 (LOOKUP ,x-name)
				 (LOOKUP ,y-name))
			   (CALL (QUOTE ,%genop)
				 (QUOTE #F)
				 (LOOKUP ,x-name)
				 (LOOKUP ,y-name))))))))))

(define-rewrite/early '&+
  (earlyrew/binaryop + '&+ fix:+ %+ 1
		     (lambda (form x-value y)
		       form		; ignored
		       (and (zero? x-value)
			    (exact? x-value)
			    y))
		     (lambda (form x y-value)
		       form		; ignored
		       (and (zero? y-value)
			    (exact? y-value)
			    x))))

(define-rewrite/early '&-
  (earlyrew/binaryop - '&- fix:- %- 1
		     earlyrew/nothing-special
		     (lambda (form x y-value)
		       form		;ignored
		       (and (zero? y-value)
			    (exact? y-value)
			    x))))

(define-rewrite/early 'QUOTIENT
  ;; quotient can overflow only when dividing by 0 or -1.
  ;; When dividing by -1 it can only overflow when the value is the
  ;; most negative fixnum (-2^(word-size-1))
  (earlyrew/binaryop careful/quotient 'QUOTIENT fix:quotient %quotient 1
		     (lambda (form x-value y)
		       form y		; ignored
		       (and (zero? x-value) `(QUOTE 0)))
		     (lambda (form x y-value)
		       form		; ignored
		       (cond ((zero? y-value)
			      (user-error "quotient: Division by zero"
					  x y-value))
			     ((= y-value 1)
			      x)
			     ((= y-value -1)
			      (earlyrew/negate form x))
			     (else
			      false)))
		     true))
		     
(define-rewrite/early 'REMAINDER
  (earlyrew/binaryop careful/remainder 'REMAINDER fix:remainder %remainder 0
		     (lambda (form x-value y)
		       form y		; ignored
		       (and (zero? x-value) `(QUOTE 0)))
		     (lambda (form x y-value)
		       form		; ignored
		       (cond ((zero? y-value)
			      (user-error "remainder: Division by zero"
					  x y-value))
			     ((or (= y-value 1) (= y-value -1))
			      `(QUOTE 0))
			     (else
			      false)))
		     true))

(define earlyrew/negate
  (let ((&- (make-primitive-procedure '&-)))
    (lambda (form z)
      ;; z is assumed to be non-constant
      (if *earlyrew-expand-genarith?*
	  (let ((z-name (earlyrew/new-name 'Z)))
	    `(CALL (LAMBDA (,z-name)
		     (IF (CALL (QUOTE ,%small-fixnum?)
			       (QUOTE #F)
			       (LOOKUP ,z-name)
			       (QUOTE 1))
			 (CALL (QUOTE ,fix:-)
			       (QUOTE #F)
			       (QUOTE 0)
			       (LOOKUP ,z-name))
			 (CALL (QUOTE ,%-)
			       (QUOTE #F)
			       (QUOTE 0)
			       (LOOKUP ,z-name))))
		   ,z))
	  `(CALL (QUOTE ,&-) (QUOTE #F) (QUOTE 0) ,z)))))

(define-rewrite/early '&*
  (let ((&* (make-primitive-procedure '&*)))

    (lambda (form x y)
      (define (equivalent form*)
	(earlyrew/remember* form* form))

      (define (by-zero expression zero-value)
	(if *earlyrew/maximize-exactness?*
	    `(IF (CALL (QUOTE ,eq?) (QUOTE #F) ,expression (QUOTE 0))
		 ,(equivalent `(QUOTE 0))
		 ,(equivalent `(QUOTE 0.0)))
	    `(BEGIN ,expression ,(equivalent `(QUOTE ,zero-value)))))

      (define (unexpanded)
	`(CALL (QUOTE ,&*) (QUOTE #F) ,x ,y))
      (define (out-of-line)
	`(CALL (QUOTE ,%*) (QUOTE #F) ,x ,y))
      (cond ((form/number? x)
	     => (lambda (x-value)
		  (cond ((form/number? y)
			 => (lambda (y-value)
			      `(QUOTE ,(* x-value y-value))))
			((zero? x-value)
			 (by-zero y x-value))
			((eqv? x-value 1) 
			 y)
			((eqv? x-value -1)
			 (earlyrew/negate form y))
			((good-factor? x-value)
			 (if (not *earlyrew-expand-genarith?*)
			     (unexpanded)
			     (let ((y-name (earlyrew/new-name 'Y))
				   (n-bits (good-factor->nbits x-value)))
			       `(CALL
				 (LAMBDA (,y-name)
				   (IF (CALL (QUOTE ,%small-fixnum?)
					     (QUOTE #F)
					     (LOOKUP ,y-name)
					     (QUOTE ,n-bits))
				       (CALL (QUOTE ,fix:*)
					     (QUOTE #F)
					     (QUOTE ,x-value)
					     (LOOKUP ,y-name))
				       (CALL (QUOTE ,%*)
					     (QUOTE #F)
					     (QUOTE ,x-value)
					     (LOOKUP ,y-name))))
				 ,y))))
			(else
			 (out-of-line)))))
	    ((form/number? y)
	     => (lambda (y-value)
		  (cond ((zero? y-value)
			 (by-zero x y-value))
			((eqv? y-value 1)
			 x)
			((eqv? y-value -1)
			 (earlyrew/negate form x))
			((good-factor? y-value)
			 (if (not *earlyrew-expand-genarith?*)
			     (unexpanded)
			     (let ((x-name (earlyrew/new-name 'X))
				   (n-bits (good-factor->nbits y-value)))
			       (bind x-name x
				     `(IF (CALL (QUOTE ,%small-fixnum?)
						(QUOTE #F)
						(LOOKUP ,x-name)
						(QUOTE ,n-bits))
					  (CALL (QUOTE ,fix:*)
						(QUOTE #F)
						(LOOKUP ,x-name)
						(QUOTE ,y-value))
					  (CALL (QUOTE ,%*)
						(QUOTE #F)
						(LOOKUP ,x-name)
						(QUOTE ,y-value)))))))
			(else
			 (out-of-line)))))
	    (else
	     (out-of-line))))))

;; NOTE: these could use 0 as the number of bits, but this would prevent
;; a common RTL-level optimization triggered by CSE.

(define-rewrite/early '&= (earlyrew/binaryop = '&= fix:= %= 1))
(define-rewrite/early '&< (earlyrew/binaryop < '&< fix:< %< 1))
(define-rewrite/early '&> (earlyrew/binaryop > '&> fix:> %> 1))

(define-rewrite/early '&/
  (lambda (form x y)
    (define (out-of-line x y)
      `(CALL (QUOTE ,%/) (QUOTE #F) ,x ,y))
    (cond ((form/number? x)
	   => (lambda (x-value)
		(cond ((form/number? y)
		       => (lambda (y-value)
			    (cond ((careful// x-value y-value)
				   => (lambda (result)
					`(QUOTE ,result)))
				  (else (out-of-line x y)))))
		      ((zero? x-value)
		       `(QUOTE 0))
		      (else
		       (out-of-line `(QUOTE ,x-value) y)))))
	  ((form/number? y)
	   => (lambda (y-value)
		(cond ((zero? y-value)
		       (out-of-line x y))
		      ((= y-value 1)
		       x)
		      ((= y-value -1)
		       (earlyrew/negate form x))
		      (else
		       (out-of-line x y)))))
	  (else
	   (out-of-line x y)))))

;;;; Rewrites of unary operations in terms of binary operations

(let ((unary-rewrite
       (lambda (binary-name rand2)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (form rand1)
	     ((rewrite-operator/early? binary-operation)
	      form
	      rand1
	      `(QUOTE ,rand2))))))
      (special-rewrite
       (lambda (binary-name rand2)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (form rand1)
	     `(CALL (QUOTE ,binary-operation)
		    (QUOTE #F)
		    ,rand1
		    (QUOTE ,rand2))))))
      (special-rewrite/left
       (lambda (binary-name rand1)
	 (let ((binary-operation (make-primitive-procedure binary-name)))
	   (lambda (form rand2)
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
    (lambda (form x)
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
	   (lambda (form size)
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
    (lambda (form size fill)
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

(define (early/indexed-reference primitive object-tag-name
				 %check/full %check/index
				 %unchecked)
  (let ((object-tag (machine-tag object-tag-name)))
    (lambda (form vec index #!optional value)

      (define (equivalent form*)
	(earlyrew/remember* form* form))

      (define (bind+ name value body)
	(if name (bind name value body) body))

      (let ((vec-name  (earlyrew/new-name object-tag-name))
	    (idx-name  (earlyrew/new-name 'INDEX))
	    (val-name  (and (not (default-object? value))
			    (earlyrew/new-name 'VALUE))))
	(let ((extra
	       (if (default-object? value) '() (list `(LOOKUP ,val-name)))))
	  (let ((test
		 (cond ((and compiler:generate-range-checks?
			     compiler:generate-type-checks?)
			`(CALL (QUOTE ,%check/full) '#F
			       (LOOKUP ,vec-name) (LOOKUP ,idx-name)))
		       (compiler:generate-range-checks?
			`(CALL (QUOTE ,%check/index) '#F
			       (LOOKUP ,vec-name) (LOOKUP ,idx-name)))
		       (compiler:generate-type-checks?
			`(CALL (QUOTE ,object-type?) '#F
			       (QUOTE ,object-tag) (LOOKUP ,vec-name)))
		       (else #F)))
		(unchecked
		 (lambda ()
		   (equivalent `(CALL (QUOTE ,%unchecked) (QUOTE #F)
				      (LOOKUP ,vec-name)
				      (LOOKUP ,idx-name)
				      ,@extra))))
		(primitive-call
		 (lambda ()
		   (equivalent `(CALL (QUOTE ,primitive) (QUOTE #F)
				      (LOOKUP ,vec-name)
				      (LOOKUP ,idx-name)
				      ,@extra)))))
	    (bind vec-name vec
		  (bind idx-name index
			(bind+ val-name (or (default-object? value) value)
			       (if test
				   (equivalent
				    `(IF ,test
					 ,(unchecked)
					 ,(primitive-call)))
				   (unchecked)))))))))))

(define-rewrite/early 'VECTOR-REF
  (early/indexed-reference (make-primitive-procedure 'VECTOR-REF) 'VECTOR
			   %vector-check %vector-check/index
			   %vector-ref))

(define-rewrite/early 'VECTOR-SET!
  (early/indexed-reference (make-primitive-procedure 'VECTOR-SET!) 'VECTOR
			   %vector-check %vector-check/index
			   %vector-set!))

(define (early/make-cxr primitive %unchecked)
  (let ((prim-pair? (make-primitive-procedure 'PAIR?)))
    (lambda (form arg-text)
      (define (equivalent form*) (earlyrew/remember* form* form))
      (if compiler:generate-type-checks?
	  (let ((text-name  (earlyrew/new-name 'OBJECT)))
	    (bind text-name arg-text
		  (equivalent
		   `(IF (CALL ',prim-pair? '#F (LOOKUP ,text-name))
			,(equivalent
			  `(CALL ',%unchecked '#F (LOOKUP ,text-name)))
			,(equivalent
			  `(CALL ',primitive  '#F (LOOKUP ,text-name)))))))
	  `(CALL ',%unchecked '#F ,arg-text)))))

(define early/car (early/make-cxr (make-primitive-procedure 'CAR) %car))
(define early/cdr (early/make-cxr (make-primitive-procedure 'CDR) %cdr))

(define-rewrite/early 'CAR early/car)
(define-rewrite/early 'CDR early/cdr)

(define-rewrite/early 'GENERAL-CAR-CDR
  (let ((prim-general-car-cdr (make-primitive-procedure 'GENERAL-CAR-CDR))
        (prim-car             (make-primitive-procedure 'CAR))
        (prim-cdr             (make-primitive-procedure 'CDR)))
    (lambda (form term pattern)
      (define (equivalent form*) (earlyrew/remember* form* form))
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
				       (equivalent
					((if (odd? num) early/car early/cdr)
					 form
					 text)))))
		      (default))))
	    (else (default))))))


(define (define-rewrite/early/global name arity handler)
  (let ((slot (hash-table/get *early-rewritten-operators* name '())))
    (hash-table/put! *early-rewritten-operators*
		     name
		     (cons (cons arity handler) slot))))

(define-rewrite/early %invoke-remote-cache 
  (lambda (form descriptor operator-cache . values)
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
			      (apply (cdr arity.handler) form default values)))
			(else (default values)))))
	    (else
	     (default values))))))


(define-rewrite/early/global 'SQRT 1
  (lambda (form default arg)
    (cond ((form/number? arg)
	   => (lambda (number)
		`(QUOTE ,(sqrt number))))
	  (else
	   (default (list arg))))))


(define-rewrite/early/global 'EXPT 2
  (let ((&* (make-primitive-procedure '&*))
	(max-multiplies 3))
    (lambda (form default* base exponent)
      (define (default)
	(default* (list base exponent)))
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
				 (earlyrew/expr expression))))
			(else (default)))))
	    (else
	     (default))))))
