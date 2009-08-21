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

;;;; LATEREW
;;; package: (compiler midend)
;;
;; Late (post CPS and closure conversion) rewrites, including some
;; generic arithmetic.
;;

(declare (usual-integrations))

(define (laterew/top-level program)
  (laterew/expr program))

(define-macro (define-late-rewriter keyword bindings . body)
  (let ((proc-name (symbol-append 'LATEREW/ keyword)))
    (call-with-values
	(lambda () (%matchup bindings '(handler) '(cdr form)))
      (lambda (names code)
	`(DEFINE ,proc-name
	   (NAMED-LAMBDA (,proc-name FORM)
	     (LET ((HANDLER (LAMBDA ,names ,@body)))
	       (LATEREW/REMEMBER ,code FORM))))))))

(define-late-rewriter LOOKUP (name)
  `(LOOKUP ,name))

(define-late-rewriter LAMBDA (lambda-list body)
  `(LAMBDA ,lambda-list
     ,(laterew/expr body)))

(define-late-rewriter LET (bindings body)
  `(LET ,(map (lambda (binding)
		(list (car binding)
		      (laterew/expr (cadr binding))))
	      bindings)
     ,(laterew/expr body)))

(define-late-rewriter LETREC (bindings body)
  `(LETREC ,(map (lambda (binding)
		   (list (car binding)
			 (laterew/expr (cadr binding))))
		 bindings)
     ,(laterew/expr body)))

(define-late-rewriter QUOTE (object)
  `(QUOTE ,object))

(define-late-rewriter DECLARE (#!rest anything)
  `(DECLARE ,@anything))

(define-late-rewriter BEGIN (#!rest actions)
  `(BEGIN ,@(laterew/expr* actions)))

(define-late-rewriter IF (pred conseq alt)
  `(IF ,(laterew/expr pred)
       ,(laterew/expr conseq)
       ,(laterew/expr alt)))

(define-late-rewriter CALL (rator #!rest rands)
  (cond ((and (QUOTE/? rator)
	      (rewrite-operator/late? (quote/text rator)))
	 => (lambda (handler)
	      (handler form (laterew/expr* rands))))
	(else
	 (let ((rands* (laterew/expr* rands)))
	   (laterew/jump (laterew/expr rator) (car rands*) (cdr rands*) 0)))))

(define (laterew/expr expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)    (laterew/quote expr))
    ((LOOKUP)   (laterew/lookup expr))
    ((LAMBDA)   (laterew/lambda expr))
    ((LET)      (laterew/let expr))
    ((DECLARE)  (laterew/declare expr))
    ((CALL)     (laterew/call expr))
    ((BEGIN)    (laterew/begin expr))
    ((IF)       (laterew/if expr))
    ((LETREC)   (laterew/letrec expr))
    (else
     (illegal expr))))

(define (laterew/expr* exprs)
  (map (lambda (expr)
	 (laterew/expr expr))
       exprs))

(define (laterew/remember new old)
  (code-rewrite/remember new old))

(define (laterew/new-name prefix)
  (new-variable prefix))

;;;; Late open-coding of generic arithmetic

(define (laterew/binaryop op %fixop %genop n-bits #!optional right-sided?)
  (let ((right-sided?
	 (if (default-object? right-sided?)
	     false
	     right-sided?))
	(%test
	 (cond ((not (number? n-bits))
		(lambda (name constant-rand)
		  (if constant-rand
		      `(CALL (QUOTE ,%small-fixnum?)
			     (QUOTE #F)
			     (LOOKUP ,name)
			     (QUOTE ,(n-bits constant-rand)))
		      `(QUOTE #F))))
	       (else
		(lambda (name constant-rand)
		  constant-rand		; ignored		  
		  `(CALL (QUOTE ,%small-fixnum?)
			 (QUOTE #F)
			 (LOOKUP ,name)
			 (QUOTE ,n-bits)))))))
    (lambda (form rands)
      (define (equivalent form*) (laterew/remember form* form))
      (let ((cont (first rands))
	    (x    (second rands))
	    (y    (third rands)))
	(let ((%continue
	       (cond ((QUOTE/? cont)
		      (lambda (expr)
			expr))
		     ((or (LOOKUP/? cont)
			  (CALL/%stack-closure-ref? cont))
		      (lambda (expr)
			(laterew/invoke-continuation cont (list expr))))
		     (else
		      (if compiler:guru?
			  (internal-warning
			   "Unexpected continuation to out-of-line hook" cont))
		      (lambda (expr)
			(let ((cont-var (new-continuation-variable)))
			  `(CALL (LAMBDA (,cont-var)
				   ,(laterew/invoke-continuation
				     `(LOOKUP ,cont-var)
				     (list expr)))
				 ,cont)))))))
	  (cond ((form/number? x)
		 => (lambda (x-value)
		      (cond ((form/number? y)
			     => (lambda (y-value)
				  (%continue `(QUOTE ,(op x-value y-value)))))
			    (right-sided?
			     `(CALL (QUOTE ,%genop) ,cont ,x ,y))
			    (else
			     (let ((y-name (laterew/new-name 'Y)))
			       `(LET ((,y-name ,y))
				  (IF ,(%test y-name x-value)
				      ,(%continue
					`(CALL (QUOTE ,%fixop)
					       (QUOTE #f)
					       (QUOTE ,x-value)
					       (LOOKUP ,y-name)))
				      ,(equivalent
					`(CALL (QUOTE ,%genop)
					       ,cont
					       (QUOTE ,x-value)
					       (LOOKUP ,y-name))))))))))
		
		((form/number? y)
		 => (lambda (y-value)
		      (let ((x-name (laterew/new-name 'X)))
			`(LET ((,x-name ,x))
			   (IF ,(%test x-name y-value)
			       ,(%continue
				 `(CALL (QUOTE ,%fixop)
					(QUOTE #f)
					(LOOKUP ,x-name)
					(QUOTE ,y-value)))
			       ,(equivalent
				 `(CALL (QUOTE ,%genop)
					,cont
					(LOOKUP ,x-name)
					(QUOTE ,y-value))))))))
		(right-sided?
		 `(CALL (QUOTE ,%genop) ,cont ,x ,y))
                (else
                 (let ((x-name (laterew/new-name 'X))
                       (y-name (laterew/new-name 'Y)))
		   `(LET ((,x-name ,x)
			  (,y-name ,y))
		      (IF ,(andify (%test x-name false)
				   (%test y-name false))
			  ,(%continue
			    `(CALL (QUOTE ,%fixop)
				   (QUOTE #F)
				   (LOOKUP ,x-name)
				   (LOOKUP ,y-name)))
			  ,(equivalent
			    `(CALL (QUOTE ,%genop)
				   ,cont
				   (LOOKUP ,x-name)
				   (LOOKUP ,y-name)))))))))))))

(define *late-rewritten-operators* (make-eq-hash-table))

(define-integrable (rewrite-operator/late? rator)
  (hash-table/get *late-rewritten-operators* rator false))

(define (define-rewrite/late operator-name-or-object handler)
  (hash-table/put! *late-rewritten-operators*
		   (if (known-operator? operator-name-or-object)
		       operator-name-or-object
		       (make-primitive-procedure operator-name-or-object))
		   handler))

(define-rewrite/late '&+
  (laterew/binaryop + fix:+ %+ 1))

(define-rewrite/late '&-
  (laterew/binaryop - fix:- %- 1))

(define-rewrite/late '&*
  (laterew/binaryop * fix:* %* good-factor->nbits))

;; NOTE: these could use 0 as the number of bits, but this would prevent
;; a common RTL-level optimization triggered by CSE.

(define-rewrite/late '&=
  (laterew/binaryop = fix:= %= 1))

(define-rewrite/late '&<
  (laterew/binaryop < fix:< %< 1))

(define-rewrite/late '&>
  (laterew/binaryop > fix:> %> 1))

(define-rewrite/late 'QUOTIENT
  (laterew/binaryop careful/quotient fix:quotient %quotient
		    (lambda (value)
		      (cond ((zero? value)
			     (user-error "QUOTIENT by 0"))
			    ((= value -1)
			     ;; Most negative fixnum overflows!
			     1)
			    (else
			     0)))
		    true))

(define-rewrite/late 'REMAINDER
  (laterew/binaryop careful/remainder fix:remainder %remainder
		    (lambda (value)
		      (if (zero? value)
			  (user-error "REMAINDER by 0")
			  0))
		    true))

(let ((not-primitive  (make-primitive-procedure 'NOT)))
  (define-rewrite/late not-primitive
    (lambda (form rands)
      form				; ignored
      (let ((cont   (first rands))
	    (x      (second rands))
	    (more?  (not (null? (cddr rands)))))
	(if (and (equal? cont '(QUOTE #F))
		 (not more?))
	    `(IF ,x (QUOTE #F) (QUOTE #T))
	    `(CALL (QUOTE ,not-primitive) ,cont ,@rands))))))

;; We transform calls and returns of the form
;;    (call ... ... predicate ...)
;;  to
;;    (if predicate
;;        (call ... #T ...)
;;        (call ... #F ...))
;;
;; where the calls have a small number of arguments*.
;;
;; What this transformation achieves is the removal of the merge point
;; for the predicate.  There is a chance that we might generate
;; something with duplicated code, so we avoid conplex continuations
;; and let-bind non-trivial expressions.  If the RTL has several
;; instructions, for example, to pop a stack frame, then RTLCSM will
;; re-merge the code.  Note that at the laterew stage, if we have a
;; predicate or conditional expression as an argument to a call, then
;; it must be simple and side effect free.
;;
;; Really, this kind of thing should be handled by RTLGEN (by targetting
;; multiple calls) or by rtl optimization (intra-block instruction
;; scheduling).  Another possibility is to undo the call-to-call
;; nature of the output in lapopt, where we have a much better idea of
;; the benefit.
;;
;; * Since we get bad code if we duplicate calls/returns with many
;; arguments, we restrict this transformation to 2 expressions.
;;
;; The main benefit of this transformation is for code that returns an
;; in-lined predicate.

(define (laterew/invoke-continuation cont rands)
  (laterew/jump `(QUOTE ,%invoke-continuation) cont rands 0))

(let ()
  (define (invocation-operator operator n-extra)
    (define-rewrite/late operator
      (lambda (form rands)
	(laterew/jump (call/operator form) (car rands) (cdr rands) n-extra))))

  ;; %internal-apply is omitted because it tends to be a sequence of
  ;; instructions and we don't really want to duplicate the sequence.
  ;; This is another reason why RTLGEN/RTLOPT/LAPOPT is a better place
  ;; for this code.

  (invocation-operator %invoke-continuation 0)
  (invocation-operator %invoke-operator-cache 2)
  (invocation-operator %invoke-remote-cache 2)
  (invocation-operator %internal-apply-unchecked 2))


(define (laterew/jump rator cont all-rands n-extra)

  (define (default)
    `(CALL ,rator ,cont ,@all-rands))

  (define (split expression test true-value false-value)
    (let loop ((rands all-rands)
	       (pos   0)
	       (rands-t '())
	       (rands-f '()))
      (define (next t f)
	(loop (cdr rands) (+ pos 1) (cons t rands-t) (cons f rands-f)))
      (cond ((null? rands)
	     `(IF ,test
		  (CALL ,rator ,cont ,@(reverse rands-t))
		  (CALL ,rator ,cont ,@(reverse rands-f))))
	    ((eq? (car rands) expression)
	     (next true-value false-value))
	    ((or (LOOKUP/? (car rands))
		 (QUOTE/? (car rands)))
	     (next (car rands) (car rands)))
	    (else
	     (let ((name (compat/new-name 'ARG)))
	       `(LET ((,name ,(car rands)))
		  ,(next `(LOOKUP ,name) `(LOOKUP ,name))))))))

  (define (predicate-call? expr)
    (and (CALL/? expr)
	 (let ((rator (call/operator expr)))
	   (and
	    (QUOTE/? rator)
	    (operator/satisfies? (quote/text rator) '(PROPER-PREDICATE))))))

  (if (and (or (LOOKUP/? cont)
	       (call/%stack-closure-ref? cont))
	   (<= (length all-rands) (+ n-extra 2)))
      (let search ((rands  (reverse all-rands)))
	(cond ((null? rands)
	       (default))
	      ((IF/? (car rands))
	       (split (car rands)
		      (if/predicate (car rands))
		      (if/consequent (car rands))
		      (if/alternative (car rands))))
	      ((predicate-call? (car rands))
	       (split (car rands)
		      (car rands)
		      `(QUOTE ,#T)
		      `(QUOTE ,#F)))
	      (else (search (cdr rands)))))
      (default)))

(define-rewrite/late %reference
  (lambda (form rands)
    rands				; ignored
    `(QUOTE ,form)))

(define-rewrite/late %make-multicell
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (layout  (second rands))
	  (values  (cddr rands)))
      (let ((name (and (QUOTE/? layout)
		       (= (vector-length (quote/text layout)) 1)
		       `(QUOTE ,(vector-ref (quote/text layout) 0)))))
	(laterew/multicell-operation cont layout name 'MAKE #F values)))))

(define-rewrite/late %multicell-ref
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (cell    (second rands))
	  (layout  (third rands))
	  (name    (fourth rands)))
      (laterew/multicell-operation cont layout name 'READ cell #F))))

(define-rewrite/late %multicell-set!
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (cell    (second rands))
	  (value   (third rands))
	  (layout  (fourth rands))
	  (name    (fifth rands)))
      (laterew/multicell-operation cont layout name 'WRITE cell value))))

(define (laterew/multicell-operation cont layout name operation cell value/s)
  (if (not (equal? cont '(QUOTE #F)))
      (internal-error "Bad continuation for Multicell operation" cont))
  (let ((layout
	 (if (QUOTE/? layout)
	     (quote/text layout)
	     (internal-error "Multicell operation needs constant LAYOUT"
			     layout)))
	(name
	 (cond ((eq? name #F)  #F)
	       ((QUOTE/? name) (quote/text name))
	       (else (internal-error "Multicell operation needs constant NAME"
				     name)))))
    (define (index)
      (let ((value  (vector-find-next-element layout name)))
	(if value
	    `(QUOTE ,value)
	    (internal-error "Multicell operation: name not found"
			    name layout))))
    (case (vector-length layout)
      ((1)
       (case operation
	 ((READ)	`(CALL ',%cell-ref '#F ,cell ',name))
	 ((WRITE)	`(CALL ',%cell-set! '#F ,cell ,value/s ',name))
	 ((MAKE)	`(CALL ',%make-cell '#F ,@value/s ',name))))
      ;;((2) (case operation
      ;;   ((READ))
      ;;   ((WRITE))
      ;;   ((MAKE))))
      (else
       (case operation
	 ((READ)	`(CALL ',%vector-ref '#F ,cell ,(index)))
	 ((WRITE)	`(CALL ',%vector-set! '#F ,cell ,(index) ,value/s))
	 ((MAKE)	`(CALL ',%vector '#F ,@value/s)))))))

(define-rewrite/late %flo:make-multicell
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (layout  (second rands))
	  (values  (cddr rands)))
      (let ((name (and (QUOTE/? layout)
		       (= (vector-length (quote/text layout)) 1)
		       `(QUOTE ,(vector-ref (quote/text layout) 0)))))
	(laterew/flo:multicell-operation cont layout name 'MAKE #F values)))))

(define-rewrite/late %flo:multicell-ref
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (cell    (second rands))
	  (layout  (third rands))
	  (name    (fourth rands)))
      (laterew/flo:multicell-operation cont layout name 'READ cell #F))))

(define-rewrite/late %flo:multicell-set!
  (lambda (form rands)
    form				; ignored
    (let ((cont    (first rands))
	  (cell    (second rands))
	  (value   (third rands))
	  (layout  (fourth rands))
	  (name    (fifth rands)))
      (laterew/flo:multicell-operation cont layout name 'WRITE cell value))))

(define (laterew/flo:multicell-operation cont layout name operation cell value/s)
  (if (not (equal? cont '(QUOTE #F)))
      (internal-error "Bad continuation for Multicell operation" cont))
  (let ((layout
	 (if (QUOTE/? layout)
	     (quote/text layout)
	     (internal-error "Multicell operation needs constant LAYOUT"
			     layout)))
	(name
	 (cond ((eq? name #F)  #F)
	       ((QUOTE/? name) (quote/text name))
	       (else (internal-error "Multicell operation needs constant NAME"
				     name)))))
    (define (index)
      (let ((value  (vector-find-next-element layout name)))
	(if value
	    `(QUOTE ,value)
	    (internal-error "Multicell operation: name not found"
			    name layout))))
    (case operation
      ((READ)	`(CALL ',flo:vector-ref '#F ,cell ,(index)))
      ((WRITE)	`(CALL ',flo:vector-set! '#F ,cell ,(index) ,value/s))
      ((MAKE)
       (let ((cell (laterew/new-name 'FLONUM-VECTOR)))
	 `(LET ((,cell (CALL ',flo:vector-cons '#F ',(vector-length layout))))
	    (BEGIN
	      ,@(map (lambda (index value)
		       `(CALL ',flo:vector-set! '#F
			      (LOOKUP ,cell)
			      (QUOTE ,index)
			      ,value))
		     (iota (length values))
		     values)
	      (LOOKUP ,cell))))))))

(define-rewrite/late %generic-index-check/ref
  ;; (CALL '%generic-index-check/ref '#F <collection> <index> '#(checks))
  (lambda (form rands)
    form				; ignored
    (let  ((collection (second rands))
	   (index      (third rands))
	   (checks     (quote/text (fourth rands))))
      (let ((collection*     (laterew/new-name 'COLLECTION))
	    (collection-tag  (vector-ref checks 0))
	    (length-ref      (vector-ref checks 1)))
	(let ((test1
	       (if collection-tag
		   `(CALL ',object-type? '#F
			  ',collection-tag
			  (LOOKUP ,collection*))
		   `(QUOTE #T)))
	      (test2
	       (if length-ref
		   `(CALL ',%word-less-than-unsigned? '#F
			  ,index
			  (CALL ',length-ref '#F (LOOKUP ,collection*)))
		   `(QUOTE #T))))
	  `(LET ((,collection* ,collection))
	     ,(andify test1 test2)))))))


(define-rewrite/late %generic-index-check/set!
  ;; (CALL '%generic-index-check/set! '#F <collection> <index> <elt> '#(checks))
  (lambda (form rands)
    form ; ignored
    (let  ((collection (second rands))
	   (index      (third rands))
	   (element    (fourth rands))
	   (checks     (quote/text (fifth rands))))
      (let ((collection* (laterew/new-name 'COLLECTION))
	    (collection-tag  (vector-ref checks 0))
	    (length-ref      (vector-ref checks 1))
	    (element-tag     (vector-ref checks 2)))
	(let ((test1
	       (if collection-tag
		   `(CALL ',object-type? '#F
			  ',collection-tag
			  (LOOKUP ,collection*))
		   `(QUOTE #T)))
	      (test2
	       (if length-ref
		   `(CALL ',%word-less-than-unsigned? '#F
			  ,index
			  (CALL ',length-ref '#F (LOOKUP ,collection*)))
		   `(QUOTE #T)))
	      (test3
	       (if element-tag
		   `(CALL ',object-type? '#F
			  ',element-tag
			  ,element)
		   `(QUOTE #T))))
	  `(LET ((,collection* ,collection))
	     ,(andify (andify test1 test2) test3)))))))
