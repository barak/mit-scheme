#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; MIT/GNU Scheme macros

(declare (usual-integrations))

;;;; Definitions

(define $define
  (spar-transformer->runtime
   (delay
     (spar-or
       (scons-rule `(id ,(optional-value-pattern))
	 (lambda (name value)
	   (scons-call keyword:define name value)))
       (scons-rule
	   `((spar
	      ,(spar-subform
		 (spar-push-subform-if identifier? spar-arg:form)
		 (spar-push-form-if mit-lambda-list? spar-arg:form)))
	     (+ any))
	 (lambda (name bvl body-forms)
	   (scons-define name
	     (apply scons-named-lambda (cons name bvl) body-forms))))
       (scons-rule
	   `((spar
	      ,(spar-subform
		 (spar-push-subform)
		 (spar-push-form-if mit-lambda-list? spar-arg:form)))
	     (+ any))
	 (lambda (nested bvl body-forms)
	   (scons-define nested
	     (apply scons-lambda bvl body-forms))))))))

(define (optional-value-pattern)
  `(or any (value-of ,unassigned-expression)))

(define (unassigned-expression)
  `(,keyword:unassigned))

(define (unspecific-expression)
  `(,keyword:unspecific))

;;;; Let-like forms

(define $let
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or id (value #f))
	   ,(let-bindings-pattern)
	   (+ any))
       (lambda (name bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (if name
	       (generate-named-let name ids vals body-forms)
	       (apply scons-call
		      (apply scons-named-lambda
			     (cons scode-lambda-name:let ids)
			     body-forms)
		      vals))))))))

(define (let-bindings-pattern)
  `(subform (* (subform (list id ,(optional-value-pattern))))))

(define named-let-strategy 'internal-definition)

(define (generate-named-let name ids vals body-forms)
  (let ((proc (apply scons-named-lambda (cons name ids) body-forms)))
    (case named-let-strategy
      ((internal-definition)
       (apply scons-call
	      (scons-let '() (scons-define name proc) name)
	      vals))
      ((letrec)
       (apply scons-call
	      (scons-letrec (list (list name proc)) name)
	      vals))
      ((letrec*)
       (apply scons-call
	      (scons-letrec* (list (list name proc)) name)
	      vals))
      ((fixed-point)
       (let ((iter (new-identifier 'iter))
	     (kernel (new-identifier 'kernel))
	     (temps (map new-identifier ids)))
	 (scons-call (scons-lambda (list kernel)
		       (apply scons-call kernel kernel vals))
		     (scons-lambda (cons iter ids)
		       (scons-call (apply scons-lambda
					  (list name)
					  (scons-declare
					   (list 'integrate-operator name))
					  body-forms)
				   (scons-lambda temps
				     (scons-declare (cons 'integrate temps))
				     (apply scons-call iter iter temps)))))))
      (else
       (error "Unrecognized strategy:" named-let-strategy)))))

(define $let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let bindings body-forms))))))

(define $let*-syntax
  (spar-transformer->runtime
   (delay
     (scons-rule
	 '((subform (* (subform (list id any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let-syntax bindings body-forms))))))

(define (expand-let* scons-let bindings body-forms)
  (if (pair? bindings)
      (fold-right (lambda (binding expr)
		    (scons-let (list binding) expr))
		  (apply scons-begin body-forms)
		  bindings)
      (apply scons-let '() body-forms)))

(define $letrec
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (let* ((ids (map car bindings))
		(vals (map cadr bindings))
		(temps (map new-identifier ids)))
	   (scons-let (map (lambda (id)
			     (list id (unassigned-expression)))
			   ids)
	     (cond ((not (pair? ids))
		    (default-object))
		   ((not (pair? (cdr ids)))
		    (scons-set! (car ids) (car vals)))
		   (else
		    (apply scons-let
			   (map list temps vals)
			   (map scons-set! ids temps))))
	     (scons-call (apply scons-lambda '() body-forms)))))))))

(define $letrec*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (scons-let (map (lambda (id)
			     (list id (unassigned-expression)))
			   ids)
	     (apply scons-begin (map scons-set! ids vals))
	     (scons-call (apply scons-lambda '() body-forms)))))))))

(define $let-values
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list ,r4rs-lambda-list? any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((body (apply scons-begin body-forms)))
	   (case (length bindings)
	     ((0)
	      (scons-let '() body))
	     ((1)
	      (scons-cwv (car (car bindings))
			 (scons-lambda '() (cadr (car bindings)))
			 body))
	     (else
	      (let-values-multi bindings body)))))))))

(define (let-values-multi bindings body)
  (let ((temps
	 (map (lambda (index)
		(new-identifier (symbol 'temp- index)))
	      (iota (length bindings))))
	(thunks
	 (map (lambda (binding)
		(scons-lambda () (cadr binding)))
	      bindings)))
    (scons-let (map list temps thunks)
      (let loop ((bvls (map car bindings)) (temps temps))
	(if (pair? bvls)
	    (scons-cwv (car bvls)
		       (car temps)
		       (loop (cdr bvls) (cdr temps)))
	    body)))))

(define-syntax $let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...)
     (let () body0 body1 ...))
    ((let*-values (binding0 binding1 ...) body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
	 body0 body1 ...)))))

;;; SRFI 8: receive

(define $receive
  (spar-transformer->runtime
   (delay
     (scons-rule `(,r4rs-lambda-list? any (+ any))
       (lambda (bvl expr body-forms)
	 (scons-cwv bvl
		    (scons-lambda '() expr)
		    (apply scons-begin body-forms)))))))

(define (scons-cwv bvl thunk body)
  (scons-call (scons-close 'call-with-values)
	      thunk
	      (scons-lambda bvl body)))

;;; SRFI 2: and-let*

;;; The SRFI document is a little unclear about the semantics, imposes
;;; the weird restriction that variables may be duplicated (citing
;;; LET*'s similar restriction, which doesn't actually exist), and the
;;; reference implementation is highly non-standard and hard to
;;; follow.  This passes all of the tests except for the one that
;;; detects duplicate bound variables, though.

(define $and-let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (list (or id (subform any) (subform id any)))))
	   (* any))
       (lambda (clauses body-exprs)
	 (let recur1 ((conjunct #t) (clauses clauses))
	   (cond ((pair? clauses)
		  (scons-and conjunct
			     (let ((clause (car clauses)))
			       (let ((rest (recur1 (car clause) (cdr clauses))))
				 (if (pair? (cdr clause))
				     (scons-let (list clause) rest)
				     rest)))))
		 ((pair? body-exprs)
		  (scons-and conjunct (apply scons-begin body-exprs)))
		 (else
		  conjunct))))))))

;;;; Conditionals

(define $cond
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((* ,cond-clause-pattern)
	   ,cond-else-clause-pattern)
       (lambda (clauses else-actions)
	 (fold-right expand-cond-clause
		     (if else-actions
			 (apply scons-begin else-actions)
			 (unspecific-expression))
		     clauses))))))

(define cond-clause-pattern
  '(subform (cons (and (not (ignore-if id=? else))
		       any)
		  (if (ignore-if id=? =>)
		      (list (value =>)
			    any)
		      (cons (value begin)
			    (* any))))))

(define cond-else-clause-pattern
  '(or (subform (ignore-if id=? else)
		(+ any))
       (value #f)))

(define (expand-cond-clause clause rest)
  ((cond-clause-expander scons-if) clause rest))

(define ((cond-clause-expander scons-if) clause rest)
  (let ((predicate (car clause))
	(type (cadr clause))
	(actions (cddr clause)))
    (case type
      ((=>)
       (let ((temp (new-identifier 'temp)))
	 (scons-let (list (list temp predicate))
	   (scons-if temp
		     (scons-call (car actions) temp)
		     rest))))
      ((begin)
       (if (pair? actions)
	   (scons-if predicate
		     (apply scons-begin actions)
		     rest)
	   (let ((temp (new-identifier 'temp)))
	     (scons-let (list (list temp predicate))
	       (scons-if temp temp rest)))))
      (else
       (error "Unknown clause type:" type)))))

(define $do
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list id any (? any)))))
	   ,cond-clause-pattern
	   (* any))
       (lambda (bindings test-clause actions)
	 (let ((loop-name (new-identifier 'do-loop)))
	   (scons-named-let loop-name
	       (map (lambda (binding)
		      (list (car binding)
			    (cadr binding)))
		    bindings)
	     (expand-cond-clause test-clause
				 (scons-begin
				   (apply scons-begin actions)
				   (apply scons-call
					  loop-name
					  (map (lambda (binding)
						 (if (pair? (cddr binding))
						     (caddr binding)
						     (car binding)))
					       bindings)))))))))))

(define $case
  (spar-transformer->runtime
   (delay
     (scons-rule
	 (let ((action-pattern
		'(if (ignore-if id=? =>)
		     (list (value =>)
			   any)
		     (cons (value begin)
			   (+ any)))))
	   `(any
	     (* (subform (cons (subform (* any))
			       ,action-pattern)))
	     (or (subform (ignore-if id=? else)
			  ,action-pattern)
		 (value #f))))
       (lambda (expr clauses else-clause)
	 (let ((temp (new-identifier 'key)))

	   (define (process-clause clause rest)
	     (if (pair? (car clause))
		 (scons-if (process-predicate (car clause))
			   (process-action (cadr clause) (cddr clause))
			   rest)
		 rest))

	   (define (process-predicate items)
	     (apply scons-or
		    (map (lambda (item)
			   (scons-call (scons-close
					(if (or (symbol? item)
						(boolean? item)
						;; implementation dependent:
						(char? item)
						(fix:fixnum? item))
					    'eq?
					    'eqv?))
				       (scons-quote item)
				       temp))
			 items)))

	   (define (process-action type exprs)
	     (cond ((eq? type 'begin) (apply scons-begin exprs))
		   ((eq? type '=>) (scons-call (car exprs) temp))
		   (else (error "Unrecognized action type:" type))))

	   (scons-let (list (list temp expr))
	     (fold-right process-clause
			 (if else-clause
			     (process-action (car else-clause)
					     (cdr else-clause))
			     (unspecific-expression))
			 clauses))))))))

(define-syntax $and
  (syntax-rules ()
    ((and) #t)
    ((and expr0) expr0)
    ((and expr0 expr1+ ...) (if expr0 (and expr1+ ...) #f))))

(define-syntax $when
  (syntax-rules ()
    ((when condition form ...)
     (if condition
	 (begin form ...)))))

(define-syntax $unless
  (syntax-rules ()
    ((unless condition form ...)
     (if (not condition)
	 (begin form ...)))))

(define-syntax $delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-unforced-promise (lambda () expression)))))

(define-syntax $delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise expression)))))

(define-syntax $parameterize
  (syntax-rules ()
    ((parameterize ((param value) ...) form ...)
     (parameterize* (list (cons param value) ...)
		    (lambda () form ...)))))

(define $guard
  (spar-transformer->runtime
   (delay
     (scons-rule `((subform id
			    (* ,cond-clause-pattern)
			    ,cond-else-clause-pattern)
		   (+ any))
       (lambda (var clauses else-actions body)
	 (let ((guard-k (new-identifier 'guard-k))
	       (condition (new-identifier 'condition)))
	   (scons-call 'call-with-current-continuation
		       (scons-lambda (list guard-k)
			 (scons-call 'with-exception-handler
				     (scons-lambda (list condition)
				       (scons-let (list (list var condition))
					 (guard-handler guard-k condition
							clauses else-actions)))
				     (apply scons-lambda '() body))))))))))

(define (guard-handler guard-k condition clauses else-actions)
  (if else-actions
      (scons-call guard-k
		  (fold-right expand-cond-clause
			      (apply scons-begin else-actions)
			      clauses))
      (fold-right (cond-clause-expander
		   (lambda (p c a)
		     (scons-if p (scons-call guard-k c) a)))
		  (scons-call 'raise-continuable condition)
		  clauses)))

(define $include
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ ,string?))
       (lambda (filenames)
	 (apply scons-begin (read-files filenames #f)))))))

(define $include-ci
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ ,string?))
       (lambda (filenames)
	 (apply scons-begin (read-files filenames #t)))))))

(define (read-files filenames fold-case?)
  (parameterize ((param:reader-fold-case? fold-case?))
    (append-map read-file filenames)))

(define $define-values
  (spar-transformer->runtime
   (delay
     (scons-rule `(,r4rs-lambda-list? any)
       (lambda (bvl expr)
	 (if (and (pair? bvl)
		  (null? (cdr bvl)))
	     (scons-define (car bvl) expr)
	     (let ((temp-bvl
		    (map-r4rs-lambda-list
		     (lambda (name)
		       (new-identifier (symbol 'temp- name)))
		     bvl)))
	       (let ((names (r4rs-lambda-list-names bvl))
		     (temps (r4rs-lambda-list-names temp-bvl)))
		 (scons-begin
		   (apply scons-begin
			  (map (lambda (name)
				 (scons-define name (unassigned-expression)))
			       names))
		   (scons-call 'call-with-values
			       (scons-lambda '() expr)
			       (apply scons-lambda
				      temp-bvl
				      (map* (list (unspecific-expression))
					    (lambda (name temp)
					      (scons-set! name temp))
					    names
					    temps))))))))))))

;;; This optimizes some simple cases, but it could be better.  Among other
;;; things it could take advantage of arity-dispatched procedures in the right
;;; circumstances.

(define $case-lambda
  (spar-transformer->runtime
   (delay
     (scons-rule `((* (subform (cons ,r4rs-lambda-list? (+ any)))))
       (lambda (clauses)
	 (if (pair? clauses)
	     (let ((clauses (case-lambda-eliminate-redundant-clauses clauses)))
	       (if (pair? (cdr clauses))
		   (let ((arities
			  (map r4rs-lambda-list-arity (map car clauses)))
			 (temps
			  (map (lambda (i)
				 (new-identifier (symbol 'p i)))
			       (iota (length clauses)))))
		     (scons-let (map (lambda (temp clause)
				       (list temp
					     (apply scons-lambda clause)))
				     temps
				     clauses)
		       (let ((choices (map cons arities temps)))
			 (if (every exact-nonnegative-integer? arities)
			     (case-lambda-no-rest choices)
			     (case-lambda-rest choices)))))
		   (apply scons-lambda (car clauses))))
	     (case-lambda-no-choices)))))))

(define (case-lambda-eliminate-redundant-clauses clauses)
  ;; For now just handle fixed arities.  Handling variable arities needs
  ;; something like intervals or an inversion list, which is a lot of hair.
  (let loop ((clauses clauses) (arities '()))
    (if (pair? clauses)
	(let ((arity (r4rs-lambda-list-arity (caar clauses))))
	  (if (memv arity arities)
	      (loop (cdr clauses) arities)
	      (cons (car clauses) (loop (cdr clauses) (cons arity arities)))))
	'())))

(define (case-lambda-no-rest choices)
  (let ((choices (sort choices (lambda (c1 c2) (fix:< (car c1) (car c2))))))
    (let ((low (apply min (map car choices)))
	  (high (apply max (map car choices))))
      (let ((args
	     (map (lambda (i)
		    (new-identifier (symbol 'a i)))
		  (iota high))))

	(define (choose i)
	  (let ((choice (assv i choices))
		(args* (list-head args i)))
	    (if choice
		(apply scons-call (cdr choice) args*)
		(scons-call 'error "No matching case-lambda clause:"
			    (apply scons-call 'list args*)))))

	(scons-lambda (append (list-head args low)
			      (list #!optional)
			      (list-tail args low))
	  (let loop ((i low))
	    (if (fix:< i high)
		(scons-if (scons-call 'default-object? (list-ref args i))
			  (choose i)
			  (loop (fix:+ i 1)))
		(choose i))))))))

(define (case-lambda-rest choices)
  (let ((args (new-identifier 'args))
	(nargs (new-identifier 'nargs)))
    (scons-lambda args
      (scons-let (list (list nargs (scons-call 'length args)))
	(let loop ((choices choices))
	  (if (pair? choices)
	      (scons-if (scons-call (if (procedure-arity-max (caar choices))
					'fix:=
					'fix:>=)
				    nargs
				    (procedure-arity-min (caar choices)))
			(scons-call 'apply (cdar choices) args)
			(loop (cdr choices)))
	      (scons-call 'error
			  "No matching case-lambda clause:"
			  args)))))))

(define (case-lambda-no-choices)
  (let ((args (new-identifier 'args)))
    (scons-lambda args
      (scons-call 'error "No matching case-lambda clause:" args))))

;;;; Quasiquote

(define-syntax $quasiquote
  (er-macro-transformer
   (lambda (form rename compare)

     (define (descend x level return)
       (cond ((pair? x) (descend-pair x level return))
	     ((vector? x) (descend-vector x level return))
	     (else (return 'quote x))))

     (define (descend-pair x level return)
       (cond ((quotation? 'quasiquote x)
	      (descend-pair* x (+ level 1) return))
	     ((quotation? 'unquote x)
	      (if (= level 0)
		  (return 'unquote (cadr x))
		  (descend-pair* x (- level 1) return)))
	     ((quotation? 'unquote-splicing x)
	      (if (= level 0)
		  (return 'unquote-splicing (cadr x))
		  (descend-pair* x (- level 1) return)))
	     (else
	      (descend-pair* x level return))))

     (define (quotation? name x)
       (and (pair? x)
	    (identifier? (car x))
	    (compare (rename name) (car x))
	    (pair? (cdr x))
	    (null? (cddr x))))

     (define (descend-pair* x level return)
       (descend (car x) level
	 (lambda (car-mode car-arg)
	   (descend (cdr x) level
	     (lambda (cdr-mode cdr-arg)
	       (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
		      (return 'quote x))
		     ((eq? car-mode 'unquote-splicing)
		      (if (and (eq? cdr-mode 'quote) (null? cdr-arg))
			  (return 'unquote car-arg)
			  (return 'append
				  (list car-arg
					(finalize cdr-mode cdr-arg)))))
		     ((and (eq? cdr-mode 'quote) (list? cdr-arg))
		      (return 'list
			      (cons (finalize car-mode car-arg)
				    (map (lambda (element)
					   (finalize 'quote element))
					 cdr-arg))))
		     ((eq? cdr-mode 'list)
		      (return 'list
			      (cons (finalize car-mode car-arg)
				    cdr-arg)))
		     (else
		      (return 'cons
			      (list (finalize car-mode car-arg)
				    (finalize cdr-mode cdr-arg))))))))))

     (define (descend-vector x level return)
       (descend (vector->list x) level
	 (lambda (mode arg)
	   (case mode
	     ((quote) (return 'quote x))
	     ((list) (return 'vector arg))
	     (else (return 'list->vector (list (finalize mode arg))))))))

     (define (finalize mode arg)
       (case mode
	 ((quote) `(,(rename 'quote) ,arg))
	 ((unquote) arg)
	 ((unquote-splicing) (syntax-error ",@ in illegal context:" arg))
	 (else `(,(rename mode) ,@arg))))

     (syntax-check '(_ expression) form)
     (descend (cadr form) 0 finalize))))

;;;; SRFI 0 and R7RS: cond-expand

(define $cond-expand
  (spar-transformer->runtime
   (delay
     (scons-rule `((value id=?)
		   (* (subform (cons ,(feature-requirement-pattern)
				     (* any))))
		   (opt (subform (cons (keep-if id=? else)
				       (* any)))))
       (lambda (id=? clauses)
	 (apply scons-begin (evaluate-cond-expand id=? clauses)))))))

(define (feature-requirement-pattern)
  (spar-pattern-fixed-point
   (lambda (feature-requirement)
     `(or (keep-if id!=? else)
	  (subform
	   (or (cons (or (keep-if id=? or)
			 (keep-if id=? and))
		     (* ,feature-requirement))
	       (list (keep-if id=? not)
		     ,feature-requirement)
	       (list (keep-if id=? library)
		     ,(library-name-pattern))))))))

(define (library-name-pattern)
  `(subform (* (or symbol ,exact-nonnegative-integer?))))

(define (evaluate-cond-expand id=? clauses)
  (let ((clause
	 (find (lambda (clause)
		 (or (id=? 'else (car clause))
		     (evaluate-feature-requirement id=? (car clause))))
	       clauses)))
    (if clause
	(cdr clause)
	'())))

(define (evaluate-feature-requirement id=? feature-requirement)

  (define (eval-req req)
    (cond ((identifier? req) (supported-feature? req))
	  ((id=? 'or (car req)) (eval-or (cdr req)))
	  ((id=? 'and (car req)) (eval-and (cdr req)))
	  ((id=? 'not (car req)) (eval-req (cadr req)))
	  (else (error "Unknown requirement:" req))))

  (define (supported-feature? req)
    (let ((p
	   (find (lambda (p)
		   (id=? (car p) req))
		 supported-features)))
      (and p
	   ((cdr p)))))

  (define (eval-or reqs)
    (and (pair? reqs)
	 (or (eval-req (car reqs))
	     (eval-or (cdr reqs)))))

  (define (eval-and reqs)
    (or (not (pair? reqs))
	(and (eval-req (car reqs))
	     (eval-and (cdr reqs)))))

  (eval-req feature-requirement))

(define (define-feature name procedure)
  (set! supported-features (cons (cons name procedure) supported-features))
  name)

(define supported-features '())

(define (always) #t)

(define-feature 'mit always)
(define-feature 'mit/gnu always)

;; r7rs features
(define-feature 'exact-closed always)
(define-feature 'exact-complex always)
(define-feature 'ieee-float always)
(define-feature 'full-unicode always)
(define-feature 'ratio always)

(define-feature 'swank always)   ;Provides SWANK module for SLIME
(define-feature 'srfi-0 always)  ;COND-EXPAND
(define-feature 'srfi-1 always)  ;List Library
(define-feature 'srfi-2 always)  ;AND-LET*
(define-feature 'srfi-6 always)  ;Basic String Ports
(define-feature 'srfi-8 always)  ;RECEIVE
(define-feature 'srfi-9 always)  ;DEFINE-RECORD-TYPE
(define-feature 'srfi-23 always) ;ERROR
(define-feature 'srfi-27 always) ;Sources of Random Bits
(define-feature 'srfi-30 always) ;Nested Multi-Line Comments (#| ... |#)
(define-feature 'srfi-39 always) ;Parameter objects
(define-feature 'srfi-62 always) ;S-expression comments
(define-feature 'srfi-69 always) ;Basic Hash Tables
(define-feature 'srfi-131 always) ;ERR5RS Record Syntax (reduced)

(define ((os? value))
  (eq? value microcode-id/operating-system))

(define-feature 'windows (os? 'nt))
(define-feature 'unix (os? 'unix))
(define-feature 'posix (os? 'unix))

(define ((os-variant? value))
  (string=? value microcode-id/operating-system-variant))

(define-feature 'darwin (os-variant? "OS X"))
(define-feature 'gnu-linux (os-variant? "GNU/Linux"))

(define-feature 'big-endian (lambda () (host-big-endian?)))
(define-feature 'little-endian (lambda () (not (host-big-endian?))))

(define ((machine? value))
  (string=? value microcode-id/machine-type))

(define-feature 'i386 (machine? "IA-32"))
(define-feature 'x86-64 (machine? "x86-64"))

(define (features)
  (filter-map (lambda (p)
		(and ((cdr p))
		     (car p)))
	      supported-features))

;;;; SRFI 9, SRFI 131, R7RS: define-record-type

(define $define-record-type
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or (and id (value #f))
	       (subform id any))
	   (or (and id (value #f))
	       (and ,not (value #f))
	       (subform id (* symbol)))
	   (or id ,not)
	   (* (subform (list symbol id (or id (value #f))))))
       (lambda (type-name parent maker-name maker-args pred-name field-specs)
	 (apply scons-begin
		(scons-define type-name
		  (scons-call (scons-close 'new-make-record-type)
			      (scons-quote type-name)
			      (scons-quote (map car field-specs))
			      (or parent (default-object))))
		(if maker-name
		    (scons-define maker-name
		      (scons-call (scons-close 'record-constructor)
				  type-name
				  (if maker-args
				      (scons-quote maker-args)
				      (default-object))))
		    (default-object))
		(if pred-name
		    (scons-define pred-name
		      (scons-call (scons-close 'record-predicate) type-name))
		    (default-object))
		(append-map (lambda (field-spec)
			      (let ((name (car field-spec))
				    (accessor (cadr field-spec))
				    (modifier (caddr field-spec)))
				(list (scons-define accessor
					(scons-call
					 (scons-close 'record-accessor)
					 type-name
					 (scons-quote name)))
				      (if modifier
					  (scons-define modifier
					    (scons-call
					     (scons-close 'record-modifier)
					     type-name
					     (scons-quote name)))
					  (default-object)))))
			    field-specs)))))))

;;;; MIT/GNU Scheme custom syntax

(define $cons-stream
  (spar-transformer->runtime
   (delay (scons-rule `(any any) scons-stream))))

(define $cons-stream*
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ any))
       (lambda (exprs)
	 (reduce-right scons-stream unspecific exprs))))))

(define (scons-stream expr1 expr2)
  (scons-call (scons-close 'cons)
	      expr1
	      (scons-delay expr2)))

(define $circular-stream
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ any))
       (lambda (exprs)
	 (let ((self (new-identifier 'self)))
	   (scons-letrec
	       (list (list self
			   (fold-right scons-stream
				       self
				       exprs)))
	     self)))))))

(define-syntax $local-declare
  (syntax-rules ()
    ((local-declare ((directive datum ...) ...) form0 form1+ ...)
     (let ()
       (declare (directive datum ...) ...)
       form0 form1+ ...))))

(define-syntax $begin0
  (syntax-rules ()
    ((begin0 form0 form1+ ...)
     (let ((result form0))
       form1+ ...
       result))))

(define-syntax $assert
  (syntax-rules ()
    ((assert condition . extra)
     (if (not condition)
         (error "Assertion failed:" 'condition . extra)))))

(define $define-integrable
  (spar-transformer->runtime
   (delay
     (spar-or
       (scons-rule `(id any)
	 (lambda (name expr)
	   (scons-begin
	     (scons-declare (list 'integrate name))
	     (scons-define name expr))))
       (scons-rule `((subform id (* id)) (+ any))
	 (lambda (name bvl body-forms)
	   (scons-begin
	     (scons-declare (list 'integrate-operator name))
	     (scons-define name
	       (apply scons-named-lambda
		      (cons name bvl)
		      (if (null? bvl)
			  body-forms
			  (cons (scons-declare (cons 'integrate bvl))
				body-forms)))))))))))

(define $fluid-let
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (let ((temps
		  (map (lambda (id)
			 (new-identifier (symbol 'temp- id)))
		       ids))
		 (swap! (new-identifier 'swap!)))
	     (scons-let (map list temps vals)
	       (scons-define swap!
		 (scons-lambda '()
		   (apply scons-begin
			  (map (lambda (id temp)
				 (scons-set! id
					     (scons-set! temp
							 (scons-set! id))))
			       ids
			       temps))
		   #f))
	       (scons-call (scons-close 'shallow-fluid-bind)
			   swap!
			   (apply scons-lambda '() body-forms)
			   swap!)))))))))

(define-syntax $bundle
  (syntax-rules ()
    (($bundle predicate name ...)
     (alist->bundle predicate
                    (list (cons 'name name) ...)))))