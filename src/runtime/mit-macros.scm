#| -*- Mode: Scheme; keyword-style: none -*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; SRFI features

(define :cond-expand
  (spar-transformer->runtime
   (delay (scons-rule (cond-expand-pattern) generate-cond-expand))
   system-global-environment))

(define (cond-expand-pattern)
  (define clause-pattern
    (let ((clause-pattern* (lambda args (apply clause-pattern args))))
      (spar-or
	(spar-push-elt-if identifier? spar-arg:form)
	(spar-call-with-values list
	  (spar-elt
	    (spar-or
	      (spar-and (spar-push-elt-if spar-arg:compare 'or spar-arg:form)
			(spar* clause-pattern*)
			(spar-match-null))
	      (spar-and (spar-push-elt-if spar-arg:compare 'and spar-arg:form)
			(spar* clause-pattern*)
			(spar-match-null))
	      (spar-and (spar-push-elt-if spar-arg:compare 'not spar-arg:form)
			clause-pattern*
			(spar-match-null))))))))
  `((values compare)
    (list (+ (list (elt (spar ,clause-pattern)
			(* any)))))))

(define (generate-cond-expand compare clauses)

  (define (process-clauses clauses)
    (cond ((not (pair? clauses))
	   (generate '()))
	  ((compare 'else (caar clauses))
	   (if (pair? (cdr clauses))
	       (syntax-error "ELSE clause must be last:" clauses))
	   (generate (cdar clauses)))
	  (else
	   (process-clause (car clauses)
			   (lambda () (process-clauses (cdr clauses)))))))

  (define (process-clause clause failure)
    (eval-req (car clause)
	      (lambda () (generate (cdr clause)))
	      failure))

  (define (eval-req req success failure)
    (cond ((identifier? req) (if (supported-feature? req) (success) (failure)))
	  ((compare 'or (car req)) (eval-or (cdr req) success failure))
	  ((compare 'and (car req)) (eval-and (cdr req) success failure))
	  ((compare 'not (car req)) (eval-req (cadr req) failure success))
	  (else (error "Unknown requirement:" req))))

  (define (supported-feature? req)
    (let ((p
	   (find (lambda (p)
		   (compare (car p) req))
		 supported-features)))
      (and p
	   ((cdr p)))))

  (define (eval-or reqs success failure)
    (if (pair? reqs)
	(eval-req (car reqs)
		  success
		  (lambda () (eval-or (cdr reqs) success failure)))
	(failure)))

  (define (eval-and reqs success failure)
    (if (pair? reqs)
	(eval-req (car reqs)
		  (lambda () (eval-and (cdr reqs) success failure))
		  failure)
	(success)))

  (define (generate forms)
    (apply scons-begin forms))

  (process-clauses clauses))

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

(define (get-supported-features)
  (filter-map (lambda (p)
		(and ((cdr p))
		     (car p)))
	      supported-features))

(define :receive
  (spar-transformer->runtime
   (delay
     (scons-rule `(,r4rs-lambda-list? any (list (+ any)))
       (lambda (bvl expr body-forms)
	 (scons-call (scons-close 'call-with-values)
		     (scons-lambda '() expr)
		     (apply scons-lambda bvl body-forms)))))
   system-global-environment))

(define :define-record-type
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or (and id (values #f))
	       (elt id any))
	   (or (and id (values #f))
	       (and ,not (values #f))
	       (elt id (list (* symbol))))
	   (or id ,not)
	   (list (* (list (elt symbol id (or id (values #f)))))))
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
			    field-specs)))))
   system-global-environment))

(define :define
  (spar-transformer->runtime
   (delay
     (spar-or
       (scons-rule `(id ,(optional-value-pattern))
	 (lambda (name value)
	   (scons-call keyword:define name value)))
       (scons-rule
	   `((spar
	      ,(spar-elt
		 (spar-push-elt-if identifier? spar-arg:form)
		 (spar-push-form-if mit-lambda-list? spar-arg:form)))
	     (list (+ any)))
	 (lambda (name bvl body-forms)
	   (scons-define name
	     (apply scons-named-lambda (cons name bvl) body-forms))))
       (scons-rule
	   `((spar
	      ,(spar-elt
		 (spar-push-elt)
		 (spar-push-form-if mit-lambda-list? spar-arg:form)))
	     (list (+ any)))
	 (lambda (nested bvl body-forms)
	   (scons-define nested
	     (apply scons-lambda bvl body-forms))))))
   system-global-environment))

(define (optional-value-pattern)
  `(or any (value-of ,unassigned-expression)))

(define :let
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((or id (values #f))
	   ,(let-bindings-pattern)
	   (list (+ any)))
       (lambda (name bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (if name
	       (generate-named-let name ids vals body-forms)
	       (apply scons-call
		      (apply scons-named-lambda
			     (cons scode-lambda-name:let ids)
			     body-forms)
		      vals))))))
   system-global-environment))

(define (let-bindings-pattern)
  `(elt (list (* (elt (list id ,(optional-value-pattern)))))))

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

(define :let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (list (+ any)))
       (lambda (bindings body-forms)
	 (expand-let* scons-let bindings body-forms))))
   system-global-environment))

(define :let*-syntax
  (spar-transformer->runtime
   (delay
     (scons-rule
	 '((elt (list (* (elt (list id any)))))
	   (list (+ any)))
       (lambda (bindings body-forms)
	 (expand-let* scons-let-syntax bindings body-forms))))
   system-global-environment))

(define (expand-let* scons-let bindings body-forms)
  (if (pair? bindings)
      (let loop ((bindings bindings))
	(if (pair? (cdr bindings))
	    (scons-let (list (car bindings)) (loop (cdr bindings)))
	    (apply scons-let (list (car bindings)) body-forms)))
      (apply scons-let '() body-forms)))

(define :letrec
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (list (+ any)))
       (lambda (bindings body-forms)
	 (let* ((ids (map car bindings))
		(vals (map cadr bindings))
		(temps (map new-identifier ids)))
	   (scons-let (map (lambda (id)
			     (list id (unassigned-expression)))
			   ids)
	     (apply scons-let
		    (map list temps vals)
		    (map scons-set! ids temps))
	     (scons-call (apply scons-lambda '() body-forms)))))))
   system-global-environment))

(define :letrec*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (list (+ any)))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (scons-let (map (lambda (id)
			     (list id (unassigned-expression)))
			   ids)
	     (apply scons-begin (map scons-set! ids vals))
	     (scons-call (apply scons-lambda '() body-forms)))))))
   system-global-environment))

(define :and
  (spar-transformer->runtime
   (delay
     (scons-rule '((list (* any)))
       (lambda (exprs)
	 (if (pair? exprs)
	     (let loop ((exprs exprs))
	       (if (pair? (cdr exprs))
		   (scons-if (car exprs)
			     (loop (cdr exprs))
			     #f)
		   (car exprs)))
	     #t))))
   system-global-environment))

(define :case
  (spar-transformer->runtime
   (delay
     (scons-rule
	 (let ((action-pattern
		'(if (keyword =>)
		     any
		     (and (values begin)
			  (+ any)))))
	   `(any
	     (list (* (list (elt (list (elt (* any)))
				 ,action-pattern))))
	     (or (list (elt (noise-keyword else)
			    ,action-pattern))
		 (values #f))))
       (lambda (expr clauses else-clause)
	 (let ((temp (new-identifier 'key)))

	   (define (process-clauses clauses)
	     (cond ((pair? clauses)
		    (process-clause (car clauses)
				    (process-clauses (cdr clauses))))
		   (else-clause
		    (process-action (car else-clause) (cdr else-clause)))
		   (else
		    (unspecific-expression))))

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
	     (process-clauses clauses))))))
   system-global-environment))

(define :cond
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((list (* ,cons-clause-pattern))
	   (or (list (elt (noise-keyword else)
			  (+ any)))
	       (values #f)))
       (lambda (clauses else-actions)
	 (let loop ((clauses clauses))
	   (cond ((pair? clauses)
		  (expand-cond-clause (car clauses)
				      (loop (cdr clauses))))
		 (else-actions (apply scons-begin else-actions))
		 (else (unspecific-expression)))))))
   system-global-environment))

(define cons-clause-pattern
  '(list (elt (and (not (noise-keyword else))
		   any)
	      (if (keyword =>)
		  any
		  (and (values begin)
		       (* any))))))

(define (expand-cond-clause clause rest)
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
	   (scons-or predicate rest)))
      (else
       (error "Unknown clause type:" type)))))

(define :do
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((list (elt (* (list (elt id any (? any))))))
	   ,cons-clause-pattern
	   (list (* any)))
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
					       bindings)))))))))
   system-global-environment))

(define-syntax :quasiquote
  (er-macro-transformer
   (lambda (form rename compare)

     (define (descend-quasiquote x level return)
       (cond ((pair? x) (descend-quasiquote-pair x level return))
	     ((vector? x) (descend-quasiquote-vector x level return))
	     (else (return 'QUOTE x))))

     (define (descend-quasiquote-pair x level return)
       (cond ((not (and (pair? x)
			(identifier? (car x))
			(pair? (cdr x))
			(null? (cddr x))))
	      (descend-quasiquote-pair* x level return))
	     ((compare (rename 'QUASIQUOTE) (car x))
	      (descend-quasiquote-pair* x (+ level 1) return))
	     ((compare (rename 'UNQUOTE) (car x))
	      (if (zero? level)
		  (return 'UNQUOTE (cadr x))
		  (descend-quasiquote-pair* x (- level 1) return)))
	     ((compare (rename 'UNQUOTE-SPLICING) (car x))
	      (if (zero? level)
		  (return 'UNQUOTE-SPLICING (cadr x))
		  (descend-quasiquote-pair* x (- level 1) return)))
	     (else
	      (descend-quasiquote-pair* x level return))))

     (define (descend-quasiquote-pair* x level return)
       (descend-quasiquote (car x) level
	 (lambda (car-mode car-arg)
	   (descend-quasiquote (cdr x) level
	     (lambda (cdr-mode cdr-arg)
	       (cond ((and (eq? car-mode 'QUOTE) (eq? cdr-mode 'QUOTE))
		      (return 'QUOTE x))
		     ((eq? car-mode 'UNQUOTE-SPLICING)
		      (if (and (eq? cdr-mode 'QUOTE) (null? cdr-arg))
			  (return 'UNQUOTE car-arg)
			  (return 'APPEND
				  (list car-arg
					(finalize-quasiquote cdr-mode
							     cdr-arg)))))
		     ((and (eq? cdr-mode 'QUOTE) (list? cdr-arg))
		      (return 'LIST
			      (cons (finalize-quasiquote car-mode car-arg)
				    (map (lambda (element)
					   (finalize-quasiquote 'QUOTE
								element))
					 cdr-arg))))
		     ((eq? cdr-mode 'LIST)
		      (return 'LIST
			      (cons (finalize-quasiquote car-mode car-arg)
				    cdr-arg)))
		     (else
		      (return
		       'CONS
		       (list (finalize-quasiquote car-mode car-arg)
			     (finalize-quasiquote cdr-mode cdr-arg))))))))))

     (define (descend-quasiquote-vector x level return)
       (descend-quasiquote (vector->list x) level
	 (lambda (mode arg)
	   (case mode
	     ((QUOTE) (return 'QUOTE x))
	     ((LIST) (return 'VECTOR arg))
	     (else
	      (return 'LIST->VECTOR
		      (list (finalize-quasiquote mode arg))))))))

     (define (finalize-quasiquote mode arg)
       (case mode
	 ((QUOTE) `(,(rename 'QUOTE) ,arg))
	 ((UNQUOTE) arg)
	 ((UNQUOTE-SPLICING) (syntax-error ",@ in illegal context:" arg))
	 (else `(,(rename mode) ,@arg))))

     (syntax-check '(_ expression) form)
     (descend-quasiquote (cadr form) 0 finalize-quasiquote))))

;;;; SRFI 2: AND-LET*

;;; The SRFI document is a little unclear about the semantics, imposes
;;; the weird restriction that variables may be duplicated (citing
;;; LET*'s similar restriction, which doesn't actually exist), and the
;;; reference implementation is highly non-standard and hard to
;;; follow.  This passes all of the tests except for the one that
;;; detects duplicate bound variables, though.

(define-syntax :and-let*
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (let ((%and (rename 'AND))
	   (%let (rename 'LET))
	   (%begin (rename 'BEGIN)))
       (cond ((syntax-match? '(() * form) (cdr form))
	      `(,%begin #T ,@(cddr form)))
	     ((syntax-match? '((* datum) * form) (cdr form))
	      (let ((clauses (cadr form))
		    (body (cddr form)))
		(define (expand clause recur)
		  (cond ((syntax-match? 'identifier clause)
			 (recur clause))
			((syntax-match? '(expression) clause)
			 (recur (car clause)))
			((syntax-match? '(identifier expression) clause)
			 (let ((tail (recur (car clause))))
			   (and tail `(,%let (,clause) ,tail))))
			(else #f)))
		(define (recur clauses make-body)
		  (expand (car clauses)
			  (let ((clauses (cdr clauses)))
			    (if (null? clauses)
				make-body
				(lambda (conjunct)
				  `(,%and ,conjunct
					  ,(recur clauses make-body)))))))
		(or (recur clauses
			   (if (null? body)
			       (lambda (conjunct) conjunct)
			       (lambda (conjunct)
				 `(,%and ,conjunct (,%begin ,@body)))))
		    (ill-formed-syntax form))))
	     (else
	      (ill-formed-syntax form)))))))

(define-syntax :access
  (er-macro-transformer
   (lambda (form rename compare)
     rename compare			;ignore
     (cond ((syntax-match? '(identifier expression) (cdr form))
	    `(,keyword:access ,@(cdr form)))
	   ((syntax-match? '(identifier identifier + form) (cdr form))
	    `(,keyword:access ,(cadr form) (,(car form) ,@(cddr form))))
	   (else
	    (ill-formed-syntax form))))))

(define-syntax :circular-stream
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (syntax-check '(_ expression * expression) form)
     (let ((self (make-synthetic-identifier 'SELF)))
       `(,(rename 'LETREC) ((,self (,(rename 'CONS-STREAM*)
				    ,@(cdr form)
				    ,self)))
	 ,self)))))

(define-syntax :cons-stream
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (syntax-check '(_ expression expression) form)
     `(,(rename 'CONS) ,(cadr form)
		       (,(rename 'DELAY) ,(caddr form))))))

(define-syntax :cons-stream*
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (cond ((syntax-match? '(expression expression) (cdr form))
	    `(,(rename 'CONS-STREAM) ,(cadr form) ,(caddr form)))
	   ((syntax-match? '(expression * expression) (cdr form))
	    `(,(rename 'CONS-STREAM) ,(cadr form)
	      (,(rename 'CONS-STREAM*) ,@(cddr form))))
	   (else
	    (ill-formed-syntax form))))))

(define-syntax :define-integrable
  (er-macro-transformer
   (lambda (form rename compare)
     compare				;ignore
     (let ((r-begin (rename 'BEGIN))
	   (r-declare (rename 'DECLARE))
	   (r-define (rename 'DEFINE)))
       (cond ((syntax-match? '(identifier expression) (cdr form))
	      `(,r-begin
		(,r-declare (INTEGRATE ,(cadr form)))
		(,r-define ,@(cdr form))))
	     ((syntax-match? '((identifier * identifier) + form) (cdr form))
	      `(,r-begin
		(,r-declare (INTEGRATE-OPERATOR ,(caadr form)))
		(,r-define ,(cadr form)
			   ,@(let ((arguments (cdadr form)))
			       (if (null? arguments)
				   '()
				   `((,r-declare (INTEGRATE ,@arguments)))))
			   ,@(cddr form))))
	     (else
	      (ill-formed-syntax form)))))))

(define-syntax :fluid-let
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (syntax-check '(_ (* (form ? expression)) + form) form)
     (let ((left-hand-sides (map car (cadr form)))
	   (right-hand-sides (map cdr (cadr form)))
	   (r-define (rename 'DEFINE))
	   (r-lambda (rename 'LAMBDA))
	   (r-let (rename 'LET))
	   (r-set! (rename 'SET!))
	   (r-shallow-fluid-bind (rename 'SHALLOW-FLUID-BIND))
	   (r-unspecific (rename 'UNSPECIFIC)))
       (let ((temporaries
	      (map (lambda (lhs)
		     (make-synthetic-identifier
		      (if (identifier? lhs) lhs 'TEMPORARY)))
		   left-hand-sides))
	     (swap! (make-synthetic-identifier 'SWAP!))
	     (body `(,r-lambda () ,@(cddr form))))
	 `(,r-let ,(map cons temporaries right-hand-sides)
	    (,r-define (,swap!)
	      ,@(map (lambda (lhs temporary)
		       `(,r-set! ,lhs (,r-set! ,temporary (,r-set! ,lhs))))
		     left-hand-sides
		     temporaries)
	      ,r-unspecific)
	    (,r-shallow-fluid-bind ,swap! ,body ,swap!)))))))

(define-syntax :parameterize
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (syntax-check '(_ (* (expression expression)) + form) form)
     (let ((r-parameterize* (rename 'parameterize*))
	   (r-list (rename 'list))
	   (r-cons (rename 'cons))
	   (r-lambda (rename 'lambda)))
       `(,r-parameterize*
	 (,r-list
	  ,@(map (lambda (binding)
		   `(,r-cons ,(car binding) ,(cadr binding)))
		 (cadr form)))
	 (,r-lambda () ,@(cddr form)))))))

(define-syntax :local-declare
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (syntax-check '(_ (* (identifier * datum)) + form) form)
     (let ((r-let (rename 'LET))
	   (r-declare (rename 'DECLARE)))
       `(,r-let ()
		(,r-declare ,@(cadr form))
		,@(cddr form))))))

(define (unspecific-expression)
  `(,keyword:unspecific))

(define (unassigned-expression)
  `(,keyword:unassigned))

(define-syntax :begin0
  (syntax-rules ()
    ((BEGIN0 form0 form1+ ...)
     (LET ((RESULT form0))
       form1+ ...
       RESULT))))

(define-syntax :assert
  (syntax-rules ()
    ((ASSERT condition . extra)
     (IF (NOT condition)
         (ERROR "Assertion failed:" 'condition . extra)))))

(define-syntax :when
  (syntax-rules ()
    ((when condition form ...)
     (if condition
	 (begin form ...)))))

(define-syntax :unless
  (syntax-rules ()
    ((unless condition form ...)
     (if (not condition)
	 (begin form ...)))))

(define-syntax :define-bundle-interface
  (sc-macro-transformer
   (lambda (form use-env)
     (syntax-check '(_ identifier identifier identifier
		       * (or symbol (symbol * (symbol * expression))))
		   form)
     (make-interface-helper (close-syntax (cadr form) use-env)
			    (close-syntax (caddr form) use-env)
			    (close-syntax (cadddr form) use-env)
			    (cddddr form)))))

(define (make-interface-helper interface constructor capturer elements)
  `(begin
     (define ,interface
       (make-bundle-interface
	',(let* ((name (identifier->symbol interface))
		 (s (symbol->string name)))
	    (if (string-suffix? "?" s)
		(string->symbol (string-head s (fix:- (string-length s) 1)))
		name))
	(list ,@(map (lambda (element)
		       (if (symbol? element)
			   `',element
			   `(list ',(car element)
				  ,@(map (lambda (p)
					   `(list ',(car p) ,@(cdr p)))
					 (cdr element)))))
		     elements))))
     (define ,constructor
       (bundle-constructor ,interface))
     (define-syntax ,capturer
       (sc-macro-transformer
	(lambda (form use-env)
	  (syntax-check '(_) form)
	  (list (quote-identifier ,constructor)
		,@(map (lambda (element)
			 `(close-syntax
			   ',(if (symbol? element)
				 element
				 (car element))
			   use-env))
		       elements)))))))