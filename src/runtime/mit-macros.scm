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
	(spar-push-subform-if identifier? spar-arg:form)
	(spar-subform
	  (spar-call-with-values list
	    (spar-or
	      (spar-and (spar-push-subform-if spar-arg:id=? 'or)
			(spar* clause-pattern*)
			(spar-match-null))
	      (spar-and (spar-push-subform-if spar-arg:id=? 'and)
			(spar* clause-pattern*)
			(spar-match-null))
	      (spar-and (spar-push-subform-if spar-arg:id=? 'not)
			clause-pattern*
			(spar-match-null))))))))
  `((value id=?)
    (+ (subform (cons (spar ,clause-pattern)
		      (* any))))))

(define (generate-cond-expand id=? clauses)

  (define (process-clauses clauses)
    (cond ((not (pair? clauses))
	   (generate '()))
	  ((id=? 'else (caar clauses))
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
	  ((id=? 'or (car req)) (eval-or (cdr req) success failure))
	  ((id=? 'and (car req)) (eval-and (cdr req) success failure))
	  ((id=? 'not (car req)) (eval-req (cadr req) failure success))
	  (else (error "Unknown requirement:" req))))

  (define (supported-feature? req)
    (let ((p
	   (find (lambda (p)
		   (id=? (car p) req))
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
     (scons-rule `(,r4rs-lambda-list? any (+ any))
       (lambda (bvl expr body-forms)
	 (scons-call (scons-close 'call-with-values)
		     (scons-lambda '() expr)
		     (apply scons-lambda bvl body-forms)))))
   system-global-environment))

(define :define-record-type
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
	     (apply scons-lambda bvl body-forms))))))
   system-global-environment))

(define (optional-value-pattern)
  `(or any (value-of ,unassigned-expression)))

(define :let
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
		      vals))))))
   system-global-environment))

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

(define :let*
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `(,(let-bindings-pattern)
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let bindings body-forms))))
   system-global-environment))

(define :let*-syntax
  (spar-transformer->runtime
   (delay
     (scons-rule
	 '((subform (* (subform (list id any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (expand-let* scons-let-syntax bindings body-forms))))
   system-global-environment))

(define (expand-let* scons-let bindings body-forms)
  (fold-right (lambda (binding expr)
		(scons-let (list binding) expr))
	      (apply scons-let '() body-forms)
	      bindings))

(define :letrec
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
	   (+ any))
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
     (scons-rule '((* any))
       (lambda (exprs)
	 (reduce-right (lambda (expr1 expr2)
			 (scons-if expr1 expr2 #f))
		       #t
		       exprs))))
   system-global-environment))

(define :case
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
			 clauses))))))
   system-global-environment))

(define :cond
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((* ,cond-clause-pattern)
	   (or (subform (ignore-if id=? else)
			(+ any))
	       (value #f)))
       (lambda (clauses else-actions)
	 (fold-right expand-cond-clause
		     (if else-actions
			 (apply scons-begin else-actions)
			 (unspecific-expression))
		     clauses))))
   system-global-environment))

(define cond-clause-pattern
  '(subform (cons (and (not (ignore-if id=? else))
		       any)
		  (if (ignore-if id=? =>)
		      (list (value =>)
			    any)
		      (cons (value begin)
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

(define :and-let*
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
		  conjunct))))))
   system-global-environment))

(define :access
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((+ symbol)
	   any)
       (lambda (names expr)
	 (fold-right (lambda (name expr)
		       (scons-call keyword:access name expr))
		     expr
		     names))))
   system-global-environment))

(define :cons-stream
  (spar-transformer->runtime
   (delay (scons-rule `(any any) scons-stream))
   system-global-environment))

(define :cons-stream*
  (spar-transformer->runtime
   (delay
     (scons-rule `((+ any))
       (lambda (exprs)
	 (if (pair? (cdr exprs))
	     (car exprs)
	     (reduce-right scons-stream unspecific exprs)))))
   system-global-environment))

(define (scons-stream expr1 expr2)
  (scons-call (scons-close 'cons)
	      expr1
	      (scons-delay expr2)))

(define :circular-stream
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
	     self)))))
   system-global-environment))

(define :define-integrable
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
				body-forms)))))))))
   system-global-environment))

(define :fluid-let
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
			   swap!)))))))
   system-global-environment))

(define :parameterize
  (spar-transformer->runtime
   (delay
     (scons-rule
	 `((subform (* (subform (list id any))))
	   (+ any))
       (lambda (bindings body-forms)
	 (let ((ids (map car bindings))
	       (vals (map cadr bindings)))
	   (scons-call (scons-close 'parameterize*)
		       (apply scons-call
			      (scons-close 'list)
			      (map (lambda (id val)
				     (scons-call (scons-close 'cons) id val))
				   ids
				   vals))
		       (apply scons-lambda '() body-forms))))))
   system-global-environment))

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