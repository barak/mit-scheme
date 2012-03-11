#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Very Simple Pattern Matcher: Early rule compilation and lookup

(declare (usual-integrations))

;;;; Database construction

(define (early-make-rule pattern variables body)
  (list pattern variables body))

(define (early-parse-rule pattern receiver)
  (extract-variables pattern receiver))

(define (extract-variables pattern receiver)
  (cond ((not (pair? pattern))
	 (receiver pattern '()))
	((eq? (car pattern) '@)
	 (error "early-parse-rule: ?@ is not an implemented pattern"
		pattern))
	((eq? (car pattern) '?)
	 (receiver (make-pattern-variable (cadr pattern))
		   (list (cons (cadr pattern)
			       (if (null? (cddr pattern))
				   '()
				   (list (cons (car pattern)
					       (cddr pattern))))))))
	(else
	 (extract-variables (car pattern)
	  (lambda (car-pattern car-variables)
	    (extract-variables (cdr pattern)
	     (lambda (cdr-pattern cdr-variables)
	       (receiver (cons car-pattern cdr-pattern)
			 (merge-variables-lists car-variables
						cdr-variables)))))))))

(define (merge-variables-lists x y)
  (cond ((null? x) y)
	((null? y) x)
	(else
	 (let ((entry (assq (caar x) y)))
	   (if entry
	       #|
	       (cons (append! (car x) (cdr entry))
		     (merge-variables-lists (cdr x)
					    (delq! entry y)))
	       |#
	       (error "early-parse-rule: repeated variables not supported"
		      (list (caar x) entry))
	       (cons (car x)
		     (merge-variables-lists (cdr x)
					    y)))))))

;;;; Early rule processing and code compilation

(define (early-pattern-lookup rules instance #!optional transformers unparsed
			      receiver limit)
  (if (default-object? limit) (set! limit *rule-limit*))
  (if (or (default-object? receiver) (null? receiver))
      (set! receiver
	    (lambda (result code)
	      (cond ((false? result)
		     (error "early-pattern-lookup: No pattern matches"
			    instance))
		    ((eq? result 'TOO-MANY)
		     (error "early-pattern-lookup: Too many patterns match"
			    limit instance))
		    (else code)))))
  (parse-instance instance
   (lambda (expression bindings)
     (apply (lambda (result program)
	      (receiver result
			(if (or (eq? result true) (eq? result 'MAYBE))
			    (scode/make-block bindings '() program)
			    false)))
	    (fluid-let ((*rule-limit* limit)
			(*transformers* (if (default-object? transformers)
					    '()
					    transformers)))
	      (try-rules rules expression
			 (scode/make-error-combination
			  "early-pattern-lookup: No pattern matches"
			  (if (or (default-object? unparsed) (null? unparsed))
			      (scode/make-constant instance)
			      unparsed))
			 list))))))

(define (parse-instance instance receiver)
  (cond ((not (pair? instance))
	 (receiver instance '()))
	((eq? (car instance) 'UNQUOTE)
	 ;; Shadowing may not permit the optimization below.
	 ;; I think the code is being careful, but...
	 (let ((expression (cadr instance)))
	   (if (scode/variable? expression)
	       (receiver (make-evaluation expression) '())
	       (let ((var (make-variable-name 'RESULT)))
		 (receiver (make-evaluation (scode/make-variable var))
			   (list (scode/make-binding var expression)))))))
	((eq? (car instance) 'UNQUOTE-SPLICING)
	 (error "parse-instance: unquote-splicing not supported" instance))
	(else (parse-instance (car instance)
	       (lambda (instance-car car-bindings)
		 (parse-instance (cdr instance)
		  (lambda (instance-cdr cdr-bindings)
		    (receiver (cons instance-car instance-cdr)
			      (append car-bindings cdr-bindings)))))))))

;;;; Find matching rules and collect them

(define *rule-limit* '())

(define (try-rules rules expression null-form receiver)
  (define (loop rules null-form bindings nrules)
    (cond ((and (not (null? *rule-limit*))
		(> nrules *rule-limit*))
	   (receiver 'TOO-MANY false))
	  ((not (null? rules))
	   (try-rule (car rules)
		     expression
		     null-form
	    (lambda (result code)
	      (cond ((false? result)
		     (loop (cdr rules) null-form bindings nrules))
		    ((eq? result 'MAYBE)
		     (let ((var (make-variable-name 'TRY-NEXT-RULE-)))
		       (loop (cdr rules)
			     (scode/make-combination (scode/make-variable var)
						     '())
			     (cons (cons var code)
				   bindings)
			     (1+ nrules))))
		    (else (receiver true code))))))
	  ((null? bindings)
	   (receiver false null-form))
	  ((null? (cdr bindings))
	   (receiver 'MAYBE (cdar bindings)))
	  (else
	   (receiver 'MAYBE
		     (scode/make-letrec
		      (map (lambda (pair)
			     (scode/make-binding
			      (car pair)
			      (scode/make-thunk (cdr pair))))
			   bindings)
		      null-form)))))
  (loop rules null-form '() 0))

;;;; Match one rule

(define (try-rule rule expression null-form continuation)
  (define (try pattern expression receiver)
    (cond ((evaluation? expression)
	   (receiver '() (list (cons expression pattern))))
	  ((not (pair? pattern))
	   (if (eqv? pattern expression)
	       (receiver '() '())
	       (continuation false null-form)))
	  ((pattern-variable? pattern)
	   (receiver (list (cons (pattern-variable-name pattern) expression))
		     '()))
	  ((not (pair? expression))
	   (continuation false null-form))
	  (else
	   (try (car pattern)
		(car expression)
		(lambda (car-bindings car-evaluations)
		  (try (cdr pattern)
		       (cdr expression)
		       (lambda (cdr-bindings cdr-evaluations)
			 (receiver (append car-bindings cdr-bindings)
				   (append car-evaluations
					   cdr-evaluations)))))))))
  (try (car rule)
       expression
       (lambda (bindings evaluations)
	 (match-bind bindings evaluations
		     (cadr rule) (caddr rule)
		     null-form continuation))))

;;;; Early rule processing

(define (match-bind bindings evaluations variables body null-form receiver)
  (process-evaluations evaluations true bindings
   (lambda (outer-test bindings)
     (define (find-early-bindings original test bindings)
       (if (null? original)
	   (generate-match-code outer-test test
				bindings body
				null-form receiver)
	   (bind-variable-early (car original)
				variables
	    (lambda (var-test var-bindings)
	      (if (false? var-test)
		  (receiver false null-form)
		  (find-early-bindings (cdr original)
				       (scode/merge-tests var-test test)
				       (append var-bindings bindings)))))))
     (if (false? outer-test)
	 (receiver false null-form)
	 (find-early-bindings bindings true '())))))

(define (process-evaluations evaluations test bindings receiver)
  (if (null? evaluations)
      (receiver test bindings)
      (let ((evaluation (car evaluations)))
	(build-comparison (cdr evaluation)
			  (cdar evaluation)
			  (lambda (new-test new-bindings)
			    (process-evaluations
			     (cdr evaluations)
			     (scode/merge-tests new-test test)
			     (append new-bindings bindings)
			     receiver))))))

;;;; Early variable processing

(define (bind-variable-early var+pattern variables receiver)
  (let ((name (car var+pattern))
	(expression (cdr var+pattern)))
    (let ((var (assq name variables)))
      (cond ((not var)
	     (error "match-bind: nonexistent variable"
		    name variables))
	    ((null? (cdr var))
	     (let ((exp (unevaluate expression)))
	       (receiver true
			 (list
			  (if (scode/constant? exp)
			      (make-early-binding name exp)
			      (make-outer-binding name exp))))))
	    (else
	     (if (not (eq? (caadr var) '?))
		 (error "match-bind: ?@ unimplemented" var))
	     (let ((transformer (cadr (cadr var)))
		   (rename (if (null? (cddr (cadr var)))
			       name
			       (caddr (cadr var)))))
	       (apply-transformer-early transformer name rename
					expression receiver)))))))

(define (unevaluate exp)
  (cond ((not (pair? exp))
	 (scode/make-constant exp))
	((evaluation? exp)
	 (evaluation-expression exp))
	(else
	 (let ((the-car (unevaluate (car exp)))
	       (the-cdr (unevaluate (cdr exp))))
	  (if (and (scode/constant? the-car)
		   (scode/constant? the-cdr))
	      (scode/make-constant (cons (scode/constant-value the-car)
					 (scode/constant-value the-cdr)))
	      (scode/make-absolute-combination 'CONS
					       (list the-car the-cdr)))))))

;;;; Rule output code

(define (generate-match-code testo testi bindings body null-form receiver)
  (define (scode/make-test test body)
    (if (eq? test true)
	body
	(scode/make-conditional test body null-form)))

  (define (collect-bindings bindings outer late early outer-names early-names)
    (if (null? bindings)
	(receiver
	 (if (and (eq? testo true) (eq? testi true))
	     true
	     'MAYBE)
	 (scode/make-test
	  testo
	  (scode/make-block
	   outer outer-names
	   (scode/make-block late '()
			     (scode/make-test
			      testi
			      (scode/make-block early early-names
						body))))))
	(let ((binding (cdar bindings)))
	  (case (caar bindings)
	    ((OUTER)
	     (collect-bindings
	      (cdr bindings) (cons binding outer)
	      late early
	      (if (or (scode/constant? (scode/binding-value binding))
		      (scode/variable? (scode/binding-value binding)))
		  (cons (scode/binding-variable binding)
			outer-names)
		  outer-names)
	      early-names))
	    ((LATE)
	     (collect-bindings (cdr bindings) outer
			       (cons binding late) early
			       outer-names early-names))
	    ((EARLY)
	     (collect-bindings (cdr bindings) outer
			       late (cons binding early)
			       outer-names
			       (cons (scode/binding-variable binding)
				     early-names)))
	    (else (error "collect bindings: Unknown type of binding"
			 (caar bindings)))))))
  (collect-bindings bindings '() '() '() '() '()))

(define ((make-binding-procedure keyword) name exp)
  (cons keyword (scode/make-binding name exp)))

(define make-early-binding (make-binding-procedure 'EARLY))
(define make-late-binding (make-binding-procedure 'LATE))
(define make-outer-binding (make-binding-procedure 'OUTER))

;;;; Compiled pattern match

(define (build-comparison pattern expression receiver)
  (define (merge-path path expression)
    (if (null? path)
	expression
	(scode/make-absolute-combination path (list expression))))

  (define (walk pattern path expression receiver)
    (cond ((not (pair? pattern))
	   (receiver true
		     (scode/make-absolute-combination 'EQ?
		      (list
		       (scode/make-constant pattern)
		       (merge-path path expression)))
		     '()))
	  ((pattern-variable? pattern)
	   (receiver false true
		     (list `(,(pattern-variable-name pattern)
			     ,@(make-evaluation
				(merge-path path expression))))))
	  (else
	   (path-step 'CAR path expression
	    (lambda (car-path car-expression)
	      (walk (car pattern) car-path car-expression
	       (lambda (car-pure? car-test car-bindings)
		 (path-step 'CDR path expression
		  (lambda (cdr-path cdr-expression)
		    (walk (cdr pattern) cdr-path cdr-expression
		     (lambda (cdr-pure? cdr-test cdr-bindings)
		       (let ((result (and car-pure? cdr-pure?)))
			 (receiver
			  result
			  (build-pair-test result car-test cdr-test
					   (merge-path path expression))
			  (append car-bindings cdr-bindings))))))))))))))

  (walk pattern '() expression
	(lambda (pure? test bindings)
	  pure?
	  (receiver test bindings))))

;;; car/cdr decomposition

(define (build-pair-test pure? car-test cdr-test expression)
  (if (not pure?)
      (scode/merge-tests (scode/make-absolute-combination 'PAIR?
							  (list expression))
			 (scode/merge-tests car-test cdr-test))
      (combination-components car-test
	(lambda (car-operator car-operands)
	  car-operator
	  (combination-components cdr-test
	    (lambda (cdr-operator cdr-operands)
	      cdr-operator
	      (scode/make-absolute-combination 'EQUAL?
	       (list
		(scode/make-constant
		 (cons (scode/constant-value (car car-operands))
		       (scode/constant-value (car cdr-operands))))
	       expression))))))))

;;;; car/cdr path compression

;; The rest of the elements are provided for canonicalization, not used.

(define path-compressions
  '((car (caar . cdar) car)
    (cdr (cadr . cddr) cdr)

    (caar (caaar . cdaar) car car)
    (cadr (caadr . cdadr) car cdr)
    (cdar (cadar . cddar) cdr car)
    (cddr (caddr . cdddr) cdr cdr)

    (caaar (caaaar . cdaaar) car caar)
    (caadr (caaadr . cdaadr) car cadr)
    (cadar (caadar . cdadar) car cdar)
    (caddr (caaddr . cdaddr) car cddr)
    (cdaar (cadaar . cddaar) cdr caar)
    (cdadr (cadadr . cddadr) cdr cadr)
    (cddar (caddar . cdddar) cdr cdar)
    (cdddr (cadddr . cddddr) cdr cddr)

    (caaaar () car caaar)
    (caaadr () car caadr)
    (caadar () car cadar)
    (caaddr () car caddr)
    (cadaar () car cdaar)
    (cadadr () car cdadr)
    (caddar () car cddar)
    (cadddr () car cdddr)
    (cdaaar () cdr caaar)
    (cdaadr () cdr caadr)
    (cdadar () cdr cadar)
    (cdaddr () cdr caddr)
    (cddaar () cdr cdaar)
    (cddadr () cdr cdadr)
    (cdddar () cdr cddar)
    (cddddr () cdr cdddr)))

(define (path-step step path expression receiver)
  (let ((info (assq path path-compressions)))
    (cond ((not info)
	   (receiver step expression))
	  ((null? (cadr info))
	   (receiver step
		     (scode/make-absolute-combination path (list expression))))
	  (else
	   (receiver (if (eq? step 'CAR) (caadr info) (cdadr info))
		     expression)))))

;;;; Transformers

(define (apply-transformer-early trans-exp name rename exp receiver)
  (let ((transformer (find-transformer trans-exp)))
    (if transformer
	(transformer trans-exp name rename exp receiver)
	(apply-transformer trans-exp name rename exp receiver))))

(define (apply-transformer transformer name rename exp receiver)
  (receiver (scode/make-variable name)
	    (transformer-bindings name rename (unevaluate exp)
	     (lambda (exp)
	       (scode/make-combination (scode/make-variable transformer)
				       (list exp))))))
      
(define (transformer-bindings name rename expression mapper)
  (if (eq? rename name)
      (list (make-outer-binding name (mapper expression)))
      (list (make-outer-binding rename expression)
	    (make-late-binding name (mapper (scode/make-variable rename))))))

(define *transformers*)

(define (find-transformer expression)
  (and (symbol? expression)
       (let ((place (assq expression *transformers*)))
	 (and place
	      (cdr place)))))

;;;; Database transformers

(define (make-database-transformer database)
  (lambda (texp name rename exp receiver)
    (let ((null-form
	   (scode/make-constant (generate-uninterned-symbol 'NOT-FOUND-))))
      (try-rules database exp null-form
       (lambda (result code)
	 (define (possible test make-binding)
	   (receiver test
		     (cons (make-binding rename code)
			   (if (eq? name rename)
			       '()
			       (list (make-binding name
						   (unevaluate exp)))))))

	 (cond ((false? result)
		(transformer-fail receiver))
	       ((eq? result 'TOO-MANY)
		(apply-transformer texp name rename exp receiver))
	       ((eq? result 'MAYBE)
		(possible (make-simple-transformer-test name null-form)
			  make-outer-binding))
	       ((can-integrate? code)
		(possible true make-early-binding))
	       (else		
		(possible true make-late-binding))))))))

;; Mega kludge!

(define (can-integrate? code)
  (if (not (scode/let? code))
      true
      (scode/let-components
       code
       (lambda (names values decls body)
	 values
	 (and (not (null? names))
	      (let ((place (assq 'INTEGRATE decls)))
		(and (not (null? place))
		     (let ((integrated (cdr place)))
		       (let loop ((left names))
			 (cond ((null? left)
				(can-integrate? body))
			       ((memq (car left) integrated)
				(loop (cdr left)))
			       (else false)))))))))))

(define-integrable (make-simple-transformer-test name tag)
  (scode/make-absolute-combination 'NOT
   (list (scode/make-absolute-combination 'EQ?
	  (list
	   (scode/make-variable name)
	   tag)))))

(define-integrable (transformer-fail receiver)
  (receiver false false))

(define-integrable (transformer-result receiver name rename out in)
  (receiver true
	    (cons (make-early-binding name (scode/make-constant out))
		  (if (eq? name rename)
		      '()
		      (list (make-early-binding rename
						(scode/make-constant in)))))))

;;;; Symbol transformers

(define (make-symbol-transformer alist)
  (lambda (texp name rename exp receiver)
    texp
    (cond ((null? alist)
	   (receiver false false))
	  ((symbol? exp)
	   (let ((pair (assq exp alist)))
	     (if (not pair)
		 (transformer-fail receiver)
		 (transformer-result receiver name rename (cdr pair) exp))))
	  ((evaluation? exp)
	   (let ((tag (generate-uninterned-symbol 'NOT-FOUND-)))
	     (receiver
	      (make-simple-transformer-test name (scode/make-constant tag))
	      (transformer-bindings name
				    rename
				    (evaluation-expression exp)
				    (lambda (expr)
				      (runtime-symbol-lookup tag
							     expr
							     alist))))))
	  (else (transformer-fail receiver)))))

(define (runtime-symbol-lookup not-found-tag expression alist)
  (if (>= (length alist) 4)
      (scode/make-absolute-combination 'CDR
       (list
	(scode/make-disjunction
	 (scode/make-absolute-combination 'ASSQ
	  (list expression
		(scode/make-constant alist)))
	 (scode/make-constant `(() . ,not-found-tag)))))
      (scode/make-case-expression
       expression
       (scode/make-constant not-found-tag)
       (map (lambda (pair)
	      (list (list (car pair))
		    (scode/make-constant (cdr pair))))
	    alist))))

;;;; Accumulation transformers

(define (make-bit-mask-transformer size alist)
  (lambda (texp name rename exp receiver)
    (cond ((null? alist)
	   (transformer-fail receiver))
	  ((evaluation? exp)
	   (apply-transformer texp name rename exp receiver))
	  (else
	   (let ((mask (make-bit-string size #!FALSE)))
	     (define (loop symbols)
	       (cond ((null? symbols)
		      (transformer-result receiver name rename mask exp))
		     ((not (pair? symbols))
		      (transformer-fail receiver))
		     ((not (symbol? (car symbols)))
		      (apply-transformer texp name rename exp receiver))
		     (else
		      (let ((place (assq (car symbols) alist)))
			(if (not place)
			    (transformer-fail receiver)
			    (begin (bit-string-set! mask (cdr place))
				   (loop (cdr symbols))))))))
	     (loop exp))))))

;;;; Scode utilities

(define-integrable scode/make-binding cons)
(define-integrable scode/binding-variable car)
(define-integrable scode/binding-value cdr)

(define-integrable (scode/make-conjunction t1 t2)
  (scode/make-conditional t1 t2 (scode/make-constant false)))

(define (scode/merge-tests t1 t2)
  (cond ((eq? t1 true) t2)
	((eq? t2 true) t1)
	(else (scode/make-conjunction t1 t2))))

(define (scode/make-thunk body)
  (scode/make-lambda lambda-tag:unnamed '() '() false '() '() body))  

(define (scode/let? obj)
  (and (scode/combination? obj)
       (scode/combination-components
	obj
	(lambda (operator operands)
	  operands
	  (and (scode/lambda? operator)
	       (scode/lambda-components
		operator
		(lambda (name . ignore)
		  ignore
		  (eq? name lambda-tag:let))))))))

(define (scode/make-let names values declarations body)
  (scode/make-combination
   (scode/make-lambda lambda-tag:let
		      names
		      '()
		      false
		      '()
		      declarations
		      body)
   values))

(define (scode/let-components lcomb receiver)
  (scode/combination-components lcomb
   (lambda (operator values)
     (scode/lambda-components operator
      (lambda (tag names opt rest aux decls body)
	tag opt rest aux
	(receiver names values decls body))))))				     

;;;; Scode utilities (continued)

(define (scode/make-block bindings integrated body)
  (if (null? bindings)
      body
      (scode/make-let (map scode/binding-variable bindings)
		      (map scode/binding-value bindings)
		      (if (null? integrated)
			  '()
			  `((INTEGRATE ,@integrated)))
		      body)))

(define (scode/make-letrec bindings body)
  (scode/make-let
   (map scode/binding-variable bindings)
   (make-list (length bindings)
	      (make-unassigned-reference-trap))
   '()
   (scode/make-sequence
    (map* body
	  (lambda (binding)
	    (scode/make-assignment (scode/binding-variable binding)
				   (scode/binding-value binding)))
	  bindings))))

(define (scode/make-case-expression expression default clauses)
  (define (kernel case-selector)
    (define (process clauses)
      (if (null? clauses)
	  default
	  (let ((selector (caar clauses)))
	    (scode/make-conditional
	     (if (null? (cdr selector))
		 (scode/make-absolute-combination 'EQ?
		  (list case-selector
			(scode/make-constant (car selector))))
		 (scode/make-absolute-combination 'MEMQ
		  (list case-selector
			(scode/make-constant selector))))
	     (cadar clauses)
	     (process (cdr clauses))))))
    (process clauses))

  (if (scode/variable? expression)
      (kernel expression)
      (let ((var (make-variable-name 'CASE-SELECTOR-)))
	(scode/make-let (list var) (list expression) '()
			(kernel (scode/make-variable var))))))

(define make-variable-name generate-uninterned-symbol)

(define evaluation-tag (list '*EVALUATION*))

(define (evaluation? exp)
  (and (pair? exp)
       (eq? (car exp) evaluation-tag)))

(define-integrable (make-evaluation name)
  (cons evaluation-tag name))

(define-integrable (evaluation-expression exp)
  (cdr exp))