#| -*- Mode: Scheme; keyword-style: none -*-

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

;;;; Rules
;;; package: (runtime regexp rules)

;;; A simple rule system.  Supports list-structured patterns, suitable for
;;; parsing syntax, as well as completely general rules.

(declare (usual-integrations))

(define-record-type <rule>
    (general-rule key predicate operation)
    rule?
  (key rule-key)
  (predicate rule-predicate)
  (operation rule-operation))

(define (pattern? object)
  (if (pair? object)
      (and (pattern? (car object))
	   (let loop ((object (cdr object)))
	     (if (pair? object)
		 (and (pattern? (car object))
		      (loop (cdr object)))
		 (or (null? object)
		     (unary-procedure? object)))))
      (or (pattern-constant? object)
	  (unary-procedure? object))))
(register-predicate! pattern? 'pattern)

(define (pattern-constant? object)
  (or (interned-symbol? object)
      (number? object)
      (char? object)
      (boolean? object)))

(define (pattern-rule extra-args pattern operation #!optional guard-pred)
  (guarantee exact-nonnegative-integer? extra-args 'pattern-rule)
  (let ((pattern-pred (pattern->predicate pattern 'pattern-rule))
	(guard-pred (if (default-object? guard-pred) #f guard-pred)))
    (receive (wrapper arity)
	(pattern-calling-convention pattern extra-args 'pattern-rule)
      (guarantee-procedure-of-arity operation arity 'pattern-rule)
      (if guard-pred
	  (guarantee-procedure-of-arity guard-pred arity 'pattern-rule))
      (general-rule pattern
		    (if guard-pred
			(join-preds extra-args
				    pattern-pred
				    (wrapper guard-pred))
			(wrap-pattern-pred extra-args pattern-pred))
		    (wrapper operation)))))

(define (join-preds extra-args pattern-pred guard-pred)
  (case extra-args
    ((0)
     (lambda (object)
       (and (pattern-pred object)
	    (guard-pred object))))
    ((1)
     (lambda (arg object)
       (and (pattern-pred object)
	    (guard-pred arg object))))
    (else
     (error "Unsupported extra-args:" extra-args))))

(define (wrap-pattern-pred extra-args predicate)
  (case extra-args
    ((0) predicate)
    ((1) (lambda (arg object) (declare (ignore arg)) (predicate object)))
    (else (error "Unsupported extra-args:" extra-args))))

(define (pattern->predicate pattern caller)
  (cond ((or (pair? pattern) (null? pattern))
	 (list-predicate pattern caller))
	((or (interned-symbol? pattern)
	     (fixnum? pattern)
	     (char? pattern)
	     (boolean? pattern))
	 (lambda (object) (eq? pattern object)))
	((number? pattern)
	 (lambda (object) (eqv? pattern object)))
	((unary-procedure? pattern)
	 pattern)
	(else
	 (error:not-a pattern? pattern caller))))

(define (list-predicate pattern caller)
  (let ((preds (parse-list-pattern pattern caller)))
    (lambda (object)
      (let loop ((preds preds) (object object))
	(if (pair? preds)
	    (and (pair? object)
		 ((car preds) (car object))
		 (loop (cdr preds) (cdr object)))
	    (preds object))))))

(define (parse-list-pattern pattern caller)
  (let loop ((pattern pattern))
    (if (pair? pattern)
	(cons (pattern->predicate (car pattern) caller)
	      (loop (cdr pattern)))
	(cond ((null? pattern) null?)
	      ((unary-procedure? pattern) (tail-predicate pattern))
	      (else (error:not-a pattern? pattern caller))))))

(define (tail-predicate pred)
  (define (predicate object)
    (if (pair? object)
	(and (pred (car object))
	     (predicate (cdr object)))
	(null? object)))
  predicate)

(define (pattern-calling-convention pattern extra-args caller)
  (cond ((pair? pattern)
	 (if (pattern-constant? (car pattern))
	     (values (pair-wrapper extra-args cdr)
		     (pattern-arity (cdr pattern) extra-args))
	     (values (pair-wrapper extra-args (lambda (x) x))
		     (pattern-arity pattern extra-args))))
	((pattern-constant? pattern)
	 (values (constant-wrapper extra-args)
		 (make-procedure-arity extra-args)))
	((procedure-of-arity? (+ 1 extra-args) pattern)
	 (values (lambda (procedure) procedure)
		 (make-procedure-arity (+ 1 extra-args))))
	(else
	 (error:not-a pattern? pattern caller))))

(define (pair-wrapper extra-args proc)
  (case extra-args
    ((0)
     (lambda (procedure)
       (lambda (object) (apply procedure (proc object)))))
    ((1)
     (lambda (procedure)
       (lambda (arg object) (apply procedure arg (proc object)))))
    (else
     (error "Unsupported extra-args:" extra-args))))

(define (constant-wrapper extra-args)
  (case extra-args
    ((0)
     (lambda (procedure)
       (lambda (object) (declare (ignore object)) (procedure))))
    ((1)
     (lambda (procedure)
       (lambda (arg object) (declare (ignore object)) (procedure arg))))
    (else
     (error "Unsupported extra-args:" extra-args))))

(define (pattern-arity pattern n)
  (cond ((pair? pattern) (pattern-arity (cdr pattern) (+ n 1)))
	((null? pattern) (make-procedure-arity n))
	(else (make-procedure-arity n #f))))

(define-record-type <rules>
    (%make-rules name extra-args adder getter)
    rules?
  (name rules-name)
  (extra-args rules-extra-args)
  (adder rules-adder)
  (getter rules-getter))

(define-print-method rules?
  (standard-print-method 'rules
    (lambda (rules)
      (list (rules-name rules)))))

(define-pp-describer rules?
  (lambda (rules)
    (let ((elts ((rules-getter rules))))
      (map list
	   (iota (length elts))
	   elts))))

(define (make-rules name #!optional extra-args)
  (let ((extra-args (if (default-object? extra-args) 0 extra-args))
	(rules '()))

    (define (add! rule)
      (set! rules
	    (cons rule
		  (remove! (lambda (rule*)
			     (equal? (rule-key rule)
				     (rule-key rule*)))
			   rules)))
      rule)

    (%make-rules name extra-args add! (lambda () rules))))

(define (rules-definer rules)
  (let ((adder (rules-adder rules))
	(extra-args (rules-extra-args rules)))
    (lambda (pattern operation #!optional predicate)
      (adder
       (if (pattern? pattern)
	   (pattern-rule extra-args pattern operation predicate)
	   (general-rule pattern predicate operation))))))

(define (rules-matcher rules)
  (let ((match
	 (let ((getter (rules-getter rules)))
	   (lambda (predicate)
	     (let ((matched (filter predicate (getter))))
	       (and (pair? matched)
		    (begin
		      (if (pair? (cdr matched))
			  (error "Multiple rule matches:" matched))
		      (car matched))))))))
    (case (rules-extra-args rules)
      ((0)
       (lambda (object)
	 (match (lambda (rule) ((rule-predicate rule) object)))))
      ((1)
       (lambda (arg object)
	 (match (lambda (rule) ((rule-predicate rule) arg object)))))
      (else
       (error "Unsupported extra-args:" (rules-extra-args rules))))))

(define (rules-rewriter rules #!optional k)
  (let ((matcher (rules-matcher rules)))
    (case (rules-extra-args rules)
      ((0)
       (letrec
	   ((rewrite
	     (let ((k (if (default-object? k) (lambda (object) object) k)))
	       (lambda (object)
		 (let ((rule (matcher object)))
		   (if rule
		       (rewrite ((rule-operation rule) object))
		       (k object)))))))
	 rewrite))
      ((1)
       (letrec
	   ((rewrite
	     (let ((k
		    (if (default-object? k)
			(lambda (arg object) (declare (ignore arg)) object)
			k)))
	       (lambda (arg object)
		 (let ((rule (matcher arg object)))
		   (if rule
		       (rewrite arg ((rule-operation rule) arg object))
		       (k arg object)))))))
	 rewrite))
      (else
       (error "Unsupported extra-args:" (rules-extra-args rules))))))

(add-boot-init! (lambda () (run-deferred-boot-actions 'regexp-rules)))