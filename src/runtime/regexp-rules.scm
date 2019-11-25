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

(define (pattern-rule pattern operation #!optional guard-pred)
  (let ((predicate (pattern->predicate pattern 'pattern-rule))
	(guard-pred (if (default-object? guard-pred) #f guard-pred)))
    (receive (wrapper arity)
	(pattern-calling-convention pattern 'pattern-rule)
      (guarantee-procedure-of-arity operation arity 'pattern-rule)
      (if guard-pred
	  (guarantee-procedure-of-arity guard-pred arity 'pattern-rule))
      (general-rule pattern
		    (if guard-pred
			(let ((wrapped (wrapper guard-pred)))
			  (lambda (object)
			    (and (predicate object)
				 (wrapped object))))
			predicate)
		    (wrapper operation)))))

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

(define (pattern-calling-convention pattern caller)
  (cond ((pair? pattern)
	 (if (pattern-constant? (car pattern))
	     (values (lambda (procedure)
		       (lambda (object)
			 (apply procedure (cdr object))))
		     (pattern-arity (cdr pattern)))
	     (values (lambda (procedure)
		       (lambda (object)
			 (apply procedure object)))
		     (pattern-arity pattern))))
	((pattern-constant? pattern)
	 (values (lambda (procedure)
		   (lambda (object)
		     (declare (ignore object))
		     (procedure)))
		 (make-procedure-arity 0)))
	((unary-procedure? pattern)
	 (values (lambda (procedure)
		   procedure)
		 (make-procedure-arity 1)))
	(else
	 (error:not-a pattern? pattern caller))))

(define (pattern-arity pattern)
  (let loop ((pattern pattern) (n 0))
    (cond ((pair? pattern) (loop (cdr pattern) (+ n 1)))
	  ((null? pattern) (make-procedure-arity n))
	  (else (make-procedure-arity n #f)))))

(define-record-type <rules>
    (%make-rules name adder matcher getter)
    rules?
  (name rules-name)
  (adder rules-adder)
  (matcher rules-matcher)
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

(define (make-rules name)
  (let ((rules '()))

    (define (add! rule)
      (set! rules
	    (cons rule
		  (remove! (lambda (rule*)
			     (equal? (rule-key rule)
				     (rule-key rule*)))
			   rules)))
      unspecific)

    (define (match object)
      (let ((matched
	     (filter (lambda (rule)
		       ((rule-predicate rule) object))
		     rules)))
	(and (pair? matched)
	     (begin
	       (if (pair? (cdr matched))
		   (error "Multiple rule matches:" matched object))
	       (car matched)))))

    (define (get)
      (list-copy rules))

    (%make-rules name add! match get)))

(define (rules-rewriter rules)
  (let ((match (rules-matcher rules)))
    (define (rewrite object)
      (let ((rule (match object)))
	(if rule
	    (rewrite ((rule-operation rule) object))
	    object)))
    rewrite))

(define (rules-definer rules)
  (let ((adder (rules-adder rules)))
    (lambda (pattern operation #!optional predicate)
      (adder
       (if (pattern? pattern)
	   (pattern-rule pattern operation predicate)
	   (general-rule pattern predicate operation))))))

(add-boot-init! (lambda () (run-deferred-boot-actions 'regexp-rules)))