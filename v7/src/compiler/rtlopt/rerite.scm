#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rerite.scm,v 1.1 1990/01/18 22:49:26 cph Rel $

Copyright (c) 1990 Massachusetts Institute of Technology

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

;;;; RTL Rewriting

(declare (usual-integrations))

(define-structure (rewriting-rules
		   (conc-name rewriting-rules/)
		   (constructor make-rewriting-rules ()))
  (assignment '())
  (statement '())
  (register '())
  (expression '())
  (generic '()))

(define rules:pre-cse (make-rewriting-rules))
(define rules:post-cse (make-rewriting-rules))

(define (rtl-rewriting:pre-cse rgraphs)
  (walk-rgraphs rules:pre-cse rgraphs))

(define (rtl-rewriting:post-cse rgraphs)
  (walk-rgraphs rules:post-cse rgraphs))

(define (add-rewriting-rule! pattern result-procedure)
  (new-rewriting-rule! rules:post-cse pattern result-procedure))

(define (walk-rgraphs rules rgraphs)
  (if (not (and (null? (rewriting-rules/assignment rules))
		(null? (rewriting-rules/statement rules))
		(null? (rewriting-rules/register rules))
		(null? (rewriting-rules/expression rules))
		(null? (rewriting-rules/generic rules))))
      (for-each (lambda (rgraph)
		  (walk-rgraph rules rgraph))
		rgraphs)))

(define (walk-rgraph rules rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (for-each (lambda (bblock) (walk-bblock rules bblock))
	      (rgraph-bblocks rgraph))))

(define (walk-bblock rules bblock)
  (bblock-walk-forward bblock
    (lambda (rinst)
      (walk-rinst rules rinst))))

(define (walk-rinst rules rinst)
  (let ((rtl (rinst-rtl rinst)))
    ;; Typically there will be few rules, and few instructions that
    ;; match, so it is worth checking before rewriting anything.
    (if (or (match-rtl-statement rules rtl)
	    (rtl:any-subexpression? rtl
	      (letrec ((loop
			(lambda (expression)
			  (or (match-rtl-expression rules expression)
			      (rtl:any-subexpression? expression loop)))))
		loop)))
	(set-rinst-rtl!
	 rinst
	 (let loop
	     ((rtl
	       (rtl:map-subexpressions rtl
		 (letrec ((loop
			   (lambda (expression)
			     (let ((match-result
				    (match-rtl-expression rules expression)))
			       (if match-result
				   (loop (match-result))
				   expression)))))
		   loop))))
	   (let ((match-result (match-rtl-statement rules rtl)))
	     (if match-result
		 (loop (match-result))
		 rtl)))))))

(define (match-rtl-statement rules rtl)
  (or (if (rtl:assign? rtl)
	  (pattern-lookup (rewriting-rules/assignment rules) rtl)
	  (let ((entries
		 (assq (rtl:expression-type rtl)
		       (rewriting-rules/statement rules))))
	    (and entries
		 (pattern-lookup (cdr entries) rtl))))
      (pattern-lookup (rewriting-rules/generic rules) rtl)))

(define (match-rtl-expression rules expression)
  (or (if (rtl:register? expression)
	  (pattern-lookup (rewriting-rules/register rules) expression)
	  (let ((entries
		 (assq (rtl:expression-type expression)
		       (rewriting-rules/expression rules))))
	    (and entries
		 (pattern-lookup (cdr entries) expression))))
      (pattern-lookup (rewriting-rules/generic rules) expression)))

(define (new-rewriting-rule! rules pattern result-procedure)
  (let ((entry (cons pattern result-procedure)))
    (if (not (and (pair? pattern) (symbol? (car pattern))))
	(set-rewriting-rules/generic! rules
				      (cons entry
					    (rewriting-rules/generic rules)))
	(let ((keyword (car pattern)))
	  (cond ((eq? keyword 'ASSIGN)
		 (set-rewriting-rules/assignment!
		  rules
		  (cons entry (rewriting-rules/assignment rules))))
		((eq? keyword 'REGISTER)
		 (set-rewriting-rules/register!
		  rules
		  (cons entry (rewriting-rules/register rules))))
		((memq keyword rtl:expression-types)
		 (let ((entries
			(assq keyword (rewriting-rules/expression rules))))
		   (if entries
		       (set-cdr! entries (cons entry (cdr entries)))
		       (set-rewriting-rules/expression!
			rules
			(cons (list keyword entry)
			      (rewriting-rules/expression rules))))))
		((or (memq keyword rtl:statement-types)
		     (memq keyword rtl:predicate-types))
		 (let ((entries
			(assq keyword (rewriting-rules/statement rules))))
		   (if entries
		       (set-cdr! entries (cons entry (cdr entries)))
		       (set-rewriting-rules/statement!
			rules
			(cons (list keyword entry)
			      (rewriting-rules/statement rules))))))
		(else
		 (error "illegal RTL type" keyword))))))
  pattern)

(define-rule
  (lambda (pattern result-procedure)
    (new-rewriting-rule! rules:pre-cse pattern result-procedure))
  (OBJECT->ADDRESS (? source))
  (QUALIFIER (value-class=address? (rtl:expression-value-class source)))
  source)