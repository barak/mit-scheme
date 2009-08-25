#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Very Simple Pattern Matcher: Parser

(declare (usual-integrations))

;;; PARSE-RULE and RULE-RESULT-EXPRESSION are used together to parse
;;; pattern/body definitions, producing Scheme code which can then be
;;; compiled.

;;; PARSE-RULE, given a PATTERN and a BODY, returns: (1) a pattern for
;;; use with the matcher; (2) the variables in the pattern, in the
;;; order that the matcher will produce their corresponding values;
;;; (3) a list of qualifier expressions; and (4) a list of actions
;;; which should be executed sequentially when the rule fires.

;;; RULE-RESULT-EXPRESSION is used to generate a lambda expression
;;; which, when passed the values resulting from the match as its
;;; arguments, will return either false, indicating that the
;;; qualifications failed, or the result of the body.

(define (parse-rule pattern body)
  (receive (pattern variables) (extract-variables pattern)
    (receive (qualifiers actions) (extract-qualifiers body)
      (let ((names (pattern-variables pattern)))
	(values pattern
		(reorder-variables variables names)
		qualifiers
		actions)))))

(define (extract-variables pattern)
  (if (pair? pattern)
      (if (memq (car pattern) '(? ?@))
	  (values (make-pattern-variable (cadr pattern))
		  (list (cons (cadr pattern)
			      (if (null? (cddr pattern))
				  '()
				  (list (cons (car pattern)
					      (cddr pattern)))))))
	  (receive (car-pattern car-variables)
	      (extract-variables (car pattern))
	    (receive (cdr-pattern cdr-variables)
		(extract-variables (cdr pattern))
	      (values (cons car-pattern cdr-pattern)
		      (merge-variables-lists car-variables
					     cdr-variables)))))
      (values pattern '())))

(define (merge-variables-lists x y)
  (cond ((null? x) y)
	((null? y) x)
	(else
	 (let ((entry (assq (caar x) y)))
	   (if entry
	       (cons (append! (car x) (cdr entry))
		     (merge-variables-lists (cdr x)
					    (delq! entry y)))
	       (cons (car x)
		     (merge-variables-lists (cdr x)
					    y)))))))

(define (extract-qualifiers body)
  (if (and (pair? (car body))
	   (eq? (caar body) 'QUALIFIER))
      (values (cdar body) (cdr body))
      (values '() body)))

(define (reorder-variables variables names)
  (map (lambda (name) (assq name variables))
       names))

(define (rule->matcher pattern body environment)
  (receive (pattern variables qualifiers actions) (parse-rule pattern body)
    (values pattern
	    (make-rule-matcher pattern
			       (rule-result-expression variables
						       qualifiers
						       `(,(close-syntax
							   'BEGIN
							   environment)
							 ,@actions)
						       environment)
			       environment))))

(define (make-rule-matcher pattern expression environment)
  (let ((r-lambda (close-syntax 'LAMBDA environment))
	(instance (close-syntax 'INSTANCE environment))
	(r-pl1 (close-syntax 'PATTERN-LOOKUP-1 environment)))
    `(,r-lambda (,instance)
       (,r-pl1 ',pattern
	       ,expression
	       ,instance))))

(define (rule-result-expression variables qualifiers body environment)
  (receive (outer-vars inner-vars xforms xqualifiers)
      (process-transformations variables environment)
    (let ((r-lambda (close-syntax 'LAMBDA environment))
	  (r-let (close-syntax 'LET environment))
	  (r-and (close-syntax 'AND environment)))
      `(,r-lambda ,outer-vars
		  (,r-let ,(map list inner-vars xforms)
			  (,r-and ,@xqualifiers
				  ,@qualifiers
				  (,r-lambda () ,body)))))))

(define (process-transformations variables environment)
  (let ((r-map (close-syntax 'MAP environment))
	(r-apply (close-syntax 'APPLY environment))
	(r-boolean/and (close-syntax 'BOOLEAN/AND environment)))
    (let loop ((variables variables))
      (if (pair? variables)
	  (receive (outer-vars inner-vars xforms qualifiers)
	      (loop (cdr variables))
	    (let ((name (caar variables))
		  (variable (cdar variables)))
	      (if (pair? variable)
		  (let ((var (car variable)))
		    (if (not (null? (cdr variable)))
			(error "Multiple variable qualifiers:"
			       (car variables)))
		    (let ((xform (cadr var))
			  (outer-var
			   (if (pair? (cddr var))
			       (caddr var)
			       name)))
		      (if (eq? (car var) '?)
			  (values (cons outer-var outer-vars)
				  (cons name inner-vars)
				  (cons `(,xform ,outer-var) xforms)
				  (cons name qualifiers))
			  (values (cons outer-var outer-vars)
				  (cons name inner-vars)
				  (cons `(,r-map ,xform ,outer-var) xforms)
				  (cons `(,r-apply ,r-boolean/and ,name)
					qualifiers)))))
		  (values (cons name outer-vars)
			  inner-vars
			  xforms
			  qualifiers))))
	  (values '() '() '() '())))))