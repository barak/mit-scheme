#| -*-Scheme-*-

$Id: pmpars.scm,v 1.6 2002/02/12 00:29:16 cph Exp $

Copyright (c) 1988, 1999, 2002 Massachusetts Institute of Technology

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
  (call-with-values (lambda () (extract-variables pattern))
    (lambda (pattern variables)
      (call-with-values (lambda () (extract-qualifiers body))
	(lambda (qualifiers actions)
	  (let ((names (pattern-variables pattern)))
	    (values pattern
		    (reorder-variables variables names)
		    qualifiers
		    actions)))))))

(define (extract-variables pattern)
  (if (pair? pattern)
      (if (memq (car pattern) '(? ?@))
	  (values (make-pattern-variable (cadr pattern))
		  (list (cons (cadr pattern)
			      (if (null? (cddr pattern))
				  '()
				  (list (cons (car pattern)
					      (cddr pattern)))))))
	  (call-with-values (lambda () (extract-variables (car pattern)))
	    (lambda (car-pattern car-variables)
	      (call-with-values (lambda () (extract-variables (cdr pattern)))
		(lambda (cdr-pattern cdr-variables)
		  (values (cons car-pattern cdr-pattern)
			  (merge-variables-lists car-variables
						 cdr-variables)))))))
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

(define (rule-result-expression variables qualifiers body environment)
  (reverse-syntactic-environments environment
    (lambda (environment)
      (call-with-values
	  (lambda () (process-transformations variables environment))
	(lambda (outer-vars inner-vars xforms xqualifiers)
	  (let ((r-lambda (close-syntax 'LAMBDA environment))
		(r-let (close-syntax 'LET environment))
		(r-and (close-syntax 'AND environment)))
	    `(,r-lambda ,outer-vars
			(,r-let ,(map list inner-vars xforms)
				(,r-and ,@xqualifiers
					,@qualifiers
					(,r-lambda () ,body))))))))))

(define (process-transformations variables environment)
  (let ((r-map (close-syntax 'MAP environment))
	(r-apply (close-syntax 'APPLY environment))
	(r-boolean/and (close-syntax 'BOOLEAN/AND environment)))
    (let loop ((variables variables))
      (if (pair? variables)
	  (call-with-values (lambda () (loop (cdr variables)))
	    (lambda (outer-vars inner-vars xforms qualifiers)
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
			    qualifiers)))))
	  (values '() '() '() '())))))