#| -*-Scheme-*-

$Id: pmpars.scm,v 1.4 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1988, 1999 Massachusetts Institute of Technology

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

(define (parse-rule pattern body receiver)
  (extract-variables
   pattern
   (lambda (pattern variables)
     (extract-qualifier
      body
      (lambda (qualifiers actions)
	(let ((names (pattern-variables pattern)))
	  (receiver pattern
		    (reorder-variables variables names)
		    qualifiers
		    actions)))))))

(define (extract-variables pattern receiver)
  (if (pair? pattern)
      (if (memq (car pattern) '(? ?@))
	  (receiver (make-pattern-variable (cadr pattern))
		    (list (cons (cadr pattern)
				(if (null? (cddr pattern))
				    '()
				    (list (cons (car pattern)
						(cddr pattern)))))))
	  (extract-variables (car pattern)
	    (lambda (car-pattern car-variables)
	      (extract-variables (cdr pattern)
		(lambda (cdr-pattern cdr-variables)
		  (receiver (cons car-pattern cdr-pattern)
			    (merge-variables-lists car-variables
						   cdr-variables)))))))
      (receiver pattern '())))

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

(define (extract-qualifier body receiver)
  (if (and (pair? (car body))
	   (eq? (caar body) 'QUALIFIER))
      (receiver (cdar body) (cdr body))
      (receiver '() body)))

(define (reorder-variables variables names)
  (map (lambda (name) (assq name variables))
       names))

(define (rule-result-expression variables qualifiers body)
  (let ((body `(lambda () ,body)))
    (process-transformations variables
      (lambda (outer-vars inner-vars xforms xqualifiers)
	(if (null? inner-vars)
	    `(lambda ,outer-vars
	       ,(if (null? qualifiers)
		    body
		    `(and ,@qualifiers ,body)))
	    `(lambda ,outer-vars
	       (let ,(map list inner-vars xforms)
		 (and ,@xqualifiers
		      ,@qualifiers
		      ,body))))))))

(define (process-transformations variables receiver)
  (if (null? variables)
      (receiver '() '() '() '())
      (process-transformations (cdr variables)
	(lambda (outer inner xform qual)
	  (let ((name (caar variables))
		(variable (cdar variables)))
	    (cond ((null? variable)
		   (receiver (cons name outer)
			     inner
			     xform
			     qual))
		  ((not (null? (cdr variable)))
		   (error "process-trasformations: Multiple qualifiers"
			  (car variables)))
		  (else
		   (let ((var (car variable)))
		     (define (handle-xform rename)
		       (if (eq? (car var) '?)
			   (receiver (cons rename outer)
				     (cons name inner)
				     (cons `(,(cadr var) ,rename)
					   xform)
				     (cons name qual))
			   (receiver (cons rename outer)
				     (cons name inner)
				     (cons `(MAP ,(cadr var) ,rename)
					   xform)
				     (cons `(APPLY BOOLEAN/AND ,name) qual))))
		     (handle-xform
		      (if (null? (cddr var))
			  name
			  (caddr var)))))))))))