#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/pmpars.scm,v 1.2 1987/07/08 21:53:25 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(define rule-result-expression)
(define parse-rule)

(let ()

(set! parse-rule
      (named-lambda (parse-rule pattern body receiver)
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
			  actions))))))))

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

(set! rule-result-expression
      (named-lambda (rule-result-expression variables qualifiers body)
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
			   ,body)))))))))

(define (process-transformations variables receiver)
  (if (null? variables)
      (receiver '() '() '() '())
      (process-transformations
       (cdr variables)
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
				    (cons `(ALL-TRUE? ,name) qual))))
		    (handle-xform
		     (if (null? (cddr var))
			 name
			 (caddr var)))))))))))

;; End of PARSE-RULE environment.
)