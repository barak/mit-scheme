#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/pmpars.scm,v 1.1 1987/04/17 08:02:27 cph Exp $

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

;;; PARSE-RULE, given a PATTERN and a BODY, returns: (1) a
;;; pattern for use with the matcher; (2) the variables in the
;;; pattern, in the order that the matcher will produce their
;;; corresponding values; (3) a transformer expression; (4) a
;;; qualifier expression; and (5) a list of actions which should be
;;; executed sequentially when the rule fires.

;;; RULE-RESULT-EXPRESSION is used to generate a lambda expression
;;; which, when passed the values resulting from the match as its
;;; arguments, will return either false, indicating that the
;;; qualifications failed, or the result of the body.  The meanings of
;;; the transformer and qualifier are made explicit here.

(define (rule-result-expression names transformer qualifier body)
  (let ((result
	 (let ((body `(LAMBDA () ,body)))
	   `(LAMBDA ,names
	      ,(if (eq? qualifier true)
		   body
		   `(AND ,qualifier ,body))))))
    (if (not transformer)
	result
	`(LAMBDA ,names
	   (,transformer ,result ,@names)))))

(define parse-rule)
(let ()

(set! parse-rule
(named-lambda (parse-rule pattern body receiver)
  (extract-variables pattern
    (lambda (pattern variables)
      (extract-qualifier body
	(lambda (qualifiers actions)
	  (let ((names (pattern-variables pattern)))
	    (receiver pattern
		      names
		      (make-transformer (reorder-variables variables names))
		      (if (null? qualifiers)
			  true
			  `(AND ,@qualifiers))
		      actions))))))))

(define (extract-variables pattern receiver)
  (if (pair? pattern)
      (if (memq (car pattern) '(? ?@))
	  (receiver (make-pattern-variable (cadr pattern))
		    (list (cons (cadr pattern)
				(if (null? (cddr pattern))
				    '()
				    (list (cons (car pattern)
						(caddr pattern)))))))
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

(define (make-transformer variables)
  (generate-qualifiers&renames variables
    (lambda (renames rename-bindings qualification-expressions)
      ;; Note this assumes that `(null? rename-bindings)' implies
      ;; `(null? qualification-expressions)'.
      (if (null? rename-bindings)
	  false				;i.e. no transformation needed.
	  `(LAMBDA (RECEIVER ,@renames)
	     (LET ,rename-bindings
	       (AND ,@qualification-expressions
		    (RECEIVER ,@renames))))))))

(define (generate-qualifiers&renames variables receiver)
  (if (null? variables)
      (receiver '() '() '())
      (generate-qualifiers&renames (cdr variables)
	(lambda (renames rename-bindings qualification-expressions)
	  (let ((variable (cdar variables))
		(rename (generate-uninterned-symbol)))
	    (cond ((null? variable)
		   (receiver `(,rename ,@renames)
			     rename-bindings
			     qualification-expressions))
		  ((not (null? (cdr variable)))
		   (error "Multiple per-variable qualifiers" variable))
		  ((eq? (caar variable) '?)
		   (receiver `(,rename ,@renames)
			     `((,rename (,(cdar variable) ,rename))
			       ,@rename-bindings)
			     `(,rename ,@qualification-expressions)))
		  ((eq? (caar variable) '?@)
		   (receiver `(,rename ,@renames)
			     `((,rename (MAP ,(cdar variable) ,rename))
			       ,@rename-bindings)
			     `((ALL-TRUE? ,rename)
			       ,@qualification-expressions)))
		  (else
		   (error "Unknown qualifier type" variable))))))))

;;; end PARSE-RULE environment.
)