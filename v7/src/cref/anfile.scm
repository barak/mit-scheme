#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/anfile.scm,v 1.2 1989/08/03 23:25:35 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Free/Bound Variable Analysis

(declare (usual-integrations))

(define (analyze/directory filename)
  (for-each
   (lambda (input-pathname)
     (let ((output-pathname (pathname-new-type input-pathname "free")))
       (if (not (compare-file-modification-times input-pathname
						 output-pathname))
	   (analyze/file input-pathname output-pathname))))
   (directory-read
    (merge-pathnames (pathname-as-directory (->pathname filename))
		     (string->pathname "*.bin")))))

(define (read-analyzed-file input-pathname)
  (let ((output-pathname (pathname-new-type input-pathname "free")))
    (if (compare-file-modification-times input-pathname output-pathname)
	(fasload output-pathname)
	(analyze/file input-pathname output-pathname))))

(define (analyze/file input-pathname output-pathname)
  (let ((analyzed-file (analyze/top-level (fasload input-pathname))))
    (if analyze/file/memoize?
	(fasdump analyzed-file output-pathname))
    analyzed-file))

(define analyze/file/memoize? false)
(define (compare-file-modification-times x y)
  (let ((x (file-modification-time x)))
    (and x
	 (let ((y (file-modification-time y)))
	   (and y
		(< x y))))))

(define (analyze/top-level expression)
  (with-values (lambda () (sort-expressions (process-top-level expression)))
    (lambda (definitions others)
      (let ((definition-analysis
	      (map analyze/top-level/definition definitions)))
	(if (not (null? others))
	    (cons (with-values (lambda ()
				 (analyze/expression (make-sequence others)))
		    (lambda (references assignments executions)
		      (vector false references assignments executions
			      'EXPRESSION)))
		  definition-analysis)
	    definition-analysis)))))

(define (sort-expressions expressions)
  (if (null? expressions)
      (values '() '())
      (let ((rest (lambda () (sort-expressions (cdr expressions)))))
	(if (block-declaration? (car expressions))
	    (rest)
	    (with-values rest
	      (lambda (definitions others)
		(if (definition? (car expressions))
		    (values (cons (car expressions) definitions) others)
		    (values definitions (cons (car expressions) others)))))))))

(define (process-top-level expression)
  (cond ((comment? expression)
	 (process-top-level (comment-expression expression)))
	((sequence? expression)
	 (mapcan process-top-level (sequence-actions expression)))
	(else
	 (list expression))))

(define (analyze/top-level/definition definition)
  (let ((name (definition-name definition))
	(expression (definition-value definition)))
    (cond ((unassigned-reference-trap? expression)
	   (vector name '() '() '() 'UNASSIGNED))
	  ((scode-constant? expression)
	   (vector name '() '() '() 'CONSTANT))
	  (else
	   (with-values (lambda () (analyze/expression expression))
	     (lambda (references assignments executions)
	       (vector name references assignments executions
		       (cond ((lambda? expression) 'LAMBDA)
			     ((delay? expression) 'DELAY)
			     (else 'EXPRESSION)))))))))

(define (analyze/expression expression)
  ((scode-walk analyze/dispatch expression) expression))

(define (analyze/expressions expressions)
  (if (null? expressions)
      (values '() '() '())
      (result-sum (lambda () (analyze/expression (car expressions)))
		  (lambda () (analyze/expressions (cdr expressions))))))

(define (analyze/uninteresting expression)
  (values (if (primitive-procedure? expression) (list expression) '())
	  '() '()))

(define (analyze/error expression)
  (error "Illegal expression" expression))

(define (analyze/access expression)
  (if (access-environment expression)
      (warn "Access to non-global environment:" (unsyntax expression)))
  (values (list expression) '() '()))

(define (analyze/variable expression)
  (values (list (variable-name expression)) '() '()))

(define (analyze/assignment expression)
  (with-values (lambda () (analyze/expression (assignment-value expression)))
    (lambda (references assignments executions)
      (values references
	      (cons (assignment-name expression) assignments)
	      executions))))

(define (analyze/combination expression)
  (result-sum (lambda ()
		(let ((operator (combination-operator expression)))
		  (cond ((variable? operator)
			 (values '() '() (list (variable-name operator))))
			((or (primitive-procedure? operator)
			     (and (access? operator)
				  (not (access-environment operator))))
			 (values '() '() (list operator)))
			(else
			 (analyze/expression operator)))))
	      (lambda ()
		(analyze/expressions (combination-operands expression)))))

(define (analyze/lambda expression)
  (lambda-components expression
    (lambda (name required optional rest auxiliary declarations body)
      name declarations
      (with-values (lambda () (analyze/expression body))
	(lambda (references assignments executions)
	  (let ((bound
		 (append required
			 optional
			 (if rest (list rest) '())
			 auxiliary)))
	    (values (multiset-difference references bound)
		    (multiset-difference assignments bound)
		    (multiset-difference executions bound))))))))

(define (analyze/error-combination expression)
  (combination-components expression
    (lambda (operator operands)
      (analyze/expressions (list operator (car operands) (cadr operands))))))

(define (analyze/delay expression)
  (analyze/expression (delay-expression expression)))

(define (analyze/sequence expression)
  (analyze/expressions (sequence-actions expression)))

(define (analyze/conditional expression)
  (analyze/expressions (conditional-components expression list)))

(define (analyze/disjunction expression)
  (analyze/expressions (disjunction-components expression list)))

(define (analyze/comment expression)
  (analyze/expression (comment-expression expression)))

(define analyze/dispatch
  (make-scode-walker
   analyze/uninteresting
   `((ACCESS ,analyze/access)
     (ASSIGNMENT ,analyze/assignment)
     (COMBINATION ,analyze/combination)
     (COMMENT ,analyze/comment)
     (CONDITIONAL ,analyze/conditional)
     (DEFINITION ,analyze/error)
     (DELAY ,analyze/delay)
     (DISJUNCTION ,analyze/disjunction)
     (ERROR-COMBINATION ,analyze/error-combination)
     (IN-PACKAGE ,analyze/error)
     (LAMBDA ,analyze/lambda)
     (SEQUENCE ,analyze/sequence)
     (VARIABLE ,analyze/variable))))

(define (result-sum first rest)
  (with-values first
    (lambda (references assignments executions)
      (with-values rest
	(lambda (references* assignments* executions*)
	  (values (append! references references*)
		  (append! assignments assignments*)
		  (append! executions executions*)))))))