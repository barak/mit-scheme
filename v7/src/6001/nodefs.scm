#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/6001/nodefs.scm,v 1.1 1991/08/22 17:42:36 arthur Exp $

Copyright (c) 1991 Massachusetts Institute of Technology

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

;;;; SCode rewriting for 6.001
;;; package: (student scode-rewriting)

(declare (usual-integrations))

(define (initialize-package!)
  (set! hook/repl-eval student/repl-eval)
  unspecific)

(define (student/repl-eval repl s-expression environment syntax-table)
  (let ((scode (rewrite-scode (syntax s-expression syntax-table) repl)))
    (with-new-history (lambda () (extended-scode-eval scode environment)))))

(define (rewrite-scode expression repl)
  (let ((expression
	 (if (open-block? expression)
	     (open-block-components expression unscan-defines)
	     expression)))
    (check-for-illegal-definitions expression)
    (make-sequence
     (map (lambda (expression)
	    (if (definition? expression)
		(let ((name (definition-name expression))
		      (value (definition-value expression)))
		  (make-sequence
		   (list expression
			 (make-combination write-definition-value
					   (list name
						 (make-variable name))))))
		expression))
	  (sequence-actions expression)))))

(define (write-definition-value name value)
  (let ((port (nearest-cmdl/output-port)))
    (fresh-line port)
    (write-string ";" port)
    (write name port)
    (write-string ": " port)
    (write value port)))

(define (check-for-illegal-definitions expression)
  (walk/expression (if (open-block? expression)
		       (open-block-components expression unscan-defines)
		       expression)
		   'LEGAL))

(define (walk/expression expression context)
  ((scode-walk walker expression) expression context))

(define-integrable (walk/no-definitions expression)
  (walk/expression expression 'ILLEGAL))

(define (walk/lambda expression context)
  context
  (let loop
      ((expressions
	(sequence-actions
	 (lambda-components expression
	   (lambda (name required optional rest auxiliary declarations body)
	     name required optional rest
	     (unscan-defines auxiliary declarations body))))))
    (if (definition? (car expressions))
	(begin
	  (walk/no-definitions (definition-value (car expressions)))
	  (if (not (null? (cdr expressions)))
	      (loop (cdr expressions))))
	(for-each walk/no-definitions expressions))))

(define (walk/definition expression context)
  (case context
    ((ILLEGAL)
     (error "Definition appears in illegal context:"
	    (unsyntax expression)))
    ((UNUSUAL)
     (warn "Definition appears in unusual context:"
	   (unsyntax expression))))
  (walk/no-definitions (definition-value expression)))

(define (walk/sequence expression context)
  (for-each (lambda (expression)
	      (walk/expression expression context))
	    (sequence-actions expression)))

(define (walk/constant expression context)
  expression context
  unspecific)

(define (walk/access expression context)
  context
  (walk/no-definitions (access-environment expression)))

(define (walk/assignment expression context)
  context
  (walk/no-definitions (assignment-value expression)))

(define (walk/combination expression context)
  context
  (walk/no-definitions (combination-operator expression))
  (for-each walk/no-definitions (combination-operands expression)))

(define (walk/comment expression context)
  (walk/expression (comment-expression expression) context))

(define (walk/conditional expression context)
  (walk/no-definitions (conditional-predicate expression))
  (let ((context (if (eq? 'LEGAL context) 'UNUSUAL context)))
    (walk/expression (conditional-consequent expression) context)
    (walk/expression (conditional-alternative expression) context)))

(define (walk/delay expression context)
  context
  (walk/no-definitions (delay-expression expression)))

(define (walk/disjunction expression context)
  (walk/no-definitions (disjunction-predicate expression))
  (walk/expression (disjunction-alternative expression)
		   (if (eq? 'LEGAL context) 'UNUSUAL context)))

(define (walk/in-package expression context)
  context
  (walk/no-definitions (in-package-environment expression))
  (check-for-illegal-definitions (in-package-expression expression)))

(define walker
  (make-scode-walker
   walk/constant
   `((ACCESS ,walk/access)
     (ASSIGNMENT ,walk/assignment)
     (COMBINATION ,walk/combination)
     (COMMENT ,walk/comment)
     (CONDITIONAL ,walk/conditional)
     (DEFINITION ,walk/definition)
     (DELAY ,walk/delay)
     (DISJUNCTION ,walk/disjunction)
     (IN-PACKAGE ,walk/in-package)
     (LAMBDA ,walk/lambda)
     (SEQUENCE ,walk/sequence))))