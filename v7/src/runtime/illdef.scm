#| -*-Scheme-*-

$Id: illdef.scm,v 1.4 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1991-1999 Massachusetts Institute of Technology

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

;;;; Check for Illegal Definitions
;;; package: (runtime illegal-definitions)

(declare (usual-integrations))

(define walker)

(define (initialize-package!)
  (set! walker
	(make-scode-walker walk/constant
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
  unspecific)

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
    (if (null? (cdr expressions))
	(walk/no-definitions (car expressions))
	(begin
	  (walk/expression (car expressions) 'LEGAL)
	  (loop (cdr expressions))))))

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