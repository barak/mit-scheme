#| -*-Scheme-*-

$Id: codwlk.scm,v 14.3 1999/01/02 06:11:34 cph Exp $

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

;;;; SCode Walker
;;; package: (runtime scode-walker)

(declare (usual-integrations))

(define-structure (scode-walker (constructor %make-scode-walker)
				(conc-name scode-walker/))
  (access false read-only true)
  (assignment false read-only true)
  (combination false read-only true)
  (comment false read-only true)
  (conditional false read-only true)
  (constant false read-only true)
  (declaration false read-only true)
  (definition false read-only true)
  (delay false read-only true)
  (disjunction false read-only true)
  (error-combination false read-only true)
  (in-package false read-only true)
  (lambda false read-only true)
  (open-block false read-only true)
  (quotation false read-only true)
  (sequence false read-only true)
  (the-environment false read-only true)
  (unassigned? false read-only true)
  (variable false read-only true))

(define (make-scode-walker default alist)
  (let ((alist
	 (map (lambda (entry)
		(cons (car entry) (cadr entry)))
	      alist)))
    (let ((result
	   (let ((lookup
		  (lambda (name default)
		    (let ((entry (assq name alist)))
		      (if entry
			  (begin (set! alist (delq! entry alist))
				 (cdr entry))
			  default)))))
	     (let ((comment-handler (lookup 'COMMENT default))
		   (combination-handler (lookup 'COMBINATION default))
		   (sequence-handler (lookup 'SEQUENCE default)))
	       (%make-scode-walker (lookup 'ACCESS default)
				   (lookup 'ASSIGNMENT default)
				   combination-handler
				   comment-handler
				   (lookup 'CONDITIONAL default)
				   default
				   (lookup 'DECLARATION comment-handler)
				   (lookup 'DEFINITION default)
				   (lookup 'DELAY default)
				   (lookup 'DISJUNCTION default)
				   (lookup 'ERROR-COMBINATION
					   combination-handler)
				   (lookup 'IN-PACKAGE default)
				   (lookup 'LAMBDA default)
				   (lookup 'OPEN-BLOCK sequence-handler)
				   (lookup 'QUOTATION default)
				   sequence-handler
				   (lookup 'THE-ENVIRONMENT default)
				   (lookup 'UNASSIGNED? combination-handler)
				   (lookup 'VARIABLE default))))))
      (if (not (null? alist))
	  (error "MAKE-SCODE-WALKER: Unrecognized alist items" alist))
      result)))

(define (scode-walk walker expression)
  ((vector-ref dispatch-vector (object-type expression)) walker expression))

(define dispatch-vector)

(define (initialize-package!)
  (set! dispatch-vector
	(let ((table (make-vector (microcode-type/code-limit) walk/constant)))
	  (for-each (lambda (entry)
		      (let ((kernel
			     (lambda (name)
			       (vector-set! table
					    (microcode-type name)
					    (cadr entry)))))
			(if (pair? (car entry))
			    (for-each kernel (car entry))
			    (kernel (car entry)))))
		    `((ACCESS ,walk/access)
		      (ASSIGNMENT ,walk/assignment)
		      ((COMBINATION
			COMBINATION-1
			COMBINATION-2
			PRIMITIVE-COMBINATION-0
			PRIMITIVE-COMBINATION-1
			PRIMITIVE-COMBINATION-2
			PRIMITIVE-COMBINATION-3)
		       ,walk/combination)
		      (COMMENT ,walk/comment)
		      (CONDITIONAL ,walk/conditional)
		      (DEFINITION ,walk/definition)
		      (DELAY ,walk/delay)
		      (DISJUNCTION ,walk/disjunction)
		      (IN-PACKAGE ,walk/in-package)
		      ((LAMBDA LEXPR EXTENDED-LAMBDA) ,walk/lambda)
		      (QUOTATION ,walk/quotation)
		      ((SEQUENCE-2 SEQUENCE-3) ,walk/sequence)
		      (THE-ENVIRONMENT ,walk/the-environment)
		      (VARIABLE ,walk/variable)))
	  table)))

(define (walk/combination walker expression)
  (let ((operator (combination-operator expression)))
    (cond ((and (or (eq? operator (ucode-primitive lexical-unassigned?))
		    (absolute-reference-to? operator 'LEXICAL-UNASSIGNED?))
		(let ((operands (combination-operands expression)))
		  (and (the-environment? (car operands))
		       (symbol? (cadr operands)))))
	   (scode-walker/unassigned? walker))
	  ((or (eq? operator (ucode-primitive error-procedure))
	       (absolute-reference-to? operator 'ERROR-PROCEDURE))
	   (scode-walker/error-combination walker))
	  (else
	   (scode-walker/combination walker)))))

(define (walk/comment walker expression)
  (if (declaration? expression)
      (scode-walker/declaration walker)
      (scode-walker/comment walker)))

(define (walk/sequence walker expression)
  (if (open-block? expression)
      (scode-walker/open-block walker)
      (scode-walker/sequence walker)))

(define (walk/access walker expression)
  expression
  (scode-walker/access walker))

(define (walk/assignment walker expression)
  expression
  (scode-walker/assignment walker))

(define (walk/conditional walker expression)
  expression
  (scode-walker/conditional walker))

(define (walk/constant walker expression)
  expression
  (scode-walker/constant walker))

(define (walk/definition walker expression)
  expression
  (scode-walker/definition walker))

(define (walk/delay walker expression)
  expression
  (scode-walker/delay walker))

(define (walk/disjunction walker expression)
  expression
  (scode-walker/disjunction walker))

(define (walk/in-package walker expression)
  expression
  (scode-walker/in-package walker))

(define (walk/lambda walker expression)
  expression
  (scode-walker/lambda walker))

(define (walk/quotation walker expression)
  expression
  (scode-walker/quotation walker))

(define (walk/the-environment walker expression)
  expression
  (scode-walker/the-environment walker))

(define (walk/variable walker expression)
  expression
  (scode-walker/variable walker))