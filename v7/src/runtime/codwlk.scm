#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/codwlk.scm,v 14.1 1988/05/20 00:54:04 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; SCode Walker
;;; scode-walker-package

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