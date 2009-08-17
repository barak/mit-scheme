#| -*-Scheme-*-

$Id: 854570faba21596a80f0b8800205d1c4fad4badd $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; SCode Walker
;;; package: (runtime scode-walker)

(declare (usual-integrations))

(define-structure (scode-walker (constructor %make-scode-walker)
				(conc-name scode-walker/))
  (access #f read-only #t)
  (assignment #f read-only #t)
  (combination #f read-only #t)
  (comment #f read-only #t)
  (conditional #f read-only #t)
  (constant #f read-only #t)
  (declaration #f read-only #t)
  (definition #f read-only #t)
  (delay #f read-only #t)
  (disjunction #f read-only #t)
  (error-combination #f read-only #t)
  (lambda #f read-only #t)
  (open-block #f read-only #t)
  (quotation #f read-only #t)
  (sequence #f read-only #t)
  (the-environment #f read-only #t)
  (unassigned? #f read-only #t)
  (variable #f read-only #t))

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