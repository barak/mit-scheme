#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/inerly.scm,v 1.1 1987/08/22 22:51:27 jinx Exp $

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

;;;; VAX Instruction Set Macros.  Early version

(declare (usual-integrations))

;;;; Instruction macros

(define early-instructions '())

(syntax-table-define early-syntax-table 'DEFINE-INSTRUCTION
  (macro (opcode . patterns)
    `(set! early-instructions
	   (cons
	    (list ',opcode
		  ,@(map (lambda (pattern)
			   `(early-parse-rule
			     ',(car pattern)
			     (lambda (pat vars)
			       (early-make-rule
				pat
				vars
				(scode-quote
				 (instruction->instruction-sequence
				  ,(parse-instruction (cadr pattern)
						      (cddr pattern)
						      true)))))))
			 patterns))
		 early-instructions))))

;;;; Transformers and utilities

(define early-transformers '())

(define (define-early-transformer name transformer)
  (set! early-transformers
	(cons (cons name transformer)
	      early-transformers)))

(syntax-table-define early-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . assoc)
    `(define-early-transformer ',name (make-symbol-transformer ',assoc))))

;; *** Is this right? ***

(syntax-table-define early-syntax-table 'DEFINE-TRANSFORMER
  (macro (name . value)
    `(define-early-transformer ',name ,value)))

(syntax-table-define early-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name category type)
    `(define-early-transformer ',name
       (make-ea-transformer 'category 'type))))

(define *immediate-type*)

(define (make-ea-transformer category type)
  (let ((kernel
	 (make-database-transformer
	  (mapcan (lambda (rule)
		    (apply
		     (lambda (pattern variables categories expression)
		       (if (memq category categories)
			   (list (early-make-rule pattern variables expression))
			   '()))
		     rule))
		  early-ea-database))))
    (if (eq? type '?)
	kernel
	(lambda all
	  (fluid-let ((*immediate-type* type))
	    (apply kernel all))))))	     

;;;; Early effective address assembly.

;;; *** NOTE: If this format changes, insutl.scm must also be changed! ***

(syntax-table-define early-syntax-table 'DEFINE-EA-DATABASE
  (macro rules
    `(define early-ea-database
       (list
	,@(map (lambda (rule)
		 (apply
		  (lambda (pattern categories . fields)
		    (let ((keyword (car pattern)))
		      `(early-parse-rule
			',pattern
			(lambda (pat vars)
			  (list pat
				vars
				',categories
				(scode-quote
				 (MAKE-EFFECTIVE-ADDRESS
				  ',keyword
				  ',categories
				  ,(process-fields fields))))))))
		  rule))
	       rules)))))

(define (make-ea-selector-expander late-name index)
  ((access scode->scode-expander package/expansion package/scode-optimizer)
   (lambda (operands if-expanded if-not-expanded)
     (define (default)
       (if-expanded (scode/make-combination (scode/make-variable late-name)
					    operands)))

     (let ((operand (car operands)))
       (if (not (scode/combination? operand))
	   (default)
	   (scode/combination-components
	    operand
	    (lambda (operator operands)
	      (if (or (not (scode/variable? operator))
		      (not (eq? (scode/variable-name operator)
				'MAKE-EFFECTIVE-ADDRESS)))
		  (default)
		  (if-expanded (list-ref operands index))))))))))

;; The indeces here are the argument number to MAKE-EFFECTIVE-ADDRESS.

(define ea-keyword-expander (make-ea-selector-expander 'EA-KEYWORD 0))
(define ea-categories-expander (make-ea-selector-expander 'EA-CATEGORIES 1))
(define ea-value-expander (make-ea-selector-expander 'EA-VALUE 2))
