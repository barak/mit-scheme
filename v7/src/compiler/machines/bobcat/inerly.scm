#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/inerly.scm,v 1.1 1987/06/25 10:24:04 jinx Exp $

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

;;;; 68000 Instruction Set Macros.  Early version

(declare (usual-integrations))

(define early-instructions '())

(syntax-table-define early-syntax-table 'DEFINE-INSTRUCTION
  (macro (opcode . patterns)
    `(set! early-instructions
	   (cons (list ',opcode
		       ,@(map (lambda (pattern)
				`(early-parse-rule
				  ',(car pattern)
				  (scode-quote
				   ,(parse-word (cadr pattern)
						(cddr pattern)))))
			      patterns))
		 early-instructions))))

(syntax-table-define early-syntax-table 'DEFINE-EA-DATABASE
  (macro rules
    `(define early-ea-database
       (list
	,@(map (lambda (rule)
		 (apply (lambda (pattern categories mode register . extension)
			  (let ((keyword (car pattern)))
			    `(early-parse-rule
			      ',pattern
			      (list ',categories
				    (scode-quote
				     (MAKE-EFFECTIVE-ADDRESS
				      ',keyword
				      ,(integer-syntaxer mode 'UNSIGNED 3)
				      ,(integer-syntaxer register 'UNSIGNED 3)
				      (lambda (IMMEDIATE-SIZE INSTRUCTION-TAIL)
					,(if (null? extension)
					     'INSTRUCTION-TAIL
					     `(CONS-SYNTAX ,(car extension)
							   INSTRUCTION-TAIL)))
				      ',categories))))))
			rule))
	       rules)))))

(syntax-table-define early-syntax-table 'EXTENSION-WORD
  (syntax-table-ref assembler-syntax-table 'EXTENSION-WORD))

(syntax-table-define early-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name . restrictions)
    `(define-transformer ',name (apply make-ea-transformer ',restrictions))))

(syntax-table-define early-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . assoc)
    `(define-transformer ',name (make-symbol-transformer ',assoc))))

(syntax-table-define early-syntax-table 'DEFINE-REG-LIST-TRANSFORMER
  (macro (name . assoc)
    `(define-transformer ',name (make-bit-mask-transformer 16 ',assoc))))

;;;; Utility procedures

(define (eq-subset? s1 s2)
  (or (null? s1)
      (and (memq (car s1) s2)
	   (eq-subset? (cdr s1) s2))))

(define (make-ea-transformer #!optional modes keywords)
  (make-database-transformer
    (mapcan (lambda (rule)
	      (apply
	       (lambda (pattern variables extra)
		 (let ((categories (car extra))
		       (expression (cadr extra)))
		   (if (and (or (unassigned? modes) (eq-subset? modes categories))
			    (or (unassigned? keywords) (not (memq (car pattern) keywords))))
		       (list (list pattern variables expression))
		       '())))
	       rule))
	    early-ea-database)))
