#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/insmac.scm,v 1.1 1987/08/13 01:14:46 jinx Exp $

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

;;;; VAX Instruction Set Macros

(declare (usual-integrations))

;;;; Effective addressing

(syntax-table-define assembler-syntax-table 'MAKE-EA-DATABASE
  (macro rules
    (compile-database
     rules
     (lambda (pattern actions)
       (let ((keyword (car pattern))
	     (categories (car actions))
	     (value (cdr actions)))
	 (declare (integrate keyword categories value))
	 `(MAKE-EFFECTIVE-ADDRESS
	   ',keyword
	   ',categories
	   ,(expand-fields value)))))))

(syntax-table-define assembler-syntax-table 'DEFINE-EA-TRANSFORMER
  (macro (name category type)
    `(define (,name expression)
       (let ((ea (process-ea expression ',type)))
	 (and ea
	      (memq ',category (ea-categories ea))
	      ea)))))

(syntax-table-define assembler-syntax-table 'DEFINE-SYMBOL-TRANSFORMER
  (macro (name . alist)
    `(begin
       (declare (integrate-operator ,name))
       (define (,name symbol)
	 (declare (integrate symbol))
	 (let ((place (assq symbol ',alist)))
	   (if (null? place)
	       #F
	       (cdr place)))))))

(syntax-table-define assembler-syntax-table 'DEFINE-TRANSFORMER
  (macro (name value)
    `(define ,name ,value)))

(define (parse-instruction opcode tail ignore)
  (expand-fields (cons opcode tail)))

(define (expand-fields fields)
  (if (null? fields)
      '()
      (case (caar fields)
	((BYTE)
	 (collect-byte (cdar field)
		       (expand-fields (cdr fields))))
	((OPERAND)
	 `(CONS-SYNTAX
	   ,(cadar fields)
	   ,(expand-fields (cdr fields))))
	((DISPLACEMENT)
	 (let ((desc (cadar field)))
	   (let ((expression (cadr desc))
		 (size (car desc)))
	     `(CONS-SYNTAX
	       ,(integer-syntaxer expression 'DISPLACEMENT size)
	       ,(expand-fields (cdr fields))))))
	(else
	 (error "expand-fields: Unknown field kind" (caar field))))))

(define (collect-byte components tail)
  (define (inner components)
    (if (null? components)
	tail
	(let ((size (caar components))
	      (expression (cadar components))
	      (type (if (null? (cddar components))
			'UNSIGNED
			'SIGNED)))
	  `(CONS-SYNTAX
	    ,(integer-syntaxer expression type size)
	    ,(inner (cdr components))))))
  (inner components))

