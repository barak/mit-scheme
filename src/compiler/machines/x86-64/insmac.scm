#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Intel 386 Instruction Set Macros

(declare (usual-integrations))

(define-syntax define-trivial-instruction
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER DATUM * DATUM) (cdr form))
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (()
	     (BITS (8 ,(close-syntax (caddr form) environment)))
	     ,@(map (lambda (extra)
		      `(BITS (8 ,(close-syntax extra environment))))
		    (cdddr form))))
	 (ill-formed-syntax form)))))

;;;; Effective addressing

(define ea-database-name
  'EA-DATABASE)

(define-syntax define-ea-database
  (rsc-macro-transformer
   (lambda (form environment)
     `(,(close-syntax 'DEFINE environment)
       ,ea-database-name
       ,(compile-database (cdr form) environment
	  (lambda (pattern actions)
	    (let ((keyword (car pattern))
		  (categories (list-ref actions 0))
		  (rex-prefix (list-ref actions 1))
		  (mode (list-ref actions 2))
		  (register (list-ref actions 3))
		  (tail (list-tail actions 4)))
	      `(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
		',keyword
		',categories
		',rex-prefix
		,(integer-syntaxer mode environment 'UNSIGNED 2)
		,(integer-syntaxer register environment 'UNSIGNED 3)
		,(if (null? tail)
		     `(,(close-syntax 'QUOTE environment) ())
		     (process-fields tail #f environment))))))))))

;; This one is necessary to distinguish between r/m-ea, m-ea, etc.

(define-syntax define-ea-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER ? SYMBOL) (cdr form))
	 `(DEFINE (,(cadr form) EXPRESSION)
	    (LET ((MATCH-RESULT (PATTERN-LOOKUP ,ea-database-name EXPRESSION)))
	      (AND MATCH-RESULT
		   ,(if (pair? (cddr form))
			`(LET ((EA (MATCH-RESULT)))
			   (AND (MEMQ ',(caddr form) (EA/CATEGORIES EA))
				EA))
			`(MATCH-RESULT)))))
	 (ill-formed-syntax form)))))

(define (parse-instruction opcode tail early? environment)
  (process-fields (cons opcode tail) early? environment))

(define (process-fields fields early? environment)
  (if (and (null? (cdr fields))
	   (eq? (caar fields) 'VARIABLE-WIDTH))
      (expand-variable-width (car fields) early? environment)
      (call-with-values (lambda () (expand-fields fields early? environment))
	(lambda (code size)
	  size				;ignore
	  code))))

(define (expand-variable-width field early? environment)
  (let ((binding (cadr field))
	(clauses (cddr field)))
    `(,(close-syntax 'LIST environment)
      ,(variable-width-expression-syntaxer
	(car binding)
	(cadr binding)
	environment
	(map (lambda (clause)
	       (receive (code size)
		   (expand-fields (cdr clause) early? environment)
		 (if (not (zero? (remainder size 8)))
		     (error "Bad clause size:" size))
		 `(,code ,size ,@(car clause))))
	     clauses)))))

(define (expand-fields fields early? environment)
  (if (pair? fields)
      (receive (tail tail-size) (expand-fields (cdr fields) early? environment)
	(case (caar fields)
	  ;; For opcodes and fixed fields of the instruction
	  ((BITS)
	   ;; (BITS (8 #xff))
	   ;; (BITS (16 (+ foo #x23) SIGNED))
	   (receive (code size) (collect-bits (cdar fields) tail environment)
	     (values code (+ size tail-size))))
	  ((PREFIX)
	   ;; (PREFIX (OPERAND size) (REGISTER [reg]) (EA ea))
	   (if early?
	       (error "No early support for PREFIX -- Fix x86-64/insmac.scm"))
	   (values (collect-prefix (cdar fields) tail environment) -1))
	  ((ModR/M)
	   ;; (ModR/M 2 source)	= /2 r/m(source)
	   ;; (ModR/M r target)	= /r r/m(target)
	   (if early?
	       (error "No early support for ModR/M -- Fix x86-64/insmac.scm"))
	   (values (collect-ModR/M (cdar fields) tail environment) -1))
	  (else
	   (error "Unknown field kind:" (caar fields)))))
      (values `(,(close-syntax 'QUOTE environment) ()) 0)))

(define (collect-bits components tail environment)
  (let loop ((components components))
    (if (pair? components)
	(receive (bits-tail bits-size) (loop (cdr components))
	  (let ((size (caar components))
		(expression (cadar components))
		(type (if (pair? (cddar components))
			  (caddar components)
			  'UNSIGNED)))
	    (values `(,(close-syntax 'CONS-SYNTAX environment)
		      ,(integer-syntaxer expression environment type size)
		      ,bits-tail)
		    (+ size bits-size))))
	(values tail 0))))

(define (collect-prefix options tail environment)
  (let loop ((options options) (operand #f) (register #f) (r/m #f))
    (if (pair? options)
	(case (caar options)
	  ((OPERAND) (loop (cdr options) (cadar options) register r/m))
	  ((OPCODE-REGISTER)
	   (loop (cdr options)
		 operand
		 (or (not (pair? (cdar options))) (cadar options))
		 r/m))
	  ((ModR/M)
	   ;; (ModR/M <r/m>), for fixed digits
	   ;; (ModR/M <reg> <r/m>), for registers
	   (if (pair? (cddar options))
	       (loop (cdr options) operand (cadar options) (caddar options))
	       (loop (cdr options) operand #f (cadar options))))
	  (else (error "Bad instruction prefix option:" (car options))))
	(let ((cons-prefix (close-syntax 'CONS-PREFIX environment)))
	  `(,cons-prefix ,operand ,register ,r/m ,tail)))))

(define (collect-ModR/M field tail environment)
  (let ((digit-or-reg (car field))
	(ea (cadr field)))
    `(,(close-syntax 'CONS-ModR/M environment)
      ,(integer-syntaxer
	(if (integer? digit-or-reg)
	    (fix:and digit-or-reg 7)
	    `(,(close-syntax 'FIX:AND environment) ,digit-or-reg 7))
	environment
	'UNSIGNED
	3)
      ,ea
      ,tail)))