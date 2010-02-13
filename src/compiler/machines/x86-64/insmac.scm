#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
	    (if (not (and (list? actions)
			  (<= 4 (length actions))))
		(error "Malformed effective address rule:" pattern actions))
	    (let ((keyword (car pattern))
		  (categories (list-ref actions 0))
		  (rex (list-ref actions 1))
		  (mode (list-ref actions 2))
		  (r/m (list-ref actions 3))
		  (extra (list-tail actions 4)))
	      `(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
		(,(close-syntax 'QUOTE environment) ,keyword)
		,(parse-categories categories environment pattern)
		,(parse-rex rex environment pattern)
		,(parse-mode mode environment pattern)
		,(parse-r/m r/m environment pattern)
		,(parse-extra extra environment pattern)))))))))

;; This one is necessary to distinguish between r/m-ea, m-ea, etc.

(define-syntax define-ea-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER * SYMBOL) (cdr form))
	 `(DEFINE (,(cadr form) EXPRESSION)
	    (LET ((MATCH-RESULT (PATTERN-LOOKUP ,ea-database-name EXPRESSION)))
	      (AND MATCH-RESULT
		   ,(let ((categories (cddr form)))
		      (if (pair? categories)
			  `(LET ((EA (MATCH-RESULT)))
			     (AND
			      (OR
			       ,@(map (lambda (category)
					`(MEMQ ',category (EA/CATEGORIES EA)))
				      categories))
			      EA))
			  `(MATCH-RESULT))))))
	 (ill-formed-syntax form)))))

(define (parse-categories categories environment context)
  ;; At the moment only one category at a time is supported.
  (if (not (and (pair? categories)
		(eq? 'CATEGORIES (car categories))
		(pair? (cdr categories))
		(memq (cadr categories) '(REGISTER MEMORY XMM))
		(null? (cddr categories))))
      (error "Malformed CATEGORIES for effective address rule:"
	     categories
	     context))
  `(,(close-syntax 'QUOTE environment) ,(cdr categories)))

(define (parse-rex rex environment context)
  (define (expression:ior a b)
    (if (and (integer? a) (integer? b))
	(fix:or a b)
	`(,(close-syntax 'FIX:OR environment) ,a ,b)))
  (define (rex-bits name)
    (case name
      ((W) #x48) ((R) #x44) ((X) #x42) ((B) #x41)
      (else (error "Malformed REX bit name:" name context))))
  (if (not (and (pair? rex) (eq? 'REX (car rex)) (list? (cdr rex))))
      (error "Malformed REX prefix for effective address rule:" rex context))
  (let loop ((terms (cdr rex)) (expression 0))
    (if (not (pair? terms))
	expression
	(loop (cdr terms)
	      (expression:ior
	       expression
	       (let ((term (car terms)))
		 (if (pair? term)
		     (begin
		       (if (not (and (pair? (cdr term)) (null? (cddr term))))
			   (error "Malformed REX prefix term:" term context))
		       `(,(close-syntax 'REGISTER-REX environment)
			 ,(cadr term)
			 ,(rex-bits (car term))))
		     (rex-bits term))))))))

(define (parse-mode mode environment context)
  (if (not (and (pair? mode)
		(eq? 'MODE (car mode))
		(pair? (cdr mode))
		(null? (cddr mode))))
      (error "Malformed MODE for effective address rule:" mode context))
  (integer-syntaxer (cadr mode) environment 'UNSIGNED 2))

(define (parse-r/m r/m environment context)
  (if (not (and (pair? r/m)
		(eq? 'R/M (car r/m))
		(pair? (cdr r/m))
		(null? (cddr r/m))))
      (error "Malformed R/M for effective address rule:" r/m context))
  (integer-syntaxer (cadr r/m) environment 'UNSIGNED 3))

(define (parse-extra extra environment context)
  context				;ignore
  (if (pair? extra)
      (process-fields extra #f environment)
      `(,(close-syntax 'QUOTE environment) ())))

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
	   ;; (PREFIX (OPERAND size) (REGISTER [reg]) (ModR/M [reg] r/m))
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
  (let loop ((options options) (operand #f) (register #f) (r/m #f) (float #f))
    (if (pair? options)
	(case (caar options)
	  ((OPERAND) (loop (cdr options) (cadar options) register r/m float))
	  ((OPCODE-REGISTER)
	   (loop (cdr options)
		 operand
		 (or (not (pair? (cdar options))) (cadar options))
		 r/m
		 float))
	  ((ModR/M)
	   ;; (ModR/M <r/m>), for fixed digits
	   ;; (ModR/M <reg> <r/m>), for registers
	   (receive (register r/m)
	       (if (pair? (cddar options))
		   (values (cadar options) (caddar options))
		   (values #f (cadar options)))
	     (loop (cdr options) operand register r/m float)))
	  ((FLOAT)
	   ;; (FLOAT <scalar/packed> <single/double>)
	   (loop (cdr options) operand register r/m (cdar options)))
	  (else (error "Bad instruction prefix option:" (car options))))
	(if float
	    (let ((cons-float-prefix
		   (close-syntax 'CONS-FLOAT-PREFIX environment)))
	      (if operand
		  (error "Float instructions can't have operand size prefix:"
			 operand))
	      `(,cons-float-prefix ,register ,r/m ,(car float) ,(cadr float)
				   ,tail))
	    (let ((cons-prefix (close-syntax 'CONS-PREFIX environment)))
	      `(,cons-prefix ,operand ,register ,r/m ,tail))))))

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