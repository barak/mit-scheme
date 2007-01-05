#| -*-Scheme-*-

$Id: insmac.scm,v 1.21 2007/01/05 21:19:21 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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
	     (BYTE (8 ,(close-syntax (caddr form) environment)))
	     ,@(map (lambda (extra)
		      `(BYTE (8 ,(close-syntax extra environment))))
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
		  (categories (car actions))
		  (mode (cadr actions))
		  (register (caddr actions))
		  (tail (cdddr actions)))
	      `(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
		',keyword
		',categories
		,(integer-syntaxer mode environment 'UNSIGNED 2)
		,(integer-syntaxer register environment 'UNSIGNED 3)
		,(if (null? tail)
		     `()
		     (process-fields tail #f environment))))))))))

;; This one is necessary to distinguish between r/mW mW, etc.

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

;; *** We can't really handle switching these right now. ***

(define-integrable *ADDRESS-SIZE* 32)
(define-integrable *OPERAND-SIZE* 32)

(define (parse-instruction opcode tail early? environment)
  (process-fields (cons opcode tail) early? environment))

(define (process-fields fields early? environment)
  (if (and (null? (cdr fields))
	   (eq? (caar fields) 'VARIABLE-WIDTH))
      (expand-variable-width (car fields) early? environment)
      (call-with-values (lambda () (expand-fields fields early? environment))
	(lambda (code size)
	  (if (not (zero? (remainder size 8)))
	      (error "Bad syllable size:" size))
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
	       (call-with-values
		   (lambda () (expand-fields (cdr clause) early? environment))
		 (lambda (code size)
		   (if (not (zero? (remainder size 8)))
		       (error "Bad clause size:" size))
		   `(,code ,size ,@(car clause)))))
	     clauses)))))

(define (expand-fields fields early? environment)
  (if (pair? fields)
      (call-with-values
	  (lambda () (expand-fields (cdr fields) early? environment))
       (lambda (tail tail-size)
	 (case (caar fields)
	   ;; For opcodes and fixed fields of the instruction
	   ((BYTE)
	    ;; (BYTE (8 #xff))
	    ;; (BYTE (16 (+ foo #x23) SIGNED))
	    (call-with-values
		(lambda ()
		  (collect-byte (cdar fields) tail environment))
	      (lambda (code size)
		(values code (+ size tail-size)))))
	   ((ModR/M)
	    ;; (ModR/M 2 source)	= /2 r/m(source)
	    ;; (ModR/M r target)	= /r r/m(target)
	    (if early?
		(error "No early support for ModR/M -- Fix i386/insmac.scm"))
	    (let ((field (car fields)))
	      (let ((digit-or-reg (cadr field))
		    (r/m (caddr field)))
		(values `(,(close-syntax 'CONS-SYNTAX environment)
			  (,(close-syntax 'EA/REGISTER environment) ,r/m)
			  (,(close-syntax 'CONS-SYNTAX environment)
			   ,(integer-syntaxer digit-or-reg environment
					      'UNSIGNED 3)
			   (,(close-syntax 'CONS-SYNTAX environment)
			    (,(close-syntax 'EA/MODE environment) ,r/m)
			    (,(close-syntax 'APPEND-SYNTAX! environment)
			     (,(close-syntax 'EA/EXTRA environment) ,r/m)
			     ,tail))))
			(+ 8 tail-size)))))
	   ;; For immediate operands whose size depends on the operand
	   ;; size for the instruction (halfword vs. longword)
	   ((IMMEDIATE)
	    (values
	     (let ((field (car fields)))
	       (let ((value (cadr field))
		     (mode (if (pair? (cddr field)) (caddr field) 'OPERAND))
		     (domain
		      (if (and (pair? (cddr field)) (pair? (cdddr field)))
			  (cadddr field)
			  'SIGNED)))
		 `(,(close-syntax 'CONS-SYNTAX environment)
		   ,(integer-syntaxer
		     value
		     environment
		     domain
		     (case mode
		       ((OPERAND) *operand-size*)
		       ((ADDRESS) *address-size*)
		       (else (error "Unknown IMMEDIATE mode:" mode))))
		   ,tail)))
	     tail-size))
	   (else
	    (error "Unknown field kind:" (caar fields))))))
      (values `'() 0)))

(define (collect-byte components tail environment)
  (let loop ((components components))
    (if (pair? components)
	(call-with-values (lambda () (loop (cdr components)))
	  (lambda (byte-tail byte-size)
	    (let ((size (caar components))
		  (expression (cadar components))
		  (type (if (pair? (cddar components))
			    (caddar components)
			    'UNSIGNED)))
	      (values `(,(close-syntax 'CONS-SYNTAX environment)
			,(integer-syntaxer expression environment type size)
			,byte-tail)
		      (+ size byte-size)))))
	(values tail 0))))