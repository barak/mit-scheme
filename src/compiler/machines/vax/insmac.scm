#| -*-Scheme-*-

$Id: 3d800c0d1c843c82e3266316781036d32675ce46 $

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

;;;; VAX Instruction Set Macros

(declare (usual-integrations))

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
		  (value (cdr actions)))
	      `(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
		',keyword
		',categories
		,(process-fields value #f environment)))))))))

(define-syntax define-ea-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER DATUM DATUM) (cdr form))
	 `(DEFINE (,(cadr form) EXPRESSION)
	    (LET ((EA (PROCESS-EA EXPRESSION ',(cadddr form))))
	      (AND EA
		   (MEMQ ',(caddr form) (EA-CATEGORIES EA))
		   EA)))
	 (ill-formed-syntax form)))))

(define-syntax define-symbol-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER * SYMBOL) (cdr form))
	 `(DEFINE-INTEGRABLE (,(cadr form) SYMBOL)
	    (LET ((PLACE (ASSQ SYMBOL ',(cddr form))))
	      (IF (PAIR? PLACE)
		  (CDR PLACE)
		  #F)))
	 (ill-formed-syntax form)))))

(define-syntax define-transformer
  (rsc-macro-transformer
   (lambda (form environment)
     `(,(close-syntax 'DEFINE environment) ,@(cdr form)))))

(define-syntax define-trivial-instruction
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL EXPRESSION) (cdr form))
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (()
	     (BYTE (8 ,(close-syntax (caddr form) environment)))))
	 (ill-formed-syntax form)))))

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
	(car binding)			; name
	(cadr binding)			; expression
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
	    ((BYTE)
	     (call-with-values
		 (lambda () (collect-byte (cdar fields) tail environment))
	       (lambda (code size)
		 (values code (+ size tail-size)))))
	    ((OPERAND)
	     (values `(,(close-syntax 'APPEND-SYNTAX! environment)
		       ,(if early?
			    `(,(close-syntax 'EA-VALUE-EARLY environment)
			      ',(cadar fields)
			      ,(caddar fields))
			    `(,(close-syntax 'EA-VALUE environment)
			      ,(caddar fields)))
		       ,tail)
		     tail-size))
	    ;; Displacements are like signed bytes.  They are a
	    ;; different keyword to allow the disassembler to do its
	    ;; thing correctly.
	    ((DISPLACEMENT)
	     (let* ((desc (cadar fields))
		    (size (car desc)))
	       (values `(,(close-syntax 'CONS-SYNTAX environment)
			 ,(integer-syntaxer (cadr desc)
					    environment
					    'SIGNED
					    size)
			 ,tail)
		       (+ size tail-size))))
	    ((IMMEDIATE)
	     (values `(,(close-syntax 'CONS-SYNTAX environment)
		       (,(close-syntax 'COERCE-TO-TYPE environment)
			,(cadar fields)
			,(close-syntax '*IMMEDIATE-TYPE* environment)
			,(and (cddar fields)
			      (eq? (caddar fields) 'UNSIGNED)))
		       ,tail)
		     tail-size))
	    (else
	     (error "Unknown field kind:" (caar fields))))))
      (values `'() 0)))

(define (collect-byte components tail environment)
  (let inner ((components components))
    (if (pair? components)
	(call-with-values (lambda () (inner (cdr components)))
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