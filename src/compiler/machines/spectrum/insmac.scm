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

;;;; Spectrum Instruction Set Macros

(declare (usual-integrations))

;;;; Definition macros

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

;;;; Fixed width instruction parsing

(define (parse-instruction first-word tail early? environment)
  (if (not (null? tail))
      (error "Unknown format:" (cons first-word tail)))
  (case (car first-word)
    ((LONG) (process-fields (cdr first-word) early? environment))
    ((VARIABLE-WIDTH) (process-variable-width first-word early? environment))
    (else (error "Unknown format:" first-word))))

(define (process-variable-width descriptor early? environment)
  (let ((binding (cadr descriptor))
	(clauses (cddr descriptor)))
    `(,(close-syntax 'LIST environment)
      ,(variable-width-expression-syntaxer
	(car binding)			; name
	(cadr binding)			; expression
	environment
	(map (lambda (clause)
	       (call-with-values
		   (lambda ()
		     (expand-fields (cdadr clause) early? environment))
		 (lambda (code size)
		   (if (not (zero? (remainder size 32)))
		       (error "Bad clause size:" size))
		   `((,(close-syntax 'LIST environment)
		      ,(optimize-group-syntax code early? environment))
		     ,size
		     ,@(car clause)))))
	     clauses)))))

(define (process-fields fields early? environment)
  (call-with-values (lambda () (expand-fields fields early? environment))
    (lambda (code size)
      (if (not (zero? (remainder size 32)))
	  (error "Bad syllable size:" size))
      `(,(close-syntax 'LIST environment)
	,(optimize-group-syntax code early? environment)))))

(define (expand-fields fields early? environment)
  (let expand ((first-word '()) (word-size 0) (fields fields))
    (if (pair? fields)
	(call-with-values
	    (lambda () (expand-field (car fields) early? environment))
	  (lambda (car-field car-size)
	    (if (and (eq? endianness 'LITTLE)
		     (= 32 (+ word-size car-size)))
		(call-with-values (lambda () (expand '() 0 (cdr fields)))
		  (lambda (tail tail-size)
		    (values (append (cons car-field first-word) tail)
			    (+ car-size tail-size))))
		(call-with-values
		    (lambda ()
		      (expand (cons car-field first-word)
			      (+ car-size word-size)
			      (cdr fields)))
		  (lambda (tail tail-size)
		    (values (if (or (zero? car-size)
				    (not (eq? endianness 'LITTLE)))
				(cons car-field tail)
				tail)
			    (+ car-size tail-size)))))))
	(values '() 0))))

(define (expand-field field early? environment)
  early?				; ignored for now
  (let ((size (car field))
	(expression (cadr field)))

    (define (default type)
      (values (integer-syntaxer expression environment type size)
	      size))

    (if (pair? (cddr field))
	(case (caddr field)
	  ((PC-REL)
	   (values (integer-syntaxer ``(,',(close-syntax '- environment)
					,,expression
					(,',(close-syntax '+ environment)
					 ,',(close-syntax '*PC* environment)
					 8))
				     environment
				     (cadddr field)
				     size)
		   size))
	  ((BLOCK-OFFSET)
	   (values `(,(close-syntax 'LIST environment)
		     'BLOCK-OFFSET
		     ,expression)
		   size))
	  (else
	   (default (caddr field))))
	(default 'UNSIGNED))))