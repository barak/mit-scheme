#| -*-Scheme-*-

$Id: insmac.scm,v 1.7 2002/11/20 19:45:50 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Alpha Instruction Set Macros
;;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Definition macros

(define-syntax define-symbol-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(DEFINE-INTEGRABLE (,(cadr form) SYMBOL)
	(LET ((PLACE (ASSQ SYMBOL ',(cddr form))))
	  (IF (PAIR? PLACE)
	      (CDR PLACE)
	      #F))))))

(define-syntax define-transformer
  (rsc-macro-transformer
   (lambda (form environment)
     `(,(close-syntax 'DEFINE environment) ,@(cdr form)))))

;;;; Fixed width instruction parsing

(define (parse-instruction first-word tail early? environment)
  (if (not (null? tail))
      (error "Unknown format:" (cons first-word tail)))
  (let loop ((first-word first-word))
    (case (car first-word)
      ((LONG)
       (process-fields (cdr first-word) early? environment))
      ((VARIABLE-WIDTH)
       (process-variable-width first-word early? environment))
      ((IF)
       `(,(close-syntax 'IF environment)
	 ,(cadr first-word)
	 ,(loop (caddr first-word))
	 ,(loop (cadddr first-word))))
      (else
       (error "Unknown format:" first-word)))))

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
	  (error "process-fields: bad syllable size" size))
      `(,(close-syntax 'LIST environment)
	,(optimize-group-syntax code early? environment)))))

(define (expand-fields fields early? environment)
  (let expand ((first-word '()) (word-size 0) (fields fields))
    (if (pair? fields)
	(call-with-values
	    (lambda () (expand-field (car fields) early? environment))
	  (lambda (car-field car-size)
	    (if (= 32 (+ word-size car-size))
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
		    (values (if (zero? car-size)
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
					 4))
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