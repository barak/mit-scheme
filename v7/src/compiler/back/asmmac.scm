#| -*-Scheme-*-

$Id: asmmac.scm,v 1.12 2002/02/08 03:06:16 cph Exp $

Copyright (c) 1988, 1990, 1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Assembler Syntax Macros

(declare (usual-integrations))

(define-syntax define-instruction
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL * (DATUM + DATUM)) (cdr form))
	 `(ADD-INSTRUCTION!
	   ',(cadr form)
	   ,(compile-database (cddr form)
	      (lambda (pattern actions)
		pattern
		(if (not (pair? actions))
		    (error "DEFINE-INSTRUCTION: Too few forms."))
		(parse-instruction (car actions) (cdr actions) #f))))
	 (ill-formed-syntax form)))))

(define (compile-database cases procedure)
  `(LIST
    ,@(map (lambda (rule)
	     (parse-rule (car rule) (cdr rule)
	       (lambda (pattern variables qualifier actions)
		 `(CONS ',pattern
			,(rule-result-expression variables
						 qualifier
						 (procedure pattern
							    actions))))))
	   cases)))

(define optimize-group-syntax
  (let ()
    (define (find-constant components)
      (cond ((null? components)
	     '())
	    ((car-constant? components)
	     (compact (car-constant-value components)
		      (cdr components)))
	    (else
	     (cons (car components)
		   (find-constant (cdr components))))))

    (define (compact bit-string components)
      (cond ((null? components)
	     (cons (make-constant bit-string) '()))
	    ((car-constant? components)
	     (compact (instruction-append bit-string
					  (car-constant-value components))
		      (cdr components)))
	    (else
	     (cons (make-constant bit-string)
		   (cons (car components)
			 (find-constant (cdr components)))))))

    (define-integrable (car-constant? expression)
      (and (eq? (caar expression) 'QUOTE)
	   (bit-string? (cadar expression))))

    (define-integrable (car-constant-value constant)
      (cadar constant))

    (define-integrable (make-constant bit-string)
      `',bit-string)

    (lambda (components early?)
      (let ((components (find-constant components)))
	(cond ((null? components)
	       (error "OPTIMIZE-GROUP-SYNTAX: No components in group!"))
	      ((null? (cdr components))
	       (car components))
	      (else
	       `(,(if early?
		      'OPTIMIZE-GROUP-EARLY
		      'OPTIMIZE-GROUP)
		 ,@components)))))))