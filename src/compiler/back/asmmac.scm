#| -*-Scheme-*-

$Id: asmmac.scm,v 1.20 2004/07/05 03:59:36 cph Exp $

Copyright 1986,1987,1988,1990,2001,2002 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Assembler Syntax Macros

(declare (usual-integrations))

(define-syntax define-instruction
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL * (DATUM + DATUM)) (cdr form))
	 `(,(close-syntax 'ADD-INSTRUCTION! environment)
	   ',(cadr form)
	   ,(compile-database (cddr form) environment
	      (lambda (pattern actions)
		pattern
		(if (not (pair? actions))
		    (error "DEFINE-INSTRUCTION: Too few forms."))
		(parse-instruction (car actions) (cdr actions) #f
				   environment))))
	 (ill-formed-syntax form)))))

(define (compile-database rules environment procedure)
  `(,(close-syntax 'LIST environment)
    ,@(map (lambda (rule)
	     (receive (pattern variables qualifiers actions)
		 (parse-rule (car rule) (cdr rule))
	       (make-rule-matcher
		pattern
		(rule-result-expression variables
					qualifiers
					(procedure pattern actions)
					environment)
		environment)))
	   rules)))

(define (optimize-group-syntax components early? environment)
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

  (define (car-constant? components)
    (and (identifier=? environment (caar components)
		       system-global-environment 'QUOTE)
	 (bit-string? (cadar components))))

  (define-integrable (car-constant-value constant)
    (cadar constant))

  (define-integrable (make-constant bit-string)
    `',bit-string)

  (let ((components (find-constant components)))
    (if (not (pair? components))
	(error "No components in group!"))
    (if (pair? (cdr components))
	`(,(close-syntax (if early?
			     'OPTIMIZE-GROUP-EARLY
			     'OPTIMIZE-GROUP)
			 environment)
	  ,@components)
	(car components))))