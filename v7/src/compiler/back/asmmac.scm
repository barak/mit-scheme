#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/asmmac.scm,v 1.3 1987/07/08 22:00:25 jinx Exp $

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

;;;; Assembler Syntax Macros

(declare (usual-integrations))

(syntax-table-define assembler-syntax-table 'DEFINE-INSTRUCTION
  (macro (keyword . rules)
    `(ADD-INSTRUCTION!
      ',keyword
      ,(compile-database rules
	 (lambda (pattern actions)
	   (if (null? actions)
	       (error "DEFINE-INSTRUCTION: Too few forms")
	       (parse-word (car actions) (cdr actions))))))))

(define (compile-database cases procedure)
  `(LIST
    ,@(map (lambda (case)
	     (parse-rule (car case) (cdr case)
	       (lambda (pattern variables qualifier actions)
		 `(CONS ',pattern
			,(rule-result-expression variables
						 qualifier
						 (procedure pattern
							    actions))))))
	   cases)))

;;;; Group Optimization

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
	     (compact (bit-string-append (car-constant-value components)
					 bit-string)
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
