#| -*-Scheme-*-

$Id: insmac.scm,v 1.134 2003/02/14 18:28:02 cph Exp $

Copyright (c) 1988, 1990, 1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; 68000 Instruction Set Macros

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
	    (if (null? (cddr actions))
		(make-position-dependent pattern actions environment)
		(make-position-independent pattern actions environment))))))))

(define-syntax extension-word
  (rsc-macro-transformer
   (lambda (form environment)
     environment
     (call-with-values (lambda () (expand-descriptors (cdr form) environment))
       (lambda (instruction size source destination)
	 (if (or source destination)
	     (error "Source or destination used:" form))
	 (if (not (zero? (remainder size 16)))
	     (error "Extensions must be 16 bit multiples:" size))
	 (optimize-group-syntax instruction #f environment))))))

(define-syntax variable-extension
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((binding (cadr form))
	   (clauses (cddr form)))
       (variable-width-expression-syntaxer
	(car binding)
	(cadr binding)
	environment
	(map (lambda (clause)
	       `((,(close-syntax 'LIST environment)
		  ,(caddr clause))
		 ,(cadr clause)
		 ,@(car clause)))
	     clauses))))))

(define (make-position-independent pattern actions environment)
  (let ((keyword (car pattern))
	(categories (car actions))
	(mode (cadr actions))
	(register (caddr actions))
	(extension (cdddr actions)))
    `(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
      ',keyword
      ,(integer-syntaxer mode environment 'UNSIGNED 3)
      ,(integer-syntaxer register environment 'UNSIGNED 3)
      (,(close-syntax 'LAMBDA environment)
       (IMMEDIATE-SIZE INSTRUCTION-TAIL)
       IMMEDIATE-SIZE			;ignore if not referenced
       ,(if (pair? extension)
	    `(,(close-syntax 'CONS-SYNTAX environment)
	      ,(car extension)
	      INSTRUCTION-TAIL)
	    `INSTRUCTION-TAIL))
      ',categories)))

(define (make-position-dependent pattern actions environment)
  (let ((keyword (car pattern))
	(categories (car actions))
	(code (cdr (cadr actions))))
    (let ((name (car code))
	  (mode (cadr code))
	  (register (caddr code))
	  (extension (cadddr code)))
      `(,(close-syntax 'LET environment)
	((,name (,(close-syntax 'GENERATE-LABEL environment) 'MARK)))
	(,(close-syntax 'MAKE-EFFECTIVE-ADDRESS environment)
	 ',keyword
	 ,(process-ea-field mode environment)
	 ,(process-ea-field register environment)
	 (,(close-syntax 'LAMBDA environment)
	  (IMMEDIATE-SIZE INSTRUCTION-TAIL)
	  IMMEDIATE-SIZE		;ignore if not referenced
	  ,(if (pair? extension)
	       `(,(close-syntax 'CONS environment)
		 (,(close-syntax 'LIST environment) 'LABEL ,name)
		 (,(close-syntax 'CONS-SYNTAX environment)
		  ,extension
		  INSTRUCTION-TAIL))
	       `INSTRUCTION-TAIL))
	 ',categories)))))

(define (process-ea-field field environment)
  (if (exact-integer? field)
      (integer-syntaxer field environment 'UNSIGNED 3)
      (let ((binding (cadr field))
	    (clauses (cddr field)))
	(variable-width-expression-syntaxer
	 (car binding)
	 (cadr binding)
	 environment
	 (map (lambda (clause)
		`((,(close-syntax 'LIST environment)
		   ,(integer-syntaxer (cadr clause) environment 'UNSIGNED 3))
		  3
		  ,@(car clause)))
	      clauses)))))

;;;; Transformers

(define-syntax define-ea-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((filter
	    (lambda (items generator extraction)
	      (if (pair? items)
		  (if (pair? (cdr items))
		      `((LET ((TEMP ,extraction))
			  (AND
			   ,@(map (lambda (item) (generator item 'TEMP))
				  items))))
		      `(,(generator (car items) extraction)))
		  '()))))
       (let ((generate-definition
	      (lambda (name generate-match)
		`(DEFINE (,name EXPRESSION)
		   (LET ((MATCH-RESULT
			  (PATTERN-LOOKUP ,ea-database-name EXPRESSION)))
		     (AND MATCH-RESULT
			  ,(generate-match `(MATCH-RESULT)))))))
	     (filter-categories
	      (lambda (categories)
		(filter categories
			(lambda (cat exp) `(MEMQ ',cat ,exp))
			`(EA-CATEGORIES EA))))
	     (filter-keywords
	      (lambda (keywords)
		(filter keywords
			(lambda (key exp) `(NOT (EQ? ',key ,exp)))
			`(EA-KEYWORD EA)))))
	 (cond ((syntax-match? '(IDENTIFIER) (cdr form))
		(generate-definition (cadr form)
		  (lambda (ea)
		    ea)))
	       ((syntax-match? '(IDENTIFIER (* DATUM)) (cdr form))
		(generate-definition (cadr form)
		  (lambda (ea)
		    `(LET ((EA ,ea))
		       (AND ,@(filter-categories (caddr form))
			    EA)))))
	       ((syntax-match? '(IDENTIFIER (* DATUM) (* DATUM)) (cdr form))
		(generate-definition (cadr form)
		  (lambda (ea)
		    `(LET ((EA (MATCH-RESULT)))
		       (AND ,@(filter-categories (caddr form))
			    ,@(filter-keywords (cadddr form))
			    EA)))))
	       (else
		(ill-formed-syntax form))))))))

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

(define-syntax define-reg-list-transformer
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER * DATUM) (cdr form))
	 `(DEFINE-INTEGRABLE (,(cadr form) REG-LIST)
	    (ENCODE-REGISTER-LIST REG-LIST ',(cddr form)))
	 (ill-formed-syntax form)))))

;;;; Utility procedures

(define (parse-instruction expression tail early? environment)
  (define (kernel)
    (case (car expression)
      ((WORD) (parse-word expression tail environment))
      ((GROWING-WORD) (parse-growing-word expression tail environment))
      (else (error "Unknown expression:" expression))))
  (if (not early?)
      (with-normal-selectors kernel)
      (with-early-selectors kernel)))

;;; Variable width instruction parsing

(define (parse-growing-word expression tail environment)
  (if (not (null? tail))
      (error "PARSE-GROWING-WORD: non null tail" tail))
  (let ((binding (cadr expression)))
    `(LIST
      ,(variable-width-expression-syntaxer
	(car binding)
	(cadr binding)
	environment
	(map (lambda (clause)
	       (if (pair? (cddr clause))
		   (error "Extension found in clause:" clause))
	       (call-with-values
		   (lambda () (expand-descriptors (cdadr clause) environment))
		(lambda (instruction size src dst)
		  (if (not (zero? (remainder size 16)))
		      (error "Instructions must be 16 bit multiples:" size))
		  `(,(collect-word instruction src dst '())
		    ,size
		    ,@(car clause)))))	; Range
	     (cddr expression))))))

;;;; Fixed width instruction parsing

(define (parse-word expression tail environment)
  (call-with-values
      (lambda () (expand-descriptors (cdr expression) environment))
    (lambda (instruction size src dst)
      (if (not (zero? (remainder size 16)))
	  (error "Instructions must be 16 bit multiples:" size))
      (collect-word instruction src dst tail))))

(define (expand-descriptors descriptors environment)
  (if (pair? descriptors)
      (call-with-values
	  (lambda () (expand-descriptors (cdr descriptors) environment))
	(lambda (instruction* size* source* destination*)
	  (call-with-values
	      (lambda () (expand-descriptor (car descriptors) environment))
	    (lambda (instruction size source destination)
	      (values (append! instruction instruction*)
		      (+ size size*)
		      (if source
			  (begin
			    (if source*
				(error "Multiple source definitions:"
				       source source*))
			    source)
			  source*)
		      (if destination
			  (begin
			    (if destination*
				(error "Multiple destination definitions:"
				       destination destination*))
			    destination)
			  destination*))))))
      (values '() 0 #f #f)))	

(define (collect-word instruction src dst tail)
  (let ((code
	 (let ((code
		(let ((code (if dst `(,@dst '()) '())))
		  (if src
		      `(,@src ,code)
		      code))))
	   (cond ((null? tail) code)
		 ((null? (cdr tail))
		  `(,(if (null? code) 'CONS 'CONS-SYNTAX)
		    ,(car tail)
		    ,code))
		 (else
		  (error "PARSE-WORD: multiple tail elements" tail))))))
    (if (pair? instruction)
	`(,(if (null? code) 'CONS 'CONS-SYNTAX)
	  ,(optimize-group-syntax instruction
				  early-instruction-parsing?
				  environment)
	  ,code)
	code)))

;;;; Hooks for early instruction processing

(define early-instruction-parsing? #f)
(define ea-keyword-selector 'EA-KEYWORD)
(define ea-categories-selector 'EA-CATEGORIES)
(define ea-mode-selector 'EA-MODE)
(define ea-register-selector 'EA-REGISTER)
(define ea-extension-selector 'EA-EXTENSION)

(define (with-normal-selectors handle)
  (fluid-let ((early-instruction-parsing? #f)
	      (ea-keyword-selector 'EA-KEYWORD)
	      (ea-categories-selector 'EA-CATEGORIES)
	      (ea-mode-selector 'EA-MODE)
	      (ea-register-selector 'EA-REGISTER)
	      (ea-extension-selector 'EA-EXTENSION))
    (handle)))

(define (with-early-selectors handle)
  (fluid-let ((early-instruction-parsing? #t)
	      (ea-keyword-selector 'EA-KEYWORD-EARLY)
	      (ea-categories-selector 'EA-CATEGORIES-EARLY)
	      (ea-mode-selector 'EA-MODE-EARLY)
	      (ea-register-selector 'EA-REGISTER-EARLY)
	      (ea-extension-selector 'EA-EXTENSION-EARLY))
    (handle)))

(define (expand-descriptor descriptor environment)
  (let ((size (car descriptor))
	(expression (close-syntax (cadr descriptor) environment))
	(coercion-type
	 (if (pair? (cddr descriptor)) (caddr descriptor) 'UNSIGNED)))
    (case coercion-type
      ((UNSIGNED SIGNED SHIFT-NUMBER QUICK BFWIDTH SCALE-FACTOR)
       (values `(,(integer-syntaxer expression environment coercion-type size))
	       size #f #f))
      ((SHORT-LABEL)
       (values `(,(integer-syntaxer ``(,',(close-syntax '- environment)
					,,expression
					(,',(close-syntax '+ environment)
					 ,',(close-syntax '*PC* environment)
					 2))
				    environment
				    'SHORT-LABEL
				    size))
	       size #f #f))
      ((SOURCE-EA)
       (values `((,(close-syntax ea-mode-selector environment) ,expression)
		 (,(close-syntax ea-register-selector environment)
		  ,expression))
	       size
	       `((,(close-syntax ea-extension-selector environment)
		  ,expression)
		 ,(cadddr descriptor))
	       #f))
      ((DESTINATION-EA)
       (values `((,(close-syntax ea-mode-selector environment) ,expression)
		 (,(close-syntax ea-register-selector environment)
		  ,expression))
	       size
	       #f
	       `((,(close-syntax ea-extension-selector environment)
		  ,expression)
		 '())))
      ((DESTINATION-EA-REVERSED)
       (values `((,(close-syntax ea-register-selector environment) ,expression)
		 (,(close-syntax ea-mode-selector environment) ,expression))
	       size
	       #f
	       `((,(close-syntax ea-extension-selector environment)
		  ,expression)
		 '())))
      (else
       (error "Badly-formed descriptor:" descriptor)))))