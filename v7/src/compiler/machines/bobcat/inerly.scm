#| -*-Scheme-*-

$Id: inerly.scm,v 1.10 2001/12/20 02:37:21 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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

;;;; 68000 Instruction Set Macros.  Early version

(declare (usual-integrations))

;;;; Transformers and utilities

(define early-ea-database)

(define (define-early-transformer name transformer)
  (set! early-transformers
	(cons (cons name transformer)
	      early-transformers))
  unspecific)

(define (make-ea-transformer #!optional modes keywords)
  (make-database-transformer
    (append-map! (lambda (rule)
		   (apply
		    (lambda (pattern variables categories expression)
		      (if (and (or (default-object? modes)
				   (eq-subset? modes categories))
			       (or (default-object? keywords)
				   (not (memq (car pattern) keywords))))
			  (list (early-make-rule pattern variables expression))
			  '()))
		    rule))
		 early-ea-database)))

(define (eq-subset? s1 s2)
  (or (null? s1)
      (and (memq (car s1) s2)
	   (eq-subset? (cdr s1) s2))))

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-EA-TRANSFORMER
  (lambda (name . restrictions)
    `(DEFINE-EARLY-TRANSFORMER ',name
       (APPLY MAKE-EA-TRANSFORMER ',restrictions))))

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-SYMBOL-TRANSFORMER
  (lambda (name . assoc)
    `(DEFINE-EARLY-TRANSFORMER ',name (MAKE-SYMBOL-TRANSFORMER ',assoc))))

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-REG-LIST-TRANSFORMER
  (lambda (name . assoc)
    `(DEFINE-EARLY-TRANSFORMER ',name (MAKE-BIT-MASK-TRANSFORMER 16 ',assoc))))

;;;; Instruction and addressing mode macros

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-INSTRUCTION
  (lambda (opcode . patterns)
    `(SET! EARLY-INSTRUCTIONS
	   (CONS
	    (LIST ',opcode
		  ,@(map (lambda (pattern)
			   `(early-parse-rule
			     ',(car pattern)
			     (lambda (pat vars)
			       (early-make-rule
				pat
				vars
				(scode-quote
				 (instruction->instruction-sequence
				  ,(parse-instruction (cadr pattern)
						      (cddr pattern)
						      true)))))))
			 patterns))
		 EARLY-INSTRUCTIONS))))

(syntax-table/define (->environment '(COMPILER))
		     'EXTENSION-WORD
  (lambda descriptors
    (expand-descriptors descriptors
      (lambda (instruction size source destination)
	(if (or source destination)
	    (error "EXTENSION-WORD: Source or destination used"))
	(if (not (zero? (remainder size 16)))
	    (error "EXTENSION-WORD: Extensions must be 16 bit multiples" size))
	(optimize-group-syntax instruction true)))))

(syntax-table/define (->environment '(COMPILER))
		     'VARIABLE-EXTENSION
  (lambda (binding . clauses)
    (variable-width-expression-syntaxer
     (car binding)
     (cadr binding)
     (map  (lambda (clause)
	     `((LIST ,(caddr clause))
	       ,(cadr clause)		; Size
	       ,@(car clause)))		; Range
	  clauses))))

;;;; Early effective address assembly.

;;; *** NOTE: If this format changes, insutl.scm must also be changed! ***

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-EA-DATABASE
  (lambda rules
    `(SET! EARLY-EA-DATABASE
	   (LIST
	    ,@(map (lambda (rule)
		     (if (null? (cdddr rule))
			 (apply make-position-dependent-early rule)
			 (apply make-position-independent-early rule)))
		   rules)))))

(define (make-ea-selector-expander late-name index)
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     if-not-expanded
     (let ((default
	     (lambda ()
	       (if-expanded
		(scode/make-combination
		 (scode/make-variable late-name)
		 operands))))
	   (operand (car operands)))
       (if (not (scode/combination? operand))
	   (default)
	   (scode/combination-components operand
	    (lambda (operator operands)
	      (if (or (not (scode/variable? operator))
		      (not (eq? (scode/variable-name operator)
				'MAKE-EFFECTIVE-ADDRESS)))
		  (default)
		  (if-expanded (list-ref operands index))))))))))

;; The indices here are the argument number to MAKE-EFFECTIVE-ADDRESS.
(define ea-keyword-expander (make-ea-selector-expander 'EA-KEYWORD 0))
(define ea-mode-expander (make-ea-selector-expander 'EA-MODE 1))
(define ea-register-expander (make-ea-selector-expander 'EA-REGISTER 2))
(define ea-extension-expander (make-ea-selector-expander 'EA-EXTENSION 3))
(define ea-categories-expander (make-ea-selector-expander 'EA-CATEGORIES 4))

;;;; Utilities

(define (make-position-independent-early pattern categories mode register
					 . extension)
  (let ((keyword (car pattern)))
    `(EARLY-PARSE-RULE
      ',pattern
      (LAMBDA (PAT VARS)
	(LIST PAT
	      VARS
	      ',categories
	      (SCODE-QUOTE
	       (MAKE-EFFECTIVE-ADDRESS
		',keyword
		,(integer-syntaxer mode 'UNSIGNED 3)
		,(integer-syntaxer register 'UNSIGNED 3)
		(LAMBDA (IMMEDIATE-SIZE INSTRUCTION-TAIL)
		  (DECLARE (INTEGRATE IMMEDIATE-SIZE INSTRUCTION-TAIL))
		  IMMEDIATE-SIZE	;ignore if not referenced
		  ,(if (null? extension)
		       'INSTRUCTION-TAIL
		       `(CONS-SYNTAX ,(car extension) INSTRUCTION-TAIL)))
		',categories)))))))

(define (make-position-dependent-early pattern categories code-list)
  (let ((keyword (car pattern))
	(code (cdr code-list)))
    (let ((name (car code))
	  (mode (cadr code))
	  (register (caddr code))
	  (extension (cadddr code)))
      `(EARLY-PARSE-RULE
	',pattern
	(LAMBDA (PAT VARS)
	  (LIST PAT
		VARS
		',categories
		(SCODE-QUOTE
		 (LET ((,name (GENERATE-LABEL 'MARK)))
		   (MAKE-EFFECTIVE-ADDRESS
		    ',keyword
		    ,(process-ea-field mode)
		    ,(process-ea-field register)
		    (LAMBDA (IMMEDIATE-SIZE INSTRUCTION-TAIL)
		      (DECLARE (INTEGRATE IMMEDIATE-SIZE INSTRUCTION-TAIL))
		      IMMEDIATE-SIZE	;ignore if not referenced
		      ,(if (null? extension)
			   'INSTRUCTION-TAIL
			   `(CONS (LIST 'LABEL ,name)
				  (CONS-SYNTAX ,extension INSTRUCTION-TAIL))))
		    ',categories)))))))))