#| -*-Scheme-*-

$Id$

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

(define-syntax define-ea-transformer
  (non-hygienic-macro-transformer
   (lambda (name . restrictions)
     `(DEFINE-EARLY-TRANSFORMER ',name
	(APPLY MAKE-EA-TRANSFORMER ',restrictions)))))

(define-syntax define-symbol-transformer
  (non-hygienic-macro-transformer
   (lambda (name . assoc)
     `(DEFINE-EARLY-TRANSFORMER ',name (MAKE-SYMBOL-TRANSFORMER ',assoc)))))

(define-syntax define-reg-list-transformer
  (non-hygienic-macro-transformer
   (lambda (name . assoc)
     `(DEFINE-EARLY-TRANSFORMER ',name
	(MAKE-BIT-MASK-TRANSFORMER 16 ',assoc)))))

;;;; Instruction and addressing mode macros

(define-syntax define-instruction
  (non-hygienic-macro-transformer
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
						       #t)))))))
			  patterns))
		  EARLY-INSTRUCTIONS)))))

(define-syntax extension-word
  (non-hygienic-macro-transformer
   (lambda descriptors
     (expand-descriptors descriptors
       (lambda (instruction size source destination)
	 (if (or source destination)
	     (error "EXTENSION-WORD: Source or destination used"))
	 (if (not (zero? (remainder size 16)))
	     (error "EXTENSION-WORD: Extensions must be 16 bit multiples"
		    size))
	 (optimize-group-syntax instruction true))))))

(define-syntax variable-extension
  (non-hygienic-macro-transformer
   (lambda (binding . clauses)
     (variable-width-expression-syntaxer
      (car binding)
      (cadr binding)
      (map (lambda (clause)
	     `((LIST ,(caddr clause))
	       ,(cadr clause)		; Size
	       ,@(car clause)))		; Range
	   clauses)))))

;;;; Early effective address assembly.

;;; *** NOTE: If this format changes, insutl.scm must also be changed! ***

(define-syntax define-ea-database
  (non-hygienic-macro-transformer
   (lambda rules
     `(SET! EARLY-EA-DATABASE
	    (LIST
	     ,@(map (lambda (rule)
		      (if (null? (cdddr rule))
			  (apply make-position-dependent-early rule)
			  (apply make-position-independent-early rule)))
		    rules))))))

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