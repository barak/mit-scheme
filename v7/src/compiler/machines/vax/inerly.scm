#| -*-Scheme-*-

$Id: inerly.scm,v 1.8 2001/12/19 21:39:30 cph Exp $

Copyright (c) 1987-1999, 2001 Massachusetts Institute of Technology

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

;;;; VAX Instruction Set Macros.  Early version

(declare (usual-integrations))

;;;; Instruction macros

(define early-instructions '())
(define early-transformers '())
(define early-ea-database '())

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-INSTRUCTION
  (lambda (opcode . patterns)
    `(SET! EARLY-INSTRUCTIONS
	   (CONS
	    (LIST ',opcode
		  ,@(map (lambda (pattern)
			   `(EARLY-PARSE-RULE
			     ',(car pattern)
			     (LAMBDA (PAT VARS)
			       (EARLY-MAKE-RULE
				PAT
				VARS
				(SCODE-QUOTE
				 (instruction->instruction-sequence
				  ,(parse-instruction (cadr pattern)
						      (cddr pattern)
						      true)))))))
			 patterns))
		 EARLY-INSTRUCTIONS))))

;;;; Transformers and utilities

(define (define-early-transformer name transformer)
  (set! early-transformers
	(cons (cons name transformer)
	      early-transformers)))

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-SYMBOL-TRANSFORMER
  (lambda (name . assoc)
    `(DEFINE-EARLY-TRANSFORMER ',name (MAKE-SYMBOL-TRANSFORMER ',assoc))))

;; *** Is this right? ***

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-TRANSFORMER
  (lambda (name value)
    `(DEFINE-EARLY-TRANSFORMER ',name ,value)))

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-EA-TRANSFORMER
  (lambda (name category type)
    `(DEFINE-EARLY-TRANSFORMER ',name
       (MAKE-EA-TRANSFORMER ',category ',type))))

(define (make-ea-transformer category type)
  type					; ignored
  (make-database-transformer
   (append-map! (lambda (rule)
		  (apply
		   (lambda (pattern variables categories expression)
		     (if (memq category categories)
			 (list (early-make-rule pattern variables expression))
			 '()))
		   rule))
		early-ea-database)))

;;;; Early effective address assembly.

;;; *** NOTE: If this format changes, insutl.scm must also be changed! ***

(syntax-table/define (->environment '(COMPILER))
		     'DEFINE-EA-DATABASE
  (lambda rules
    `(SET! EARLY-EA-DATABASE
       (LIST
	,@(map (lambda (rule)
		 (apply
		  (lambda (pattern categories . fields)
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
				  ',categories
				  ,(process-fields fields true))))))))
		  rule))
	       rules)))))

;; This is super hairy because of immediate operands!
;; The index 2 here is the argument number to MAKE-EFFECTIVE-ADDRESS.

(define ea-value-expander
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     if-not-expanded			; ignored
     (define (default)
       (if-expanded (scode/make-combination (scode/make-variable 'EA-VALUE)
					    (cdr operands))))

     (let ((operand (cadr operands))
	   (type (car operands)))
       (if (not (scode/combination? operand))
	   (default)
	   (scode/combination-components
	    operand
	    (lambda (operator operands)
	      (if (or (not (scode/variable? operator))
		      (not (eq? (scode/variable-name operator)
				'MAKE-EFFECTIVE-ADDRESS)))
		  (default)
		  (if-expanded
		   (scode/make-combination
		    (scode/make-lambda lambda-tag:let
				       '(*IMMEDIATE-TYPE*)
				       '()
				       false
				       '()
				       '((INTEGRATE *IMMEDIATE-TYPE*))
				       (scode/make-sequence
					(list (scode/make-variable '*IMMEDIATE-TYPE*)
					      (list-ref operands 2))))
		    (list type)))))))))))

#|
;; Not used currently

(define coerce-to-type-expander
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     (define (handle coercion name)
       (if-expanded
	(if (scode/constant? (car operands))
	    (scode/make-constant
	     (coercion (scode/constant-value (car operands))))
	    (scode/make-combination (scode/make-variable name)
				    (list (car operands))))))

     (if (not (scode/constant? (cadr operands)))
	 (if-not-expanded)
	 (case (scode/constant-value (cadr operands))
	   ((b) (handle coerce-8-bit-signed 'coerce-8-bit-signed))
	   ((w) (handle coerce-16-bit-signed 'coerce-16-bit-signed))
	   ((l) (handle coerce-32-bit-signed 'coerce-32-bit-signed))
	   (else (if-not-expanded)))))))
|#