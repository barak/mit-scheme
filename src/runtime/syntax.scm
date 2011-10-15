#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Syntactic Closures
;;;  Based on a design by Alan Bawden.

;;; This is a two-stage program: the first stage classifies input
;;; expressions into types, e.g. "definition", "lambda body",
;;; "expression", etc., and the second stage compiles those classified
;;; expressions ("items") into output code.  The classification stage
;;; permits discovery of internal definitions prior to code
;;; generation.  It also identifies keywords and variables, which
;;; allows a powerful form of syntactic binding to be implemented.

;;; See "Syntactic Closures", by Alan Bawden and Jonathan Rees, in
;;; Proceedings of the 1988 ACM Conference on Lisp and Functional
;;; Programming, page 86.

(declare (usual-integrations))

;;;; Top level

(define (syntax form environment)
  (syntax* (list form) environment))

(define (syntax* forms environment)
  (guarantee-list forms 'SYNTAX*)
  (guarantee-syntactic-environment environment 'SYNTAX*)
  (fluid-let ((*rename-database* (initial-rename-database)))
    (output/post-process-expression
     (if (syntactic-environment/top-level? environment)
	 (compile-body-item/top-level
	  (let ((environment
		 (make-top-level-syntactic-environment environment)))
	    (classify/body forms
			   environment
			   environment)))
	 (output/sequence (compile/expressions forms environment))))))

(define (compile/expression expression environment)
  (compile-item/expression (classify/expression expression environment)))

(define (compile/expressions expressions environment)
  (map (lambda (expression)
	 (compile/expression expression environment))
       expressions))

;;;; Syntactic closures

(define-record-type <syntactic-closure>
    (%make-syntactic-closure environment free-names form)
    syntactic-closure?
  (environment syntactic-closure/environment)
  (free-names syntactic-closure/free-names)
  (form syntactic-closure/form))

(define-guarantee syntactic-closure "syntactic closure")

(define (make-syntactic-closure environment free-names form)
  (guarantee-syntactic-environment environment 'MAKE-SYNTACTIC-CLOSURE)
  (guarantee-list-of-type free-names identifier?
			  "list of identifiers" 'MAKE-SYNTACTIC-CLOSURE)
  (if (or (memq form free-names)	;LOOKUP-IDENTIFIER assumes this.
	  (and (syntactic-closure? form)
	       (null? (syntactic-closure/free-names form))
	       (not (identifier? (syntactic-closure/form form))))
	  (not (or (syntactic-closure? form)
		   (pair? form)
		   (symbol? form))))
      form
      (%make-syntactic-closure environment free-names form)))

(define (strip-syntactic-closures object)
  (if (let loop ((object object))
	(if (pair? object)
	    (or (loop (car object))
		(loop (cdr object)))
	    (syntactic-closure? object)))
      (let loop ((object object))
	(if (pair? object)
	    (cons (loop (car object))
		  (loop (cdr object)))
	    (if (syntactic-closure? object)
		(loop (syntactic-closure/form object))
		object)))
      object))

(define (close-syntax form environment)
  (make-syntactic-closure environment '() form))

;;;; Identifiers

(define (identifier? object)
  (or (and (symbol? object)
	   ;; This makes `:keyword' objects be self-evaluating.
	   (not (keyword? object)))
      (synthetic-identifier? object)))

(define (synthetic-identifier? object)
  (and (syntactic-closure? object)
       (identifier? (syntactic-closure/form object))))

(define-guarantee identifier "identifier")
(define-guarantee synthetic-identifier "synthetic identifier")

(define (make-synthetic-identifier identifier)
  (close-syntax identifier null-syntactic-environment))

(define (identifier->symbol identifier)
  (or (let loop ((identifier identifier))
	(if (syntactic-closure? identifier)
	    (loop (syntactic-closure/form identifier))
	    (and (symbol? identifier)
		 identifier)))
      (error:not-identifier identifier 'IDENTIFIER->SYMBOL)))

(define (identifier=? environment-1 identifier-1 environment-2 identifier-2)
  (let ((item-1 (lookup-identifier identifier-1 environment-1))
	(item-2 (lookup-identifier identifier-2 environment-2)))
    (or (eq? item-1 item-2)
	;; This is necessary because an identifier that is not
	;; explicitly bound by an environment is mapped to a variable
	;; item, and the variable items are not cached.  Therefore
	;; two references to the same variable result in two
	;; different variable items.
	(and (variable-item? item-1)
	     (variable-item? item-2)
	     (eq? (variable-item/name item-1)
		  (variable-item/name item-2))))))

(define (lookup-identifier identifier environment)
  (let ((item (syntactic-environment/lookup environment identifier)))
    (cond (item
	   (if (reserved-name-item? item)
	       (syntax-error "Premature reference to reserved name:" identifier)
	       item))
	  ((symbol? identifier)
	   (make-variable-item identifier))
	  ((syntactic-closure? identifier)
	   (lookup-identifier (syntactic-closure/form identifier)
			      (syntactic-closure/environment identifier)))
	  (else
	   (error:not-identifier identifier 'LOOKUP-IDENTIFIER)))))

;;;; Utilities

(define (syntax-error . rest)
  (apply error rest))

(define (classifier->keyword classifier)
  (item->keyword (make-classifier-item classifier)))

(define (compiler->keyword compiler)
  (item->keyword (make-compiler-item compiler)))

(define (item->keyword item)
  (let ((environment
	 (make-internal-syntactic-environment null-syntactic-environment)))
    (syntactic-environment/define environment 'KEYWORD item)
    (close-syntax 'KEYWORD environment)))

(define (capture-syntactic-environment expander)
  `(,(classifier->keyword
      (lambda (form environment definition-environment)
	form				;ignore
	(classify/form (expander environment)
		       environment
		       definition-environment)))))

(define (reverse-syntactic-environments environment procedure)
  (capture-syntactic-environment
   (lambda (closing-environment)
     (close-syntax (procedure closing-environment) environment))))