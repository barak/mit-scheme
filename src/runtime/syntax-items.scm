#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Syntax items and compiler

(declare (usual-integrations))

(add-boot-deps! '(runtime predicate-dispatch)
		'(runtime compound-predicate)
		'(runtime comparator))

(define-deferred compile-item
  (cached-standard-predicate-dispatcher 'compile-item 1))

(define-deferred compile-expr-item
  (cached-standard-predicate-dispatcher 'compile-expr-item 1))

(define-deferred render-item
  (cached-standard-predicate-dispatcher 'render-item 1
    (lambda (item)
      item)))

(define (define-item-compiler predicate compiler #!optional expr-compiler)
  (add-boot-init!
   (lambda ()
     (define-predicate-dispatch-handler compile-item
       (list predicate)
       compiler)
     (if expr-compiler
	 (define-predicate-dispatch-handler compile-expr-item
	   (list predicate)
	   (if (default-object? expr-compiler) compiler expr-compiler))))))

(define (define-item-renderer predicate renderer)
  (add-boot-init!
   (lambda ()
     (define-predicate-dispatch-handler render-item
       (list predicate)
       renderer))))

(define (illegal-expression-compiler description)
  (let ((message (string description " may not be used as an expression:")))
    (lambda (item)
      (error message item))))

;;; These items can be stored in a syntactic environment.

;;; Variable items represent run-time variables.

(define (var-item id)
  (guarantee identifier? id 'var-item)
  (%var-item id))

(define-record-type <var-item>
    (%var-item id)
    var-item?
  (id var-item-id))

(define-print-method var-item?
  (standard-print-method 'var-item
    (lambda (item)
      (list (var-item-id item)))))

(define-item-compiler var-item?
  (lambda (item)
    (output/variable (var-item-id item))))

(define-item-renderer var-item?
  (lambda (item)
    `(var-item ,(var-item-id item))))

;;; Keyword items represent syntactic keywords.

(define-deferred keyword-item?
  (disjoin classifier-item? transformer-item?))

(define-item-compiler classifier-item?
  (illegal-expression-compiler "Classifier"))

(define-item-compiler transformer-item?
  (illegal-expression-compiler "Transformer"))

;;; Reserved name items do not represent any form, but instead are
;;; used to reserve a particular name in a syntactic environment.  If
;;; the classifier refers to a reserved name, a syntax error is
;;; signalled.  This is used in the implementation of LETREC-SYNTAX
;;; to signal a meaningful error when one of the <init>s refers to
;;; one of the names being bound.

(define-record-type <reserved-name-item>
    (reserved-name-item)
    reserved-name-item?)

(define-item-compiler reserved-name-item?
  (illegal-expression-compiler "Reserved name"))

;;; These items can't be stored in a syntactic environment.

;;; Definition items, whether top-level or internal, keyword or variable.

(define (syntax-defn-item ctx id value)
  (guarantee identifier? id 'syntax-defn-item)
  (guarantee defn-item-value? value 'syntax-defn-item)
  (%defn-item ctx id value #t))

(define (defn-item ctx id value)
  (guarantee identifier? id 'defn-item)
  (guarantee defn-item-value? value 'defn-item)
  (%defn-item ctx id value #f))

(define (defn-item-value? object)
  (not (reserved-name-item? object)))
(register-predicate! defn-item-value? 'defn-item-value)

(define-record-type <defn-item>
    (%defn-item ctx id value syntax?)
    defn-item?
  (ctx defn-item-ctx)
  (id defn-item-id)
  (value defn-item-value)
  (syntax? defn-item-syntax?))

(define-print-method defn-item?
  (standard-print-method 'defn-item
    (lambda (item)
      (list (defn-item-id item)
	    (defn-item-value item)))))

(define-item-compiler defn-item?
  (lambda (item)
    (let ((name (defn-item-id item))
	  (value (compile-expr-item (defn-item-value item))))
      (if (defn-item-syntax? item)
	  (output/syntax-definition name value)
	  (output/definition name value))))
  (illegal-expression-compiler "Definition"))

(define-item-renderer defn-item?
  (lambda (item)
    `(,(if (defn-item-syntax? item)
	   'define-syntax-item
	   'define-item)
      ,(defn-item-id item)
      ,(render-item (defn-item-value item)))))

;;; Sequence items.

(define (seq-item ctx elements)
  (let ((elements (flatten-items elements)))
    (if (and (pair? elements)
	     (null? (cdr elements)))
	(car elements)
	(%seq-item ctx elements))))

(define-record-type <seq-item>
    (%seq-item ctx elements)
    seq-item?
  (ctx seq-item-ctx)
  (elements seq-item-elements))

(define (flatten-items items)
  (append-map item->list items))

(define (item->list item)
  (if (seq-item? item)
      (seq-item-elements item)
      (list item)))

(define-item-compiler seq-item?
  (lambda (item)
    (output/sequence (map compile-item (seq-item-elements item))))
  (lambda (item)
    (output/sequence (map compile-expr-item (seq-item-elements item)))))

(define-item-renderer seq-item?
  (lambda (item)
    `(seq-item ,@(map render-item (seq-item-elements item)))))

(define (body-item ctx forms)
  (%body-item ctx (flatten-items forms)))

(define-record-type <body-item>
    (%body-item ctx forms)
    body-item?
  (ctx body-item-ctx)
  (forms body-item-forms))

(define-item-compiler body-item?
  (lambda (item)
    (output/body (map compile-item (body-item-forms item)))))

(define-item-renderer body-item?
  (lambda (item)
    `(body-item ,@(map render-item (body-item-forms item)))))

;;; Expression items represent any kind of expression other than a
;;; run-time variable or a sequence.

(define-record-type <expr-item>
    (expr-item ctx parts compiler renderer)
    expr-item?
  (ctx expr-item-ctx)
  (parts expr-item-parts)
  (compiler expr-item-compiler)
  (renderer expr-item-renderer))

(define-item-compiler expr-item?
  (lambda (item)
    (apply (expr-item-compiler item)
	   (map (lambda (part)
		  (compile-expr-item
		   (if (promise? part)
		       (force part)
		       part)))
		(expr-item-parts item)))))

(define-item-renderer expr-item?
  (lambda (item)
    (apply (expr-item-renderer item)
	   (map (lambda (part)
		  (if (promise? part)
		      (if (promise-forced? part)
			  (render-item (force part))
			  part)
		      (render-item part)))
		(expr-item-parts item)))))

(define (combination-item ctx operator operands)
  (expr-item ctx (cons operator operands)
    (lambda (operator . operands)
      (output/combination operator operands))
    (lambda (operator . operands)
      `(call-item ,operator ,@operands))))

(define (constant-item ctx datum)
  (expr-item ctx '()
    (lambda ()
      (output/constant datum))
    (lambda ()
      `(quote-item ,datum))))