#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;; These items can be stored in a syntactic environment.

;;; Variable items represent run-time variables.

(define (var-item id)
  (guarantee identifier? id 'var-item)
  (%var-item id))

(define-record-type <var-item>
    (%var-item id)
    var-item?
  (id var-item-id))

(define-unparser-method var-item?
  (simple-unparser-method 'var-item
    (lambda (item)
      (list (var-item-id item)))))

;;; Reserved name items do not represent any form, but instead are
;;; used to reserve a particular name in a syntactic environment.  If
;;; the classifier refers to a reserved name, a syntax error is
;;; signalled.  This is used in the implementation of LETREC-SYNTAX
;;; to signal a meaningful error when one of the <init>s refers to
;;; one of the names being bound.

(define-record-type <reserved-name-item>
    (reserved-name-item)
    reserved-name-item?)

;;; These items can't be stored in a syntactic environment.

;;; Definition items, whether top-level or internal, keyword or variable.

(define (syntax-defn-item id value)
  (guarantee identifier? id 'syntax-defn-item)
  (guarantee defn-item-value? value 'syntax-defn-item)
  (%defn-item id value #t))

(define (defn-item id value)
  (guarantee identifier? id 'defn-item)
  (guarantee defn-item-value? value 'defn-item)
  (%defn-item id value #f))

(define (defn-item-value? object)
  (not (reserved-name-item? object)))
(register-predicate! defn-item-value? 'defn-item-value)

(define-record-type <defn-item>
    (%defn-item id value syntax?)
    defn-item?
  (id defn-item-id)
  (value defn-item-value)
  (syntax? defn-item-syntax?))

(define-unparser-method defn-item?
  (simple-unparser-method 'defn-item
    (lambda (item)
      (list (defn-item-id item)
	    (defn-item-value item)))))

;;; Sequence items.

(define (seq-item elements)
  (let ((elements (flatten-items elements)))
    (if (and (pair? elements)
	     (null? (cdr elements)))
	(car elements)
	(%seq-item elements))))

(define-record-type <seq-item>
    (%seq-item elements)
    seq-item?
  (elements seq-item-elements))

(define (flatten-items items)
  (append-map item->list items))

(define (item->list item)
  (if (seq-item? item)
      (seq-item-elements item)
      (list item)))

;;; Expression items represent any kind of expression other than a
;;; run-time variable or a sequence.

(define-record-type <expr-item>
    (expr-item compiler)
    expr-item?
  (compiler expr-item-compiler))

;;;; Specific expression items

(define (combination-item operator operands)
  (expr-item
   (lambda ()
     (output/combination (compile-expr-item operator)
			 (map compile-expr-item operands)))))

(define (constant-item datum)
  (expr-item
   (lambda ()
     (output/constant datum))))

(define (lambda-item name bvl classify-body)
  (expr-item
   (lambda ()
     (output/lambda name bvl (compile-item (classify-body))))))

(define (let-item names value-items body-item)
  (expr-item
   (lambda ()
     (output/let names
		 (map compile-expr-item value-items)
		 (compile-item body-item)))))

(define (body-item items)
  (expr-item
   (lambda ()
     (output/body (map compile-item (flatten-items items))))))

(define (if-item predicate consequent alternative)
  (expr-item
   (lambda ()
     (output/conditional (compile-expr-item predicate)
			 (compile-expr-item consequent)
			 (compile-expr-item alternative)))))

(define (quoted-id-item var-item)
  (expr-item
   (lambda ()
     (output/quoted-identifier (var-item-id var-item)))))

(define (assignment-item id rhs-item)
  (expr-item
   (lambda ()
     (output/assignment id (compile-expr-item rhs-item)))))

(define (access-assignment-item name env-item rhs-item)
  (expr-item
   (lambda ()
     (output/access-assignment name
			       (compile-expr-item env-item)
			       (compile-expr-item rhs-item)))))

(define (delay-item classify)
  (expr-item
   (lambda ()
     (output/delay (compile-expr-item (classify))))))

(define (or-item items)
  (expr-item
   (lambda ()
     (output/disjunction (map compile-expr-item items)))))

(define (decl-item classify)
  (expr-item
   (lambda ()
     (output/declaration (classify)))))

(define (the-environment-item)
  (expr-item output/the-environment))

(define (unspecific-item)
  (expr-item output/unspecific))

(define (unassigned-item)
  (expr-item output/unassigned))

;;;; Compiler

(define compile-item)
(define compile-expr-item)
(add-boot-init!
 (lambda ()
   (set! compile-item
	 (cached-standard-predicate-dispatcher 'compile-item 1))
   (set! compile-expr-item
	 (cached-standard-predicate-dispatcher 'compile-expr-item 1))
   (run-deferred-boot-actions 'define-item-compiler)))

(define (define-item-compiler predicate compiler #!optional expr-compiler)
  (defer-boot-action 'define-item-compiler
    (lambda ()
      (define-predicate-dispatch-handler compile-item
	(list predicate)
	compiler)
      (if expr-compiler
	  (define-predicate-dispatch-handler compile-expr-item
	    (list predicate)
	    (if (default-object? expr-compiler) compiler expr-compiler))))))

(define (define-expr-item-compiler predicate compiler)
  (defer-boot-action 'define-item-compiler
    (lambda ()
      (define-predicate-dispatch-handler compile-expr-item
	(list predicate)
	compiler))))

(define-item-compiler var-item?
  (lambda (item)
    (output/variable (var-item-id item))))

(define-item-compiler expr-item?
  (lambda (item)
    ((expr-item-compiler item))))

(define-item-compiler seq-item?
  (lambda (item)
    (output/sequence (map compile-item (seq-item-elements item))))
  (lambda (item)
    (output/sequence (map compile-expr-item (seq-item-elements item)))))

(define-item-compiler defn-item?
  (lambda (item)
    (let ((name (defn-item-id item))
	  (value (compile-expr-item (defn-item-value item))))
      (if (defn-item-syntax? item)
	  (output/syntax-definition name value)
	  (output/definition name value))))
  #f)

(define (illegal-expression-compiler description)
  (let ((message (string description " may not be used as an expression:")))
    (lambda (item)
      (error message item))))

(define-expr-item-compiler defn-item?
  (illegal-expression-compiler "Definition"))

(define-item-compiler reserved-name-item?
  (illegal-expression-compiler "Reserved name"))

(define-item-compiler keyword-item?
  (illegal-expression-compiler "Syntactic keyword"))