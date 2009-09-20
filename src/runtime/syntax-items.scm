#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Syntax Items

(declare (usual-integrations))

;;; Reserved name items do not represent any form, but instead are
;;; used to reserve a particular name in a syntactic environment.  If
;;; the classifier refers to a reserved name, a syntax error is
;;; signalled.  This is used in the implementation of LETREC-SYNTAX
;;; to signal a meaningful error when one of the <init>s refers to
;;; one of the names being bound.

(define-record-type <reserved-name-item>
    (make-reserved-name-item)
    reserved-name-item?)

;;; Keyword items represent macro keywords.  There are several flavors
;;; of keyword item.

(define-record-type <classifier-item>
    (make-classifier-item classifier)
    classifier-item?
  (classifier classifier-item/classifier))

(define-record-type <compiler-item>
    (make-compiler-item compiler)
    compiler-item?
  (compiler compiler-item/compiler))

(define-record-type <expander-item>
    (make-expander-item expander)
    expander-item?
  (expander expander-item/expander))

(define-record-type <keyword-value-item>
    (make-keyword-value-item item expression)
    keyword-value-item?
  (item keyword-value-item/item)
  (expression keyword-value-item/expression))

(define (keyword-item? item)
  (or (classifier-item? item)
      (compiler-item? item)
      (expander-item? item)
      (keyword-value-item? item)))

;;; Variable items represent run-time variables.

(define-record-type <variable-item>
    (make-variable-item name)
    variable-item?
  (name variable-item/name))

;;; Expression items represent any kind of expression other than a
;;; run-time variable or a sequence.

(define-record-type <expression-item>
    (make-expression-item compiler)
    expression-item?
  (compiler expression-item/compiler))

;;; Body items represent sequences (e.g. BEGIN).

(define-record-type <body-item>
    (make-body-item components)
    body-item?
  (components body-item/components))

(define (flatten-body-items items)
  (append-map item->list items))

(define (item->list item)
  (if (body-item? item)
      (flatten-body-items (body-item/components item))
      (list item)))

;;; Declaration items represent block-scoped declarations that are to
;;; be passed through to the compiler.

(define-record-type <declaration-item>
    (make-declaration-item get-text)
    declaration-item?
  (get-text declaration-item/get-text))

(define (declaration-item/text item)
  ((declaration-item/get-text item)))

;;; Binding items represent definitions, whether top-level or
;;; internal, keyword or variable.  Null binding items are for
;;; definitions that don't emit code.

(define-record-type <binding-item>
    (make-binding-item name value)
    binding-item?
  (name binding-item/name)
  (value binding-item/value))

(define-record-type <null-binding-item>
    (make-null-binding-item)
    null-binding-item?)