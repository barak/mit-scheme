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

;;;; Syntax Items

(declare (usual-integrations))

;;; These items can be stored in a syntactic environment.

;;; Keyword items represent macro keywords.  There are several flavors
;;; of keyword item.

(define-record-type <classifier-item>
    (classifier-item impl)
    classifier-item?
  (impl classifier-item-impl))

(define-record-type <compiler-item>
    (compiler-item impl)
    compiler-item?
  (impl compiler-item-impl))

(define-record-type <expander-item>
    (expander-item impl)
    expander-item?
  (impl expander-item-impl))

(define-record-type <keyword-value-item>
    (keyword-value-item keyword expr)
    keyword-value-item?
  (keyword keyword-value-item-keyword)
  (expr keyword-value-item-expr))

(define (keyword-item? object)
  (or (classifier-item? object)
      (compiler-item? object)
      (expander-item? object)
      (keyword-value-item? object)))

(register-predicate! keyword-item? 'keyword-item)
(set-predicate<=! classifier-item? keyword-item?)
(set-predicate<=! compiler-item? keyword-item?)
(set-predicate<=! expander-item? keyword-item?)
(set-predicate<=! keyword-value-item? keyword-item?)

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

(define (defn-item id value)
  (guarantee identifier? id 'defn-item)
  (guarantee defn-item-value? value 'defn-item)
  (%defn-item id value))

(define (defn-item-value? object)
  (not (or (reserved-name-item? object)
	   (decl-item? object))))
(register-predicate! defn-item-value? 'defn-item-value)

(define-record-type <defn-item>
    (%defn-item id value)
    defn-item?
  (id defn-item-id)
  (value defn-item-value))

(define-unparser-method defn-item?
  (simple-unparser-method 'defn-item
    (lambda (item)
      (list (defn-item-id item)
	    (defn-item-value item)))))

;;; Sequence items.

(define (seq-item elements)
  (%seq-item (flatten-items elements)))

(define-record-type <seq-item>
    (%seq-item elements)
    seq-item?
  (elements seq-item-elements))

(define (extract-declarations-from-body seq-item)
  (partition decl-item? (seq-item-elements seq-item)))

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

;;; Declaration items represent block-scoped declarations that are to
;;; be passed through to the compiler.

(define-record-type <decl-item>
    (decl-item text-getter)
    decl-item?
  (text-getter decl-item-text-getter))

(define (decl-item-text item)
  ((decl-item-text-getter item)))