#| -*-Scheme-*-

$Id: object.scm,v 1.12 2001/08/18 04:48:44 cph Exp $

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Package Model Data Structures

(declare (usual-integrations))

(define-structure
    (package-description
     (type vector)
     (named
      (string->symbol "#[(cross-reference)package-description]"))
     (constructor make-package-description (name parent))
     (conc-name package-description/))
  (name #f read-only #t)
  (file-cases '())
  (parent #f read-only #t)
  (initialization #f)
  (finalization #f)
  (exports '())
  (imports '()))

(define-structure
    (pmodel
     (type vector)
     (named (string->symbol "#[(cross-reference)pmodel]"))
     (conc-name pmodel/))
  (root-package #f read-only #t)
  (primitive-package #f read-only #t)
  (packages #f read-only #t)
  (extra-packages #f read-only #t)
  (pathname #f read-only #t))

(define-structure
    (package
     (type vector)
     (named (string->symbol "#[(cross-reference)package]"))
     (constructor make-package (name parent))
     (conc-name package/)
     (print-procedure
      (standard-unparser-method 'PACKAGE
	(lambda (package port)
	  (write-char #\space port)
	  (write (package/name package) port)))))

  (name #f read-only #t)
  (file-cases '())
  (files '())
  (initialization #f)
  (finalization #f)
  parent
  (children '())
  (bindings (make-rb-tree eq? symbol<?) read-only #t)
  (references (make-rb-tree eq? symbol<?) read-only #t))

(define-integrable (package/n-files package)
  (length (package/files package)))

(define-integrable (package/root? package)
  (null? (package/name package)))

(define-integrable (package/find-binding package name)
  (rb-tree/lookup (package/bindings package) name #f))

(define-integrable (package/sorted-bindings package)
  (rb-tree/datum-list (package/bindings package)))

(define-integrable (package/sorted-references package)
  (rb-tree/datum-list (package/references package)))

(define-integrable (file-case/type file-case)
  (car file-case))

(define-integrable (file-case/clauses file-case)
  (cdr file-case))

(define-integrable (file-case-clause/keys clause)
  (car clause))

(define-integrable (file-case-clause/files clause)
  (cdr clause))

(define-structure
    (binding
     (type vector)
     (named (string->symbol "#[(cross-reference)binding]"))
     (constructor %make-binding (package name value-cell new?))
     (conc-name binding/)
     (print-procedure
      (standard-unparser-method 'BINDING
	(lambda (binding port)
	  (write-char #\space port)
	  (write (binding/name binding) port)
	  (write-char #\space port)
	  (write (package/name (binding/package binding)) port)))))
  (package #f read-only #t)
  (name #f read-only #t)
  (value-cell #f read-only #t)
  (new? #f)
  (references '())
  (links '()))

(define (make-binding package name value-cell new?)
  (let ((binding (%make-binding package name value-cell new?)))
    (set-value-cell/bindings!
     value-cell
     (cons binding (value-cell/bindings value-cell)))
    binding))

(define-integrable (binding/expressions binding)
  (value-cell/expressions (binding/value-cell binding)))

(define-integrable (binding/source-binding binding)
  (value-cell/source-binding (binding/value-cell binding)))

(define (binding/internal? binding)
  (eq? binding (binding/source-binding binding)))

(define-structure
    (value-cell
     (type vector)
     (named (string->symbol "#[(cross-reference)value-cell]"))
     (constructor make-value-cell ())
     (conc-name value-cell/))
  (bindings '())
  (expressions '())
  (source-binding #f))

(define-structure
    (link
     (type vector)
     (named (string->symbol "#[(cross-reference)link]"))
     (constructor %make-link (source destination new?))
     (conc-name link/))
  (source #f read-only #t)
  (destination #f read-only #t)
  (new? #f read-only #t))

(define (make-link source-binding destination-binding new?)
  (let ((link (%make-link source-binding destination-binding new?)))
    (set-binding/links! source-binding
			(cons link (binding/links source-binding)))
    link))

(define-structure
    (expression
     (type vector)
     (named (string->symbol "#[(cross-reference)expression]"))
     (constructor make-expression (package file type))
     (conc-name expression/))
  (package #f read-only #t)
  (file #f read-only #t)
  (type #f read-only #t)
  (references '())
  (value-cell #f))

(define-structure
    (reference
     (type vector)
     (named (string->symbol "#[(cross-reference)reference]"))
     (constructor %make-reference (package name))
     (conc-name reference/)
     (print-procedure
      (standard-unparser-method 'REFERENCE
	(lambda (reference port)
	  (write-char #\space port)
	  (write (reference/name reference) port)
	  (write-char #\space port)
	  (write (package/name (reference/package reference)) port)))))
  (package #f read-only #t)
  (name #f read-only #t)
  (expressions '())
  (binding #f))

(define (symbol-list=? x y)
  (if (pair? x)
      (and (pair? y)
	   (eq? (car x) (car y))
	   (symbol-list=? (cdr x) (cdr y)))
      (not (pair? y))))

(define (symbol-list<? x y)
  (and (pair? y)
       (or (not (pair? x))
	   (symbol<? (car x) (car y))
	   (and (eq? (car x) (car y))
		(symbol-list<? (cdr x) (cdr y))))))

(define (package<? x y)
  (symbol-list<? (package/name x) (package/name y)))

(define (binding<? x y)
  (symbol<? (binding/name x) (binding/name y)))

(define (reference<? x y)
  (symbol<? (reference/name x) (reference/name y)))