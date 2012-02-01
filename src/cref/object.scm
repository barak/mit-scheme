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

;;;; Package Model Data Structures

(declare (usual-integrations))

(define-structure (package-description
		   (constructor make-package-description (name parent))
		   (conc-name package-description/))
  (name #f read-only #t)
  (file-cases '())
  (parent #f read-only #t)
  (initializations '())
  (finalizations '())
  (exports '())
  (imports '()))

(define-structure (pmodel (conc-name pmodel/))
  (root-package #f read-only #t)
  (primitive-package #f read-only #t)
  (packages #f read-only #t)
  (extra-packages #f read-only #t)
  (loads #f read-only #t)
  (pathname #f read-only #t))

(define-structure (package
		   (constructor make-package (name parent))
		   (conc-name package/)
		   (print-procedure
		    (standard-unparser-method 'PACKAGE
		      (lambda (package port)
			(write-char #\space port)
			(write (package/name package) port)))))
  (name #f read-only #t)
  (files '())
  parent
  (children '())
  (bindings '())
  (references '())
  (links '()))

(define-integrable (package/n-files package)
  (length (package/files package)))

(define-integrable (package/root? package)
  (null? (package/name package)))

(define-integrable (package/find-reference package name)
  (find-matching-item (package/references package)
		      (lambda (ref) (eq? (reference/name ref) name))))

(define-integrable (package/put-reference! package reference)
  (set-package/references! package
			   (cons reference (package/references package))))

(define-integrable (package/find-binding package name)
  (find-matching-item (package/bindings package)
		      (lambda (ref) (eq? (binding/name ref) name))))

(define-integrable (package/put-binding! package binding)
  (set-package/bindings! package
			 (cons binding (package/bindings package))))

(define-integrable (file-case/type file-case)
  (car file-case))

(define-integrable (file-case/clauses file-case)
  (cdr file-case))

(define-integrable (file-case-clause/keys clause)
  (car clause))

(define-integrable (file-case-clause/files clause)
  (cdr clause))

(define-structure (package-load
		   (conc-name package-load/))
  (package #f read-only #t)
  (file-cases '())
  (initializations #f)
  (finalizations #f))

(define-structure (binding
		   (constructor %make-binding (package name value-cell new?))
		   (conc-name binding/)
		   (print-procedure
		    (standard-unparser-method 'BINDING
		      (lambda (binding port)
			(write-char #\space port)
			(write (binding/name binding) port)
			(write-char #\space port)
			(write (package/name (binding/package binding))
			       port)))))
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

(define-structure (value-cell
		   (constructor make-value-cell ())
		   (conc-name value-cell/))
  (bindings '())
  (expressions '())
  (source-binding #f))

(define-structure (link
		   (constructor %make-link (source destination owner new?))
		   (conc-name link/))
  (source #f read-only #t)
  (destination #f read-only #t)
  (owner #f read-only #t)
  (new? #f read-only #t))

(define (make-link source-binding destination-binding owner new?)
  (let ((link (%make-link source-binding destination-binding owner new?)))
    (set-binding/links! source-binding
			(cons link (binding/links source-binding)))
    (set-package/links! owner (cons link (package/links owner)))
    link))

(define-structure (expression
		   (constructor make-expression (package file type))
		   (conc-name expression/))
  (package #f read-only #t)
  (file #f read-only #t)
  (type #f read-only #t)
  (references '())
  (value-cell #f))

(define-structure (reference
		   (constructor %make-reference (package name))
		   (conc-name reference/)
		   (print-procedure
		    (standard-unparser-method 'REFERENCE
		      (lambda (reference port)
			(write-char #\space port)
			(write (reference/name reference) port)
			(write-char #\space port)
			(write (package/name (reference/package reference))
			       port)))))
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

(declare (integrate-operator name->string))
(define (name->string name)
  (if (interned-symbol? name)
      (symbol-name name)
      (write-to-string name)))

(define-integrable (name<? x y)
  (string<? (name->string x) (name->string y)))

(define-integrable (binding<? x y)
  (name<? (binding/name x) (binding/name y)))

(define-integrable (reference<? x y)
  (name<? (reference/name x) (reference/name y)))