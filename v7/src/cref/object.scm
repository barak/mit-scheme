#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/cref/object.scm,v 1.1 1988/06/13 12:38:28 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Package Model Data Structures

(declare (usual-integrations))

(define-structure (package-description (constructor make-package-description)
				       (conc-name package-description/))
  (name false read-only true)
  (file-cases false read-only true)
  (parent false read-only true)
  (initialization false read-only true)
  (exports false read-only true)
  (imports false read-only true))

(define-structure (pmodel (conc-name pmodel/))
  (root-package false read-only true)
  (primitive-package false read-only true)
  (packages false read-only true)
  (extra-packages false read-only true)
  (default-pathname false read-only true))

(define-structure (package
		   (constructor %make-package
				(name file-cases files initialization parent))
		   (conc-name package/))
  (name false read-only true)
  (file-cases false read-only true)
  (files false read-only true)
  (initialization false read-only true)
  parent
  (children '())
  (bindings (make-btree symbol<? binding/name) read-only true)
  (references (make-btree symbol<? reference/name) read-only true))

(define (make-package name file-cases initialization parent)
  (let ((files
	 (mapcan (lambda (file-case)
		   (mapcan (lambda (clause) (list-copy (cdr clause)))
			   (cdr file-case)))
		 file-cases)))
    (%make-package name
		   file-cases
		   files
		   initialization
		   parent)))

(define-integrable (package/n-files package)
  (length (package/files package)))

(define-integrable (package/root? package)
  (null? (package/name package)))

(define (package/find-binding package name)
  (btree-lookup (package/bindings package) name
    identity-procedure
    (lambda (name) name false)))

(define-integrable (file-case/type file-case)
  (car file-case))

(define-integrable (file-case/clauses file-case)
  (cdr file-case))

(define-integrable (file-case-clause/keys clause)
  (car clause))

(define-integrable (file-case-clause/files clause)
  (cdr clause))

(define-structure (binding (constructor %make-binding
					(package name value-cell))
			   (conc-name binding/))
  (package false read-only true)
  (name false read-only true)
  (value-cell false read-only true)
  (references '())
  (links '()))

(define (make-binding package name value-cell)
  (let ((binding (%make-binding package name value-cell)))
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

(define-structure (value-cell (constructor make-value-cell ())
			      (conc-name value-cell/))
  (bindings '())
  (expressions '())
  (source-binding false))

(define-structure (link (constructor %make-link)
			(conc-name link/))
  (source false read-only true)
  (destination false read-only true))

(define (make-link source-binding destination-binding)
  (let ((link (%make-link source-binding destination-binding)))
    (set-binding/links! source-binding
			(cons link (binding/links source-binding)))
    link))

(define-structure (expression (constructor make-expression (package file type))
			      (conc-name expression/))
  (package false read-only true)
  (file false read-only true)
  (type false read-only true)
  (references '())
  (value-cell false))

(define-structure (reference (constructor %make-reference (package name))
			     (conc-name reference/))
  (package false read-only true)
  (name false read-only true)
  (expressions '())
  (binding false))

(define-integrable (symbol<? x y)
  (string<? (symbol->string x) (symbol->string y)))

(define (symbol-list=? x y)
  (if (null? x)
      (null? y)
      (and (not (null? y))
	   (eq? (car x) (car y))
	   (symbol-list=? (cdr x) (cdr y)))))

(define (symbol-list<? x y)
  (and (not (null? y))
       (if (or (null? x)
	       (symbol<? (car x) (car y)))
	   true
	   (and (eq? (car x) (car y))
		(symbol-list<? (cdr x) (cdr y))))))

(define (package<? x y)
  (symbol-list<? (package/name x) (package/name y)))

(define (binding<? x y)
  (symbol<? (binding/name x) (binding/name y)))

(define (reference<? x y)
  (symbol<? (reference/name x) (reference/name y)))