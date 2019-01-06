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

;;;; Support for library tests

(define test-pathname
  (current-load-pathname))

(define test-directory
  (directory-pathname test-pathname))

(define-comparator library-export=? 'library-export=?)
(define-comparator library-import=? 'library-import=?)

(define (convert-import import)
  (case (car import)
    ((only except prefix)
     `(,(car import)
       ,(convert-import (cadr import))
       ,@(cddr import)))
    ((rename)
     `(,(car import)
       ,(convert-import (cadr import))
       ,@(map (lambda (p)
		(cons (car p) (cadr p)))
	      (cddr import))))
    (else
     `(library ,import))))

(define (convert-export export)
  (if (symbol? export)
      (make-library-export export)
      (make-library-export (cadr export) (caddr export))))

(define (convert-content content)
  (case (car content)
    ((include include-ci)
     (list
      (cons (car content)
	    (map (lambda (path)
		   (merge-pathnames path test-directory))
		 (cdr content)))))
    ((begin) (list content))
    (else (error "Unknown content:" content))))

(define ex1-imports
  '((foo mumble)
    (only (foo bletch) make-bletch bletch? bletch-thing)
    (prefix (foo grumble) grumble-)
    (except (foo quux) make-foo-quux)
    (rename (only (foo quux) make-foo-quux) (make-foo-quux create-foo-quux))))

(define ex1-exports
  '(make-bar
    bar?
    bar-v1
    bar-v2
    (rename set-bar-v1! bar-v1!)))

(define ex1-contents
  '((include "foo-bar-1")
    (include-ci "foo-bar-2")
    (begin
      (define-record-type <bar>
	  (make-bar v1 v2)
	  bar?
	(v1 bar-v1 set-bar-v1!)
	(v2 bar-v2)))))

(define ex1
  `(define-library (foo bar)
     (import ,@ex1-imports)
     (export ,@ex1-exports)
     ,@ex1-contents))

(define ex2-extra-imports
  '((scheme base)))

(define ex2-extra-exports
  '(<foo> foo?))

(define ex2-extra-contents
  '((begin
      (define-record-type <foo>
	  (make-foo)
	  foo?))))

(define ex2
  `(define-library (foo bar)
     (import ,@ex1-imports)
     (export ,@ex1-exports)
     (include-library-declarations "test-library-data/foo-foo")
     ,@ex1-contents))

(define (read-dependencies)
  (r7rs-source-libraries (read-r7rs-source dependencies-filename)))

(define dependencies-filename
  (->namestring
   (merge-pathnames "test-library-data/dependencies.scm"
		    test-directory)))

(define r7rs-example-filename
  (->namestring
   (merge-pathnames "test-library-data/r7rs-example.scm"
		    test-directory)))