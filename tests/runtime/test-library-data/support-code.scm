#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;;; Support for library tests

(define test-pathname
  (current-load-pathname))

(define test-directory
  (directory-pathname test-pathname))

(define-comparator library-ixport=? 'library-ixport=?)

(define (convert-export library export)
  (if (symbol? export)
      (make-library-ixport library export)
      (make-library-ixport library (cadr export) (caddr export))))

(define (convert-exports library exports)
  (map (lambda (name)
	 (convert-export library name))
       exports))

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
  '((scheme base)
    (foo mumble)
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
  '((include "test-library-data/foo-bar-1.scm")
    (include-ci "test-library-data/foo-bar-2.scm")
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
     (include-library-declarations "test-library-data/foo-foo.scm")
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

(define srfi-140-example-filename
  (->namestring
   (merge-pathnames "test-library-data/srfi-140-example.scm"
		    test-directory)))

(define private-exports-example-filename
  (->namestring
   (merge-pathnames "test-library-data/private-exports-example.scm"
		    test-directory)))

(define (exports-of name db)
  (library-exports (registered-library name db)))

(define (ixport-union . ixports)
  (apply lset-union library-ixport=? ixports))

(define (drop-collisions set1 set2)
  (remove (lambda (ixport)
	    (any (lambda (ixport*)
		   (eq? (library-ixport-to ixport)
			(library-ixport-to ixport*)))
		 set2))
	  set1))

(define (take-collisions set1 set2)
  (filter (lambda (ixport)
	    (any (lambda (ixport*)
		   (eq? (library-ixport-to ixport)
			(library-ixport-to ixport*)))
		 set2))
	  set1))