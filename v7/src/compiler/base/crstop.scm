#| -*-Scheme-*-

$Id: crstop.scm,v 1.18 2007/06/13 13:33:37 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Cross Compiler Top Level.
;;; This code shares and should be merged with "toplev.scm".
;;; Many of the procedures only differ in the default extensions.

(declare (usual-integrations))

(define (in-cross-compiler thunk)
  (fluid-let ((compiler:compile-by-procedures? #f)
	      (compiler:dump-info-file compiler:dump-inf-file))
    (in-compiler thunk)))

(define (cross-assemble&link info-output-pathname)
  (phase/assemble)
  (if info-output-pathname
      (cross-compiler-phase/info-generation-2 info-output-pathname))
  (cross-compiler-phase/link)
  *result*)

(define (cross-compile-bin-file-end input-string #!optional output-string)
  (compiler-pathnames
   input-string
   (and (not (default-object? output-string)) output-string)
   (make-pathname #f #f #f #f "moc" 'NEWEST)
   (lambda (input-pathname output-pathname)
     output-pathname			; ignored
     (cross-compile-scode-end (compiler-fasload input-pathname)))))

(define (cross-compile-scode-end cross-compilation)
  (in-compiler
   (lambda ()
     (cross-link-end cross-compilation)
     *result*)))

(define (cross-compiler-phase/info-generation-2 pathname)
  (info-generation-2 pathname set-cc-code-block/debugging-info!))

(define (cross-compiler-phase/link)
  (compiler-phase
   "Cross Linkification"
   (lambda ()
     (set! *result*
	   (cc-vector/make *code-vector*
			   (last-reference *entry-label*)
			   (last-reference *entry-points*)
			   (last-reference *label-bindings*)
			   (last-reference *ic-procedure-headers*)))
     unspecific)))

(define-structure (cc-code-block (type vector)
				 (conc-name cc-code-block/))
  (debugging-info #f read-only #f)
  (bit-string #f read-only #t)
  (objects #f read-only #t)
  (object-width #f read-only #t))

(define-structure (cc-vector (type vector)
			     (constructor cc-vector/make)
			     (conc-name cc-vector/))
  (code-vector #f read-only #t)
  (entry-label #f read-only #t)
  (entry-points #f read-only #t)
  (label-bindings #f read-only #t)
  (ic-procedure-headers #f read-only #t))