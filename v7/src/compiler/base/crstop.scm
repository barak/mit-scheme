#| -*-Scheme-*-

$Id: crstop.scm,v 1.20 2008/01/30 20:01:42 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Cross compiler

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