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

;;;; MIT/GNU Scheme syntax

;;; Procedures to convert transformers to internal form.  Required
;;; during cold load, so must be loaded very early in the sequence.

(declare (usual-integrations))

(define (sc-macro-transformer->expander transformer closing-environment)
  (make-expander-item
   (lambda (form use-environment)
     (close-syntax (transformer form use-environment)
		   closing-environment))))

(define (rsc-macro-transformer->expander transformer closing-environment)
  (make-expander-item
   (lambda (form use-environment)
     (close-syntax (transformer form closing-environment)
		   use-environment))))

(define (er-macro-transformer->expander transformer closing-environment)
  (make-expander-item
   (lambda (form use-environment)
     (close-syntax (transformer form
				(make-er-rename closing-environment)
				(make-er-compare use-environment))
		   use-environment))))

(define (make-er-rename closing-environment)
  (let ((renames '()))
    (lambda (identifier)
      (let ((p (assq identifier renames)))
	(if p
	    (cdr p)
	    (let ((rename (close-syntax identifier closing-environment)))
	      (set! renames (cons (cons identifier rename) renames))
	      rename))))))

(define (make-er-compare use-environment)
  (lambda (x y)
    (identifier=? use-environment x
		  use-environment y)))

(define (non-hygienic-macro-transformer->expander transformer
						  closing-environment)
  closing-environment
  (make-expander-item
   (lambda (form use-environment)
     (close-syntax (apply transformer (cdr form))
		   use-environment))))

(define (syntactic-keyword->item keyword environment)
  (let ((item (environment-lookup-macro environment keyword)))
    (if (not item)
	(error:bad-range-argument keyword 'SYNTACTIC-KEYWORD->ITEM))
    item))