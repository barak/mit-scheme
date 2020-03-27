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

;;;; Syntax -- cold-load support

;;; Procedures to convert classifiers and transformers to internal form.
;;; Required during cold load, so must be loaded very early in the sequence.

(declare (usual-integrations))

(define (sc-macro-transformer->expander transformer env)
  (transformer-item (sc-wrapper transformer (runtime-getter env))))

(define (sc-macro-transformer->item transformer closing-senv expr)
  (transformer-item (sc-wrapper transformer (lambda () closing-senv))
		    expr))

(define (sc-wrapper transformer get-closing-senv)
  (lambda (form use-senv hist)
    (declare (ignore hist))
    (close-syntax (transformer form use-senv)
		  (get-closing-senv))))

(define (rsc-macro-transformer->expander transformer env)
  (transformer-item (rsc-wrapper transformer (runtime-getter env))))

(define (rsc-macro-transformer->item transformer closing-senv expr)
  (transformer-item (rsc-wrapper transformer (lambda () closing-senv))
		    expr))

(define (rsc-wrapper transformer get-closing-senv)
  (lambda (form use-senv hist)
    (declare (ignore use-senv hist))
    (transformer form (get-closing-senv))))

(define (er-macro-transformer->expander transformer env)
  (transformer-item (er-wrapper transformer (runtime-getter env))))

(define (er-macro-transformer->item transformer closing-senv expr)
  (transformer-item (er-wrapper transformer (lambda () closing-senv))
		    expr))

(define (er-wrapper transformer get-closing-senv)
  (lambda (form use-senv hist)
    (declare (ignore hist))
    (transformer form
		 (make-er-rename (get-closing-senv))
		 (make-er-compare use-senv))))

(define (make-er-rename closing-senv)
  (let ((renames '()))
    (lambda (id)
      (guarantee identifier? id)
      (let ((p (assq id renames)))
	(if p
	    (cdr p)
	    (let ((rename (close-syntax id closing-senv)))
	      (set! renames (cons (cons id rename) renames))
	      rename))))))

(define (make-er-compare use-senv)
  (lambda (x y)
    (identifier=? use-senv x use-senv y)))

(define (spar-macro-transformer->expander spar env expr)
  (transformer-item (spar-wrapper spar (runtime-getter env))
		    expr))

(define (spar-macro-transformer->item spar closing-senv expr)
  (transformer-item (spar-wrapper spar (lambda () closing-senv))
		    expr))

(define (spar-wrapper spar get-closing-senv)
  (lambda (form use-senv hist)
    (spar-call spar form use-senv hist (get-closing-senv))))

(define (runtime-getter env)
  (lambda ()
    (runtime-environment->syntactic env)))

;;; Keyword items represent syntactic keywords.

;; A transformer has the signature (form senv hist) -> form.
(define-record-type <transformer-item>
    (%transformer-item impl expr)
    transformer-item?
  (impl transformer-item-impl)
  (expr transformer-item-expr))

(define (transformer-item impl #!optional expr)
  (%transformer-item impl expr))

(define (transformer-item-has-expr? item)
  (not (default-object? (transformer-item-expr item))))

;; A classifier has the signature (form senv hist) -> item.
(define-record-type <classifier-item>
    (classifier-item impl)
    classifier-item?
  (impl classifier-item-impl))

(define (classifier->runtime classifier)
  (make-unmapped-macro-reference-trap (classifier-item classifier)))

(define (classifier->keyword classifier)
  (close-syntax 'keyword
		(make-classifier-senv 'keyword
				      (classifier-item classifier))))

(define (spar-classifier->runtime promise)
  (classifier->runtime (spar-classifier-promise-caller promise)))

(define (spar-classifier->keyword promise)
  (classifier->keyword (spar-classifier-promise-caller promise)))

(define (spar-classifier-promise-caller promise)
  (lambda (form senv hist)
    (spar-call (force promise) form senv hist senv)))

(define (spar-transformer->runtime promise)
  (make-unmapped-macro-reference-trap
   (transformer-item
    (lambda (form use-senv hist)
      (spar-call (force promise)
		 form
		 use-senv
		 hist
		 (top-level-senv))))))

(define-deferred top-level-senv
  (make-unsettable-parameter #f))