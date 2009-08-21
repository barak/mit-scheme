#| -*-Scheme-*-

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

;;;; MIT/GNU Scheme syntax

;;; Procedures to convert transformers to internal form.  Required
;;; during cold load, so must be loaded very early in the sequence.

(declare (usual-integrations))

;;;; Items

(define (item-constructor rtd fields)
  (let ((constructor (record-constructor rtd fields)))
    (lambda (history . arguments)
      (make-item history (apply constructor arguments)))))

(define (keyword-constructor type fields)
  (let ((constructor (item-constructor type fields)))
    (lambda arguments
      (apply constructor #f arguments))))

(define <item>)
(define make-item)
(define <expander-item>)
(define make-expander-item)

(define (initialize-syntax-transforms!)
  (set! <item>
	(make-record-type "item" '(HISTORY RECORD)))
  (set! make-item
	(record-constructor <item> '(HISTORY RECORD)))
  (set! <expander-item>
	(make-record-type "expander-item" '(EXPANDER ENVIRONMENT)))
  (set! make-expander-item
	(keyword-constructor <expander-item> '(EXPANDER ENVIRONMENT)))
  unspecific)

(define (sc-macro-transformer->expander transformer closing-environment)
  (make-expander-item (lambda (form environment closing-environment)
			(make-syntactic-closure closing-environment '()
			  (transformer form environment)))
		      closing-environment))

(define (rsc-macro-transformer->expander transformer closing-environment)
  (make-expander-item (lambda (form environment closing-environment)
			(make-syntactic-closure environment '()
			  (transformer form closing-environment)))
		      closing-environment))

(define (er-macro-transformer->expander transformer closing-environment)
  (make-expander-item
   (lambda (form environment closing-environment)
     (make-syntactic-closure environment '()
       (transformer
	form
	(let ((renames '()))
	  (lambda (identifier)
	    (let ((association (assq identifier renames)))
	      (if association
		  (cdr association)
		  (let ((rename
			 (make-syntactic-closure closing-environment '()
			   identifier)))
		    (set! renames (cons (cons identifier rename) renames))
		    rename)))))
	(lambda (x y)
	  (identifier=? environment x environment y)))))
   closing-environment))

(define (non-hygienic-macro-transformer->expander transformer
						  closing-environment)
  (make-expander-item (lambda (form environment closing-environment)
			closing-environment
			(make-syntactic-closure environment '()
			  (apply transformer (cdr form))))
		      closing-environment))

(define (syntactic-keyword->item keyword environment)
  (let ((item (environment-lookup-macro environment keyword)))
    (if (not item)
	(error:bad-range-argument keyword 'SYNTACTIC-KEYWORD->ITEM))
    item))