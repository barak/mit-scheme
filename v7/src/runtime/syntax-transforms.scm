#| -*-Scheme-*-

$Id: syntax-transforms.scm,v 14.4 2003/02/14 18:28:34 cph Exp $

Copyright 1989-1991, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; MIT Scheme syntax

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

(define item-rtd)
(define make-item)
(define expander-item-rtd)
(define make-expander-item)

(define (initialize-syntax-transforms!)
  (set! item-rtd
	(make-record-type "item" '(HISTORY RECORD)))
  (set! make-item
	(record-constructor item-rtd '(HISTORY RECORD)))
  (set! expander-item-rtd
	(make-record-type "expander-item" '(EXPANDER ENVIRONMENT)))
  (set! make-expander-item
	(keyword-constructor expander-item-rtd '(EXPANDER ENVIRONMENT)))
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