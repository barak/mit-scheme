#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Syntax Compiler

(declare (usual-integrations))

(define (compile-item/top-level item)
  (if (binding-item? item)
      (let ((name (binding-item/name item))
	    (value (binding-item/value item)))
	(if (keyword-value-item? value)
	    (output/top-level-syntax-definition
	     name
	     (compile-item/expression (keyword-value-item/expression value)))
	    (output/top-level-definition
	     name
	     (compile-item/expression value))))
      (compile-item/expression item)))

(define (compile-body-item/top-level body-item)
  (receive (declaration-items body-items)
      (extract-declarations-from-body body-item)
    (output/top-level-sequence (map declaration-item/text declaration-items)
			       (map compile-item/top-level body-items))))

(define (compile-body-items items)
  (let ((items (flatten-body-items items)))
    (if (not (pair? items))
	(syntax-error "Empty body"))
    (output/sequence
     (append-map
      (lambda (item)
	(if (binding-item? item)
	    (let ((value (binding-item/value item)))
	      (if (keyword-value-item? value)
		  '()
		  (list (output/definition (binding-item/name item)
					   (compile-item/expression value)))))
	    (list (compile-item/expression item))))
      items))))

(define compile-item/expression)
(add-boot-init!
 (lambda ()
   (set! compile-item/expression
	 (standard-predicate-dispatcher 'compile-item/expression 1))
   (run-deferred-boot-actions 'define-item-compiler)))

(define (define-item-compiler predicate compiler)
  (defer-boot-action 'define-item-compiler
    (lambda ()
      (define-predicate-dispatch-handler compile-item/expression
	(list predicate)
	compiler))))

(define-item-compiler variable-item?
  (lambda (item)
    (output/variable (variable-item/name item))))

(define-item-compiler expression-item?
  (lambda (item)
    ((expression-item/compiler item))))

(define-item-compiler body-item?
  (lambda (item)
    (compile-body-items (body-item/components item))))

(define (illegal-expression-compiler description)
  (lambda (item)
    (syntax-error (string description " may not be used as an expression:")
		  item)))

(define-item-compiler reserved-name-item?
  (illegal-expression-compiler "Reserved name"))

(define-item-compiler keyword-item?
  (illegal-expression-compiler "Syntactic keyword"))

(define-item-compiler declaration-item?
  (illegal-expression-compiler "Declaration"))

(define-item-compiler binding-item?
  (illegal-expression-compiler "Definition"))