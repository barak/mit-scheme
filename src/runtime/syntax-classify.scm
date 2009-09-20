#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Syntax Classifier

(declare (usual-integrations))

(define (classify/form form environment definition-environment)
  (cond ((identifier? form)
	 (let ((item (lookup-identifier form environment)))
	   (if (keyword-item? item)
	       (make-keyword-value-item
		(strip-keyword-value-item item)
		(make-expression-item
		 (let ((name (identifier->symbol form)))
		   (lambda ()
		     (output/combination
		      (output/runtime-reference 'SYNTACTIC-KEYWORD->ITEM)
		      (list (output/constant name)
			    (output/the-environment)))))))
	       item)))
	((syntactic-closure? form)
	 (let ((form (syntactic-closure/form form))
	       (free-names (syntactic-closure/free-names form))
	       (closing-env (syntactic-closure/environment form)))
	   (classify/form form
			  (make-partial-syntactic-environment free-names
							      environment
							      closing-env)
			  definition-environment)))
	((pair? form)
	 (let ((item
		(strip-keyword-value-item
		 (classify/expression (car form) environment))))
	   (cond ((classifier-item? item)
		  ((classifier-item/classifier item) form
						     environment
						     definition-environment))
		 ((compiler-item? item)
		  (make-expression-item
		   (let ((compiler (compiler-item/compiler item)))
		     (lambda ()
		       (compiler form environment)))))
		 ((expander-item? item)
		  (classify/form ((expander-item/expander item) form
								environment)
				 environment
				 definition-environment))
		 (else
		  (if (not (list? (cdr form)))
		      (syntax-error "Combination must be a proper list:" form))
		  (make-expression-item
		   (let ((items (classify/expressions (cdr form) environment)))
		     (lambda ()
		       (output/combination
			(compile-item/expression item)
			(map compile-item/expression items)))))))))
	(else
	 (make-expression-item (lambda () (output/constant form))))))

(define (strip-keyword-value-item item)
  (if (keyword-value-item? item)
      (keyword-value-item/item item)
      item))

(define (classify/forms forms environment definition-environment)
  (map (lambda (form)
	 (classify/form form environment definition-environment))
       forms))

(define (classify/expression expression environment)
  (classify/form expression environment null-syntactic-environment))

(define (classify/expressions expressions environment)
  (classify/forms expressions environment null-syntactic-environment))

(define (classify/body forms environment definition-environment)
  ;; Top-level syntactic definitions affect all forms that appear
  ;; after them, so classify FORMS in order.
  (make-body-item
   (let forms-loop ((forms forms) (body-items '()))
     (if (pair? forms)
	 (let items-loop
	     ((items
	       (item->list
		(classify/form (car forms)
			       environment
			       definition-environment)))
	      (body-items body-items))
	   (if (pair? items)
	       (items-loop (cdr items)
			   (if (null-binding-item? (car items))
			       body-items
			       (cons (car items) body-items)))
	       (forms-loop (cdr forms) body-items)))
	 (reverse! body-items)))))

(define (extract-declarations-from-body items)
  (let loop ((items items) (declarations '()) (items* '()))
    (if (pair? items)
	(if (declaration-item? (car items))
	    (loop (cdr items)
		  (cons (car items) declarations)
		  items*)
	    (loop (cdr items)
		  declarations
		  (cons (car items) items*)))
	(values (reverse! declarations) (reverse! items*)))))