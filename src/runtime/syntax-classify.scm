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

;;;; Syntax Classifier

(declare (usual-integrations))

(define (classify/form form environment)
  (cond ((identifier? form)
	 (let ((item (lookup-identifier form environment)))
	   (if (keyword-item? item)
	       (keyword-value-item
		(strip-keyword-value-item item)
		(expr-item
		 (let ((name (identifier->symbol form)))
		   (lambda ()
		     (output/combination
		      (output/runtime-reference 'syntactic-keyword->item)
		      (list (output/constant name)
			    (output/the-environment)))))))
	       item)))
	((syntactic-closure? form)
	 (classify/form
	  (syntactic-closure-form form)
	  (make-partial-syntactic-environment (syntactic-closure-free form)
					      environment
					      (syntactic-closure-senv form))))
	((pair? form)
	 (let ((item
		(strip-keyword-value-item
		 (classify/expression (car form) environment))))
	   (cond ((classifier-item? item)
		  ((classifier-item-impl item) form environment))
		 ((compiler-item? item)
		  (expr-item
		   (let ((compiler (compiler-item-impl item)))
		     (lambda ()
		       (compiler form environment)))))
		 ((expander-item? item)
		  (classify/form ((expander-item-impl item) form environment)
				 environment))
		 (else
		  (if (not (list? (cdr form)))
		      (syntax-error "Combination must be a proper list:" form))
		  (expr-item
		   (let ((items (classify/expressions (cdr form) environment)))
		     (lambda ()
		       (output/combination
			(compile-item/expression item)
			(map compile-item/expression items)))))))))
	(else
	 (expr-item (lambda () (output/constant form))))))

(define (strip-keyword-value-item item)
  (if (keyword-value-item? item)
      (keyword-value-item-keyword item)
      item))

(define (classify/expression expression environment)
  (classify/form expression environment))

(define (classify/expressions expressions environment)
  (map (lambda (expression)
	 (classify/expression expression environment))
       expressions))

(define (classify/body forms environment)
  ;; Syntactic definitions affect all forms that appear after them, so classify
  ;; FORMS in order.
  (seq-item
   (let loop ((forms forms) (items '()))
     (if (pair? forms)
	 (loop (cdr forms)
	       (reverse* (item->list (classify/form (car forms) environment))
			 items))
	 (reverse! items)))))