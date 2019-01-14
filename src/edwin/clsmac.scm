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

;;;; Class/Object System

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define-syntax define-class
  (rsc-macro-transformer
   (lambda (form environment)
     (if (and (syntax-match? '(identifier datum (* symbol)) (cdr form))
	      (or (identifier? (caddr form))
		  (null? (caddr form))))
	 (let ((name (cadr form))
	       (superclass (if (null? (caddr form)) #f (caddr form)))
	       (variables (cadddr form)))
	   ;; Compile-time definition.
	   (make-class (identifier->symbol name)
		       (and superclass
			    (name->class (identifier->symbol superclass)))
		       variables)
	   ;; Load-time definition.
	   `(,(close-syntax 'define environment)
	     ,name
	     (,(close-syntax 'make-class environment)
	      ',(identifier->symbol name)
	      ,superclass
	      ',variables)))
	 (ill-formed-syntax form)))))

(define-syntax define-method
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((finish
	    (lambda (name operation expression)
	      `(,(close-syntax 'class-method-define environment)
		,name
		',operation
		,expression))))
       (cond ((syntax-match? '(identifier symbol expression) (cdr form))
	      (finish (cadr form) (caddr form) (cadddr form)))
	     ((and (syntax-match? '(identifier (symbol . mit-bvl) + expression)
				  (cdr form))
		   (pair? (cdr (caddr form)))
		   (identifier? (cadr (caddr form))))
	      (finish (cadr form)
		      (car (caddr form))
		      `(,(close-syntax 'named-lambda environment)
			,(caddr form)
			(,(close-syntax 'with-instance-variables environment)
			 ,(cadr form)
			 ,(cadr (caddr form))
			 ()
			 ,@(cdddr form)))))
	     (else
	      (ill-formed-syntax form)))))))

(define with-instance-variables
  (classifier->runtime
   ;; Rest arg facilitates cross-compiling from 9.2.
   ;; It should be removed after 9.3 release.
   (lambda (form senv . rest)
     (syntax-check '(_ identifier expression (* identifier) + expression) form)
     (let ((class-name (cadr form))
	   (self-item (apply classify-form (caddr form) senv rest))
	   (free-names (cadddr form))
	   (body-item
	    (apply classify-form
		   `(,(close-syntax 'begin
				    (runtime-environment->syntactic
				     system-global-environment))
		     ,@(cddddr form))
		   senv
		   rest)))
       (expr-item #f
	 (lambda ()
	   (transform-instance-variables
	    (class-instance-transforms
	     (name->class (identifier->symbol class-name)))
	    (compile-expr-item self-item)
	    free-names
	    (compile-item body-item))))))))

(define-syntax ==>
  (syntax-rules ()
    ((==> object operation argument ...)
     (let ((temp object))
       ((object-method temp 'operation) temp argument ...)))))

(define-syntax usual==>
  (syntax-rules ()
    ((usual==> object operation argument ...)
     (let ((temp object))
       ((usual-method (object-class temp) 'operation) temp argument ...)))))