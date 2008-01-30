#| -*-Scheme-*-

$Id: valclass.scm,v 1.12 2008/01/30 20:01:56 cph Exp $

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

;;;; RTL Value Classes

(declare (usual-integrations))

(define-structure (value-class
		   (conc-name value-class/)
		   (constructor %make-value-class (name parent))
		   (print-procedure
		    (unparser/standard-method 'VALUE-CLASS
		      (lambda (state class)
			(unparse-object state (value-class/name class))))))
  (name false read-only true)
  (parent false read-only true)
  (children '())
  (properties (make-1d-table) read-only true))

(define (make-value-class name parent)
  (let ((class (%make-value-class name parent)))
    (if parent
	(set-value-class/children!
	 parent
	 (cons class (value-class/children parent))))
    class))

(define (value-class/ancestor-or-self? class ancestor)
  (or (eq? class ancestor)
      (let loop ((class (value-class/parent class)))
	(and class
	     (or (eq? class ancestor)
		 (loop (value-class/parent class)))))))

(define (value-class/ancestry class)
  (value-class/partial-ancestry class value-class=value))

(define (value-class/partial-ancestry class ancestor)
  (let loop ((class* class) (ancestry '()))
    (if (not class*)
	(error "value-class not an ancestor" class ancestor))
    (let ((ancestry (cons class* ancestry)))
      (if (eq? class* ancestor)
	  ancestry
	  (loop (value-class/parent class*) ancestry)))))

(define (value-class/nearest-common-ancestor x y)
  (let loop
      ((join false)
       (x (value-class/ancestry x))
       (y (value-class/ancestry y)))
    (if (and (not (null? x))
	     (not (null? y))
	     (eq? (car x) (car y)))
	(loop (car x) (cdr x) (cdr y))
	join)))

(define-syntax define-value-class
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (parent-name (caddr form)))
       (let* ((name->variable
	       (lambda (name)
		 (symbol-append 'VALUE-CLASS= name)))
	      (variable (name->variable name)))
	 `(BEGIN
	    (DEFINE ,variable
	      (MAKE-VALUE-CLASS
	       ',name
	       ,(if parent-name
		    (close-syntax (name->variable parent-name)
				  environment)
		    `#F)))
	    (DEFINE (,(symbol-append variable '?) CLASS)
	      (VALUE-CLASS/ANCESTOR-OR-SELF? CLASS ,variable))
	    (DEFINE (,(symbol-append 'REGISTER- variable '?) REGISTER)
	      (VALUE-CLASS/ANCESTOR-OR-SELF?
	       (REGISTER-VALUE-CLASS REGISTER)
	       ,variable))))))))

(define-value-class value #f)
(define-value-class float value)
(define-value-class word value)
(define-value-class object word)
(define-value-class unboxed word)
(define-value-class address unboxed)
(define-value-class immediate unboxed)
(define-value-class ascii immediate)
(define-value-class datum immediate)
(define-value-class fixnum immediate)
(define-value-class type immediate)