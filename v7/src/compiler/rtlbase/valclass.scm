#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/valclass.scm,v 1.2 1990/01/18 22:45:58 cph Rel $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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

(let-syntax
    ((define-value-class
       (lambda (name parent-name)
	 (let* ((name->variable
		 (lambda (name) (symbol-append 'VALUE-CLASS= name)))
		(variable (name->variable name)))
	   `(BEGIN
	      (DEFINE ,variable
		(MAKE-VALUE-CLASS ',name
				  ,(if parent-name
				       (name->variable parent-name)
				       `#F)))
	      (DEFINE (,(symbol-append variable '?) CLASS)
		(VALUE-CLASS/ANCESTOR-OR-SELF? CLASS ,variable))
	      (DEFINE
		(,(symbol-append 'REGISTER- variable '?) REGISTER)
		(VALUE-CLASS/ANCESTOR-OR-SELF? (REGISTER-VALUE-CLASS REGISTER)
					       ,variable)))))))

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

)