#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlexp.scm,v 4.3 1988/03/14 21:04:40 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Register Transfer Language: Expression Operations

(declare (usual-integrations))

(define-integrable (rtl:invocation? rtl)
  (memq (rtl:expression-type rtl)
	'(INVOCATION:APPLY
	  INVOCATION:JUMP
	  INVOCATION:LEXPR
	  INVOCATION:PRIMITIVE
	  INVOCATION:SPECIAL-PRIMITIVE
	  INVOCATION:UUO-LINK
	  INVOCATION:CACHE-REFERENCE
	  INVOCATION:LOOKUP)))

(define (rtl:trivial-expression? expression)
  (if (memq (rtl:expression-type expression)
	    '(REGISTER
	      CONSTANT
	      ENTRY:CONTINUATION
	      ENTRY:PROCEDURE
	      UNASSIGNED
	      VARIABLE-CACHE
	      ASSIGNMENT-CACHE))
      true
      (and (rtl:offset? expression)
	   (interpreter-stack-pointer? (rtl:offset-register expression)))))

(define (rtl:machine-register-expression? expression)
  (and (rtl:register? expression)
       (machine-register? (rtl:register-number expression))))

(define (rtl:pseudo-register-expression? expression)
  (and (rtl:register? expression)
       (pseudo-register? (rtl:register-number expression))))

(define (rtl:address-valued-expression? expression)
  (if (rtl:register? expression)
      (register-contains-address? (rtl:register-number expression))
      (or (rtl:object->address? expression)
	  (rtl:variable-cache? expression)
	  (rtl:assignment-cache? expression))))

(define (rtl:map-subexpressions expression procedure)
  (if (rtl:constant? expression)
      (map identity-procedure expression)
      (cons (car expression)
	    (map (lambda (x)
		   (if (pair? x)
		       (procedure x)
		       x))
		 (cdr expression)))))

(define (rtl:for-each-subexpression expression procedure)
  (if (not (rtl:constant? expression))
      (for-each (lambda (x)
		  (if (pair? x)
		      (procedure x)))
		(cdr expression))))

(define (rtl:any-subexpression? expression predicate)
  (and (not (rtl:constant? expression))
       (there-exists? (cdr expression)
		      (lambda (x)
			(and (pair? x)
			     (predicate x))))))

(define (rtl:all-subexpressions? expression predicate)
  (or (rtl:constant? expression)
      (for-all? (cdr expression)
		(lambda (x)
		  (or (not (pair? x))
		      (predicate x))))))

(define (rtl:reduce-subparts expression operator initial if-expression if-not)
  (let ((remap
	 (if (rtl:constant? expression)
	     if-not
	     (lambda (x)
	       (if (pair? x)
		   (if-expression x)
		   (if-not x))))))
    (define (loop parts accum)
      (if (null? parts)
	  accum
	  (loop (cdr parts)
		(operator accum (remap (car parts))))))
    (loop (cdr expression) initial)))

(define (rtl:match-subexpressions x y predicate)
  (let ((type (rtl:expression-type x)))
    (and (eq? type (rtl:expression-type y))
	 (if (eq? type 'CONSTANT)
	     (eqv? (cadr x) (cadr y))
	     (let loop ((x (cdr x)) (y (cdr y)))
	       ;; Because of fixed format, all expressions of same
	       ;; type have the same length, and each entry is either
	       ;; a subexpression or a non-expression.
	       (or (null? x)
		   (and (if (pair? (car x))
			    (predicate (car x) (car y))
			    (eqv? (car x) (car y)))
			(loop (cdr x) (cdr y)))))))))

(define (rtl:modify-subexpressions expression procedure)
  (if (not (rtl:constant? expression))
      (let loop ((tail (cdr expression)))
	(if (not (null? tail))
	    (begin (if (pair? (car tail))
		       (procedure (car tail)
				  (lambda (expression)
				    (set-car! tail expression))))
		   (loop (cdr tail)))))))