#| -*-Scheme-*-

$Id: rcseep.scm,v 4.9 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1987, 1988, 1990, 1999 Massachusetts Institute of Technology

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

;;;; RTL Common Subexpression Elimination: Expression Predicates
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define (expression-equivalent? x y validate?)
  ;; If VALIDATE? is true, assume that Y comes from the hash table and
  ;; should have its register references validated.
  (define (loop x y)
    (let ((type (rtl:expression-type x)))
      (and (eq? type (rtl:expression-type y))
	   (cond ((eq? type 'REGISTER)
		  (register-equivalent? x y))
		 ((and (memq type '(OFFSET BYTE-OFFSET))
		       (interpreter-stack-pointer? (rtl:offset-base x)))
		  (and (interpreter-stack-pointer? (rtl:offset-base y))
		       (eq? (stack-reference-quantity x)
			    (stack-reference-quantity y))))
		 (else
		  (rtl:match-subexpressions x y loop))))))

  (define (register-equivalent? x y)
    (let ((x (rtl:register-number x))
	  (y (rtl:register-number y)))
      (and (eq? (get-register-quantity x) (get-register-quantity y))
	   (or (not validate?)
	       (= (register-in-table y) (register-tick y))))))

  (loop x y))

(define (expression-refers-to? x y)
  ;; True iff any subexpression of X matches Y.
  (define (loop x)
    (or (eq? x y)
	(if (eq? (rtl:expression-type x) (rtl:expression-type y))
	    (expression-equivalent? x y false)
	    (rtl:any-subexpression? x loop))))
  (loop x))

(define (interpreter-register-reference? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))))

(define (expression-address-varies? expression)
  (and (not (interpreter-register-reference? expression))
       (or (memq (rtl:expression-type expression)
		 '(OFFSET BYTE-OFFSET PRE-INCREMENT POST-INCREMENT)))
       (rtl:any-subexpression? expression expression-address-varies?)))