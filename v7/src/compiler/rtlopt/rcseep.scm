#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcseep.scm,v 4.6 1990/01/18 22:47:53 cph Rel $

Copyright (c) 1987, 1988, 1990 Massachusetts Institute of Technology

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