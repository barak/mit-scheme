#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcseep.scm,v 1.2 1987/03/20 05:12:44 cph Exp $

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

;;;; RTL Common Subexpression Elimination: Expression Predicates
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define (expression-equivalent? x y validate?)
  ;; If VALIDATE? is true, assume that Y comes from the hash table and
  ;; should have its register references validated.
  (define (loop x y)
    (let ((type (rtl:expression-type x)))
      (and (eq? type (rtl:expression-type y))
	   (case type
	     ((REGISTER)
	      (register-equivalent? x y))
	     ((OFFSET)
	      (let ((rx (rtl:offset-register x)))
		(and (register-equivalent? rx (rtl:offset-register y))
		     (if (interpreter-stack-pointer? rx)
			 (eq? (stack-reference-quantity x)
			      (stack-reference-quantity y))
			 (= (rtl:offset-number x)
			    (rtl:offset-number y))))))
	     (else
	      (rtl:match-subexpressions x y loop))))))

  (define (register-equivalent? x y)
    (let ((x (rtl:register-number x))
	  (y (rtl:register-number y)))
      (and (eq? (register-quantity x) (register-quantity y))
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

(define (expression-address-varies? expression)
  (if (memq (rtl:expression-type expression)
	    '(OFFSET PRE-INCREMENT POST-INCREMENT))
      (register-expression-varies? (rtl:address-register expression))
      (rtl:any-subexpression? expression expression-address-varies?)))

(define (expression-varies? expression)
  ;; This procedure should not be called on a register expression.
  (let ((type (rtl:expression-type expression)))
    (or (memq type '(OFFSET PRE-INCREMENT POST-INCREMENT))
	(if (eq? type 'REGISTER)
	    (register-expression-varies? expression)
	    (rtl:any-subexpression? expression expression-varies?)))))

(define (register-expression-varies? expression)
  (not (= regnum:regs-pointer (rtl:register-number expression))))

(define (stack-push/pop? expression)
  (and (pre/post-increment? expression)
       (interpreter-stack-pointer? (rtl:address-register expression))))

(define (heap-allocate? expression)
  (and (pre/post-increment? expression)
       (interpreter-free-pointer? (rtl:address-register expression))))

(define-integrable (pre/post-increment? expression)
  (memq (rtl:expression-type expression) '(PRE-INCREMENT POST-INCREMENT)))

(define-integrable (expression-not-object? expression)
  (memq (rtl:expression-type expression)
  (loop x))