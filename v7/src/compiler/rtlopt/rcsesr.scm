#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcsesr.scm,v 4.1 1987/12/08 13:56:09 cph Exp $

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

;;;; RTL Common Subexpression Elimination: Stack References

(declare (usual-integrations))

(define *stack-offset*)
(define *stack-reference-quantities*)

(define (stack-push/pop? expression)
  (and (memq (rtl:expression-type expression) '(PRE-INCREMENT POST-INCREMENT))
       (interpreter-stack-pointer? (rtl:address-register expression))))

(define (stack-reference? expression)
  (and (eq? (rtl:expression-type expression) 'OFFSET)
       (interpreter-stack-pointer? (rtl:address-register expression))))

(define (stack-reference-quantity expression)
  (let ((n (+ *stack-offset* (rtl:offset-number expression))))
    (let ((entry (ass= n *stack-reference-quantities*)))
      (if entry
	  (cdr entry)
	  (let ((quantity (new-quantity false)))
	    (set! *stack-reference-quantities*
		  (cons (cons n quantity)
			*stack-reference-quantities*))
	    quantity)))))

(define-integrable (stack-pointer-adjust! offset)
  (set! *stack-offset* (+ (stack->memory-offset offset) *stack-offset*))
  (stack-pointer-invalidate!))

(define-integrable (stack-pointer-invalidate!)
  (register-expression-invalidate! (interpreter-stack-pointer)))

(define-integrable (stack-invalidate!)
  (set! *stack-reference-quantities* '()))

(define (stack-region-invalidate! start end)
  (let ((end (+ *stack-offset* end)))
    (define (loop i quantities)
      (if (< i end)
	  (loop (1+ i)
		(del-ass=! i quantities))
	  (set! *stack-reference-quantities* quantities)))
    (loop (+ *stack-offset* start) *stack-reference-quantities*)))

(define (stack-reference-invalidate! expression)
  (expression-invalidate! expression)
  (set! *stack-reference-quantities*
	(del-ass=! (+ *stack-offset* (rtl:offset-number expression))
		   *stack-reference-quantities*)))

(define ass= (association-procedure = car))
(define del-ass=! (delete-association-procedure list-deletor! = car))