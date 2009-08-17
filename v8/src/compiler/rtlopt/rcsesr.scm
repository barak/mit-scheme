#| -*-Scheme-*-

$Id: 99e73a9a97a27ca406d1c044800da8165c5fca7a $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; RTL Common Subexpression Elimination: Stack References

(declare (usual-integrations))

(define *stack-offset*)
(define *stack-reference-quantities*)

(define-integrable (memory->stack-offset offset)
  ;; Assume this operation is a self-inverse.
  (stack->memory-offset offset))

(define (stack-push? expression)
  (and (rtl:pre-increment? expression)
       (interpreter-stack-pointer? (rtl:address-register expression))
       (= -1 (memory->stack-offset (rtl:address-number expression)))))

(define (stack-pop? expression)
  (and (rtl:post-increment? expression)
       (interpreter-stack-pointer? (rtl:address-register expression))
       (= 1 (memory->stack-offset (rtl:address-number expression)))))

(define (stack-reference? expression)
  (and (rtl:offset? expression)
       (interpreter-stack-pointer? (rtl:address-register expression))))

(define (stack-reference-quantity expression)
  (let ((n (+ *stack-offset*
	      (rtl:machine-constant-value (rtl:offset-offset expression)))))
    (let ((entry (ass= n *stack-reference-quantities*)))
      (if entry
	  (cdr entry)
	  (let ((quantity (new-quantity false)))
	    (set! *stack-reference-quantities*
		  (cons (cons n quantity)
			*stack-reference-quantities*))
	    quantity)))))

(define (set-stack-reference-quantity! expression quantity)
  (let ((n (+ *stack-offset*
	      (rtl:machine-constant-value (rtl:offset-offset expression)))))
    (let ((entry (ass= n *stack-reference-quantities*)))
      (if entry
	  (set-cdr! entry quantity)
	  (set! *stack-reference-quantities*
		(cons (cons n quantity)
		      *stack-reference-quantities*)))))
  unspecific)

(define (stack-pointer-adjust! offset)
  (let ((offset (memory->stack-offset offset)))
    (if (positive? offset)		;i.e. if a pop
	(stack-region-invalidate! 0 offset)))
  (set! *stack-offset* (+ *stack-offset* offset))
  (stack-pointer-invalidate!))

(define-integrable (stack-pointer-invalidate!)
  (register-expression-invalidate! (interpreter-stack-pointer)))

(define-integrable (stack-invalidate!)
  (set! *stack-reference-quantities* '()))

(define (stack-region-invalidate! start end)
  (let loop ((i start) (quantities *stack-reference-quantities*))
    (if (< i end)
	(loop (1+ i)
	      (del-ass=! (+ *stack-offset* (stack->memory-offset i))
			 quantities))
	(set! *stack-reference-quantities* quantities))))

(define (stack-reference-invalidate! expression)
  (expression-invalidate! expression)
  (set! *stack-reference-quantities*
	(del-ass=! (+ *stack-offset*
		      (rtl:machine-constant-value
		       (rtl:offset-offset expression)))
		   *stack-reference-quantities*)))

(define ass= (association-procedure = car))
(define del-ass=! (delete-association-procedure list-deletor! = car))