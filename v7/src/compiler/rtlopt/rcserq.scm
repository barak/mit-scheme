#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlopt/rcserq.scm,v 4.5 1989/08/10 11:39:35 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; RTL Common Subexpression Elimination: Register/Quantity Abstractions
;;;  Based on the GNU C Compiler

(declare (usual-integrations))

(define-structure (quantity
		   (copier quantity-copy)
		   (print-procedure
		    (standard-unparser (symbol->string 'QUANTITY) false)))
  (number false read-only true)
  (first-register false)
  (last-register false))

(define (get-register-quantity register)
  (or (register-quantity register)
      (let ((quantity (new-quantity register)))
	(set-register-quantity! register quantity)
	quantity)))

(define (new-quantity register)
  (make-quantity (let ((n *next-quantity-number*))
		   (set! *next-quantity-number* (1+ *next-quantity-number*))
		   n)
		 register
		 register))

(define *next-quantity-number*)

(define (register-tables/make n-registers)
  (vector (make-vector n-registers)
	  (make-vector n-registers)
	  (make-vector n-registers)
	  (make-vector n-registers)
	  (make-vector n-registers)
	  (make-vector n-registers)))

(define (register-tables/reset! register-tables)
  (vector-fill! (vector-ref register-tables 0) false)
  (vector-fill! (vector-ref register-tables 1) false)
  (vector-fill! (vector-ref register-tables 2) false)
  (let ((expressions (vector-ref register-tables 3)))
    (vector-fill! expressions false)
    (for-each-machine-register
     (lambda (register)
       (vector-set! expressions
		    register
		    (rtl:make-machine-register register)))))
  (vector-fill! (vector-ref register-tables 4) 0)
  (vector-fill! (vector-ref register-tables 5) -1))

(define (register-tables/copy register-tables)
  (vector (vector-map (vector-ref register-tables 0)
		      (lambda (quantity)
			(and quantity
			     (quantity-copy quantity))))
	  (vector-copy (vector-ref register-tables 1))
	  (vector-copy (vector-ref register-tables 2))
	  (vector-copy (vector-ref register-tables 3))
	  (vector-copy (vector-ref register-tables 4))
	  (vector-copy (vector-ref register-tables 5))))

(define *register-tables*)

(define-integrable (register-quantity register)
  (vector-ref (vector-ref *register-tables* 0) register))

(define-integrable (set-register-quantity! register quantity)
  (vector-set! (vector-ref *register-tables* 0) register quantity))

(define-integrable (register-next-equivalent register)
  (vector-ref (vector-ref *register-tables* 1) register))

(define-integrable (set-register-next-equivalent! register next-equivalent)
  (vector-set! (vector-ref *register-tables* 1) register next-equivalent))

(define-integrable (register-previous-equivalent register)
  (vector-ref (vector-ref *register-tables* 2) register))

(define-integrable
  (set-register-previous-equivalent! register previous-equivalent)
  (vector-set! (vector-ref *register-tables* 2) register previous-equivalent))

(define-integrable (register-expression register)
  (vector-ref (vector-ref *register-tables* 3) register))

(define-integrable (set-register-expression! register expression)
  (vector-set! (vector-ref *register-tables* 3) register expression))

(define-integrable (register-tick register)
  (vector-ref (vector-ref *register-tables* 4) register))

(define-integrable (set-register-tick! register tick)
  (vector-set! (vector-ref *register-tables* 4) register tick))

(define-integrable (register-in-table register)
  (vector-ref (vector-ref *register-tables* 5) register))

(define-integrable (set-register-in-table! register in-table)
  (vector-set! (vector-ref *register-tables* 5) register in-table))