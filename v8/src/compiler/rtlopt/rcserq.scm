#| -*-Scheme-*-

$Id: rcserq.scm,v 1.1 1994/11/19 02:06:38 adams Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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
;;; package: (compiler rtl-cse)

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
  (vector (make-vector n-registers)	; quantity
	  (make-vector n-registers)	; next equivalent
	  (make-vector n-registers)	; previous equivalent
	  (make-vector n-registers)	; expression
	  (make-vector n-registers)	; tick
	  (make-vector n-registers)	; in table
	  (make-vector n-registers)	; preserved?
	  ))

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
  (vector-fill! (vector-ref register-tables 5) -1)
  (vector-fill! (vector-ref register-tables 6) false))

(define (register-tables/copy register-tables)
  (vector (vector-map (vector-ref register-tables 0)
		      (lambda (quantity)
			(and quantity
			     (quantity-copy quantity))))
	  (vector-copy (vector-ref register-tables 1))
	  (vector-copy (vector-ref register-tables 2))
	  (vector-copy (vector-ref register-tables 3))
	  (vector-copy (vector-ref register-tables 4))
	  (vector-copy (vector-ref register-tables 5))
	  (vector-copy (vector-ref register-tables 6))))

(define (register-tables/restore! register-tables)
  ;; Nothing is preserved.
  (vector-fill! (vector-ref register-tables 6) false))

(define-integrable (%register-quantity tables register)
  (vector-ref (vector-ref tables 0) register))

(define-integrable (%set-register-quantity! tables register quantity)
  (vector-set! (vector-ref tables 0) register quantity))

(define-integrable (%register-next-equivalent tables register)
  (vector-ref (vector-ref tables 1) register))

(define-integrable
  (%set-register-next-equivalent! tables register next-equivalent)
  (vector-set! (vector-ref tables 1) register next-equivalent))

(define-integrable (%register-previous-equivalent tables register)
  (vector-ref (vector-ref tables 2) register))

(define-integrable
  (%set-register-previous-equivalent! tables register previous-equivalent)
  (vector-set! (vector-ref tables 2) register previous-equivalent))

(define-integrable (%register-expression tables register)
  (vector-ref (vector-ref tables 3) register))

(define-integrable (%set-register-expression! tables register expression)
  (vector-set! (vector-ref tables 3) register expression))

(define-integrable (%register-tick tables register)
  (vector-ref (vector-ref tables 4) register))

(define-integrable (%set-register-tick! tables register tick)
  (vector-set! (vector-ref tables 4) register tick))

(define-integrable (%register-in-table tables register)
  (vector-ref (vector-ref tables 5) register))

(define-integrable (%set-register-in-table! tables register in-table)
  (vector-set! (vector-ref tables 5) register in-table))

(define-integrable (%register-preserved? tables register)
  (vector-ref (vector-ref tables 6) register))

(define-integrable (%set-register-preserved?! tables register state)
  (vector-set! (vector-ref tables 6) register state))

(define *register-tables*)

(define-integrable (register-quantity register)
  (%register-quantity *register-tables* register))

(define-integrable (set-register-quantity! register quantity)
  (%set-register-quantity! *register-tables* register quantity))

(define-integrable (register-next-equivalent register)
  (%register-next-equivalent *register-tables* register))

(define-integrable (set-register-next-equivalent! register next-equivalent)
  (%set-register-next-equivalent! *register-tables* register next-equivalent))

(define-integrable (register-previous-equivalent register)
  (%register-previous-equivalent *register-tables* register))

(define-integrable
  (set-register-previous-equivalent! register previous-equivalent)
  (%set-register-previous-equivalent! *register-tables*
				      register previous-equivalent))

(define-integrable (register-expression register)
  (%register-expression *register-tables* register))

(define-integrable (set-register-expression! register expression)
  (%set-register-expression! *register-tables* register expression))

(define-integrable (register-tick register)
  (%register-tick *register-tables* register))

(define-integrable (set-register-tick! register tick)
  (%set-register-tick! *register-tables* register tick))

(define-integrable (register-in-table register)
  (%register-in-table *register-tables* register))

(define-integrable (set-register-in-table! register in-table)
  (%set-register-in-table! *register-tables* register in-table))

(define (register-preserved? register)
  (%register-preserved? *register-tables* register))

(define (set-register-preserved?! register state)
  (%set-register-preserved?! *register-tables* register state))