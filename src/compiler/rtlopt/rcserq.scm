#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
  (vector (vector-map (lambda (quantity)
			(and quantity
			     (quantity-copy quantity)))
		      (vector-ref register-tables 0))
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