#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; RTL Common Subexpression Elimination: Hash Table Abstraction
;;;  Based on the GNU C Compiler
;;; package: (compiler rtl-cse)

(declare (usual-integrations))

(define (make-hash-table)
  (make-vector 31 false))

(define *hash-table*)

(define-integrable (hash-table-size)
  (vector-length *hash-table*))

(define-integrable (hash-table-ref hash)
  (vector-ref *hash-table* hash))

(define-integrable (hash-table-set! hash element)
  (vector-set! *hash-table* hash element))

(define-structure (element
		   (constructor %make-element)
		   (constructor make-element (expression))
		   (print-procedure
		    (standard-unparser (symbol->string 'ELEMENT) false)))
  (expression false read-only true)
  (cost false)
  (in-memory? false)
  (next-hash false)
  (previous-hash false)
  (next-value false)
  (previous-value false)
  (first-value false))

(define (hash-table-lookup hash expression)
  (let loop ((element (hash-table-ref hash)))
    (and element
	 (if (let ((expression* (element-expression element)))
	       (or (eq? expression expression*)
		   (expression-equivalent? expression expression* true)))
	     element
	     (loop (element-next-hash element))))))

(define (hash-table-insert! hash expression class)
  (let ((element (make-element expression))
	(cost (rtl:expression-cost expression)))
    (set-element-cost! element cost)
    (if hash
	(begin
	  (let ((next (hash-table-ref hash)))
	    (set-element-next-hash! element next)
	    (if next (set-element-previous-hash! next element)))
	  (hash-table-set! hash element)))
    (cond ((not class)
	   (set-element-first-value! element element))
	  ((or (< cost (element-cost class))
	       (and (= cost (element-cost class))
		    (rtl:register? expression)
		    (not (rtl:register? (element-expression class)))))
	   (set-element-next-value! element class)
	   (set-element-previous-value! class element)
	   (let loop ((x element))
	     (if x
		 (begin
		   (set-element-first-value! x element)
		   (loop (element-next-value x))))))
	  (else
	   (set-element-first-value! element class)
	   (let loop ((previous class) (next (element-next-value class)))
	     (cond ((not next)
		    (set-element-next-value! element false)
		    (set-element-next-value! previous element)
		    (set-element-previous-value! element previous))
		   ((or (< cost (element-cost next))
			(and (= cost (element-cost next))
			     (or (rtl:register? expression)
				 (not (rtl:register?
				       (element-expression next))))))
		    (set-element-next-value! element next)
		    (set-element-previous-value! next element)
		    (set-element-next-value! previous element)
		    (set-element-previous-value! element previous))
		   (else
		    (loop next (element-next-value next)))))))
    element))

(define (hash-table-delete! hash element)
  (if element
      (begin
       ;; **** Mark this element as removed.  [ref crock-1]
       (set-element-first-value! element false)
       (let ((next (element-next-value element))
	     (previous (element-previous-value element)))
	 (if next (set-element-previous-value! next previous))
	 (if previous
	     (set-element-next-value! previous next)
	     (let loop ((element next))
	       (if element
		   (begin
		     (set-element-first-value! element next)
		     (loop (element-next-value element)))))))
       (let ((next (element-next-hash element))
	     (previous (element-previous-hash element)))
	 (if next (set-element-previous-hash! next previous))
	 (if previous
	     (set-element-next-hash! previous next)
	     (hash-table-set! hash next))))))

(define (hash-table-delete-class! predicate)
  (let table-loop ((i 0))
    (if (< i (hash-table-size))
	(let bucket-loop ((element (hash-table-ref i)))
	  (if element
	      (begin
		(if (predicate element) (hash-table-delete! i element))
		(bucket-loop (element-next-hash element)))
	      (table-loop (1+ i)))))))

(define (hash-table-copy table)
  ;; During this procedure, the `element-cost' slots of `table' are
  ;; reused as "broken hearts".
  (let ((elements (vector->list table)))
    (let ((elements*
	   (map (lambda (element)
		  (let per-element ((element element) (previous false))
		    (and element
			 (let ((element*
				(%make-element
				 (element-expression element)
				 (element-cost element)
				 (element-in-memory? element)
				 false
				 previous
				 (element-next-value element)
				 (element-previous-value element)
				 (element-first-value element))))
			   (set-element-cost! element element*)
			   (set-element-next-hash!
			    element*
			    (per-element (element-next-hash element)
					 element*))
			   element*))))
		elements)))
      (letrec ((per-element
		(lambda (element)
		  (if element
		      (begin
			(if (element-first-value element)
			    (set-element-first-value!
			     element
			     (element-cost (element-first-value element))))
			(if (element-previous-value element)
			    (set-element-previous-value!
			     element
			     (element-cost (element-previous-value element))))
			(if (element-next-value element)
			    (set-element-next-value!
			     element
			     (element-cost (element-next-value element))))
			(per-element (element-next-hash element)))))))
	(for-each per-element elements*))
      (letrec ((per-element
		(lambda (element)
		  (if element
		      (begin
			(set-element-cost!
			 element
			 (element-cost (element-cost element)))
			(per-element (element-next-hash element)))))))
	(for-each per-element elements))
      (list->vector elements*))))