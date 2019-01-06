#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; RTL Common Subexpression Elimination: Hash Table Abstraction
;;;  Based on the GNU C Compiler
;;; package: (compiler rtl-cse)

(declare (usual-integrations))

(define (make-rcse-ht)
  (make-vector 31 false))

(define *hash-table*)

(define-integrable (rcse-ht-size)
  (vector-length *hash-table*))

(define-integrable (rcse-ht-ref hash)
  (vector-ref *hash-table* hash))

(define-integrable (rcse-ht-set! hash element)
  (vector-set! *hash-table* hash element))

(define-structure (element
		   (constructor %make-element)
		   (constructor make-element (expression))
		   (print-procedure (standard-print-method "LIAR:element")))
  (expression false read-only true)
  (cost false)
  (in-memory? false)
  (next-hash false)
  (previous-hash false)
  (next-value false)
  (previous-value false)
  (first-value false))

(define (rcse-ht-lookup hash expression)
  (let loop ((element (rcse-ht-ref hash)))
    (and element
	 (if (let ((expression* (element-expression element)))
	       (or (eq? expression expression*)
		   (expression-equivalent? expression expression* true)))
	     element
	     (loop (element-next-hash element))))))

(define (rcse-ht-insert! hash expression class)
  (let ((element (make-element expression))
	(cost (rtl:expression-cost expression)))
    (set-element-cost! element cost)
    (if hash
	(begin
	  (let ((next (rcse-ht-ref hash)))
	    (set-element-next-hash! element next)
	    (if next (set-element-previous-hash! next element)))
	  (rcse-ht-set! hash element)))
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

(define (rcse-ht-delete! hash element)
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
	     (rcse-ht-set! hash next))))))

(define (rcse-ht-delete-class! predicate)
  (let table-loop ((i 0))
    (if (< i (rcse-ht-size))
	(let bucket-loop ((element (rcse-ht-ref i)))
	  (if element
	      (begin
		(if (predicate element) (rcse-ht-delete! i element))
		(bucket-loop (element-next-hash element)))
	      (table-loop (1+ i)))))))

(define (rcse-ht-copy table)
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