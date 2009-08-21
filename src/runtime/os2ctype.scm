#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; OS/2 C Type Model
;;; package: (runtime os2-graphics)

(declare (usual-integrations))

;;;; Generic Type Modelling

(define-structure (c-type (conc-name c-type/) (constructor #f) (predicate #f))
  (size #f read-only #t)
  (alignment #f read-only #t))

(define-structure (c-number-type (conc-name c-number-type/))
  (size #f read-only #t)
  (alignment #f read-only #t)
  (reader #f read-only #t)
  (writer #f read-only #t))

(define-structure (c-pointer-type
		   (conc-name c-pointer-type/)
		   (constructor %make-c-pointer-type))
  (size #f read-only #t)
  (alignment #f read-only #t)
  (element-type #f read-only #t))

(define-structure (c-array-type
		   (conc-name c-array-type/)
		   (constructor %make-c-array-type))
  (size #f read-only #t)
  (alignment #f read-only #t)
  (element-type #f read-only #t)
  (n-elements #f read-only #t)
  (element-spacing #f read-only #t))

(define-structure (c-struct-type
		   (conc-name c-struct-type/)
		   (constructor %make-c-struct-type))
  (size #f read-only #t)
  (alignment #f read-only #t)
  (elements #f read-only #t))

(define-structure (c-struct-element (conc-name c-struct-element/))
  (name #f read-only #t)
  (type #f read-only #t)
  (offset #f read-only #t))

(define (define-c-type name type)
  (hash-table/put! c-type-names name (canonicalize-c-type type)))

(define (lookup-c-type name)
  (let ((type (hash-table/get c-type-names name #f)))
    (if (not type)
	(error "Unknown C type name:" name))
    type))

(define c-type-names)

(define (canonicalize-c-type type)
  (cond ((or (c-number-type? type)
	     (c-pointer-type? type)
	     (c-array-type? type)
	     (c-struct-type? type))
	 type)
	((string? type)
	 (lookup-c-type type))
	((and (pair? type)
	      (eq? 'ARRAY (car type))
	      (pair? (cdr type))
	      (pair? (cddr type))
	      (exact-nonnegative-integer? (caddr type))
	      (null? (cdddr type)))
	 (make-c-array-type (canonicalize-c-type (cadr type)) (caddr type)))
	((and (pair? type)
	      (eq? 'POINTER (car type))
	      (pair? (cdr type))
	      (null? (cddr type)))
	 (make-c-pointer-type (canonicalize-c-type (cadr type))))
	((and (pair? type)
	      (eq? 'STRUCT (car type))
	      (list? (cdr type))
	      (for-all? (cdr type)
		(lambda (element)
		  (and (pair? element)
		       (pair? (cdr element))
		       (string? (cadr element))
		       (null? (cddr element))))))
	 (make-c-struct-type (map (lambda (element)
				    (cons (cadr element)
					  (canonicalize-c-type (car element))))
				  (cdr type))))
	(else
	 (error "Malformed C type expression:" type))))

(define (define-c-integer-type name signed? size)
  (define-c-type name
    (if signed?
	(make-c-number-type size size
			    (signed-integer-reader size)
			    (signed-integer-writer size))
	(make-c-number-type size size
			    (unsigned-integer-reader size)
			    (unsigned-integer-writer size)))))

(define (unsigned-integer-reader n-bytes)
  (lambda (bytes start)
    (let ((end (+ start n-bytes)))
      (let loop ((index start) (accum 0) (factor 1))
	(if (< index end)
	    (loop (+ index 1)
		  (+ accum (* (vector-8b-ref bytes index) factor))
		  (* factor 256))
	    accum)))))

(define (signed-integer-reader n-bytes)
  (let ((read-raw (unsigned-integer-reader n-bytes))
	(split (expt 2 (- (* n-bytes 8) 1))))
    (let ((radix (* split 2)))
      (lambda (bytes start)
	(let ((raw (read-raw bytes start)))
	  (if (< raw split)
	      raw
	      (- raw radix)))))))

(define (unsigned-integer-writer n-bytes)
  (lambda (bytes start value)
    (let ((end (+ start n-bytes)))
      (let loop ((index start) (value value))
	(if (< index end)
	    (let ((q.r (integer-divide value 256)))
	      (vector-8b-set! bytes index (integer-divide-remainder q.r))
	      (loop (+ index 1) (integer-divide-quotient q.r))))))))

(define (signed-integer-writer n-bytes)
  (let ((write-raw (unsigned-integer-writer n-bytes))
	(radix (expt 2 (* n-bytes 8))))
    (lambda (bytes start value)
      (write-raw bytes start (if (< value 0) (+ value radix) value)))))

(define (make-c-pointer-type element-type)
  (%make-c-pointer-type (implementation/pointer-size element-type)
			(implementation/pointer-alignment element-type)
			element-type))

(define (make-c-array-type element-type n-elements)
  (let ((element-spacing (implementation/array-element-spacing element-type)))
    (let ((size (* element-spacing n-elements)))
      (%make-c-array-type size
			  (implementation/array-alignment element-type size)
			  element-type
			  n-elements
			  element-spacing))))

(define (make-c-struct-type element-alist)
  (let loop ((offset 0) (alist element-alist) (elements '()))
    (if (null? alist)
	(let ((elements (reverse elements)))
	  (%make-c-struct-type offset
			       (implementation/struct-alignment elements
								offset)
			       elements))
	(let ((offset
	       (implementation/struct-element-offset (cdar alist) offset)))
	  (loop (+ offset (c-type/size (cdar alist)))
		(cdr alist)
		(cons (make-c-struct-element (caar alist) (cdar alist) offset)
		      elements))))))

(define (c-number-reader type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      (guarantee-number-type type)
      (let ((reader (c-number-type/reader type)))
	(lambda (bytes)
	  (reader bytes offset))))))

(define (c-number-writer type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      (guarantee-number-type type)
      (let ((writer (c-number-type/writer type)))
	(lambda (bytes value)
	  (writer bytes offset value))))))

(define (c-element-type type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      offset
      type)))

(define (c-element-offset type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      type
      offset)))

(define (c-array-reader type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      (let ((element-type (c-array-type/element-type type))
	    (element-spacing (c-array-type/element-spacing type)))
	(guarantee-number-type element-type)
	(let ((reader (c-number-type/reader element-type)))
	  (lambda (bytes index)
	    (reader bytes (+ offset (* element-spacing index)))))))))

(define (c-array-writer type offset . selectors)
  (call-with-values (lambda () (select-c-type type offset selectors))
    (lambda (type offset)
      (let ((element-type (c-array-type/element-type type))
	    (element-spacing (c-array-type/element-spacing type)))
	(guarantee-number-type element-type)
	(let ((writer (c-number-type/writer element-type)))
	  (lambda (bytes index value)
	    (writer bytes (+ offset (* element-spacing index)) value)))))))

(define (guarantee-number-type type)
  (if (not (c-number-type? type))
      (error "Selected type is not a number type:" type)))

(define (select-c-type type offset selectors)
  (if (null? selectors)
      (values type offset)
      (call-with-values
	  (lambda () (select-c-type-1 type offset (car selectors)))
	(lambda (type offset)
	  (select-c-type type offset (cdr selectors))))))

(define (select-c-type-1 type offset selector)
  (cond ((c-array-type? type)
	 (if (not (exact-nonnegative-integer? selector))
	     (error "Illegal selector for C array:" selector))
	 (values (c-array-type/element-type type)
		 (+ offset (* (c-array-type/element-spacing type) selector))))
	((c-struct-type? type)
	 (if (not (string? selector))
	     (error "Illegal selector for C struct:" selector))
	 (let loop ((elements (c-struct-type/elements type)))
	   (if (null? elements)
	       (error "No element with this name:" selector))
	   (if (string=? selector (c-struct-element/name (car elements)))
	       (values (c-struct-element/type (car elements))
		       (+ offset (c-struct-element/offset (car elements))))
	       (loop (cdr elements)))))
	(else
	 (error "Can't select this type:" type))))

;;;; OS/2 Type Specification

(define (initialize-c-types!)
  (set! c-type-names (make-equal-hash-table))

  (define-c-integer-type "signed char"  #t 1)
  (define-c-integer-type "signed short" #t 2)
  (define-c-integer-type "signed int"   #t 4)
  (define-c-integer-type "signed long"  #t 4)

  (define-c-integer-type "unsigned char"  #f 1)
  (define-c-integer-type "unsigned short" #f 2)
  (define-c-integer-type "unsigned int"   #f 4)
  (define-c-integer-type "unsigned long"  #f 4)

  (define-c-type "char"  "signed char")
  (define-c-type "short" "signed short")
  (define-c-type "int"   "signed int")
  (define-c-type "long"  "signed long"))

(define (implementation/pointer-size element-type) element-type 4)
(define (implementation/pointer-alignment element-type) element-type 4)

(define (implementation/array-element-spacing element-type)
  (let ((size (c-type/size element-type))
	(alignment (c-type/alignment element-type)))
    (let ((delta (remainder size alignment)))
      (if (= 0 delta)
	  size
	  (+ size (- alignment delta))))))

(define (implementation/array-alignment element-type array-size)
  (if (< array-size 4)
      (c-type/alignment element-type)
      4))

(define (implementation/struct-element-offset element-type prev-end)
  (let ((a (c-type/alignment element-type)))
    (let ((r (remainder prev-end a)))
      (if (= 0 r)
	  prev-end
	  (+ prev-end (- a r))))))

(define (implementation/struct-alignment elements struct-size)
  (if (< struct-size 4)
      (apply max (map c-type/alignment (map c-struct-element/type elements)))
      4))