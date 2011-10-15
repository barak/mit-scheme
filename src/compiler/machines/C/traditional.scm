#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; C-output fake assembler and linker
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Object constructors
;; This is the 'traditional' way, i.e. when stackify is not used
;; It generates C code to explicitly construct the objects.

(define (->constructors names objects)
  (let ((table (build-table objects)))
    (receive (prefix suffix) (top-level-constructors table)
      (values prefix
	      (c:group suffix
		       (c:group* (map (lambda (object&name)
					(top-level-updater object&name table))
				      table))
		       (c:group*
			(map (lambda (name object)
			       (c:= name (constructor object table)))
			     names
			     objects)))))))

(define (build-table nodes)
  (map cdr
       (sort (sort/enumerate
	      (keep-matching-items
		  (let loop ((nodes nodes) (table '()))
		    (if (pair? nodes)
			(loop (cdr nodes)
			      (insert-in-table (car nodes) 0 table))
			table))
		cdr))
	     (lambda (entry1 entry2)
	       (let ((obj1 (cadr entry1))
		     (obj2 (cadr entry2)))
		 (if (fake-compiled-block? obj1)
		     (if (fake-compiled-block? obj2)
			 (< (fake-block/index obj1)
			    (fake-block/index obj2))
			 #t)
		     (if (fake-compiled-block? obj2)
			 #f
			 (< (car entry1) (car entry2)))))))))

(define-integrable (table/find table value)
  ;; assv ?
  (assq value table))  

(define (top-level-constructors table)
  (let loop ((table (reverse table)) (prefix (c:group)) (suffix (c:group)))
    (if (pair? table)
	(receive (prefix* suffix*) (top-level-constructor (car table))
	  (loop (cdr table)
		(c:group prefix* prefix)
		(c:group suffix* suffix)))
	(values prefix suffix))))

(define (top-level-constructor o.n)
  (let ((object (car o.n))
	(name (cdr o.n)))
    (cond ((pair? object)
	   (values (c:group)
		   (c:= name (c:ecall "CONS" #f #f))))
	  ((fake-compiled-block? object)
	   (set! *subblocks* (cons object *subblocks*))
	   (values (c:= name
			(c:ecall 'initialize_subblock
				 (fake-block/c-proc object)))
		   (c:group)))
	  ((fake-compiled-procedure? object)
	   (values (c:group)
		   (c:= name (compiled-procedure-constructor object))))
	  ((reference-trap? object)
	   (if (not (unassigned-reference-trap? object))
	       (error "Can't dump reference trap:" object))
	   (values (c:group)
		   (c:= name (->simple-C-object object))))
	  ((%record? object)
	   (values (c:group)
		   (c:= name
			(c:ecall "ALLOCATE_RECORD" (%record-length object)))))
	  ((vector? object)
	   (values (c:group)
		   (c:= name
			(c:ecall "ALLOCATE_VECTOR" (vector-length object)))))
	  (else
	   (values (c:group)
		   (c:= name (->simple-C-object object)))))))

(define (top-level-updater o.n table)
  (let ((object (car o.n))
	(name (cdr o.n)))

    (define-integrable (do-vector-like object vlength vref vset-name)
      (let loop ((i (vlength object)) (code (c:group)))
	(if (zero? i)
	    code
	    (let ((i-1 (- i 1)))
	      (loop i-1
		    (c:group (c:scall vset-name
				      name
				      i-1
				      (constructor (vref object i-1) table))
			     code))))))

    (cond ((pair? object)
	   (c:group (c:scall "SET_PAIR_CAR"
			     name
			     (constructor (car object) table))
		    (c:scall "SET_PAIR_CDR"
			     name
			     (constructor (cdr object) table))))
	  ((or (fake-compiled-block? object)
	       (fake-compiled-procedure? object)
	       (reference-trap? object))
	   (c:group))
	  ((%record? object)
	   (do-vector-like object %record-length %record-ref "RECORD_SET"))
	  ((vector? object)
	   (do-vector-like object vector-length vector-ref "VECTOR_SET"))
	  (else
	   (c:group)))))

(define (constructor object table)
  (let process ((object object))
    (cond ((table/find table object) => cdr)
	  ((pair? object)
	   (let ((elts
		  (let loop
		      ((object (cdr object))
		       (elts (list (process (car object)))))
		    (if (pair? object)
			(let ((p (table/find table object)))
			  (if p
			      (cons p elts)
			      (loop (cdr object)
				    (cons (process (car object))
					  elts))))
			(cons object elts)))))
	     (let ((n-elts (length elts)))
	       (if (fix:= n-elts 2)
		   (c:ecall "CONS" (cadr elts) (car elts))
		   (apply c:ecall "RCONSM" n-elts elts)))))
	  ((fake-compiled-procedure? object)
	   (compiled-procedure-constructor object))
	  ((reference-trap? object)
	   (->simple-C-object object))
	  ((or (fake-compiled-block? object)
	       (vector? object)
	       (%record? object))
	   (error "constructor: Can't build directly:" object))
	  (else
	   (->simple-C-object object)))))

(define (compiled-procedure-constructor object)
  (c:ecall "CC_BLOCK_TO_ENTRY"
	   (fake-procedure/block-name object)
	   (fake-procedure/label-tag object)))

(define (->simple-C-object object)
  (cond ((symbol? object)
	 (let ((name (symbol->string object)))
	   (c:ecall "C_SYM_INTERN"
		    (string-length name)
		    (c:string (C-quotify-string name)))))
	((string? object)
	 (c:ecall "C_STRING_TO_SCHEME_STRING"
		  (string-length object)
		  (c:string (C-quotify-string object))))
	((number? object)
	 (let process ((number object))
	   (cond ((flo:flonum? number)
		  (c:ecall "DOUBLE_TO_FLONUM" number))
		 ((guaranteed-long? number)
		  (c:ecall "LONG_TO_INTEGER" number))
		 ((exact-integer? number)
		  (let ((bignum-string
			 (number->string (if (negative? number)
					     (- number)
					     number)
					 16)))
		    (c:ecall "DIGIT_STRING_TO_INTEGER"
			     (negative? number)
			     (string-length bignum-string)
			     bignum-string)))
		 ((and (exact? number) (rational? number))
		  (c:ecall "MAKE_RATIO"
			   (process (numerator number))
			   (process (denominator number))))
		 ((and (complex? number) (not (real? number)))
		  (c:ecall "MAKE_complext"
			   (process (real-part number))
			   (process (imag-part number))))
		 (else
		  (error "->simple-C-object: Unknown number:" number)))))
	((not object) "SHARP_F")
	((eq? #t object) "SHARP_T")
	((null? object) "EMPTY_LIST")
	((eq? object unspecific) "UNSPECIFIC")
	((scode/primitive-procedure? object)
	 (let ((arity (primitive-procedure-arity object)))
	   (if (< arity -1)
	       (error "->simple-C-object: Unknown arity primitive:" object))
	   (c:ecall "MAKE_PRIMITIVE_PROCEDURE"
		    (c:string (primitive-procedure-name object))
		    arity)))
	((char? object)
	 (c:ecall "MAKE_CHAR"
		  (c:hex (char-bits object))
		  (c:hex (char-code object))))
	((bit-string? object)
	 (let ((string
		(number->string (bit-string->unsigned-integer object) 16)))
	   (c:ecall "DIGIT_STRING_TO_BIT_STRING"
		    (bit-string-length object)
		    (string-length string)
		    (c:string (string-reverse string)))))
	;; This one is here for multi-definitions with no initial value
	((unassigned-reference-trap? object)
	 "UNASSIGNED_OBJECT")
	((object-non-pointer? object)
	 (c:make-object (c:hex (object-type object))
			(c:hex (object-datum object))))
	(else
	 (error "->simple-C-object: unrecognized object:" object))))

;;; Hack to make sort a stable sort

(define (sort/enumerate l)
  (let loop ((l l) (n 0) (l* '()))
    (if (null? l)
	l*
	(loop (cdr l)
	      (+ n 1)
	      (cons (cons n (car l)) l*)))))

(define (insert-in-table node depth table)
  (cond ((or (not node)
	     (eq? node #t)
	     (null? node)
	     (eq? node unspecific)
	     (guaranteed-fixnum? node)
	     (reference-trap? node))
	 table)
	((table/find table node)
	 => (lambda (pair)
	      (if (not (cdr pair))
		  (set-cdr! pair (generate-variable-name)))
	      table))
	(else
	 (let* ((name (name-if-complicated node depth))
		(depth* (if name 1 (+ depth 1)))
		(table (cons (cons node name) table)))

	   (define-integrable (do-vector-like node vlength vref)
	     (let loop ((table table) (i (vlength node)))
	       (if (fix:> i 0)
		   (loop (insert-in-table (vref node (fix:- i 1))
					  depth*
					  table)
			 (fix:- i 1))
		   table)))
	     
	   (cond ((pair? node)
		  ;; Special treatment on the CDR because of RCONSM.
		  (insert-in-table
		   (car node)
		   depth*
		   (insert-in-table (cdr node)
				    (if name 1 depth)
				    table)))
		 ((vector? node)
		  (do-vector-like node vector-length vector-ref))
		 ((or (fake-compiled-procedure? node)
		      (fake-compiled-block? node))
		  table)
		 ((%record? node)
		  (do-vector-like node %record-length %record-ref))
		 ;; Atom
		 (else table))))))

(define num)
(define new-variables)
(define *depth-limit* 2)

(define (generate-variable-name)
  (let ((var (string-append "tmpObj" (number->string num))))
    (set! new-variables (cons var new-variables))
    (set! num (+ num 1))
    var))

(define (name-if-complicated node depth)
  (cond ((fake-compiled-block? node)
	 (let ((name (fake-block/name node)))
	   (set! new-variables (cons name new-variables))
	   name))
	((or (%record? node)
	     (vector? node)
	     (> depth *depth-limit*))
	 (generate-variable-name))
	(else #f)))