#| -*-Scheme-*-

$Id: traditional.scm,v 1.1 2006/09/16 11:19:09 gjr Exp $

Copyright (c) 1992-1999, 2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; C-output fake assembler and linker
;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Object constructors
;; This is the 'traditional' way, i.e. when stackify is not used
;; It generates C code to explicitly construct the objects.

(define num)
(define new-variables)

(define (generate-variable-name)
  (let ((var (string-append "tmpObj" (number->string num))))
    (set! new-variables (cons var new-variables))
    (set! num (1+ num))
    var))

(define-integrable (table/find table value)
  ;; assv ?
  (assq value table))

(define trivial-objects
  (list #f #t '() unspecific))

(define (trivial? object)
  (or (memq object trivial-objects)
      (guaranteed-fixnum? object)
      (reference-trap? object)))

(define *depth-limit* 2)

(define (name-if-complicated node depth)
  (cond ((fake-compiled-block? node)
	 (let ((name (fake-block/name node)))
	   (set! new-variables (cons name new-variables))
	   name))
	((or (%record? node)
	     (vector? node)
	     (> depth *depth-limit*))
	 (generate-variable-name))
	(else
	 false)))  

(define (build-table nodes)
  (map cdr
       (sort (sort/enumerate
	      (list-transform-positive
		  (let loop ((nodes nodes)
			     (table '()))
		    (if (null? nodes)
			table
			(loop (cdr nodes)
			      (insert-in-table (car nodes)
					       0
					       table))))
		(lambda (pair)
		  (cdr pair))))
	     (lambda (entry1 entry2)
	       (let ((obj1 (cadr entry1))
		     (obj2 (cadr entry2)))
		 (if (not (fake-compiled-block? obj2))
		     (or (fake-compiled-block? obj1)
			 (< (car entry1) (car entry2)))
		     (and (fake-compiled-block? obj1)
			  (< (fake-block/index obj1)
			     (fake-block/index obj2)))))))))

;; Hack to make sort a stable sort

(define (sort/enumerate l)
  (let loop ((l l) (n 0) (l* '()))
    (if (null? l)
	l*
	(loop (cdr l)
	      (1+ n)
	      (cons (cons n (car l))
		    l*)))))

(define (insert-in-table node depth table)
  (cond ((trivial? node)
	 table)
	((table/find table node)
	 => (lambda (pair)
	      (if (not (cdr pair))
		  (set-cdr! pair (generate-variable-name)))
	      table))
	(else
	 (let* ((name (name-if-complicated node depth))
		(depth* (if name 1 (1+ depth)))
		(table (cons (cons node name) table)))

	   (define-integrable (do-vector-like node vlength vref)
	     (let loop ((table table)
			(i (vlength node)))
	       (if (zero? i)
		   table
		   (let ((i-1 (-1+ i)))
		     (loop (insert-in-table (vref node i-1)
					    depth*
					    table)
			   i-1)))))
	     
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
		 (else
		  ;; Atom
		  table))))))

(define (top-level-constructor object&name)
  ;; (values prefix suffix)
  (let ((name (cdr object&name))
	(object (car object&name)))
    (cond ((pair? object)
	   (values '()
		   (list name " = (CONS (SHARP_F, SHARP_F));\n\t")))
	  ((fake-compiled-block? object)
	   (set! *subblocks* (cons object *subblocks*))
	   (values (list name " = (initialize_subblock (\""
			 (fake-block/c-proc object)
			 "\"));\n\t")
		   '()))
	  ((fake-compiled-procedure? object)
	   (values '()
		   (list name " = "
			 (compiled-procedure-constructor
			  object)
			 ";\n\t")))
	  ((reference-trap? object)
	   (if (not (unassigned-reference-trap? object))
	       (error "Can't dump reference trap" object)
	       (values '()
		       (list name
			     " = "
			     (->simple-C-object object)))))
	  ((%record? object)
	   (values '()
		   (list name " = (ALLOCATE_RECORD ("
			 (number->string (%record-length object))
			 "));\n\t")))
	  ((vector? object)
	   (values '()
		   (list name " = (ALLOCATE_VECTOR ("
			 (number->string (vector-length object))
			 "));\n\t")))
	  (else
	   (values '()
		   (list name "\n\t  = "
			 (->simple-C-object object)
			 ";\n\t"))))))

(define (top-level-updator object&name table)
  (let ((name (cdr object&name))
	(object (car object&name)))

    (define-integrable (do-vector-like object vlength vref vset-name)
      (let loop ((i (vlength object))
		 (code '()))
	(if (zero? i)
	    code
	    (let ((i-1 (- i 1)))
	      (loop i-1
		    `(,vset-name " (" ,name ", "
				 ,(number->string i-1) ", "
				 ,(constructor (vref object i-1)
					       table)
				 ");\n\t"
				 ,@code))))))

    (cond ((pair? object)
	   (list "SET_PAIR_CAR (" name ", "
		 (constructor (car object) table) ");\n\t"
		 "SET_PAIR_CDR (" name ", "
		 (constructor (cdr object) table) ");\n\t"))
	  ((or (fake-compiled-block? object)
	       (fake-compiled-procedure? object)
	       (reference-trap? object))
	   '(""))
	  ((%record? object)
	   (do-vector-like object %record-length %record-ref "RECORD_SET"))
	  ((vector? object)
	   (do-vector-like object vector-length vector-ref "VECTOR_SET"))
	  (else
	   '("")))))

(define (constructor object table)
  (let process ((object object))
    (cond ((table/find table object) => cdr)
	  ((pair? object)
	   (cond ((or (not (pair? (cdr object)))
		      (table/find table (cdr object)))
		  (string-append "(CONS (" (process (car object)) ", "
				 (process (cdr object)) "))"))
		 (else
		  (let loop ((npairs 0)
			     (object object)
			     (frobs '()))
		    (if (and (pair? object) (not (table/find table object)))
			(loop (1+ npairs)
			      (cdr object)
			      (cons (car object) frobs))
			;; List is reversed to call rconsm
			(string-append
			 "(RCONSM (" (number->string (1+ npairs))
			 (apply string-append
				(map (lambda (frob)
				       (string-append ",\n\t\t"
						      (process frob)))
				     (cons object frobs)))
			 "))"))))))
	  ((fake-compiled-procedure? object)
	   (compiled-procedure-constructor object))
	  ((reference-trap? object)
	   (->simple-C-object object))
	  ((or (fake-compiled-block? object)
	       (vector? object)
	       (%record? object))
	   (error "constructor: Can't build directly"
		  object))
	  (else
	   (->simple-C-object object)))))

(define (compiled-procedure-constructor object)
  (string-append "(CC_BLOCK_TO_ENTRY ("
		 (fake-procedure/block-name object)
		 ", "
		 (number->string
		  (fake-procedure/label-tag object))
		 "))"))

(define (top-level-constructors table)
  ;; (values prefix suffix)
  ;; (append-map top-level-constructor table)
  (let loop ((table (reverse table)) (prefix '()) (suffix '()))
    (if (null? table)
	(values prefix suffix)
	(with-values (lambda () (top-level-constructor (car table)))
	  (lambda (prefix* suffix*)
	    (loop (cdr table)
		  (append prefix* prefix)
		  (append suffix* suffix)))))))

(define (->constructors names objects)
  ;; (values prefix-code suffix-code)
  (let* ((table (build-table objects)))
    (with-values (lambda () (top-level-constructors table))
      (lambda (prefix suffix)
	(values prefix
		(append suffix
			(append-map (lambda (object&name)
				      (top-level-updator object&name table))
				    table)
			(append-map
			 (lambda (name object)
			   (list (string-append name "\n\t  = "
						(constructor object table)
						";\n\t")))
			 names
			 objects)))))))

(define (->simple-C-object object)
  (cond ((symbol? object)
	 (let ((name (symbol->string object)))
	   (string-append "(C_SYM_INTERN ("
			  (number->string (string-length name))
			  "L, \"" (C-quotify-string name) "\"))")))
	((string? object)
	 (string-append "(C_STRING_TO_SCHEME_STRING ("
			(number->string (string-length object))
			"L, \"" (C-quotify-string object) "\"))"))
	((number? object)
	 (let process ((number object))
	   (cond ((flo:flonum? number)
		  (string-append "(DOUBLE_TO_FLONUM ("
				 (number->string number) "))"))
		 ((guaranteed-long? number)
		  (string-append "(LONG_TO_INTEGER ("
				 (number->string number) "L))"))
		 ((exact-integer? number)
		  (let ((bignum-string
			 (number->string (if (negative? number)
					     (- number)
					     number)
					 16)))
		    (string-append "(DIGIT_STRING_TO_INTEGER ("
				   (if (negative? number)
				       "true, "
				       "false, ")
				   (number->string
				    (string-length bignum-string))
				   "L, \"" bignum-string "\"))")))
		 ((and (exact? number) (rational? number))
		  (string-append "(MAKE_RATIO ("
				 (process (numerator number))
				 ", " (process (denominator number))
				 "))"))
		 ((and (complex? number) (not (real? number)))
		  (string-append "(MAKE_COMPLEX ("
				 (process (real-part number))
				 ", " (process (imag-part number))
				 "))"))
		 (else
		  (error "scheme->C-object: Unknown number" number)))))
	((eq? #f object)
	 "SHARP_F")
	((eq? #t object)
	 "SHARP_T")
	((null? object)
	 "NIL")
	((eq? object unspecific)
	 "UNSPECIFIC")

	((primitive-procedure? object)
	 (let ((arity (primitive-procedure-arity object)))
	   (if (< arity -1)
	       (error "scheme->C-object: Unknown arity primitive" object)
	       (string-append "(MAKE_PRIMITIVE_PROCEDURE (\""
			      (symbol->string
			       (primitive-procedure-name object))
			      "\", "
			      (number->string arity)
			      "))"))))
	((char? object)
	 (string-append "(MAKE_CHAR ("
			(let ((bits (char-bits object)))
			  (if (zero? bits)
			      "0"
			      (string-append "0x" (number->string bits 16))))
			", ((unsigned) "
			(C-quotify-char (make-char (char-code object) 0))
			")))"))
	((bit-string? object)
	 (let ((string (number->string (bit-string->unsigned-integer object)
				       16)))
	   (string-append "(DIGIT_STRING_TO_BIT_STRING ("
			  (number->string (bit-string-length object)) "L, "
			  (number->string (string-length string)) "L, \""
			  (string-reverse string)
			  "\"))")))
	((or (object-type? (object-type #t) object)
	     (object-type? (object-type '()) object))
	 ;; Random assorted objects, e.g.: #!rest, #!optional
	 (string-append "(MAKE_OBJECT ("
			(if (object-type? (object-type #t) object)
			    "TC_CONSTANT"
			    "TC_NULL")
			", "
			(number->string (object-datum object))
			"L))"))
	;; This one is here for multi-definitions with no initial value
	((reference-trap? object)
	 (if (not (unassigned-reference-trap? object))
	     (error "Can't dump reference trap" object)
	     "UNASSIGNED_OBJECT"))
	;; Note: The following is here because of the Scode interpreter
	;; and the runtime system.
	;; They are not necessary for ordinary code.
	((interpreter-return-address? object)
	 (string-append "(MAKE_OBJECT (TC_RETURN_CODE, 0x"
			(number->string (object-datum object) 16)
			"))"))
	(else
	 (error "->simple-C-object: unrecognized-type"
		object))))

(define char-set:C-char-quoted
  (char-set-union
   ;; Not char-set:not-graphic
   (char-set-difference char-set:all
			(char-set-intersection char-set:graphic
					       (ascii-range->char-set 0 #x7f)))
   (char-set #\\ #\' (integer->char #xA0))))

;; The following routine relies on the fact that Scheme and C use the
;; same quoting convention for the named characters.

(define (C-quotify-char char)
  (cond ((not (char-set-member? char-set:C-char-quoted char))
	 (string #\' char #\'))
	((char-set-member? char-set:C-named-chars char)
	 (string-append
	  "'"
	  (let ((s (write-to-string (make-string 1 char))))
	    (substring s 1 (-1+ (string-length s))))
	  "'"))
	((char=? char #\')
	 "'\\''")
	((char=? char #\NUL)
	 "'\\0'")
	(else
	 (string-append
	  "'\\"
	  (let ((s (number->string (char-code char) 8)))
	    (if (< (string-length s) 3)
		(string-append (make-string (- 3 (string-length s)) #\0)
			       s)
		s))
	  "'"))))