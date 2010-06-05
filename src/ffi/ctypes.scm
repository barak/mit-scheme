#| -*-Scheme-*-

Copyright (C) 2006, 2007, 2008, 2009, 2010 Matthew Birkholz

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

;;;; C Types and C Type Simplification
;;; package: (ffi syntax)


;;; C Types

(define (ctype/basic? ctype)
  ;; Returns #t iff CTYPE is a basic C type, e.g. char, int or double.
  (and (symbol? ctype)
       (not (eq? ctype '*))
       (assq ctype peek-poke-primitives)))

(define (ctype/pointer? ctype)
  ;; Returns #t iff CTYPE is a pointer type, e.g. (* GtkWidget).
  (or (eq? ctype '*)
      (and (pair? ctype) (eq? '* (car ctype))
	   (pair? (cdr ctype)) (null? (cddr ctype)))))

(define ctype-pointer/target-type cadr)

(define (ctype/void? ctype)
  (eq? ctype 'VOID))

(define (ctype/const? ctype)
  (and (pair? ctype) (eq? 'CONST (car ctype))
       (pair? (cdr ctype)) (null? (cddr ctype))))

(define ctype-const/qualified-type cadr)

(define (ctype/struct-name? ctype)
  ;; Returns #t iff CTYPE is a struct name, e.g. (struct _GValue).
  (and (pair? ctype) (eq? 'STRUCT (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (null? (cddr ctype))))

(define (ctype/struct-anon? ctype)
  ;; Returns #t iff CTYPE is an anonymous struct
  ;; -- (struct (MEMBER . TYPE)...).
  (and (pair? ctype) (eq? 'STRUCT (car ctype))
       (pair? (cdr ctype)) (pair? (cadr ctype))))

(define (ctype/struct-named? ctype)
  ;; Returns #t iff CTYPE is a named struct
  ;; -- (struct NAME (MEMBER VALUE)...).
  (and (pair? ctype) (eq? 'STRUCT (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (pair? (cddr ctype)) (pair? (caddr ctype))))

(define (ctype/struct-defn? ctype)
  (or (ctype/struct-anon? ctype)
      (ctype/struct-named? ctype)))

(define (ctype-struct-defn/members ctype)
  (cond ((ctype/struct-anon? ctype) (cdr ctype))
	((ctype/struct-named? ctype) (cddr ctype))
	(else (error "Bogus C struct type:" ctype))))

(define (ctype/struct? ctype)
  (or (ctype/struct-name? ctype) (ctype/struct-defn? ctype)))

(define (ctype-struct/name ctype)
  ;; This works on a struct name as well as definitions.
  (and (or (and (eq? 'STRUCT (car ctype))
		(pair? (cdr ctype)))
	   (error:wrong-type-argument ctype "C struct type" 'ctype-struct/name))
       (symbol? (cadr ctype))
       (cadr ctype)))

(define (make-ctype-struct name members)
  (if name
      (cons* 'STRUCT name members)
      (cons 'STRUCT members)))

(define (ctype/union-name? ctype)
  ;; Returns #t iff CTYPE is a union name, e.g. (union _GdkEvent).
  (and (pair? ctype) (eq? 'UNION (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (null? (cddr ctype))))

(define (ctype/union-anon? ctype)
  ;; Returns #t iff CTYPE is an anonymous union
  ;; -- (union (MEMBER . TYPE)...).
  (and (pair? ctype) (eq? 'UNION (car ctype))
       (pair? (cdr ctype)) (pair? (cadr ctype))))

(define (ctype/union-named? ctype)
  ;; Returns #t iff CTYPE is a named union
  ;; -- (union NAME (MEMBER TYPE)...).
  (and (pair? ctype) (eq? 'UNION (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (pair? (cddr ctype)) (pair? (caddr ctype))))

(define (ctype/union-defn? ctype)
  (or (ctype/union-anon? ctype)
      (ctype/union-named? ctype)))

(define (ctype-union-defn/members ctype)
  (cond ((ctype/union-named? ctype) (cddr ctype))
	((ctype/union-anon? ctype) (cdr ctype))
	(else (error "Bogus C union type:" ctype))))

(define (ctype/union? ctype)
  (or (ctype/union-name? ctype) (ctype/union-defn? ctype)))

(define (ctype-union/name ctype)
  ;; This works on union names as well as definitions.
  (and (or (and (eq? 'UNION (car ctype))
		(pair? (cdr ctype)))
	   (error:wrong-type-argument ctype "C union type" 'ctype-union/name))
       (symbol? (cadr ctype))
       (cadr ctype)))

(define (make-ctype-union name members)
  (if name
      (cons* 'UNION name members)
      (cons 'UNION members)))

(define (ctype/enum-name? ctype)
  ;; Returns #t iff CTYPE is an enum name, e.g. (enum GdkEventType).
  (and (pair? ctype) (eq? 'ENUM (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (null? (cddr ctype))))

(define (ctype/enum-anon? ctype)
  ;; Returns #t iff CTYPE is an anonymous enum
  ;; -- (enum (CONSTANT . VALUE)...).
  (and (pair? ctype) (eq? 'ENUM (car ctype))
       (pair? (cdr ctype)) (pair? (cadr ctype))))

(define (ctype/enum-named? ctype)
  ;; Returns #t iff CTYPE is a named enum
  ;; -- (enum NAME (CONSTANT . VALUE)...).
  (and (pair? ctype) (eq? 'ENUM (car ctype))
       (pair? (cdr ctype)) (symbol? (cadr ctype))
       (pair? (cddr ctype)) (pair? (caddr ctype))))

(define (ctype/enum-defn? ctype)
  (or (ctype/enum-anon? ctype)
      (ctype/enum-named? ctype)))

(define (ctype-enum-defn/constants ctype)
  (cond ((ctype/enum-named? ctype) (cddr ctype))
	((ctype/enum-anon? ctype) (cdr ctype))
	(else (error "Bogus C enum type:" ctype))))

(define (ctype/enum? ctype)
  (or (ctype/enum-name? ctype) (ctype/enum-defn? ctype)))

(define (ctype-enum/name ctype)
  ;; This works on enum names as well as definitions.
  (and (or (and (eq? 'ENUM (car ctype))
		(pair? (cdr ctype)))
	   (error:wrong-type-argument ctype "C enum type" 'ctype-enum/name))
       (symbol? (cadr ctype))
       (cadr ctype)))

(define (make-ctype-enum name constants)
  (if name
      (cons* 'ENUM name constants)
      (cons 'ENUM constants)))

(define (ctype/array? ctype)
  ;; Returns #t iff CTYPE is an array type, e.g. (ARRAY (* GtkWidget) 5).
  (and (pair? ctype) (eq? 'ARRAY (car ctype))
       (pair? (cdr ctype))
       (or (null? (cddr ctype))
	   (and (pair? (cddr ctype)) (null? (cdddr ctype))))))

(define ctype-array/element-type cadr)

(define (ctype-array/size ctype)
  (and (pair? (cddr ctype)) (caddr ctype)))

(define (make-ctype-array ctype size)
  (list 'ARRAY ctype size))

(define (ctype/primitive-accessor ctype)
  ;; Returns the primitive to use when reading from CTYPE, a basic ctype.
  (let ((entry (assq ctype peek-poke-primitives)))
    (and entry
	 (car (cdr entry)))))

(define (ctype/primitive-modifier ctype)
  ;; Returns the primitive to use when writing to CTYPE, a basic ctype.
  (let ((entry (assq ctype peek-poke-primitives)))
    (and entry
	 (cadr (cdr entry)))))

(define peek-poke-primitives
  ;; Alist: basic type names x (prim-access prim-modify).
  ;; 
  ;; A couple type converters in generator.scm depend on handling
  ;; ALL of this list.
  `((char      ,(ucode-primitive c-peek-char 2)   ,(ucode-primitive c-poke-char 3))
    (uchar     ,(ucode-primitive c-peek-uchar 2)  ,(ucode-primitive c-poke-uchar 3))
    (short     ,(ucode-primitive c-peek-short 2)  ,(ucode-primitive c-poke-short 3))
    (ushort    ,(ucode-primitive c-peek-ushort 2) ,(ucode-primitive c-poke-ushort 3))
    (int       ,(ucode-primitive c-peek-int 2)    ,(ucode-primitive c-poke-int 3))
    (uint      ,(ucode-primitive c-peek-uint 2)   ,(ucode-primitive c-poke-uint 3))
    (long      ,(ucode-primitive c-peek-long 2)   ,(ucode-primitive c-poke-long 3))
    (ulong     ,(ucode-primitive c-peek-ulong 2)  ,(ucode-primitive c-poke-ulong 3))
    (float     ,(ucode-primitive c-peek-float 2)  ,(ucode-primitive c-poke-float 3))
    (double    ,(ucode-primitive c-peek-double 2) ,(ucode-primitive c-poke-double 3))
    (*         ,(ucode-primitive c-peek-pointer 3),(ucode-primitive c-poke-pointer 3))
    ))


;;; C Type Lookup

(define (definite-ctype ctype includes)
  ;; Returns a definite C type equivalent to CTYPE.  If CTYPE is a
  ;; name, e.g.
  ;;
  ;; |GdkColor|, (struct |_GdkColor|), (union |_GdkEvent|)
  ;;
  ;; returns the definite C type of its definition per INCLUDES.  A
  ;; definite C type is a basic type name, array or pointer type, or
  ;; struct, union or enum names or definitions.

  (let loop ((stack '())
	     (ctype ctype))
    (cond ((or (ctype/basic? ctype)
	       (ctype/void? ctype)
	       (eq? 'ENUM ctype)
	       (eq? '* ctype)) ctype)
	  ((symbol? ctype)
	   (if (memq ctype stack)
	       (error "Circular definition of C type:" (car (last-pair stack))))
	   (let ((entry (assq ctype (c-includes/type-names includes))))
	     (if (not entry)
		 (error "Unknown type:" ctype)
		 (loop (cons ctype stack) (cadr entry)))))
	  ((ctype/const? ctype)
	   (loop stack (ctype-const/qualified-type ctype)))
	  ((or (ctype/array? ctype)
	       (ctype/pointer? ctype)
	       (ctype/struct? ctype)
	       (ctype/union? ctype)
	       (ctype/enum? ctype)) ctype)
	  (else
	   (error:wrong-type-argument ctype "a C type" 'definite-ctype)))))

(define (ctype-definition ctype includes)
  (let ((type (definite-ctype ctype includes)))
    (cond ((or (ctype/basic? type)
	       (ctype/void? type)
	       (ctype/array? type)
	       (ctype/pointer? type)
	       (ctype/struct-defn? type)
	       (ctype/union-defn? type)
	       (ctype/enum-defn? type)
	       ;; Enum constants are not enumerated in -const.scm files.
	       (eq? 'ENUM type)) type)
	  ((ctype/struct-name? type)
	   (let ((entry (assq (cadr type) (c-includes/structs includes))))
	     (if (not entry)
		 (error "Unknown type:" type)
		 (cadr entry))))
	  ((ctype/union-name? type)
	   (let ((entry (assq (cadr type) (c-includes/unions includes))))
	     (if (not entry)
		 (error "Unknown type:" type)
		 (cadr entry))))
	  ((ctype/enum-name? type)
	   (let ((entry (assq (cadr type) (c-includes/enums includes))))
	     (if (not entry)
		 (error "Unknown type:" type)
		 (cadr entry))))
	  (else (error "Unexpected C type:" ctype)))))