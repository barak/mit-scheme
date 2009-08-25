#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Machine Model for SVM
;;; package: (compiler)

(declare (usual-integrations))

(define use-pre/post-increment? #t)
(define-integrable scheme-type-width 6)
(define-integrable scheme-type-limit #x40)
(define-integrable scheme-object-width 32) ;could be 64 too
(define-integrable float-width 64)
(define-integrable float-alignment scheme-object-width)
(define-integrable addressing-granularity 8)

(define-integrable address-units-per-float
  (quotient float-width addressing-granularity))

(define-integrable address-units-per-object
  (quotient scheme-object-width addressing-granularity))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)

(define-integrable (machine-register-known-value register)
  register
  #f)

(define (machine-register-value-class register)
  (guarantee-limited-index-fixnum register
				  number-of-machine-registers
				  'MACHINE-REGISTER-VALUE-CLASS)
  (cond ((or (fix:= register regnum:stack-pointer)
	     (fix:= register regnum:dynamic-link)
	     (fix:= register regnum:free-pointer))
	 value-class=address)
	((fix:< register regnum:float-0) value-class=object)
	(else value-class=float)))

;;;; RTL Generator Interface

(define-syntax define-machine-register
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(symbol identifier) (cdr form))
	 (let ((name (symbol-append 'INTERPRETER- (cadr form)))
	       (offset (close-syntax (caddr form) environment)))
	   `(BEGIN
	      (DEFINE (,(close-syntax name environment))
		(RTL:MAKE-MACHINE-REGISTER ,offset))
	      (DEFINE (,(close-syntax (symbol-append name '?) environment)
		       EXPRESSION)
		(AND (RTL:REGISTER? EXPRESSION)
		     (FIX:= (RTL:REGISTER-NUMBER EXPRESSION) ,offset)))))
	 (ill-formed-syntax form)))))

(define-machine-register stack-pointer regnum:stack-pointer)
(define-machine-register dynamic-link regnum:dynamic-link)
(define-machine-register free-pointer regnum:free-pointer)
(define-machine-register value-register regnum:value)

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((FREE) (interpreter-free-pointer))
    ((VALUE) (interpreter-value-register))
    (else #f)))

(define (rtl:interpreter-register->offset locative)
  (error "Unknown register type:" locative))

(define (rtl:constant-cost expression)
  (let ((if-integer
	 (lambda (value)
	   value
	   ;; Can this be done in fewer bytes for suitably small values?
	   1))				; MOV immediate
	(get-pc-cost
	 (+ 3				; CALL
	    4))				; POP
	(based-reference-cost
	 1)				; MOV r/m
	(address-offset-cost
	 1))				; LEA instruction

    (define (if-synthesized-constant type datum)
      (if-integer (make-non-pointer-literal type datum)))

    (case (rtl:expression-type expression)
      ((CONSTANT)
       (let ((value (rtl:constant-value expression)))
	 (if (object-non-pointer? value)
	     (if-synthesized-constant (object-type value) (object-datum value))
	     (+ get-pc-cost based-reference-cost))))
      ((MACHINE-CONSTANT)
       (if-integer (rtl:machine-constant-value expression)))
      ((ENTRY:PROCEDURE ENTRY:CONTINUATION)
       (+ get-pc-cost address-offset-cost))
      ((ASSIGNMENT-CACHE VARIABLE-CACHE)
       (+ get-pc-cost based-reference-cost))
      ((OFFSET-ADDRESS BYTE-OFFSET-ADDRESS FLOAT-OFFSET-ADDRESS)
       address-offset-cost)
      ((CONS-POINTER)
       (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	    (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	    (if-synthesized-constant
	     (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	     (rtl:machine-constant-value
	      (rtl:cons-pointer-datum expression)))))
      (else
       #f))))

(define compiler:open-code-floating-point-arithmetic?
  #t)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/
		  VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS))

;;;; Closure format

;; See microcode/cmpintmd/svm1.c for a description of the layout.

;; Offset of the first object in the closure from the address of the
;; first closure entry point, in words.  In order to make this work,
;; we add padding to the closure-count field so that the first entry
;; is aligned on an object boundary.

(define (closure-first-offset count entry)
  entry
  (if (= count 0)
      1
      (+ (integer-ceiling (* count 3) address-units-per-object)
	 count)))

;; Offset of the first object in the closure from the address of the
;; manifest-closure header word, in words.

(define (closure-object-first-offset count)
  (if (= count 0)
      1
      (+ 2 (closure-first-offset count 0))))

;; Increment from one closure entry address to another, in bytes.

(define (closure-entry-distance count entry entry*)
  (* 3 (- entry* entry)))

;; Increment from a given closure address to the first closure
;; address, in bytes.  Usually negative.

(define (closure-environment-adjustment count entry)
  (closure-entry-distance count entry 0))