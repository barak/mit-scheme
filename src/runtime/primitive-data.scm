#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

;;;; Low-level primitive data structures
;;; package: (runtime primitive-data)

(declare (usual-integrations))

(define-integrable (%bignum? object)
  (object-type? (ucode-type bignum) object))

(define-integrable (%bit-string? object)
  (object-type? (ucode-type vector-1b) object))

(define-integrable (%broken-heart? object)
  (object-type? (ucode-type broken-heart) object))

(define-integrable (%bytevector? object)
  (object-type? (ucode-type bytevector) object))

(define-integrable (%cell? object)
  (object-type? (ucode-type cell) object))

(define-integrable (%char? object)
  (object-type? (ucode-type character) object))

(define-integrable (%compiled-code-block? object)
  (object-type? (ucode-type compiled-code-block) object))

(define-integrable (%compiled-entry-address? object)
  (object-type? (ucode-type compiled-entry) object))

(define-integrable (%compiled-return-address? object)
  (object-type? (ucode-type compiled-return) object))

(define-integrable (%constant? object)
  (object-type? (ucode-type constant) object))

(define-integrable (%control-point? object)
  (object-type? (ucode-type control-point) object))

(define-integrable (%delayed? object)
  (object-type? (ucode-type delayed) object))

(define-integrable (%entity? object)
  (object-type? (ucode-type entity) object))

(define-integrable (%ephemeron? object)
  (object-type? (ucode-type ephemeron) object))

(define-integrable (%extended-procedure? object)
  (object-type? (ucode-type extended-procedure) object))

(define-integrable (%fixnum? object)
  (object-type? (ucode-type fixnum) object))

(define-integrable (%flonum? object)
  (object-type? (ucode-type flonum) object))

(define-integrable (%hunk3-a? object)
  (object-type? (ucode-type hunk3-a) object))

(define-integrable (%hunk3-b? object)
  (object-type? (ucode-type hunk3-b) object))

(define-integrable (%ic-environment? object)
  (object-type? (ucode-type environment) object))

(define-integrable (%interned-symbol? object)
  (object-type? (ucode-type interned-symbol) object))

(define-integrable (%legacy-string? object)
  (object-type? (ucode-type string) object))

(define-integrable (%manifest-nm-vector? object)
  (object-type? (ucode-type manifest-nm-vector) object))

(define-integrable (%pair? object)
  (object-type? (ucode-type pair) object))

(define-integrable (%primitive-procedure? object)
  (object-type? (ucode-type primitive) object))

(define-integrable (%ratnum? object)
  (object-type? (ucode-type ratnum) object))

(define-integrable (%recnum? object)
  (object-type? (ucode-type recnum) object))

(define-integrable (%%record? object)
  (object-type? (ucode-type record) object))

(define-integrable (%scode-access? object)
  (object-type? (ucode-type access) object))

(define-integrable (%scode-assignment? object)
  (object-type? (ucode-type assignment) object))

(define-integrable (%scode-combination? object)
  (object-type? (ucode-type combination) object))

(define-integrable (%scode-comment? object)
  (object-type? (ucode-type comment) object))

(define-integrable (%scode-conditional? object)
  (object-type? (ucode-type conditional) object))

(define-integrable (%scode-definition? object)
  (object-type? (ucode-type definition) object))

(define-integrable (%scode-delay? object)
  (object-type? (ucode-type delay) object))

(define-integrable (%scode-disjunction? object)
  (object-type? (ucode-type disjunction) object))

(define-integrable (%scode-extended-lambda? object)
  (object-type? (ucode-type extended-lambda) object))

(define-integrable (%scode-lexpr? object)
  (object-type? (ucode-type lexpr) object))

(define-integrable (%scode-quotation? object)
  (object-type? (ucode-type quotation) object))

(define-integrable (%scode-sequence? object)
  (object-type? (ucode-type sequence) object))

(define-integrable (%scode-simple-lambda? object)
  (object-type? (ucode-type lambda) object))

(define-integrable (%scode-the-environment? object)
  (object-type? (ucode-type the-environment) object))

(define-integrable (%scode-variable? object)
  (object-type? (ucode-type variable) object))

(define-integrable (%simple-procedure? object)
  (object-type? (ucode-type procedure) object))

(define-integrable (%stack-address? object)
  (object-type? (ucode-type stack-environment) object))

(define-integrable (%tagged-object? object)
  (object-type? (ucode-type tagged-object) object))

(define-integrable (%unicode-string? object)
  (object-type? (ucode-type unicode-string) object))

(define-integrable (%uninterned-symbol? object)
  (object-type? (ucode-type uninterned-symbol) object))

(define-integrable (%vector? object)
  (object-type? (ucode-type vector) object))

(define-integrable (%weak-pair? object)
  (object-type? (ucode-type weak-cons) object))

(define-integrable (%any-object? object)
   (declare (ignore object))
   #t)

(define-integrable (%no-object? object)
   (declare (ignore object))
   #f)

(define (code->gct-name code)
  (if (not (and (fixnum? code)
		(fix:>= code -4)
		(fix:<= code 5)))
      (error "Illegal GC type code:" code))
  (vector-ref gct-names (gct-code->index code)))

(define-integrable (name->gct-code name)
  (gct-index->code (name->gct-index name)))

(define (names->gct-mask names)
  (let loop ((names names) (mask 0))
    (if (pair? names)
	(loop (cdr names)
	      (fix:or (fix:lsh 1 (name->gct-index (car names))) mask))
	mask)))

(define (name->gct-index name)
  (let* ((v gct-names)
	 (n (vector-length v)))
    (let loop ((i 0))
      (if (not (fix:< i n))
	  (error "Illegal GC type name:" name))
      (if (eq? name (vector-ref v i))
	  i
	  (loop (fix:+ i 1))))))

(define-integrable (gct-code->index code)
  (fix:+ code 4))

(define-integrable (gct-index->code index)
  (fix:- index 4))

(define gct-names
  ;; Must match gc_type_t in microcode/gc.h.
  '#(compiled-entry vector gc-internal undefined non-pointer
		    cell pair triple quadruple compiled-return))

(define-integrable (%gc-non-pointer? object)
  (fix:= 0 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (%gc-non-pointer-type-code? code)
  (fix:= 0 ((ucode-primitive type->gc-type 1) code)))

(define (%gc-pointer? object)
  (match-gct-mask gc-pointer-mask ((ucode-primitive object-gc-type 1) object)))

(define (%gc-pointer-type-code? code)
  (match-gct-mask gc-pointer-mask ((ucode-primitive type->gc-type 1) code)))

(define gc-pointer-mask
  (names->gct-mask
   '(cell pair triple quadruple vector compiled-entry compiled-return)))

(define-integrable (match-gct-mask mask code)
  (not (fix:= 0 (fix:and mask (fix:lsh 1 (gct-code->index code))))))

(define-integrable (%system-cell? object)
  (fix:= 1 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (system-cell-type-code? code)
  (fix:= 1 ((ucode-primitive type->gc-type 1) code)))

(define-integrable (%system-pair? object)
  (fix:= 2 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (system-pair-type-code? code)
  (fix:= 2 ((ucode-primitive type->gc-type 1) code)))

(define-integrable (%system-triple? object)
  (fix:= 3 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (system-triple-type-code? code)
  (fix:= 3 ((ucode-primitive type->gc-type 1) code)))

(define-integrable (%system-quadruple? object)
  (fix:= 4 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (system-quadruple-type-code? code)
  (fix:= 4 ((ucode-primitive type->gc-type 1) code)))

(define-integrable (%system-vector? object)
  (fix:= -3 ((ucode-primitive object-gc-type 1) object)))

(define-integrable (system-vector-type-code? code)
  (fix:= -3 ((ucode-primitive type->gc-type 1) code)))

(define (object-non-pointer? object)
  (or (%gc-non-pointer? object)
      (%manifest-nm-vector? object)))

(define (object-pointer? object)
  (or (%gc-pointer? object)
      (%broken-heart? object)))

(define (non-pointer-type-code? code)
  (or (%gc-non-pointer-type-code? code)
      (fix:= (ucode-type manifest-nm-vector) code)))

(define (pointer-type-code? code)
  (or (%gc-pointer-type-code? code)
      (fix:= (ucode-type broken-heart) code)))

(define-primitives
  (%make-tagged-object 2)
  (%record -1)
  (%record-length 1)
  (%record-ref 2)
  (%record-set! 3)
  (%tagged-object-datum 1)
  (%tagged-object-tag 1)
  (cell-contents 1)
  (make-cell 1)
  (make-non-pointer-object 1)
  (object-datum 1)
  (object-new-type object-set-type 2)
  (object-type 1)
  (object-type? 2)
  (primitive-memory-hash 3)
  (primitive-object-hash 1)
  (primitive-object-hash-2 2)
  (primitive-object-ref 2)
  (primitive-object-set! 3)
  (set-cell-contents! 2)
  (system-list->vector system-list-to-vector 2)
  (system-pair-car 1)
  (system-pair-cdr 1)
  (system-pair-cons 3)
  (system-pair-set-car! 2)
  (system-pair-set-cdr! 2)
  (system-triple-cons system-hunk3-cons 4)
  (system-triple-first system-hunk3-cxr0 1)
  (system-triple-second system-hunk3-cxr1 1)
  (system-triple-third system-hunk3-cxr2 1)
  (system-triple-set-first! system-hunk3-set-cxr0! 2)
  (system-triple-set-second! system-hunk3-set-cxr1! 2)
  (system-triple-set-third! system-hunk3-set-cxr2! 2)
  (system-vector-length system-vector-size 1)
  (system-vector-ref 2)
  (system-vector-set! 3))

(define (%make-record tag length #!optional fill)
  (let ((fill (if (default-object? fill) #f fill)))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (declare (ignore use-env))
	     (let ((limit (cadr form))	;must be a power of 2
		   (gen-accessor
		    (lambda (i)
		      `(%record tag ,@(make-list (- i 1) 'fill)))))
	       `(if (and (fix:fixnum? length)
			 (fix:> length 0)
			 (fix:<= length ,limit))
		    ,(let loop ((low 1) (high limit))
		       (if (< low high)
			   (let ((mid (quotient (- (+ high low) 1) 2)))
			     `(if (fix:<= length ,mid)
				  ,(loop low mid)
				  ,(loop (+ mid 1) high)))
			   (gen-accessor low)))
		    (let ((record
			   ((ucode-primitive %make-record 2) length fill)))
		      (%record-set! record 0 tag)
		      record)))))))
      (expand-cases 16))))

(define-integrable (%make-delayed forced? value)
  (system-pair-cons (ucode-type delayed) forced? value))

(define-integrable (%delayed-forced? delayed)
  (system-pair-car delayed))

(define-integrable (%delayed-value delayed)
  (system-pair-cdr delayed))

(define-integrable (%make-entity procedure extra)
  (system-pair-cons (ucode-type entity) procedure extra))

(define-integrable (%entity-procedure entity)
  (system-pair-car entity))

(define-integrable (%entity-extra entity)
  (system-pair-cdr entity))

(define-integrable (%set-entity-procedure! entity procedure)
  (system-pair-set-car! entity procedure))

(define-integrable (%set-entity-extra! entity extra)
  (system-pair-set-cdr! entity extra))

(define-integrable apply-hook-type
  ;; TODO: replace #x07 with (ucode-type apply-hook).
  #x07)

(define-integrable (%apply-hook? object)
  (object-type? apply-hook-type object))

(define-integrable (%make-apply-hook procedure extra)
  (system-pair-cons apply-hook-type procedure extra))

(define-integrable (%apply-hook-procedure apply-hook)
  (system-pair-car apply-hook))

(define-integrable (%apply-hook-extra apply-hook)
  (system-pair-cdr apply-hook))

(define-integrable (%set-apply-hook-procedure! apply-hook procedure)
  (system-pair-set-car! apply-hook procedure))

(define-integrable (%set-apply-hook-extra! apply-hook extra)
  (system-pair-set-cdr! apply-hook extra))

(declare (integrate-operator %compound-procedure?))
(define (%compound-procedure? object)
  (or (%simple-procedure? object)
      (%extended-procedure? object)))

(define-integrable (%make-simple-procedure lambda environment)
  (system-pair-cons (ucode-type procedure) lambda environment))

(define-integrable (%compound-procedure-lambda procedure)
  (system-pair-car procedure))

(define-integrable (%compound-procedure-environment procedure)
  (system-pair-cdr procedure))

(define-integrable (%symbol-name symbol)
  (system-pair-car symbol))

(define-integrable (%symbol-value symbol)
  (system-pair-cdr symbol))

(define-integrable (%make-ratnum numerator denominator)
  (system-pair-cons (ucode-type ratnum) numerator denominator))

(define-integrable (%ratnum-numerator ratnum)
  (system-pair-car ratnum))

(define-integrable (%ratnum-denominator ratnum)
  (system-pair-cdr ratnum))

(define-integrable (%make-recnum real imaginary)
  (system-pair-cons (ucode-type recnum) real imaginary))

(define-integrable (%recnum-real recnum)
  (system-pair-car recnum))

(define-integrable (%recnum-imaginary recnum)
  (system-pair-cdr recnum))