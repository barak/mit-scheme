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

;;;; Abstract Data Field
;;; package: (runtime scode-data)

(declare (usual-integrations))

(define-primitives
  (make-non-pointer-object 1)
  (object-new-type object-set-type 2)
  (primitive-datum-ref 2)
  (primitive-datum-set! 3)
  (primitive-object-ref 2)
  (primitive-object-ref-new-type 3)
  (primitive-object-set! 3)
  (primitive-type-ref 2)
  (primitive-type-set! 3)
  (system-pair? 1)
  (system-vector-length system-vector-size 1)
  (system-vector? 1)
  (triple-cons hunk3-cons 3))

(define-record-type <manifest-nmv>
    make-manifest-nmv
    manifest-nmv?
  (datum manifest-nmv-datum))

(define-print-method manifest-nmv?
  (standard-print-method 'manifest-nmv
    (lambda (m-nmv)
      (list (manifest-nmv-datum m-nmv)))))

(define (%safe-memory-ref object index)
  (let ((type (primitive-type-ref object index)))
    (cond ((fix:= type (ucode-type reference-trap))
	   (%safe-map-reference-trap object index))
	  ((fix:= type (ucode-type manifest-nm-vector))
	   (make-manifest-nmv (primitive-datum-ref object index)))
	  (else
	   (primitive-object-ref object index)))))

(define (%safe-memory-set! object index value)
  (cond ((reference-trap? value)
	 (%safe-unmap-reference-trap object index value))
	((manifest-nmv? value)
	 (primitive-object-set! object index
				(make-non-pointer-object
				 (manifest-nmv-datum value)))
	 (primitive-type-set! object index (ucode-type manifest-nm-vector)))
	(else
	 (primitive-object-set! object index value))))

(define (safe-cons car cdr)
  (let ((pair (cons #f #f)))
    (%safe-memory-set! pair 0 car)
    (%safe-memory-set! pair 1 cdr)
    pair))

(define-integrable (%safe-car pair)
  (%safe-memory-ref pair 0))

(define-integrable (%safe-cdr pair)
  (%safe-memory-ref pair 1))

(define-integrable (%safe-set-car! pair value)
  (%safe-memory-set! pair 0 value))

(define-integrable (%safe-set-cdr! pair value)
  (%safe-memory-set! pair 1 value))

(define (safe-car pair)
  (guarantee pair? pair 'safe-car)
  (%safe-memory-ref pair 0))

(define (safe-cdr pair)
  (guarantee pair? pair 'safe-cdr)
  (%safe-memory-ref pair 1))

(define (safe-set-car! pair value)
  (guarantee pair? pair 'safe-set-car!)
  (%safe-memory-set! pair 0 value))

(define (safe-set-cdr! pair value)
  (guarantee pair? pair 'safe-set-cdr!)
  (%safe-memory-set! pair 1 value))

(define (safe-system-pair-cons type car cdr)
  (if (not (eq? 'pair (type-code->gc-type type)))
      (error:bad-range-argument type 'safe-system-pair-cons))
  (object-new-type type (safe-cons car cdr)))

(define (safe-system-pair-car pair)
  (guarantee system-pair? pair 'safe-system-pair-car)
  (%safe-memory-ref pair 0))

(define (safe-system-pair-cdr pair)
  (guarantee system-pair? pair 'safe-system-pair-cdr)
  (%safe-memory-ref pair 1))

(define (safe-system-pair-set-car! pair value)
  (guarantee system-pair? pair 'safe-system-pair-set-car!)
  (%safe-memory-set! pair 0 value))

(define (safe-system-pair-set-cdr! pair value)
  (guarantee system-pair? pair 'safe-system-pair-set-cdr!)
  (%safe-memory-set! pair 1 value))

(define (system-triple? object)
  (eq? 'triple (object-gc-type object)))

(define (safe-system-triple-cons type first second third)
  (if (not (eq? 'triple (type-code->gc-type type)))
      (error:bad-range-argument type 'safe-system-triple-cons))
  (let ((triple (triple-cons #f #f #f)))
    (%safe-memory-set! triple 0 first)
    (%safe-memory-set! triple 1 second)
    (%safe-memory-set! triple 2 third)
    (object-new-type type triple)))

(define (safe-triple-cons first second third)
  (safe-system-triple-cons (ucode-type hunk3) first second third))

(define (safe-system-triple-first triple)
  (guarantee system-triple? triple 'safe-system-triple-first)
  (%safe-memory-ref triple 0))

(define (safe-system-triple-second triple)
  (guarantee system-triple? triple 'safe-system-triple-second)
  (%safe-memory-ref triple 1))

(define (safe-system-triple-third triple)
  (guarantee system-triple? triple 'safe-system-triple-third)
  (%safe-memory-ref triple 2))

(define (safe-system-triple-set-first! triple value)
  (guarantee system-triple? triple 'safe-system-triple-set-first!)
  (%safe-memory-set! triple 0 value))

(define (safe-system-triple-set-second! triple value)
  (guarantee system-triple? triple 'safe-system-triple-set-second!)
  (%safe-memory-set! triple 1 value))

(define (safe-system-triple-set-third! triple value)
  (guarantee system-triple? triple 'safe-system-triple-set-third!)
  (%safe-memory-set! triple 2 value))

(define-integrable (%safe-vector-ref vector index)
  (%safe-memory-ref vector (fix:+ 1 index)))

(define-integrable (%safe-vector-set! vector index value)
  (%safe-memory-set! vector (fix:+ 1 index) value))

(define (%safe-vector->list vector start end)
  (fold-right (lambda (index list)
		(cons (%safe-vector-ref vector index) list))
	      '()
	      (iota (fix:- end start))))

(define (safe-vector-ref vector index)
  (guarantee vector? vector 'safe-vector-ref)
  (guarantee non-negative-fixnum? index 'safe-vector-ref)
  (if (not (fix:< index (vector-length vector)))
      (error:bad-range-argument index 'safe-vector-ref))
  (%safe-vector-ref vector index))

(define (safe-vector-set! vector index value)
  (guarantee vector? vector 'safe-vector-set!)
  (guarantee non-negative-fixnum? index 'safe-vector-set!)
  (if (not (fix:< index (vector-length vector)))
      (error:bad-range-argument index 'safe-vector-set!))
  (%safe-vector-set! vector index value))

(define (safe-vector->list vector #!optional start end)
  (guarantee vector? vector 'safe-vector->list)
  (let ((end (fix:end-index end (vector-length vector) 'safe-vector->list))
	(start (fix:start-index start end 'safe-vector->list)))
    (%safe-vector->list vector start end)))

(define (safe-system-vector-cons type length)
  (if (not (eq? 'vector (type-code->gc-type type)))
      (error:bad-range-argument type 'safe-system-vector-cons))
  (object-new-type type (make-vector length)))

(define (safe-system-vector-ref vector index)
  (guarantee system-vector? vector 'safe-system-vector-ref)
  (guarantee non-negative-fixnum? index 'safe-system-vector-ref)
  (if (not (fix:< index (system-vector-length vector)))
      (error:bad-range-argument index 'safe-system-vector-ref))
  (%safe-vector-ref vector index))

(define (safe-system-vector-set! vector index value)
  (guarantee system-vector? vector 'safe-system-vector-set!)
  (guarantee non-negative-fixnum? index 'safe-system-vector-set!)
  (if (not (fix:< index (system-vector-length vector)))
      (error:bad-range-argument index 'safe-system-vector-set!))
  (%safe-vector-set! vector index value))

(define (safe-system-vector->list vector #!optional start end)
  (guarantee system-vector? vector 'safe-system-vector->list)
  (let ((end
	 (fix:end-index end (system-vector-length vector)
			'safe-system-vector->list))
	(start (fix:start-index start end 'safe-system-vector->list)))
    (%safe-vector->list vector start end)))