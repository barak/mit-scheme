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

(define-integrable (default-object)
  #!default)

(define-integrable (eof-object)
  ((ucode-primitive primitive-object-set-type) (ucode-type constant) 6))

(define-integrable (gc-reclaimed-object)
  #!reclaimed)

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

(define-integrable apply-hook-type-code
  ;; TODO: replace #x07 with (ucode-type apply-hook).
  #x07)

(define-integrable (%make-apply-hook procedure extra)
  (system-pair-cons apply-hook-type-code procedure extra))

(define-integrable (%apply-hook-procedure apply-hook)
  (system-pair-car apply-hook))

(define-integrable (%apply-hook-extra apply-hook)
  (system-pair-cdr apply-hook))

(define-integrable (%set-apply-hook-procedure! apply-hook procedure)
  (system-pair-set-car! apply-hook procedure))

(define-integrable (%set-apply-hook-extra! apply-hook extra)
  (system-pair-set-cdr! apply-hook extra))

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