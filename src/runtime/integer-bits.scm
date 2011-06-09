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

;;;; Operations on the Two's-Complement Representation of Integers
;;; package: (runtime integer-bits)

(declare (usual-integrations))

(define-primitives
  ;; Alphabetical order
  (bit-antimask integer-negative-zero-bits 2)
  (bit-count integer-bit-count 1)
  (bit-mask integer-nonnegative-one-bits 2)
  (bitwise-not integer-bitwise-not 1)
  (first-set-bit integer-first-set-bit 1)
  (hamming-distance integer-hamming-distance 2)
  (integer-length 1)
  (shift-left integer-shift-left 2)
  (shift-right integer-shift-right 2)

  ;; Truth table order
  (bitwise-and integer-bitwise-and 2)
  (bitwise-andc2 integer-bitwise-andc2 2)
  (bitwise-andc1 integer-bitwise-andc1 2)
  (bitwise-xor integer-bitwise-xor 2)
  (bitwise-ior integer-bitwise-ior 2)
  (bitwise-nor integer-bitwise-nor 2)
  (bitwise-eqv integer-bitwise-eqv 2)
  (bitwise-orc2 integer-bitwise-orc2 2)
  (bitwise-orc1 integer-bitwise-orc1 2)
  (bitwise-nand integer-bitwise-nand 2))

(define (arithmetic-shift integer shift)
  (if (negative? shift)
      (shift-right integer (- 0 shift))
      (shift-left integer shift)))

;; (define (shift number amount)
;;   (cond ((exact-integer? number) (arithmetic-shift number amount))
;; 	((flonum? number) (flonum-denormalize number amount))
;; 	...))

;;; Eventually the next two should be primitives with nice definitions
;;; on bignums requiring only a single copy and nice open-codings for
;;; the fixnum case.

(define (edit-bit-field selector size a-position a b-position b)
  (bitwise-merge (shift-left (extract-bit-field size 0 selector) a-position)
		 a
		 (shift-left (extract-bit-field size b-position b)
			     a-position)))

(define (splice-bit-field size a-position a b-position b)
  ;; (edit-bit-field (bit-mask size 0) size a-position a b-position b)
  (bitwise-merge (bit-mask size a-position)
		 a
		 (shift-left (extract-bit-field size b-position b)
			     a-position)))

(define-integrable (extract-bit-field size position integer)
  ;; (splice-bit-field size 0 0 position integer)
  (bitwise-and (bit-mask size 0) (shift-right integer position)))

(define (replace-bit-field size position integer field)
  ;; (splice-bit-field size position integer 0 field)
  (bitwise-ior (shift-left (extract-bit-field size 0 field) position)
	       (bitwise-andc2 integer (bit-mask size position))))

(define-integrable (test-bit-field? size position integer)
  (not (zero? (extract-bit-field size position integer))))

(declare (integrate-operator test-bit-field))
(define (test-bit-field size position integer mask)
  (declare (integrate position integer mask))
  (bitwise-and (extract-bit-field size 0 mask)
	       (extract-bit-field size position integer)))

(define-integrable (any-bits-set? size position integer mask)
  (not (zero? (test-bit-field size position integer mask))))

(declare (integrate-operator all-bits-set?))
(define (all-bits-set? size position integer mask)
  (declare (integrate size position integer))
  (= mask (test-bit-field size position integer mask)))

(declare (integrate-operator bitwise-merge))
(define (bitwise-merge mask a b)
  (declare (integrate a b))
  (bitwise-ior (bitwise-and a mask)
	       (bitwise-andc2 b mask)))

(define-integrable (set-bit bit integer)
  (bitwise-ior integer (shift-left 1 bit)))

(define-integrable (clear-bit bit integer)
  (bitwise-andc2 integer (shift-left 1 bit)))

(define-integrable (toggle-bit bit integer)
  (bitwise-xor integer (shift-left 1 bit)))

(define-integrable (extract-bit bit integer)
  (extract-bit-field 1 bit integer))

(define-integrable (bit-set? bit integer)
  (not (bit-clear? bit integer)))

(define-integrable (bit-clear? bit integer)
  (zero? (extract-bit-field 1 bit integer)))

;;; SRFI 60 operations

(define (copy-bit index integer set?)
  (if set?
      (set-bit index integer)
      (clear-bit index integer)))

(define (bit-field integer start end)
  (extract-bit-field (- end start) start integer))

(define (copy-bit-field to from start end)
  (replace-bit-field (- end start) start to from))

(define (rotate-bit-field integer count start end)
  (let ((size (- end start)))
    (replace-bit-field size start integer
      (let ((count (remainder count size))
	    (bit-field (extract-bit-field size start integer)))
	(bitwise-ior (shift-left bit-field count)
		     (shift-right bit-field (- size count)))))))

(define (bit-reverse size integer)
  (define (loop size integer result)
    (if (positive? size)
	(loop (- size 1)
	      (shift-right integer 1)
	      (bitwise-ior (shift-left result 1) (bitwise-and integer 1)))
	result))
  (if (negative? integer)
      (bitwise-not (loop size (bitwise-not integer) 0))
      (loop size integer 0)))

(define (reverse-bit-field integer start end)
  (let ((size (- end start)))
    (replace-bit-field size start
      (bit-reverse (extract-bit-field size start integer)))))

(define (integer->list integer #!optional length)
  (if (default-object? length)
      (do ((integer integer (shift-right integer 1))
	   (bits '() (cons (odd? integer) bits)))
	  ((zero? integer) bits))
      (begin
	(guarantee-index-fixnum length 'INTEGER->LIST)
	(do ((length length (- length 1))
	     (integer integer (shift-right integer 1))
	     (bits '() (cons (odd? integer) bits)))
	    ((zero? length) bits)))))

(define (list->integer bits)
  (do ((bits bits (cdr bits))
       (integer 0 (bitwise-ior (shift-left integer 1) (if (car bits) 1 0))))
      ((not (pair? bits)) integer)))
