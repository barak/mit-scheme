#| -*-Scheme-*-

$MC68020-Header: coerce.scm,v 1.10 88/08/31 05:56:37 GMT cph Exp $

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

(declare (usual-integrations))

;;;; Strange hppa coercions

(define (coerce-right-signed nbits)
  (let ((offset (1+ (expt 2 nbits))))
    (lambda (n)
      (unsigned-integer->bit-string nbits
				    (if (negative? n)
					(+ (* n 2) offset)
					(* n 2))))))

(define (coerce-assemble12:x nbits)
  (let ((range (expt 2 11)))
    (lambda (n)
      (let ((n (machine-word-offset n range))
	    (r (unsigned-integer->bit-string nbits 0)))
	(bit-substring-move-right! n 0 10 r 1)
	(bit-substring-move-right! n 10 11 r 0)
	r))))

(define (coerce-assemble12:y nbits)
  (let ((range (expt 2 11)))
    (lambda (n)
      (let ((r (unsigned-integer->bit-string nbits 0)))
	(bit-substring-move-right! (machine-word-offset n range) 11 12 r 0)
	r))))

(define (coerce-assemble17:x nbits)
  (let ((range (expt 2 16)))
    (lambda (n)
      (let ((r (unsigned-integer->bit-string nbits 0)))
	(bit-substring-move-right! (machine-word-offset n range) 11 16 r 0)
	r))))

(define (coerce-assemble17:y nbits)
  (let ((range (expt 2 16)))
    (lambda (n)
      (let ((n (machine-word-offset n range))
	    (r (unsigned-integer->bit-string nbits 0)))
	(bit-substring-move-right! n 0 10 r 1)
	(bit-substring-move-right! n 10 11 r 0)
	r))))

(define (coerce-assemble17:z nbits)
  (let ((range (expt 2 16)))
    (lambda (n)
      (let ((r (unsigned-integer->bit-string nbits 0)))
	(bit-substring-move-right! (machine-word-offset n range) 16 17 r 0)
	r))))

(define (coerce-assemble21:x nbits)
  ;; This one does not check for range.  Should it?
  (lambda (n)
    (let ((n (integer->word n))
	  (r (unsigned-integer->bit-string nbits 0)))
      (bit-substring-move-right! n 0 2 r 12)
      (bit-substring-move-right! n 2 7 r 16)
      (bit-substring-move-right! n 7 9 r 14)
      (bit-substring-move-right! n 9 20 r 1)
      (bit-substring-move-right! n 20 21 r 0)
      r)))

(define (machine-word-offset n range)
  (let ((value (integer-divide n 4)))
    (if (not (zero? (integer-divide-remainder value)))
	(error "machine-word-offset: Invalid offset" n))
    (let ((result (integer-divide-quotient value)))
      (if (and (< result range)
	       (>= result (- range)))
	  (integer->word result)
	  (error "machine-word-offset: Doesn't fit" n range)))))

(define (integer->word x)
  (unsigned-integer->bit-string
   32
   (let ((x (if (negative? x) (+ x #x100000000) x)))
     (if (not (and (not (negative? x)) (< x #x100000000)))
	 (error "Integer too large to be encoded" x))
     x)))

;;; Coercion top level

(define make-coercion
  (coercion-maker
   `((ASSEMBLE12:X . ,coerce-assemble12:x)
     (ASSEMBLE12:Y . ,coerce-assemble12:y)
     (ASSEMBLE17:X . ,coerce-assemble17:x)
     (ASSEMBLE17:Y . ,coerce-assemble17:y)
     (ASSEMBLE17:Z . ,coerce-assemble17:z)
     (ASSEMBLE21:X . ,coerce-assemble21:x)
     (RIGHT-SIGNED . ,coerce-right-signed)
     (UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer))))

(define coerce-1-bit-unsigned (make-coercion 'UNSIGNED 1))
(define coerce-2-bit-unsigned (make-coercion 'UNSIGNED 2))
(define coerce-3-bit-unsigned (make-coercion 'UNSIGNED 3))
(define coerce-4-bit-unsigned (make-coercion 'UNSIGNED 4))
(define coerce-5-bit-unsigned (make-coercion 'UNSIGNED 5))
(define coerce-6-bit-unsigned (make-coercion 'UNSIGNED 6))
(define coerce-7-bit-unsigned (make-coercion 'UNSIGNED 7))
(define coerce-8-bit-unsigned (make-coercion 'UNSIGNED 8))
(define coerce-9-bit-unsigned (make-coercion 'UNSIGNED 9))
(define coerce-10-bit-unsigned (make-coercion 'UNSIGNED 10))
(define coerce-11-bit-unsigned (make-coercion 'UNSIGNED 11))
(define coerce-12-bit-unsigned (make-coercion 'UNSIGNED 12))
(define coerce-13-bit-unsigned (make-coercion 'UNSIGNED 13))
(define coerce-14-bit-unsigned (make-coercion 'UNSIGNED 14))
(define coerce-15-bit-unsigned (make-coercion 'UNSIGNED 15))
(define coerce-16-bit-unsigned (make-coercion 'UNSIGNED 16))
(define coerce-32-bit-unsigned (make-coercion 'UNSIGNED 32))

(define coerce-8-bit-signed (make-coercion 'SIGNED 8))
(define coerce-16-bit-signed (make-coercion 'SIGNED 16))
(define coerce-32-bit-signed (make-coercion 'SIGNED 32))

(define coerce-5-bit-right-signed (make-coercion 'RIGHT-SIGNED 5))
(define coerce-11-bit-right-signed (make-coercion 'RIGHT-SIGNED 11))
(define coerce-14-bit-right-signed (make-coercion 'RIGHT-SIGNED 14))
(define coerce-11-bit-assemble12:x (make-coercion 'ASSEMBLE12:X 11))
(define coerce-1-bit-assemble12:y (make-coercion 'ASSEMBLE12:Y 1))
(define coerce-5-bit-assemble17:x (make-coercion 'ASSEMBLE17:X 5))
(define coerce-11-bit-assemble17:y (make-coercion 'ASSEMBLE17:Y 11))
(define coerce-1-bit-assemble17:z (make-coercion 'ASSEMBLE17:Z 1))
(define coerce-21-bit-assemble21:x (make-coercion 'ASSEMBLE21:X 21))