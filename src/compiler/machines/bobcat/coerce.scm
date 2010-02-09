#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; 68000 Specific Coercions

(declare (usual-integrations))

(define coerce-quick
  (standard-coercion
   (lambda (n)
     (cond ((< 0 n 8) n)
	   ((= n 8) 0)
	   (else (error "Bad quick immediate" n))))))

(define coerce-short-label
  (standard-coercion
   (lambda (offset)
     (or (if (negative? offset)
	     (and (>= offset -128) (+ offset 256))
	     (and (< offset 128) offset))
	 (error "Short label out of range" offset)))))

(define coerce-bit-field-width
  (standard-coercion
   (lambda (w)
     (cond ((< 0 w 32) w)
	   ((= w 32) 0)
	   (else (error "Bad bit field width" w))))))

(define coerce-index-scale
  (standard-coercion
   (lambda (sf)
     (case sf
       ((1) #b00)
       ((2) #b01)
       ((4) #b10)
       ((8) #b11)
       (else (error "Bad index scale" sf))))))

;; *** NOTE ***
;; If you add coercions here, remember to also add them to
;; EXPAND-DESCRIPTOR in "insmac.scm".

(define make-coercion
  (coercion-maker
   `((UNSIGNED . ,coerce-unsigned-integer)
     (SIGNED . ,coerce-signed-integer)
     (QUICK . ,coerce-quick)
     (SHIFT-NUMBER . ,coerce-quick)
     (SHORT-LABEL . ,coerce-short-label)
     (BFWIDTH . ,coerce-bit-field-width)
     (SCALE-FACTOR . ,coerce-index-scale))))

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

(define coerce-3-bit-quick (make-coercion 'QUICK 3))
(define coerce-5-bit-bfwidth (make-coercion 'BFWIDTH 5))
(define coerce-3-bit-shift-number (make-coercion 'SHIFT-NUMBER 3))
(define coerce-8-bit-short-label (make-coercion 'SHORT-LABEL 8))
(define coerce-2-bit-scale-factor (make-coercion 'SCALE-FACTOR 2))