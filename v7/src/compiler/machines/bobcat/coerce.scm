#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/coerce.scm,v 1.10 1988/08/31 05:56:37 cph Rel $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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