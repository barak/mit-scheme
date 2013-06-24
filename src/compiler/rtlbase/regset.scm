#| -*-Scheme-*-

$Id: regset.scm,v 1.6 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1988, 1990, 1999 Massachusetts Institute of Technology

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

;;;; RTL Register Sets

(declare (usual-integrations))

(define-integrable (make-regset n-registers)
  (make-bit-string n-registers false))

(define (for-each-regset-member regset procedure)
  (let ((end (bit-string-length regset)))
    (let loop ((start 0))
      (let ((register (bit-substring-find-next-set-bit regset start end)))
	(if register
	    (begin
	      (procedure register)
	      (loop (1+ register))))))))

(define (regset->list regset)
  (let ((end (bit-string-length regset)))
    (let loop ((start 0))
      (let ((register (bit-substring-find-next-set-bit regset start end)))
	(if register
	    (cons register (loop (1+ register)))
	    '())))))

(define-integrable (regset-clear! regset)
  (bit-string-fill! regset false))

(define-integrable (regset-disjoint? x y)
  (regset-null? (regset-intersection x y)))

(define-integrable regset-allocate bit-string-allocate)
(define-integrable regset-adjoin! bit-string-set!)
(define-integrable regset-delete! bit-string-clear!)
(define-integrable regset-member? bit-string-ref)
(define-integrable regset=? bit-string=?)
(define-integrable regset-null? bit-string-zero?)

(define-integrable regset-copy! bit-string-move!)
(define-integrable regset-union! bit-string-or!)
(define-integrable regset-difference! bit-string-andc!)
(define-integrable regset-intersection! bit-string-and!)

(define-integrable regset-copy bit-string-copy)
(define-integrable regset-union bit-string-or)
(define-integrable regset-difference bit-string-andc)
(define-integrable regset-intersection bit-string-and)

#| Alternate representation.

(define-integrable (make-regset n-registers)
  n-registers
  (list 'REGSET))

(define-integrable (regset-allocate n-registers)
  n-registers
  (list 'REGSET))

(define-integrable (for-each-regset-member regset procedure)
  (for-each procedure (cdr regset)))

(define-integrable (regset->list regset)
  (list-copy (cdr regset)))

(define-integrable (regset-clear! regset)
  (set-cdr! regset '()))

(define-integrable (regset-disjoint? x y)
  (eq-set-disjoint? (cdr x) (cdr y)))

(define (regset-adjoin! regset register)
  (if (not (memq register (cdr regset)))
      (set-cdr! regset (cons register (cdr regset)))))

(define (regset-delete! regset register)
  (set-cdr! regset (delq register (cdr regset))))

(define-integrable (regset-member? regset register)
  (memq register (cdr regset)))

(define-integrable (regset=? x y)
  (eq-set-same-set? (cdr x) (cdr y)))

(define-integrable (regset-null? regset)
  (null? (cdr regset)))

(define-integrable (regset-copy! destination source)
  (set-cdr! destination (cdr source)))

(define (regset-union! destination source)
  (set-cdr! destination (eq-set-union (cdr source) (cdr destination))))

(define (regset-difference! destination source)
  (set-cdr! destination (eq-set-difference (cdr destination) (cdr source))))

(define (regset-intersection! destination source)
  (set-cdr! destination (eq-set-intersection (cdr source) (cdr destination))))

(define-integrable regset-copy list-copy)

(define-integrable (regset-union x y)
  (cons 'REGSET (eq-set-union (cdr x) (cdr y))))

(define-integrable (regset-difference x y)
  (cons 'REGSET (eq-set-difference (cdr x) (cdr y))))

(define-integrable (regset-intersection x y)
  (cons 'REGSET (eq-set-intersection (cdr x) (cdr y))))

|#