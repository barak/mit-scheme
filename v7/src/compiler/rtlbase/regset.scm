#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/regset.scm,v 1.3 1990/02/02 18:39:46 cph Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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