#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/sdata.scm,v 14.2 1991/08/12 13:38:39 jinx Exp $

Copyright (c) 1988-1991 Massachusetts Institute of Technology

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

;;;; Abstract Data Field
;;; package: (runtime scode-data)

(declare (usual-integrations))

(define (&typed-singleton-cons type element)
  (system-pair-cons type (unmap-reference-trap element) '()))

(define (&singleton-element singleton)
  (map-reference-trap (lambda () (system-pair-car singleton))))

(define (&singleton-set-element! singleton new-element)
  (system-pair-set-car! singleton (unmap-reference-trap new-element)))

(define (&typed-pair-cons type car cdr)
  (system-pair-cons type
		    (unmap-reference-trap car)
		    (unmap-reference-trap cdr)))

(define (&pair-car pair)
  (map-reference-trap (lambda () (system-pair-car pair))))

(define (&pair-set-car! pair new-car)
  (system-pair-set-car! pair (unmap-reference-trap new-car)))

(define (&pair-cdr pair)
  (map-reference-trap (lambda () (system-pair-cdr pair))))

(define (&pair-set-cdr! pair new-cdr)
  (system-pair-set-cdr! pair (unmap-reference-trap new-cdr)))

(define (&typed-triple-cons type first second third)
  (object-new-type type
		   (hunk3-cons (unmap-reference-trap first)
			       (unmap-reference-trap second)
			       (unmap-reference-trap third))))

(define (&triple-first triple)
  (map-reference-trap (lambda () (system-hunk3-cxr0 triple))))

(define (&triple-set-first! triple new-first)
  (system-hunk3-set-cxr0! triple (unmap-reference-trap new-first)))

(define (&triple-second triple)
  (map-reference-trap (lambda () (system-hunk3-cxr1 triple))))

(define (&triple-set-second! triple new-second)
  (system-hunk3-set-cxr1! triple (unmap-reference-trap new-second)))

(define (&triple-third triple)
  (map-reference-trap (lambda () (system-hunk3-cxr2 triple))))

(define (&triple-set-third! triple new-third)
  (system-hunk3-set-cxr2! triple (unmap-reference-trap new-third)))

(define (&typed-vector-cons type elements)
  (system-list->vector
   type
   (let loop ((elements elements))
     (if (null? elements)
	 '()
	 (cons (unmap-reference-trap (car elements))
	       (loop (cdr elements)))))))

(define (&vector-length vector)
  (system-vector-length vector))

(define (&vector-ref vector index)
  (map-reference-trap (lambda () (system-vector-ref vector index))))

(define (&subvector->list vector start stop)
  (let loop ((sublist (system-subvector->list vector start stop)))
    (if (null? sublist)
	'()
	(cons (map-reference-trap (lambda () (car sublist)))
	      (loop (cdr sublist))))))