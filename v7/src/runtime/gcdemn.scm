#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcdemn.scm,v 14.4 1991/12/10 23:24:16 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

;;;; Garbage Collector Daemons
;;; package: (runtime gc-daemons)

(declare (usual-integrations))

(define (initialize-package!)
  (set! gc-daemons '())
  (set! secondary-gc-daemons '())
  (let ((fixed-objects (get-fixed-objects-vector)))
    (vector-set! fixed-objects #x0B trigger-gc-daemons)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define gc-daemons)
(define secondary-gc-daemons)

(define (trigger-gc-daemons)
  (do ((daemons gc-daemons (cdr daemons)))
      ((null? daemons))
    ((car daemons))))

(define (trigger-secondary-gc-daemons!)
  (do ((daemons secondary-gc-daemons (cdr daemons)))
      ((null? daemons))
    ((car daemons))))

(define (add-gc-daemon! daemon)
  (set! gc-daemons (cons daemon gc-daemons))
  unspecific)

(define (add-secondary-gc-daemon! daemon)
  (set! secondary-gc-daemons (cons daemon secondary-gc-daemons))
  unspecific)

(define (gc-clean #!optional threshold)
  (let ((threshold
	 (cond ((default-object? threshold) 100)
	       ((not (negative? threshold)) threshold)
	       (else (error "threshold must be non-negative" threshold)))))
    (let loop ((previous-free (gc-flip)))
      (trigger-secondary-gc-daemons!)
      (let ((this-free (gc-flip)))
	;; Don't bother to continue if the savings starts getting small.
	(if (<= (- this-free previous-free) threshold)
	    this-free
	    (loop this-free))))))