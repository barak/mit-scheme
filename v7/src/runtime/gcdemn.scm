#| -*-Scheme-*-

$Id: gcdemn.scm,v 14.7 1993/11/18 15:09:01 gjr Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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
  (set! primitive-gc-daemons (make-queue))
  (set! trigger-primitive-gc-daemons! (make-trigger primitive-gc-daemons))
  (set! add-primitive-gc-daemon! (make-adder primitive-gc-daemons))
  (set! gc-daemons (make-queue))
  (set! trigger-gc-daemons! (make-trigger gc-daemons))
  (set! add-gc-daemon! (make-adder gc-daemons))
  (set! secondary-gc-daemons (make-queue))
  (set! trigger-secondary-gc-daemons! (make-trigger secondary-gc-daemons))
  (set! add-secondary-gc-daemon! (make-adder secondary-gc-daemons))
  (let ((fixed-objects (get-fixed-objects-vector)))
    (vector-set! fixed-objects #x0B trigger-primitive-gc-daemons!)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

;;; PRIMITIVE-GC-DAEMONS are executed during the GC.  They must not
;;; allocate any storage and they must be prepared to run at times
;;; when many data structures are not consistent.
(define primitive-gc-daemons)
(define trigger-primitive-gc-daemons!)
(define add-primitive-gc-daemon!)

;;; GC-DAEMONS are executed after each GC from an interrupt handler.
;;; This interrupt handler has lower priority than the GC interrupt,
;;; which guarantees that these daemons will not be run inside of
;;; critical sections.  As a result, the daemons may allocate storage
;;; and use most of the runtime facilities.
(define gc-daemons)
(define trigger-gc-daemons!)
(define add-gc-daemon!)
(define (add-gc-daemon!/no-restore daemon)
  (add-gc-daemon!
   (lambda ()
     (if (not *within-restore-window?*)
	 (daemon)))))  

;;; SECONDARY-GC-DAEMONS are executed rarely.  Their purpose is to
;;; reclaim storage that is either unlikely to be reclaimed or
;;; expensive to reclaim.
(define secondary-gc-daemons)
(define trigger-secondary-gc-daemons!)
(define add-secondary-gc-daemon!)

(define (make-trigger daemons)
  (lambda ()
    (for-each (lambda (thunk) (thunk))
	      (queue->list/unsafe daemons))))

(define (make-adder daemons)
  (lambda (daemon)
    (enqueue! daemons daemon)))

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