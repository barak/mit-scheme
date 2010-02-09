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
  (let ((fixed-objects ((ucode-primitive get-fixed-objects-vector))))
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