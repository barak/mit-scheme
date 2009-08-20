#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Continuations
;;; package: (runtime continuation)

(declare (usual-integrations))

(define (call-with-current-continuation receiver)
  ((ucode-primitive call-with-current-continuation 1)
   (lambda (control-point)
     (let ((k
	    (make-continuation control-point
			       (get-dynamic-state)
			       (get-thread-event-block))))
       (%within-continuation k #f (lambda () (receiver k)))))))

(define (within-continuation k thunk)
  (guarantee-continuation k 'WITHIN-CONTINUATION)
  (%within-continuation k #f thunk))

(define (make-continuation control-point dynamic-state block-thread-events?)
  (make-entity (lambda (k value) (%within-continuation k #f (lambda () value)))
	       (make-%continuation control-point
				   dynamic-state
				   block-thread-events?)))

(define (%within-continuation k thread-switch? thunk)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point k)
   (lambda ()
     (set-dynamic-state! (continuation/dynamic-state k) thread-switch?)
     (set-thread-event-block! (continuation/block-thread-events? k))
     (thunk))))

(define (continuation? object)
  (and (entity? object)
       (if (%continuation? (entity-extra object))
	   #t
	   (continuation? (entity-procedure object)))))

(define-guarantee continuation "continuation")

(define-integrable (continuation/control-point k)
  (%continuation/control-point (entity-extra k)))

(define-integrable (continuation/dynamic-state k)
  (%continuation/dynamic-state (entity-extra k)))

(define-integrable (continuation/block-thread-events? k)
  (%continuation/block-thread-events? (entity-extra k)))

(define-structure (%continuation (constructor make-%continuation)
				 (conc-name %continuation/))
  (control-point #f read-only #t)
  (dynamic-state #f read-only #t)
  (block-thread-events? #f read-only #t))