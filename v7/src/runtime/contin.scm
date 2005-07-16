#| -*-Scheme-*-

$Id: contin.scm,v 14.14 2005/07/16 03:44:12 cph Exp $

Copyright 1988,1989,1991,1992,1999,2005 Massachusetts Institute of Technology

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
       (%within-continuation k (lambda () (receiver k)))))))

(define (within-continuation k thunk)
  (guarantee-continuation k 'WITHIN-CONTINUATION)
  (%within-continuation k thunk))

(define (make-continuation control-point dynamic-state block-thread-events?)
  (make-entity (lambda (k value) (%within-continuation k (lambda () value)))
	       (make-%continuation control-point
				   dynamic-state
				   block-thread-events?)))

(define-integrable (%within-continuation k thunk)
  ((ucode-primitive within-control-point 2)
   (continuation/control-point k)
   (lambda ()
     (set-dynamic-state! (continuation/dynamic-state k) #f)
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