;;; -*-Scheme-*-
;;;
;;; $Id: tvector.scm,v 1.3 2002/11/20 19:46:23 cph Exp $
;;;
;;; Copyright (c) 1995-1999 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT Scheme.
;;;
;;; MIT Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation; either version 2 of the License,
;;; or (at your option) any later version.
;;;
;;; MIT Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT Scheme; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Tagged Vectors

(declare (usual-integrations))

;;; These procedures are optimized for safety.  Applications that need
;;; speed are assumed to break this abstraction and use "%record"
;;; calls to construct and access tagged vectors.

(define (make-tagged-vector tag length)
  (guarantee-dispatch-tag tag 'MAKE-TAGGED-VECTOR)
  (guarantee-index-integer length 'MAKE-TAGGED-VECTOR)
  (let ((result
	 (object-new-type (ucode-type record)
			  (make-vector (fix:+ length 1)
				       record-slot-uninitialized))))
    (%record-set! result 0 tag)
    result))

(define (tagged-vector tag . elements)
  (guarantee-dispatch-tag tag 'MAKE-TAGGED-VECTOR)
  (object-new-type (ucode-type record) (apply vector tag elements)))

(define (tagged-vector? object)
  (and (%record? object)
       (dispatch-tag? (%record-ref object 0))))

(define (tagged-vector-tag vector)
  (guarantee-tagged-vector vector 'TAGGED-VECTOR-TAG)
  (%record-ref vector 0))

(define (set-tagged-vector-tag! vector tag)
  (guarantee-tagged-vector vector 'SET-TAGGED-VECTOR-TAG!)
  (guarantee-dispatch-tag tag 'SET-TAGGED-VECTOR-TAG!)
  (%record-set! vector 0 tag))

(define (tagged-vector-length vector)
  (guarantee-tagged-vector vector 'TAGGED-VECTOR-LENGTH)
  (fix:- (%record-length vector) 1))

(define (tagged-vector-element vector index)
  (guarantee-tagged-vector-ref vector index 'TAGGED-VECTOR-ELEMENT)
  (%record-ref vector (fix:+ index 1)))

(define (set-tagged-vector-element! vector index value)
  (guarantee-tagged-vector-ref vector index 'SET-TAGGED-VECTOR-ELEMENT!)
  (%record-set! vector (fix:+ index 1) value))

(define (tagged-vector-element-initialized? vector index)
  (guarantee-tagged-vector-ref vector index
			       'TAGGED-VECTOR-ELEMENT-INITIALIZED?)
  (not (eq? (%record-ref vector (fix:+ index 1)) record-slot-uninitialized)))

(define (guarantee-tagged-vector vector caller)
  (if (not (tagged-vector? vector))
      (error:wrong-type-argument vector "tagged vector" caller)))

(define (guarantee-tagged-vector-ref vector index caller)
  (guarantee-tagged-vector vector caller)
  (guarantee-index-integer index caller)
  (if (not (fix:< index (fix:- (%record-length vector) 1)))
      (error:bad-range-argument index caller)))

(define (guarantee-index-integer index caller)
  (if (not (and (fix:fixnum? index) (fix:>= index 0)))
      (error:wrong-type-argument vector "non-negative fixnum" caller)))

(define record-slot-uninitialized)

(define (initialize-tagged-vector!)
  (set! record-slot-uninitialized (intern "#[record-slot-uninitialized]"))
  unspecific)