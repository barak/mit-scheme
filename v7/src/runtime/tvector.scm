;;; -*-Scheme-*-
;;;
;;; $Id: tvector.scm,v 1.1 1996/04/23 20:38:03 cph Exp $
;;;
;;; Copyright (c) 1995-96 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

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