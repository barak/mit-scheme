;;; -*-Scheme-*-
;;;
;;; $Id: gentag.scm,v 1.1 1996/04/23 20:37:51 cph Exp $
;;;
;;; Copyright (c) 1993-96 Massachusetts Institute of Technology
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

;;;; Tags for Generic Procedure Dispatch

;;; From "Efficient Method Dispatch in PCL", Gregor Kiczales and Luis
;;; Rodriguez, Proceedings of the 1990 ACM Conference on Lisp and
;;; Functional Programming.  Parts of this code are based on the
;;; September 16, 1992 PCL implementation.

(declare (usual-integrations))

(define (make-dispatch-tag contents)
  (let ((tag
	 (object-new-type
	  (ucode-type record)
	  ((ucode-primitive vector-cons) dispatch-tag-index-end #f))))
    (%record-set! tag 0 dispatch-tag-marker)
    (%record-set! tag 1 contents)
    (do ((i dispatch-tag-index-start (fix:+ i 1)))
	((fix:= i dispatch-tag-index-end))
      (%record-set! tag i (get-dispatch-tag-cache-number)))
    tag))

(define-integrable (dispatch-tag? object)
  (and (%record? object)
       (eq? dispatch-tag-marker (%record-ref object 0))))

(define-integrable dispatch-tag-marker
  ((ucode-primitive string->symbol) "#[dispatch-tag]"))

(define-integrable dispatch-tag-index-start 2)
(define-integrable dispatch-tag-index-end 10)
(define-integrable dispatch-tag-ref %record-ref)
(define-integrable dispatch-tag-set! %record-set!)

(define (dispatch-tag-contents tag)
  (guarantee-dispatch-tag tag 'DISPATCH-TAG-CONTENTS)
  (%record-ref tag 1))

(define (set-dispatch-tag-contents! tag contents)
  (guarantee-dispatch-tag tag 'SET-DISPATCH-TAG-CONTENTS!)
  (%record-set! tag 1 contents))

(define-integrable (guarantee-dispatch-tag tag caller)
  (if (not (dispatch-tag? tag))
      (error:wrong-type-argument tag "dispatch tag" caller)))

(declare (integrate-operator next-dispatch-tag-index))
(define (next-dispatch-tag-index index)
  (and (fix:< (fix:+ index 1) dispatch-tag-index-end)
       (fix:+ index 1)))

(define-integrable dispatch-tag-cache-number-adds-ok
  ;; This constant controls the number of non-zero bits tag cache
  ;; numbers will have.
  ;;
  ;; The value of this constant is the number of tag cache numbers
  ;; that can be added and still be certain the result will be a
  ;; fixnum.  This is implicitly used by all the code that computes
  ;; primary cache locations from multiple tags.
  4)

(define get-dispatch-tag-cache-number)

(define (initialize-tag-constants!)
  (set! get-dispatch-tag-cache-number
	(let ((modulus
	       (int:quotient
		(let loop ((n 2)) (if (fix:fixnum? n) (loop (int:* n 2)) n))
		dispatch-tag-cache-number-adds-ok))
	      (state (make-random-state)))
	  (lambda ()
	    (random modulus state))))
  unspecific)