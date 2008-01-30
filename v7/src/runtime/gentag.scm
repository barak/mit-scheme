#| -*-Scheme-*-

$Id: gentag.scm,v 1.9 2008/01/30 20:02:31 cph Exp $

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