#| -*-Scheme-*-

$Id: cpoint.scm,v 14.6 2002/11/20 19:46:19 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Control Points
;;; package: (runtime control-point)

(declare (usual-integrations))

(define-integrable (control-point? object)
  (object-type? (ucode-type control-point) object))

(define-integrable (control-point/reusable? control-point)
  (system-vector-ref control-point 0))

(define-integrable (control-point/unused-length control-point)
  (object-datum (system-vector-ref control-point 1)))

(define-integrable (control-point/interrupt-mask control-point)
  (control-point-ref control-point 1))

(define-integrable (control-point/history control-point)
  (control-point-ref control-point 3))

(define-integrable (control-point/previous-history-offset control-point)
  (control-point-ref control-point 4))

(define-integrable (control-point/previous-history-control-point control-point)
  (control-point-ref control-point 5))

(define-integrable (control-point-ref control-point index)
  (system-vector-ref control-point (control-point-index control-point index)))

(define-integrable (control-point-index control-point index)
  (+ (control-point/unused-length control-point) (+ 2 index)))

(define-integrable (control-point/first-element-index control-point)
  (control-point-index control-point 6))

#|

;; Disabled because some procedures in conpar.scm and uenvir.scm
;; depend on the actual length for finding compiled code variables,
;; etc.

(define (control-point/n-elements control-point)
  (let ((real-length (- (system-vector-length control-point)
			(control-point/first-element-index control-point))))
    (if (control-point/next-control-point? control-point)
	(- real-length 2)
	real-length)))
|#

(define (control-point/n-elements control-point)
  (- (system-vector-length control-point)
     (control-point/first-element-index control-point)))

(define (control-point/element-stream control-point)
  (let ((end (let ((end (system-vector-length control-point)))
	       (if (control-point/next-control-point? control-point)
		   (- end 2)
		   end))))
    (let loop ((index (control-point/first-element-index control-point)))
      (cond ((= index end) '())
	    (((ucode-primitive primitive-object-type? 2)
	      (ucode-type manifest-nm-vector)
	      (system-vector-ref control-point index))
	     (let ((n-skips
		    (object-datum (system-vector-ref control-point index))))
	       (cons-stream
		(make-non-pointer-object n-skips)
		(let skip-loop ((n n-skips) (index (1+ index)))
		  (if (zero? n)
		      (loop index)
		      (cons-stream false (skip-loop (-1+ n) (1+ index))))))))
	    (else
	     (cons-stream (system-vector-ref control-point index)
			  (loop (1+ index))))))))

(define (control-point/next-control-point control-point)
  (and (control-point/next-control-point? control-point)
       (system-vector-ref control-point
			  (-1+ (system-vector-length control-point)))))

(define (make-control-point reusable?
			    unused-length
			    interrupt-mask
			    history
			    previous-history-offset
			    previous-history-control-point
			    element-stream
			    next-control-point)
  (let ((unused-length
	 (if (eq? microcode-id/stack-type 'STACKLETS)
	     (max unused-length 7)
	     unused-length)))
    (let ((result (make-vector (+ 8
				  unused-length
				  (stream-length element-stream)
				  (if next-control-point 2 0)))))
      (vector-set! result 0 reusable?)
      (vector-set! result 1 (make-non-pointer-object unused-length))
      (vector-set! result (+ 2 unused-length)
		   (ucode-return-address restore-interrupt-mask))
      (vector-set! result (+ 3 unused-length) interrupt-mask)
      (vector-set! result (+ 4 unused-length)
		   (ucode-return-address restore-history))
      (vector-set! result (+ 5 unused-length) history)
      (vector-set! result (+ 6 unused-length) previous-history-offset)
      (vector-set! result (+ 7 unused-length) previous-history-control-point)
      (let loop ((stream element-stream) (index (+ 8 unused-length)))
	(cond ((stream-pair? stream)
	       (vector-set! result index (stream-car stream))
	       (loop (stream-cdr stream) (1+ index)))
	      (next-control-point
	       (vector-set! result index (ucode-return-address join-stacklets))
	       (vector-set! result (1+ index) next-control-point))))
      (object-new-type (ucode-type control-point) result))))

(define (control-point/next-control-point? control-point)
  ((ucode-primitive primitive-object-eq? 2)
   (system-vector-ref control-point (- (system-vector-length control-point) 2))
   (ucode-return-address join-stacklets)))