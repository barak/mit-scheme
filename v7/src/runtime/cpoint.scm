#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/cpoint.scm,v 14.1 1988/05/20 00:55:10 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Control Points
;;; package: control-point-package

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

(define (control-point-ref control-point index)
  (system-vector-ref control-point
		     (+ (control-point/unused-length control-point) 2 index)))

(define (control-point/element-stream control-point)
  (let ((end (system-vector-length control-point)))
    (let loop ((index (+ (control-point/unused-length control-point) 8)))
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