#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/insseq.scm,v 1.1 1987/06/25 10:48:10 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Lap instruction sequences

(declare (usual-integrations))

(define-integrable empty-lap-instructions '())

(define (lap-instructions->directives insts)
  (car insts))

(define (->instruction-sequence bits)
  (if (null? bits)
      empty-lap-instructions
      (cons bits (last-pair bits))))

(define (->lap-instructions pattern)
  (->instruction-sequence ((access syntax-instruction lap-syntax-package)
			   pattern)))

(define (append-lap-instructions! directives directives*)
  (cond ((null? directives) directives*)
	((null? directives*) directives)
	(else
	 (if (and (bit-string? (cadr directives))
		  (bit-string? (caar directives*)))
	     (let ((result (bit-string-append (caar directives*)
					      (cadr directives))))
	       (set-car! (cdr directives) result)
	       (if (not (eq? (car directives*) (cdr directives*)))
		   (begin (set-cdr! (cdr directives) (cdr (car directives*)))
			  (set-cdr! directives (cdr directives*)))))
	     (begin (set-cdr! (cdr directives) (car directives*))
		    (set-cdr! directives (cdr directives*))))
	 directives)))