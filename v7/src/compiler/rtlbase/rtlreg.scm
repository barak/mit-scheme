#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlreg.scm,v 1.1 1987/03/19 00:44:37 cph Exp $

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

;;;; RTL Registers

(declare (usual-integrations))

(define machine-register-map
  (make-vector number-of-machine-registers))

(let loop ((n 0))
  (if (< n number-of-machine-registers)
      (begin (vector-set! machine-register-map n (%make-register n))
	     (loop (1+ n)))))

(define-integrable (rtl:make-machine-register n)
  (vector-ref machine-register-map n))

(define *next-pseudo-number*)
(define *temporary->register-map*)

(define (rtl:make-pseudo-register)
  (let ((n *next-pseudo-number*))
    (set! *next-pseudo-number* (1+ *next-pseudo-number*))
    (%make-register n)))

(define (temporary->register temporary)
  (let ((entry (assq temporary *temporary->register-map*)))
    (if entry
	(cdr entry)
	(let ((register (rtl:make-pseudo-register)))
	  (set! *temporary->register-map*
		(cons (cons temporary register)
		      *temporary->register-map*))
	  register))))