#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/insseq.scm,v 4.3 1990/01/18 22:41:55 cph Rel $

Copyright (c) 1987, 1988, 1990 Massachusetts Institute of Technology

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

(define (instruction-sequence->directives instruction-sequence)
  (if (null? instruction-sequence)
      '()
      (car instruction-sequence)))

(define empty-instruction-sequence
  '())

(define (directive->instruction-sequence directive)
  (let ((pair (cons directive '())))
    (cons pair pair)))

(define (instruction->instruction-sequence directives)
  ;; This procedure is expanded in the syntaxer.  See "syerly".
  (cons directives (last-pair directives)))

(define (copy-instruction-sequence instruction-sequence)
  (if (null? instruction-sequence)
      '()
      (let with-last-pair ((l (car instruction-sequence)) (receiver cons))
	(if (null? (cdr l))
	    (receiver l l)
	    (with-last-pair (cdr l)
	      (lambda (rest last)
		(receiver (cons (car l) rest) last)))))))

(define (append-instruction-sequences! x y)
  (cond ((null? x) y)
	((null? y) x)
	(else
	 (set-cdr! (cdr x) (car y))
	 (set-cdr! x (cdr y))
	 x)))