#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/insseq.scm,v 1.3 1987/08/13 02:00:21 jinx Exp $

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

(define lap:syntax-instruction)
(define instruction-append)

(define (instruction-sequence->directives insts)
  (if (null? insts)
      '()
      (car insts)))

;; instruction->instruction-sequence is expanded.

(declare (integrate empty-instruction-sequence)
	 (integrate-operator directive->instruction-sequence))

(define empty-instruction-sequence '())

(define (directive->instruction-sequence directive)
  (declare (integrate directive))
  (let ((pair (cons directive '())))
    (cons pair pair)))

(define (instruction->instruction-sequence inst)
  (cons inst (last-pair inst)))

(define (copy-instruction-sequence seq)
  (define (with-last-pair l receiver)
    (if (null? (cdr l))
	(receiver l l)
	(with-last-pair (cdr l)
			(lambda (rest last)
			  (receiver (cons (car l) rest)
				    last)))))

  (if (null? seq)
      '()
      (with-last-pair (car seq) cons)))

(define (append-instruction-sequences! seq1 seq2)
  (cond ((null? seq1) seq2)
	((null? seq2) seq1)
	(else
	 (if (and (bit-string? (cadr seq1))
		  (bit-string? (caar seq2)))
	     (let ((result (instruction-append (cadr seq1) (caar seq2))))
	       (set-car! (cdr seq1) result)
	       (if (not (eq? (car seq2) (cdr seq2)))
		   (begin (set-cdr! (cdr seq1) (cdr (car seq2)))
			  (set-cdr! seq1 (cdr seq2)))))
	     (begin (set-cdr! (cdr seq1) (car seq2))
		    (set-cdr! seq1 (cdr seq2))))
	 seq1)))