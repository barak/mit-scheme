#| -*-Scheme-*-

$Id: insseq.scm,v 4.6 2003/02/14 18:28:00 cph Exp $

Copyright (c) 1987, 1988, 1990, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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