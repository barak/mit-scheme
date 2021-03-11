#| -*-Scheme-*-

Copyright (c) 1987, 1988, 1990, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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