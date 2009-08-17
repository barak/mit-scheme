#| -*-Scheme-*-

$Id: 4eeb6bc5bb93975e9727019802993995272f53a7 $

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