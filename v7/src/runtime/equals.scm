#| -*-Scheme-*-

$Id: equals.scm,v 14.9 2001/12/18 18:39:29 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Equality
;;; package: (runtime equality)

(declare (usual-integrations))

(define (eqv? x y)
  ;; EQV? is officially supposed to work on booleans, characters, and
  ;; numbers specially, but it turns out that EQ? does the right thing
  ;; for everything but numbers, so we take advantage of that.
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (and (not (fix:fixnum? x))
	       (if (number? y)
		   (and (= x y)
			(boolean=? (exact? x) (exact? y)))
		   (and (object-type? (ucode-type vector) y)
			(fix:zero? (vector-length x))
			(fix:zero? (vector-length y)))))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))

(define (equal? x y)
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (cond ((pair? y)
		 (and (equal? (car x) (car y))
		      (equal? (cdr x) (cdr y))))
		((vector? y)
		 (let ((size (vector-length x)))
		   (and (fix:= size (vector-length y))
			(let loop ((index 0))
			  (or (fix:= index size)
			      (and (equal? (vector-ref x index)
					   (vector-ref y index))
				   (loop (fix:+ index 1))))))))
		((string? y)
		 (string=? x y))
		((number? y)
		 (and (= x y)
		      (boolean=? (exact? x) (exact? y))))
		((cell? y)
		 (equal? (cell-contents x) (cell-contents y)))
		((bit-string? y)
		 (bit-string=? x y))
		((pathname? x)
		 (and (pathname? y)
		      (pathname=? x y)))
		(else #f))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))