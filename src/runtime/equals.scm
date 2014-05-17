#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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
		   (number:eqv? x y)
		   (and (object-type? (ucode-type vector) y)
			(fix:zero? (vector-length x))
			(fix:zero? (vector-length y)))))
	  (and (number? x)
	       (number? y)
	       (number:eqv? x y)))))

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
		 (number:eqv? x y))
		((cell? y)
		 (equal? (cell-contents x) (cell-contents y)))
		((bit-string? y)
		 (bit-string=? x y))
		((pathname? x)
		 (and (pathname? y)
		      (pathname=? x y)))
		((char-set? x)
		 (and (char-set? y)
		      (char-set=? x y)))
		(else #f))
	  (and (number? x)
	       (number? y)
	       (number:eqv? x y)))))