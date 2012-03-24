#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Tests of string copying performance

(declare (usual-integrations))

(define (test-noop length iterations)
  (let ((from (make-string length))
	(to (make-string (fix:* 2 length))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i iterations)))))

(define (test-left length iterations)
  (let ((from (make-string length))
	(to (make-string (fix:* 2 length))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i iterations))
      (substring-move-left! from 0 length to length))))

(define (test-right length iterations)
  (let ((from (make-string length))
	(to (make-string (fix:* 2 length))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i iterations))
      (substring-move-right! from 0 length to length))))

(define (test-inline length iterations)
  (let ((from (make-string length))
	(to (make-string (fix:* 2 length))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i iterations))
      (do ((fi 0 (fix:+ fi 1))
	   (ti length (fix:+ ti 1)))
	  ((fix:= fi length))
	(string-set! to ti (string-ref from fi))))))