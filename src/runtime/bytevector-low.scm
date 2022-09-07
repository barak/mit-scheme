#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; R7RS bytevectors (early in cold-load)
;;; package: (runtime bytevector)

(declare (usual-integrations))

(define-primitives
  (allocate-bytevector 1)
  (bytevector-length 1)
  (bytevector-u8-ref 2)
  (bytevector-u8-set! 3)
  (bytevector? 1)
  (integer-length-in-bits 1)
  (legacy-string-allocate string-allocate 1)
  (legacy-string? string? 1))

(define (bytevector<? b1 b2)
  (let ((l1 (bytevector-length b1))
	(l2 (bytevector-length b2)))
    (let ((end (fix:min l1 l2)))
      (let loop ((index 0))
	(if (fix:< index end)
	    (let ((u1 (bytevector-u8-ref b1 index))
		  (u2 (bytevector-u8-ref b2 index)))
	      (if (fix:= u1 u2)
		  (loop (fix:+ index 1))
		  (fix:< u1 u2)))
	    (fix:< l1 l2))))))