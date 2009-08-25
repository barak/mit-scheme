#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Miscellaneous Win32 Facilities

(declare (usual-integrations))

(define (win32-clipboard-write-text s)
  (let* ((len (+ (string-length s) 1))
	 (mem
	  (global-alloc #x2002		;(GMEM_MOVEABLE | GMEM_DDESHARE)
			len)))
    (if (= mem 0)
	(error "Unable to allocate global memory of length" len))
    (copy-memory (global-lock mem) s len)
    (global-unlock mem)
    (open-clipboard 0)
    (empty-clipboard)
    (set-clipboard-data CF_TEXT mem)
    (close-clipboard)))

(define (win32-clipboard-read-text)
  (open-clipboard 0)
  (let ((mem (get-clipboard-data CF_TEXT)))
    (and (not (= mem 0))
	 (let* ((maxlen (global-size mem))
		(s (string-allocate maxlen))
		(ptr (global-lock mem)))
	   (copy-memory s ptr maxlen)
	   (global-unlock mem)
	   (close-clipboard)
	   (set-string-length! s (vector-8b-find-next-char s 0 maxlen 0))
	   s))))

(define (win32-screen-width)
  (get-system-metrics SM_CXSCREEN))

(define (win32-screen-height)
  (get-system-metrics SM_CYSCREEN))