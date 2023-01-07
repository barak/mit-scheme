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

;;;; SRFI 14: Character-set Library

(define-library (srfi 14)
  (import (scheme base)
	  (scheme char)
	  (only (srfi 1)
		drop-right
		last)
	  (srfi 143)
	  (only (mit legacy runtime)
		->char-set
		char-set->list
		char-set->string
		char-set-adjoin
		char-set-adjoin!
		char-set-any
		char-set-complement
		char-set-complement!
		char-set-contains?
		char-set-copy
		char-set-count
		char-set-cursor
		char-set-cursor-next
		char-set-delete
		char-set-delete!
		char-set-diff+intersection
		char-set-diff+intersection!
		char-set-difference
		char-set-difference!
		char-set-every
		char-set-filter
		char-set-filter!
		char-set-fold
		char-set-for-each
		char-set-hash
		char-set-intersection
		char-set-intersection!
		char-set-map
		char-set-ref
		char-set-size
		char-set-unfold
		char-set-unfold!
		char-set-union
		char-set-union!
		char-set-xor
		char-set-xor!
		char-set:ascii
		char-set:blank
		char-set:digit
		char-set:empty
		char-set:full
		char-set:graphic
		char-set:hex-digit
		char-set:iso-control
		char-set:letter
		char-set:letter+digit
		char-set:lower-case
		char-set:printing
		char-set:punctuation
		char-set:symbol
		char-set:title-case
		char-set:upper-case
		char-set:whitespace
		char-set<=
		char-set=
		char-set?
		end-of-char-set?
		list->char-set
		list->char-set!
		string->char-set
		string->char-set!
		ucs-range->char-set
		ucs-range->char-set!))
  (export ->char-set
	  char-set
	  char-set->list
	  char-set->string
	  char-set-adjoin
	  char-set-adjoin!
	  char-set-any
	  char-set-complement
	  char-set-complement!
	  char-set-contains?
	  char-set-copy
	  char-set-count
	  char-set-cursor
	  char-set-cursor-next
	  char-set-delete
	  char-set-delete!
	  char-set-diff+intersection
	  char-set-diff+intersection!
	  char-set-difference
	  char-set-difference!
	  char-set-every
	  char-set-filter
	  char-set-filter!
	  char-set-fold
	  char-set-for-each
	  char-set-hash
	  char-set-intersection
	  char-set-intersection!
	  char-set-map
	  char-set-ref
	  char-set-size
	  char-set-unfold
	  char-set-unfold!
	  char-set-union
	  char-set-union!
	  char-set-xor
	  char-set-xor!
	  char-set:ascii
	  char-set:blank
	  char-set:digit
	  char-set:empty
	  char-set:full
	  char-set:graphic
	  char-set:hex-digit
	  char-set:iso-control
	  char-set:letter
	  char-set:letter+digit
	  char-set:lower-case
	  char-set:printing
	  char-set:punctuation
	  char-set:symbol
	  char-set:title-case
	  char-set:upper-case
	  char-set:whitespace
	  char-set<=
	  char-set=
	  char-set?
	  end-of-char-set?
	  list->char-set
	  list->char-set!
	  string->char-set
	  string->char-set!
	  ucs-range->char-set
	  ucs-range->char-set!)
  (begin
    (define (char-set . chars)
      (list->char-set chars))
    ))