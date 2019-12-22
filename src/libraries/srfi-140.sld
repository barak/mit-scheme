#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; SRFI 140: Strings

(define-library (srfi 140)
  (import (scheme base)
	  (scheme char)
	  (only (srfi 1)
		drop-right
		last)
	  (srfi 143)
	  (only (mit legacy runtime)
		char->string
		default-object?
		error:bad-range-argument
		fix:end-index
		fix:start-index
		guarantee
		istring?
		non-negative-fixnum?
		string->immutable
		string->utf16
		string->utf16be
		string->utf16le
		string-concatenate
		string-builder
		string-fold
		string-fold-right
		string-joiner*
		string-null?
		string-padder
		string-search-backward
		string-search-forward
		string-slice
		string-titlecase
		string-trimmer
		unspecific
		utf16->string
		utf16be->string
		utf16le->string))
  (export istring?
	  list->string
	  ;make-string
	  ;mstring?
	  reverse-list->string
	  string
	  string->list
	  string->utf16
	  string->utf16be
	  string->utf16le
	  string->utf8
	  string->vector
	  string-any
	  string-append
	  ;string-append!
	  string-ci<=?
	  string-ci<?
	  string-ci=?
	  string-ci>=?
	  string-ci>?
	  string-concatenate
	  string-concatenate-reverse
	  string-contains
	  string-contains-right
	  ;string-copy
	  ;string-copy!
	  string-count
	  string-downcase
	  string-drop
	  string-drop-right
	  string-every
	  ;string-fill!
	  string-filter
	  string-fold
	  string-fold-right
	  string-foldcase
	  string-for-each
	  string-for-each-index
	  string-index
	  string-index-right
	  string-join
	  string-length
	  string-map
	  string-map-index
	  string-null?
	  string-pad
	  string-pad-right
	  string-prefix-length
	  string-prefix?
	  string-ref
	  string-remove
	  string-repeat
	  string-replace
	  ;string-replace!
	  ;string-set!
	  string-skip
	  string-skip-right
	  string-split
	  string-suffix-length
	  string-suffix?
	  string-tabulate
	  string-take
	  string-take-right
	  string-titlecase
	  string-trim
	  string-trim-both
	  string-trim-right
	  string-unfold
	  string-unfold-right
	  string-upcase
	  string<=?
	  string<?
	  string=?
	  string>=?
	  string>?
	  string?
	  substring
	  utf16->string
	  utf16be->string
	  utf16le->string
	  utf8->string
	  vector->string
	  xsubstring)
  (include "srfi-140-impl.scm"))

(define-library* (srfi 140 base)
  (import (drop (scheme base)
		(exports (srfi 140)))
	  (srfi 140))
  (export (exports (scheme base))
	  (exports (srfi 140))))

(define-library* (srfi 140 char)
  (import (drop (scheme char)
		(exports (srfi 140)))
	  (take (srfi 140)
		(exports (scheme char))))
  (export (exports (scheme char))))

(define-library* (srfi 140 istrings)
  (define istring-names
    (union list->string
	   reverse-list->string
	   string
	   string-append
	   string-concatenate
	   string-concatenate-reverse
	   string-downcase
	   string-drop
	   string-drop-right
	   string-filter
	   string-foldcase
	   string-join
	   string-map
	   string-map-index
	   string-pad
	   string-pad-right
	   string-remove
	   string-repeat
	   string-replace
	   string-tabulate
	   string-take
	   string-take-right
	   string-titlecase
	   string-trim
	   string-trim-both
	   string-trim-right
	   string-unfold
	   string-unfold-right
	   string-upcase
	   substring
	   utf16->string
	   utf16be->string
	   utf16le->string
	   utf8->string
	   vector->string
	   xsubstring))
  (import (take (srfi 140) istring-names))
  (export istring-names))